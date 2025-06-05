# Streaming Function Calls Documentation

## Overview

This document provides comprehensive documentation for the streaming function calls implementation in the Erlang AI Agent System. The implementation enables real-time streaming of AI responses while detecting, aggregating, and executing function calls mid-stream.

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Core Components](#core-components)
3. [Implementation Details](#implementation-details)
4. [API Integration](#api-integration)
5. [Usage Examples](#usage-examples)
6. [Event Flow](#event-flow)
7. [Error Handling](#error-handling)
8. [Performance Considerations](#performance-considerations)
9. [Testing Guide](#testing-guide)
10. [Troubleshooting](#troubleshooting)

## Visual Documentation

- **ASCII Diagrams**: See [STREAMING_FUNCTION_CALLS_DIAGRAMS.md](STREAMING_FUNCTION_CALLS_DIAGRAMS.md) for detailed ASCII art diagrams and flowcharts
- **Interactive Visualization**: Open [streaming_visualization.html](streaming_visualization.html) in a browser for an interactive demo
- **Mermaid Diagrams**: View [streaming_flow_mermaid.md](streaming_flow_mermaid.md) for professional flow diagrams (requires Mermaid support)

## Architecture Overview

The streaming function calls architecture consists of three main layers:

```
┌─────────────────────────────────────────────────────────────┐
│                    Client Application                        │
│                 (Receives streaming events)                  │
└─────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────┐
│                    Agent Instance                            │
│          (Orchestrates streaming and execution)              │
├─────────────────────────────────────────────────────────────┤
│  • handle_streaming_response/3                               │
│  • handle_streaming_response_events/3                        │
│  • process_streaming_chat_with_*_api/3                      │
└─────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────┐
│              Streaming Function Handler                      │
│           (Aggregates and executes functions)                │
├─────────────────────────────────────────────────────────────┤
│  • init_accumulator/0                                        │
│  • process_stream_event/3                                    │
│  • finalize_stream/3                                         │
│  • execute_tool_calls_parallel/1                            │
└─────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────┐
│                  OpenAI API Clients                          │
│            (Handle HTTP streaming & SSE parsing)             │
├─────────────────────────────────────────────────────────────┤
│  • openai_chat:create_streaming_completion/3                 │
│  • openai_responses:create_streaming_response/3              │
│  • process_sse_events/1                                      │
└─────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. streaming_function_handler.erl

The central module responsible for handling streaming function calls.

#### Key Functions:

##### `init_accumulator/0`
```erlang
-spec init_accumulator() -> #accumulator{}.
```
Initializes an empty accumulator record for tracking streaming state.

**Returns:**
- `#accumulator{}` - Empty accumulator with:
  - `content`: Empty binary for text content
  - `tool_calls`: Empty list for completed tool calls
  - `tool_call_map`: Empty map for in-progress tool calls
  - `metadata`: Empty map for additional data

##### `process_stream_event/3`
```erlang
-spec process_stream_event(Event :: map(), 
                          Accumulator :: #accumulator{}, 
                          StreamPid :: pid()) -> 
    {continue, #accumulator{}} | 
    {done, #accumulator{}} | 
    {error, term()}.
```
Processes a single streaming event and updates the accumulator.

**Parameters:**
- `Event`: The streaming event to process
- `Accumulator`: Current accumulator state
- `StreamPid`: PID to send streaming notifications to

**Returns:**
- `{continue, NewAccumulator}`: Continue processing with updated state
- `{done, FinalAccumulator}`: Stream is complete
- `{error, Reason}`: An error occurred

##### `finalize_stream/3`
```erlang
-spec finalize_stream(Accumulator :: #accumulator{},
                     StreamPid :: pid(),
                     State :: term()) ->
    {ok, Response :: map(), NewState :: term()} |
    {error, Reason :: term()}.
```
Finalizes the stream, executes any accumulated tool calls, and returns results.

**Parameters:**
- `Accumulator`: Final accumulator with all streamed data
- `StreamPid`: PID to send final results to
- `State`: Agent state

**Returns:**
- `{ok, Response, NewState}`: Success with final response and updated state
- `{error, Reason}`: Error occurred during finalization

### 2. Event Types

The system handles various streaming event types:

#### Content Events
```erlang
{content_delta, Delta :: binary()}
```
Partial text content from the AI response.

#### Function Call Events
```erlang
{tool_call_start, ToolCall :: map()}
{tool_call_delta, Index :: integer(), Delta :: map()}
{tool_call_complete, Index :: integer()}
```
Function call lifecycle events.

#### Control Events
```erlang
{stream_done, Metadata :: map()}
{error, Reason :: term()}
```
Stream completion and error events.

### 3. Accumulator Structure

```erlang
-record(accumulator, {
    content = <<>> :: binary(),           % Accumulated text content
    tool_calls = [] :: list(),           % Completed tool calls
    tool_call_map = #{} :: map(),        % In-progress tool calls by index
    metadata = #{} :: map()              % Additional metadata
}).
```

## Implementation Details

### Function Call Aggregation

Tool calls are aggregated across multiple streaming chunks:

1. **Initial Detection**: When a tool call starts, it's added to `tool_call_map` with its index
2. **Delta Accumulation**: Arguments are concatenated as deltas arrive
3. **Completion**: When complete, the tool call moves from `tool_call_map` to `tool_calls`
4. **Execution**: All accumulated tool calls are executed in parallel

Example aggregation flow:
```erlang
% Initial state
#{tool_call_map => #{}}

% After tool_call_start
#{tool_call_map => #{
    0 => #{
        <<"id">> => <<"call_123">>,
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"search">>,
            <<"arguments">> => <<>>
        }
    }
}}

% After multiple deltas
#{tool_call_map => #{
    0 => #{
        <<"function">> => #{
            <<"name">> => <<"search">>,
            <<"arguments">> => <<"{\"query\": \"weather\"}">>
        }
    }
}}

% After completion
#{tool_calls => [ToolCall], tool_call_map => #{}}
```

### Parallel Tool Execution

Tools are executed concurrently for optimal performance:

```erlang
execute_tool_calls_parallel(ToolCalls) ->
    Parent = self(),
    
    % Spawn workers for each tool call
    Workers = lists:map(fun(ToolCall) ->
        spawn_link(fun() ->
            Result = execute_single_tool_call(ToolCall),
            Parent ! {tool_result, self(), Result}
        end)
    end, ToolCalls),
    
    % Collect results in order
    collect_results(Workers, []).
```

## API Integration

### Chat Completions API

Uses the standard OpenAI streaming format with SSE:

```erlang
% Enable streaming
Options = #{
    tools => Tools,
    tool_choice => <<"auto">>,
    stream => true
},

% Create streaming completion
openai_chat:create_streaming_completion(Model, Messages, Options).
```

Streaming events arrive as:
```
data: {"choices":[{"delta":{"content":"Hello"}}]}
data: {"choices":[{"delta":{"tool_calls":[{"index":0,"function":{"name":"search"}}]}}]}
data: [DONE]
```

### Responses API

Uses the newer Responses API streaming format:

```erlang
% Enable streaming with responses API
Options = #{
    tools => Tools,
    tool_choice => <<"auto">>,
    parallel_tool_calls => true,
    stream => true
},

% Create streaming response
openai_responses:create_streaming_response(Input, Model, Options).
```

Events arrive as:
```
event: response.output_text.delta
data: {"delta": "Hello"}

event: response.output_item.added
data: {"item": {"type": "function_call", "name": "search"}}

event: response.function_call_arguments.delta
data: {"delta": "{\"query\":", "item_index": 0}

event: response.done
data: {"response": {...}}
```

## Usage Examples

### Basic Streaming with Function Calls

```erlang
%% Create an agent with tools
{ok, AgentPid} = agent_supervisor:start_agent(#{
    name => <<"Assistant">>,
    model => <<"gpt-4o">>,
    tools => [web_search, calculator, file_operations],
    api_preference => chat
}),

%% Start streaming chat
StreamPid = self(),
agent:stream_chat(AgentPid, <<"What's the weather in Paris?">>, StreamPid),

%% Receive streaming events
receive_stream_events().

receive_stream_events() ->
    receive
        {stream_start, Info} ->
            io:format("Stream started: ~p~n", [Info]),
            receive_stream_events();
            
        {stream_token, Token} ->
            io:format("~s", [Token]),
            receive_stream_events();
            
        {stream_function_call_started, FuncCall} ->
            io:format("~nFunction call started: ~p~n", [FuncCall]),
            receive_stream_events();
            
        {stream_function_execution_start} ->
            io:format("Executing functions...~n"),
            receive_stream_events();
            
        {stream_tool_result, Result} ->
            io:format("Tool result: ~p~n", [Result]),
            receive_stream_events();
            
        {stream_complete, Response} ->
            io:format("~nStream complete!~n"),
            Response;
            
        {stream_error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            error
    after 30000 ->
        timeout
    end.
```

### Advanced Multi-Tool Streaming

```erlang
%% Message that triggers multiple tool calls
Message = <<"Search for Paris weather, calculate 20°C to Fahrenheit, 
             and save the results to weather_report.txt">>,

%% Stream with multiple parallel function calls
agent:stream_chat(AgentPid, Message, self()),

%% The system will:
%% 1. Stream initial response text
%% 2. Detect multiple function calls
%% 3. Execute them in parallel
%% 4. Stream the final response with all results
```

### Custom Stream Processing

```erlang
%% Implement custom stream processing
process_custom_stream(AgentPid, Message) ->
    Self = self(),
    StreamHandler = spawn(fun() -> 
        custom_stream_handler(Self, [])
    end),
    
    agent:stream_chat(AgentPid, Message, StreamHandler).

custom_stream_handler(Parent, Tokens) ->
    receive
        {stream_token, Token} ->
            % Accumulate tokens
            custom_stream_handler(Parent, [Token | Tokens]);
            
        {stream_function_call_started, _} ->
            % Notify parent about function call
            Parent ! {function_detected},
            custom_stream_handler(Parent, Tokens);
            
        {stream_complete, Response} ->
            % Send accumulated data to parent
            AllText = iolist_to_binary(lists:reverse(Tokens)),
            Parent ! {complete, AllText, Response}
    end.
```

## Event Flow

### Successful Stream with Function Call

```
Client                Agent              Handler           OpenAI API
  │                     │                  │                  │
  ├──stream_chat────────►                  │                  │
  │                     ├──create_stream───►                  │
  │                     │                  ├──HTTP stream─────►
  │                     │                  │                  │
  ◄──stream_start───────┤                  ◄──SSE: role───────┤
  │                     │                  │                  │
  ◄──stream_token───────┤◄─content_delta───┤◄──SSE: content──┤
  │                     │                  │                  │
  ◄──function_started───┤◄─tool_start──────┤◄──SSE: tool_call┤
  │                     │                  │                  │
  ◄──function_args─────┤◄─tool_delta──────┤◄──SSE: args─────┤
  │                     │                  │                  │
  ◄──function_complete──┤◄─tool_complete───┤◄──SSE: [DONE]───┤
  │                     │                  │                  │
  ◄──execution_start────┤                  │                  │
  │                     ├──execute_tools───►                  │
  │                     │                  │                  │
  ◄──tool_result────────┤◄─────────────────┤                  │
  │                     │                  │                  │
  │                     ├──continue_stream─►                  │
  │                     │                  ├──HTTP stream─────►
  │                     │                  │                  │
  ◄──stream_token───────┤◄─content_delta───┤◄──SSE: content──┤
  │                     │                  │                  │
  ◄──stream_complete────┤◄─stream_done─────┤◄──SSE: [DONE]───┤
  │                     │                  │                  │
```

### Error Handling Flow

```
Client                Agent              Handler           OpenAI API
  │                     │                  │                  │
  ├──stream_chat────────►                  │                  │
  │                     ├──create_stream───►                  │
  │                     │                  ├──HTTP stream─────►
  │                     │                  │                  │
  ◄──stream_start───────┤                  │                  │
  │                     │                  │                  │
  ◄──stream_token───────┤◄─content_delta───┤◄──SSE: content──┤
  │                     │                  │                  │
  │                     │                  ◄──HTTP error──────┤
  │                     │                  │                  │
  ◄──stream_error───────┤◄─error───────────┤                  │
  │                     │                  │                  │
```

## Error Handling

### Timeout Handling

All streaming operations have configurable timeouts:

```erlang
%% Default timeouts
-define(STREAM_TIMEOUT, 30000).      % 30 seconds per chunk
-define(TOOL_TIMEOUT, 30000).        % 30 seconds per tool
-define(COMPLETE_TIMEOUT, 60000).    % 60 seconds total

%% Custom timeout configuration
Options = #{
    timeout => 45000,  % Override chunk timeout
    tool_timeout => 60000  % Override tool timeout
}.
```

### Error Recovery

The system implements graceful error recovery:

1. **Partial Content Recovery**: If streaming fails mid-way, accumulated content is preserved
2. **Tool Failure Isolation**: Individual tool failures don't crash the entire stream
3. **Automatic Retries**: Network errors trigger automatic retry with exponential backoff

```erlang
handle_stream_error(Error, Accumulator, StreamPid) ->
    case Error of
        timeout ->
            % Send partial content if available
            Content = Accumulator#accumulator.content,
            StreamPid ! {stream_partial, Content},
            {error, timeout};
            
        {tool_error, ToolId, Reason} ->
            % Continue with other tools
            StreamPid ! {tool_failed, ToolId, Reason},
            continue_stream(Accumulator, StreamPid);
            
        {network_error, _} ->
            % Retry with backoff
            timer:sleep(1000),
            retry_stream(Accumulator, StreamPid)
    end.
```

### Error Types

Common error types and their handling:

| Error Type | Description | Recovery Strategy |
|------------|-------------|-------------------|
| `timeout` | Stream chunk timeout | Return partial content |
| `tool_timeout` | Tool execution timeout | Mark tool as failed, continue |
| `parse_error` | Invalid JSON in stream | Skip chunk, continue |
| `api_error` | OpenAI API error | Propagate to client |
| `network_error` | Connection issues | Retry with backoff |

## Performance Considerations

### Memory Management

The accumulator is designed for efficient memory usage:

- Binary concatenation uses efficient binary construction
- Tool calls are moved from map to list when complete
- Partial results are streamed immediately to avoid buffering

### Concurrency

- Tool execution uses lightweight Erlang processes
- No shared state between tool executions
- Results are collected in order despite parallel execution

### Optimization Tips

1. **Limit Tool Calls**: Configure `max_tool_calls` to prevent excessive parallelism
2. **Stream Buffer Size**: Adjust buffer size for optimal chunk processing
3. **Timeout Tuning**: Set appropriate timeouts based on tool complexity

```erlang
%% Performance-optimized configuration
Config = #{
    max_tool_calls => 5,           % Limit parallel tools
    stream_buffer_size => 4096,    % 4KB chunks
    tool_timeout => 20000,         % 20s per tool
    enable_compression => true     % Compress large responses
}.
```

## Testing Guide

### Unit Tests

Test individual components:

```erlang
%% Test accumulator operations
test_accumulator() ->
    Acc0 = streaming_function_handler:init_accumulator(),
    
    % Test content accumulation
    Event1 = #{type => content, data => <<"Hello">>},
    {continue, Acc1} = streaming_function_handler:process_stream_event(
        Event1, Acc0, self()
    ),
    <<"Hello">> = Acc1#accumulator.content,
    
    % Test tool call accumulation
    ToolEvent = #{
        type => tool_calls, 
        data => [#{<<"index">> => 0, <<"id">> => <<"test">>}]
    },
    {continue, Acc2} = streaming_function_handler:process_stream_event(
        ToolEvent, Acc1, self()
    ),
    1 = maps:size(Acc2#accumulator.tool_call_map),
    
    ok.
```

### Integration Tests

Test complete streaming flows:

```erlang
%% Test full streaming with mock API
test_streaming_integration() ->
    % Start mock server
    {ok, MockPid} = start_mock_openai_server(),
    
    % Configure agent to use mock
    {ok, AgentPid} = agent_supervisor:start_agent(#{
        name => <<"Test Agent">>,
        model => <<"gpt-4">>,
        tools => [test_tool],
        base_url => <<"http://localhost:8081">>
    }),
    
    % Send message and collect events
    Events = collect_all_events(AgentPid, <<"Test message">>),
    
    % Verify event sequence
    assert_event_sequence(Events, [
        {stream_start, _},
        {stream_token, _},
        {stream_function_call_started, _},
        {stream_function_execution_start},
        {stream_tool_result, _},
        {stream_complete, _}
    ]),
    
    % Cleanup
    stop_mock_server(MockPid),
    ok.
```

### Load Tests

Test streaming under load:

```erlang
%% Concurrent streaming test
test_concurrent_streams() ->
    {ok, AgentPid} = create_test_agent(),
    
    % Start multiple concurrent streams
    Pids = [spawn(fun() ->
        stream_and_report(AgentPid, N)
    end) || N <- lists:seq(1, 100)],
    
    % Wait for completion
    Results = collect_results(Pids),
    
    % Verify all succeeded
    100 = length([ok || ok <- Results]),
    
    ok.
```

## Troubleshooting

### Common Issues

#### 1. Function Calls Not Detected

**Symptom**: Stream completes but function calls aren't executed

**Possible Causes**:
- Model not instructed to use tools
- Tools not properly configured
- Streaming parsing error

**Solution**:
```erlang
% Ensure tools are properly configured
Tools = agent_tools:get_enhanced_tools([web_search, calculator]),
io:format("Configured tools: ~p~n", [Tools]),

% Enable debug logging
application:set_env(agents, debug_streaming, true),

% Check system prompt includes tool instructions
SystemPrompt = <<"You have access to tools. Use them when needed.">>.
```

#### 2. Incomplete Function Arguments

**Symptom**: Tool execution fails with "invalid arguments"

**Possible Causes**:
- Arguments split across chunks
- Incomplete JSON in stream

**Solution**:
```erlang
% Enable argument validation
validate_tool_arguments(ToolCall) ->
    Args = maps:get(<<"arguments">>, 
                   maps:get(<<"function">>, ToolCall, #{}), 
                   <<>>),
    case jsx:is_json(Args) of
        true -> ok;
        false -> {error, incomplete_arguments}
    end.
```

#### 3. Stream Timeouts

**Symptom**: Stream stops with timeout error

**Possible Causes**:
- Slow network
- Complex tool execution
- API rate limits

**Solution**:
```erlang
% Increase timeouts
Options = #{
    timeout => 60000,        % 60 seconds
    tool_timeout => 45000,   % 45 seconds per tool
    idle_timeout => 5000     % 5 seconds between chunks
}.
```

### Debug Mode

Enable comprehensive debugging:

```erlang
%% Enable debug mode
application:set_env(agents, debug_streaming, true),
application:set_env(agents, log_stream_events, true),
application:set_env(agents, trace_tool_calls, true),

%% Debug wrapper
debug_stream(AgentPid, Message) ->
    Self = self(),
    DebugPid = spawn(fun() -> debug_handler(Self) end),
    agent:stream_chat(AgentPid, Message, DebugPid).

debug_handler(Parent) ->
    debug_handler_loop(Parent, []).

debug_handler_loop(Parent, Events) ->
    receive
        Event ->
            io:format("[DEBUG] ~p: ~p~n", [timestamp(), Event]),
            Parent ! Event,
            debug_handler_loop(Parent, [Event | Events])
    after 30000 ->
        io:format("[DEBUG] Stream complete. Total events: ~p~n", 
                  [length(Events)]),
        Parent ! {debug_complete, lists:reverse(Events)}
    end.
```

### Monitoring

Monitor streaming performance:

```erlang
%% Streaming metrics collector
-record(stream_metrics, {
    start_time :: erlang:timestamp(),
    first_token_time :: erlang:timestamp() | undefined,
    token_count = 0 :: non_neg_integer(),
    function_count = 0 :: non_neg_integer(),
    error_count = 0 :: non_neg_integer(),
    total_bytes = 0 :: non_neg_integer()
}).

collect_stream_metrics(AgentPid, Message) ->
    Metrics = #stream_metrics{start_time = erlang:timestamp()},
    stream_with_metrics(AgentPid, Message, Metrics).
```

## Best Practices

1. **Always Handle All Event Types**: Even if you only care about content, handle function events to avoid message queue buildup

2. **Use Appropriate Timeouts**: Set timeouts based on expected tool execution time

3. **Implement Graceful Degradation**: Handle tool failures without breaking the entire stream

4. **Monitor Memory Usage**: For long streams, periodically check accumulator size

5. **Test Error Scenarios**: Include timeout and failure tests in your test suite

6. **Log Important Events**: Log function executions and errors for debugging

7. **Version Your Tools**: Include version info in tool definitions for compatibility

## Future Enhancements

Planned improvements for streaming function calls:

1. **Streaming Tool Results**: Stream tool execution progress for long-running tools
2. **Partial Execution**: Execute tools as soon as minimum arguments are available  
3. **Caching**: Cache tool results for repeated calls within a session
4. **Batching**: Intelligently batch related tool calls
5. **Fallback Strategies**: Automatic fallback when tools fail
6. **Compression**: Compress large streaming responses
7. **Priority Queues**: Prioritize critical tool executions

## Conclusion

The streaming function calls implementation provides a robust, performant solution for real-time AI interactions with tool usage. By properly handling streaming events, aggregating function calls, and executing tools in parallel, the system delivers a seamless experience for end users while maintaining the flexibility and power of function calling.

For additional support or questions, please refer to the main project documentation or submit an issue on the project repository.