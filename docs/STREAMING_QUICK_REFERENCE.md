# Streaming Function Calls - Quick Reference

## Quick Start

```erlang
%% 1. Create agent with tools
{ok, Agent} = agent_supervisor:start_agent(#{
    name => <<"Assistant">>,
    model => <<"gpt-4o">>,
    tools => [web_search, calculator],
    api_preference => chat  % or 'responses'
}),

%% 2. Start streaming
agent:stream_chat(Agent, <<"Search for Erlang tutorials">>),

%% 3. Receive events
receive
    {stream_token, Token} -> io:format("~s", [Token]);
    {stream_function_call_started, F} -> io:format("Function: ~p~n", [F]);
    {stream_complete, Response} -> io:format("Done: ~p~n", [Response])
end.
```

## Event Types Cheat Sheet

| Event | Description | Data |
|-------|-------------|------|
| `{stream_start, Info}` | Stream started | `#{agent_id => Id}` |
| `{stream_token, Token}` | Text chunk | `binary()` |
| `{stream_function_call_started, Call}` | Function detected | `#{<<"name">> => Name}` |
| `{stream_function_arguments_delta, Idx, Delta}` | Partial args | `{Index, binary()}` |
| `{stream_function_call_complete, Call}` | Args complete | Full function call map |
| `{stream_function_execution_start}` | Executing tools | `{}` |
| `{stream_tool_result, Result}` | Tool result | `#{tool_call => Call, result => Result}` |
| `{stream_complete, Response}` | Stream done | `#{message => Text, tool_calls => [...]}` |
| `{stream_error, Reason}` | Error occurred | `term()` |

## Common Patterns

### Collect All Events
```erlang
collect_stream() ->
    collect_stream([], []).

collect_stream(Tokens, Events) ->
    receive
        {stream_token, T} -> 
            collect_stream([T|Tokens], Events);
        {stream_complete, R} -> 
            {lists:reverse(Tokens), lists:reverse(Events), R};
        Event -> 
            collect_stream(Tokens, [Event|Events])
    after 30000 -> 
        timeout
    end.
```

### Filter Token Stream
```erlang
stream_tokens_only() ->
    receive
        {stream_token, Token} -> 
            io:format("~s", [Token]),
            stream_tokens_only();
        {stream_complete, _} -> 
            ok;
        _ -> 
            stream_tokens_only()
    end.
```

### Track Function Calls
```erlang
track_functions() ->
    track_functions([]).

track_functions(Calls) ->
    receive
        {stream_function_call_started, Call} ->
            track_functions([Call | Calls]);
        {stream_tool_result, #{tool_call := Call, result := R}} ->
            io:format("Function ~s returned: ~p~n", 
                     [maps:get(<<"name">>, Call), R]),
            track_functions(Calls);
        {stream_complete, _} ->
            {ok, Calls}
    end.
```

## Configuration Quick Reference

### Agent Creation Options
```erlang
#{
    name => <<"My Agent">>,
    model => <<"gpt-4o">>,           % or <<"gpt-4">>
    tools => [tool1, tool2],         % Available tools
    api_preference => chat,          % 'chat' or 'responses'
    streaming_config => #{
        chunk_timeout => 30000,      % ms per chunk
        tool_timeout => 30000,       % ms per tool
        max_tool_calls => 10         % parallel limit
    }
}
```

### Available Tools
- `web_search` - Search the web
- `jina_search` - Jina AI search
- `jina_deep_search` - Deep search with reasoning
- `file_read` - Read files
- `file_write` - Write files
- `calculator` - Math calculations
- `knowledge_base_retrieval` - Search knowledge bases

## Error Handling

### Basic Error Handling
```erlang
safe_stream(Agent, Message) ->
    agent:stream_chat(Agent, Message),
    receive
        {stream_error, Reason} -> 
            {error, Reason};
        {stream_complete, Response} -> 
            {ok, Response}
    after 60000 -> 
        {error, timeout}
    end.
```

### With Retry
```erlang
stream_with_retry(Agent, Message, Retries) when Retries > 0 ->
    case safe_stream(Agent, Message) of
        {error, _} -> 
            timer:sleep(1000),
            stream_with_retry(Agent, Message, Retries - 1);
        Result -> 
            Result
    end;
stream_with_retry(_, _, 0) ->
    {error, max_retries}.
```

## Debugging

### Enable Debug Logging
```erlang
application:set_env(agents, debug_streaming, true).
```

### Log All Events
```erlang
debug_stream(Agent, Message) ->
    agent:stream_chat(Agent, Message),
    debug_loop().

debug_loop() ->
    receive
        Event ->
            io:format("[~p] ~p~n", [calendar:local_time(), Event]),
            case Event of
                {stream_complete, _} -> ok;
                {stream_error, _} -> ok;
                _ -> debug_loop()
            end
    end.
```

### Measure Performance
```erlang
timed_stream(Agent, Message) ->
    Start = erlang:system_time(millisecond),
    agent:stream_chat(Agent, Message),
    case wait_complete() of
        {ok, Response} ->
            Time = erlang:system_time(millisecond) - Start,
            {ok, Response, Time};
        Error -> 
            Error
    end.
```

## Common Issues & Solutions

| Issue | Cause | Solution |
|-------|-------|----------|
| No function calls detected | Tools not configured | Ensure tools are in agent config |
| Timeout errors | Slow network/API | Increase chunk_timeout |
| Incomplete function args | Parsing error | Check debug logs |
| Tool execution fails | Invalid arguments | Validate tool inputs |
| Stream stops abruptly | Network disconnect | Implement retry logic |

## Testing Streaming

### Unit Test Helper
```erlang
mock_stream_test() ->
    TestPid = self(),
    
    % Simulate streaming events
    spawn(fun() ->
        TestPid ! {stream_start, #{}},
        TestPid ! {stream_token, <<"Hello">>},
        TestPid ! {stream_token, <<" world">>},
        TestPid ! {stream_function_call_started, #{<<"name">> => <<"test">>}},
        TestPid ! {stream_complete, #{message => <<"Hello world">>}}
    end),
    
    % Collect and verify
    {Tokens, _, _} = collect_stream(),
    <<"Hello world">> = iolist_to_binary(Tokens).
```

### Integration Test
```erlang
test_real_streaming() ->
    {ok, Agent} = create_test_agent(),
    
    Start = erlang:timestamp(),
    {ok, Response} = safe_stream(Agent, <<"Test message">>),
    Duration = timer:now_diff(erlang:timestamp(), Start) / 1000,
    
    io:format("Streaming took ~p ms~n", [Duration]),
    
    % Verify response
    true = is_map(Response),
    true = maps:is_key(message, Response).
```

## Best Practices Summary

1. **Always handle all event types** - Even if you ignore them
2. **Set appropriate timeouts** - Based on expected response time
3. **Use pattern matching** - For robust event handling
4. **Log errors** - For debugging production issues
5. **Test error scenarios** - Network failures, timeouts
6. **Monitor performance** - Track streaming metrics
7. **Implement graceful degradation** - Handle partial results

## Example: Production-Ready Stream Handler

```erlang
-module(prod_stream_handler).
-export([stream/2]).

-record(state, {
    tokens = [] :: [binary()],
    functions = [] :: [map()],
    errors = [] :: [term()],
    start_time :: integer()
}).

stream(AgentPid, Message) ->
    State = #state{start_time = erlang:system_time(millisecond)},
    
    try
        agent:stream_chat(AgentPid, Message),
        handle_stream(State)
    catch
        Type:Reason:Stack ->
            error_logger:error_msg("Stream failed: ~p:~p~n~p~n", 
                                 [Type, Reason, Stack]),
            {error, {Type, Reason}}
    end.

handle_stream(State) ->
    receive
        {stream_token, Token} ->
            NewState = State#state{
                tokens = [Token | State#state.tokens]
            },
            handle_stream(NewState);
            
        {stream_function_call_started, Call} ->
            NewState = State#state{
                functions = [Call | State#state.functions]
            },
            handle_stream(NewState);
            
        {stream_complete, Response} ->
            Duration = erlang:system_time(millisecond) - State#state.start_time,
            
            {ok, #{
                response => Response,
                tokens => lists:reverse(State#state.tokens),
                functions => lists:reverse(State#state.functions),
                duration_ms => Duration,
                errors => State#state.errors
            }};
            
        {stream_error, Reason} ->
            NewState = State#state{
                errors = [Reason | State#state.errors]
            },
            
            % Continue collecting partial results
            handle_stream(NewState);
            
        Unknown ->
            error_logger:warning_msg("Unknown stream event: ~p~n", [Unknown]),
            handle_stream(State)
            
    after 60000 ->
        % Return partial results on timeout
        {timeout, #{
            tokens => lists:reverse(State#state.tokens),
            functions => lists:reverse(State#state.functions),
            errors => [timeout | State#state.errors]
        }}
    end.
```