# Streaming Function Calls API Reference

## Module: streaming_function_handler

### Types

```erlang
-type event_type() :: content_delta | tool_call_start | tool_call_delta | 
                     tool_call_complete | stream_done | error.

-type stream_event() :: #{
    type => event_type(),
    data => term(),
    index => non_neg_integer() | undefined,
    metadata => map()
}.

-type accumulator() :: #accumulator{
    content :: binary(),
    tool_calls :: [tool_call()],
    tool_call_map :: #{non_neg_integer() => tool_call()},
    metadata :: map()
}.

-type tool_call() :: #{
    <<"id">> => binary(),
    <<"type">> => binary(),
    <<"function">> => #{
        <<"name">> => binary(),
        <<"arguments">> => binary()
    }
}.

-type stream_result() :: {continue, accumulator()} | 
                        {done, accumulator()} | 
                        {error, term()}.
```

### Exported Functions

#### init_accumulator/0

```erlang
-spec init_accumulator() -> accumulator().
```

Creates a new accumulator for streaming operations.

**Example:**
```erlang
Acc = streaming_function_handler:init_accumulator().
```

---

#### process_stream_event/3

```erlang
-spec process_stream_event(Event :: stream_event() | map(), 
                          Accumulator :: accumulator(), 
                          StreamPid :: pid()) -> stream_result().
```

Processes a single streaming event and updates the accumulator.

**Parameters:**
- `Event` - The streaming event to process
- `Accumulator` - Current accumulator state  
- `StreamPid` - Process to receive streaming notifications

**Returns:**
- `{continue, NewAccumulator}` - Continue streaming with updated state
- `{done, FinalAccumulator}` - Streaming is complete
- `{error, Reason}` - An error occurred

**Example:**
```erlang
Event = #{type => content, data => <<"Hello">>},
{continue, NewAcc} = streaming_function_handler:process_stream_event(
    Event, Acc, self()
).
```

---

#### finalize_stream/3

```erlang
-spec finalize_stream(Accumulator :: accumulator(),
                     StreamPid :: pid(), 
                     State :: term()) -> 
    {ok, Response :: map(), NewState :: term()} | 
    {error, Reason :: term()}.
```

Finalizes the stream, executes accumulated tool calls, and returns the complete response.

**Parameters:**
- `Accumulator` - Final accumulator with all streamed data
- `StreamPid` - Process to receive final notifications
- `State` - Agent state (passed through)

**Returns:**
- `{ok, Response, NewState}` - Success with complete response
- `{error, Reason}` - An error occurred

**Example:**
```erlang
{ok, Response, NewState} = streaming_function_handler:finalize_stream(
    FinalAcc, self(), AgentState
).
```

---

#### handle_responses_api_stream/3

```erlang
-spec handle_responses_api_stream(StreamPid :: pid(), 
                                 Input :: term(), 
                                 State :: term()) -> 
    {ok, Response :: map(), NewState :: term()} | 
    {error, Reason :: term()}.
```

High-level handler for OpenAI Responses API streaming.

**Parameters:**
- `StreamPid` - Process to receive streaming events
- `Input` - Input messages/context for the API
- `State` - Agent state

**Example:**
```erlang
{ok, Response, NewState} = streaming_function_handler:handle_responses_api_stream(
    self(), Input, AgentState
).
```

---

#### handle_chat_api_stream/3

```erlang
-spec handle_chat_api_stream(StreamPid :: pid(), 
                            Messages :: [map()], 
                            State :: term()) -> 
    {ok, Response :: map(), NewState :: term()} | 
    {error, Reason :: term()}.
```

High-level handler for OpenAI Chat API streaming.

**Parameters:**
- `StreamPid` - Process to receive streaming events
- `Messages` - Chat messages array
- `State` - Agent state

**Example:**
```erlang
Messages = [
    #{role => <<"system">>, content => <<"You are helpful">>},
    #{role => <<"user">>, content => <<"Hello">>}
],
{ok, Response, NewState} = streaming_function_handler:handle_chat_api_stream(
    self(), Messages, AgentState
).
```

---

## Module: agent

### Streaming Functions

#### stream_chat/2

```erlang
-spec stream_chat(Pid :: pid(), Message :: binary()) -> ok.
```

Initiates a streaming chat with an agent. Events are sent to the calling process.

**Parameters:**
- `Pid` - Agent process ID
- `Message` - User message

**Example:**
```erlang
agent:stream_chat(AgentPid, <<"What's the weather?">>).
```

---

#### stream_chat/3

```erlang
-spec stream_chat(Pid :: pid(), 
                 Message :: binary(), 
                 SubscriberPid :: pid()) -> ok.
```

Initiates a streaming chat with custom subscriber.

**Parameters:**
- `Pid` - Agent process ID
- `Message` - User message
- `SubscriberPid` - Process to receive streaming events

**Example:**
```erlang
HandlerPid = spawn(fun() -> handle_stream_events() end),
agent:stream_chat(AgentPid, <<"Calculate something">>, HandlerPid).
```

---

## Module: agent_instance

### Internal Streaming Functions

#### process_streaming_chat/3

```erlang
-spec process_streaming_chat(Message :: binary(), 
                           StreamPid :: pid(), 
                           State :: #state{}) -> 
    {ok, Response :: map(), NewState :: #state{}} | 
    {error, Reason :: term()}.
```

Internal function that processes streaming chat based on API preference.

---

#### handle_streaming_response/3

```erlang
-spec handle_streaming_response(StreamPid :: pid(), 
                              Messages :: [map()], 
                              State :: #state{}) -> 
    {ok, Response :: map(), NewState :: #state{}} | 
    {error, Reason :: term()}.
```

Handles streaming response for Chat API, including function call execution.

---

#### handle_streaming_response_events/3

```erlang
-spec handle_streaming_response_events(StreamPid :: pid(), 
                                     Input :: term(), 
                                     State :: #state{}) -> 
    {ok, Response :: map(), NewState :: #state{}} | 
    {error, Reason :: term()}.
```

Handles streaming events for Responses API.

---

## Streaming Event Types

### Client-Facing Events

Events sent to the stream subscriber:

#### stream_start
```erlang
{stream_start, #{agent_id => AgentId :: binary()}}
```
Indicates streaming has begun.

#### stream_token  
```erlang
{stream_token, Token :: binary()}
```
A text token from the AI response.

#### stream_function_call_started
```erlang
{stream_function_call_started, FunctionCall :: map()}
```
A function call has been initiated.

#### stream_function_arguments_delta
```erlang
{stream_function_arguments_delta, Index :: non_neg_integer(), Delta :: binary()}
```
Partial function arguments being streamed.

#### stream_function_call_complete
```erlang
{stream_function_call_complete, FunctionCall :: map()}
```
Function call arguments are complete.

#### stream_function_execution_start
```erlang
{stream_function_execution_start}
```
Tool execution has begun.

#### stream_tool_result
```erlang
{stream_tool_result, #{
    tool_call => ToolCall :: map(),
    result => Result :: term()
}}
```
Result from tool execution.

#### stream_complete
```erlang
{stream_complete, Response :: map()}
```
Streaming is complete with final response.

#### stream_error
```erlang
{stream_error, Reason :: term()}
```
An error occurred during streaming.

---

## Usage Patterns

### Basic Streaming Pattern

```erlang
stream_basic(AgentPid, Message) ->
    agent:stream_chat(AgentPid, Message),
    collect_stream().

collect_stream() ->
    collect_stream([]).

collect_stream(Tokens) ->
    receive
        {stream_token, Token} ->
            io:format("~s", [Token]),
            collect_stream([Token | Tokens]);
            
        {stream_complete, Response} ->
            {ok, lists:reverse(Tokens), Response};
            
        {stream_error, Reason} ->
            {error, Reason}
            
    after 30000 ->
        {error, timeout}
    end.
```

### Advanced Pattern with Function Handling

```erlang
stream_with_functions(AgentPid, Message) ->
    Self = self(),
    Collector = spawn_link(fun() -> 
        advanced_collector(Self, #{
            tokens => [],
            functions => [],
            results => []
        })
    end),
    
    agent:stream_chat(AgentPid, Message, Collector),
    
    receive
        {stream_result, Result} -> Result
    after 60000 ->
        {error, timeout}
    end.

advanced_collector(Parent, State) ->
    receive
        {stream_token, Token} ->
            NewState = State#{
                tokens => [Token | maps:get(tokens, State)]
            },
            advanced_collector(Parent, NewState);
            
        {stream_function_call_started, FuncCall} ->
            NewState = State#{
                functions => [FuncCall | maps:get(functions, State)]
            },
            advanced_collector(Parent, NewState);
            
        {stream_tool_result, Result} ->
            NewState = State#{
                results => [Result | maps:get(results, State)]
            },
            advanced_collector(Parent, NewState);
            
        {stream_complete, Response} ->
            Parent ! {stream_result, {ok, State, Response}};
            
        {stream_error, Reason} ->
            Parent ! {stream_result, {error, Reason}}
    end.
```

### Async Pattern with GenServer

```erlang
-module(stream_handler).
-behaviour(gen_server).

-export([start_link/0, stream_message/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stream_message(Pid, Message) ->
    gen_server:call(Pid, {stream, Message}).

init([]) ->
    {ok, AgentPid} = agent_supervisor:start_agent(#{
        name => <<"Stream Handler Agent">>,
        tools => [web_search, calculator]
    }),
    {ok, #{agent => AgentPid, streams => #{}}}.

handle_call({stream, Message}, From, State = #{agent := AgentPid}) ->
    StreamId = make_ref(),
    agent:stream_chat(AgentPid, Message, self()),
    
    NewState = State#{
        streams => maps:put(StreamId, #{
            from => From,
            tokens => [],
            start_time => erlang:system_time(millisecond)
        }, maps:get(streams, State))
    },
    
    {noreply, NewState}.

handle_info({stream_token, Token}, State) ->
    % Update current stream with token
    {noreply, update_stream_tokens(Token, State)};

handle_info({stream_complete, Response}, State) ->
    % Complete the stream and reply to caller
    {noreply, complete_stream(Response, State)};

handle_info({stream_error, Reason}, State) ->
    % Handle error and notify caller
    {noreply, error_stream(Reason, State)}.
```

---

## Configuration Options

### Stream Configuration

```erlang
%% In sys.config or application environment
{agents, [
    {streaming, [
        {buffer_size, 4096},           % Chunk buffer size
        {timeout, 30000},              % Timeout per chunk (ms)
        {tool_timeout, 30000},         % Timeout per tool (ms)
        {max_tool_calls, 10},          % Max parallel tools
        {enable_compression, false},    % Response compression
        {debug_streaming, false}       % Debug logging
    ]}
]}
```

### Per-Agent Configuration

```erlang
%% When creating an agent
{ok, AgentPid} = agent_supervisor:start_agent(#{
    name => <<"Streaming Agent">>,
    model => <<"gpt-4o">>,
    tools => [web_search, file_ops],
    streaming_config => #{
        chunk_timeout => 45000,
        parallel_tools => true,
        max_retries => 3
    }
}).
```

---

## Error Handling

### Error Types

```erlang
-type stream_error() :: 
    timeout |                           % Stream timeout
    {parse_error, binary()} |          % JSON parse error
    {tool_error, ToolId :: binary(), Reason :: term()} | % Tool execution error
    {api_error, Code :: integer(), Message :: binary()} | % API error
    {network_error, inet:posix()} |    % Network error
    connection_closed.                  % Unexpected close
```

### Error Recovery Example

```erlang
robust_stream(AgentPid, Message) ->
    robust_stream(AgentPid, Message, 3).

robust_stream(_AgentPid, _Message, 0) ->
    {error, max_retries_exceeded};
robust_stream(AgentPid, Message, Retries) ->
    try
        agent:stream_chat(AgentPid, Message),
        collect_with_recovery()
    catch
        error:{stream_error, {network_error, _}} ->
            timer:sleep(1000 * (4 - Retries)),
            robust_stream(AgentPid, Message, Retries - 1);
        error:Reason ->
            {error, Reason}
    end.

collect_with_recovery() ->
    collect_with_recovery([], erlang:system_time(millisecond)).

collect_with_recovery(Tokens, StartTime) ->
    Timeout = max(1000, 30000 - (erlang:system_time(millisecond) - StartTime)),
    receive
        {stream_token, Token} ->
            collect_with_recovery([Token | Tokens], StartTime);
            
        {stream_complete, Response} ->
            {ok, lists:reverse(Tokens), Response};
            
        {stream_error, Reason} ->
            % Return partial results if any
            case Tokens of
                [] -> {error, Reason};
                _ -> {partial, lists:reverse(Tokens), Reason}
            end
    after Timeout ->
        {timeout, lists:reverse(Tokens)}
    end.
```

---

## Performance Metrics

### Tracking Stream Performance

```erlang
-record(stream_metrics, {
    stream_id :: reference(),
    start_time :: integer(),
    first_token_time :: integer() | undefined,
    end_time :: integer() | undefined,
    token_count = 0 :: non_neg_integer(),
    byte_count = 0 :: non_neg_integer(),
    function_count = 0 :: non_neg_integer(),
    tool_execution_time = 0 :: non_neg_integer(),
    errors = [] :: [term()]
}).

measure_stream_performance(AgentPid, Message) ->
    Metrics = #stream_metrics{
        stream_id = make_ref(),
        start_time = erlang:system_time(millisecond)
    },
    
    Self = self(),
    MetricsPid = spawn_link(fun() -> 
        metrics_collector(Self, Metrics) 
    end),
    
    agent:stream_chat(AgentPid, Message, MetricsPid),
    
    receive
        {metrics, FinalMetrics} -> 
            analyze_metrics(FinalMetrics)
    after 60000 ->
        {error, timeout}
    end.

analyze_metrics(#stream_metrics{} = M) ->
    TotalTime = M#stream_metrics.end_time - M#stream_metrics.start_time,
    TimeToFirstToken = case M#stream_metrics.first_token_time of
        undefined -> undefined;
        T -> T - M#stream_metrics.start_time
    end,
    
    #{
        total_time_ms => TotalTime,
        time_to_first_token_ms => TimeToFirstToken,
        tokens_per_second => (M#stream_metrics.token_count * 1000) / TotalTime,
        bytes_per_second => (M#stream_metrics.byte_count * 1000) / TotalTime,
        function_calls => M#stream_metrics.function_count,
        tool_time_ms => M#stream_metrics.tool_execution_time,
        errors => M#stream_metrics.errors
    }.
```

---

## Integration Examples

### Web Handler Integration

```erlang
%% Cowboy WebSocket handler for streaming
-module(ws_stream_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req, _Opts) ->
    {cowboy_websocket, Req, #{}}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Message}, State) ->
    Request = jsx:decode(Message, [return_maps]),
    case maps:get(<<"action">>, Request) of
        <<"stream_chat">> ->
            AgentId = maps:get(<<"agent_id">>, Request),
            UserMessage = maps:get(<<"message">>, Request),
            
            case agent_registry:find_agent(AgentId) of
                {ok, AgentPid} ->
                    agent:stream_chat(AgentPid, UserMessage, self()),
                    {ok, State};
                _ ->
                    {reply, {text, jsx:encode(#{
                        error => <<"Agent not found">>
                    })}, State}
            end
    end.

websocket_info({stream_token, Token}, State) ->
    {reply, {text, jsx:encode(#{
        type => <<"token">>,
        data => Token
    })}, State};

websocket_info({stream_function_call_started, FuncCall}, State) ->
    {reply, {text, jsx:encode(#{
        type => <<"function_start">>,
        data => FuncCall
    })}, State};

websocket_info({stream_complete, Response}, State) ->
    {reply, {text, jsx:encode(#{
        type => <<"complete">>,
        data => Response
    })}, State}.
```

### REST API Integration

```erlang
%% REST endpoint for streaming with SSE
-module(sse_stream_handler).
-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:stream_reply(200, #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>
    }, Req0),
    
    AgentId = cowboy_req:binding(agent_id, Req0),
    Message = cowboy_req:binding(message, Req0),
    
    case agent_registry:find_agent(AgentId) of
        {ok, AgentPid} ->
            agent:stream_chat(AgentPid, Message, self()),
            stream_loop(Req);
        _ ->
            cowboy_req:stream_body(
                <<"event: error\ndata: Agent not found\n\n">>, 
                fin, Req
            )
    end,
    
    {ok, Req, State}.

stream_loop(Req) ->
    receive
        {stream_token, Token} ->
            Event = [
                <<"event: token\n">>,
                <<"data: ">>, jsx:encode(#{text => Token}), <<"\n\n">>
            ],
            cowboy_req:stream_body(iolist_to_binary(Event), nofin, Req),
            stream_loop(Req);
            
        {stream_complete, Response} ->
            Event = [
                <<"event: complete\n">>,
                <<"data: ">>, jsx:encode(Response), <<"\n\n">>
            ],
            cowboy_req:stream_body(iolist_to_binary(Event), fin, Req);
            
        {stream_error, Reason} ->
            Event = [
                <<"event: error\n">>,
                <<"data: ">>, jsx:encode(#{error => Reason}), <<"\n\n">>
            ],
            cowboy_req:stream_body(iolist_to_binary(Event), fin, Req)
            
    after 30000 ->
        cowboy_req:stream_body(
            <<"event: timeout\ndata: {}\n\n">>, 
            fin, Req
        )
    end.
```