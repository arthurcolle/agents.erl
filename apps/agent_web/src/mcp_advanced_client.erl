-module(mcp_advanced_client).
-behaviour(gen_server).

%% API
-export([start_link/2,
         call_tool/4,
         list_tools/1,
         list_resources/1,
         read_resource/2,
         subscribe_resource/2,
         unsubscribe_resource/2,
         complete/2,
         get_connection_status/1,
         get_metrics/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(DEFAULT_TIMEOUT, 30000).
-define(CONNECT_TIMEOUT, 10000).
-define(RETRY_INTERVAL, 5000).
-define(MAX_RETRIES, 3).
-define(CIRCUIT_BREAKER_THRESHOLD, 5).
-define(CIRCUIT_BREAKER_TIMEOUT, 60000).

-record(state, {
    server_id,
    config,
    transport_mod,
    transport_state,
    connection_status = disconnected,
    request_id = 1,
    pending_requests = #{},
    capabilities = #{},
    server_info = #{},
    
    % Advanced features
    metrics = #{
        requests_sent => 0,
        requests_succeeded => 0,
        requests_failed => 0,
        total_latency => 0,
        connection_attempts => 0,
        last_error => undefined
    },
    
    % Circuit breaker
    circuit_breaker = #{
        state => closed,  % closed, open, half_open
        failure_count => 0,
        last_failure_time => 0,
        success_count => 0
    },
    
    % Connection pool
    pool_size = 3,
    connections = [],
    
    % Retry logic
    retry_count = 0,
    retry_timer = undefined,
    
    % Resource subscriptions
    subscriptions = #{}
}).

%% API Functions
start_link(ServerId, Config) ->
    gen_server:start_link(?MODULE, {ServerId, Config}, []).

call_tool(Pid, ToolName, Arguments, Timeout) ->
    case check_circuit_breaker(Pid) of
        open ->
            {error, circuit_breaker_open};
        _ ->
            gen_server:call(Pid, {call_tool, ToolName, Arguments}, Timeout)
    end.

list_tools(Pid) ->
    gen_server:call(Pid, list_tools).

list_resources(Pid) ->
    gen_server:call(Pid, list_resources).

read_resource(Pid, Uri) ->
    gen_server:call(Pid, {read_resource, Uri}).

subscribe_resource(Pid, Uri) ->
    gen_server:call(Pid, {subscribe_resource, Uri}).

unsubscribe_resource(Pid, Uri) ->
    gen_server:call(Pid, {unsubscribe_resource, Uri}).

complete(Pid, Ref) ->
    gen_server:call(Pid, {complete, Ref}).

get_connection_status(Pid) ->
    gen_server:call(Pid, get_connection_status).

get_metrics(Pid) ->
    gen_server:call(Pid, get_metrics).

%% gen_server callbacks
init({ServerId, Config}) ->
    process_flag(trap_exit, true),
    
    State = #state{
        server_id = ServerId,
        config = Config
    },
    
    % Start connection with retry
    self() ! connect,
    
    {ok, State}.

handle_call({call_tool, ToolName, Arguments}, From, State) ->
    case State#state.connection_status of
        connected ->
            RequestId = State#state.request_id,
            Request = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => <<"tools/call">>,
                <<"params">> => #{
                    <<"name">> => ToolName,
                    <<"arguments">> => Arguments
                },
                <<"id">> => RequestId
            },
            
            StartTime = erlang:monotonic_time(millisecond),
            
            case send_request(Request, State) of
                ok ->
                    NewPending = maps:put(RequestId, {From, tool_call, StartTime}, 
                                         State#state.pending_requests),
                    Metrics = update_metrics(request_sent, State#state.metrics),
                    {noreply, State#state{
                        request_id = RequestId + 1,
                        pending_requests = NewPending,
                        metrics = Metrics
                    }};
                {error, Reason} ->
                    Metrics = update_metrics({request_failed, Reason}, State#state.metrics),
                    {reply, {error, Reason}, State#state{metrics = Metrics}}
            end;
        _ ->
            {reply, {error, not_connected}, State}
    end;

handle_call(list_tools, From, State) ->
    case State#state.connection_status of
        connected ->
            RequestId = State#state.request_id,
            Request = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => <<"tools/list">>,
                <<"params">> => #{},
                <<"id">> => RequestId
            },
            
            case send_request(Request, State) of
                ok ->
                    NewPending = maps:put(RequestId, {From, list_tools, erlang:monotonic_time(millisecond)}, 
                                         State#state.pending_requests),
                    {noreply, State#state{
                        request_id = RequestId + 1,
                        pending_requests = NewPending
                    }};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        _ ->
            {reply, {error, not_connected}, State}
    end;

handle_call(get_connection_status, _From, State) ->
    Status = #{
        status => State#state.connection_status,
        server_id => State#state.server_id,
        server_info => State#state.server_info,
        capabilities => State#state.capabilities,
        circuit_breaker => State#state.circuit_breaker
    },
    {reply, Status, State};

handle_call(get_metrics, _From, State) ->
    Metrics = calculate_metrics(State#state.metrics),
    {reply, Metrics, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect, State) ->
    NewState = attempt_connection(State),
    {noreply, NewState};

handle_info({transport_message, Message}, State) ->
    NewState = handle_transport_message(Message, State),
    {noreply, NewState};

handle_info({transport_closed, Reason}, State) ->
    io:format("[MCP_CLIENT] Transport closed: ~p~n", [Reason]),
    NewState = handle_disconnect(State, Reason),
    {noreply, NewState};

handle_info(retry_connection, State) ->
    NewState = State#state{retry_timer = undefined},
    self() ! connect,
    {noreply, NewState};

handle_info(circuit_breaker_timeout, State) ->
    CB = State#state.circuit_breaker,
    NewCB = CB#{
        state => half_open,
        success_count => 0
    },
    io:format("[MCP_CLIENT] Circuit breaker entering half-open state~n"),
    {noreply, State#state{circuit_breaker = NewCB}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Clean up connections
    case State#state.transport_state of
        undefined -> ok;
        TransportState ->
            catch (State#state.transport_mod):disconnect(TransportState)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
attempt_connection(State) ->
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{connection_attempts => maps:get(connection_attempts, Metrics, 0) + 1},
    
    case connect_transport(State#state.config) of
        {ok, TransportMod, TransportState} ->
            io:format("[MCP_CLIENT] Connected to server ~s~n", [State#state.server_id]),
            
            % Send initialize request
            InitRequest = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"method">> => <<"initialize">>,
                <<"params">> => #{
                    <<"protocolVersion">> => <<"1.0">>,
                    <<"capabilities">> => #{
                        <<"tools">> => #{},
                        <<"resources">> => #{<<"subscribe">> => true}
                    },
                    <<"clientInfo">> => #{
                        <<"name">> => <<"erlang_mcp_advanced_client">>,
                        <<"version">> => <<"1.0.0">>
                    }
                },
                <<"id">> => 0
            },
            
            TransportMod:send(TransportState, InitRequest),
            
            % Reset circuit breaker on successful connection
            CB = State#state.circuit_breaker,
            NewCB = CB#{
                state => closed,
                failure_count => 0,
                success_count => 0
            },
            
            State#state{
                transport_mod = TransportMod,
                transport_state = TransportState,
                connection_status = initializing,
                metrics = NewMetrics,
                circuit_breaker = NewCB,
                retry_count = 0
            };
        {error, Reason} ->
            io:format("[MCP_CLIENT] Failed to connect: ~p~n", [Reason]),
            handle_connection_failure(State#state{metrics = NewMetrics}, Reason)
    end.

connect_transport(Config) ->
    Url = maps:get(url, Config),
    
    case determine_transport(Url, Config) of
        {stdio, Command, Args} ->
            mcp_transport_stdio:connect(Command, Args);
        {websocket, WsUrl} ->
            mcp_transport_websocket:connect(WsUrl, Config);
        {sse, SseUrl} ->
            mcp_transport_sse:connect(SseUrl, Config);
        {error, Reason} ->
            {error, Reason}
    end.

determine_transport(Url, Config) when is_binary(Url) ->
    case Url of
        <<"http://", _/binary>> -> {sse, Url};
        <<"https://", _/binary>> -> {sse, Url};
        <<"ws://", _/binary>> -> {websocket, Url};
        <<"wss://", _/binary>> -> {websocket, Url};
        _ ->
            % Check if it's a stdio command
            case maps:find(command, Config) of
                {ok, Command} ->
                    Args = maps:get(args, Config, []),
                    {stdio, Command, Args};
                error ->
                    % Try to parse as command
                    case binary:split(Url, <<" ">>, [global]) of
                        [Command | Args] -> {stdio, Command, Args};
                        _ -> {error, invalid_url}
                    end
            end
    end;
determine_transport(_, _) ->
    {error, invalid_url}.

handle_transport_message(Message, State) ->
    case Message of
        #{<<"id">> := Id} ->
            case maps:find(Id, State#state.pending_requests) of
                {ok, {From, _Type, StartTime}} ->
                    EndTime = erlang:monotonic_time(millisecond),
                    Latency = EndTime - StartTime,
                    
                    % Update metrics
                    Metrics = State#state.metrics,
                    NewMetrics = Metrics#{
                        requests_succeeded => maps:get(requests_succeeded, Metrics, 0) + 1,
                        total_latency => maps:get(total_latency, Metrics, 0) + Latency
                    },
                    
                    % Handle response
                    Response = case Message of
                        #{<<"result">> := Result} -> {ok, Result};
                        #{<<"error">> := Error} -> {error, Error}
                    end,
                    
                    gen_server:reply(From, Response),
                    
                    % Update circuit breaker
                    CB = update_circuit_breaker_success(State#state.circuit_breaker),
                    
                    State#state{
                        pending_requests = maps:remove(Id, State#state.pending_requests),
                        metrics = NewMetrics,
                        circuit_breaker = CB
                    };
                _ when Id =:= 0 ->
                    % Initialize response
                    case Message of
                        #{<<"result">> := Result} ->
                            ServerInfo = maps:get(<<"serverInfo">>, Result, #{}),
                            Capabilities = maps:get(<<"capabilities">>, Result, #{}),
                            
                            io:format("[MCP_CLIENT] Initialized with server: ~p~n", 
                                     [maps:get(<<"name">>, ServerInfo, unknown)]),
                            
                            State#state{
                                connection_status = connected,
                                server_info = ServerInfo,
                                capabilities = Capabilities
                            };
                        _ ->
                            State
                    end;
                error ->
                    State
            end;
        #{<<"method">> := Method} ->
            % Handle notifications
            handle_notification(Method, Message, State);
        _ ->
            State
    end.

handle_notification(<<"resources/updated">>, Message, State) ->
    Uri = maps:get([<<"params">>, <<"uri">>], Message, undefined),
    case maps:find(Uri, State#state.subscriptions) of
        {ok, Subscribers} ->
            % Notify all subscribers
            lists:foreach(fun(Pid) ->
                Pid ! {resource_updated, Uri, Message}
            end, Subscribers);
        error ->
            ok
    end,
    State;

handle_notification(_Method, _Message, State) ->
    State.

handle_disconnect(State, Reason) ->
    % Cancel pending requests
    maps:foreach(fun(_Id, {From, _, _}) ->
        gen_server:reply(From, {error, disconnected})
    end, State#state.pending_requests),
    
    % Update metrics
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{
        last_error => {disconnected, Reason}
    },
    
    % Schedule reconnection
    NewState = State#state{
        connection_status = disconnected,
        transport_state = undefined,
        pending_requests = #{},
        metrics = NewMetrics
    },
    
    schedule_retry(NewState).

handle_connection_failure(State, Reason) ->
    % Update circuit breaker
    CB = update_circuit_breaker_failure(State#state.circuit_breaker),
    
    % Update metrics
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{
        last_error => {connection_failed, Reason}
    },
    
    NewState = State#state{
        circuit_breaker = CB,
        metrics = NewMetrics
    },
    
    case CB of
        #{state := open} ->
            io:format("[MCP_CLIENT] Circuit breaker opened due to failures~n"),
            erlang:send_after(?CIRCUIT_BREAKER_TIMEOUT, self(), circuit_breaker_timeout),
            NewState;
        _ ->
            schedule_retry(NewState)
    end.

schedule_retry(State) ->
    case State#state.retry_count < ?MAX_RETRIES of
        true ->
            Delay = calculate_backoff(State#state.retry_count),
            Timer = erlang:send_after(Delay, self(), retry_connection),
            State#state{
                retry_count = State#state.retry_count + 1,
                retry_timer = Timer
            };
        false ->
            io:format("[MCP_CLIENT] Max retries reached, giving up~n"),
            State
    end.

calculate_backoff(RetryCount) ->
    BaseDelay = 1000,
    MaxDelay = 30000,
    min(BaseDelay * math:pow(2, RetryCount), MaxDelay).

send_request(Request, State) ->
    case State#state.transport_state of
        undefined ->
            {error, not_connected};
        TransportState ->
            (State#state.transport_mod):send(TransportState, Request)
    end.

check_circuit_breaker(Pid) ->
    try
        Status = gen_server:call(Pid, get_connection_status, 100),
        maps:get([circuit_breaker, state], Status, closed)
    catch
        _:_ -> closed
    end.

update_circuit_breaker_success(CB) ->
    case maps:get(state, CB) of
        closed ->
            CB;
        half_open ->
            SuccessCount = maps:get(success_count, CB, 0) + 1,
            if
                SuccessCount >= 3 ->
                    CB#{state => closed, failure_count => 0, success_count => 0};
                true ->
                    CB#{success_count => SuccessCount}
            end;
        open ->
            CB
    end.

update_circuit_breaker_failure(CB) ->
    FailureCount = maps:get(failure_count, CB, 0) + 1,
    Now = erlang:system_time(second),
    
    NewCB = CB#{
        failure_count => FailureCount,
        last_failure_time => Now
    },
    
    case maps:get(state, CB) of
        closed when FailureCount >= ?CIRCUIT_BREAKER_THRESHOLD ->
            NewCB#{state => open};
        half_open ->
            NewCB#{state => open};
        _ ->
            NewCB
    end.

update_metrics(request_sent, Metrics) ->
    Metrics#{requests_sent => maps:get(requests_sent, Metrics, 0) + 1};
update_metrics({request_failed, Reason}, Metrics) ->
    Metrics#{
        requests_failed => maps:get(requests_failed, Metrics, 0) + 1,
        last_error => {request_failed, Reason}
    }.

calculate_metrics(Metrics) ->
    TotalRequests = maps:get(requests_sent, Metrics, 0),
    Succeeded = maps:get(requests_succeeded, Metrics, 0),
    _Failed = maps:get(requests_failed, Metrics, 0),
    TotalLatency = maps:get(total_latency, Metrics, 0),
    
    AvgLatency = case Succeeded of
        0 -> 0;
        _ -> TotalLatency div Succeeded
    end,
    
    SuccessRate = case TotalRequests of
        0 -> 0.0;
        _ -> (Succeeded / TotalRequests) * 100
    end,
    
    Metrics#{
        average_latency_ms => AvgLatency,
        success_rate => SuccessRate,
        total_requests => TotalRequests
    }.