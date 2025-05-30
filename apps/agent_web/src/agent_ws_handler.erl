-module(agent_ws_handler).

-export([init/2, broadcast/1]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(_State) ->
    {ok, #{}}.

websocket_handle({text, Msg}, State) ->
    case jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"create_stream">>, <<"agent_id">> := AgentId} ->
            NormalizedId = normalize_agent_id(AgentId),
            case agent_registry:find_agent(NormalizedId) of
                {ok, Pid} ->
                    agent:subscribe(Pid, self()),
                    {reply, {text, jsx:encode(#{status => <<"subscribed">>})}, State#{agent_pid => Pid}};
                {error, agent_not_found} ->
                    {reply, {text, jsx:encode(#{error => <<"Agent not found">>})}, State}
            end;
            
        #{<<"type">> := <<"stream_chat">>, <<"message">> := Message} ->
            case maps:get(agent_pid, State, undefined) of
                undefined ->
                    {reply, {text, jsx:encode(#{error => <<"No agent connected">>})}, State};
                Pid ->
                    MessageStr = case Message of
                        B when is_binary(B) -> binary_to_list(B);
                        L when is_list(L) -> L;
                        _ -> io_lib:format("~p", [Message])
                    end,
                    spawn(fun() ->
                        case catch agent:stream_chat(Pid, MessageStr) of
                            ok -> ok;
                            Error -> 
                                self() ! {stream_error, Error}
                        end
                    end),
                    {ok, State}
            end;
            
        #{<<"type">> := <<"run_example">>, <<"category">> := Category, <<"name">> := Name} ->
            spawn(fun() ->
                run_streaming_example(Category, Name, self())
            end),
            {ok, State};
            
        #{<<"type">> := <<"subscribe_monitoring">>} ->
            % Subscribe to real-time monitoring updates
            case monitor_agents(self()) of
                ok ->
                    {reply, {text, jsx:encode(#{status => <<"monitoring_started">>})}, State#{monitoring => true}};
                Error ->
                    {reply, {text, jsx:encode(#{error => Error})}, State}
            end;
            
        #{<<"type">> := <<"get_system_metrics">>} ->
            Metrics = get_system_metrics(),
            {reply, {text, jsx:encode(#{type => <<"system_metrics">>, data => Metrics})}, State};
            
        #{<<"type">> := <<"get_agent_metrics">>, <<"agent_id">> := AgentId} ->
            NormalizedId = normalize_agent_id(AgentId),
            case get_agent_metrics(NormalizedId) of
                {ok, Metrics} ->
                    {reply, {text, jsx:encode(#{type => <<"agent_metrics">>, data => Metrics})}, State};
                {error, Reason} ->
                    {reply, {text, jsx:encode(#{error => Reason})}, State}
            end;
            
        #{<<"type">> := <<"client_log">>, <<"level">> := Level, <<"message">> := Message, <<"timestamp">> := Timestamp} = MsgData ->
            % Log client-side events to server
            UserAgent = maps:get(<<"user_agent">>, MsgData, <<"Unknown">>),
            error_logger:info_msg("CLIENT LOG [~s] [~s] ~s (User-Agent: ~s)~n", 
                                [Timestamp, Level, Message, UserAgent]),
            {ok, State};
            
        #{<<"type">> := <<"button_click">>, <<"data">> := ClickData} ->
            % Log button clicks to server
            Button = maps:get(<<"button">>, ClickData, <<"unknown">>),
            Context = maps:get(<<"context">>, ClickData, #{}),
            Timestamp = maps:get(<<"timestamp">>, ClickData, <<"unknown">>),
            error_logger:info_msg("BUTTON CLICK [~s] ~s ~s~n", 
                                [Timestamp, Button, jsx:encode(Context)]),
            {ok, State};
            
        _ ->
            {reply, {text, jsx:encode(#{error => <<"Unknown message type">>})}, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({agent_event, Event}, State) ->
    SafeEvent = ensure_json_safe(Event),
    Response = jsx:encode(#{
        type => <<"agent_event">>,
        event => SafeEvent
    }),
    {reply, {text, Response}, State};

websocket_info({stream_token, Token}, State) ->
    SafeToken = ensure_json_safe(Token),
    Response = jsx:encode(#{
        type => <<"stream_token">>,
        token => SafeToken
    }),
    {reply, {text, Response}, State};

websocket_info({stream_complete, Result}, State) ->
    SafeResult = ensure_json_safe(Result),
    Response = jsx:encode(#{
        type => <<"stream_complete">>,
        result => SafeResult
    }),
    {reply, {text, Response}, State};

websocket_info({example_update, Update}, State) ->
    SafeUpdate = ensure_json_safe(Update),
    Response = jsx:encode(#{
        type => <<"example_update">>,
        data => SafeUpdate
    }),
    {reply, {text, Response}, State};

websocket_info({monitoring_update, Update}, State) ->
    SafeUpdate = ensure_json_safe(Update),
    Response = jsx:encode(#{
        type => <<"monitoring_update">>,
        data => SafeUpdate
    }),
    {reply, {text, Response}, State};

websocket_info({agent_collaboration, Message}, State) ->
    SafeMessage = ensure_json_safe(Message),
    Response = jsx:encode(#{
        type => <<"agent_collaboration">>,
        message => SafeMessage
    }),
    {reply, {text, Response}, State};

websocket_info({stream_error, Error}, State) ->
    Response = jsx:encode(#{
        type => <<"stream_error">>,
        error => ensure_json_safe(Error)
    }),
    {reply, {text, Response}, State};

websocket_info(_Info, State) ->
    {ok, State}.

run_streaming_example(<<"streaming">>, <<"pipeline">>, WsPid) ->
    StreamFun = fun(Data) ->
        WsPid ! {example_update, #{stage => <<"processing">>, data => Data}}
    end,
    Result = advanced_streaming_async:streaming_pipeline_with_callback(StreamFun),
    WsPid ! {example_update, #{stage => <<"complete">>, result => Result}};

run_streaming_example(_, _, WsPid) ->
    WsPid ! {example_update, #{error => <<"Unknown streaming example">>}}.

%% Monitoring functions
monitor_agents(WsPid) ->
    % Start periodic monitoring
    spawn(fun() -> monitoring_loop(WsPid) end),
    ok.

monitoring_loop(WsPid) ->
    timer:sleep(1000), % Update every second
    case process_info(WsPid) of
        undefined -> ok; % WebSocket closed
        _ ->
            Agents = agent_registry:list_agents(),
            Updates = lists:map(fun({Id, Pid, Meta}) ->
                #{
                    id => list_to_binary(Id),
                    pid => list_to_binary(pid_to_list(Pid)),
                    status => get_agent_status(Pid),
                    memory => get_process_memory(Pid),
                    message_queue => get_message_queue_len(Pid),
                    meta => Meta
                }
            end, Agents),
            
            SystemMetrics = get_system_metrics(),
            
            WsPid ! {monitoring_update, #{
                agents => Updates,
                system => SystemMetrics,
                timestamp => os:system_time(millisecond)
            }},
            
            monitoring_loop(WsPid)
    end.

get_agent_status(Pid) ->
    case process_info(Pid) of
        undefined -> <<"dead">>;
        _ -> <<"alive">>
    end.

get_process_memory(Pid) ->
    case process_info(Pid, memory) of
        {memory, Mem} -> Mem;
        _ -> 0
    end.

get_message_queue_len(Pid) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, Len} -> Len;
        _ -> 0
    end.

get_system_metrics() ->
    #{
        node => node(),
        uptime => erlang:statistics(wall_clock),
        total_memory => erlang:memory(total),
        process_count => erlang:system_info(process_count),
        run_queue => erlang:statistics(run_queue),
        schedulers => erlang:system_info(schedulers_online)
    }.

get_agent_metrics(AgentId) ->
    case agent_registry:find_agent(AgentId) of
        {ok, Pid} ->
            case catch gen_server:call(Pid, get_state, 5000) of
                State when is_map(State) ->
                    {ok, State};
                _ ->
                    % If get_state fails, provide basic metrics
                    BasicMetrics = #{
                        id => AgentId,
                        pid => list_to_binary(pid_to_list(Pid)),
                        status => <<"active">>,
                        uptime => erlang:system_time(millisecond)
                    },
                    {ok, BasicMetrics}
            end;
        {error, agent_not_found} ->
            {error, <<"Agent not found">>}
    end.

%% Helper functions
normalize_agent_id(Id) when is_binary(Id) -> Id;
normalize_agent_id(Id) when is_list(Id) -> list_to_binary(Id);
normalize_agent_id(Id) when is_atom(Id) -> atom_to_binary(Id, utf8);
normalize_agent_id(Id) -> list_to_binary(io_lib:format("~p", [Id])).

%% JSON safety function to handle invalid data types
ensure_json_safe(Data) when is_binary(Data) -> Data;
ensure_json_safe(Data) when is_number(Data) -> Data;
ensure_json_safe(Data) when is_boolean(Data) -> Data;
ensure_json_safe(Data) when is_atom(Data) ->
    case Data of
        null -> null;
        undefined -> null;
        true -> true;
        false -> false;
        _ -> atom_to_binary(Data, utf8)
    end;
ensure_json_safe(Data) when is_pid(Data) ->
    list_to_binary(pid_to_list(Data));
ensure_json_safe(Data) when is_reference(Data) ->
    list_to_binary(ref_to_list(Data));
ensure_json_safe(Data) when is_port(Data) ->
    list_to_binary(port_to_list(Data));
ensure_json_safe(Data) when is_function(Data) ->
    <<"#Fun">>;
ensure_json_safe(Data) when is_tuple(Data) ->
    ensure_json_safe(tuple_to_list(Data));
ensure_json_safe(Data) when is_list(Data) ->
    try
        lists:map(fun ensure_json_safe/1, Data)
    catch
        _:_ -> <<"#List">>
    end;
ensure_json_safe(Data) when is_map(Data) ->
    try
        maps:fold(fun(K, V, Acc) ->
            SafeK = ensure_json_safe(K),
            SafeV = ensure_json_safe(V),
            Acc#{SafeK => SafeV}
        end, #{}, Data)
    catch
        _:_ -> #{error => <<"invalid_map">>}
    end;
ensure_json_safe(_Data) ->
    <<"#Unknown">>.

%% Broadcast function for notifying all connected websocket clients
broadcast(Message) ->
    io:format("[WS] Broadcasting message: ~p~n", [Message]),
    %% For now, just log the broadcast - in a full implementation,
    %% this would maintain a registry of connected websocket processes
    %% and send the message to all of them
    ok.