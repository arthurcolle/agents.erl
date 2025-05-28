-module(agent_ws_handler).

-export([init/2]).
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
            case agent_registry:find_agent(binary_to_list(AgentId)) of
                {ok, Pid} ->
                    agent:subscribe(Pid, self()),
                    {reply, {text, jsx:encode(#{status => <<"subscribed">>})}, State#{agent_pid => Pid}};
                {error, not_found} ->
                    {reply, {text, jsx:encode(#{error => <<"Agent not found">>})}, State}
            end;
            
        #{<<"type">> := <<"stream_chat">>, <<"message">> := Message} ->
            case maps:get(agent_pid, State, undefined) of
                undefined ->
                    {reply, {text, jsx:encode(#{error => <<"No agent connected">>})}, State};
                Pid ->
                    spawn(fun() ->
                        agent:stream_chat(Pid, binary_to_list(Message))
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
            case get_agent_metrics(binary_to_list(AgentId)) of
                {ok, Metrics} ->
                    {reply, {text, jsx:encode(#{type => <<"agent_metrics">>, data => Metrics})}, State};
                {error, Reason} ->
                    {reply, {text, jsx:encode(#{error => Reason})}, State}
            end;
            
        _ ->
            {reply, {text, jsx:encode(#{error => <<"Unknown message type">>})}, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({agent_event, Event}, State) ->
    Response = jsx:encode(#{
        type => <<"agent_event">>,
        event => Event
    }),
    {reply, {text, Response}, State};

websocket_info({stream_token, Token}, State) ->
    Response = jsx:encode(#{
        type => <<"stream_token">>,
        token => Token
    }),
    {reply, {text, Response}, State};

websocket_info({stream_complete, Result}, State) ->
    Response = jsx:encode(#{
        type => <<"stream_complete">>,
        result => Result
    }),
    {reply, {text, Response}, State};

websocket_info({example_update, Update}, State) ->
    Response = jsx:encode(#{
        type => <<"example_update">>,
        data => Update
    }),
    {reply, {text, Response}, State};

websocket_info({monitoring_update, Update}, State) ->
    Response = jsx:encode(#{
        type => <<"monitoring_update">>,
        data => Update
    }),
    {reply, {text, Response}, State};

websocket_info({agent_collaboration, Message}, State) ->
    Response = jsx:encode(#{
        type => <<"agent_collaboration">>,
        message => Message
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
                timestamp => erlang:timestamp()
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
            case gen_server:call(Pid, get_state, 5000) of
                State when is_map(State) ->
                    {ok, State};
                _ ->
                    {error, <<"Failed to get agent state">>}
            end;
        {error, not_found} ->
            {error, <<"Agent not found">>}
    end.