-module(agent_ws_handler).

-export([init/2, broadcast/1]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(_State) ->
    % Start periodic system metrics updates with error handling
    try
        erlang:send_after(10000, self(), send_system_metrics), % Changed from 5s to 10s
        {ok, #{}}
    catch
        _:Error ->
            error_logger:error_msg("WebSocket init error: ~p~n", [Error]),
            {ok, #{}}
    end.

websocket_handle({text, Msg}, State) ->
    try jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"create_stream">>, <<"agent_id">> := AgentId} ->
            NormalizedId = normalize_agent_id(AgentId),
            case agent_registry:find_agent(NormalizedId) of
                {ok, Pid} ->
                    agent:subscribe(Pid, self()),
                    % Log conversation start
                    ConversationId = generate_conversation_id(),
                    conversation_stats_logger:log_conversation_start(ConversationId, NormalizedId),
                    {reply, {text, jsx:encode(#{status => <<"subscribed">>})}, 
                     State#{agent_pid => Pid, conversation_id => ConversationId, agent_id => NormalizedId}};
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
                    
                    % Enhanced logging for streaming chat
                    colored_logger:data(processed, io_lib:format("[WS_CHAT] 💬 Starting stream chat with agent ~p", [Pid])),
                    colored_logger:data(processed, io_lib:format("[WS_CHAT] Message: ~s", [MessageStr])),
                    
                    % Log user message
                    ConversationId = maps:get(conversation_id, State, <<"unknown">>),
                    AgentId = maps:get(agent_id, State, <<"unknown">>),
                    conversation_stats_logger:log_message(ConversationId, AgentId, <<"user">>, Message),
                    
                    % Use the new stream_chat/3 function to send updates to this websocket process
                    WsPid = self(),
                    spawn(fun() ->
                        colored_logger:ocean(surface, io_lib:format("[WS_CHAT] 🚀 Spawned streaming worker for WS ~p", [WsPid])),
                        case catch agent:stream_chat(Pid, MessageStr, WsPid) of
                            ok -> 
                                colored_logger:complete(success, "[WS_CHAT] ✅ Stream chat completed successfully");
                            Error -> 
                                colored_logger:fire(inferno, io_lib:format("[WS_CHAT] ❌ Stream chat error: ~p", [Error])),
                                WsPid ! {stream_error, Error}
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
                {error, Reason} ->
                    {reply, {text, jsx:encode(#{error => Reason})}, State}
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
            % Enhanced button click logging
            Button = maps:get(<<"button">>, ClickData, <<"unknown">>),
            Context = maps:get(<<"context">>, ClickData, #{}),
            Timestamp = maps:get(<<"timestamp">>, ClickData, <<"unknown">>),
            Action = maps:get(<<"action">>, ClickData, <<"click">>),
            
            % Log via interaction logger for comprehensive tracking
            interaction_logger:log_button_click(Action, ClickData),
            
            % Direct console output for scripts/start_simple.sh visibility
            io:format("🖱️  WS BUTTON: ~s | ACTION: ~s | TIME: ~s~n", [Button, Action, Timestamp]),
            
            {ok, State};
            
        #{<<"type">> := <<"log_interaction">>} = LogData ->
            % Generic interaction logging via WebSocket
            interaction_logger:log_interaction(LogData),
            {ok, State};
            
        #{<<"type">> := <<"log_error">>} = ErrorData ->
            % Error logging via WebSocket with AI processing
            Message = maps:get(<<"message">>, ErrorData, <<"unknown error">>),
            Context = maps:get(<<"context">>, ErrorData, <<"websocket">>),
            interaction_logger:log_error(ErrorData, Context),
            
            % Process error with AI for dynamic analysis
            ai_error_processor:process_websocket_error(ErrorData, <<"client_error">>),
            
            % Direct console output for scripts/start_simple.sh visibility
            io:format("🔴 WS ERROR: ~s | CONTEXT: ~s~n", [Message, Context]),
            
            {ok, State};
            
        #{<<"type">> := <<"ping">>} = PingData ->
            % Handle ping messages from frontend
            {reply, {text, jsx:encode(#{type => <<"pong">>, timestamp => os:system_time(millisecond)})}, State};
            
        Other ->
            % ENHANCED LOGGING: Log the exact unhandled message type and structure
            MsgType = maps:get(<<"type">>, Other, "no_type_field"),
            error_logger:warning_msg("[WS_UNHANDLED] Unhandled websocket message - Type: ~p, Full message: ~p~n", [MsgType, Other]),
            io:format("🔴 UNHANDLED WEBSOCKET MESSAGE:\n  Type: ~p\n  Full: ~p\n", [MsgType, Other]),
            {ok, State}
    catch
        _:DecodeError ->
            error_logger:error_msg("WebSocket decode error: ~p for message: ~p~n", [DecodeError, Msg]),
            {reply, {text, jsx:encode(#{error => <<"Invalid JSON">>})}, State}
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

websocket_info({stream_start, Info}, State) ->
    SafeInfo = ensure_json_safe(Info),
    Response = jsx:encode(#{
        type => <<"chat_response_start">>,
        info => SafeInfo
    }),
    {reply, {text, Response}, State};

websocket_info({stream_token, Token}, State) ->
    % CRITICAL: Process token to ensure we send readable text, NOT binary bytes!
    ProcessedToken = case catch streaming_function_handler:process_token_for_display(Token) of
        {'EXIT', _} -> ensure_json_safe(Token);  % Fallback if function not available
        DisplayToken -> ensure_json_safe(DisplayToken)
    end,
    % Enhanced logging for token streaming
    try
        colored_logger:ocean(surface, io_lib:format("[WS_STREAM] 🌊 Sending token: ~p", [ProcessedToken]))
    catch
        _:_ -> io:format("[WS_STREAM] 🌊 Sending token: ~p~n", [ProcessedToken])
    end,
    Response = jsx:encode(#{
        type => <<"chat_response_token">>,
        token => ProcessedToken
    }),
    % Log the actual JSON being sent
    io:format("[WS_STREAM] Sending JSON: ~s~n", [Response]),
    {reply, {text, Response}, State};

websocket_info({stream_complete, Result}, State) ->
    SafeResult = ensure_json_safe(Result),
    
    % Log agent response
    ConversationId = maps:get(conversation_id, State, <<"unknown">>),
    AgentId = maps:get(agent_id, State, <<"unknown">>),
    conversation_stats_logger:log_message(ConversationId, AgentId, <<"agent">>, SafeResult),
    
    Response = jsx:encode(#{
        type => <<"chat_response_complete">>,
        content => SafeResult
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
    % Process error with AI for dynamic analysis
    ai_error_processor:process_websocket_error(#{
        <<"error">> => ensure_json_safe(Error),
        <<"handler">> => <<"agent_ws_handler">>,
        <<"function">> => <<"websocket_info">>,
        <<"timestamp">> => erlang:system_time(second)
    }, <<"websocket_stream_error">>),
    
    Response = jsx:encode(#{
        type => <<"stream_error">>,
        error => ensure_json_safe(Error)
    }),
    {reply, {text, Response}, State};

websocket_info({system_health, HealthData}, State) ->
    Response = jsx:encode(#{
        type => <<"system_health">>,
        data => ensure_json_safe(HealthData)
    }),
    {reply, {text, Response}, State};

websocket_info({system_error, ErrorData}, State) ->
    % Process system error with AI for dynamic analysis
    ai_error_processor:process_websocket_error(#{
        <<"error_data">> => ensure_json_safe(ErrorData),
        <<"handler">> => <<"agent_ws_handler">>,
        <<"function">> => <<"websocket_info">>,
        <<"timestamp">> => erlang:system_time(second),
        <<"error_type">> => <<"system_error">>
    }, <<"websocket_system_error">>),
    
    Response = jsx:encode(#{
        type => <<"system_error">>,
        data => ensure_json_safe(ErrorData)
    }),
    {reply, {text, Response}, State};

websocket_info({learning_event, Event}, State) ->
    % Forward learning protocol events to the UI
    SafeEvent = ensure_json_safe(Event),
    Response = jsx:encode(#{
        type => <<"learning_event">>,
        event => SafeEvent
    }),
    {reply, {text, Response}, State};

websocket_info(send_system_metrics, State) ->
    Metrics = get_system_metrics(),
    Response = jsx:encode(#{
        type => <<"system_metrics">>,
        data => Metrics
    }),
    % Schedule next update in 10 seconds
    erlang:send_after(10000, self(), send_system_metrics),
    {reply, {text, Response}, State};

websocket_info(Info, State) ->
    % ENHANCED LOGGING: Log all unhandled websocket_info messages
    error_logger:warning_msg("[WS_INFO_UNHANDLED] Unhandled websocket_info message: ~p~n", [Info]),
    io:format("🔴 UNHANDLED WEBSOCKET INFO MESSAGE: ~p\n", [Info]),
    {ok, State}.

run_streaming_example(<<"streaming">>, <<"pipeline">>, WsPid) ->
    % Streaming pipeline example
    WsPid ! {example_update, #{stage => <<"processing">>, data => <<"Starting streaming pipeline">>}},
    timer:sleep(1000),
    WsPid ! {example_update, #{stage => <<"complete">>, result => <<"Pipeline completed successfully">>}};

run_streaming_example(_, _, WsPid) ->
    WsPid ! {example_update, #{error => <<"Unknown streaming example">>}}.

%% Monitoring functions
monitor_agents(WsPid) ->
    % Start periodic monitoring with state tracking
    spawn(fun() -> monitoring_loop(WsPid, undefined) end),
    ok.

monitoring_loop(WsPid, LastSnapshot) ->
    timer:sleep(5000), % Update every 5 seconds instead of 1
    case process_info(WsPid) of
        undefined -> ok; % WebSocket closed
        _ ->
            % Only get basic agent info to reduce overhead
            Agents = agent_registry:list_agents(),
            Updates = lists:map(fun({Id, Pid, Meta}) ->
                % Only include essential info to reduce message size
                BasicUpdate = #{
                    id => list_to_binary(Id),
                    pid => list_to_binary(pid_to_list(Pid)),
                    status => get_agent_status(Pid)
                },
                % Add memory only if significant (> 1MB)
                WithMemory = case get_process_memory(Pid) of
                    Mem when Mem > 1048576 -> BasicUpdate#{memory => Mem};
                    _ -> BasicUpdate
                end,
                % Add queue length only if non-zero
                WithQueue = case get_message_queue_len(Pid) of
                    0 -> WithMemory;
                    Len -> WithMemory#{message_queue => Len}
                end,
                % Add filtered metadata - only keep essential fields
                FilteredMeta = case Meta of
                    M when is_map(M) -> maps:with([name, type, model, created_at], M);
                    _ -> #{}
                end,
                WithQueue#{meta => FilteredMeta}
            end, Agents),
            
            % Create normalized snapshot for comparison (remove memory/queue fields for comparison)
            NormalizedUpdates = lists:map(fun(Update) ->
                maps:without([memory, message_queue], Update)
            end, Updates),
            CurrentSnapshot = lists:sort(NormalizedUpdates),
            
            % Only send update if agents changed significantly
            case CurrentSnapshot =:= LastSnapshot of
                true ->
                    % No significant changes detected, skip this update
                    monitoring_loop(WsPid, LastSnapshot);
                false ->
                    % Changes detected, send update
                    SystemMetrics = get_system_metrics(),
                    
                    WsPid ! {monitoring_update, #{
                        agents => Updates,
                        system => SystemMetrics,
                        timestamp => os:system_time(millisecond)
                    }},
                    
                    monitoring_loop(WsPid, CurrentSnapshot)
            end
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
    % Get memory info
    TotalMemory = erlang:memory(total),
    UsedMemory = erlang:memory(processes) + erlang:memory(system),
    MemoryUsagePercent = round((UsedMemory / TotalMemory) * 100),
    
    % Get CPU usage (simplified - based on scheduler utilization)
    SchedulerUsage = try
        scheduler_usage()
    catch
        _:_ -> 0.0
    end,
    CpuUsagePercent = round(SchedulerUsage * 100),
    
    % Get uptime as milliseconds (not tuple)
    {WallClock, _} = erlang:statistics(wall_clock),
    
    #{
        node => atom_to_binary(node(), utf8),
        uptime => WallClock,
        total_memory => TotalMemory,
        used_memory => UsedMemory,
        process_count => erlang:system_info(process_count),
        run_queue => erlang:statistics(run_queue),
        schedulers => erlang:system_info(schedulers_online),
        % Add the percentage fields expected by frontend
        cpuUsage => CpuUsagePercent,
        memoryUsage => MemoryUsagePercent
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
scheduler_usage() ->
    case erlang:statistics(scheduler_wall_time) of
        undefined ->
            % Enable scheduler wall time if not enabled
            erlang:system_flag(scheduler_wall_time, true),
            timer:sleep(100),
            scheduler_usage();
        OldTimes ->
            timer:sleep(100),
            NewTimes = erlang:statistics(scheduler_wall_time),
            calculate_scheduler_usage(OldTimes, NewTimes)
    end.

calculate_scheduler_usage(OldTimes, NewTimes) ->
    %% Sort both lists by scheduler ID to ensure proper matching
    SortedOld = lists:keysort(1, OldTimes),
    SortedNew = lists:keysort(1, NewTimes),
    
    %% Calculate usage for matching schedulers
    {TotalUsage, Count} = lists:foldl(
        fun({I, A1, T1}, {AccUsage, AccCount}) ->
            case lists:keyfind(I, 1, SortedOld) of
                {I, A0, T0} ->
                    Active = A1 - A0,
                    Total = T1 - T0,
                    case Total of
                        0 -> {AccUsage, AccCount};
                        _ -> {AccUsage + (Active / Total), AccCount + 1}
                    end;
                false ->
                    %% New scheduler, skip
                    {AccUsage, AccCount}
            end
        end, 
        {0.0, 0}, 
        SortedNew
    ),
    
    %% Return average usage or 0 if no schedulers
    case Count of
        0 -> 0.0;
        _ -> TotalUsage / Count
    end.

normalize_agent_id(Id) when is_binary(Id) -> Id;
normalize_agent_id(Id) when is_list(Id) -> list_to_binary(Id);
normalize_agent_id(Id) when is_atom(Id) -> atom_to_binary(Id, utf8);
normalize_agent_id(Id) -> list_to_binary(io_lib:format("~p", [Id])).

generate_conversation_id() ->
    base64:encode(crypto:strong_rand_bytes(8)).

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
    % First check if this is a UTF-8 string disguised as a list
    case unicode:characters_to_binary(Data) of
        {error, _, _} ->
            % Not a valid UTF-8 string, treat as generic list
            try
                lists:map(fun ensure_json_safe/1, Data)
            catch
                _:_ -> <<"#List">>
            end;
        {incomplete, _, _} ->
            % Incomplete UTF-8, treat as generic list
            try
                lists:map(fun ensure_json_safe/1, Data)
            catch
                _:_ -> <<"#List">>
            end;
        BinaryData when is_binary(BinaryData) ->
            % Valid UTF-8 string, return as binary
            BinaryData
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
    colored_logger:ocean(surface, io_lib:format("[BROADCAST] WS Broadcasting message: ~p", [Message])),
    % Try to get websocket connections from ranch
    try
        Connections = ranch:procs(agent_web_listener, connections),
        lists:foreach(fun(ConnPid) ->
            % Send to all websocket connections
            ConnPid ! {learning_event, Message}
        end, Connections),
        colored_logger:complete(success, io_lib:format("[BROADCAST] Sent to ~p connections", [length(Connections)]))
    catch
        Type:Error ->
            colored_logger:alarm(medium, io_lib:format("[BROADCAST] Failed to broadcast: ~p:~p", [Type, Error]))
    end,
    ok.