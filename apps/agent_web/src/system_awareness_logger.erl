-module(system_awareness_logger).
-behaviour(gen_server).

%% API
-export([start_link/0, log_http_request/5, log_websocket_event/3, log_process_event/3,
         log_memory_event/2, log_network_event/3, log_error/3, log_performance/2,
         log_database_event/3, log_file_operation/3, log_security_event/3,
         get_comprehensive_status/0, get_real_time_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Ultra-detailed logging macros
-define(LOG_HTTP(Msg), colored_logger:network(high, "[HTTP] " ++ Msg)).
-define(LOG_HTTP(Msg, Args), colored_logger:network(high, io_lib:format("[HTTP] " ++ Msg, Args))).
-define(LOG_WS(Msg), colored_logger:ocean(deep, "[WEBSOCKET] " ++ Msg)).
-define(LOG_WS(Msg, Args), colored_logger:ocean(deep, io_lib:format("[WEBSOCKET] " ++ Msg, Args))).
-define(LOG_PROC(Msg), colored_logger:system(cpu, "[PROCESS] " ++ Msg)).
-define(LOG_PROC(Msg, Args), colored_logger:system(cpu, io_lib:format("[PROCESS] " ++ Msg, Args))).
-define(LOG_MEM(Msg), colored_logger:performance(memory, "[MEMORY] " ++ Msg)).
-define(LOG_MEM(Msg, Args), colored_logger:performance(memory, io_lib:format("[MEMORY] " ++ Msg, Args))).
-define(LOG_NET(Msg), colored_logger:data(transmission, "[NETWORK] " ++ Msg)).
-define(LOG_NET(Msg, Args), colored_logger:data(transmission, io_lib:format("[NETWORK] " ++ Msg, Args))).
-define(LOG_ERR(Msg), colored_logger:alarm(critical, "[ERROR] " ++ Msg)).
-define(LOG_ERR(Msg, Args), colored_logger:alarm(critical, io_lib:format("[ERROR] " ++ Msg, Args))).
-define(LOG_PERF(Msg), colored_logger:complete(optimization, "[PERFORMANCE] " ++ Msg)).
-define(LOG_PERF(Msg, Args), colored_logger:complete(optimization, io_lib:format("[PERFORMANCE] " ++ Msg, Args))).
-define(LOG_DB(Msg), colored_logger:data(processed, "[DATABASE] " ++ Msg)).
-define(LOG_DB(Msg, Args), colored_logger:data(processed, io_lib:format("[DATABASE] " ++ Msg, Args))).
-define(LOG_FILE(Msg), colored_logger:forest(deep, "[FILE] " ++ Msg)).
-define(LOG_FILE(Msg, Args), colored_logger:forest(deep, io_lib:format("[FILE] " ++ Msg, Args))).
-define(LOG_SEC(Msg), colored_logger:security(maximum, "[SECURITY] " ++ Msg)).
-define(LOG_SEC(Msg, Args), colored_logger:security(maximum, io_lib:format("[SECURITY] " ++ Msg, Args))).
-define(LOG_DASHBOARD(Msg), colored_logger:celebration(party, "[DASHBOARD] " ++ Msg)).
-define(LOG_DASHBOARD(Msg, Args), colored_logger:celebration(party, io_lib:format("[DASHBOARD] " ++ Msg, Args))).

-record(state, {
    http_requests = [], % {Timestamp, Method, Path, Headers, Status, Duration, ClientIP, UserAgent}
    websocket_events = [], % {Timestamp, Event, Data, ConnectionId, ClientIP}
    process_events = [], % {Timestamp, Event, ProcessId, Details}
    memory_events = [], % {Timestamp, Type, Usage, Details}
    network_events = [], % {Timestamp, Type, Data, Source, Destination}
    errors = [], % {Timestamp, Type, Error, Context, Stacktrace}
    performance_metrics = [], % {Timestamp, Metric, Value, Context}
    database_events = [], % {Timestamp, Operation, Table, Details}
    file_operations = [], % {Timestamp, Operation, File, Details}
    security_events = [], % {Timestamp, Event, Details, Severity}
    system_snapshots = [], % {Timestamp, FullSystemState}
    alerts = [], % {Timestamp, AlertType, Message, Severity}
    last_snapshot = 0,
    snapshot_interval = 10000 % Every 10 seconds
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log_http_request(Method, Path, Headers, Status, Duration) ->
    gen_server:cast(?MODULE, {log_http_request, Method, Path, Headers, Status, Duration}).

log_websocket_event(Event, Data, ConnectionId) ->
    gen_server:cast(?MODULE, {log_websocket_event, Event, Data, ConnectionId}).

log_process_event(Event, ProcessId, Details) ->
    gen_server:cast(?MODULE, {log_process_event, Event, ProcessId, Details}).

log_memory_event(Type, Details) ->
    gen_server:cast(?MODULE, {log_memory_event, Type, Details}).

log_network_event(Type, Data, Details) ->
    gen_server:cast(?MODULE, {log_network_event, Type, Data, Details}).

log_error(Type, Error, Context) ->
    gen_server:cast(?MODULE, {log_error, Type, Error, Context}).

log_performance(Metric, Value) ->
    gen_server:cast(?MODULE, {log_performance, Metric, Value}).

log_database_event(Operation, Table, Details) ->
    gen_server:cast(?MODULE, {log_database_event, Operation, Table, Details}).

log_file_operation(Operation, File, Details) ->
    gen_server:cast(?MODULE, {log_file_operation, Operation, File, Details}).

log_security_event(Event, Details, Severity) ->
    gen_server:cast(?MODULE, {log_security_event, Event, Details, Severity}).

get_comprehensive_status() ->
    gen_server:call(?MODULE, get_comprehensive_status).

get_real_time_metrics() ->
    gen_server:call(?MODULE, get_real_time_metrics).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ?LOG_DASHBOARD("Starting ultra-detailed system awareness logger"),
    ?LOG_DASHBOARD("All system events will be tracked with maximum detail"),
    
    % Schedule periodic system snapshots
    erlang:send_after(5000, self(), take_system_snapshot),
    % Temporarily disable dashboard display due to Unicode formatting issues
    % erlang:send_after(5000, self(), display_comprehensive_dashboard),
    
    {ok, #state{}}.

handle_call(get_comprehensive_status, _From, State) ->
    Status = generate_comprehensive_status(State),
    {reply, Status, State};

handle_call(get_real_time_metrics, _From, State) ->
    Metrics = generate_real_time_metrics(State),
    {reply, Metrics, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({log_http_request, Method, Path, Headers, Status, Duration}, State) ->
    Now = erlang:system_time(millisecond),
    ClientIP = extract_client_ip(Headers),
    UserAgent = extract_user_agent(Headers),
    
    Entry = {Now, Method, Path, Headers, Status, Duration, ClientIP, UserAgent},
    
    ?LOG_HTTP("~s ~s -> ~p (~pms) from ~s [~s]", 
        [Method, Path, Status, Duration, ClientIP, UserAgent]),
    
    % Log detailed headers for debugging
    lists:foreach(fun({Key, Value}) ->
        ?LOG_HTTP("  Header: ~s: ~s", [Key, Value])
    end, maps:to_list(Headers)),
    
    NewState = State#state{
        http_requests = lists:sublist([Entry | State#state.http_requests], 1000)
    },
    
    % Check for performance issues
    if Duration > 1000 ->
        ?LOG_PERF("SLOW REQUEST DETECTED: ~s ~s took ~pms", [Method, Path, Duration]);
    true -> ok
    end,
    
    {noreply, NewState};

handle_cast({log_websocket_event, Event, Data, ConnectionId}, State) ->
    Now = erlang:system_time(millisecond),
    Entry = {Now, Event, Data, ConnectionId, <<"unknown">>},
    
    ?LOG_WS("Connection ~s: ~s - ~p", [ConnectionId, Event, Data]),
    
    NewState = State#state{
        websocket_events = lists:sublist([Entry | State#state.websocket_events], 1000)
    },
    {noreply, NewState};

handle_cast({log_process_event, Event, ProcessId, Details}, State) ->
    Now = erlang:system_time(millisecond),
    Entry = {Now, Event, ProcessId, Details},
    
    ?LOG_PROC("Process ~p: ~s - ~p", [ProcessId, Event, Details]),
    
    NewState = State#state{
        process_events = lists:sublist([Entry | State#state.process_events], 1000)
    },
    {noreply, NewState};

handle_cast({log_memory_event, Type, Details}, State) ->
    Now = erlang:system_time(millisecond),
    Entry = {Now, Type, erlang:memory(), Details},
    
    MemInfo = erlang:memory(),
    TotalMB = proplists:get_value(total, MemInfo) div 1024 div 1024,
    ProcessMB = proplists:get_value(processes, MemInfo) div 1024 div 1024,
    
    ?LOG_MEM("~s: Total=~pMB, Processes=~pMB - ~p", [Type, TotalMB, ProcessMB, Details]),
    
    % Memory alerts
    if TotalMB > 1000 ->
        ?LOG_ERR("HIGH MEMORY USAGE: ~pMB total", [TotalMB]);
    true -> ok
    end,
    
    NewState = State#state{
        memory_events = lists:sublist([Entry | State#state.memory_events], 1000)
    },
    {noreply, NewState};

handle_cast({log_network_event, Type, Data, Details}, State) ->
    Now = erlang:system_time(millisecond),
    Entry = {Now, Type, Data, <<"unknown">>, <<"unknown">>},
    
    ?LOG_NET("~s: ~p - ~p", [Type, Data, Details]),
    
    NewState = State#state{
        network_events = lists:sublist([Entry | State#state.network_events], 1000)
    },
    {noreply, NewState};

handle_cast({log_error, Type, Error, Context}, State) ->
    Now = erlang:system_time(millisecond),
    Stacktrace = try throw(error) catch _:_:Stack -> Stack end,
    Entry = {Now, Type, Error, Context, Stacktrace},
    
    ?LOG_ERR("~s: ~p in context ~p", [Type, Error, Context]),
    
    NewState = State#state{
        errors = lists:sublist([Entry | State#state.errors], 500)
    },
    {noreply, NewState};

handle_cast({log_performance, Metric, Value}, State) ->
    Now = erlang:system_time(millisecond),
    Entry = {Now, Metric, Value, #{}},
    
    ?LOG_PERF("~s: ~p", [Metric, Value]),
    
    NewState = State#state{
        performance_metrics = lists:sublist([Entry | State#state.performance_metrics], 1000)
    },
    {noreply, NewState};

handle_cast({log_database_event, Operation, Table, Details}, State) ->
    Now = erlang:system_time(millisecond),
    Entry = {Now, Operation, Table, Details},
    
    ?LOG_DB("~s on ~s - ~p", [Operation, Table, Details]),
    
    NewState = State#state{
        database_events = lists:sublist([Entry | State#state.database_events], 1000)
    },
    {noreply, NewState};

handle_cast({log_file_operation, Operation, File, Details}, State) ->
    Now = erlang:system_time(millisecond),
    Entry = {Now, Operation, File, Details},
    
    ?LOG_FILE("~s: ~s - ~p", [Operation, File, Details]),
    
    NewState = State#state{
        file_operations = lists:sublist([Entry | State#state.file_operations], 1000)
    },
    {noreply, NewState};

handle_cast({log_security_event, Event, Details, Severity}, State) ->
    Now = erlang:system_time(millisecond),
    Entry = {Now, Event, Details, Severity},
    
    ?LOG_SEC("~s [~s]: ~p", [Event, Severity, Details]),
    
    % Security alerts
    case Severity of
        high -> ?LOG_ERR("HIGH SECURITY ALERT: ~s - ~p", [Event, Details]);
        critical -> ?LOG_ERR("CRITICAL SECURITY ALERT: ~s - ~p", [Event, Details]);
        _ -> ok
    end,
    
    NewState = State#state{
        security_events = lists:sublist([Entry | State#state.security_events], 500)
    },
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(take_system_snapshot, State) ->
    Now = erlang:system_time(millisecond),
    
    % Comprehensive system snapshot
    Snapshot = #{
        timestamp => Now,
        memory => erlang:memory(),
        processes => erlang:system_info(process_count),
        process_limit => erlang:system_info(process_limit),
        ports => erlang:system_info(port_count),
        port_limit => erlang:system_info(port_limit),
        atoms => erlang:system_info(atom_count),
        atom_limit => erlang:system_info(atom_limit),
        ets_tables => length(ets:all()),
        schedulers => erlang:system_info(schedulers),
        scheduler_utilization => try erlang:statistics(scheduler_wall_time) catch _:_ -> [] end,
        garbage_collection => erlang:statistics(garbage_collection),
        reductions => erlang:statistics(reductions),
        runtime => erlang:statistics(runtime),
        wall_clock => erlang:statistics(wall_clock),
        io => erlang:statistics(io),
        uptime => erlang:statistics(wall_clock),
        registered_processes => length(erlang:registered())
    },
    
    NewState = State#state{
        system_snapshots = lists:sublist([Snapshot | State#state.system_snapshots], 100),
        last_snapshot = Now
    },
    
    % Schedule next snapshot
    erlang:send_after(State#state.snapshot_interval, self(), take_system_snapshot),
    
    {noreply, NewState};

handle_info(display_comprehensive_dashboard, State) ->
    display_ultra_detailed_dashboard(State),
    
    % Schedule next display (every 15 seconds)
    erlang:send_after(15000, self(), display_comprehensive_dashboard),
    
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

extract_client_ip(Headers) ->
    maps:get(<<"x-forwarded-for">>, Headers, 
        maps:get(<<"x-real-ip">>, Headers, <<"unknown">>)).

extract_user_agent(Headers) ->
    maps:get(<<"user-agent">>, Headers, <<"unknown">>).

generate_comprehensive_status(State) ->
    Now = erlang:system_time(millisecond),
    
    #{
        timestamp => Now,
        uptime_seconds => element(1, erlang:statistics(wall_clock)) div 1000,
        http_requests => #{
            total => length(State#state.http_requests),
            last_minute => count_events_in_window(State#state.http_requests, 60000),
            status_codes => analyze_status_codes(State#state.http_requests)
        },
        websocket_events => #{
            total => length(State#state.websocket_events),
            last_minute => count_events_in_window(State#state.websocket_events, 60000),
            active_connections => count_active_connections(State#state.websocket_events)
        },
        process_events => #{
            total => length(State#state.process_events),
            last_minute => count_events_in_window(State#state.process_events, 60000)
        },
        memory_usage => get_detailed_memory_info(),
        errors => #{
            total => length(State#state.errors),
            last_hour => count_events_in_window(State#state.errors, 3600000),
            by_type => group_errors_by_type(State#state.errors)
        },
        performance => analyze_performance_metrics(State#state.performance_metrics),
        alerts => State#state.alerts
    }.

generate_real_time_metrics(State) ->
    LastSnapshot = case State#state.system_snapshots of
        [Latest | _] -> Latest;
        [] -> #{}
    end,
    
    #{
        current_time => erlang:system_time(millisecond),
        memory => erlang:memory(),
        process_count => erlang:system_info(process_count),
        recent_http_requests => lists:sublist(State#state.http_requests, 10),
        recent_websocket_events => lists:sublist(State#state.websocket_events, 10),
        recent_errors => lists:sublist(State#state.errors, 5),
        system_snapshot => LastSnapshot
    }.

count_events_in_window(Events, WindowMs) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - WindowMs,
    length(lists:filter(fun({Timestamp, _}) -> Timestamp > Cutoff;
                          ({Timestamp, _, _}) -> Timestamp > Cutoff;
                          ({Timestamp, _, _, _}) -> Timestamp > Cutoff;
                          ({Timestamp, _, _, _, _}) -> Timestamp > Cutoff;
                          ({Timestamp, _, _, _, _, _}) -> Timestamp > Cutoff;
                          ({Timestamp, _, _, _, _, _, _}) -> Timestamp > Cutoff;
                          ({Timestamp, _, _, _, _, _, _, _}) -> Timestamp > Cutoff;
                          (_) -> false
                       end, Events)).

analyze_status_codes(HttpRequests) ->
    StatusCodes = [Status || {_, _, _, _, Status, _, _, _} <- HttpRequests],
    lists:foldl(fun(Status, Acc) ->
        maps:update_with(Status, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, StatusCodes).

count_active_connections(WebSocketEvents) ->
    % Count unique connection IDs in last 5 minutes
    Now = erlang:system_time(millisecond),
    Cutoff = Now - 300000,
    RecentEvents = lists:filter(fun({Timestamp, _, _, _, _}) -> Timestamp > Cutoff end, WebSocketEvents),
    ConnectionIds = [ConnId || {_, _, _, ConnId, _} <- RecentEvents],
    length(lists:usort(ConnectionIds)).

get_detailed_memory_info() ->
    MemInfo = erlang:memory(),
    #{
        total_mb => proplists:get_value(total, MemInfo) div 1024 div 1024,
        processes_mb => proplists:get_value(processes, MemInfo) div 1024 div 1024,
        system_mb => proplists:get_value(system, MemInfo) div 1024 div 1024,
        atom_mb => proplists:get_value(atom, MemInfo) div 1024 div 1024,
        binary_mb => proplists:get_value(binary, MemInfo) div 1024 div 1024,
        code_mb => proplists:get_value(code, MemInfo) div 1024 div 1024,
        ets_mb => proplists:get_value(ets, MemInfo) div 1024 div 1024
    }.

group_errors_by_type(Errors) ->
    lists:foldl(fun({_, Type, _, _, _}, Acc) ->
        maps:update_with(Type, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, Errors).

analyze_performance_metrics(Metrics) ->
    case Metrics of
        [] -> #{};
        _ ->
            #{
                total_metrics => length(Metrics),
                last_hour => count_events_in_window(Metrics, 3600000)
            }
    end.

display_ultra_detailed_dashboard(State) ->
    ?LOG_DASHBOARD(""),
    ?LOG_DASHBOARD("╔════════════════════════════════════════════════════════════════════════════════════════════════╗"),
    ?LOG_DASHBOARD("║                           🔍 ULTRA-DETAILED SYSTEM AWARENESS DASHBOARD 🔍                      ║"),
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    
    Now = calendar:system_time_to_rfc3339(erlang:system_time(second)),
    ?LOG_DASHBOARD("║ Timestamp: ~s                                                    ║", [Now]),
    
    % System health overview
    display_system_health_section(State),
    
    % HTTP activity
    display_http_activity_section(State),
    
    % WebSocket activity  
    display_websocket_activity_section(State),
    
    % Process monitoring
    display_process_monitoring_section(State),
    
    % Memory analysis
    display_memory_analysis_section(State),
    
    % Error analysis
    display_error_analysis_section(State),
    
    % Performance metrics
    display_performance_section(State),
    
    % Network activity
    display_network_activity_section(State),
    
    % Security status
    display_security_section(State),
    
    ?LOG_DASHBOARD("╚════════════════════════════════════════════════════════════════════════════════════════════════╝"),
    ?LOG_DASHBOARD("").

display_system_health_section(State) ->
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    ?LOG_DASHBOARD("║                                    💚 SYSTEM HEALTH 💚                                        ║"),
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    
    MemInfo = erlang:memory(),
    ProcessCount = erlang:system_info(process_count),
    ProcessLimit = erlang:system_info(process_limit),
    
    ?LOG_DASHBOARD("║ 🔋 Processes: ~p/~p (~.1f%%)   🧠 Memory: ~pMB   ⚡ Schedulers: ~p            ║", 
        [ProcessCount, ProcessLimit, (ProcessCount/ProcessLimit)*100, 
         proplists:get_value(total, MemInfo) div 1024 div 1024,
         erlang:system_info(schedulers)]),
    
    {UptimeMs, _} = erlang:statistics(wall_clock),
    UptimeHours = UptimeMs div 1000 div 3600,
    UptimeMinutes = (UptimeMs div 1000 rem 3600) div 60,
    
    ?LOG_DASHBOARD("║ ⏰ Uptime: ~ph ~pm   📊 ETS Tables: ~p   🔤 Atoms: ~p/~p              ║",
        [UptimeHours, UptimeMinutes, length(ets:all()),
         erlang:system_info(atom_count), erlang:system_info(atom_limit)]).

display_http_activity_section(State) ->
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    ?LOG_DASHBOARD("║                                   🌐 HTTP ACTIVITY 🌐                                         ║"),
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    
    TotalRequests = length(State#state.http_requests),
    RecentRequests = count_events_in_window(State#state.http_requests, 60000),
    StatusCodes = analyze_status_codes(State#state.http_requests),
    
    ?LOG_DASHBOARD("║ 📈 Total Requests: ~p   📊 Last Minute: ~p   ✅ 2xx: ~p   ❌ 4xx/5xx: ~p        ║",
        [TotalRequests, RecentRequests, 
         maps:get(200, StatusCodes, 0) + maps:get(201, StatusCodes, 0),
         maps:get(400, StatusCodes, 0) + maps:get(404, StatusCodes, 0) + maps:get(500, StatusCodes, 0)]),
    
    % Show recent requests
    RecentHttpRequests = lists:sublist(State#state.http_requests, 3),
    lists:foreach(fun({Timestamp, Method, Path, _Headers, Status, Duration, ClientIP, _UserAgent}) ->
        SecsAgo = (erlang:system_time(millisecond) - Timestamp) div 1000,
        PathShort = case byte_size(list_to_binary(Path)) > 30 of
            true -> binary_to_list(binary:part(list_to_binary(Path), 0, 27)) ++ "...";
            false -> Path
        end,
        ?LOG_DASHBOARD("║   ~ps ago: ~s ~s -> ~p (~pms) from ~s                              ║",
            [SecsAgo, Method, PathShort, Status, Duration, ClientIP])
    end, RecentHttpRequests).

display_websocket_activity_section(State) ->
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    ?LOG_DASHBOARD("║                                 🔌 WEBSOCKET ACTIVITY 🔌                                      ║"),
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    
    TotalWsEvents = length(State#state.websocket_events),
    RecentWsEventsCount = count_events_in_window(State#state.websocket_events, 60000),
    ActiveConnections = count_active_connections(State#state.websocket_events),
    
    ?LOG_DASHBOARD("║ 🔗 Active Connections: ~p   📡 Total Events: ~p   📊 Last Minute: ~p                  ║",
        [ActiveConnections, TotalWsEvents, RecentWsEventsCount]),
    
    % Show recent WebSocket events
    RecentWsEventsList = lists:sublist(State#state.websocket_events, 3),
    case RecentWsEventsList of
        [] ->
            ?LOG_DASHBOARD("║ No recent WebSocket events                                                                      ║");
        _ ->
            lists:foreach(fun({Timestamp, Event, Data, ConnectionId, _ClientIP}) ->
        SecsAgo = (erlang:system_time(millisecond) - Timestamp) div 1000,
        ConnIdShort = case byte_size(ConnectionId) > 8 of
            true -> binary:part(ConnectionId, 0, 8);
            false -> ConnectionId
        end,
        DataStr = case Data of
            B when is_binary(B) -> binary_to_list(binary:part(B, 0, min(20, byte_size(B))));
            _ -> io_lib:format("~p", [Data])
        end,
        ?LOG_DASHBOARD("║   ~ps ago: [~s] ~s: ~s                                                     ║",
            [SecsAgo, ConnIdShort, Event, DataStr])
    end, RecentWsEventsList)
    end.

display_process_monitoring_section(State) ->
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    ?LOG_DASHBOARD("║                                  ⚙️  PROCESS MONITORING ⚙️                                     ║"),
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    
    TotalProcEvents = length(State#state.process_events),
    RecentProcEvents = count_events_in_window(State#state.process_events, 60000),
    
    ?LOG_DASHBOARD("║ 🔄 Process Events: ~p   📊 Last Minute: ~p   📝 Registered: ~p                     ║",
        [TotalProcEvents, RecentProcEvents, length(erlang:registered())]),
    
    % Show critical process status
    CriticalProcesses = [mcp_registry, mcp_connection_manager, mcp_manager, conversation_stats_logger],
    lists:foreach(fun(Process) ->
        Status = case whereis(Process) of
            undefined -> <<"❌ DOWN"/utf8>>;
            Pid when is_pid(Pid) -> <<"✅ RUNNING"/utf8>>
        end,
        ProcessName = atom_to_list(Process),
        ?LOG_DASHBOARD("║   ~s: ~ts                                                                   ║",
            [ProcessName, Status])
    end, CriticalProcesses).

display_memory_analysis_section(State) ->
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    ?LOG_DASHBOARD("║                                   🧠 MEMORY ANALYSIS 🧠                                       ║"),
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    
    MemInfo = get_detailed_memory_info(),
    
    ?LOG_DASHBOARD("║ 📊 Total: ~pMB  🔧 Processes: ~pMB  ⚡ System: ~pMB  💎 Binary: ~pMB           ║",
        [maps:get(total_mb, MemInfo), maps:get(processes_mb, MemInfo), 
         maps:get(system_mb, MemInfo), maps:get(binary_mb, MemInfo)]),
    
    ?LOG_DASHBOARD("║ 🔤 Atoms: ~pMB  💾 Code: ~pMB  📋 ETS: ~pMB                                      ║",
        [maps:get(atom_mb, MemInfo), maps:get(code_mb, MemInfo), maps:get(ets_mb, MemInfo)]),
    
    TotalMemEvents = length(State#state.memory_events),
    RecentMemEvents = count_events_in_window(State#state.memory_events, 300000), % 5 minutes
    
    ?LOG_DASHBOARD("║ 📈 Memory Events: ~p   📊 Last 5 Minutes: ~p                                        ║",
        [TotalMemEvents, RecentMemEvents]).

display_error_analysis_section(State) ->
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    ?LOG_DASHBOARD("║                                   🚨 ERROR ANALYSIS 🚨                                        ║"),
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    
    TotalErrors = length(State#state.errors),
    RecentErrors = count_events_in_window(State#state.errors, 3600000), % 1 hour
    ErrorsByType = group_errors_by_type(State#state.errors),
    
    ?LOG_DASHBOARD("║ ⚠️  Total Errors: ~p   📊 Last Hour: ~p   🔍 Error Types: ~p                      ║",
        [TotalErrors, RecentErrors, maps:size(ErrorsByType)]),
    
    % Show recent errors
    RecentErrorList = lists:sublist(State#state.errors, 2),
    lists:foreach(fun({Timestamp, Type, Error, Context, _Stacktrace}) ->
        SecsAgo = (erlang:system_time(millisecond) - Timestamp) div 1000,
        ErrorStr = case Error of
            B when is_binary(B) -> binary_to_list(binary:part(B, 0, min(30, byte_size(B))));
            _ -> lists:sublist(io_lib:format("~p", [Error]), 30)
        end,
        ?LOG_DASHBOARD("║   ~ps ago: ~s - ~s in ~p                                                     ║",
            [SecsAgo, Type, ErrorStr, Context])
    end, RecentErrorList).

display_performance_section(State) ->
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    ?LOG_DASHBOARD("║                                 🚀 PERFORMANCE METRICS 🚀                                     ║"),
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    
    TotalMetrics = length(State#state.performance_metrics),
    RecentMetrics = count_events_in_window(State#state.performance_metrics, 3600000),
    
    {_, Reductions} = erlang:statistics(reductions),
    {RuntimeMs, _} = erlang:statistics(runtime),
    
    ?LOG_DASHBOARD("║ 📊 Perf Metrics: ~p   ⚡ Reductions: ~p   🕒 Runtime: ~ps                    ║",
        [TotalMetrics, Reductions, RuntimeMs div 1000]),
    
    % Show garbage collection stats
    {GCCount, GCTime, _} = erlang:statistics(garbage_collection),
    ?LOG_DASHBOARD("║ 🗑️  GC Runs: ~p   ⏱️  GC Time: ~pms                                                ║",
        [GCCount, GCTime]).

display_network_activity_section(State) ->
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    ?LOG_DASHBOARD("║                                  🌐 NETWORK ACTIVITY 🌐                                       ║"),
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    
    TotalNetEvents = length(State#state.network_events),
    RecentNetEvents = count_events_in_window(State#state.network_events, 60000),
    
    % Get I/O statistics
    {{InputBytes, OutputBytes}, _} = erlang:statistics(io),
    
    ?LOG_DASHBOARD("║ 📡 Network Events: ~p   📊 Last Minute: ~p                                           ║",
        [TotalNetEvents, RecentNetEvents]),
    
    ?LOG_DASHBOARD("║ 📥 Input: ~p bytes   📤 Output: ~p bytes                                            ║",
        [InputBytes, OutputBytes]).

display_security_section(State) ->
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    ?LOG_DASHBOARD("║                                   🔒 SECURITY STATUS 🔒                                       ║"),
    ?LOG_DASHBOARD("╠════════════════════════════════════════════════════════════════════════════════════════════════╣"),
    
    TotalSecEvents = length(State#state.security_events),
    RecentSecEvents = count_events_in_window(State#state.security_events, 3600000), % 1 hour
    
    ?LOG_DASHBOARD("║ 🛡️  Security Events: ~p   📊 Last Hour: ~p   🚨 Active Alerts: ~p                  ║",
        [TotalSecEvents, RecentSecEvents, length(State#state.alerts)]).