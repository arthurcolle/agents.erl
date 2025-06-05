-module(mcp_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0,
         get_system_health/0,
         get_server_metrics/1,
         get_all_metrics/0,
         record_event/2,
         get_events/1,
         set_alert_threshold/2,
         get_alerts/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(METRICS_INTERVAL, 5000).
-define(EVENT_RETENTION, 3600). % 1 hour
-define(METRICS_TABLE, mcp_metrics).
-define(EVENTS_TABLE, mcp_events).
-define(ALERTS_TABLE, mcp_alerts).

-record(state, {
    metrics_timer,
    alert_thresholds = #{
        error_rate => 10.0,          % percentage
        latency => 5000,             % milliseconds
        memory_usage => 80.0,        % percentage
        connection_failures => 5      % count per minute
    },
    current_alerts = []
}).

-record(metric, {
    key,              % {server_id, metric_type}
    value,
    timestamp,
    metadata = #{}
}).

-record(event, {
    id,
    server_id,
    type,
    severity,         % info, warning, error, critical
    message,
    timestamp,
    metadata = #{}
}).

-record(alert, {
    id,
    server_id,
    type,
    threshold,
    current_value,
    message,
    timestamp,
    status            % active, resolved
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_system_health() ->
    gen_server:call(?SERVER, get_system_health).

get_server_metrics(ServerId) ->
    gen_server:call(?SERVER, {get_server_metrics, ServerId}).

get_all_metrics() ->
    gen_server:call(?SERVER, get_all_metrics).

record_event(ServerId, Event) ->
    gen_server:cast(?SERVER, {record_event, ServerId, Event}).

get_events(Options) ->
    gen_server:call(?SERVER, {get_events, Options}).

set_alert_threshold(Type, Value) ->
    gen_server:call(?SERVER, {set_alert_threshold, Type, Value}).

get_alerts() ->
    gen_server:call(?SERVER, get_alerts).

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    
    % Use persistent table manager to create tables
    persistent_table_manager:ensure_ets_table(?METRICS_TABLE, 
        [set, public, {keypos, #metric.key}], true),
    persistent_table_manager:ensure_ets_table(?EVENTS_TABLE, 
        [ordered_set, public, {keypos, #event.id}], true),
    persistent_table_manager:ensure_ets_table(?ALERTS_TABLE, 
        [set, public, {keypos, #alert.id}], true),
    
    % Start metrics collection timer
    Timer = erlang:send_after(?METRICS_INTERVAL, self(), collect_metrics),
    
    State = #state{
        metrics_timer = Timer
    },
    
    {ok, State}.

handle_call(get_system_health, _From, State) ->
    Health = calculate_system_health(State),
    {reply, Health, State};

handle_call({get_server_metrics, ServerId}, _From, State) ->
    Metrics = get_server_metrics_internal(ServerId),
    {reply, Metrics, State};

handle_call(get_all_metrics, _From, State) ->
    AllMetrics = get_all_metrics_internal(),
    {reply, AllMetrics, State};

handle_call({get_events, Options}, _From, State) ->
    Events = get_events_internal(Options),
    {reply, Events, State};

handle_call({set_alert_threshold, Type, Value}, _From, State) ->
    NewThresholds = maps:put(Type, Value, State#state.alert_thresholds),
    {reply, ok, State#state{alert_thresholds = NewThresholds}};

handle_call(get_alerts, _From, State) ->
    Alerts = get_active_alerts(),
    {reply, Alerts, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({record_event, ServerId, Event}, State) ->
    record_event_internal(ServerId, Event),
    % Check if event should trigger alert
    NewState = check_event_alerts(ServerId, Event, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_metrics, State) ->
    % Cancel old timer
    erlang:cancel_timer(State#state.metrics_timer),
    
    % Collect metrics from all servers
    collect_all_metrics(),
    
    % Check alert conditions
    NewState = check_metric_alerts(State),
    
    % Clean up old data
    cleanup_old_data(),
    
    % Schedule next collection
    NewTimer = erlang:send_after(?METRICS_INTERVAL, self(), collect_metrics),
    
    {noreply, NewState#state{metrics_timer = NewTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    erlang:cancel_timer(State#state.metrics_timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
calculate_system_health(State) ->
    % Get all server statuses
    Servers = mcp_registry:list_servers(),
    
    % Calculate overall metrics
    TotalServers = length(Servers),
    ConnectedServers = length([S || S <- Servers, 
                              element(3, S) =:= connected]),
    
    % Get system resources
    {TotalMem, AllocatedMem, _} = memsup:get_memory_data(),
    MemoryUsage = case TotalMem of
        0 -> 0;
        _ -> (AllocatedMem / TotalMem) * 100
    end,
    
    % Get CPU usage
    CpuUsage = case cpu_sup:util() of
        {ok, Busy, _, _} -> Busy;
        _ -> 0
    end,
    
    % Calculate error rates
    ErrorRate = calculate_error_rate(),
    
    % Determine health status
    Status = determine_health_status(#{
        connected_ratio => ConnectedServers / max(TotalServers, 1),
        memory_usage => MemoryUsage,
        cpu_usage => CpuUsage,
        error_rate => ErrorRate,
        active_alerts => length(State#state.current_alerts)
    }),
    
    #{
        status => Status,
        metrics => #{
            total_servers => TotalServers,
            connected_servers => ConnectedServers,
            memory_usage_percent => MemoryUsage,
            cpu_usage_percent => CpuUsage,
            error_rate => ErrorRate
        },
        active_alerts => length(State#state.current_alerts),
        timestamp => erlang:system_time(second)
    }.

get_server_metrics_internal(ServerId) ->
    Pattern = [{#metric{key = {ServerId, '_'}, _ = '_'}, [], ['$_']}],
    Metrics = ets:select(?METRICS_TABLE, Pattern),
    
    % Convert to map format
    maps:from_list([{Type, #{
        value => Value,
        timestamp => Timestamp,
        metadata => Metadata
    }} || #metric{key = {_, Type}, value = Value, 
                  timestamp = Timestamp, metadata = Metadata} <- Metrics]).

get_all_metrics_internal() ->
    AllMetrics = ets:tab2list(?METRICS_TABLE),
    
    % Group by server
    GroupedMetrics = lists:foldl(fun(#metric{key = {ServerId, Type}} = M, Acc) ->
        ServerMetrics = maps:get(ServerId, Acc, #{}),
        UpdatedMetrics = maps:put(Type, #{
            value => M#metric.value,
            timestamp => M#metric.timestamp,
            metadata => M#metric.metadata
        }, ServerMetrics),
        maps:put(ServerId, UpdatedMetrics, Acc)
    end, #{}, AllMetrics),
    
    GroupedMetrics.

collect_all_metrics() ->
    % Get all registered servers
    Servers = try
        mcp_registry:list_servers()
    catch
        _:_ ->
            % Registry not available, return empty list
            []
    end,
    
    lists:foreach(fun(#{id := ServerId, status := StatusBin, config := Config}) ->
        Status = binary_to_atom(StatusBin, utf8),
        % Basic connectivity metric
        record_metric(ServerId, connectivity, 
                     case Status of connected -> 1; _ -> 0 end),
        
        % Get detailed metrics from server if available
        case Status of
            connected ->
                collect_server_metrics(ServerId, Config);
            _ ->
                ok
        end
    end, Servers).

collect_server_metrics(ServerId, _Config) ->
    % Get metrics from MCP client if available
    case mcp_registry:get_server_pid(ServerId) of
        {ok, Pid} ->
            case catch mcp_advanced_client:get_metrics(Pid) of
                Metrics when is_map(Metrics) ->
                    record_metric(ServerId, request_count, 
                                 maps:get(total_requests, Metrics, 0)),
                    record_metric(ServerId, success_rate, 
                                 maps:get(success_rate, Metrics, 0)),
                    record_metric(ServerId, avg_latency, 
                                 maps:get(average_latency_ms, Metrics, 0));
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

record_metric(ServerId, Type, Value) ->
    Metric = #metric{
        key = {ServerId, Type},
        value = Value,
        timestamp = erlang:system_time(second)
    },
    ets:insert(?METRICS_TABLE, Metric).

record_event_internal(ServerId, EventData) ->
    Event = #event{
        id = erlang:unique_integer([positive, monotonic]),
        server_id = ServerId,
        type = maps:get(type, EventData, general),
        severity = maps:get(severity, EventData, info),
        message = maps:get(message, EventData, <<>>),
        timestamp = erlang:system_time(second),
        metadata = maps:get(metadata, EventData, #{})
    },
    ets:insert(?EVENTS_TABLE, Event).

get_events_internal(Options) ->
    Since = maps:get(since, Options, erlang:system_time(second) - 3600),
    ServerId = maps:get(server_id, Options, '_'),
    Severity = maps:get(severity, Options, '_'),
    
    % Build match spec
    MatchSpec = [{#event{
        server_id = ServerId,
        severity = Severity,
        timestamp = '$1',
        _ = '_'
    }, [{'>=', '$1', Since}], ['$_']}],
    
    Events = ets:select(?EVENTS_TABLE, MatchSpec),
    lists:reverse(Events). % Return newest first

check_event_alerts(ServerId, Event, State) ->
    Severity = maps:get(severity, Event, info),
    
    case Severity of
        critical ->
            create_alert(ServerId, event_critical, Event, State);
        error ->
            % Count recent errors
            RecentErrors = count_recent_events(ServerId, error, 60),
            Threshold = maps:get(error_rate, State#state.alert_thresholds),
            if
                RecentErrors > Threshold ->
                    create_alert(ServerId, high_error_rate, 
                               #{count => RecentErrors, threshold => Threshold}, State);
                true ->
                    State
            end;
        _ ->
            State
    end.

check_metric_alerts(State) ->
    AllMetrics = get_all_metrics_internal(),
    
    maps:fold(fun(ServerId, Metrics, AccState) ->
        % Check latency
        check_latency_alert(ServerId, Metrics, AccState),
        
        % Check success rate
        check_success_rate_alert(ServerId, Metrics, AccState)
    end, State, AllMetrics).

check_latency_alert(ServerId, Metrics, State) ->
    case maps:find(avg_latency, Metrics) of
        {ok, #{value := Latency}} ->
            Threshold = maps:get(latency, State#state.alert_thresholds),
            if
                Latency > Threshold ->
                    create_alert(ServerId, high_latency, 
                               #{latency => Latency, threshold => Threshold}, State);
                true ->
                    resolve_alert(ServerId, high_latency, State)
            end;
        _ ->
            State
    end.

check_success_rate_alert(ServerId, Metrics, State) ->
    case maps:find(success_rate, Metrics) of
        {ok, #{value := Rate}} when Rate < 90.0 ->
            create_alert(ServerId, low_success_rate, 
                       #{rate => Rate, threshold => 90.0}, State);
        _ ->
            resolve_alert(ServerId, low_success_rate, State)
    end.

create_alert(ServerId, Type, Data, State) ->
    AlertId = {ServerId, Type},
    
    Alert = #alert{
        id = AlertId,
        server_id = ServerId,
        type = Type,
        threshold = maps:get(threshold, Data, undefined),
        current_value = Data,
        message = format_alert_message(Type, Data),
        timestamp = erlang:system_time(second),
        status = active
    },
    
    ets:insert(?ALERTS_TABLE, Alert),
    
    % Notify alert handlers
    notify_alert(Alert),
    
    % Update current alerts
    CurrentAlerts = lists:usort([AlertId | State#state.current_alerts]),
    State#state{current_alerts = CurrentAlerts}.

resolve_alert(ServerId, Type, State) ->
    AlertId = {ServerId, Type},
    
    case ets:lookup(?ALERTS_TABLE, AlertId) of
        [Alert] when Alert#alert.status =:= active ->
            UpdatedAlert = Alert#alert{
                status = resolved,
                timestamp = erlang:system_time(second)
            },
            ets:insert(?ALERTS_TABLE, UpdatedAlert),
            
            % Update current alerts
            CurrentAlerts = lists:delete(AlertId, State#state.current_alerts),
            State#state{current_alerts = CurrentAlerts};
        _ ->
            State
    end.

get_active_alerts() ->
    MatchSpec = [{#alert{status = active, _ = '_'}, [], ['$_']}],
    ets:select(?ALERTS_TABLE, MatchSpec).

format_alert_message(high_latency, #{latency := L, threshold := T}) ->
    io_lib:format("High latency detected: ~pms (threshold: ~pms)", [L, T]);
format_alert_message(low_success_rate, #{rate := R}) ->
    io_lib:format("Low success rate: ~.1f%", [R]);
format_alert_message(high_error_rate, #{count := C}) ->
    io_lib:format("High error rate: ~p errors in last minute", [C]);
format_alert_message(event_critical, Event) ->
    maps:get(message, Event, "Critical event occurred").

notify_alert(Alert) ->
    % Send to WebSocket clients
    mcp_advanced_logger:log(alert, #{
        server_id => Alert#alert.server_id,
        type => Alert#alert.type,
        message => Alert#alert.message,
        status => Alert#alert.status
    }).

count_recent_events(ServerId, Severity, WindowSeconds) ->
    Since = erlang:system_time(second) - WindowSeconds,
    MatchSpec = [{#event{
        server_id = ServerId,
        severity = Severity,
        timestamp = '$1',
        _ = '_'
    }, [{'>=', '$1', Since}], [true]}],
    
    length(ets:select(?EVENTS_TABLE, MatchSpec)).

calculate_error_rate() ->
    % Calculate error rate across all servers in last 5 minutes
    Since = erlang:system_time(second) - 300,
    
    TotalEvents = count_events_since(Since, '_'),
    ErrorEvents = count_events_since(Since, error) + count_events_since(Since, critical),
    
    case TotalEvents of
        0 -> 0.0;
        _ -> (ErrorEvents / TotalEvents) * 100
    end.

count_events_since(Since, Severity) ->
    MatchSpec = [{#event{
        severity = Severity,
        timestamp = '$1',
        _ = '_'
    }, [{'>=', '$1', Since}], [true]}],
    
    length(ets:select(?EVENTS_TABLE, MatchSpec)).

determine_health_status(Metrics) ->
    #{
        connected_ratio := ConnRatio,
        memory_usage := MemUsage,
        cpu_usage := CpuUsage,
        error_rate := ErrorRate,
        active_alerts := AlertCount
    } = Metrics,
    
    if
        AlertCount > 5 orelse ErrorRate > 20.0 -> critical;
        AlertCount > 2 orelse ErrorRate > 10.0 orelse 
        MemUsage > 90.0 orelse CpuUsage > 90.0 -> warning;
        ConnRatio < 0.5 orelse MemUsage > 80.0 orelse 
        CpuUsage > 80.0 -> degraded;
        true -> healthy
    end.

cleanup_old_data() ->
    CutoffTime = erlang:system_time(second) - ?EVENT_RETENTION,
    
    % Clean old events
    ets:select_delete(?EVENTS_TABLE, [{#event{timestamp = '$1', _ = '_'}, 
                                      [{'<', '$1', CutoffTime}], [true]}]),
    
    % Clean resolved alerts older than 1 hour
    AlertCutoff = erlang:system_time(second) - 3600,
    ets:select_delete(?ALERTS_TABLE, [{#alert{status = resolved, 
                                             timestamp = '$1', _ = '_'}, 
                                      [{'<', '$1', AlertCutoff}], [true]}]).