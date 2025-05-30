-module(mcp_advanced_logger).
-behaviour(gen_server).

%% Advanced logging and metrics collection for MCP operations
%% Provides structured logging, metrics aggregation, and performance monitoring

-export([start_link/0, 
         log_connection_event/3, log_discovery_event/2, log_performance_metric/3,
         get_metrics_summary/0, get_recent_events/1, get_performance_stats/1,
         enable_debug_logging/0, disable_debug_logging/0, set_log_level/1,
         export_metrics/1, clear_metrics/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
    events = [], % List of recent events (limited size)
    metrics = #{}, % Aggregated metrics
    performance_stats = #{}, % Performance statistics
    log_level = info,
    debug_enabled = false,
    max_events = 1000 % Maximum events to keep in memory
}).

-record(event, {
    timestamp,
    type,
    level,
    server_id,
    details,
    metadata
}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    io:format("[MCP_LOG] Starting advanced MCP logger~n"),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} ->
            io:format("[MCP_LOG] Advanced logger started successfully with PID ~p~n", [Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("[ERROR] Failed to start MCP advanced logger: ~p~n", [Reason]),
            {error, Reason}
    end.

log_connection_event(ServerId, EventType, Details) ->
    gen_server:cast(?MODULE, {log_event, connection, info, ServerId, EventType, Details}).

log_discovery_event(EventType, Details) ->
    gen_server:cast(?MODULE, {log_event, discovery, info, undefined, EventType, Details}).

log_performance_metric(Operation, Duration, Metadata) ->
    gen_server:cast(?MODULE, {log_performance, Operation, Duration, Metadata}).

get_metrics_summary() ->
    gen_server:call(?MODULE, get_metrics_summary).

get_recent_events(Count) ->
    gen_server:call(?MODULE, {get_recent_events, Count}).

get_performance_stats(Operation) ->
    gen_server:call(?MODULE, {get_performance_stats, Operation}).

enable_debug_logging() ->
    gen_server:call(?MODULE, {set_debug, true}).

disable_debug_logging() ->
    gen_server:call(?MODULE, {set_debug, false}).

set_log_level(Level) ->
    gen_server:call(?MODULE, {set_log_level, Level}).

export_metrics(Format) ->
    gen_server:call(?MODULE, {export_metrics, Format}).

clear_metrics() ->
    gen_server:call(?MODULE, clear_metrics).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    
    % Start metrics aggregation timer
    erlang:send_after(60000, self(), aggregate_metrics), % Every minute
    
    % Start log rotation timer
    erlang:send_after(300000, self(), rotate_logs), % Every 5 minutes
    
    {ok, #state{}}.

handle_call(get_metrics_summary, _From, State) ->
    Summary = #{
        connection_events => count_events_by_type(connection, State#state.events),
        discovery_events => count_events_by_type(discovery, State#state.events),
        error_events => count_events_by_level(error, State#state.events),
        warning_events => count_events_by_level(warning, State#state.events),
        total_events => length(State#state.events),
        performance_ops => maps:keys(State#state.performance_stats),
        uptime_seconds => get_uptime_seconds()
    },
    {reply, Summary, State};

handle_call({get_recent_events, Count}, _From, State) ->
    RecentEvents = lists:sublist(State#state.events, Count),
    FormattedEvents = [format_event(Event) || Event <- RecentEvents],
    {reply, FormattedEvents, State};

handle_call({get_performance_stats, Operation}, _From, State) ->
    Stats = maps:get(Operation, State#state.performance_stats, #{}),
    {reply, Stats, State};

handle_call({set_debug, Enabled}, _From, State) ->
    {reply, ok, State#state{debug_enabled = Enabled}};

handle_call({set_log_level, Level}, _From, State) ->
    {reply, ok, State#state{log_level = Level}};

handle_call({export_metrics, Format}, _From, State) ->
    case Format of
        json ->
            ExportData = #{
                metrics => State#state.metrics,
                performance_stats => State#state.performance_stats,
                recent_events => [format_event(E) || E <- lists:sublist(State#state.events, 100)],
                timestamp => erlang:system_time(second)
            },
            {reply, {ok, jsx:encode(ExportData)}, State};
        prometheus ->
            PrometheusData = format_prometheus_metrics(State),
            {reply, {ok, PrometheusData}, State};
        _ ->
            {reply, {error, unsupported_format}, State}
    end;

handle_call(clear_metrics, _From, State) ->
    {reply, ok, State#state{
        events = [],
        metrics = #{},
        performance_stats = #{}
    }};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({log_event, Type, Level, ServerId, EventType, Details}, State) ->
    Event = #event{
        timestamp = erlang:system_time(second),
        type = Type,
        level = Level,
        server_id = ServerId,
        details = #{event_type => EventType, details => Details},
        metadata = #{node => node(), pid => self()}
    },
    
    % Add event to list and trim if necessary
    NewEvents = trim_events([Event | State#state.events], State#state.max_events),
    
    % Update metrics
    NewMetrics = update_event_metrics(Event, State#state.metrics),
    
    % Log to console if appropriate
    maybe_log_to_console(Event, State),
    
    {noreply, State#state{events = NewEvents, metrics = NewMetrics}};

handle_cast({log_performance, Operation, Duration, Metadata}, State) ->
    % Update performance statistics
    NewPerfStats = update_performance_stats(Operation, Duration, Metadata, State#state.performance_stats),
    
    % Create performance event
    Event = #event{
        timestamp = erlang:system_time(second),
        type = performance,
        level = debug,
        server_id = maps:get(server_id, Metadata, undefined),
        details = #{operation => Operation, duration => Duration, metadata => Metadata},
        metadata = #{node => node(), pid => self()}
    },
    
    NewEvents = trim_events([Event | State#state.events], State#state.max_events),
    
    {noreply, State#state{events = NewEvents, performance_stats = NewPerfStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(aggregate_metrics, State) ->
    % Perform periodic metrics aggregation
    AggregatedMetrics = aggregate_metrics(State#state.events, State#state.metrics),
    
    % Log summary
    case State#state.debug_enabled of
        true ->
            io:format("[MCP_LOG] Metrics summary: ~p~n", [AggregatedMetrics]);
        false -> ok
    end,
    
    % Schedule next aggregation
    erlang:send_after(60000, self(), aggregate_metrics),
    
    {noreply, State#state{metrics = AggregatedMetrics}};

handle_info(rotate_logs, State) ->
    % Rotate old events
    NewEvents = trim_events(State#state.events, State#state.max_events div 2),
    
    % Schedule next rotation
    erlang:send_after(300000, self(), rotate_logs),
    
    {noreply, State#state{events = NewEvents}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[MCP_LOG] Advanced logger shutting down~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

trim_events(Events, MaxSize) ->
    case length(Events) > MaxSize of
        true -> lists:sublist(Events, MaxSize);
        false -> Events
    end.

update_event_metrics(Event, Metrics) ->
    Type = Event#event.type,
    Level = Event#event.level,
    
    % Update counters
    TypeKey = {events, Type},
    LevelKey = {events, Level},
    
    Metrics1 = maps:update_with(TypeKey, fun(X) -> X + 1 end, 1, Metrics),
    Metrics2 = maps:update_with(LevelKey, fun(X) -> X + 1 end, 1, Metrics1),
    
    % Update rate tracking
    Now = erlang:system_time(second),
    RateKey = {rate, Type},
    case maps:find(RateKey, Metrics2) of
        {ok, {LastUpdate, Count, Rate}} when Now - LastUpdate < 60 ->
            maps:put(RateKey, {Now, Count + 1, Rate}, Metrics2);
        {ok, {LastUpdate, Count, _Rate}} ->
            NewRate = Count / max(1, Now - LastUpdate),
            maps:put(RateKey, {Now, 1, NewRate}, Metrics2);
        error ->
            maps:put(RateKey, {Now, 1, 0.0}, Metrics2)
    end.

update_performance_stats(Operation, Duration, Metadata, PerfStats) ->
    case maps:find(Operation, PerfStats) of
        {ok, Stats} ->
            NewStats = #{
                count => maps:get(count, Stats, 0) + 1,
                total_duration => maps:get(total_duration, Stats, 0) + Duration,
                min_duration => min(maps:get(min_duration, Stats, Duration), Duration),
                max_duration => max(maps:get(max_duration, Stats, Duration), Duration),
                last_duration => Duration,
                last_metadata => Metadata
            },
            AvgDuration = maps:get(total_duration, NewStats) / maps:get(count, NewStats),
            maps:put(Operation, NewStats#{avg_duration => AvgDuration}, PerfStats);
        error ->
            NewStats = #{
                count => 1,
                total_duration => Duration,
                min_duration => Duration,
                max_duration => Duration,
                avg_duration => Duration,
                last_duration => Duration,
                last_metadata => Metadata
            },
            maps:put(Operation, NewStats, PerfStats)
    end.

maybe_log_to_console(Event, State) ->
    ShouldLog = case {Event#event.level, State#state.log_level} of
        {error, _} -> true;
        {warning, Level} when Level =/= error -> true;
        {info, Level} when Level =:= info orelse Level =:= debug -> true;
        {debug, debug} -> State#state.debug_enabled;
        _ -> false
    end,
    
    case ShouldLog of
        true ->
            FormattedEvent = format_event(Event),
            io:format("[MCP_LOG] ~s~n", [FormattedEvent]);
        false -> ok
    end.

format_event(Event) ->
    Timestamp = format_timestamp(Event#event.timestamp),
    Level = string:uppercase(atom_to_list(Event#event.level)),
    Type = Event#event.type,
    ServerId = case Event#event.server_id of
        undefined -> "SYSTEM";
        Id -> binary_to_list(Id)
    end,
    Details = format_details(Event#event.details),
    io_lib:format("~s [~s] [~s] [~s] ~s", [Timestamp, Level, Type, ServerId, Details]).

format_timestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(
        Timestamp + calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
                 [Year, Month, Day, Hour, Min, Sec]).

format_details(Details) when is_map(Details) ->
    EventType = maps:get(event_type, Details, unknown),
    DetailsMap = maps:get(details, Details, #{}),
    io_lib:format("~p: ~p", [EventType, DetailsMap]);
format_details(Details) ->
    io_lib:format("~p", [Details]).

count_events_by_type(Type, Events) ->
    length([E || E <- Events, E#event.type =:= Type]).

count_events_by_level(Level, Events) ->
    length([E || E <- Events, E#event.level =:= Level]).

aggregate_metrics(Events, CurrentMetrics) ->
    % Calculate additional derived metrics
    Now = erlang:system_time(second),
    RecentEvents = [E || E <- Events, Now - E#event.timestamp < 3600], % Last hour
    
    DerivedMetrics = #{
        recent_event_count => length(RecentEvents),
        error_rate => calculate_error_rate(RecentEvents),
        connection_success_rate => calculate_connection_success_rate(RecentEvents)
    },
    
    maps:merge(CurrentMetrics, DerivedMetrics).

calculate_error_rate(Events) ->
    ErrorCount = count_events_by_level(error, Events),
    TotalCount = length(Events),
    case TotalCount of
        0 -> 0.0;
        _ -> ErrorCount / TotalCount
    end.

calculate_connection_success_rate(Events) ->
    ConnectionEvents = [E || E <- Events, E#event.type =:= connection],
    SuccessCount = length([E || E <- ConnectionEvents, 
                          maps:get(event_type, E#event.details, undefined) =:= connected]),
    TotalCount = length(ConnectionEvents),
    case TotalCount of
        0 -> 1.0; % No events = 100% success rate
        _ -> SuccessCount / TotalCount
    end.

format_prometheus_metrics(State) ->
    Lines = [
        "# HELP mcp_events_total Total number of MCP events",
        "# TYPE mcp_events_total counter",
        format_prometheus_counter("mcp_events_total", "connection", 
                                 count_events_by_type(connection, State#state.events)),
        format_prometheus_counter("mcp_events_total", "discovery", 
                                 count_events_by_type(discovery, State#state.events)),
        "",
        "# HELP mcp_performance_duration_seconds Performance metrics",
        "# TYPE mcp_performance_duration_seconds summary"
    ] ++ format_prometheus_performance_metrics(State#state.performance_stats),
    
    string:join(Lines, "\n").

format_prometheus_counter(Name, Label, Value) ->
    io_lib:format("~s{type=\"~s\"} ~p", [Name, Label, Value]).

format_prometheus_performance_metrics(PerfStats) ->
    maps:fold(fun(Operation, Stats, Acc) ->
        OpStr = atom_to_list(Operation),
        Count = maps:get(count, Stats, 0),
        AvgDuration = maps:get(avg_duration, Stats, 0) / 1000, % Convert to seconds
        [io_lib:format("mcp_performance_duration_seconds_count{operation=\"~s\"} ~p", [OpStr, Count]),
         io_lib:format("mcp_performance_duration_seconds_sum{operation=\"~s\"} ~.3f", [OpStr, AvgDuration * Count]) | Acc]
    end, [], PerfStats).

get_uptime_seconds() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    UpTime div 1000.