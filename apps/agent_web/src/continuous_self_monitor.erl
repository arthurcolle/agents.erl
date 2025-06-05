%%%-------------------------------------------------------------------
%%% @doc Continuous Self-Monitor
%%% Advanced continuous monitoring system that watches system health,
%%% performance, and behavior patterns. Feeds data to autonomous
%%% transformation engines for self-healing and self-improvement.
%%% @end
%%%-------------------------------------------------------------------
-module(continuous_self_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([add_monitor_target/3,
         remove_monitor_target/1,
         get_monitoring_status/0,
         get_system_health/0,
         enable_adaptive_monitoring/0,
         disable_adaptive_monitoring/0,
         set_monitoring_frequency/2,
         add_health_threshold/3,
         remove_health_threshold/1,
         trigger_immediate_scan/0,
         get_performance_trends/0,
         subscribe_to_alerts/1,
         unsubscribe_from_alerts/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_MONITOR_INTERVAL, 5000).   % 5 seconds
-define(ADAPTIVE_INTERVAL_MIN, 1000).     % 1 second minimum
-define(ADAPTIVE_INTERVAL_MAX, 60000).    % 1 minute maximum
-define(TREND_ANALYSIS_WINDOW, 300000).   % 5 minutes
-define(ALERT_COOLDOWN, 30000).           % 30 seconds

-record(state, {
    %% Monitoring configuration
    monitor_targets = #{} :: map(),
    monitoring_frequency = ?DEFAULT_MONITOR_INTERVAL :: integer(),
    adaptive_monitoring = false :: boolean(),
    adaptive_intervals = #{} :: map(),
    
    %% Health thresholds
    health_thresholds = #{} :: map(),
    performance_baselines = #{} :: map(),
    anomaly_detection = #{} :: map(),
    
    %% Data collection
    monitoring_data = [] :: list(),
    performance_history = [] :: list(),
    health_snapshots = [] :: list(),
    trend_data = #{} :: map(),
    
    %% Alerting
    alert_subscribers = [] :: list(),
    alert_history = [] :: list(),
    alert_cooldowns = #{} :: map(),
    
    %% Analysis engines
    trend_analyzer_pid :: pid() | undefined,
    anomaly_detector_pid :: pid() | undefined,
    performance_analyzer_pid :: pid() | undefined,
    
    %% Adaptive behavior
    learning_enabled = true :: boolean(),
    adaptation_parameters = #{} :: map(),
    behavior_patterns = #{} :: map(),
    optimization_suggestions = [] :: list(),
    
    %% System integration
    transformation_engine_pid :: pid() | undefined,
    self_healing_enabled = true :: boolean(),
    auto_optimization = false :: boolean()
}).

-record(monitor_target, {
    name :: atom(),
    type :: atom(),
    target :: term(),
    check_function :: fun(),
    frequency :: integer(),
    last_check :: integer(),
    threshold :: term(),
    metadata = #{} :: map()
}).

-record(health_snapshot, {
    timestamp :: integer(),
    overall_health :: float(),
    component_health = #{} :: map(),
    performance_metrics = #{} :: map(),
    resource_usage = #{} :: map(),
    error_indicators = #{} :: map(),
    warnings = [] :: list()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    start_link(#{}).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

%% @doc Add a monitoring target
add_monitor_target(Name, Type, Config) ->
    gen_server:call(?SERVER, {add_monitor_target, Name, Type, Config}).

%% @doc Remove a monitoring target
remove_monitor_target(Name) ->
    gen_server:call(?SERVER, {remove_monitor_target, Name}).

%% @doc Get current monitoring status
get_monitoring_status() ->
    gen_server:call(?SERVER, get_monitoring_status).

%% @doc Get current system health
get_system_health() ->
    gen_server:call(?SERVER, get_system_health).

%% @doc Enable adaptive monitoring
enable_adaptive_monitoring() ->
    gen_server:call(?SERVER, enable_adaptive_monitoring).

%% @doc Disable adaptive monitoring
disable_adaptive_monitoring() ->
    gen_server:call(?SERVER, disable_adaptive_monitoring).

%% @doc Set monitoring frequency for a target
set_monitoring_frequency(Target, Frequency) ->
    gen_server:call(?SERVER, {set_monitoring_frequency, Target, Frequency}).

%% @doc Add health threshold
add_health_threshold(Component, Metric, Threshold) ->
    gen_server:call(?SERVER, {add_health_threshold, Component, Metric, Threshold}).

%% @doc Remove health threshold
remove_health_threshold(Component) ->
    gen_server:call(?SERVER, {remove_health_threshold, Component}).

%% @doc Trigger immediate scan
trigger_immediate_scan() ->
    gen_server:cast(?SERVER, immediate_scan).

%% @doc Get performance trends
get_performance_trends() ->
    gen_server:call(?SERVER, get_performance_trends).

%% @doc Subscribe to alerts
subscribe_to_alerts(Pid) ->
    gen_server:call(?SERVER, {subscribe_to_alerts, Pid}).

%% @doc Unsubscribe from alerts
unsubscribe_from_alerts(Pid) ->
    gen_server:call(?SERVER, {unsubscribe_from_alerts, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Config]) ->
    %% Initialize monitoring system
    error_logger:info_msg("[MONITOR] Starting continuous self-monitor~n"),
    
    %% Start analysis engines
    TrendAnalyzerPid = spawn_link(fun() -> trend_analyzer_loop() end),
    AnomalyDetectorPid = spawn_link(fun() -> anomaly_detector_loop() end),
    PerformanceAnalyzerPid = spawn_link(fun() -> performance_analyzer_loop() end),
    
    %% Initialize default monitoring targets
    DefaultTargets = initialize_default_targets(),
    
    %% Initialize health thresholds
    DefaultThresholds = initialize_default_thresholds(),
    
    %% Connect to transformation engine
    TransformationEnginePid = connect_to_transformation_engine(),
    
    %% Schedule initial monitoring cycle
    MonitoringFrequency = maps:get(monitoring_frequency, Config, ?DEFAULT_MONITOR_INTERVAL),
    erlang:send_after(MonitoringFrequency, self(), monitoring_cycle),
    
    %% Schedule trend analysis
    erlang:send_after(?TREND_ANALYSIS_WINDOW, self(), trend_analysis),
    
    {ok, #state{
        monitor_targets = DefaultTargets,
        monitoring_frequency = MonitoringFrequency,
        health_thresholds = DefaultThresholds,
        trend_analyzer_pid = TrendAnalyzerPid,
        anomaly_detector_pid = AnomalyDetectorPid,
        performance_analyzer_pid = PerformanceAnalyzerPid,
        transformation_engine_pid = TransformationEnginePid,
        adaptive_monitoring = maps:get(adaptive_monitoring, Config, false),
        learning_enabled = maps:get(learning_enabled, Config, true),
        auto_optimization = maps:get(auto_optimization, Config, false)
    }}.

handle_call({add_monitor_target, Name, Type, Config}, _From, State) ->
    Target = create_monitor_target(Name, Type, Config),
    NewTargets = maps:put(Name, Target, State#state.monitor_targets),
    NewState = State#state{monitor_targets = NewTargets},
    {reply, ok, NewState};

handle_call({remove_monitor_target, Name}, _From, State) ->
    NewTargets = maps:remove(Name, State#state.monitor_targets),
    NewState = State#state{monitor_targets = NewTargets},
    {reply, ok, NewState};

handle_call(get_monitoring_status, _From, State) ->
    Status = compile_monitoring_status(State),
    {reply, {ok, Status}, State};

handle_call(get_system_health, _From, State) ->
    Health = compile_system_health(State),
    {reply, {ok, Health}, State};

handle_call(enable_adaptive_monitoring, _From, State) ->
    NewState = State#state{adaptive_monitoring = true},
    {reply, ok, NewState};

handle_call(disable_adaptive_monitoring, _From, State) ->
    NewState = State#state{adaptive_monitoring = false},
    {reply, ok, NewState};

handle_call({set_monitoring_frequency, Target, Frequency}, _From, State) ->
    case maps:get(Target, State#state.monitor_targets, undefined) of
        undefined ->
            {reply, {error, target_not_found}, State};
        MonitorTarget ->
            UpdatedTarget = MonitorTarget#monitor_target{frequency = Frequency},
            NewTargets = maps:put(Target, UpdatedTarget, State#state.monitor_targets),
            NewState = State#state{monitor_targets = NewTargets},
            {reply, ok, NewState}
    end;

handle_call({add_health_threshold, Component, Metric, Threshold}, _From, State) ->
    ComponentThresholds = maps:get(Component, State#state.health_thresholds, #{}),
    NewComponentThresholds = maps:put(Metric, Threshold, ComponentThresholds),
    NewThresholds = maps:put(Component, NewComponentThresholds, State#state.health_thresholds),
    NewState = State#state{health_thresholds = NewThresholds},
    {reply, ok, NewState};

handle_call({remove_health_threshold, Component}, _From, State) ->
    NewThresholds = maps:remove(Component, State#state.health_thresholds),
    NewState = State#state{health_thresholds = NewThresholds},
    {reply, ok, NewState};

handle_call(get_performance_trends, _From, State) ->
    Trends = analyze_performance_trends(State#state.performance_history),
    {reply, {ok, Trends}, State};

handle_call({subscribe_to_alerts, Pid}, _From, State) ->
    NewSubscribers = [Pid | State#state.alert_subscribers],
    NewState = State#state{alert_subscribers = NewSubscribers},
    {reply, ok, NewState};

handle_call({unsubscribe_from_alerts, Pid}, _From, State) ->
    NewSubscribers = lists:delete(Pid, State#state.alert_subscribers),
    NewState = State#state{alert_subscribers = NewSubscribers},
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(immediate_scan, State) ->
    NewState = perform_immediate_monitoring_scan(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(monitoring_cycle, State) ->
    %% Perform monitoring cycle
    NewState = perform_monitoring_cycle(State),
    
    %% Schedule next cycle (adaptive or fixed)
    NextInterval = calculate_next_monitoring_interval(NewState),
    erlang:send_after(NextInterval, self(), monitoring_cycle),
    
    {noreply, NewState};

handle_info(trend_analysis, State) ->
    %% Perform trend analysis
    NewState = perform_trend_analysis(State),
    
    %% Schedule next trend analysis
    erlang:send_after(?TREND_ANALYSIS_WINDOW, self(), trend_analysis),
    
    {noreply, NewState};

handle_info({trend_analysis_result, TrendData}, State) ->
    NewState = handle_trend_analysis_result(TrendData, State),
    {noreply, NewState};

handle_info({anomaly_detected, AnomalyData}, State) ->
    NewState = handle_anomaly_detection(AnomalyData, State),
    {noreply, NewState};

handle_info({performance_analysis_result, AnalysisData}, State) ->
    NewState = handle_performance_analysis_result(AnalysisData, State),
    {noreply, NewState};

handle_info({health_alert, AlertType, AlertData}, State) ->
    NewState = handle_health_alert(AlertType, AlertData, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:info_msg("[MONITOR] Continuous self-monitor stopping~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

perform_monitoring_cycle(State) ->
    %% Collect monitoring data from all targets
    MonitoringResults = collect_monitoring_data(State#state.monitor_targets),
    
    %% Create health snapshot
    HealthSnapshot = create_health_snapshot(MonitoringResults),
    
    %% Check thresholds and detect issues
    Issues = check_health_thresholds(HealthSnapshot, State#state.health_thresholds),
    
    %% Process issues and trigger responses
    ProcessedState = process_health_issues(Issues, State),
    
    %% Update monitoring history
    NewMonitoringData = [MonitoringResults | lists:sublist(State#state.monitoring_data, 999)],
    NewHealthSnapshots = [HealthSnapshot | lists:sublist(State#state.health_snapshots, 999)],
    
    %% Send data to analysis engines
    send_to_analysis_engines(MonitoringResults, ProcessedState),
    
    ProcessedState#state{
        monitoring_data = NewMonitoringData,
        health_snapshots = NewHealthSnapshots
    }.

perform_immediate_monitoring_scan(State) ->
    %% Perform comprehensive immediate scan
    ScanResults = perform_comprehensive_scan(State),
    
    %% Analyze results immediately
    Issues = analyze_scan_results(ScanResults, State),
    
    %% Trigger immediate responses if needed
    process_immediate_issues(Issues, State).

perform_trend_analysis(State) ->
    %% Analyze trends in monitoring data
    TrendData = analyze_monitoring_trends(State#state.monitoring_data),
    
    %% Update trend data
    NewTrendData = update_trend_data(TrendData, State#state.trend_data),
    
    %% Generate optimization suggestions
    OptimizationSuggestions = generate_optimization_suggestions(TrendData),
    
    State#state{
        trend_data = NewTrendData,
        optimization_suggestions = OptimizationSuggestions
    }.

collect_monitoring_data(MonitorTargets) ->
    %% Collect data from all monitoring targets
    maps:fold(fun(Name, Target, Acc) ->
        case should_check_target(Target) of
            true ->
                CheckResult = perform_target_check(Target),
                maps:put(Name, CheckResult, Acc);
            false ->
                Acc
        end
    end, #{}, MonitorTargets).

create_health_snapshot(MonitoringResults) ->
    %% Create comprehensive health snapshot
    Timestamp = erlang:system_time(millisecond),
    
    %% Calculate overall health score
    OverallHealth = calculate_overall_health_score(MonitoringResults),
    
    %% Extract component health
    ComponentHealth = extract_component_health(MonitoringResults),
    
    %% Extract performance metrics
    PerformanceMetrics = extract_performance_metrics(MonitoringResults),
    
    %% Extract resource usage
    ResourceUsage = extract_resource_usage(MonitoringResults),
    
    %% Detect error indicators
    ErrorIndicators = detect_error_indicators(MonitoringResults),
    
    %% Generate warnings
    Warnings = generate_health_warnings(MonitoringResults),
    
    #health_snapshot{
        timestamp = Timestamp,
        overall_health = OverallHealth,
        component_health = ComponentHealth,
        performance_metrics = PerformanceMetrics,
        resource_usage = ResourceUsage,
        error_indicators = ErrorIndicators,
        warnings = Warnings
    }.

check_health_thresholds(HealthSnapshot, Thresholds) ->
    %% Check all thresholds and identify violations
    maps:fold(fun(Component, ComponentThresholds, Acc) ->
        ComponentHealth = maps:get(Component, HealthSnapshot#health_snapshot.component_health, #{}),
        ComponentIssues = check_component_thresholds(Component, ComponentHealth, ComponentThresholds),
        Acc ++ ComponentIssues
    end, [], Thresholds).

process_health_issues(Issues, State) ->
    %% Process identified health issues
    lists:foldl(fun(Issue, AccState) ->
        process_single_health_issue(Issue, AccState)
    end, State, Issues).

calculate_next_monitoring_interval(State) ->
    case State#state.adaptive_monitoring of
        true ->
            calculate_adaptive_interval(State);
        false ->
            State#state.monitoring_frequency
    end.

calculate_adaptive_interval(State) ->
    %% Calculate adaptive monitoring interval based on system state
    case get_latest_health_snapshot(State) of
        undefined ->
            State#state.monitoring_frequency;
        HealthSnapshot ->
            OverallHealth = HealthSnapshot#health_snapshot.overall_health,
            
            %% More frequent monitoring for lower health
            if
                OverallHealth < 0.5 ->
                    ?ADAPTIVE_INTERVAL_MIN;
                OverallHealth < 0.7 ->
                    round(?ADAPTIVE_INTERVAL_MIN * 2);
                OverallHealth < 0.9 ->
                    State#state.monitoring_frequency;
                true ->
                    min(?ADAPTIVE_INTERVAL_MAX, State#state.monitoring_frequency * 2)
            end
    end.

handle_trend_analysis_result(TrendData, State) ->
    %% Process trend analysis results
    case identify_concerning_trends(TrendData) of
        [] ->
            State;
        ConcerningTrends ->
            %% Trigger proactive responses
            trigger_proactive_responses(ConcerningTrends, State)
    end.

handle_anomaly_detection(AnomalyData, State) ->
    %% Handle detected anomalies
    case classify_anomaly_severity(AnomalyData) of
        critical ->
            trigger_critical_response(AnomalyData, State);
        warning ->
            trigger_warning_response(AnomalyData, State);
        info ->
            log_anomaly_info(AnomalyData, State)
    end.

handle_performance_analysis_result(AnalysisData, State) ->
    %% Handle performance analysis results
    case extract_performance_insights(AnalysisData) of
        {optimization_needed, Optimizations} ->
            suggest_optimizations(Optimizations, State);
        {degradation_detected, DegradationData} ->
            handle_performance_degradation(DegradationData, State);
        {improvement_opportunity, Opportunities} ->
            suggest_improvements(Opportunities, State);
        _ ->
            State
    end.

handle_health_alert(AlertType, AlertData, State) ->
    %% Handle health alerts
    case should_send_alert(AlertType, AlertData, State) of
        true ->
            send_alert_to_subscribers(AlertType, AlertData, State),
            record_alert(AlertType, AlertData, State);
        false ->
            State
    end.

%% Helper functions

initialize_default_targets() ->
    #{
        system_processes => #monitor_target{
            name = system_processes,
            type = process_monitor,
            target = all,
            check_function = fun check_system_processes/0,
            frequency = 5000,
            threshold = #{max_processes => 1000000}
        },
        memory_usage => #monitor_target{
            name = memory_usage,
            type = memory_monitor,
            target = system,
            check_function = fun check_memory_usage/0,
            frequency = 5000,
            threshold = #{max_usage_percent => 80}
        },
        cpu_utilization => #monitor_target{
            name = cpu_utilization,
            type = cpu_monitor,
            target = system,
            check_function = fun check_cpu_utilization/0,
            frequency = 3000,
            threshold = #{max_utilization => 90}
        },
        message_queues => #monitor_target{
            name = message_queues,
            type = queue_monitor,
            target = all,
            check_function = fun check_message_queues/0,
            frequency = 2000,
            threshold = #{max_queue_length => 1000}
        },
        error_rates => #monitor_target{
            name = error_rates,
            type = error_monitor,
            target = system,
            check_function = fun check_error_rates/0,
            frequency = 10000,
            threshold = #{max_error_rate => 0.05}
        }
    }.

initialize_default_thresholds() ->
    #{
        system => #{
            cpu_usage => 85.0,
            memory_usage => 80.0,
            process_count => 800000,
            error_rate => 0.05
        },
        network => #{
            latency => 100,
            packet_loss => 0.01,
            throughput => 1000000
        },
        storage => #{
            disk_usage => 90.0,
            io_wait => 20.0
        }
    }.

connect_to_transformation_engine() ->
    case whereis(autonomous_self_transformation_engine) of
        undefined -> undefined;
        Pid -> Pid
    end.

create_monitor_target(Name, Type, Config) ->
    #monitor_target{
        name = Name,
        type = Type,
        target = maps:get(target, Config, system),
        check_function = maps:get(check_function, Config, fun() -> ok end),
        frequency = maps:get(frequency, Config, ?DEFAULT_MONITOR_INTERVAL),
        threshold = maps:get(threshold, Config, #{})
    }.

compile_monitoring_status(State) ->
    #{
        monitor_targets => maps:size(State#state.monitor_targets),
        adaptive_monitoring => State#state.adaptive_monitoring,
        monitoring_frequency => State#state.monitoring_frequency,
        alert_subscribers => length(State#state.alert_subscribers),
        data_points => length(State#state.monitoring_data),
        health_snapshots => length(State#state.health_snapshots),
        learning_enabled => State#state.learning_enabled,
        auto_optimization => State#state.auto_optimization
    }.

compile_system_health(State) ->
    case get_latest_health_snapshot(State) of
        undefined ->
            #{overall_health => unknown, components => #{}};
        HealthSnapshot ->
            #{
                overall_health => HealthSnapshot#health_snapshot.overall_health,
                timestamp => HealthSnapshot#health_snapshot.timestamp,
                components => HealthSnapshot#health_snapshot.component_health,
                performance => HealthSnapshot#health_snapshot.performance_metrics,
                resources => HealthSnapshot#health_snapshot.resource_usage,
                warnings => HealthSnapshot#health_snapshot.warnings
            }
    end.

get_latest_health_snapshot(State) ->
    case State#state.health_snapshots of
        [] -> undefined;
        [Latest | _] -> Latest
    end.

%% Analysis engine loops

trend_analyzer_loop() ->
    receive
        {analyze_trends, Data, ReplyTo} ->
            TrendResults = analyze_data_trends(Data),
            ReplyTo ! {trend_analysis_result, TrendResults},
            trend_analyzer_loop();
        stop -> ok
    after 5000 ->
        trend_analyzer_loop()
    end.

anomaly_detector_loop() ->
    receive
        {detect_anomalies, Data, ReplyTo} ->
            Anomalies = detect_data_anomalies(Data),
            lists:foreach(fun(Anomaly) ->
                ReplyTo ! {anomaly_detected, Anomaly}
            end, Anomalies),
            anomaly_detector_loop();
        stop -> ok
    after 3000 ->
        anomaly_detector_loop()
    end.

performance_analyzer_loop() ->
    receive
        {analyze_performance, Data, ReplyTo} ->
            PerformanceResults = analyze_performance_data(Data),
            ReplyTo ! {performance_analysis_result, PerformanceResults},
            performance_analyzer_loop();
        stop -> ok
    after 10000 ->
        performance_analyzer_loop()
    end.

%% Placeholder implementations

should_check_target(_Target) -> true.
perform_target_check(_Target) -> #{status => ok}.
calculate_overall_health_score(_Results) -> 0.8.
extract_component_health(_Results) -> #{}.
extract_performance_metrics(_Results) -> #{}.
extract_resource_usage(_Results) -> #{}.
detect_error_indicators(_Results) -> #{}.
generate_health_warnings(_Results) -> [].

check_component_thresholds(_Component, _Health, _Thresholds) -> [].
process_single_health_issue(_Issue, State) -> State.

send_to_analysis_engines(_Results, _State) -> ok.

perform_comprehensive_scan(_State) -> #{}.
analyze_scan_results(_Results, _State) -> [].
process_immediate_issues(_Issues, State) -> State.

analyze_monitoring_trends(_Data) -> #{}.
update_trend_data(_TrendData, OldData) -> OldData.
generate_optimization_suggestions(_TrendData) -> [].

analyze_performance_trends(_History) -> #{}.

identify_concerning_trends(_TrendData) -> [].
trigger_proactive_responses(_Trends, State) -> State.

classify_anomaly_severity(_AnomalyData) -> info.
trigger_critical_response(_AnomalyData, State) -> State.
trigger_warning_response(_AnomalyData, State) -> State.
log_anomaly_info(_AnomalyData, State) -> State.

extract_performance_insights(_AnalysisData) -> no_insights.
suggest_optimizations(_Optimizations, State) -> State.
handle_performance_degradation(_DegradationData, State) -> State.
suggest_improvements(_Opportunities, State) -> State.

should_send_alert(_AlertType, _AlertData, _State) -> false.
send_alert_to_subscribers(_AlertType, _AlertData, State) -> State.
record_alert(_AlertType, _AlertData, State) -> State.

analyze_data_trends(_Data) -> #{}.
detect_data_anomalies(_Data) -> [].
analyze_performance_data(_Data) -> #{}.

check_system_processes() -> #{process_count => 1000}.
check_memory_usage() -> #{usage_percent => 50}.
check_cpu_utilization() -> #{utilization => 30}.
check_message_queues() -> #{max_queue_length => 10}.
check_error_rates() -> #{error_rate => 0.01}.