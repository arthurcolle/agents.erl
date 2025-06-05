%%%-------------------------------------------------------------------
%%% @doc Meta-Meta Monitor
%%% Monitors the meta-systems themselves, analyzing their performance,
%%% effectiveness, and emergent behaviors. This creates a recursive
%%% monitoring loop where the monitoring systems are themselves monitored.
%%% @end
%%%-------------------------------------------------------------------
-module(meta_meta_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([monitor_meta_system/2,
         analyze_meta_performance/0,
         detect_meta_anomalies/0,
         get_meta_health_report/0,
         optimize_meta_systems/0,
         get_emergent_behaviors/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MONITOR_INTERVAL, 15000). % 15 seconds

-record(state, {
    %% Meta-system monitoring data
    meta_metrics = #{},          % Performance metrics for each meta-system
    meta_health = #{},           % Health status of meta-systems
    meta_interactions = [],      % Interactions between meta-systems
    
    %% Analysis results
    performance_trends = #{},    % Performance trends over time
    anomalies = [],             % Detected anomalies
    emergent_behaviors = [],    % Emergent patterns
    optimization_history = [],  % History of optimizations
    
    %% Recursive monitoring
    self_metrics = #{},         % Metrics about this monitor itself
    recursion_depth = 0,        % Current recursion depth
    meta_meta_insights = #{}    % Insights about the monitoring process
}).

-record(meta_metric, {
    system_name,
    timestamp,
    cpu_usage,
    memory_usage,
    message_queue_len,
    response_time,
    effectiveness_score,
    custom_metrics = #{}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Monitor a specific meta-system
monitor_meta_system(SystemName, Metrics) ->
    gen_server:cast(?SERVER, {monitor_system, SystemName, Metrics}).

%% @doc Analyze performance of all meta-systems
analyze_meta_performance() ->
    gen_server:call(?SERVER, analyze_performance).

%% @doc Detect anomalies in meta-system behavior
detect_meta_anomalies() ->
    gen_server:call(?SERVER, detect_anomalies).

%% @doc Get comprehensive health report
get_meta_health_report() ->
    gen_server:call(?SERVER, get_health_report).

%% @doc Optimize meta-systems based on monitoring data
optimize_meta_systems() ->
    gen_server:call(?SERVER, optimize_systems).

%% @doc Get detected emergent behaviors
get_emergent_behaviors() ->
    gen_server:call(?SERVER, get_emergent_behaviors).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Start periodic monitoring
    erlang:send_after(?MONITOR_INTERVAL, self(), monitor_tick),
    
    %% Register with meta-layer coordinator
    self() ! register_with_coordinator,
    
    %% Start self-monitoring
    self() ! monitor_self,
    
    {ok, #state{}}.

handle_call(analyze_performance, _From, State) ->
    Analysis = perform_performance_analysis(State),
    {reply, {ok, Analysis}, State};

handle_call(detect_anomalies, _From, State) ->
    Anomalies = detect_anomalies_impl(State),
    {reply, {ok, Anomalies}, State#state{anomalies = Anomalies}};

handle_call(get_health_report, _From, State) ->
    Report = generate_health_report(State),
    {reply, {ok, Report}, State};

handle_call(optimize_systems, _From, State) ->
    {Optimizations, NewState} = perform_optimizations(State),
    {reply, {ok, Optimizations}, NewState};

handle_call(get_emergent_behaviors, _From, State) ->
    {reply, {ok, State#state.emergent_behaviors}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({monitor_system, SystemName, Metrics}, State) ->
    %% Store metrics for the system
    MetricEntry = #meta_metric{
        system_name = SystemName,
        timestamp = erlang:system_time(millisecond),
        cpu_usage = maps:get(cpu, Metrics, 0),
        memory_usage = maps:get(memory, Metrics, 0),
        message_queue_len = maps:get(queue_len, Metrics, 0),
        response_time = maps:get(response_time, Metrics, 0),
        effectiveness_score = calculate_effectiveness(Metrics),
        custom_metrics = maps:get(custom, Metrics, #{})
    },
    
    %% Update state
    CurrentMetrics = maps:get(SystemName, State#state.meta_metrics, []),
    UpdatedMetrics = [MetricEntry | limit_metrics(CurrentMetrics, 100)],
    
    NewMetrics = maps:put(SystemName, UpdatedMetrics, State#state.meta_metrics),
    
    %% Update health status
    Health = assess_health(UpdatedMetrics),
    NewHealth = maps:put(SystemName, Health, State#state.meta_health),
    
    {noreply, State#state{
        meta_metrics = NewMetrics,
        meta_health = NewHealth
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(register_with_coordinator, State) ->
    case erlang:whereis(meta_layer_coordinator) of
        undefined ->
            erlang:send_after(5000, self(), register_with_coordinator);
        Pid ->
            meta_layer_coordinator:register_meta_system(meta_meta_monitor, self())
    end,
    {noreply, State};

handle_info(monitor_tick, State) ->
    %% Perform regular monitoring tasks
    NewState = perform_monitoring(State),
    
    %% Detect emergent behaviors
    EmergentBehaviors = detect_emergent_behaviors(NewState),
    
    %% Check for meta-anomalies
    check_meta_anomalies(NewState),
    
    %% Schedule next tick
    erlang:send_after(?MONITOR_INTERVAL, self(), monitor_tick),
    
    {noreply, NewState#state{emergent_behaviors = EmergentBehaviors}};

handle_info(monitor_self, State) ->
    %% Recursive self-monitoring
    SelfMetrics = collect_self_metrics(),
    
    %% Check recursion depth
    NewDepth = min(State#state.recursion_depth + 1, 3),
    
    %% Store self-metrics
    NewSelfMetrics = maps:put(
        erlang:system_time(millisecond),
        SelfMetrics,
        State#state.self_metrics
    ),
    
    %% Analyze self-performance if at depth limit
    NewInsights = case NewDepth >= 3 of
        true ->
            analyze_self_performance(NewSelfMetrics);
        false ->
            State#state.meta_meta_insights
    end,
    
    %% Schedule next self-monitoring
    erlang:send_after(30000, self(), monitor_self),
    
    {noreply, State#state{
        self_metrics = NewSelfMetrics,
        recursion_depth = NewDepth,
        meta_meta_insights = NewInsights
    }};

handle_info({meta_event, EventType, EventData}, State) ->
    %% Handle events from meta-layer coordinator
    NewState = handle_meta_event(EventType, EventData, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

perform_monitoring(State) ->
    %% Monitor all registered meta-systems
    MetaSystems = get_registered_meta_systems(),
    
    lists:foreach(fun({Name, Pid}) ->
        case erlang:process_info(Pid) of
            undefined ->
                %% System is down
                monitor_meta_system(Name, #{status => down});
            Info ->
                %% Extract metrics
                Metrics = #{
                    cpu => get_cpu_usage(Info),
                    memory => proplists:get_value(memory, Info, 0),
                    queue_len => proplists:get_value(message_queue_len, Info, 0),
                    response_time => measure_response_time(Pid),
                    custom => get_custom_metrics(Name, Pid)
                },
                monitor_meta_system(Name, Metrics)
        end
    end, MetaSystems),
    
    %% Update performance trends
    NewTrends = update_performance_trends(State#state.meta_metrics),
    
    State#state{performance_trends = NewTrends}.

calculate_effectiveness(Metrics) ->
    %% Calculate effectiveness score based on various factors
    BaseScore = 100,
    
    %% Deduct points for high resource usage
    CpuPenalty = min(maps:get(cpu, Metrics, 0) * 0.5, 30),
    MemoryPenalty = min(maps:get(memory, Metrics, 0) / 1000000 * 0.3, 20),
    QueuePenalty = min(maps:get(queue_len, Metrics, 0) * 0.1, 20),
    
    %% Add points for good response time
    ResponseBonus = case maps:get(response_time, Metrics, 1000) of
        RT when RT < 100 -> 10;
        RT when RT < 500 -> 5;
        _ -> 0
    end,
    
    max(0, BaseScore - CpuPenalty - MemoryPenalty - QueuePenalty + ResponseBonus).

assess_health(MetricsList) ->
    case MetricsList of
        [] -> #{status => unknown};
        [Latest | _] ->
            EffScore = Latest#meta_metric.effectiveness_score,
            Status = if
                EffScore > 80 -> healthy;
                EffScore > 60 -> degraded;
                EffScore > 40 -> warning;
                true -> critical
            end,
            
            #{
                status => Status,
                effectiveness => EffScore,
                last_update => Latest#meta_metric.timestamp
            }
    end.

detect_emergent_behaviors(State) ->
    %% Analyze interactions between meta-systems
    Interactions = analyze_interactions(State),
    
    %% Detect patterns in collective behavior
    Patterns = detect_collective_patterns(State#state.meta_metrics),
    
    %% Identify feedback loops
    FeedbackLoops = identify_feedback_loops(Interactions),
    
    %% Check for self-organization
    SelfOrganization = detect_self_organization(State),
    
    [
        {interactions, Interactions},
        {patterns, Patterns},
        {feedback_loops, FeedbackLoops},
        {self_organization, SelfOrganization}
    ].

analyze_interactions(State) ->
    %% Analyze how meta-systems interact
    %% This is simplified - in reality would track actual message passing
    SystemNames = maps:keys(State#state.meta_metrics),
    
    %% Check for correlated behaviors
    Correlations = [{S1, S2, calculate_correlation(
        maps:get(S1, State#state.meta_metrics, []),
        maps:get(S2, State#state.meta_metrics, [])
    )} || S1 <- SystemNames, S2 <- SystemNames, S1 < S2],
    
    %% Filter significant correlations
    [C || C = {_, _, Corr} <- Correlations, abs(Corr) > 0.7].

calculate_correlation(Metrics1, Metrics2) ->
    %% Simple correlation calculation
    case {Metrics1, Metrics2} of
        {[], _} -> 0.0;
        {_, []} -> 0.0;
        _ ->
            %% Compare effectiveness scores
            Scores1 = [M#meta_metric.effectiveness_score || M <- Metrics1],
            Scores2 = [M#meta_metric.effectiveness_score || M <- Metrics2],
            
            %% Take same length
            Len = min(length(Scores1), length(Scores2)),
            S1 = lists:sublist(Scores1, Len),
            S2 = lists:sublist(Scores2, Len),
            
            %% Calculate Pearson correlation (simplified)
            Avg1 = lists:sum(S1) / Len,
            Avg2 = lists:sum(S2) / Len,
            
            Numerator = lists:sum([
                (X1 - Avg1) * (X2 - Avg2) || 
                {X1, X2} <- lists:zip(S1, S2)
            ]),
            
            Denominator1 = math:sqrt(lists:sum([
                math:pow(X - Avg1, 2) || X <- S1
            ])),
            
            Denominator2 = math:sqrt(lists:sum([
                math:pow(X - Avg2, 2) || X <- S2
            ])),
            
            case Denominator1 * Denominator2 of
                +0.0 -> +0.0;
                D -> Numerator / D
            end
    end.

detect_collective_patterns(MetricsMap) ->
    %% Look for synchronized behaviors
    AllTimestamps = lists:usort(lists:flatten([
        [M#meta_metric.timestamp || M <- Metrics] || 
        Metrics <- maps:values(MetricsMap)
    ])),
    
    %% Check if systems spike together
    Spikes = lists:filter(fun(Timestamp) ->
        SystemsAtTime = [
            get_metric_at_time(Metrics, Timestamp) || 
            Metrics <- maps:values(MetricsMap)
        ],
        
        HighLoad = [M || M <- SystemsAtTime, 
                     M =/= undefined,
                     M#meta_metric.cpu_usage > 70],
        
        length(HighLoad) > map_size(MetricsMap) * 0.6
    end, AllTimestamps),
    
    #{synchronized_spikes => length(Spikes)}.

get_metric_at_time(Metrics, Timestamp) ->
    lists:keyfind(Timestamp, #meta_metric.timestamp, Metrics).

identify_feedback_loops(Interactions) ->
    %% Identify potential feedback loops in system interactions
    %% Simplified - looks for circular dependencies
    Loops = lists:filter(fun({S1, S2, _}) ->
        %% Check if S2 also correlates back to S1
        lists:any(fun({S2_check, S1_check, _}) ->
            S1 =:= S1_check andalso S2 =:= S2_check
        end, Interactions)
    end, Interactions),
    
    Loops.

detect_self_organization(State) ->
    %% Check if systems are self-organizing
    %% Look for decreasing entropy over time
    case State#state.performance_trends of
        Trends when map_size(Trends) > 0 ->
            %% Check if variance is decreasing (systems converging)
            VarianceTrend = calculate_variance_trend(Trends),
            
            #{
                converging => VarianceTrend < 0,
                variance_trend => VarianceTrend
            };
        _ ->
            #{converging => false}
    end.

calculate_variance_trend(Trends) ->
    %% Calculate if variance is increasing or decreasing
    Variances = maps:fold(fun(_System, Trend, Acc) ->
        case Trend of
            #{variance := V} -> [V | Acc];
            _ -> Acc
        end
    end, [], Trends),
    
    case Variances of
        [] -> 0;
        [_] -> 0;
        _ ->
            %% Simple linear regression on variances
            Indexed = lists:zip(lists:seq(1, length(Variances)), Variances),
            {Xs, Ys} = lists:unzip(Indexed),
            
            N = length(Xs),
            SumX = lists:sum(Xs),
            SumY = lists:sum(Ys),
            SumXY = lists:sum([X * Y || {X, Y} <- Indexed]),
            SumX2 = lists:sum([X * X || X <- Xs]),
            
            %% Slope of regression line
            Numerator = N * SumXY - SumX * SumY,
            Denominator = N * SumX2 - SumX * SumX,
            
            case Denominator of
                0 -> 0;
                _ -> Numerator / Denominator
            end
    end.

perform_performance_analysis(State) ->
    %% Comprehensive performance analysis
    #{
        system_health => State#state.meta_health,
        performance_trends => State#state.performance_trends,
        average_effectiveness => calculate_average_effectiveness(State),
        resource_usage => analyze_resource_usage(State),
        bottlenecks => identify_bottlenecks(State)
    }.

calculate_average_effectiveness(State) ->
    AllScores = lists:flatten([
        [M#meta_metric.effectiveness_score || M <- Metrics] ||
        Metrics <- maps:values(State#state.meta_metrics)
    ]),
    
    case AllScores of
        [] -> 0;
        _ -> lists:sum(AllScores) / length(AllScores)
    end.

analyze_resource_usage(State) ->
    maps:map(fun(_System, Metrics) ->
        case Metrics of
            [] -> #{};
            _ ->
                Recent = lists:sublist(Metrics, 10),
                #{
                    avg_cpu => avg([M#meta_metric.cpu_usage || M <- Recent]),
                    avg_memory => avg([M#meta_metric.memory_usage || M <- Recent]),
                    avg_queue => avg([M#meta_metric.message_queue_len || M <- Recent])
                }
        end
    end, State#state.meta_metrics).

identify_bottlenecks(State) ->
    %% Identify systems that might be bottlenecks
    ResourceUsage = analyze_resource_usage(State),
    
    Bottlenecks = maps:fold(fun(System, Usage, Acc) ->
        case Usage of
            #{avg_cpu := CPU} when CPU > 80 ->
                [{System, cpu_bottleneck} | Acc];
            #{avg_queue := Queue} when Queue > 100 ->
                [{System, queue_bottleneck} | Acc];
            _ ->
                Acc
        end
    end, [], ResourceUsage),
    
    Bottlenecks.

detect_anomalies_impl(State) ->
    %% Detect anomalies in meta-system behavior
    maps:fold(fun(System, Metrics, Acc) ->
        case detect_system_anomalies(Metrics) of
            [] -> Acc;
            Anomalies -> [{System, Anomalies} | Acc]
        end
    end, [], State#state.meta_metrics).

detect_system_anomalies(Metrics) when length(Metrics) < 10 ->
    [];
detect_system_anomalies(Metrics) ->
    %% Use statistical methods to detect anomalies
    Recent = lists:sublist(Metrics, 20),
    
    %% Calculate statistics
    CPUs = [M#meta_metric.cpu_usage || M <- Recent],
    AvgCPU = avg(CPUs),
    StdCPU = std_dev(CPUs, AvgCPU),
    
    %% Find anomalies (> 2 standard deviations)
    Anomalies = lists:filter(fun(M) ->
        abs(M#meta_metric.cpu_usage - AvgCPU) > 2 * StdCPU orelse
        M#meta_metric.effectiveness_score < 40
    end, Recent),
    
    Anomalies.

generate_health_report(State) ->
    #{
        overall_health => assess_overall_health(State),
        system_health => State#state.meta_health,
        anomalies => State#state.anomalies,
        emergent_behaviors => State#state.emergent_behaviors,
        self_monitoring => #{
            recursion_depth => State#state.recursion_depth,
            self_insights => State#state.meta_meta_insights
        },
        recommendations => generate_recommendations(State)
    }.

assess_overall_health(State) ->
    HealthStatuses = [H || #{status := H} <- maps:values(State#state.meta_health)],
    
    CriticalCount = length([S || S <- HealthStatuses, S =:= critical]),
    WarningCount = length([S || S <- HealthStatuses, S =:= warning]),
    
    if
        CriticalCount > 0 -> critical;
        WarningCount > 2 -> warning;
        WarningCount > 0 -> degraded;
        true -> healthy
    end.

generate_recommendations(State) ->
    Recs = [],
    
    %% Check for bottlenecks
    R1 = case identify_bottlenecks(State) of
        [] -> Recs;
        Bottlenecks ->
            [{address_bottlenecks, Bottlenecks} | Recs]
    end,
    
    %% Check anomalies
    R2 = case State#state.anomalies of
        [] -> R1;
        _ -> [{investigate_anomalies, State#state.anomalies} | R1]
    end,
    
    %% Check emergent behaviors
    R3 = case State#state.emergent_behaviors of
        [] -> R2;
        Behaviors ->
            case proplists:get_value(self_organization, Behaviors) of
                #{converging := true} ->
                    [{monitor_convergence, Behaviors} | R2];
                _ -> R2
            end
    end,
    
    R3.

perform_optimizations(State) ->
    %% Perform actual optimizations based on monitoring data
    Optimizations = [],
    
    %% Optimize high CPU systems
    O1 = optimize_high_cpu_systems(State),
    
    %% Optimize based on feedback loops
    O2 = optimize_feedback_loops(State),
    
    %% Record optimization
    OptRecord = #{
        timestamp => erlang:system_time(millisecond),
        optimizations => O1 ++ O2
    },
    
    NewHistory = [OptRecord | lists:sublist(State#state.optimization_history, 50)],
    
    {O1 ++ O2, State#state{optimization_history = NewHistory}}.

optimize_high_cpu_systems(State) ->
    ResourceUsage = analyze_resource_usage(State),
    
    lists:filtermap(fun({System, #{avg_cpu := CPU}}) when CPU > 70 ->
        %% Request optimization from coordinator
        meta_layer_coordinator:coordinate_cross_system_action(
            system_optimization,
            #{target => System, reason => high_cpu, cpu_usage => CPU}
        ),
        {true, {optimized, System, high_cpu}};
    ({_, _}) ->
        false
    end, maps:to_list(ResourceUsage)).

optimize_feedback_loops(State) ->
    case State#state.emergent_behaviors of
        [] -> [];
        Behaviors ->
            case proplists:get_value(feedback_loops, Behaviors) of
                [] -> [];
                Loops ->
                    %% Optimize detected feedback loops
                    [{feedback_loop_optimization, Loops}]
            end
    end.

%% Helper functions

limit_metrics(List, MaxSize) when length(List) >= MaxSize ->
    lists:sublist(List, MaxSize - 1);
limit_metrics(List, _) ->
    List.

get_registered_meta_systems() ->
    %% Get list of registered meta-systems from coordinator
    case erlang:whereis(meta_layer_coordinator) of
        undefined -> [];
        _Pid ->
            case meta_layer_coordinator:get_meta_insights() of
                {ok, Insights} ->
                    %% Extract system list (simplified)
                    [{unified_feedback_system, erlang:whereis(unified_feedback_system)},
                     {meta_layer_coordinator, erlang:whereis(meta_layer_coordinator)},
                     {cross_system_intelligence, erlang:whereis(cross_system_intelligence)}];
                _ -> []
            end
    end.

get_cpu_usage(ProcessInfo) ->
    %% Estimate CPU usage from reductions
    Reductions = proplists:get_value(reductions, ProcessInfo, 0),
    
    %% This is a rough estimate - in production would use proper CPU monitoring
    min(100, Reductions / 10000).

measure_response_time(Pid) ->
    %% Measure response time with a simple ping
    Start = erlang:monotonic_time(microsecond),
    
    try
        gen_server:call(Pid, ping, 100)
    catch
        _:_ -> timeout
    end,
    
    End = erlang:monotonic_time(microsecond),
    (End - Start) / 1000. % Convert to milliseconds

get_custom_metrics(_Name, _Pid) ->
    %% Placeholder for system-specific metrics
    #{}.

update_performance_trends(MetricsMap) ->
    maps:map(fun(_System, Metrics) ->
        case length(Metrics) > 5 of
            true ->
                Recent = lists:sublist(Metrics, 20),
                Scores = [M#meta_metric.effectiveness_score || M <- Recent],
                
                #{
                    trend => calculate_trend(Scores),
                    variance => variance(Scores),
                    stability => stability_score(Scores)
                };
            false ->
                #{}
        end
    end, MetricsMap).

calculate_trend([]) -> stable;
calculate_trend(Scores) when length(Scores) < 3 -> stable;
calculate_trend(Scores) ->
    %% Simple trend detection
    Recent = lists:sublist(Scores, 5),
    Older = lists:sublist(lists:nthtail(5, Scores), 5),
    
    RecentAvg = avg(Recent),
    OlderAvg = avg(Older),
    
    if
        RecentAvg > OlderAvg * 1.1 -> improving;
        RecentAvg < OlderAvg * 0.9 -> declining;
        true -> stable
    end.

stability_score(Scores) ->
    %% Calculate how stable the scores are
    case variance(Scores) of
        V when V < 10 -> high;
        V when V < 50 -> medium;
        _ -> low
    end.

check_meta_anomalies(State) ->
    %% Check for anomalies and report to coordinator
    case State#state.anomalies of
        [] -> ok;
        Anomalies ->
            meta_layer_coordinator:broadcast_meta_event(
                meta_anomalies_detected,
                #{anomalies => Anomalies, severity => high}
            )
    end.

collect_self_metrics() ->
    %% Collect metrics about this monitor process
    SelfInfo = erlang:process_info(self()),
    
    #{
        memory => proplists:get_value(memory, SelfInfo, 0),
        message_queue_len => proplists:get_value(message_queue_len, SelfInfo, 0),
        reductions => proplists:get_value(reductions, SelfInfo, 0),
        monitoring_count => length(get_registered_meta_systems())
    }.

analyze_self_performance(SelfMetrics) ->
    %% Analyze the monitor's own performance
    MetricsList = maps:values(SelfMetrics),
    
    %% Check if monitor is using too many resources
    AvgMemory = avg([maps:get(memory, M, 0) || M <- MetricsList]),
    AvgQueue = avg([maps:get(message_queue_len, M, 0) || M <- MetricsList]),
    
    #{
        self_health => if
            AvgMemory > 50000000 -> overloaded;
            AvgQueue > 50 -> busy;
            true -> healthy
        end,
        avg_memory => AvgMemory,
        avg_queue => AvgQueue,
        recommendation => if
            AvgMemory > 50000000 -> reduce_monitoring_frequency;
            AvgQueue > 50 -> optimize_processing;
            true -> continue_normal_operation
        end
    }.

handle_meta_event(feedback_analysis_complete, Data, State) ->
    %% Handle feedback analysis results
    %% Could trigger optimizations based on recommendations
    State;

handle_meta_event(meta_anomalies_detected, #{anomalies := Anomalies}, State) ->
    %% Meta-meta monitoring: anomalies in the anomaly detection!
    io:format("Meta-meta alert: Anomalies detected in meta-systems: ~p~n", [Anomalies]),
    State;

handle_meta_event(_, _, State) ->
    State.

%% Utility functions

avg([]) -> 0;
avg(List) -> lists:sum(List) / length(List).

variance([]) -> 0;
variance(List) ->
    Avg = avg(List),
    lists:sum([math:pow(X - Avg, 2) || X <- List]) / length(List).

std_dev(List, Avg) ->
    math:sqrt(lists:sum([math:pow(X - Avg, 2) || X <- List]) / length(List)).