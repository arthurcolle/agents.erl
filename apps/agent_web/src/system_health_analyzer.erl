-module(system_health_analyzer).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_health_report/0, get_health_score/0, force_health_check/0]).

-define(SERVER, ?MODULE).
-define(HEALTH_CHECK_INTERVAL, 10000). % Check every 10 seconds

-record(state, {
    last_check = 0,
    health_score = 100,
    health_report = #{},
    trend_history = []
}).

start_link() ->
    colored_logger:ocean(deep, "[HEALTH] Starting System Health Analyzer"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    colored_logger:success("[HEALTH] System Health Analyzer initialized", []),
    
    %% Start health monitoring
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), perform_health_check),
    
    {ok, #state{
        last_check = erlang:system_time(millisecond)
    }}.

handle_call(get_health_report, _From, State) ->
    {reply, State#state.health_report, State};

handle_call(get_health_score, _From, State) ->
    {reply, State#state.health_score, State};

handle_call(force_health_check, _From, State) ->
    NewState = perform_comprehensive_health_check(State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(perform_health_check, State) ->
    NewState = perform_comprehensive_health_check(State),
    
    %% Schedule next check
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), perform_health_check),
    
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    colored_logger:warning("[HEALTH] ⚠️ System Health Analyzer terminating"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Public API
get_health_report() ->
    gen_server:call(?SERVER, get_health_report).

get_health_score() ->
    gen_server:call(?SERVER, get_health_score).

force_health_check() ->
    gen_server:call(?SERVER, force_health_check).

%% Perform comprehensive health check
perform_comprehensive_health_check(State) ->
    Timestamp = erlang:system_time(millisecond),
    
    colored_logger:data(processed, "[HEALTH] 🔍 Performing comprehensive health check"),
    
    %% Collect health metrics
    HealthMetrics = #{
        system => check_system_health(),
        memory => check_memory_health(),
        processes => check_process_health(),
        supervisors => check_supervisor_health(),
        agents => check_agent_health(),
        network => check_network_health(),
        storage => check_storage_health()
    },
    
    %% Calculate overall health score
    OverallScore = calculate_overall_health_score(HealthMetrics),
    
    %% Create comprehensive report
    HealthReport = create_health_report(HealthMetrics, OverallScore, Timestamp),
    
    %% Log health status with colors
    log_health_status(OverallScore, HealthMetrics),
    
    %% Update trend history
    NewTrendHistory = update_trend_history(State#state.trend_history, OverallScore, Timestamp),
    
    %% Trigger alerts if needed
    maybe_trigger_health_alerts(OverallScore, HealthMetrics, State#state.health_score),
    
    State#state{
        last_check = Timestamp,
        health_score = OverallScore,
        health_report = HealthReport,
        trend_history = NewTrendHistory
    }.

%% Check system-level health
check_system_health() ->
    try
        {TotalMem, _} = erlang:memory([total, system]),
        ProcessCount = erlang:system_info(process_count),
        ProcessLimit = erlang:system_info(process_limit),
        RunQueue = erlang:statistics(run_queue),
        
        %% Calculate scores
        ProcessUtilization = (ProcessCount / ProcessLimit) * 100,
        RunQueueScore = max(0, 100 - (RunQueue * 10)),
        
        #{
            status => healthy,
            score => round((RunQueueScore + max(0, 100 - ProcessUtilization)) / 2),
            metrics => #{
                total_memory => TotalMem,
                process_count => ProcessCount,
                process_limit => ProcessLimit,
                process_utilization => ProcessUtilization,
                run_queue => RunQueue
            }
        }
    catch
        _:Error ->
            colored_logger:fire(bright, "[HEALTH] ❌ System health check failed: ~p", [Error]),
            #{status => error, score => 0, error => Error}
    end.

%% Check memory health
check_memory_health() ->
    try
        Memory = erlang:memory(),
        Total = proplists:get_value(total, Memory),
        Processes = proplists:get_value(processes, Memory),
        System = proplists:get_value(system, Memory),
        
        %% Calculate memory pressure
        ProcessMemoryRatio = (Processes / Total) * 100,
        SystemMemoryRatio = (System / Total) * 100,
        
        %% Score based on memory usage patterns
        Score = case {ProcessMemoryRatio, SystemMemoryRatio} of
            {P, S} when P > 80 orelse S > 80 -> 20;  % Critical
            {P, S} when P > 60 orelse S > 60 -> 50;  % Warning
            {P, S} when P > 40 orelse S > 40 -> 80;  % Caution
            _ -> 100  % Good
        end,
        
        #{
            status => determine_status(Score),
            score => Score,
            metrics => #{
                total => Total,
                processes => Processes,
                system => System,
                process_ratio => ProcessMemoryRatio,
                system_ratio => SystemMemoryRatio
            }
        }
    catch
        _:Error ->
            #{status => error, score => 0, error => Error}
    end.

%% Check process health
check_process_health() ->
    try
        Processes = erlang:processes(),
        ProcessCount = length(Processes),
        
        %% Sample process health
        {HealthyCount, UnhealthyCount, CriticalCount} = lists:foldl(fun(Pid, {H, U, C}) ->
            case analyze_process_health(Pid) of
                healthy -> {H + 1, U, C};
                unhealthy -> {H, U + 1, C};
                critical -> {H, U, C + 1}
            end
        end, {0, 0, 0}, lists:sublist(Processes, 100)), % Sample first 100 processes
        
        %% Calculate score
        SampleSize = HealthyCount + UnhealthyCount + CriticalCount,
        HealthRatio = case SampleSize of
            0 -> 100;
            _ -> (HealthyCount / SampleSize) * 100
        end,
        
        Score = round(HealthRatio - (CriticalCount * 10)), % Penalize critical processes
        
        #{
            status => determine_status(Score),
            score => max(0, Score),
            metrics => #{
                total_processes => ProcessCount,
                sampled => SampleSize,
                healthy => HealthyCount,
                unhealthy => UnhealthyCount,
                critical => CriticalCount,
                health_ratio => HealthRatio
            }
        }
    catch
        _:Error ->
            #{status => error, score => 0, error => Error}
    end.

%% Analyze individual process health
analyze_process_health(Pid) ->
    try
        case process_info(Pid, [memory, message_queue_len, status]) of
            undefined ->
                unhealthy;
            ProcInfo ->
                Memory = proplists:get_value(memory, ProcInfo, 0),
                MessageQueue = proplists:get_value(message_queue_len, ProcInfo, 0),
                Status = proplists:get_value(status, ProcInfo, running),
                
                case {Memory, MessageQueue, Status} of
                    {M, Q, _} when M > 50000000 orelse Q > 1000 -> critical;  % 50MB or 1000 messages
                    {M, Q, _} when M > 10000000 orelse Q > 100 -> unhealthy;  % 10MB or 100 messages
                    {_, _, waiting} -> healthy;
                    {_, _, running} -> healthy;
                    _ -> unhealthy
                end
        end
    catch
        _:_ -> unhealthy
    end.

%% Check supervisor health
check_supervisor_health() ->
    try
        Supervisors = [agent_web_sup, agent_supervisor, openai_sup, agents_sup],
        
        {RunningCount, StoppedCount, ErrorCount} = lists:foldl(fun(SupName, {R, S, E}) ->
            case whereis(SupName) of
                undefined -> {R, S + 1, E};
                Pid ->
                    case process_info(Pid) of
                        undefined -> {R, S, E + 1};
                        _ -> {R + 1, S, E}
                    end
            end
        end, {0, 0, 0}, Supervisors),
        
        TotalSups = length(Supervisors),
        Score = case TotalSups of
            0 -> 100;
            _ -> round((RunningCount / TotalSups) * 100)
        end,
        
        #{
            status => determine_status(Score),
            score => Score,
            metrics => #{
                total => TotalSups,
                running => RunningCount,
                stopped => StoppedCount,
                error => ErrorCount
            }
        }
    catch
        _:Error ->
            #{status => error, score => 0, error => Error}
    end.

%% Check agent health
check_agent_health() ->
    try
        case whereis(agent_registry) of
            undefined ->
                #{status => error, score => 0, error => agent_registry_not_running};
            _ ->
                Agents = agent_registry:list_agents(),
                AgentCount = length(Agents),
                
                %% Analyze agent health
                {HealthyAgents, UnhealthyAgents} = lists:foldl(fun({_Id, Pid, _Meta}, {H, U}) ->
                    case analyze_process_health(Pid) of
                        healthy -> {H + 1, U};
                        _ -> {H, U + 1}
                    end
                end, {0, 0}, Agents),
                
                Score = case AgentCount of
                    0 -> 100;  % No agents is fine
                    _ -> round((HealthyAgents / AgentCount) * 100)
                end,
                
                #{
                    status => determine_status(Score),
                    score => Score,
                    metrics => #{
                        total => AgentCount,
                        healthy => HealthyAgents,
                        unhealthy => UnhealthyAgents
                    }
                }
        end
    catch
        _:Error ->
            #{status => error, score => 0, error => Error}
    end.

%% Check network health
check_network_health() ->
    try
        %% Simple network health check - could be expanded
        %% Check if HTTP server is responding
        HttpScore = case whereis(agent_web_sup) of
            undefined -> 0;
            _Pid -> 100
        end,
        
        #{
            status => determine_status(HttpScore),
            score => HttpScore,
            metrics => #{
                http_server => case HttpScore of 100 -> running; _ -> stopped end
            }
        }
    catch
        _:Error ->
            #{status => error, score => 0, error => Error}
    end.

%% Check storage health
check_storage_health() ->
    try
        %% Check disk space, database connections, etc.
        %% Simplified for now
        #{
            status => healthy,
            score => 100,
            metrics => #{
                disk_space => ok,
                database => ok
            }
        }
    catch
        _:Error ->
            #{status => error, score => 0, error => Error}
    end.

%% Calculate overall health score
calculate_overall_health_score(HealthMetrics) ->
    %% Weighted average of all health scores
    Weights = #{
        system => 0.25,
        memory => 0.20,
        processes => 0.15,
        supervisors => 0.20,
        agents => 0.10,
        network => 0.05,
        storage => 0.05
    },
    
    WeightedSum = maps:fold(fun(Component, Weight, Acc) ->
        ComponentHealth = maps:get(Component, HealthMetrics, #{score => 0}),
        Score = maps:get(score, ComponentHealth, 0),
        Acc + (Score * Weight)
    end, 0, Weights),
    
    round(WeightedSum).

%% Create comprehensive health report
create_health_report(HealthMetrics, OverallScore, Timestamp) ->
    #{
        timestamp => Timestamp,
        overall_score => OverallScore,
        overall_status => determine_status(OverallScore),
        components => HealthMetrics,
        recommendations => generate_recommendations(HealthMetrics, OverallScore)
    }.

%% Generate health recommendations
generate_recommendations(HealthMetrics, OverallScore) ->
    Recommendations = [],
    
    %% Add recommendations based on component health
    NewRecs = maps:fold(fun(Component, Health, Acc) ->
        Score = maps:get(score, Health, 100),
        case {Component, Score} of
            {memory, S} when S < 50 ->
                ["Consider memory optimization or garbage collection" | Acc];
            {processes, S} when S < 50 ->
                ["Review process health and consider cleanup" | Acc];
            {supervisors, S} when S < 80 ->
                ["Check supervisor status and restart if needed" | Acc];
            {agents, S} when S < 70 ->
                ["Monitor agent health and consider restarting unhealthy agents" | Acc];
            _ ->
                Acc
        end
    end, Recommendations, HealthMetrics),
    
    %% Add overall recommendations
    case OverallScore of
        S when S < 50 ->
            ["System health is critical - immediate attention required" | NewRecs];
        S when S < 70 ->
            ["System health is degraded - investigation recommended" | NewRecs];
        _ ->
            NewRecs
    end.

%% Determine status from score
determine_status(Score) when Score >= 80 -> healthy;
determine_status(Score) when Score >= 60 -> degraded;
determine_status(Score) when Score >= 40 -> warning;
determine_status(_) -> critical.

%% Log health status with appropriate colors
log_health_status(OverallScore, HealthMetrics) ->
    case determine_status(OverallScore) of
        healthy ->
            colored_logger:success("[HEALTH] System health: HEALTHY (~p/100)", [OverallScore]);
        degraded ->
            colored_logger:warning("[HEALTH] ⚠️ System health: DEGRADED (~p/100)", [OverallScore]);
        warning ->
            colored_logger:warning("[HEALTH] ⚠️ System health: WARNING (~p/100)", [OverallScore]);
        critical ->
            colored_logger:fire(bright, "[HEALTH] 🚨 System health: CRITICAL (~p/100)", [OverallScore])
    end,
    
    %% Log component details
    maps:foreach(fun(Component, Health) ->
        Score = maps:get(score, Health, 0),
        Status = maps:get(status, Health, error),
        case Status of
            healthy when Score >= 80 ->
                colored_logger:data(processed, "[HEALTH] ✅ ~p: ~p (~p/100)", [Component, Status, Score]);
            _ when Score >= 60 ->
                colored_logger:warning("[HEALTH] ⚠️ ~p: ~p (~p/100)", [Component, Status, Score]);
            _ ->
                colored_logger:fire(bright, "[HEALTH] ❌ ~p: ~p (~p/100)", [Component, Status, Score])
        end
    end, HealthMetrics).

%% Update trend history
update_trend_history(History, Score, Timestamp) ->
    NewEntry = #{score => Score, timestamp => Timestamp},
    NewHistory = [NewEntry | History],
    %% Keep last 100 entries
    lists:sublist(NewHistory, 100).

%% Maybe trigger health alerts
maybe_trigger_health_alerts(CurrentScore, HealthMetrics, PreviousScore) ->
    %% Trigger alerts for significant health degradation
    case {determine_status(PreviousScore), determine_status(CurrentScore)} of
        {healthy, degraded} ->
            colored_logger:warning("[HEALTH] 📉 Health degraded from healthy to degraded");
        {healthy, warning} ->
            colored_logger:warning("[HEALTH] 📉 Health degraded from healthy to warning");
        {healthy, critical} ->
            colored_logger:fire(bright, "[HEALTH] 📉 Health critically degraded from healthy");
        {degraded, critical} ->
            colored_logger:fire(bright, "[HEALTH] 📉 Health critically degraded from degraded");
        {critical, warning} ->
            colored_logger:success("[HEALTH] Health improved from critical to warning", []);
        {warning, degraded} ->
            colored_logger:success("[HEALTH] Health improved from warning to degraded", []);
        {degraded, healthy} ->
            colored_logger:success("[HEALTH] Health improved from degraded to healthy", []);
        _ ->
            ok
    end,
    
    %% Trigger auto-healing for critical components
    maps:foreach(fun(Component, Health) ->
        Score = maps:get(score, Health, 100),
        case Score < 30 of
            true ->
                colored_logger:fire(bright, "[HEALTH] 🚨 Triggering auto-healing for critical component: ~p", [Component]),
                auto_healing_coordinator:trigger_healing({component_critical, Component}, Health);
            false ->
                ok
        end
    end, HealthMetrics).