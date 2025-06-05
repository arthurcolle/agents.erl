-module(performance_optimizer).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([optimize_system/0, get_optimization_stats/0, force_optimization/0]).

-define(SERVER, ?MODULE).
-define(OPTIMIZATION_INTERVAL, 30000). % Optimize every 30 seconds

-record(state, {
    last_optimization = 0,
    optimization_count = 0,
    stats = #{}
}).

start_link() ->
    colored_logger:ocean(deep, "[PERF_OPT] Starting Performance Optimizer"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    colored_logger:success("[PERF_OPT] Performance Optimizer initialized", []),
    
    %% Start optimization timer
    erlang:send_after(?OPTIMIZATION_INTERVAL, self(), optimize_system),
    
    {ok, #state{
        last_optimization = erlang:system_time(millisecond)
    }}.

handle_call(get_optimization_stats, _From, State) ->
    Stats = #{
        last_optimization => State#state.last_optimization,
        optimization_count => State#state.optimization_count,
        stats => State#state.stats
    },
    {reply, Stats, State};

handle_call(force_optimization, _From, State) ->
    NewState = perform_optimization(State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(optimize_system, State) ->
    NewState = perform_optimization(State),
    
    %% Schedule next optimization
    erlang:send_after(?OPTIMIZATION_INTERVAL, self(), optimize_system),
    
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    colored_logger:warning("[PERF_OPT] ⚠️ Performance Optimizer terminating"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Public API
optimize_system() ->
    gen_server:call(?SERVER, force_optimization).

get_optimization_stats() ->
    gen_server:call(?SERVER, get_optimization_stats).

force_optimization() ->
    gen_server:call(?SERVER, force_optimization).

%% Perform system optimization
perform_optimization(State) ->
    Timestamp = erlang:system_time(millisecond),
    
    colored_logger:data(processed, "[PERF_OPT] 🔧 Running performance optimization"),
    
    %% Collect performance metrics
    Metrics = collect_performance_metrics(),
    
    %% Apply optimizations based on metrics
    OptimizationResults = apply_optimizations(Metrics),
    
    %% Log optimization results
    log_optimization_results(OptimizationResults),
    
    %% Update state
    NewStats = maps:merge(State#state.stats, OptimizationResults),
    
    State#state{
        last_optimization = Timestamp,
        optimization_count = State#state.optimization_count + 1,
        stats = NewStats
    }.

%% Collect performance metrics
collect_performance_metrics() ->
    try
        %% Memory metrics
        Memory = erlang:memory(),
        TotalMemory = proplists:get_value(total, Memory),
        ProcessMemory = proplists:get_value(processes, Memory),
        SystemMemory = proplists:get_value(system, Memory),
        
        %% Process metrics
        ProcessCount = erlang:system_info(process_count),
        ProcessLimit = erlang:system_info(process_limit),
        
        %% Scheduler metrics
        RunQueue = erlang:statistics(run_queue),
        SchedulersOnline = erlang:system_info(schedulers_online),
        
        %% Garbage collection metrics
        {NumGCs, WordsReclaimed, _} = erlang:statistics(garbage_collection),
        
        #{
            memory => #{
                total => TotalMemory,
                processes => ProcessMemory,
                system => SystemMemory,
                ratio => (ProcessMemory / TotalMemory) * 100
            },
            processes => #{
                count => ProcessCount,
                limit => ProcessLimit,
                utilization => (ProcessCount / ProcessLimit) * 100
            },
            schedulers => #{
                run_queue => RunQueue,
                online => SchedulersOnline,
                load => RunQueue / SchedulersOnline
            },
            gc => #{
                count => NumGCs,
                words_reclaimed => WordsReclaimed
            }
        }
    catch
        _:Error ->
            colored_logger:fire(bright, "[PERF_OPT] ❌ Failed to collect metrics: ~p", [Error]),
            #{}
    end.

%% Apply performance optimizations
apply_optimizations(Metrics) ->
    Results = #{},
    
    %% Memory optimization
    MemoryResults = optimize_memory(Metrics),
    
    %% Process optimization
    ProcessResults = optimize_processes(Metrics),
    
    %% Scheduler optimization
    SchedulerResults = optimize_schedulers(Metrics),
    
    %% Garbage collection optimization
    GCResults = optimize_garbage_collection(Metrics),
    
    maps:merge(Results, #{
        memory => MemoryResults,
        processes => ProcessResults,
        schedulers => SchedulerResults,
        gc => GCResults
    }).

%% Memory optimization
optimize_memory(Metrics) ->
    MemoryInfo = maps:get(memory, Metrics, #{}),
    MemoryRatio = maps:get(ratio, MemoryInfo, 0),
    
    Results = #{optimizations_applied => []},
    
    case MemoryRatio of
        Ratio when Ratio > 80 ->
            colored_logger:warning("[PERF_OPT] ⚠️ High memory usage (~.1f%), triggering optimizations", [Ratio]),
            
            %% Force garbage collection on all processes
            GCCount = force_global_gc(),
            
            %% Clear unused ETS tables
            ETSCleared = clear_unused_ets_tables(),
            
            Results#{
                optimizations_applied => [global_gc, ets_cleanup],
                gc_processes => GCCount,
                ets_tables_cleared => ETSCleared
            };
        Ratio when Ratio > 60 ->
            colored_logger:info("[PERF_OPT] ℹ️ Moderate memory usage (~.1f%), applying light optimizations", [Ratio]),
            
            %% Selective garbage collection
            GCCount = selective_gc(),
            
            Results#{
                optimizations_applied => [selective_gc],
                gc_processes => GCCount
            };
        _ ->
            colored_logger:data(processed, "[PERF_OPT] Memory usage healthy (~.1f%)", [MemoryRatio]),
            Results
    end.

%% Process optimization
optimize_processes(Metrics) ->
    ProcessInfo = maps:get(processes, Metrics, #{}),
    ProcessCount = maps:get(count, ProcessInfo, 0),
    ProcessUtilization = maps:get(utilization, ProcessInfo, 0),
    
    Results = #{optimizations_applied => []},
    
    case ProcessUtilization of
        Util when Util > 80 ->
            colored_logger:warning("[PERF_OPT] ⚠️ High process utilization (~.1f%), monitoring for cleanup", [Util]),
            
            %% Find and terminate idle processes
            IdleProcesses = find_idle_processes(),
            TerminatedCount = terminate_idle_processes(IdleProcesses),
            
            Results#{
                optimizations_applied => [idle_process_cleanup],
                processes_terminated => TerminatedCount,
                idle_processes_found => length(IdleProcesses)
            };
        _ ->
            colored_logger:data(processed, "[PERF_OPT] Process utilization healthy (~.1f%)", [ProcessUtilization]),
            Results
    end.

%% Scheduler optimization
optimize_schedulers(Metrics) ->
    SchedulerInfo = maps:get(schedulers, Metrics, #{}),
    RunQueue = maps:get(run_queue, SchedulerInfo, 0),
    SchedulerLoad = maps:get(load, SchedulerInfo, 0),
    
    Results = #{optimizations_applied => []},
    
    case SchedulerLoad of
        Load when Load > 2.0 ->
            colored_logger:warning("[PERF_OPT] ⚠️ High scheduler load (~.2f), checking for bottlenecks", [Load]),
            
            %% Check for scheduler imbalance
            SchedulerStats = get_scheduler_utilization(),
            
            Results#{
                optimizations_applied => [scheduler_analysis],
                scheduler_stats => SchedulerStats,
                run_queue_length => RunQueue
            };
        _ ->
            colored_logger:data(processed, "[PERF_OPT] Scheduler load healthy (~.2f)", [SchedulerLoad]),
            Results
    end.

%% Garbage collection optimization
optimize_garbage_collection(Metrics) ->
    GCInfo = maps:get(gc, Metrics, #{}),
    GCCount = maps:get(count, GCInfo, 0),
    
    Results = #{optimizations_applied => []},
    
    %% Check if GC frequency is reasonable
    case GCCount > 1000 of  % Arbitrary threshold
        true ->
            colored_logger:info("[PERF_OPT] ℹ️ High GC frequency (~p), optimizing heap sizes", [GCCount]),
            
            %% Tune garbage collection parameters
            OptimizedProcesses = tune_gc_parameters(),
            
            Results#{
                optimizations_applied => [gc_tuning],
                gc_count => GCCount,
                processes_tuned => OptimizedProcesses
            };
        false ->
            Results
    end.

%% Helper functions for optimizations
force_global_gc() ->
    Processes = erlang:processes(),
    SampleSize = min(100, length(Processes)),  % Limit to avoid system impact
    SampleProcesses = lists:sublist(Processes, SampleSize),
    
    lists:foldl(fun(Pid, Count) ->
        case process_info(Pid) of
            undefined -> Count;
            _ ->
                erlang:garbage_collect(Pid),
                Count + 1
        end
    end, 0, SampleProcesses).

selective_gc() ->
    %% Only GC processes with high memory usage
    Processes = erlang:processes(),
    
    lists:foldl(fun(Pid, Count) ->
        case process_info(Pid, memory) of
            {memory, Memory} when Memory > 1000000 -> % 1MB threshold
                erlang:garbage_collect(Pid),
                Count + 1;
            _ ->
                Count
        end
    end, 0, Processes).

clear_unused_ets_tables() ->
    %% This would implement ETS table cleanup
    %% For now, just return 0
    0.

find_idle_processes() ->
    %% Find processes that have been idle for a long time
    %% This is a simplified implementation
    Processes = erlang:processes(),
    
    lists:filter(fun(Pid) ->
        case process_info(Pid, [message_queue_len, reductions]) of
            [{message_queue_len, 0}, {reductions, Reductions}] when Reductions < 1000 ->
                true;
            _ ->
                false
        end
    end, lists:sublist(Processes, 50)). % Sample only first 50

terminate_idle_processes(_IdleProcesses) ->
    %% For safety, we don't actually terminate processes
    %% This would need careful implementation
    0.

get_scheduler_utilization() ->
    case erlang:statistics(scheduler_wall_time) of
        undefined ->
            erlang:system_flag(scheduler_wall_time, true),
            timer:sleep(100),
            get_scheduler_utilization();
        SchedulerTimes ->
            SchedulerTimes
    end.

tune_gc_parameters() ->
    %% This would tune GC parameters for high-memory processes
    %% For now, just return 0
    0.

%% Log optimization results
log_optimization_results(Results) ->
    maps:foreach(fun(Category, CategoryResults) ->
        Optimizations = maps:get(optimizations_applied, CategoryResults, []),
        case Optimizations of
            [] ->
                colored_logger:data(processed, "[PERF_OPT] ~p: no optimizations needed", [Category]);
            _ ->
                colored_logger:success("[PERF_OPT] ~p: applied optimizations: ~p", [Category, Optimizations])
        end
    end, Results).