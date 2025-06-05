%%%-------------------------------------------------------------------
%%% @doc Unified Feedback System
%%% Aggregates and processes feedback from all system components including
%%% errors, performance metrics, self-healing actions, code modifications,
%%% and agent behaviors. Provides a centralized feedback loop for system
%%% improvement and learning.
%%% @end
%%%-------------------------------------------------------------------
-module(unified_feedback_system).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([report_error/2,
         report_performance/2,
         report_healing_action/2,
         report_code_modification/2,
         report_agent_behavior/2,
         get_feedback_summary/0,
         get_feedback_summary/1,
         analyze_patterns/0,
         get_recommendations/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(ANALYSIS_INTERVAL, 30000). % 30 seconds

-record(state, {
    error_feedback = [],         % Error occurrences and fixes
    performance_feedback = [],   % Performance metrics
    healing_feedback = [],       % Self-healing actions taken
    modification_feedback = [],  % Code modifications made
    behavior_feedback = [],      % Agent behavior patterns
    
    %% Aggregated insights
    patterns = #{},             % Identified patterns
    recommendations = [],       % System recommendations
    correlations = #{},        % Cross-system correlations
    
    %% Analysis state
    last_analysis = 0,         % Last analysis timestamp
    analysis_count = 0         % Number of analyses performed
}).

-record(feedback_entry, {
    timestamp,
    source,      % System that generated feedback
    type,        % error | performance | healing | modification | behavior
    data,        % Actual feedback data
    impact       % Estimated impact level
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Report an error occurrence
report_error(Source, ErrorData) ->
    gen_server:cast(?SERVER, {feedback, error, Source, ErrorData}).

%% @doc Report performance metrics
report_performance(Source, PerfData) ->
    gen_server:cast(?SERVER, {feedback, performance, Source, PerfData}).

%% @doc Report a self-healing action
report_healing_action(Source, HealingData) ->
    gen_server:cast(?SERVER, {feedback, healing, Source, HealingData}).

%% @doc Report a code modification
report_code_modification(Source, ModData) ->
    gen_server:cast(?SERVER, {feedback, modification, Source, ModData}).

%% @doc Report agent behavior
report_agent_behavior(Source, BehaviorData) ->
    gen_server:cast(?SERVER, {feedback, behavior, Source, BehaviorData}).

%% @doc Get summary of all feedback
get_feedback_summary() ->
    gen_server:call(?SERVER, get_summary).

%% @doc Get summary of specific feedback type
get_feedback_summary(Type) ->
    gen_server:call(?SERVER, {get_summary, Type}).

%% @doc Analyze patterns in feedback
analyze_patterns() ->
    gen_server:call(?SERVER, analyze_patterns).

%% @doc Get system recommendations based on feedback
get_recommendations() ->
    gen_server:call(?SERVER, get_recommendations).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Start periodic analysis
    erlang:send_after(?ANALYSIS_INTERVAL, self(), perform_analysis),
    
    %% Register with meta-layer coordinator
    self() ! register_with_coordinator,
    
    {ok, #state{last_analysis = erlang:system_time(millisecond)}}.

handle_call(get_summary, _From, State) ->
    Summary = #{
        error_count => length(State#state.error_feedback),
        performance_count => length(State#state.performance_feedback),
        healing_count => length(State#state.healing_feedback),
        modification_count => length(State#state.modification_feedback),
        behavior_count => length(State#state.behavior_feedback),
        patterns => State#state.patterns,
        last_analysis => State#state.last_analysis,
        analysis_count => State#state.analysis_count
    },
    {reply, {ok, Summary}, State};

handle_call({get_summary, Type}, _From, State) ->
    Feedback = get_feedback_by_type(Type, State),
    Summary = create_type_summary(Type, Feedback),
    {reply, {ok, Summary}, State};

handle_call(analyze_patterns, _From, State) ->
    Patterns = analyze_feedback_patterns(State),
    {reply, {ok, Patterns}, State#state{patterns = Patterns}};

handle_call(get_recommendations, _From, State) ->
    {reply, {ok, State#state.recommendations}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({feedback, Type, Source, Data}, State) ->
    Entry = #feedback_entry{
        timestamp = erlang:system_time(millisecond),
        source = Source,
        type = Type,
        data = Data,
        impact = calculate_impact(Type, Data)
    },
    
    NewState = store_feedback(Entry, State),
    
    %% Check if immediate action needed
    check_immediate_action(Entry, NewState),
    
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(register_with_coordinator, State) ->
    case erlang:whereis(meta_layer_coordinator) of
        undefined ->
            %% Retry later
            erlang:send_after(5000, self(), register_with_coordinator);
        _Pid ->
            meta_layer_coordinator:register_meta_system(unified_feedback_system, self())
    end,
    {noreply, State};

handle_info(perform_analysis, State) ->
    %% Perform comprehensive analysis
    NewState = comprehensive_analysis(State),
    
    %% Schedule next analysis
    erlang:send_after(?ANALYSIS_INTERVAL, self(), perform_analysis),
    
    {noreply, NewState};

handle_info({meta_event, EventType, EventData}, State) ->
    %% Handle events from meta-layer coordinator
    handle_meta_event(EventType, EventData, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

store_feedback(#feedback_entry{type = error} = Entry, State) ->
    State#state{
        error_feedback = [Entry | limit_feedback(State#state.error_feedback, 1000)]
    };
store_feedback(#feedback_entry{type = performance} = Entry, State) ->
    State#state{
        performance_feedback = [Entry | limit_feedback(State#state.performance_feedback, 1000)]
    };
store_feedback(#feedback_entry{type = healing} = Entry, State) ->
    State#state{
        healing_feedback = [Entry | limit_feedback(State#state.healing_feedback, 1000)]
    };
store_feedback(#feedback_entry{type = modification} = Entry, State) ->
    State#state{
        modification_feedback = [Entry | limit_feedback(State#state.modification_feedback, 1000)]
    };
store_feedback(#feedback_entry{type = behavior} = Entry, State) ->
    State#state{
        behavior_feedback = [Entry | limit_feedback(State#state.behavior_feedback, 1000)]
    }.

limit_feedback(List, MaxSize) when length(List) >= MaxSize ->
    lists:sublist(List, MaxSize - 1);
limit_feedback(List, _) ->
    List.

calculate_impact(error, #{severity := critical}) -> high;
calculate_impact(error, #{severity := high}) -> high;
calculate_impact(error, _) -> medium;
calculate_impact(performance, #{degradation := D}) when D > 50 -> high;
calculate_impact(performance, _) -> low;
calculate_impact(healing, #{success := false}) -> high;
calculate_impact(healing, _) -> medium;
calculate_impact(modification, #{scope := system}) -> high;
calculate_impact(modification, _) -> medium;
calculate_impact(behavior, #{anomaly := true}) -> high;
calculate_impact(behavior, _) -> low.

check_immediate_action(#feedback_entry{impact = high, type = error, data = Data}, _State) ->
    %% High-impact error - trigger immediate response
    meta_layer_coordinator:coordinate_cross_system_action(
        error_recovery,
        #{error => Data, priority => high}
    );
check_immediate_action(#feedback_entry{impact = high, type = performance, data = Data}, _State) ->
    %% Performance degradation - trigger optimization
    meta_layer_coordinator:coordinate_cross_system_action(
        system_optimization,
        #{target => maps:get(component, Data, system), metrics => Data}
    );
check_immediate_action(_, _) ->
    ok.

comprehensive_analysis(State) ->
    %% Analyze patterns across all feedback types
    Patterns = analyze_feedback_patterns(State),
    
    %% Find correlations
    Correlations = find_correlations(State),
    
    %% Generate recommendations
    Recommendations = generate_recommendations(Patterns, Correlations),
    
    %% Send insights to meta-layer coordinator
    meta_layer_coordinator:broadcast_meta_event(
        feedback_analysis_complete,
        #{
            patterns => Patterns,
            correlations => Correlations,
            recommendations => Recommendations
        }
    ),
    
    State#state{
        patterns = Patterns,
        correlations = Correlations,
        recommendations = Recommendations,
        last_analysis = erlang:system_time(millisecond),
        analysis_count = State#state.analysis_count + 1
    }.

analyze_feedback_patterns(State) ->
    #{
        error_patterns => analyze_error_patterns(State#state.error_feedback),
        performance_patterns => analyze_performance_patterns(State#state.performance_feedback),
        healing_patterns => analyze_healing_patterns(State#state.healing_feedback),
        modification_patterns => analyze_modification_patterns(State#state.modification_feedback),
        behavior_patterns => analyze_behavior_patterns(State#state.behavior_feedback)
    }.

analyze_error_patterns(Errors) ->
    %% Group errors by type and frequency
    Grouped = lists:foldl(fun(#feedback_entry{data = Data}, Acc) ->
        Type = maps:get(type, Data, unknown),
        maps:update_with(Type, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Errors),
    
    %% Identify recurring errors
    Recurring = maps:filter(fun(_, Count) -> Count > 5 end, Grouped),
    
    #{
        grouped => Grouped,
        recurring => Recurring,
        total => length(Errors)
    }.

analyze_performance_patterns(PerfData) ->
    %% Analyze performance trends
    case PerfData of
        [] -> #{};
        _ ->
            %% Calculate averages and trends
            Metrics = lists:map(fun(#feedback_entry{data = D}) ->
                maps:get(metric_value, D, 0)
            end, PerfData),
            
            #{
                average => lists:sum(Metrics) / length(Metrics),
                trend => calculate_trend(Metrics),
                anomalies => detect_anomalies(Metrics)
            }
    end.

analyze_healing_patterns(HealingData) ->
    %% Analyze self-healing effectiveness
    {Successful, Failed} = lists:partition(
        fun(#feedback_entry{data = D}) ->
            maps:get(success, D, false)
        end,
        HealingData
    ),
    
    #{
        success_rate => length(Successful) / max(1, length(HealingData)),
        common_fixes => extract_common_fixes(Successful),
        failure_reasons => extract_failure_reasons(Failed)
    }.

analyze_modification_patterns(ModData) ->
    %% Analyze code modification patterns
    ModuleChanges = lists:foldl(fun(#feedback_entry{data = D}, Acc) ->
        Module = maps:get(module, D, unknown),
        maps:update_with(Module, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, ModData),
    
    #{
        hot_modules => maps:filter(fun(_, Count) -> Count > 3 end, ModuleChanges),
        total_modifications => length(ModData)
    }.

analyze_behavior_patterns(BehaviorData) ->
    %% Analyze agent behavior patterns
    Anomalies = lists:filter(
        fun(#feedback_entry{data = D}) ->
            maps:get(anomaly, D, false)
        end,
        BehaviorData
    ),
    
    #{
        anomaly_count => length(Anomalies),
        behavior_types => categorize_behaviors(BehaviorData)
    }.

find_correlations(State) ->
    %% Find correlations between different feedback types
    
    %% Error-Healing correlation
    ErrorHealingCorr = calculate_correlation(
        State#state.error_feedback,
        State#state.healing_feedback,
        fun(E, H) ->
            same_time_window(E#feedback_entry.timestamp, H#feedback_entry.timestamp, 5000)
        end
    ),
    
    %% Performance-Modification correlation
    PerfModCorr = calculate_correlation(
        State#state.performance_feedback,
        State#state.modification_feedback,
        fun(P, M) ->
            %% Check if modification improved performance
            same_time_window(P#feedback_entry.timestamp, M#feedback_entry.timestamp, 10000)
        end
    ),
    
    #{
        error_healing => ErrorHealingCorr,
        performance_modification => PerfModCorr
    }.

generate_recommendations(Patterns, _Correlations) ->
    Recommendations = [],
    
    %% Check error patterns
    ErrorPatterns = maps:get(error_patterns, Patterns, #{}),
    R1 = case maps:get(recurring, ErrorPatterns, #{}) of
        Recurring when map_size(Recurring) > 0 ->
            [{focus_on_recurring_errors, Recurring} | Recommendations];
        _ ->
            Recommendations
    end,
    
    %% Check performance patterns
    PerfPatterns = maps:get(performance_patterns, Patterns, #{}),
    R2 = case maps:get(trend, PerfPatterns, stable) of
        declining ->
            [{investigate_performance_degradation, PerfPatterns} | R1];
        _ ->
            R1
    end,
    
    %% Check healing effectiveness
    HealingPatterns = maps:get(healing_patterns, Patterns, #{}),
    R3 = case maps:get(success_rate, HealingPatterns, 1.0) of
        Rate when Rate < 0.7 ->
            [{improve_healing_strategies, HealingPatterns} | R2];
        _ ->
            R2
    end,
    
    R3.

%% Helper functions

get_feedback_by_type(error, State) -> State#state.error_feedback;
get_feedback_by_type(performance, State) -> State#state.performance_feedback;
get_feedback_by_type(healing, State) -> State#state.healing_feedback;
get_feedback_by_type(modification, State) -> State#state.modification_feedback;
get_feedback_by_type(behavior, State) -> State#state.behavior_feedback.

create_type_summary(Type, Feedback) ->
    #{
        type => Type,
        count => length(Feedback),
        recent => lists:sublist(Feedback, 10),
        impact_distribution => count_by_impact(Feedback)
    }.

count_by_impact(Feedback) ->
    lists:foldl(fun(#feedback_entry{impact = Impact}, Acc) ->
        maps:update_with(Impact, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Feedback).

calculate_trend([]) -> stable;
calculate_trend(Metrics) when length(Metrics) < 3 -> stable;
calculate_trend(Metrics) ->
    %% Simple trend detection
    Recent = lists:sublist(Metrics, 5),
    Older = lists:sublist(lists:nthtail(5, Metrics), 5),
    
    RecentAvg = lists:sum(Recent) / length(Recent),
    OlderAvg = lists:sum(Older) / max(1, length(Older)),
    
    if
        RecentAvg > OlderAvg * 1.1 -> improving;
        RecentAvg < OlderAvg * 0.9 -> declining;
        true -> stable
    end.

detect_anomalies([]) -> [];
detect_anomalies(Metrics) ->
    Avg = lists:sum(Metrics) / length(Metrics),
    StdDev = math:sqrt(lists:sum([math:pow(M - Avg, 2) || M <- Metrics]) / length(Metrics)),
    
    %% Find values more than 2 standard deviations from mean
    [{Index, Value} || {Index, Value} <- lists:zip(lists:seq(1, length(Metrics)), Metrics),
                       abs(Value - Avg) > 2 * StdDev].

extract_common_fixes(Successful) ->
    lists:foldl(fun(#feedback_entry{data = D}, Acc) ->
        Fix = maps:get(fix_type, D, unknown),
        maps:update_with(Fix, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Successful).

extract_failure_reasons(Failed) ->
    lists:foldl(fun(#feedback_entry{data = D}, Acc) ->
        Reason = maps:get(failure_reason, D, unknown),
        maps:update_with(Reason, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Failed).

categorize_behaviors(BehaviorData) ->
    lists:foldl(fun(#feedback_entry{data = D}, Acc) ->
        Type = maps:get(behavior_type, D, unknown),
        maps:update_with(Type, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, BehaviorData).

calculate_correlation(List1, List2, CorrelationFun) ->
    Matches = length([{A, B} || A <- List1, B <- List2, CorrelationFun(A, B)]),
    TotalPairs = length(List1) * length(List2),
    
    case TotalPairs of
        0 -> 0.0;
        _ -> Matches / TotalPairs
    end.

same_time_window(T1, T2, Window) ->
    abs(T1 - T2) =< Window.

handle_meta_event(system_down, #{system := SystemName}, State) ->
    %% Record system failure as feedback
    Entry = #feedback_entry{
        timestamp = erlang:system_time(millisecond),
        source = meta_layer_coordinator,
        type = error,
        data = #{type => system_crash, system => SystemName},
        impact = high
    },
    
    NewState = store_feedback(Entry, State),
    {noreply, NewState};

handle_meta_event(_, _, State) ->
    {noreply, State}.