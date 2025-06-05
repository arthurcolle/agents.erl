%%%-------------------------------------------------------------------
%%% @doc Metacognitive Code Analyzer
%%% This module enables agents to think about their thinking process
%%% while generating code. It provides metacognitive awareness of:
%%% - Decision-making processes
%%% - Alternative approaches considered
%%% - Reasoning chains
%%% - Uncertainty and confidence levels
%%% - Learning from previous code generation
%%% @end
%%%-------------------------------------------------------------------
-module(metacognitive_code_analyzer).

-behaviour(gen_server).

%% API
-export([start_link/0,
         analyze_decision/3,
         record_alternative/3,
         evaluate_confidence/2,
         reflect_on_process/2,
         get_metacognitive_report/1,
         enable_for_agent/1,
         disable_for_agent/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    agent_sessions = #{} :: #{pid() => session()},
    enabled_agents = [] :: [pid()],
    learning_database = #{} :: #{pattern() => outcome()}
}).

-record(session, {
    agent_pid :: pid(),
    decisions = [] :: [decision()],
    alternatives = [] :: [alternative()],
    confidence_levels = [] :: float(),
    reasoning_chains = [] :: [reasoning()],
    start_time :: erlang:timestamp()
}).

-record(decision, {
    timestamp :: erlang:timestamp(),
    context :: binary(),
    choice :: binary(),
    reasoning :: binary(),
    confidence :: float(),
    alternatives_considered :: integer()
}).

-record(alternative, {
    timestamp :: erlang:timestamp(),
    option :: binary(),
    pros :: [binary()],
    cons :: [binary()],
    rejection_reason :: binary()
}).

-record(reasoning, {
    step :: integer(),
    thought :: binary(),
    conclusion :: binary(),
    uncertainty :: float()
}).

-type pattern() :: binary().
-type outcome() :: #{success := boolean(), metrics := map()}.
-type session() :: #session{}.
-type decision() :: #decision{}.
-type alternative() :: #alternative{}.
-type reasoning() :: #reasoning{}.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Analyze a code generation decision
analyze_decision(AgentPid, Context, Decision) ->
    gen_server:call(?SERVER, {analyze_decision, AgentPid, Context, Decision}).

%% @doc Record an alternative approach that was considered but not taken
record_alternative(AgentPid, Alternative, RejectionReason) ->
    gen_server:call(?SERVER, {record_alternative, AgentPid, Alternative, RejectionReason}).

%% @doc Evaluate confidence level for current code generation
evaluate_confidence(AgentPid, CodeContext) ->
    gen_server:call(?SERVER, {evaluate_confidence, AgentPid, CodeContext}).

%% @doc Reflect on the code generation process
reflect_on_process(AgentPid, GeneratedCode) ->
    gen_server:call(?SERVER, {reflect_on_process, AgentPid, GeneratedCode}, infinity).

%% @doc Get a comprehensive metacognitive report for an agent
get_metacognitive_report(AgentPid) ->
    gen_server:call(?SERVER, {get_report, AgentPid}).

%% @doc Enable metacognitive analysis for an agent
enable_for_agent(AgentPid) ->
    gen_server:call(?SERVER, {enable, AgentPid}).

%% @doc Disable metacognitive analysis for an agent
disable_for_agent(AgentPid) ->
    gen_server:call(?SERVER, {disable, AgentPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    io:format("[METACOGNITIVE] Starting metacognitive code analyzer~n"),
    {ok, #state{}}.

handle_call({analyze_decision, AgentPid, Context, DecisionData}, _From, State) ->
    case lists:member(AgentPid, State#state.enabled_agents) of
        true ->
            Session = get_or_create_session(State, AgentPid),
            
            %% Analyze the decision
            Analysis = perform_decision_analysis(Context, DecisionData, State#state.learning_database),
            
            %% Record the decision
            Decision = #decision{
                timestamp = erlang:timestamp(),
                context = ensure_binary(Context),
                choice = maps:get(choice, DecisionData),
                reasoning = maps:get(reasoning, DecisionData),
                confidence = maps:get(confidence, Analysis),
                alternatives_considered = maps:get(alternatives_count, DecisionData, 0)
            },
            
            UpdatedSession = Session#session{
                decisions = [Decision | Session#session.decisions]
            },
            
            NewState = update_session(State, AgentPid, UpdatedSession),
            {reply, {ok, Analysis}, NewState};
        false ->
            {reply, {error, not_enabled}, State}
    end;

handle_call({record_alternative, AgentPid, AlternativeData, RejectionReason}, _From, State) ->
    case lists:member(AgentPid, State#state.enabled_agents) of
        true ->
            Session = get_or_create_session(State, AgentPid),
            
            %% Analyze why this alternative was rejected
            Analysis = analyze_alternative_rejection(AlternativeData, RejectionReason),
            
            Alternative = #alternative{
                timestamp = erlang:timestamp(),
                option = maps:get(option, AlternativeData),
                pros = maps:get(pros, AlternativeData, []),
                cons = maps:get(cons, AlternativeData, []),
                rejection_reason = ensure_binary(RejectionReason)
            },
            
            UpdatedSession = Session#session{
                alternatives = [Alternative | Session#session.alternatives]
            },
            
            NewState = update_session(State, AgentPid, UpdatedSession),
            {reply, {ok, Analysis}, NewState};
        false ->
            {reply, {error, not_enabled}, State}
    end;

handle_call({evaluate_confidence, AgentPid, CodeContext}, _From, State) ->
    case lists:member(AgentPid, State#state.enabled_agents) of
        true ->
            Session = get_or_create_session(State, AgentPid),
            
            %% Evaluate confidence based on multiple factors
            Confidence = calculate_confidence(CodeContext, Session, State#state.learning_database),
            
            UpdatedSession = Session#session{
                confidence_levels = [Confidence | Session#session.confidence_levels]
            },
            
            NewState = update_session(State, AgentPid, UpdatedSession),
            
            %% Provide detailed confidence analysis
            ConfidenceAnalysis = #{
                level => Confidence,
                factors => analyze_confidence_factors(CodeContext, Session),
                recommendation => generate_confidence_recommendation(Confidence)
            },
            
            {reply, {ok, ConfidenceAnalysis}, NewState};
        false ->
            {reply, {error, not_enabled}, State}
    end;

handle_call({reflect_on_process, AgentPid, GeneratedCode}, _From, State) ->
    case lists:member(AgentPid, State#state.enabled_agents) of
        true ->
            Session = get_or_create_session(State, AgentPid),
            
            %% Perform deep reflection on the entire process
            Reflection = perform_process_reflection(Session, GeneratedCode, State#state.learning_database),
            
            %% Update learning database
            NewLearningData = extract_learning_patterns(Session, GeneratedCode, Reflection),
            UpdatedLearningDB = update_learning_database(State#state.learning_database, NewLearningData),
            
            %% Generate metacognitive insights
            Insights = generate_metacognitive_insights(Session, Reflection),
            
            NewState = State#state{learning_database = UpdatedLearningDB},
            
            {reply, {ok, Reflection, Insights}, NewState};
        false ->
            {reply, {error, not_enabled}, State}
    end;

handle_call({get_report, AgentPid}, _From, State) ->
    case maps:get(AgentPid, State#state.agent_sessions, undefined) of
        undefined ->
            {reply, {error, no_session}, State};
        Session ->
            Report = generate_metacognitive_report(Session, State#state.learning_database),
            {reply, {ok, Report}, State}
    end;

handle_call({enable, AgentPid}, _From, State) ->
    NewAgents = case lists:member(AgentPid, State#state.enabled_agents) of
        true -> State#state.enabled_agents;
        false -> [AgentPid | State#state.enabled_agents]
    end,
    {reply, ok, State#state{enabled_agents = NewAgents}};

handle_call({disable, AgentPid}, _From, State) ->
    NewAgents = lists:delete(AgentPid, State#state.enabled_agents),
    {reply, ok, State#state{enabled_agents = NewAgents}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_or_create_session(State, AgentPid) ->
    case maps:get(AgentPid, State#state.agent_sessions, undefined) of
        undefined ->
            #session{
                agent_pid = AgentPid,
                start_time = erlang:timestamp()
            };
        Session ->
            Session
    end.

update_session(State, AgentPid, Session) ->
    State#state{
        agent_sessions = maps:put(AgentPid, Session, State#state.agent_sessions)
    }.

perform_decision_analysis(Context, DecisionData, LearningDB) ->
    %% Analyze the decision in context
    ContextAnalysis = analyze_context(Context),
    
    %% Check against learned patterns
    PatternMatch = find_matching_patterns(Context, LearningDB),
    
    %% Calculate confidence based on analysis
    BaseConfidence = maps:get(confidence, DecisionData, 0.5),
    AdjustedConfidence = adjust_confidence_by_patterns(BaseConfidence, PatternMatch),
    
    #{
        context_type => ContextAnalysis,
        pattern_matches => PatternMatch,
        confidence => AdjustedConfidence,
        reasoning_quality => evaluate_reasoning_quality(maps:get(reasoning, DecisionData, <<>>))
    }.

analyze_alternative_rejection(AlternativeData, RejectionReason) ->
    %% Analyze why this alternative was rejected
    Pros = maps:get(pros, AlternativeData, []),
    Cons = maps:get(cons, AlternativeData, []),
    
    %% Evaluate the decision
    ProWeight = length(Pros),
    ConWeight = length(Cons),
    
    #{
        balance => ConWeight - ProWeight,
        rejection_type => categorize_rejection(RejectionReason),
        learning_opportunity => ProWeight > ConWeight
    }.

calculate_confidence(CodeContext, Session, LearningDB) ->
    %% Multiple factors contribute to confidence
    Factors = [
        historical_success_rate(Session, LearningDB),
        pattern_familiarity(CodeContext, LearningDB),
        decision_consistency(Session),
        alternatives_thoroughness(Session)
    ],
    
    %% Weighted average
    Weights = [0.3, 0.3, 0.2, 0.2],
    WeightedSum = lists:sum([F * W || {F, W} <- lists:zip(Factors, Weights)]),
    
    %% Ensure confidence is between 0 and 1
    max(0.0, min(1.0, WeightedSum)).

analyze_confidence_factors(CodeContext, Session) ->
    #{
        decisions_made => length(Session#session.decisions),
        alternatives_considered => length(Session#session.alternatives),
        average_confidence => case Session#session.confidence_levels of
            [] -> 0.5;
            Levels -> lists:sum(Levels) / length(Levels)
        end,
        context_complexity => estimate_context_complexity(CodeContext)
    }.

generate_confidence_recommendation(Confidence) when Confidence < 0.3 ->
    <<"Low confidence detected. Consider exploring more alternatives or seeking additional context.">>;
generate_confidence_recommendation(Confidence) when Confidence < 0.6 ->
    <<"Moderate confidence. The approach seems reasonable but may benefit from validation.">>;
generate_confidence_recommendation(Confidence) when Confidence < 0.8 ->
    <<"Good confidence level. The chosen approach aligns well with learned patterns.">>;
generate_confidence_recommendation(_) ->
    <<"High confidence. The decision is well-supported by experience and analysis.">>.

perform_process_reflection(Session, GeneratedCode, LearningDB) ->
    %% Reflect on the entire code generation process
    #{
        process_summary => summarize_process(Session),
        decision_path => reconstruct_decision_path(Session),
        alternative_analysis => analyze_unexplored_paths(Session),
        pattern_recognition => identify_patterns_used(GeneratedCode, LearningDB),
        metacognitive_assessment => assess_thinking_process(Session),
        learning_moments => identify_learning_moments(Session),
        improvement_suggestions => generate_improvement_suggestions(Session, GeneratedCode)
    }.

extract_learning_patterns(Session, GeneratedCode, Reflection) ->
    %% Extract patterns that can be learned from this session
    Patterns = [],
    
    %% Pattern: Decision context -> Outcome
    DecisionPatterns = lists:map(fun(#decision{context = Ctx, choice = Choice, confidence = Conf}) ->
        #{
            pattern => create_pattern_key(Ctx, Choice),
            confidence => Conf,
            success => evaluate_decision_success(Choice, GeneratedCode)
        }
    end, Session#session.decisions),
    
    %% Pattern: Alternative rejection reasons
    RejectionPatterns = lists:map(fun(#alternative{option = Opt, rejection_reason = Reason}) ->
        #{
            pattern => create_rejection_pattern(Opt, Reason),
            valid_rejection => true  % Could be evaluated more sophisticatedly
        }
    end, Session#session.alternatives),
    
    DecisionPatterns ++ RejectionPatterns ++ Patterns.

update_learning_database(LearningDB, NewLearningData) ->
    %% Update the learning database with new patterns
    lists:foldl(fun(#{pattern := Pattern} = Data, DB) ->
        Existing = maps:get(Pattern, DB, #{occurrences => 0, outcomes => []}),
        Updated = Existing#{
            occurrences => maps:get(occurrences, Existing) + 1,
            outcomes => [Data | maps:get(outcomes, Existing)]
        },
        maps:put(Pattern, Updated, DB)
    end, LearningDB, NewLearningData).

generate_metacognitive_insights(Session, Reflection) ->
    %% Generate high-level insights about the thinking process
    #{
        thinking_style => identify_thinking_style(Session),
        decision_patterns => extract_decision_patterns(Session),
        confidence_trajectory => analyze_confidence_trajectory(Session),
        alternative_exploration => rate_alternative_exploration(Session),
        metacognitive_maturity => assess_metacognitive_maturity(Session, Reflection),
        recommendations => generate_metacognitive_recommendations(Session, Reflection)
    }.

generate_metacognitive_report(Session, LearningDB) ->
    %% Generate comprehensive report
    #{
        session_duration => timer:now_diff(erlang:timestamp(), Session#session.start_time) / 1000000,
        total_decisions => length(Session#session.decisions),
        total_alternatives => length(Session#session.alternatives),
        confidence_metrics => calculate_confidence_metrics(Session),
        decision_quality => assess_decision_quality(Session, LearningDB),
        thinking_patterns => identify_thinking_patterns(Session),
        learning_progress => assess_learning_progress(Session, LearningDB),
        detailed_timeline => create_decision_timeline(Session)
    }.

%% Helper functions

analyze_context(Context) ->
    %% Analyze the type and complexity of the context
    ContextBin = ensure_binary(Context),
    Size = byte_size(ContextBin),
    
    if
        Size < 100 -> simple;
        Size < 500 -> moderate;
        true -> complex
    end.

find_matching_patterns(Context, LearningDB) ->
    %% Find patterns in the learning database that match this context
    ContextKey = create_context_key(Context),
    
    maps:fold(fun(Pattern, Data, Matches) ->
        case pattern_matches(Pattern, ContextKey) of
            true -> [#{pattern => Pattern, data => Data} | Matches];
            false -> Matches
        end
    end, [], LearningDB).

adjust_confidence_by_patterns(BaseConfidence, PatternMatches) ->
    %% Adjust confidence based on historical pattern success
    case PatternMatches of
        [] -> BaseConfidence;
        Matches ->
            %% Calculate average success rate from matches
            SuccessRates = lists:map(fun(#{data := Data}) ->
                calculate_pattern_success_rate(Data)
            end, Matches),
            
            AvgSuccess = lists:sum(SuccessRates) / length(SuccessRates),
            
            %% Adjust confidence
            BaseConfidence * 0.7 + AvgSuccess * 0.3
    end.

evaluate_reasoning_quality(Reasoning) ->
    %% Evaluate the quality of reasoning
    ReasoningBin = ensure_binary(Reasoning),
    
    %% Simple heuristics for reasoning quality
    Length = byte_size(ReasoningBin),
    HasBecause = binary:match(ReasoningBin, <<"because">>) =/= nomatch,
    HasTherefore = binary:match(ReasoningBin, <<"therefore">>) =/= nomatch,
    
    Quality = 0.3 +  % Base score
              (min(Length, 200) / 200 * 0.3) +  % Length contributes
              (if HasBecause -> 0.2; true -> 0 end) +
              (if HasTherefore -> 0.2; true -> 0 end),
    
    min(1.0, Quality).

categorize_rejection(RejectionReason) ->
    %% Categorize why an alternative was rejected
    ReasonBin = ensure_binary(RejectionReason),
    
    Patterns = [
        {<<"complex">>, too_complex},
        {<<"performance">>, performance_concerns},
        {<<"maintain">>, maintainability},
        {<<"simple">>, prefer_simpler},
        {<<"pattern">>, pattern_mismatch}
    ],
    
    case lists:dropwhile(fun({Pattern, _}) ->
        binary:match(ReasonBin, Pattern) =:= nomatch
    end, Patterns) of
        [{_, Category} | _] -> Category;
        [] -> other
    end.

historical_success_rate(Session, LearningDB) ->
    %% Calculate historical success rate for similar decisions
    0.7.  % Simplified for now

pattern_familiarity(CodeContext, LearningDB) ->
    %% How familiar is this pattern based on learning database
    ContextKey = create_context_key(CodeContext),
    case maps:get(ContextKey, LearningDB, undefined) of
        undefined -> 0.3;
        Data -> min(1.0, maps:get(occurrences, Data, 0) / 10)
    end.

decision_consistency(Session) ->
    %% How consistent are the decisions in this session
    case Session#session.decisions of
        [] -> 0.5;
        [_] -> 0.7;
        Decisions ->
            %% Check for consistency in reasoning patterns
            0.8  % Simplified
    end.

alternatives_thoroughness(Session) ->
    %% How thoroughly were alternatives explored
    DecisionCount = length(Session#session.decisions),
    AlternativeCount = length(Session#session.alternatives),
    
    if
        DecisionCount =:= 0 -> 0.5;
        true ->
            Ratio = AlternativeCount / DecisionCount,
            min(1.0, Ratio / 3)  % Expect ~3 alternatives per decision for good thoroughness
    end.

estimate_context_complexity(CodeContext) ->
    %% Estimate complexity of the code context
    moderate.  % Simplified

summarize_process(Session) ->
    #{
        total_decisions => length(Session#session.decisions),
        total_alternatives => length(Session#session.alternatives),
        avg_confidence => case Session#session.confidence_levels of
            [] -> 0.5;
            Levels -> lists:sum(Levels) / length(Levels)
        end,
        duration => timer:now_diff(erlang:timestamp(), Session#session.start_time) / 1000000
    }.

reconstruct_decision_path(Session) ->
    %% Reconstruct the path of decisions made
    lists:reverse(lists:map(fun(#decision{timestamp = TS, choice = Choice, confidence = Conf}) ->
        #{
            timestamp => TS,
            choice => Choice,
            confidence => Conf
        }
    end, Session#session.decisions)).

analyze_unexplored_paths(Session) ->
    %% Analyze alternatives that weren't explored
    lists:map(fun(#alternative{option = Opt, pros = Pros, cons = Cons, rejection_reason = Reason}) ->
        #{
            option => Opt,
            potential_value => length(Pros) - length(Cons),
            rejection => Reason
        }
    end, Session#session.alternatives).

identify_patterns_used(_GeneratedCode, _LearningDB) ->
    %% Identify which learned patterns were used
    [].  % Simplified

assess_thinking_process(Session) ->
    %% Assess the quality of the thinking process
    #{
        depth => length(Session#session.reasoning_chains),
        breadth => length(Session#session.alternatives),
        quality => evaluate_overall_thinking_quality(Session)
    }.

identify_learning_moments(Session) ->
    %% Identify key learning moments in the session
    lists:filtermap(fun(#decision{confidence = Conf, alternatives_considered = AltCount}) ->
        if
            Conf < 0.5 andalso AltCount > 2 ->
                {true, <<"Low confidence despite considering alternatives - learning opportunity">>};
            Conf > 0.8 andalso AltCount == 0 ->
                {true, <<"High confidence without alternatives - potential overconfidence">>};
            true ->
                false
        end
    end, Session#session.decisions).

generate_improvement_suggestions(Session, _GeneratedCode) ->
    Suggestions = [],
    
    %% Check for low alternative exploration
    AvgAlternatives = length(Session#session.alternatives) / max(1, length(Session#session.decisions)),
    S1 = if
        AvgAlternatives < 1 ->
            [<<"Consider exploring more alternatives before making decisions">> | Suggestions];
        true ->
            Suggestions
    end,
    
    %% Check for confidence patterns
    ConfLevels = Session#session.confidence_levels,
    AvgConfidence = case ConfLevels of
        [] -> 0.5;
        _ -> lists:sum(ConfLevels) / length(ConfLevels)
    end,
    
    S2 = if
        AvgConfidence < 0.4 ->
            [<<"Low overall confidence - consider building more domain knowledge">> | S1];
        AvgConfidence > 0.9 ->
            [<<"Very high confidence - ensure not overlooking edge cases">> | S1];
        true ->
            S1
    end,
    
    S2.

create_pattern_key(Context, Choice) ->
    %% Create a key for pattern matching
    crypto:hash(sha256, <<(ensure_binary(Context))/binary, (ensure_binary(Choice))/binary>>).

evaluate_decision_success(_Choice, _GeneratedCode) ->
    %% Evaluate if a decision was successful
    true.  % Simplified

create_rejection_pattern(Option, Reason) ->
    %% Create a pattern for rejection reasons
    crypto:hash(sha256, <<(ensure_binary(Option))/binary, (ensure_binary(Reason))/binary>>).

create_context_key(Context) ->
    %% Create a key from context for pattern matching
    crypto:hash(sha256, ensure_binary(Context)).

pattern_matches(Pattern, ContextKey) ->
    %% Check if a pattern matches the context
    Pattern =:= ContextKey.  % Simplified

calculate_pattern_success_rate(Data) ->
    %% Calculate success rate for a pattern
    Outcomes = maps:get(outcomes, Data, []),
    case Outcomes of
        [] -> 0.5;
        _ ->
            Successes = length([O || O <- Outcomes, maps:get(success, O, false)]),
            Successes / length(Outcomes)
    end.

identify_thinking_style(Session) ->
    %% Identify the agent's thinking style
    DecisionCount = length(Session#session.decisions),
    AlternativeCount = length(Session#session.alternatives),
    
    if
        AlternativeCount > DecisionCount * 2 -> exploratory;
        AlternativeCount < DecisionCount -> decisive;
        true -> balanced
    end.

extract_decision_patterns(Session) ->
    %% Extract patterns in decision-making
    #{
        avg_alternatives_per_decision => length(Session#session.alternatives) / max(1, length(Session#session.decisions)),
        confidence_distribution => analyze_confidence_distribution(Session#session.confidence_levels)
    }.

analyze_confidence_trajectory(Session) ->
    %% Analyze how confidence changed over time
    case Session#session.confidence_levels of
        [] -> stable;
        [_] -> insufficient_data;
        Levels ->
            First = hd(lists:reverse(Levels)),
            Last = hd(Levels),
            if
                Last > First + 0.2 -> improving;
                Last < First - 0.2 -> declining;
                true -> stable
            end
    end.

rate_alternative_exploration(Session) ->
    %% Rate how well alternatives were explored
    Score = alternatives_thoroughness(Session),
    if
        Score > 0.8 -> excellent;
        Score > 0.6 -> good;
        Score > 0.4 -> adequate;
        true -> needs_improvement
    end.

assess_metacognitive_maturity(Session, _Reflection) ->
    %% Assess the maturity of metacognitive processes
    Factors = [
        length(Session#session.decisions) > 0,
        length(Session#session.alternatives) > 0,
        length(Session#session.reasoning_chains) > 0,
        Session#session.confidence_levels =/= []
    ],
    
    MaturityScore = length([true || true <- Factors]) / length(Factors),
    
    if
        MaturityScore > 0.8 -> mature;
        MaturityScore > 0.5 -> developing;
        true -> nascent
    end.

generate_metacognitive_recommendations(Session, Reflection) ->
    %% Generate recommendations for improving metacognitive processes
    generate_improvement_suggestions(Session, <<>>).

calculate_confidence_metrics(Session) ->
    case Session#session.confidence_levels of
        [] -> #{average => 0.5, min => 0.5, max => 0.5, stddev => 0.0};
        Levels ->
            Avg = lists:sum(Levels) / length(Levels),
            Min = lists:min(Levels),
            Max = lists:max(Levels),
            
            %% Calculate standard deviation
            Variance = lists:sum([(L - Avg) * (L - Avg) || L <- Levels]) / length(Levels),
            StdDev = math:sqrt(Variance),
            
            #{average => Avg, min => Min, max => Max, stddev => StdDev}
    end.

assess_decision_quality(Session, _LearningDB) ->
    %% Assess overall quality of decisions
    QualityFactors = [
        {reasoning_quality, average_reasoning_quality(Session)},
        {alternative_consideration, alternatives_thoroughness(Session)},
        {confidence_appropriateness, assess_confidence_appropriateness(Session)}
    ],
    
    maps:from_list(QualityFactors).

identify_thinking_patterns(Session) ->
    %% Identify patterns in thinking
    [identify_thinking_style(Session)].

assess_learning_progress(Session, LearningDB) ->
    %% Assess learning progress
    #{
        patterns_learned => maps:size(LearningDB),
        session_contribution => length(extract_learning_patterns(Session, <<>>, #{}))
    }.

create_decision_timeline(Session) ->
    %% Create a timeline of decisions and alternatives
    AllEvents = 
        [{decision, D} || D <- Session#session.decisions] ++
        [{alternative, A} || A <- Session#session.alternatives],
    
    %% Sort by timestamp
    Sorted = lists:sort(fun({_, #decision{timestamp = T1}}, {_, #decision{timestamp = T2}}) ->
                            T1 =< T2;
                        ({_, #alternative{timestamp = T1}}, {_, #alternative{timestamp = T2}}) ->
                            T1 =< T2;
                        ({_, #decision{timestamp = T1}}, {_, #alternative{timestamp = T2}}) ->
                            T1 =< T2;
                        ({_, #alternative{timestamp = T1}}, {_, #decision{timestamp = T2}}) ->
                            T1 =< T2
                     end, AllEvents),
    
    [format_timeline_event(Event) || Event <- Sorted].

format_timeline_event({decision, #decision{timestamp = TS, choice = Choice, confidence = Conf}}) ->
    #{type => decision, timestamp => TS, choice => Choice, confidence => Conf};
format_timeline_event({alternative, #alternative{timestamp = TS, option = Opt, rejection_reason = Reason}}) ->
    #{type => alternative, timestamp => TS, option => Opt, rejection => Reason}.

average_reasoning_quality(Session) ->
    case Session#session.decisions of
        [] -> 0.5;
        Decisions ->
            Qualities = [evaluate_reasoning_quality(D#decision.reasoning) || D <- Decisions],
            lists:sum(Qualities) / length(Qualities)
    end.

assess_confidence_appropriateness(Session) ->
    %% Assess if confidence levels were appropriate
    0.7.  % Simplified

analyze_confidence_distribution(Levels) ->
    case Levels of
        [] -> uniform;
        _ ->
            Avg = lists:sum(Levels) / length(Levels),
            Variance = lists:sum([(L - Avg) * (L - Avg) || L <- Levels]) / length(Levels),
            
            if
                Variance < 0.01 -> uniform;
                Variance < 0.1 -> normal;
                true -> varied
            end
    end.

evaluate_overall_thinking_quality(Session) ->
    %% Evaluate overall quality of thinking
    Factors = [
        average_reasoning_quality(Session),
        alternatives_thoroughness(Session),
        decision_consistency(Session)
    ],
    
    lists:sum(Factors) / length(Factors).

ensure_binary(Data) when is_binary(Data) -> Data;
ensure_binary(Data) when is_list(Data) -> list_to_binary(Data);
ensure_binary(Data) -> list_to_binary(io_lib:format("~p", [Data])).