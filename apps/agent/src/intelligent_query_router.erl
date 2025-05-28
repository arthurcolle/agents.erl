%% intelligent_query_router.erl
%% Advanced query routing with multi-agent orchestration
-module(intelligent_query_router).
-behaviour(gen_server).

-export([
    start_link/0,
    route_query/2,
    route_complex_query/3,
    get_routing_analytics/0,
    optimize_routing_strategy/1,
    handle_query_feedback/3
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ROUTING_HISTORY, query_routing_history).
-define(ANALYTICS_TABLE, routing_analytics).

-record(state, {
    routing_strategies = #{},
    performance_cache = #{},
    learning_engine,
    load_balancer,
    circuit_breakers = #{},
    routing_metrics = #{}
}).

-record(query_route, {
    query_id,
    original_query,
    preprocessing_steps = [],
    selected_agents = [],
    routing_strategy,
    coordination_method,
    expected_response_time,
    quality_expectations = #{},
    fallback_options = [],
    started_at,
    completed_at,
    status,
    results = [],
    feedback_scores = #{},
    lessons_learned = []
}).

-record(routing_decision, {
    primary_agent,
    supporting_agents = [],
    confidence_score,
    reasoning,
    coordination_strategy,
    timeout_strategy,
    quality_assurance_method,
    fallback_plan
}).

-record(agent_recommendation, {
    agent_id,
    relevance_score,
    confidence,
    reasoning,
    estimated_quality,
    load_factor,
    availability
}).

%% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Route a query to the most appropriate agent(s)
route_query(Query, Options) ->
    gen_server:call(?MODULE, {route_query, Query, Options}, 30000).

%% Route complex queries requiring multi-agent coordination
route_complex_query(Query, Complexity, CoordinationStrategy) ->
    gen_server:call(?MODULE, {route_complex, Query, Complexity, CoordinationStrategy}, 60000).

%% Get routing analytics and performance metrics
get_routing_analytics() ->
    gen_server:call(?MODULE, get_analytics).

%% Optimize routing strategies based on historical performance
optimize_routing_strategy(Domain) ->
    gen_server:cast(?MODULE, {optimize_strategy, Domain}).

%% Handle feedback to improve future routing decisions
handle_query_feedback(QueryId, Feedback, Outcome) ->
    gen_server:cast(?MODULE, {feedback, QueryId, Feedback, Outcome}).

%% Gen_server callbacks

init([]) ->
    % Create ETS tables
    ets:new(?ROUTING_HISTORY, [named_table, public, ordered_set]),
    ets:new(?ANALYTICS_TABLE, [named_table, public, set]),
    
    % Initialize routing strategies
    RoutingStrategies = initialize_routing_strategies(),
    
    % Start load balancer
    LoadBalancer = spawn_link(fun() -> load_balancer_loop(#{}) end),
    
    % Initialize learning engine
    LearningEngine = spawn_link(fun() -> learning_engine_loop(#{}) end),
    
    {ok, #state{
        routing_strategies = RoutingStrategies,
        load_balancer = LoadBalancer,
        learning_engine = LearningEngine,
        routing_metrics = initialize_metrics()
    }}.

handle_call({route_query, Query, Options}, From, State) ->
    QueryId = generate_query_id(),
    
    % Spawn async routing process
    spawn_link(fun() ->
        Result = process_query_routing(QueryId, Query, Options, State),
        gen_server:reply(From, Result)
    end),
    
    {noreply, State};

handle_call({route_complex, Query, Complexity, Strategy}, From, State) ->
    QueryId = generate_query_id(),
    
    % Spawn async complex routing process
    spawn_link(fun() ->
        Result = process_complex_routing(QueryId, Query, Complexity, Strategy, State),
        gen_server:reply(From, Result)
    end),
    
    {noreply, State};

handle_call(get_analytics, _From, State) ->
    Analytics = compile_routing_analytics(),
    {reply, {ok, Analytics}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({optimize_strategy, Domain}, State) ->
    spawn(fun() -> optimize_domain_strategy(Domain) end),
    {noreply, State};

handle_cast({feedback, QueryId, Feedback, Outcome}, State) ->
    process_routing_feedback(QueryId, Feedback, Outcome),
    NewState = update_learning_models(QueryId, Feedback, Outcome, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({route_completed, QueryId, Results}, State) ->
    update_routing_history(QueryId, Results),
    NewMetrics = update_routing_metrics(Results, State#state.routing_metrics),
    {noreply, State#state{routing_metrics = NewMetrics}};

handle_info({circuit_breaker, AgentId, Status}, State) ->
    NewCircuitBreakers = maps:put(AgentId, Status, State#state.circuit_breakers),
    {noreply, State#state{circuit_breakers = NewCircuitBreakers}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Core routing logic

process_query_routing(QueryId, Query, Options, State) ->
    try
        % Step 1: Query analysis and preprocessing
        QueryAnalysis = analyze_and_preprocess_query(Query, Options),
        
        % Step 2: Route decision making
        RoutingDecision = make_routing_decision(QueryAnalysis, Options, State),
        
        % Step 3: Agent selection and validation
        ValidatedAgents = validate_and_prepare_agents(RoutingDecision),
        
        % Step 4: Create query route record
        QueryRoute = create_query_route(QueryId, Query, QueryAnalysis, RoutingDecision),
        
        % Step 5: Execute routing
        Results = execute_routing(QueryRoute, ValidatedAgents, State),
        
        % Step 6: Post-process and quality assurance
        FinalResults = post_process_results(Results, QueryRoute),
        
        % Step 7: Update metrics and learning
        update_routing_analytics(QueryRoute, FinalResults),
        
        {ok, #{
            query_id => QueryId,
            routing_decision => RoutingDecision,
            results => FinalResults,
            performance_metrics => extract_performance_metrics(QueryRoute)
        }}
        
    catch
        E:R:S ->
            handle_routing_error(QueryId, E, R, S),
            {error, {routing_failed, E, R, S}}
    end.

process_complex_routing(QueryId, Query, Complexity, Strategy, State) ->
    try
        % Enhanced analysis for complex queries
        ComplexAnalysis = analyze_complex_query(Query, Complexity),
        
        % Multi-stage routing strategy
        RoutingPlan = create_multi_stage_routing_plan(ComplexAnalysis, Strategy),
        
        % Execute coordinated routing
        Results = execute_coordinated_routing(QueryId, RoutingPlan, State),
        
        % Synthesize multi-agent results
        SynthesizedResults = synthesize_multi_agent_results(Results, ComplexAnalysis),
        
        {ok, #{
            query_id => QueryId,
            routing_plan => RoutingPlan,
            agent_results => Results,
            synthesized_result => SynthesizedResults,
            coordination_metrics => extract_coordination_metrics(Results)
        }}
        
    catch
        E:R:S ->
            {error, {complex_routing_failed, E, R, S}}
    end.

analyze_and_preprocess_query(Query, Options) ->
    % Multi-dimensional query analysis
    BasicAnalysis = expert_agent_selector:select_expert_for_query(Query, #{}),
    
    % Enhanced preprocessing
    PreprocessedQuery = apply_query_preprocessing(Query, Options),
    
    % Context extraction
    Context = extract_query_context(Query, Options),
    
    % Intent classification
    Intent = classify_query_intent(PreprocessedQuery),
    
    % Urgency and priority assessment
    {Urgency, Priority} = assess_urgency_and_priority(Query, Options),
    
    #{
        original_query => Query,
        preprocessed_query => PreprocessedQuery,
        basic_analysis => BasicAnalysis,
        context => Context,
        intent => Intent,
        urgency => Urgency,
        priority => Priority,
        complexity_indicators => extract_complexity_indicators(Query),
        domain_specificity => assess_domain_specificity(Query)
    }.

make_routing_decision(QueryAnalysis, Options, State) ->
    % Select routing strategy based on query characteristics
    Strategy = select_routing_strategy(QueryAnalysis, State),
    
    % Apply strategy to select agents
    AgentSelection = apply_routing_strategy(Strategy, QueryAnalysis, Options, State),
    
    % Determine coordination method
    CoordinationMethod = determine_coordination_method(AgentSelection, QueryAnalysis),
    
    % Create fallback plan
    FallbackPlan = create_fallback_plan(AgentSelection, QueryAnalysis),
    
    #routing_decision{
        primary_agent = maps:get(primary, AgentSelection),
        supporting_agents = maps:get(supporting, AgentSelection, []),
        confidence_score = maps:get(confidence, AgentSelection),
        reasoning = maps:get(reasoning, AgentSelection),
        coordination_strategy = CoordinationMethod,
        timeout_strategy = determine_timeout_strategy(QueryAnalysis),
        quality_assurance_method = select_qa_method(QueryAnalysis),
        fallback_plan = FallbackPlan
    }.

select_routing_strategy(QueryAnalysis, State) ->
    % Strategy selection based on query characteristics
    Complexity = maps:get(complexity_indicators, QueryAnalysis),
    DomainCount = length(maps:get(detected_domains, QueryAnalysis, [])),
    Urgency = maps:get(urgency, QueryAnalysis),
    
    Strategies = State#state.routing_strategies,
    
    case {Complexity, DomainCount, Urgency} of
        {low, 1, _} -> maps:get(single_expert, Strategies);
        {medium, 1, high} -> maps:get(fast_expert, Strategies);
        {high, 1, _} -> maps:get(deep_expert, Strategies);
        {_, N, _} when N > 1 -> maps:get(multi_domain, Strategies);
        {high, _, _} -> maps:get(collaborative, Strategies);
        _ -> maps:get(adaptive, Strategies)
    end.

apply_routing_strategy(Strategy, QueryAnalysis, Options, State) ->
    case Strategy of
        single_expert ->
            apply_single_expert_strategy(QueryAnalysis, Options, State);
        fast_expert ->
            apply_fast_expert_strategy(QueryAnalysis, Options, State);
        deep_expert ->
            apply_deep_expert_strategy(QueryAnalysis, Options, State);
        multi_domain ->
            apply_multi_domain_strategy(QueryAnalysis, Options, State);
        collaborative ->
            apply_collaborative_strategy(QueryAnalysis, Options, State);
        adaptive ->
            apply_adaptive_strategy(QueryAnalysis, Options, State)
    end.

apply_single_expert_strategy(QueryAnalysis, Options, _State) ->
    % Select single best expert
    {ok, Recommendation} = expert_agent_selector:select_expert_for_query(
        maps:get(original_query, QueryAnalysis), Options),
    
    #{
        primary => Recommendation#agent_recommendation.agent_id,
        supporting => [],
        confidence => Recommendation#agent_recommendation.confidence,
        reasoning => "Single expert strategy: " ++ Recommendation#agent_recommendation.reasoning,
        strategy => single_expert
    }.

apply_multi_domain_strategy(QueryAnalysis, _Options, _State) ->
    % Select experts from different domains
    {ok, Recommendations} = expert_agent_selector:route_to_best_agents(
        maps:get(original_query, QueryAnalysis), 3, diverse),
    
    [Primary | Supporting] = Recommendations,
    
    #{
        primary => Primary#agent_recommendation.agent_id,
        supporting => [R#agent_recommendation.agent_id || R <- Supporting],
        confidence => calculate_multi_agent_confidence(Recommendations),
        reasoning => "Multi-domain strategy: diverse expertise required",
        strategy => multi_domain
    }.

apply_collaborative_strategy(QueryAnalysis, _Options, _State) ->
    % Select complementary agents for collaboration
    {ok, Recommendations} = expert_agent_selector:route_to_best_agents(
        maps:get(original_query, QueryAnalysis), 4, complementary),
    
    [Primary | Supporting] = Recommendations,
    
    #{
        primary => Primary#agent_recommendation.agent_id,
        supporting => [R#agent_recommendation.agent_id || R <- Supporting],
        confidence => calculate_collaborative_confidence(Recommendations),
        reasoning => "Collaborative strategy: complex problem requiring multiple perspectives",
        strategy => collaborative
    }.

execute_routing(QueryRoute, ValidatedAgents, State) ->
    RouteId = QueryRoute#query_route.query_id,
    
    % Check circuit breakers
    AvailableAgents = filter_by_circuit_breakers(ValidatedAgents, State),
    
    % Apply load balancing
    BalancedAgents = apply_load_balancing(AvailableAgents, State),
    
    % Execute based on coordination strategy
    CoordinationStrategy = QueryRoute#query_route.coordination_method,
    
    case CoordinationStrategy of
        sequential ->
            execute_sequential_routing(RouteId, BalancedAgents);
        parallel ->
            execute_parallel_routing(RouteId, BalancedAgents);
        hierarchical ->
            execute_hierarchical_routing(RouteId, BalancedAgents);
        consensus ->
            execute_consensus_routing(RouteId, BalancedAgents)
    end.

execute_parallel_routing(RouteId, Agents) ->
    % Execute agents in parallel and collect results
    ParentPid = self(),
    
    _AgentPids = lists:map(fun(AgentId) ->
        spawn_link(fun() ->
            Result = execute_agent_query(AgentId, RouteId),
            ParentPid ! {agent_result, AgentId, Result}
        end)
    end, Agents),
    
    % Collect results with timeout
    collect_parallel_results(RouteId, Agents, 30000).

execute_sequential_routing(RouteId, Agents) ->
    % Execute agents sequentially, passing context between them
    execute_sequential_chain(RouteId, Agents, [], #{}).

execute_hierarchical_routing(RouteId, [Primary | Supporting]) ->
    % Primary agent coordinates supporting agents
    PrimaryResult = execute_agent_query(Primary, RouteId),
    
    % Use primary result to guide supporting agents
    SupportingResults = lists:map(fun(AgentId) ->
        execute_agent_query(AgentId, RouteId, #{context => PrimaryResult})
    end, Supporting),
    
    % Synthesize results hierarchically
    synthesize_hierarchical_results(PrimaryResult, SupportingResults).

execute_consensus_routing(RouteId, Agents) ->
    % Execute agents and reach consensus
    Results = execute_parallel_routing(RouteId, Agents),
    % Apply consensus mechanism
    reach_consensus(Results).

post_process_results(Results, QueryRoute) ->
    % Apply quality assurance
    QAResults = apply_quality_assurance(Results, QueryRoute),
    
    % Aggregate and synthesize
    Synthesized = synthesize_results(QAResults, QueryRoute),
    
    % Add metadata
    add_result_metadata(Synthesized, QueryRoute).

%% Utility functions

generate_query_id() ->
    list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

initialize_routing_strategies() ->
    #{
        single_expert => single_expert,
        fast_expert => fast_expert,
        deep_expert => deep_expert,
        multi_domain => multi_domain,
        collaborative => collaborative,
        adaptive => adaptive
    }.

initialize_metrics() ->
    #{
        total_queries => 0,
        successful_routes => 0,
        failed_routes => 0,
        average_response_time => 0.0,
        agent_utilization => #{},
        strategy_effectiveness => #{}
    }.

calculate_multi_agent_confidence(Recommendations) ->
    Confidences = [R#agent_recommendation.confidence || R <- Recommendations],
    lists:sum(Confidences) / length(Confidences).

calculate_collaborative_confidence(Recommendations) ->
    % Higher confidence for collaborative approaches
    BaseConfidence = calculate_multi_agent_confidence(Recommendations),
    min(1.0, BaseConfidence * 1.1).

% Placeholder implementations for complex functions
analyze_complex_query(_Query, _Complexity) -> #{}.
create_multi_stage_routing_plan(_Analysis, _Strategy) -> #{}.
execute_coordinated_routing(_QueryId, _Plan, _State) -> #{}.
synthesize_multi_agent_results(_Results, _Analysis) -> #{}.
extract_coordination_metrics(_Results) -> #{}.
apply_query_preprocessing(Query, _Options) -> Query.
extract_query_context(_Query, _Options) -> #{}.
classify_query_intent(_Query) -> informational.
assess_urgency_and_priority(_Query, _Options) -> {normal, medium}.
extract_complexity_indicators(_Query) -> low.
assess_domain_specificity(_Query) -> general.
determine_coordination_method(_Selection, _Analysis) -> parallel.
create_fallback_plan(_Selection, _Analysis) -> [].
determine_timeout_strategy(_Analysis) -> standard.
select_qa_method(_Analysis) -> basic.
validate_and_prepare_agents(Decision) ->
    [Decision#routing_decision.primary_agent | Decision#routing_decision.supporting_agents].
create_query_route(QueryId, Query, Analysis, Decision) ->
    #query_route{
        query_id = QueryId,
        original_query = Query,
        selected_agents = [Decision#routing_decision.primary_agent | Decision#routing_decision.supporting_agents],
        routing_strategy = maps:get(strategy, Analysis, unknown),
        coordination_method = Decision#routing_decision.coordination_strategy,
        started_at = erlang:system_time(millisecond),
        status = running
    }.
extract_performance_metrics(_QueryRoute) -> #{}.
handle_routing_error(_QueryId, _E, _R, _S) -> ok.
filter_by_circuit_breakers(Agents, _State) -> Agents.
apply_load_balancing(Agents, _State) -> Agents.
collect_parallel_results(_RouteId, _Agents, _Timeout) -> [].
execute_sequential_chain(_RouteId, _Agents, _Results, _Context) -> [].
execute_agent_query(_AgentId, _RouteId) -> #{}.
execute_agent_query(_AgentId, _RouteId, _Options) -> #{}.
synthesize_hierarchical_results(_Primary, _Supporting) -> #{}.
apply_quality_assurance(Results, _QueryRoute) -> Results.
synthesize_results(Results, _QueryRoute) -> Results.
add_result_metadata(Results, _QueryRoute) -> Results.
apply_fast_expert_strategy(_Analysis, _Options, _State) -> #{}.
apply_deep_expert_strategy(_Analysis, _Options, _State) -> #{}.
apply_adaptive_strategy(_Analysis, _Options, _State) -> #{}.
compile_routing_analytics() -> #{}.
optimize_domain_strategy(_Domain) -> ok.
process_routing_feedback(_QueryId, _Feedback, _Outcome) -> ok.
update_learning_models(_QueryId, _Feedback, _Outcome, State) -> State.
update_routing_history(_QueryId, _Results) -> ok.
update_routing_metrics(_Results, Metrics) -> Metrics.
update_routing_analytics(_QueryRoute, _Results) -> ok.
load_balancer_loop(State) ->
    receive
        {get_load, AgentId, From} ->
            Load = maps:get(AgentId, State, 0.0),
            From ! {load, Load},
            load_balancer_loop(State);
        {update_load, AgentId, Load} ->
            NewState = maps:put(AgentId, Load, State),
            load_balancer_loop(NewState);
        stop ->
            ok
    end.
learning_engine_loop(State) ->
    receive
        {learn, _Data} ->
            % Process learning data
            learning_engine_loop(State);
        {get_model, Domain, From} ->
            Model = maps:get(Domain, State, default_model),
            From ! {model, Model},
            learning_engine_loop(State);
        stop ->
            ok
    end.
reach_consensus(Results) -> Results.