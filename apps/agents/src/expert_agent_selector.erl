%% expert_agent_selector.erl
%% Intelligent expert agent selection and routing system
-module(expert_agent_selector).
-behaviour(gen_server).

-export([
    start_link/0,
    select_expert_for_query/2,
    route_to_best_agents/3,
    evaluate_agent_performance/2,
    get_agent_expertise_profile/1,
    update_expertise_model/2,
    recommend_agent_composition/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(EXPERTISE_TABLE, agent_expertise_profiles).
-define(PERFORMANCE_TABLE, agent_performance_history).
-define(QUERY_ANALYSIS_TABLE, query_analysis_cache).

-record(state, {
    expertise_models = #{},
    performance_history = #{},
    routing_strategies = #{},
    agent_loads = #{},
    learning_models = #{}
}).

-record(expertise_profile, {
    agent_id,
    primary_domains = [],
    secondary_domains = [],
    skill_scores = #{},
    specializations = [],
    performance_metrics = #{},
    last_updated,
    confidence_intervals = #{},
    learning_rate = 0.1
}).

-record(query_analysis, {
    query_text,
    detected_domains = [],
    complexity_score,
    urgency_level,
    required_expertise = [],
    confidence_score,
    estimated_effort,
    similar_queries = []
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

%% Select the most appropriate expert agent for a query
select_expert_for_query(Query, Options) ->
    gen_server:call(?MODULE, {select_expert, Query, Options}).

%% Route query to multiple complementary agents
route_to_best_agents(Query, NumAgents, Strategy) ->
    gen_server:call(?MODULE, {route_multi, Query, NumAgents, Strategy}).

%% Evaluate and update agent performance based on outcomes
evaluate_agent_performance(AgentId, PerformanceData) ->
    gen_server:cast(?MODULE, {update_performance, AgentId, PerformanceData}).

%% Get detailed expertise profile for an agent
get_agent_expertise_profile(AgentId) ->
    gen_server:call(?MODULE, {get_profile, AgentId}).

%% Update expertise model based on new evidence
update_expertise_model(AgentId, ExpertiseUpdate) ->
    gen_server:cast(?MODULE, {update_expertise, AgentId, ExpertiseUpdate}).

%% Recommend optimal agent composition for complex tasks
recommend_agent_composition(TaskDescription, Requirements) ->
    gen_server:call(?MODULE, {recommend_composition, TaskDescription, Requirements}).

%% Gen_server callbacks

init([]) ->
    % Create ETS tables for caching
    ets:new(?EXPERTISE_TABLE, [named_table, public, {keypos, #expertise_profile.agent_id}]),
    ets:new(?PERFORMANCE_TABLE, [named_table, public, set]),
    ets:new(?QUERY_ANALYSIS_TABLE, [named_table, public, set]),
    
    % Initialize expertise profiles for all templates
    initialize_agent_profiles(),
    
    % Load learning models
    LearningModels = initialize_learning_models(),
    
    {ok, #state{
        learning_models = LearningModels,
        routing_strategies = get_routing_strategies()
    }}.

handle_call({select_expert, Query, Options}, _From, State) ->
    QueryAnalysis = analyze_query(Query),
    Recommendation = select_best_agent(QueryAnalysis, Options, State),
    {reply, {ok, Recommendation}, State};

handle_call({route_multi, Query, NumAgents, Strategy}, _From, State) ->
    QueryAnalysis = analyze_query(Query),
    Recommendations = select_multiple_agents(QueryAnalysis, NumAgents, Strategy, State),
    {reply, {ok, Recommendations}, State};

handle_call({get_profile, AgentId}, _From, State) ->
    Profile = get_expertise_profile(AgentId),
    {reply, {ok, Profile}, State};

handle_call({recommend_composition, TaskDescription, Requirements}, _From, State) ->
    Composition = recommend_optimal_composition(TaskDescription, Requirements, State),
    {reply, {ok, Composition}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({update_performance, AgentId, PerformanceData}, State) ->
    update_agent_performance(AgentId, PerformanceData),
    NewState = update_expertise_from_performance(AgentId, PerformanceData, State),
    {noreply, NewState};

handle_cast({update_expertise, AgentId, ExpertiseUpdate}, State) ->
    update_agent_expertise(AgentId, ExpertiseUpdate),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({periodic_model_update}, State) ->
    NewState = perform_periodic_learning_update(State),
    schedule_periodic_update(),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

analyze_query(Query) ->
    % Multi-dimensional query analysis
    QueryText = normalize_query_text(Query),
    
    % Check cache first
    case ets:lookup(?QUERY_ANALYSIS_TABLE, QueryText) of
        [{_, CachedAnalysis}] ->
            CachedAnalysis;
        [] ->
            % Perform fresh analysis
            Analysis = #query_analysis{
                query_text = QueryText,
                detected_domains = detect_domains(QueryText),
                complexity_score = assess_complexity(QueryText),
                urgency_level = detect_urgency(QueryText),
                required_expertise = extract_required_expertise(QueryText),
                confidence_score = calculate_analysis_confidence(QueryText),
                estimated_effort = estimate_effort_requirement(QueryText),
                similar_queries = find_similar_queries(QueryText)
            },
            
            % Cache the analysis
            ets:insert(?QUERY_ANALYSIS_TABLE, {QueryText, Analysis}),
            Analysis
    end.

detect_domains(QueryText) ->
    % Use multiple approaches to detect relevant domains
    KeywordDomains = detect_domains_by_keywords(QueryText),
    SemanticDomains = detect_domains_by_semantics(QueryText),
    ContextDomains = detect_domains_by_context(QueryText),
    
    % Combine and rank domains
    AllDomains = lists:append([KeywordDomains, SemanticDomains, ContextDomains]),
    rank_domains_by_relevance(AllDomains, QueryText).

detect_domains_by_keywords(QueryText) ->
    % Define keyword patterns for different domains
    DomainKeywords = #{
        psychology => ["psychology", "behavior", "mental", "cognitive", "therapy", "counseling", "emotion"],
        medicine => ["medical", "health", "disease", "treatment", "diagnosis", "clinical", "patient"],
        education => ["education", "learning", "teaching", "curriculum", "student", "pedagogy", "academic"],
        technology => ["technology", "software", "programming", "algorithm", "system", "computer", "digital"],
        science => ["research", "experiment", "data", "analysis", "scientific", "study", "hypothesis"],
        philosophy => ["philosophy", "ethics", "logic", "reasoning", "moral", "philosophical", "concept"],
        history => ["history", "historical", "past", "ancient", "medieval", "timeline", "era"],
        sociology => ["society", "social", "culture", "community", "group", "demographic", "sociological"],
        economics => ["economics", "financial", "market", "economy", "business", "economic", "trade"],
        linguistics => ["language", "linguistic", "grammar", "syntax", "phonetics", "semantics", "communication"]
    },
    
    % Score each domain based on keyword matches
    LowerQuery = string:lowercase(QueryText),
    DomainScores = maps:map(fun(_Domain, Keywords) ->
        lists:sum([score_keyword_presence(LowerQuery, Keyword) || Keyword <- Keywords])
    end, DomainKeywords),
    
    % Return domains with scores above threshold
    Threshold = 1.0,
    [{Domain, Score} || {Domain, Score} <- maps:to_list(DomainScores), Score >= Threshold].

detect_domains_by_semantics(QueryText) ->
    % Simplified semantic analysis
    % In production, this would use embeddings and semantic similarity
    SemanticPatterns = [
        {psychology, ["how.*feel", "why.*behave", "mental.*state", "emotional.*response"]},
        {medicine, ["symptoms.*of", "treatment.*for", "diagnosis.*of", "medical.*condition"]},
        {education, ["how.*learn", "teaching.*method", "educational.*approach", "curriculum.*design"]},
        {technology, ["implement.*system", "algorithm.*for", "software.*development", "technical.*solution"]},
        {philosophy, ["what.*is.*meaning", "ethical.*consideration", "philosophical.*question", "moral.*dilemma"]}
    ],
    
    % Check for semantic pattern matches
    LowerQuery = string:lowercase(QueryText),
    lists:foldl(fun({Domain, Patterns}, Acc) ->
        case lists:any(fun(Pattern) -> 
            re:run(LowerQuery, Pattern, [caseless]) =/= nomatch 
        end, Patterns) of
            true -> [{Domain, 0.8} | Acc];
            false -> Acc
        end
    end, [], SemanticPatterns).

detect_domains_by_context(QueryText) ->
    % Context-based domain detection using co-occurrence patterns
    % Simplified implementation
    ContextClues = extract_context_clues(QueryText),
    
    % Map context clues to domains
    lists:foldl(fun(Clue, Acc) ->
        case map_context_to_domain(Clue) of
            {Domain, Score} -> [{Domain, Score} | Acc];
            undefined -> Acc
        end
    end, [], ContextClues).

assess_complexity(QueryText) ->
    % Multi-factor complexity assessment
    Factors = [
        assess_linguistic_complexity(QueryText),
        assess_conceptual_complexity(QueryText), 
        assess_interdisciplinary_complexity(QueryText),
        assess_technical_complexity(QueryText)
    ],
    
    % Weighted combination
    Weights = [0.2, 0.3, 0.25, 0.25],
    weighted_average(Factors, Weights).

assess_linguistic_complexity(QueryText) ->
    % Analyze sentence structure, vocabulary complexity
    Words = string:tokens(QueryText, " "),
    
    % Basic metrics
    WordCount = length(Words),
    AvgWordLength = lists:sum([length(W) || W <- Words]) / WordCount,
    SentenceCount = length(string:tokens(QueryText, ".!?")),
    
    % Complexity scoring
    LengthScore = min(1.0, WordCount / 50.0),
    VocabScore = min(1.0, AvgWordLength / 8.0),
    StructureScore = min(1.0, (WordCount / SentenceCount) / 20.0),
    
    (LengthScore + VocabScore + StructureScore) / 3.0.

assess_conceptual_complexity(QueryText) ->
    % Count abstract concepts, theoretical terms
    AbstractTerms = ["concept", "theory", "principle", "framework", "paradigm", 
                     "methodology", "approach", "perspective", "dimension"],
    
    LowerQuery = string:lowercase(QueryText),
    AbstractCount = lists:sum([count_term_occurrences(LowerQuery, Term) || Term <- AbstractTerms]),
    
    min(1.0, AbstractCount / 3.0).

assess_interdisciplinary_complexity(QueryText) ->
    % Detect if query spans multiple domains
    DetectedDomains = detect_domains(QueryText),
    DomainCount = length(DetectedDomains),
    
    case DomainCount of
        0 -> 0.2;
        1 -> 0.4;
        2 -> 0.6;
        3 -> 0.8;
        _ -> 1.0
    end.

assess_technical_complexity(QueryText) ->
    % Detect technical jargon, specialized terminology
    TechnicalIndicators = ["algorithm", "methodology", "implementation", "optimization",
                          "analysis", "evaluation", "framework", "systematic"],
    
    LowerQuery = string:lowercase(QueryText),
    TechnicalCount = lists:sum([count_term_occurrences(LowerQuery, Term) || Term <- TechnicalIndicators]),
    
    min(1.0, TechnicalCount / 2.0).

select_best_agent(QueryAnalysis, Options, State) ->
    % Get all available agents
    AllAgents = get_all_agent_profiles(),
    
    % Score each agent for this query
    ScoredAgents = lists:map(fun(Agent) ->
        Score = calculate_agent_relevance_score(Agent, QueryAnalysis, State),
        {Agent, Score}
    end, AllAgents),
    
    % Sort by score and apply filters
    FilteredAgents = apply_selection_filters(ScoredAgents, Options),
    SortedAgents = lists:reverse(lists:keysort(2, FilteredAgents)),
    
    % Return top recommendation
    case SortedAgents of
        [{BestAgent, Score} | _] ->
            create_recommendation(BestAgent, Score, QueryAnalysis);
        [] ->
            {error, no_suitable_agent}
    end.

calculate_agent_relevance_score(Agent, QueryAnalysis, State) ->
    % Multi-factor scoring
    DomainRelevance = calculate_domain_relevance(Agent, QueryAnalysis),
    ExpertiseMatch = calculate_expertise_match(Agent, QueryAnalysis),
    PerformanceHistory = get_performance_score(Agent#expertise_profile.agent_id),
    LoadFactor = get_load_factor(Agent#expertise_profile.agent_id, State),
    AvailabilityScore = get_availability_score(Agent#expertise_profile.agent_id),
    
    % Weighted combination
    Weights = #{
        domain_relevance => 0.3,
        expertise_match => 0.25,
        performance => 0.2,
        load_factor => 0.15,
        availability => 0.1
    },
    
    Score = DomainRelevance * maps:get(domain_relevance, Weights) +
            ExpertiseMatch * maps:get(expertise_match, Weights) +
            PerformanceHistory * maps:get(performance, Weights) +
            (1.0 - LoadFactor) * maps:get(load_factor, Weights) +
            AvailabilityScore * maps:get(availability, Weights),
    
    Score.

calculate_domain_relevance(Agent, QueryAnalysis) ->
    QueryDomains = [Domain || {Domain, _Score} <- QueryAnalysis#query_analysis.detected_domains],
    AgentDomains = Agent#expertise_profile.primary_domains ++ Agent#expertise_profile.secondary_domains,
    
    % Calculate overlap score
    Overlap = length(lists:intersection(QueryDomains, AgentDomains)),
    MaxRelevant = max(1, length(QueryDomains)),
    
    % Boost for primary domain matches
    PrimaryOverlap = length(lists:intersection(QueryDomains, Agent#expertise_profile.primary_domains)),
    PrimaryBonus = PrimaryOverlap * 0.2,
    
    BaseScore = Overlap / MaxRelevant,
    min(1.0, BaseScore + PrimaryBonus).

calculate_expertise_match(Agent, QueryAnalysis) ->
    RequiredExpertise = QueryAnalysis#query_analysis.required_expertise,
    AgentSkills = maps:keys(Agent#expertise_profile.skill_scores),
    
    case RequiredExpertise of
        [] -> 0.5;  % No specific expertise required
        _ ->
            MatchedSkills = lists:intersection(RequiredExpertise, AgentSkills),
            SkillScores = [maps:get(Skill, Agent#expertise_profile.skill_scores, 0.0) || Skill <- MatchedSkills],
            
            case SkillScores of
                [] -> 0.1;
                _ -> lists:sum(SkillScores) / length(SkillScores)
            end
    end.

select_multiple_agents(QueryAnalysis, NumAgents, Strategy, State) ->
    AllAgents = get_all_agent_profiles(),
    
    case Strategy of
        diverse ->
            select_diverse_agents(AllAgents, QueryAnalysis, NumAgents, State);
        complementary ->
            select_complementary_agents(AllAgents, QueryAnalysis, NumAgents, State);
        redundant ->
            select_redundant_agents(AllAgents, QueryAnalysis, NumAgents, State);
        specialized ->
            select_specialized_agents(AllAgents, QueryAnalysis, NumAgents, State)
    end.

select_diverse_agents(AllAgents, QueryAnalysis, NumAgents, State) ->
    % Select agents from different domains/specializations
    ScoredAgents = score_all_agents(AllAgents, QueryAnalysis, State),
    
    % Group by primary domain
    DomainGroups = group_agents_by_domain(ScoredAgents),
    
    % Select best agent from each domain, up to NumAgents
    SelectedAgents = select_from_domain_groups(DomainGroups, NumAgents),
    
    lists:map(fun({Agent, Score}) ->
        create_recommendation(Agent, Score, QueryAnalysis)
    end, SelectedAgents).

recommend_optimal_composition(TaskDescription, Requirements, State) ->
    % Analyze task complexity and requirements
    TaskAnalysis = analyze_task_complexity(TaskDescription, Requirements),
    
    % Determine optimal team size and composition
    OptimalSize = calculate_optimal_team_size(TaskAnalysis),
    RequiredRoles = identify_required_roles(TaskAnalysis),
    
    % Select agents for each role
    TeamComposition = select_agents_for_roles(RequiredRoles, OptimalSize, State),
    
    #{
        team_size => OptimalSize,
        roles => RequiredRoles,
        recommended_agents => TeamComposition,
        coordination_strategy => suggest_coordination_strategy(TaskAnalysis),
        estimated_effort => estimate_total_effort(TaskAnalysis, TeamComposition)
    }.

initialize_agent_profiles() ->
    {ok, Templates} = agent_templates:list_templates(),
    
    lists:foreach(fun(Template) ->
        AgentId = maps:get(id, Template),
        Profile = create_expertise_profile_from_template(Template),
        ets:insert(?EXPERTISE_TABLE, Profile)
    end, Templates).

create_expertise_profile_from_template(Template) ->
    AgentId = maps:get(id, Template),
    Name = maps:get(name, Template),
    Description = maps:get(description, Template),
    
    % Infer domains and skills from template
    PrimaryDomains = infer_primary_domains(Name, Description),
    SecondaryDomains = infer_secondary_domains(Description),
    Skills = infer_skills_from_template(Template),
    
    #expertise_profile{
        agent_id = AgentId,
        primary_domains = PrimaryDomains,
        secondary_domains = SecondaryDomains,
        skill_scores = Skills,
        specializations = infer_specializations(Description),
        performance_metrics = initialize_performance_metrics(),
        last_updated = erlang:system_time(millisecond),
        confidence_intervals = initialize_confidence_intervals(Skills)
    }.

infer_primary_domains(Name, Description) ->
    % Extract domains from agent name and description
    NameStr = binary_to_list(Name),
    DescStr = binary_to_list(Description),
    
    DomainMappings = [
        {psychology, ["psychologist", "mental health", "psychological", "therapy", "counseling"]},
        {medicine, ["medical", "clinical", "health", "physician", "doctor"]},
        {education, ["educational", "teaching", "learning", "curriculum", "pedagogy"]},
        {technology, ["developer", "engineer", "programming", "software", "technical"]},
        {science, ["scientist", "research", "analysis", "data", "scientific"]},
        {philosophy, ["philosopher", "ethics", "philosophical", "reasoning", "logic"]},
        {history, ["historian", "historical", "history", "past", "chronological"]},
        {sociology, ["sociologist", "social", "society", "community", "cultural"]},
        {anthropology, ["anthropologist", "cultural", "ethnographic", "human behavior"]},
        {linguistics, ["linguist", "language", "communication", "linguistic", "speech"]}
    ],
    
    Text = string:lowercase(NameStr ++ " " ++ DescStr),
    
    lists:foldl(fun({Domain, Keywords}, Acc) ->
        case lists:any(fun(Keyword) ->
            string:str(Text, Keyword) > 0
        end, Keywords) of
            true -> [Domain | Acc];
            false -> Acc
        end
    end, [], DomainMappings).

%% Utility functions

normalize_query_text(Query) when is_binary(Query) ->
    binary_to_list(Query);
normalize_query_text(Query) when is_list(Query) ->
    Query.

weighted_average(Values, Weights) ->
    WeightedSum = lists:sum([V * W || {V, W} <- lists:zip(Values, Weights)]),
    TotalWeight = lists:sum(Weights),
    WeightedSum / TotalWeight.

score_keyword_presence(Text, Keyword) ->
    case string:str(Text, Keyword) of
        0 -> 0.0;
        _ -> 1.0
    end.

count_term_occurrences(Text, Term) ->
    length(string:tokens(Text, Term)) - 1.

get_all_agent_profiles() ->
    ets:tab2list(?EXPERTISE_TABLE).

apply_selection_filters(ScoredAgents, _Options) ->
    % Apply various filters based on options
    % For now, just return all agents
    ScoredAgents.

create_recommendation(Agent, Score, QueryAnalysis) ->
    #agent_recommendation{
        agent_id = Agent#expertise_profile.agent_id,
        relevance_score = Score,
        confidence = calculate_confidence(Score, QueryAnalysis),
        reasoning = generate_reasoning(Agent, Score, QueryAnalysis),
        estimated_quality = estimate_response_quality(Agent, QueryAnalysis),
        load_factor = 0.5,  % Placeholder
        availability = 1.0   % Placeholder
    }.

% Placeholder implementations
extract_context_clues(_QueryText) -> [].
map_context_to_domain(_Clue) -> undefined.
detect_urgency(_QueryText) -> normal.
extract_required_expertise(_QueryText) -> [].
calculate_analysis_confidence(_QueryText) -> 0.8.
estimate_effort_requirement(_QueryText) -> medium.
find_similar_queries(_QueryText) -> [].
rank_domains_by_relevance(Domains, _QueryText) -> Domains.
get_performance_score(_AgentId) -> 0.8.
get_load_factor(_AgentId, _State) -> 0.5.
get_availability_score(_AgentId) -> 1.0.
score_all_agents(Agents, QueryAnalysis, State) ->
    [{Agent, calculate_agent_relevance_score(Agent, QueryAnalysis, State)} || Agent <- Agents].
group_agents_by_domain(_ScoredAgents) -> #{}.
select_from_domain_groups(_DomainGroups, _NumAgents) -> [].
select_complementary_agents(_Agents, _Analysis, _Num, _State) -> [].
select_redundant_agents(_Agents, _Analysis, _Num, _State) -> [].
select_specialized_agents(_Agents, _Analysis, _Num, _State) -> [].
analyze_task_complexity(_Task, _Requirements) -> #{}.
calculate_optimal_team_size(_Analysis) -> 3.
identify_required_roles(_Analysis) -> [analyst, coordinator, specialist].
select_agents_for_roles(_Roles, _Size, _State) -> [].
suggest_coordination_strategy(_Analysis) -> hierarchical.
estimate_total_effort(_Analysis, _Team) -> high.
infer_secondary_domains(_Description) -> [].
infer_skills_from_template(_Template) -> #{}.
infer_specializations(_Description) -> [].
initialize_performance_metrics() -> #{accuracy => 0.8, speed => 0.7, satisfaction => 0.9}.
initialize_confidence_intervals(_Skills) -> #{}.
calculate_confidence(_Score, _Analysis) -> 0.8.
generate_reasoning(_Agent, _Score, _Analysis) -> "High domain relevance and expertise match".
estimate_response_quality(_Agent, _Analysis) -> 0.85.
get_expertise_profile(AgentId) ->
    case ets:lookup(?EXPERTISE_TABLE, AgentId) of
        [Profile] -> Profile;
        [] -> undefined
    end.
update_agent_performance(_AgentId, _PerformanceData) -> ok.
update_expertise_from_performance(_AgentId, _PerformanceData, State) -> State.
update_agent_expertise(_AgentId, _ExpertiseUpdate) -> ok.
perform_periodic_learning_update(State) -> State.
schedule_periodic_update() ->
    erlang:send_after(3600000, self(), {periodic_model_update}).
initialize_learning_models() -> #{}.
get_routing_strategies() -> #{diverse => fun select_diverse_agents/4}.