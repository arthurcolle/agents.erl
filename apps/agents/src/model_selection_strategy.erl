%% model_selection_strategy.erl
%% Dynamic model selection for agents based on task requirements and load balancing
-module(model_selection_strategy).
-behaviour(gen_server).

-export([
    start_link/0,
    select_model_for_agent/2,
    select_model_for_task/3,
    register_model_usage/2,
    get_model_statistics/0,
    get_model_recommendations/1,
    update_model_performance/3,
    balance_model_load/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(model_stats, {
    usage_count = 0 :: integer(),
    active_agents = 0 :: integer(),
    average_response_time = 0.0 :: float(),
    success_rate = 1.0 :: float(),
    total_requests = 0 :: integer(),
    failed_requests = 0 :: integer(),
    cost_per_request = 0.0 :: float(),
    performance_score = 1.0 :: float()
}).

-record(state, {
    model_stats = #{} :: #{binary() => #model_stats{}},
    agent_models = #{} :: #{pid() => binary()},
    task_patterns = #{} :: #{binary() => [binary()]}, % task_type -> preferred_models
    load_balancing_enabled = true :: boolean(),
    selection_strategy = balanced :: balanced | performance | cost | random,
    preferred_models = [] :: [binary()] % User-specified model preferences
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Select a model for a new agent based on agent type and configuration
select_model_for_agent(AgentType, Config) ->
    gen_server:call(?SERVER, {select_model_for_agent, AgentType, Config}).

%% Select a model for a specific task type
select_model_for_task(TaskType, Requirements, AgentId) ->
    gen_server:call(?SERVER, {select_model_for_task, TaskType, Requirements, AgentId}).

%% Register model usage by an agent
register_model_usage(AgentPid, Model) ->
    gen_server:cast(?SERVER, {register_model_usage, AgentPid, Model}).

%% Get usage statistics for all models
get_model_statistics() ->
    gen_server:call(?SERVER, get_model_statistics).

%% Get model recommendations for a specific use case
get_model_recommendations(UseCase) ->
    gen_server:call(?SERVER, {get_model_recommendations, UseCase}).

%% Update model performance metrics
update_model_performance(Model, ResponseTime, Success) ->
    gen_server:cast(?SERVER, {update_model_performance, Model, ResponseTime, Success}).

%% Balance model load across available models
balance_model_load() ->
    gen_server:call(?SERVER, balance_model_load).

%% gen_server callbacks
init([]) ->
    % Initialize with preferred models from the user's request
    PreferredModels = [
        <<"o4-mini">>,
        <<"gpt-4.1">>,
        <<"gpt-4.1-mini">>,
        <<"gpt-4.1-nano">>,
        <<"o3">>
    ],
    
    % Initialize stats for preferred models
    InitialStats = lists:foldl(fun(Model, Acc) ->
        maps:put(Model, #model_stats{}, Acc)
    end, #{}, PreferredModels),
    
    % Set up task patterns based on model capabilities
    TaskPatterns = #{
        <<"reasoning">> => [<<"o3">>, <<"o4-mini">>, <<"gpt-4.1">>],
        <<"chat">> => [<<"gpt-4.1">>, <<"gpt-4.1-mini">>, <<"o4-mini">>],
        <<"cost_optimized">> => [<<"gpt-4.1-nano">>, <<"gpt-4.1-mini">>, <<"o4-mini">>],
        <<"complex_tasks">> => [<<"o3">>, <<"gpt-4.1">>, <<"o4-mini">>],
        <<"fast_response">> => [<<"gpt-4.1-nano">>, <<"gpt-4.1-mini">>, <<"o4-mini">>],
        <<"general">> => [<<"gpt-4.1">>, <<"o4-mini">>, <<"gpt-4.1-mini">>]
    },
    
    State = #state{
        model_stats = InitialStats,
        agent_models = #{},
        task_patterns = TaskPatterns,
        load_balancing_enabled = true,
        selection_strategy = balanced,
        preferred_models = PreferredModels
    },
    
    % Start periodic load balancing
    erlang:send_after(60000, self(), balance_load), % Every minute
    
    {ok, State}.

handle_call({select_model_for_agent, AgentType, Config}, _From, State) ->
    % Check if model is specified in config
    case maps:get(model, Config, undefined) of
        undefined ->
            % Select based on agent type and current load
            SelectedModel = select_optimal_model(AgentType, general, State),
            {reply, {ok, SelectedModel}, State};
        SpecifiedModel ->
            % Validate the specified model
            case lists:member(SpecifiedModel, State#state.preferred_models) of
                true ->
                    {reply, {ok, SpecifiedModel}, State};
                false ->
                    % Fall back to optimal selection
                    SelectedModel = select_optimal_model(AgentType, general, State),
                    {reply, {ok, SelectedModel}, State}
            end
    end;

handle_call({select_model_for_task, TaskType, _Requirements, _AgentId}, _From, State) ->
    SelectedModel = select_optimal_model(TaskType, TaskType, State),
    {reply, {ok, SelectedModel}, State};

handle_call(get_model_statistics, _From, State) ->
    Stats = maps:map(fun(_Model, #model_stats{} = MS) ->
        #{
            usage_count => MS#model_stats.usage_count,
            active_agents => MS#model_stats.active_agents,
            average_response_time => MS#model_stats.average_response_time,
            success_rate => MS#model_stats.success_rate,
            total_requests => MS#model_stats.total_requests,
            failed_requests => MS#model_stats.failed_requests,
            cost_per_request => MS#model_stats.cost_per_request,
            performance_score => MS#model_stats.performance_score
        }
    end, State#state.model_stats),
    {reply, {ok, Stats}, State};

handle_call({get_model_recommendations, UseCase}, _From, State) ->
    Recommendations = get_recommendations_for_use_case(UseCase, State),
    {reply, {ok, Recommendations}, State};

handle_call(balance_model_load, _From, State) ->
    NewState = perform_load_balancing(State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({register_model_usage, AgentPid, Model}, State) ->
    % Update agent -> model mapping
    NewAgentModels = maps:put(AgentPid, Model, State#state.agent_models),
    
    % Update model stats
    ModelStats = maps:get(Model, State#state.model_stats, #model_stats{}),
    UpdatedStats = ModelStats#model_stats{
        usage_count = ModelStats#model_stats.usage_count + 1,
        active_agents = ModelStats#model_stats.active_agents + 1
    },
    
    NewModelStats = maps:put(Model, UpdatedStats, State#state.model_stats),
    
    {noreply, State#state{
        agent_models = NewAgentModels,
        model_stats = NewModelStats
    }};

handle_cast({update_model_performance, Model, ResponseTime, Success}, State) ->
    ModelStats = maps:get(Model, State#state.model_stats, #model_stats{}),
    
    NewTotalRequests = ModelStats#model_stats.total_requests + 1,
    NewFailedRequests = case Success of
        true -> ModelStats#model_stats.failed_requests;
        false -> ModelStats#model_stats.failed_requests + 1
    end,
    
    % Calculate rolling average response time
    CurrentAvg = ModelStats#model_stats.average_response_time,
    NewAvg = case ModelStats#model_stats.total_requests of
        0 -> ResponseTime;
        N -> (CurrentAvg * N + ResponseTime) / (N + 1)
    end,
    
    NewSuccessRate = (NewTotalRequests - NewFailedRequests) / NewTotalRequests,
    
    % Calculate performance score (weighted combination of speed and success rate)
    PerformanceScore = calculate_performance_score(NewAvg, NewSuccessRate),
    
    UpdatedStats = ModelStats#model_stats{
        average_response_time = NewAvg,
        success_rate = NewSuccessRate,
        total_requests = NewTotalRequests,
        failed_requests = NewFailedRequests,
        performance_score = PerformanceScore
    },
    
    NewModelStats = maps:put(Model, UpdatedStats, State#state.model_stats),
    
    {noreply, State#state{model_stats = NewModelStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(balance_load, State) ->
    NewState = perform_load_balancing(State),
    % Schedule next load balancing
    erlang:send_after(60000, self(), balance_load),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

%% Select optimal model based on strategy and current conditions
select_optimal_model(AgentType, TaskType, State) ->
    case State#state.selection_strategy of
        balanced ->
            select_balanced_model(TaskType, State);
        performance ->
            select_performance_model(TaskType, State);
        cost ->
            select_cost_optimized_model(TaskType, State);
        random ->
            select_random_model(State);
        _ ->
            select_balanced_model(TaskType, State)
    end.

%% Select model based on balanced load and performance
select_balanced_model(TaskType, State) ->
    % Get preferred models for task type
    PreferredModels = maps:get(TaskType, State#state.task_patterns, 
                               State#state.preferred_models),
    
    % Score models based on load, performance, and suitability
    ScoredModels = lists:map(fun(Model) ->
        ModelStats = maps:get(Model, State#state.model_stats, #model_stats{}),
        
        % Load factor (lower is better)
        LoadFactor = ModelStats#model_stats.active_agents / 10.0, % Normalize by expected max
        
        % Performance factor (higher is better)
        PerformanceFactor = ModelStats#model_stats.performance_score,
        
        % Task suitability (higher is better)
        SuitabilityFactor = case TaskType of
            <<"reasoning">> when Model =:= <<"o3">> -> 1.0;
            <<"reasoning">> when Model =:= <<"o4-mini">> -> 0.8;
            <<"cost_optimized">> when Model =:= <<"gpt-4.1-nano">> -> 1.0;
            <<"cost_optimized">> when Model =:= <<"gpt-4.1-mini">> -> 0.9;
            <<"fast_response">> when Model =:= <<"gpt-4.1-nano">> -> 1.0;
            <<"fast_response">> when Model =:= <<"gpt-4.1-mini">> -> 0.9;
            _ -> 0.7 % Default suitability
        end,
        
        % Combined score (higher is better)
        Score = (PerformanceFactor * 0.4 + (1.0 - LoadFactor) * 0.3 + SuitabilityFactor * 0.3),
        
        {Model, Score}
    end, PreferredModels),
    
    % Select model with highest score
    {BestModel, _Score} = lists:foldl(fun({Model, Score}, {CurrentModel, CurrentScore}) ->
        if Score > CurrentScore -> {Model, Score};
           true -> {CurrentModel, CurrentScore}
        end
    end, hd(ScoredModels), tl(ScoredModels)),
    BestModel.

%% Select model optimized for performance
select_performance_model(TaskType, State) ->
    PreferredModels = maps:get(TaskType, State#state.task_patterns, 
                               State#state.preferred_models),
    
    % Find model with best performance score
    BestModel = lists:foldl(fun(Model, CurrentBest) ->
        ModelStats = maps:get(Model, State#state.model_stats, #model_stats{}),
        CurrentStats = maps:get(CurrentBest, State#state.model_stats, #model_stats{}),
        
        case ModelStats#model_stats.performance_score > CurrentStats#model_stats.performance_score of
            true -> Model;
            false -> CurrentBest
        end
    end, hd(PreferredModels), tl(PreferredModels)),
    
    BestModel.

%% Select cost-optimized model
select_cost_optimized_model(_TaskType, State) ->
    % Prefer nano and mini variants for cost optimization
    CostOptimizedModels = [<<"gpt-4.1-nano">>, <<"gpt-4.1-mini">>, <<"o4-mini">>],
    AvailableModels = [M || M <- CostOptimizedModels, 
                           lists:member(M, State#state.preferred_models)],
    
    case AvailableModels of
        [] -> hd(State#state.preferred_models);
        [FirstModel | _] -> FirstModel
    end.

%% Select random model from preferred list
select_random_model(State) ->
    Models = State#state.preferred_models,
    RandomIndex = rand:uniform(length(Models)),
    lists:nth(RandomIndex, Models).

%% Calculate performance score based on response time and success rate
calculate_performance_score(AvgResponseTime, SuccessRate) ->
    % Normalize response time (assuming 1-10 second range)
    NormalizedTime = max(0.0, min(1.0, (10.0 - AvgResponseTime) / 9.0)),
    
    % Weight success rate more heavily than speed
    SuccessRate * 0.7 + NormalizedTime * 0.3.

%% Get model recommendations for specific use cases
get_recommendations_for_use_case(UseCase, State) ->
    case UseCase of
        <<"complex_reasoning">> ->
            [<<"o3">>, <<"gpt-4.1">>, <<"o4-mini">>];
        <<"fast_response">> ->
            [<<"gpt-4.1-nano">>, <<"gpt-4.1-mini">>];
        <<"cost_effective">> ->
            [<<"gpt-4.1-nano">>, <<"gpt-4.1-mini">>, <<"o4-mini">>];
        <<"balanced">> ->
            [<<"gpt-4.1">>, <<"o4-mini">>, <<"gpt-4.1-mini">>];
        _ ->
            State#state.preferred_models
    end.

%% Perform load balancing across models
perform_load_balancing(State) ->
    % Get current load distribution
    TotalAgents = maps:size(State#state.agent_models),
    
    case TotalAgents > 0 of
        true ->
            % Calculate ideal distribution
            NumModels = length(State#state.preferred_models),
            IdealAgentsPerModel = TotalAgents / NumModels,
            
            % Identify overloaded and underloaded models
            {Overloaded, Underloaded} = lists:foldl(fun(Model, {Over, Under}) ->
                ModelStats = maps:get(Model, State#state.model_stats, #model_stats{}),
                ActiveAgents = ModelStats#model_stats.active_agents,
                
                if
                    ActiveAgents > IdealAgentsPerModel * 1.2 ->
                        {[{Model, ActiveAgents} | Over], Under};
                    ActiveAgents < IdealAgentsPerModel * 0.8 ->
                        {Over, [{Model, ActiveAgents} | Under]};
                    true ->
                        {Over, Under}
                end
            end, {[], []}, State#state.preferred_models),
            
            % Log load balancing information
            case {Overloaded, Underloaded} of
                {[], []} ->
                    State; % Already balanced
                _ ->
                    io:format("Load balancing: Overloaded=~p, Underloaded=~p~n", [Overloaded, Underloaded]),
                    State % For now, just log. In production, could trigger agent migration
            end;
        false ->
            State
    end.