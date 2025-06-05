%%%-------------------------------------------------------------------
%%% @doc Model Comparison Module
%%% Provides utilities for comparing and selecting the best model for specific tasks
%%% @end
%%%-------------------------------------------------------------------
-module(model_comparison).

-export([
    compare_models/2,
    recommend_model/1,
    get_best_model_for_task/1,
    get_cost_performance_ranking/0,
    get_speed_ranking/0,
    get_intelligence_ranking/0,
    compare_model_costs/2,
    estimate_task_cost/2
]).

-type task_type() :: general_chat | coding | reasoning | creative_writing | 
                    translation | summarization | analysis | real_time_interaction |
                    image_generation | audio_processing | embeddings | moderation.

-type task_requirements() :: #{
    task_type := task_type(),
    priority := cost | speed | intelligence | balanced,
    features => [atom()],
    max_cost_tier => atom(),
    min_intelligence => atom()
}.

%% @doc Compare two models
-spec compare_models(binary(), binary()) -> map().
compare_models(ModelId1, ModelId2) ->
    case {model_registry:get_model_info(ModelId1), 
          model_registry:get_model_info(ModelId2)} of
        {{ok, Model1}, {ok, Model2}} ->
            #{
                model1 => Model1,
                model2 => Model2,
                cost_comparison => compare_cost_tiers(
                    maps:get(cost_tier, Model1),
                    maps:get(cost_tier, Model2)
                ),
                speed_comparison => compare_speed_tiers(
                    maps:get(speed_tier, Model1),
                    maps:get(speed_tier, Model2)
                ),
                intelligence_comparison => compare_intelligence_tiers(
                    maps:get(intelligence_tier, Model1),
                    maps:get(intelligence_tier, Model2)
                ),
                shared_capabilities => lists_intersection(
                    maps:get(capabilities, Model1),
                    maps:get(capabilities, Model2)
                ),
                unique_to_model1 => lists:subtract(
                    maps:get(capabilities, Model1),
                    maps:get(capabilities, Model2)
                ),
                unique_to_model2 => lists:subtract(
                    maps:get(capabilities, Model2),
                    maps:get(capabilities, Model1)
                )
            };
        _ ->
            #{error => model_not_found}
    end.

%% @doc Recommend a model based on requirements
-spec recommend_model(task_requirements()) -> {ok, binary()} | {error, no_suitable_model}.
recommend_model(Requirements) ->
    TaskType = maps:get(task_type, Requirements),
    Priority = maps:get(priority, Requirements),
    RequiredFeatures = maps:get(features, Requirements, []),
    MaxCostTier = maps:get(max_cost_tier, Requirements, very_high),
    MinIntelligence = maps:get(min_intelligence, Requirements, basic),
    
    % Get all models
    AllModels = model_registry:get_all_models(),
    
    % Filter by required capabilities
    SuitableModels = filter_by_task_capabilities(AllModels, TaskType, RequiredFeatures),
    
    % Filter by cost and intelligence constraints
    ConstrainedModels = lists:filter(
        fun(Model) ->
            cost_tier_lte(maps:get(cost_tier, Model), MaxCostTier) andalso
            intelligence_tier_gte(maps:get(intelligence_tier, Model), MinIntelligence)
        end,
        SuitableModels
    ),
    
    % Sort by priority
    SortedModels = sort_by_priority(ConstrainedModels, Priority),
    
    case SortedModels of
        [BestModel | _] ->
            {ok, maps:get(id, BestModel)};
        [] ->
            {error, no_suitable_model}
    end.

%% @doc Get the best model for a specific task type
-spec get_best_model_for_task(task_type()) -> {ok, binary()} | {error, no_suitable_model}.
get_best_model_for_task(TaskType) ->
    recommend_model(#{
        task_type => TaskType,
        priority => balanced
    }).

%% @doc Get models ranked by cost-performance ratio
-spec get_cost_performance_ranking() -> [map()].
get_cost_performance_ranking() ->
    Models = [M || M <- model_registry:get_all_models(),
                   not maps:get(deprecated, M)],
    
    ModelsWithScore = lists:map(
        fun(Model) ->
            Score = calculate_cost_performance_score(Model),
            Model#{cost_performance_score => Score}
        end,
        Models
    ),
    
    lists:reverse(lists:sort(
        fun(A, B) ->
            maps:get(cost_performance_score, A) =< maps:get(cost_performance_score, B)
        end,
        ModelsWithScore
    )).

%% @doc Get models ranked by speed
-spec get_speed_ranking() -> [map()].
get_speed_ranking() ->
    Models = [M || M <- model_registry:get_all_models(),
                   not maps:get(deprecated, M)],
    
    lists:sort(
        fun(A, B) ->
            speed_tier_gt(maps:get(speed_tier, A), maps:get(speed_tier, B))
        end,
        Models
    ).

%% @doc Get models ranked by intelligence
-spec get_intelligence_ranking() -> [map()].
get_intelligence_ranking() ->
    Models = [M || M <- model_registry:get_all_models(),
                   not maps:get(deprecated, M)],
    
    lists:sort(
        fun(A, B) ->
            intelligence_tier_gt(maps:get(intelligence_tier, A), maps:get(intelligence_tier, B))
        end,
        Models
    ).

%% @doc Compare costs between two models for a task
-spec compare_model_costs(binary(), binary()) -> map().
compare_model_costs(ModelId1, ModelId2) ->
    case {model_registry:get_model_info(ModelId1),
          model_registry:get_model_info(ModelId2)} of
        {{ok, Model1}, {ok, Model2}} ->
            Cost1 = cost_tier_to_numeric(maps:get(cost_tier, Model1)),
            Cost2 = cost_tier_to_numeric(maps:get(cost_tier, Model2)),
            #{
                model1 => #{
                    id => ModelId1,
                    cost_tier => maps:get(cost_tier, Model1),
                    relative_cost => Cost1
                },
                model2 => #{
                    id => ModelId2,
                    cost_tier => maps:get(cost_tier, Model2),
                    relative_cost => Cost2
                },
                cost_ratio => Cost1 / Cost2,
                cheaper_model => if Cost1 < Cost2 -> ModelId1; true -> ModelId2 end
            };
        _ ->
            #{error => model_not_found}
    end.

%% @doc Estimate relative cost for a task
-spec estimate_task_cost(binary(), integer()) -> map().
estimate_task_cost(ModelId, EstimatedTokens) ->
    case model_registry:get_model_info(ModelId) of
        {ok, Model} ->
            CostTier = maps:get(cost_tier, Model),
            BaseCost = cost_tier_to_numeric(CostTier),
            % Rough estimation - actual costs depend on specific pricing
            EstimatedRelativeCost = BaseCost * EstimatedTokens / 1000,
            #{
                model => ModelId,
                cost_tier => CostTier,
                estimated_tokens => EstimatedTokens,
                relative_cost => EstimatedRelativeCost,
                note => <<"This is a relative cost estimate. Check current pricing for accurate costs.">>
            };
        {error, _} ->
            #{error => model_not_found}
    end.

%% Internal functions

filter_by_task_capabilities(Models, TaskType, RequiredFeatures) ->
    RequiredCaps = task_type_to_capabilities(TaskType) ++ RequiredFeatures,
    lists:filter(
        fun(Model) ->
            ModelCaps = maps:get(capabilities, Model),
            lists:all(fun(Cap) -> lists:member(Cap, ModelCaps) end, RequiredCaps)
        end,
        Models
    ).

task_type_to_capabilities(general_chat) -> [chat];
task_type_to_capabilities(coding) -> [chat];
task_type_to_capabilities(reasoning) -> [reasoning];
task_type_to_capabilities(creative_writing) -> [chat];
task_type_to_capabilities(translation) -> [chat];
task_type_to_capabilities(summarization) -> [chat];
task_type_to_capabilities(analysis) -> [chat];
task_type_to_capabilities(real_time_interaction) -> [realtime];
task_type_to_capabilities(image_generation) -> [image_generation];
task_type_to_capabilities(audio_processing) -> [audio_input];
task_type_to_capabilities(embeddings) -> [embeddings];
task_type_to_capabilities(moderation) -> [moderation].

sort_by_priority(Models, cost) ->
    lists:sort(
        fun(A, B) ->
            cost_tier_lte(maps:get(cost_tier, A), maps:get(cost_tier, B))
        end,
        Models
    );
sort_by_priority(Models, speed) ->
    lists:sort(
        fun(A, B) ->
            speed_tier_gt(maps:get(speed_tier, A), maps:get(speed_tier, B))
        end,
        Models
    );
sort_by_priority(Models, intelligence) ->
    lists:sort(
        fun(A, B) ->
            intelligence_tier_gt(maps:get(intelligence_tier, A), maps:get(intelligence_tier, B))
        end,
        Models
    );
sort_by_priority(Models, balanced) ->
    lists:sort(
        fun(A, B) ->
            ScoreA = calculate_balanced_score(A),
            ScoreB = calculate_balanced_score(B),
            ScoreA >= ScoreB
        end,
        Models
    ).

calculate_balanced_score(Model) ->
    CostScore = 4 - cost_tier_to_numeric(maps:get(cost_tier, Model)),
    SpeedScore = speed_tier_to_numeric(maps:get(speed_tier, Model)),
    IntelligenceScore = intelligence_tier_to_numeric(maps:get(intelligence_tier, Model)),
    (CostScore * 0.3 + SpeedScore * 0.3 + IntelligenceScore * 0.4).

calculate_cost_performance_score(Model) ->
    Cost = cost_tier_to_numeric(maps:get(cost_tier, Model)),
    Intelligence = intelligence_tier_to_numeric(maps:get(intelligence_tier, Model)),
    Speed = speed_tier_to_numeric(maps:get(speed_tier, Model)),
    (Intelligence * Speed) / Cost.

%% Tier comparison functions
cost_tier_to_numeric(low) -> 1;
cost_tier_to_numeric(medium) -> 2;
cost_tier_to_numeric(high) -> 3;
cost_tier_to_numeric(very_high) -> 4.

speed_tier_to_numeric(slow) -> 1;
speed_tier_to_numeric(medium) -> 2;
speed_tier_to_numeric(fast) -> 3;
speed_tier_to_numeric(very_fast) -> 4.

intelligence_tier_to_numeric(basic) -> 1;
intelligence_tier_to_numeric(standard) -> 2;
intelligence_tier_to_numeric(advanced) -> 3;
intelligence_tier_to_numeric(expert) -> 4.

cost_tier_lte(A, B) ->
    cost_tier_to_numeric(A) =< cost_tier_to_numeric(B).

speed_tier_gt(A, B) ->
    speed_tier_to_numeric(A) > speed_tier_to_numeric(B).

intelligence_tier_gte(A, B) ->
    intelligence_tier_to_numeric(A) >= intelligence_tier_to_numeric(B).

intelligence_tier_gt(A, B) ->
    intelligence_tier_to_numeric(A) > intelligence_tier_to_numeric(B).

compare_cost_tiers(A, B) ->
    NumA = cost_tier_to_numeric(A),
    NumB = cost_tier_to_numeric(B),
    if
        NumA < NumB -> cheaper;
        NumA > NumB -> more_expensive;
        true -> equal
    end.

compare_speed_tiers(A, B) ->
    NumA = speed_tier_to_numeric(A),
    NumB = speed_tier_to_numeric(B),
    if
        NumA > NumB -> faster;
        NumA < NumB -> slower;
        true -> equal
    end.

compare_intelligence_tiers(A, B) ->
    NumA = intelligence_tier_to_numeric(A),
    NumB = intelligence_tier_to_numeric(B),
    if
        NumA > NumB -> more_intelligent;
        NumA < NumB -> less_intelligent;
        true -> equal
    end.

lists_intersection(List1, List2) ->
    [X || X <- List1, lists:member(X, List2)].