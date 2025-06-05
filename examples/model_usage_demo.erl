%%%-------------------------------------------------------------------
%%% @doc Model Usage Demo
%%% Demonstrates how to use the new model registry and comparison features
%%% @end
%%%-------------------------------------------------------------------
-module(model_usage_demo).

-export([run/0]).

run() ->
    io:format("=== Model Registry Demo ===~n~n"),
    
    % 1. List all available models
    demo_list_models(),
    
    % 2. Compare specific models
    demo_compare_models(),
    
    % 3. Get model recommendations
    demo_model_recommendations(),
    
    % 4. Compare costs
    demo_cost_comparison(),
    
    % 5. Create agents with different models
    demo_create_agents(),
    
    io:format("~n=== Demo Complete ===~n").

demo_list_models() ->
    io:format("1. Available Models by Category:~n~n"),
    
    Categories = model_registry:get_model_categories(),
    lists:foreach(fun(Category) ->
        Models = model_registry:get_models_by_category(Category),
        io:format("  ~p (~p models):~n", [Category, length(Models)]),
        lists:foreach(fun(Model) ->
            io:format("    - ~s: ~s~n", 
                     [maps:get(id, Model), maps:get(description, Model)])
        end, lists:sublist(Models, 3)),
        if length(Models) > 3 -> io:format("    ... and ~p more~n", [length(Models) - 3]); true -> ok end,
        io:format("~n")
    end, lists:sublist(Categories, 5)).

demo_compare_models() ->
    io:format("~n2. Model Comparison:~n~n"),
    
    Comparison = model_comparison:compare_models(<<"gpt-4.1">>, <<"gpt-4.1-mini">>),
    io:format("  Comparing GPT-4.1 vs GPT-4.1-mini:~n"),
    io:format("  - Cost: ~p~n", [maps:get(cost_comparison, Comparison)]),
    io:format("  - Speed: ~p~n", [maps:get(speed_comparison, Comparison)]),
    io:format("  - Intelligence: ~p~n", [maps:get(intelligence_comparison, Comparison)]),
    io:format("~n").

demo_model_recommendations() ->
    io:format("~n3. Model Recommendations:~n~n"),
    
    % Recommend for different use cases
    UseCases = [
        #{task_type => general_chat, priority => balanced},
        #{task_type => reasoning, priority => intelligence},
        #{task_type => coding, priority => speed},
        #{task_type => real_time_interaction, priority => speed}
    ],
    
    lists:foreach(fun(UseCase) ->
        case model_comparison:recommend_model(UseCase) of
            {ok, ModelId} ->
                {ok, ModelInfo} = model_registry:get_model_info(ModelId),
                io:format("  For ~p with ~p priority:~n", 
                         [maps:get(task_type, UseCase), maps:get(priority, UseCase)]),
                io:format("    Recommended: ~s (~s)~n", 
                         [ModelId, maps:get(description, ModelInfo)]);
            {error, Reason} ->
                io:format("  For ~p: Error - ~p~n", 
                         [maps:get(task_type, UseCase), Reason])
        end
    end, UseCases),
    io:format("~n").

demo_cost_comparison() ->
    io:format("~n4. Cost-Performance Rankings:~n~n"),
    
    Rankings = model_comparison:get_cost_performance_ranking(),
    io:format("  Top 5 models by cost-performance ratio:~n"),
    lists:foreach(fun(Model) ->
        io:format("    - ~s: Score ~.2f (~p tier, ~p speed)~n", 
                 [maps:get(id, Model), 
                  maps:get(cost_performance_score, Model),
                  maps:get(cost_tier, Model),
                  maps:get(speed_tier, Model)])
    end, lists:sublist(Rankings, 5)),
    io:format("~n").

demo_create_agents() ->
    io:format("~n5. Creating Agents with Different Models:~n~n"),
    
    % Start the agent system if not already running
    case whereis(agent_supervisor) of
        undefined -> agent:start();
        _ -> ok
    end,
    
    % Create agents with different models
    Agents = [
        {<<"reasoning_agent">>, <<"o3">>, <<"Expert reasoning agent for complex problems">>},
        {<<"fast_helper">>, <<"gpt-4.1-nano">>, <<"Quick responses for simple queries">>},
        {<<"balanced_agent">>, <<"gpt-4.1-mini">>, <<"Balanced performance and cost">>},
        {<<"creative_agent">>, <<"gpt-4.1">>, <<"High-quality creative tasks">>}
    ],
    
    lists:foreach(fun({Name, Model, Purpose}) ->
        Config = #{
            name => Name,
            model => Model,
            system_prompt => Purpose
        },
        case agent:create(Config) of
            {ok, AgentId} ->
                io:format("  Created ~s with model ~s~n", [Name, Model]),
                % Test the agent
                {ok, Response} = agent:execute(AgentId, #{
                    action => <<"chat">>,
                    message => <<"Hello! What model are you using?">>
                }),
                io:format("    Response: ~s~n~n", [Response]);
            {error, Reason} ->
                io:format("  Failed to create ~s: ~p~n", [Name, Reason])
        end
    end, Agents),
    
    io:format("~n").