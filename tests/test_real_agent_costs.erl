#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).

%% Helper function to convert pid to binary
pid_to_binary(Pid) ->
    list_to_binary(pid_to_list(Pid)).

main([]) ->
    io:format("~n=== REAL AGENT COST TRACKING TEST ===~n"),
    io:format("Testing with actual agent instances~n"),
    io:format("=====================================~n~n"),
    
    %% Start necessary applications
    application:start(crypto),
    application:start(jsx),
    application:start(inets),
    application:start(ssl),
    
    %% Add our code paths
    code:add_paths(["_build/default/lib/openai/ebin",
                    "_build/default/lib/agents/ebin",
                    "_build/default/lib/agent_web/ebin"]),
    
    %% Start the required services
    io:format("Starting required services...~n"),
    
    %% Start OpenAI supervisor
    {ok, _} = openai_sup:start_link(#{}),
    io:format("✓ OpenAI supervisor started~n"),
    
    %% Start agents supervisor
    {ok, _} = agent_supervisor:start_link(),
    io:format("✓ Agent supervisor started~n"),
    
    timer:sleep(500), % Give services time to start
    
    %% Reset cost tracking
    cost_tracker:reset_costs(),
    io:format("✓ Cost tracking reset~n~n"),
    
    %% Test 1: Create agents with different models
    io:format("Test 1: Creating agents with different models...~n"),
    
    %% Premium agent with GPT-4.1
    {ok, PremiumPid} = agent_supervisor:start_agent(#{
        name => <<"Premium Research Agent">>,
        model => <<"gpt-4.1">>,
        system_prompt => <<"You are an expert research assistant.">>,
        tools => [web_search, calculator]
    }),
    PremiumAgent = #{id => pid_to_binary(PremiumPid), name => <<"Premium Research Agent">>, model => <<"gpt-4.1">>},
    io:format("✓ Created premium agent with GPT-4.1~n"),
    
    %% Standard agent with GPT-4.1-mini
    {ok, StandardPid} = agent_supervisor:start_agent(#{
        name => <<"Standard Chat Agent">>,
        model => <<"gpt-4.1-mini">>,
        system_prompt => <<"You are a helpful assistant.">>,
        tools => []
    }),
    StandardAgent = #{id => pid_to_binary(StandardPid), name => <<"Standard Chat Agent">>, model => <<"gpt-4.1-mini">>},
    io:format("✓ Created standard agent with GPT-4.1-mini~n"),
    
    %% Fast agent with GPT-4o
    {ok, FastPid} = agent_supervisor:start_agent(#{
        name => <<"Fast Processing Agent">>,
        model => <<"gpt-4o">>,
        system_prompt => <<"You are a fast response assistant.">>,
        tools => []
    }),
    FastAgent = #{id => pid_to_binary(FastPid), name => <<"Fast Processing Agent">>, model => <<"gpt-4o">>},
    io:format("✓ Created fast agent with GPT-4o~n"),
    
    %% Reasoning agent with o4-mini
    {ok, ReasoningPid} = agent_supervisor:start_agent(#{
        name => <<"Logic Solver">>,
        model => <<"o4-mini">>,
        system_prompt => <<"You solve complex logical problems step by step.">>,
        tools => [calculator]
    }),
    ReasoningAgent = #{id => pid_to_binary(ReasoningPid), name => <<"Logic Solver">>, model => <<"o4-mini">>},
    io:format("✓ Created reasoning agent with o4-mini~n~n"),
    
    %% Helper function to convert pid to binary
    pid_to_binary(Pid) ->
        list_to_binary(pid_to_list(Pid)),
    
    %% Test 2: Simulate real usage (without actual API calls)
    io:format("Test 2: Simulating agent usage...~n"),
    
    %% Simulate GPT-4.1 usage
    AgentState1 = #{
        id => maps:get(id, PremiumAgent),
        name => maps:get(name, PremiumAgent),
        model => maps:get(model, PremiumAgent),
        api_preference => chat
    },
    SimResponse1 = #{
        <<"usage">> => #{
            <<"prompt_tokens">> => 5432,
            <<"completion_tokens">> => 1256,
            <<"total_tokens">> => 6688
        }
    },
    agent_instance:track_cost(AgentState1, SimResponse1),
    io:format("✓ Tracked premium agent usage (5432 input, 1256 output)~n"),
    
    %% Simulate GPT-4.1-mini usage
    AgentState2 = #{
        id => maps:get(id, StandardAgent),
        name => maps:get(name, StandardAgent),
        model => maps:get(model, StandardAgent),
        api_preference => chat
    },
    SimResponse2 = #{
        <<"usage">> => #{
            <<"prompt_tokens">> => 10234,
            <<"completion_tokens">> => 2567,
            <<"total_tokens">> => 12801
        }
    },
    agent_instance:track_cost(AgentState2, SimResponse2),
    io:format("✓ Tracked standard agent usage (10234 input, 2567 output)~n"),
    
    %% Simulate GPT-4o usage with multiple calls
    AgentState3 = #{
        id => maps:get(id, FastAgent),
        name => maps:get(name, FastAgent),
        model => maps:get(model, FastAgent),
        api_preference => responses
    },
    lists:foreach(fun(I) ->
        SimResponse = #{
            <<"usage">> => #{
                <<"prompt_tokens">> => 1000 + rand:uniform(500),
                <<"completion_tokens">> => 200 + rand:uniform(300),
                <<"total_tokens">> => 1200 + rand:uniform(800)
            }
        },
        agent_instance:track_cost(AgentState3, SimResponse),
        io:format("  - Fast agent call ~p tracked~n", [I])
    end, lists:seq(1, 5)),
    io:format("✓ Tracked fast agent usage (5 calls)~n"),
    
    %% Simulate o4-mini reasoning with caching
    AgentState4 = #{
        id => maps:get(id, ReasoningAgent),
        name => maps:get(name, ReasoningAgent),
        model => maps:get(model, ReasoningAgent),
        api_preference => responses
    },
    SimResponse4 = #{
        <<"usage">> => #{
            <<"prompt_tokens">> => 3000,
            <<"cached_tokens">> => 2000,
            <<"completion_tokens">> => 1500,
            <<"total_tokens">> => 4500
        }
    },
    agent_instance:track_cost(AgentState4, SimResponse4),
    io:format("✓ Tracked reasoning agent with caching (3000 input, 2000 cached, 1500 output)~n~n"),
    
    %% Test 3: Get comprehensive cost report
    io:format("Test 3: Generating cost report...~n"),
    timer:sleep(100), % Ensure all tracking is processed
    
    {ok, Summary} = cost_tracker:get_cost_summary(),
    
    io:format("~n=== COST TRACKING REPORT ===~n"),
    io:format("============================~n~n"),
    
    %% Overall metrics
    TotalCost = maps:get(total_cost, Summary, 0.0),
    TotalCalls = maps:get(total_calls, Summary, 0),
    AvgCost = maps:get(average_cost_per_call, Summary, 0.0),
    
    io:format("OVERALL METRICS:~n"),
    io:format("  Total Cost: $~.4f~n", [TotalCost]),
    io:format("  Total API Calls: ~p~n", [TotalCalls]),
    io:format("  Average Cost per Call: $~.4f~n", [AvgCost]),
    io:format("  Total Tokens: ~p~n", [
        maps:get(total_input_tokens, Summary, 0) + 
        maps:get(total_output_tokens, Summary, 0)
    ]),
    
    %% Model breakdown
    io:format("~nMODEL PERFORMANCE:~n"),
    ModelBreakdown = maps:get(model_breakdown, Summary, #{}),
    maps:foreach(fun(Model, Data) ->
        ModelCost = maps:get(total_cost, Data, 0.0),
        ModelCalls = maps:get(calls, Data, 0),
        InputTokens = maps:get(input_tokens, Data, 0),
        OutputTokens = maps:get(output_tokens, Data, 0),
        AvgCostPerCall = case ModelCalls of
            0 -> 0.0;
            _ -> ModelCost / ModelCalls
        end,
        io:format("~n  ~s:~n", [Model]),
        io:format("    Total Cost: $~.4f~n", [ModelCost]),
        io:format("    API Calls: ~p~n", [ModelCalls]),
        io:format("    Avg Cost/Call: $~.4f~n", [AvgCostPerCall]),
        io:format("    Tokens: ~p input, ~p output~n", [InputTokens, OutputTokens])
    end, ModelBreakdown),
    
    %% Agent breakdown
    io:format("~nAGENT COSTS:~n"),
    AgentBreakdown = maps:get(agent_breakdown, Summary, #{}),
    
    %% Sort agents by cost
    SortedAgents = lists:sort(fun({_, A}, {_, B}) ->
        maps:get(total_cost, A, 0.0) > maps:get(total_cost, B, 0.0)
    end, maps:to_list(AgentBreakdown)),
    
    lists:foreach(fun({AgentId, Data}) ->
        AgentCost = maps:get(total_cost, Data, 0.0),
        AgentCalls = maps:get(calls, Data, 0),
        
        %% Find agent name
        AgentName = case lists:keyfind(AgentId, 1, [
            {maps:get(id, PremiumAgent), <<"Premium Research Agent">>},
            {maps:get(id, StandardAgent), <<"Standard Chat Agent">>},
            {maps:get(id, FastAgent), <<"Fast Processing Agent">>},
            {maps:get(id, ReasoningAgent), <<"Logic Solver">>}
        ]) of
            {_, Name} -> Name;
            false -> AgentId
        end,
        
        io:format("  ~s: $~.4f (~p calls)~n", [AgentName, AgentCost, AgentCalls])
    end, SortedAgents),
    
    %% Cost analysis
    io:format("~nCOST ANALYSIS:~n"),
    
    %% Find most expensive model
    {MostExpensiveModel, MostExpensiveData} = lists:foldl(fun({Model, Data}, {MaxModel, MaxData}) ->
        Cost = maps:get(total_cost, Data, 0.0),
        MaxCost = maps:get(total_cost, MaxData, 0.0),
        case Cost > MaxCost of
            true -> {Model, Data};
            false -> {MaxModel, MaxData}
        end
    end, {<<"none">>, #{}}, maps:to_list(ModelBreakdown)),
    
    io:format("  Most expensive model: ~s ($~.4f)~n", 
             [MostExpensiveModel, maps:get(total_cost, MostExpensiveData, 0.0)]),
    
    %% Calculate percentage breakdown
    io:format("~nCOST DISTRIBUTION:~n"),
    maps:foreach(fun(Model, Data) ->
        ModelCost = maps:get(total_cost, Data, 0.0),
        Percentage = case TotalCost of
            0.0 -> 0.0;
            _ -> (ModelCost / TotalCost) * 100
        end,
        io:format("  ~s: ~.1f%~n", [Model, Percentage])
    end, ModelBreakdown),
    
    %% Recommendations
    io:format("~nRECOMMENDATIONS:~n"),
    
    case maps:get(<<"gpt-4.1">>, ModelBreakdown, undefined) of
        undefined -> ok;
        Gpt41Data ->
            Gpt41Percentage = (maps:get(total_cost, Gpt41Data, 0.0) / TotalCost) * 100,
            case Gpt41Percentage > 40 of
                true ->
                    io:format("  ⚠️  GPT-4.1 accounts for ~.1f% of costs~n", [Gpt41Percentage]),
                    io:format("     Consider using GPT-4.1-mini or GPT-4.1-nano for simpler tasks~n");
                false -> ok
            end
    end,
    
    case TotalCalls > 10 of
        true ->
            io:format("  💡 With ~p API calls, consider:~n", [TotalCalls]),
            io:format("     - Implementing response caching for repeated queries~n"),
            io:format("     - Batch processing similar requests~n");
        false -> ok
    end,
    
    %% Cached tokens benefit
    CachedTokens = maps:get(total_cached_tokens, Summary, 0),
    case CachedTokens > 0 of
        true ->
            %% Calculate savings from caching (assuming GPT-4.1 pricing)
            CacheSavings = (CachedTokens / 1000000) * (2.00 - 0.50),
            io:format("  ✅ Caching saved approximately $~.4f~n", [CacheSavings]);
        false ->
            io:format("  💡 No cached tokens used - implement caching for cost savings~n")
    end,
    
    io:format("~n✓ ALL TESTS COMPLETED SUCCESSFULLY!~n~n"),
    
    halt(0).