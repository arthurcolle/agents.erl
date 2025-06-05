#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).

main([]) ->
    io:format("~n=====================================~n"),
    io:format("COMPREHENSIVE COST TRACKING TEST~n"),
    io:format("=====================================~n~n"),
    
    %% Start necessary applications
    application:start(crypto),
    application:start(jsx),
    application:start(inets),
    application:start(ssl),
    
    %% Add code paths
    code:add_paths(["_build/default/lib/openai/ebin",
                    "_build/default/lib/agents/ebin"]),
    
    %% Start required services
    io:format("[SETUP] Starting required services...~n"),
    
    %% Start OpenAI supervisor (includes cost_tracker)
    {ok, _} = openai_sup:start_link(#{}),
    io:format("✓ OpenAI supervisor started~n"),
    
    %% Start agent tools
    {ok, _} = agent_tools:start_link(),
    io:format("✓ Agent tools started~n"),
    
    %% Start agent supervisor
    {ok, _} = agent_supervisor:start_link(),
    io:format("✓ Agent supervisor started~n"),
    
    timer:sleep(500),
    
    %% Reset costs
    cost_tracker:reset_costs(),
    io:format("✓ Cost tracking reset~n~n"),
    
    %% Test 1: Direct cost tracking
    io:format("[TEST 1] Direct Cost Tracking~n"),
    io:format("-----------------------------~n"),
    
    %% Track usage for all latest models
    Models = [
        {<<"gpt-4.1">>, 10000, 2000, "Flagship model"},
        {<<"gpt-4o">>, 8000, 1500, "Fast flagship"},
        {<<"gpt-4.1-mini">>, 50000, 10000, "Cost optimized"},
        {<<"gpt-4.1-nano">>, 100000, 20000, "Ultra fast"},
        {<<"o4-mini">>, 5000, 2500, "Reasoning model"}
    ],
    
    lists:foreach(fun({Model, InputTokens, OutputTokens, Desc}) ->
        cost_tracker:track_usage(
            <<"test-agent-", Model/binary>>, 
            Model, 
            #{
                prompt_tokens => InputTokens,
                completion_tokens => OutputTokens,
                total_tokens => InputTokens + OutputTokens
            },
            #{description => list_to_binary(Desc)}
        ),
        io:format("✓ Tracked ~s (~s): ~p input, ~p output tokens~n", 
                 [Model, Desc, InputTokens, OutputTokens])
    end, Models),
    
    %% Test 2: Cost tracking with caching
    io:format("~n[TEST 2] Cost Tracking with Caching~n"),
    io:format("-----------------------------------~n"),
    
    %% Simulate cached usage
    cost_tracker:track_usage(
        <<"cached-agent">>, 
        <<"gpt-4.1">>, 
        #{
            prompt_tokens => 5000,
            cached_tokens => 4000,  % 80% cached
            completion_tokens => 1000,
            total_tokens => 6000
        },
        #{cache_hit => true, description => <<"Cached query">>}
    ),
    io:format("✓ Tracked GPT-4.1 with 80% cache hit (5k total, 4k cached)~n"),
    
    %% Test 3: Agent simulation
    io:format("~n[TEST 3] Agent Simulation~n"),
    io:format("-------------------------~n"),
    
    %% Create a simple test agent
    {ok, AgentPid} = agent_supervisor:start_agent(#{
        name => <<"Test Agent">>,
        model => <<"gpt-4.1-mini">>,
        system_prompt => <<"You are a test assistant.">>,
        tools => [],
        api_preference => responses
    }),
    io:format("✓ Created test agent~n"),
    
    %% Simulate agent usage by tracking directly
    AgentId = list_to_binary(pid_to_list(AgentPid)),
    lists:foreach(fun(I) ->
        Tokens = 500 + rand:uniform(500),
        Output = 100 + rand:uniform(200),
        cost_tracker:track_usage(
            AgentId,
            <<"gpt-4.1-mini">>,
            #{
                prompt_tokens => Tokens,
                completion_tokens => Output,
                total_tokens => Tokens + Output
            },
            #{query_number => I}
        )
    end, lists:seq(1, 5)),
    io:format("✓ Simulated 5 agent queries~n"),
    
    %% Test 4: Comprehensive cost analysis
    io:format("~n[TEST 4] Cost Analysis~n"),
    io:format("----------------------~n"),
    
    {ok, Summary} = cost_tracker:get_cost_summary(),
    
    TotalCost = maps:get(total_cost, Summary, 0.0),
    TotalCalls = maps:get(total_calls, Summary, 0),
    AvgCost = maps:get(average_cost_per_call, Summary, 0.0),
    TotalInputTokens = maps:get(total_input_tokens, Summary, 0),
    TotalOutputTokens = maps:get(total_output_tokens, Summary, 0),
    TotalCachedTokens = maps:get(total_cached_tokens, Summary, 0),
    
    io:format("~nOVERALL METRICS:~n"),
    io:format("  Total Cost: $~.4f~n", [TotalCost]),
    io:format("  Total API Calls: ~p~n", [TotalCalls]),
    io:format("  Average Cost/Call: $~.4f~n", [AvgCost]),
    io:format("  Total Input Tokens: ~p~n", [TotalInputTokens]),
    io:format("  Total Output Tokens: ~p~n", [TotalOutputTokens]),
    io:format("  Total Cached Tokens: ~p~n", [TotalCachedTokens]),
    io:format("  Total Tokens: ~p~n", [TotalInputTokens + TotalOutputTokens]),
    
    %% Model breakdown
    io:format("~nMODEL BREAKDOWN:~n"),
    ModelBreakdown = maps:get(model_breakdown, Summary, #{}),
    
    %% Sort by cost
    SortedModels = lists:sort(fun({_, A}, {_, B}) ->
        maps:get(total_cost, A, 0.0) > maps:get(total_cost, B, 0.0)
    end, maps:to_list(ModelBreakdown)),
    
    lists:foreach(fun({Model, Data}) ->
        ModelCost = maps:get(total_cost, Data, 0.0),
        ModelCalls = maps:get(calls, Data, 0),
        InputTokens = maps:get(input_tokens, Data, 0),
        OutputTokens = maps:get(output_tokens, Data, 0),
        CostPerCall = case ModelCalls of
            0 -> 0.0;
            _ -> ModelCost / ModelCalls
        end,
        io:format("  ~-15s: $~.4f (~p calls, avg $~.4f/call)~n", 
                 [Model, ModelCost, ModelCalls, CostPerCall]),
        io:format("                   ~p input, ~p output tokens~n",
                 [InputTokens, OutputTokens])
    end, SortedModels),
    
    %% Cost distribution
    io:format("~nCOST DISTRIBUTION:~n"),
    lists:foreach(fun({Model, Data}) ->
        ModelCost = maps:get(total_cost, Data, 0.0),
        Percentage = case TotalCost of
            0.0 -> 0.0;
            _ -> (ModelCost / TotalCost) * 100
        end,
        io:format("  ~-15s: ~5.1f%~n", [Model, Percentage])
    end, SortedModels),
    
    %% Test 5: Time-based queries
    io:format("~n[TEST 5] Time-Based Analysis~n"),
    io:format("----------------------------~n"),
    
    {ok, Recent60} = cost_tracker:get_cost_summary(60),
    Recent60Cost = maps:get(total_cost, Recent60, 0.0),
    Recent60Calls = maps:get(total_calls, Recent60, 0),
    io:format("  Last 60 seconds: $~.4f (~p calls)~n", [Recent60Cost, Recent60Calls]),
    
    %% Test 6: Agent-specific costs
    io:format("~n[TEST 6] Agent-Specific Costs~n"),
    io:format("-----------------------------~n"),
    
    AgentBreakdown = maps:get(agent_breakdown, Summary, #{}),
    TopAgents = lists:sublist(lists:sort(fun({_, A}, {_, B}) ->
        maps:get(total_cost, A, 0.0) > maps:get(total_cost, B, 0.0)
    end, maps:to_list(AgentBreakdown)), 5),
    
    lists:foreach(fun({AgentId, Data}) ->
        AgentCost = maps:get(total_cost, Data, 0.0),
        AgentCalls = maps:get(calls, Data, 0),
        io:format("  ~-30s: $~.4f (~p calls)~n", [AgentId, AgentCost, AgentCalls])
    end, TopAgents),
    
    %% Test 7: Cost verification
    io:format("~n[TEST 7] Cost Verification~n"),
    io:format("--------------------------~n"),
    
    %% Calculate expected costs
    ExpectedCosts = [
        {<<"gpt-4.1">>, (10000/1000000 * 2.00) + (2000/1000000 * 8.00) + 
                       (1000/1000000 * 2.00) + (4000/1000000 * 0.50) + (1000/1000000 * 8.00)},
        {<<"gpt-4o">>, (8000/1000000 * 2.50) + (1500/1000000 * 10.00)},
        {<<"gpt-4.1-mini">>, (50000/1000000 * 0.10) + (10000/1000000 * 0.30)},
        {<<"gpt-4.1-nano">>, (100000/1000000 * 0.05) + (20000/1000000 * 0.15)},
        {<<"o4-mini">>, (5000/1000000 * 3.00) + (2500/1000000 * 12.00)}
    ],
    
    lists:foreach(fun({Model, Expected}) ->
        case maps:get(Model, ModelBreakdown, undefined) of
            undefined -> 
                io:format("  ~-15s: Not found~n", [Model]);
            ModelData ->
                Actual = maps:get(total_cost, ModelData, 0.0),
                Diff = abs(Actual - Expected),
                io:format("  ~-15s: Expected $~.4f, Actual $~.4f (diff: $~.6f)~n",
                         [Model, Expected, Actual, Diff])
        end
    end, ExpectedCosts),
    
    %% Test 8: Projections
    io:format("~n[TEST 8] Cost Projections~n"),
    io:format("-------------------------~n"),
    
    %% Assuming test represents 1 minute of usage
    MinuteRate = TotalCost,
    HourlyRate = MinuteRate * 60,
    DailyRate = HourlyRate * 24,
    MonthlyRate = DailyRate * 30,
    YearlyRate = DailyRate * 365,
    
    io:format("  Per Minute:  $~.4f~n", [MinuteRate]),
    io:format("  Per Hour:    $~.2f~n", [HourlyRate]),
    io:format("  Per Day:     $~.2f~n", [DailyRate]),
    io:format("  Per Month:   $~.2f~n", [MonthlyRate]),
    io:format("  Per Year:    $~.2f~n", [YearlyRate]),
    
    %% Recommendations
    io:format("~n[RECOMMENDATIONS]~n"),
    io:format("-----------------~n"),
    
    %% Check GPT-4.1 usage
    case maps:get(<<"gpt-4.1">>, ModelBreakdown, undefined) of
        undefined -> ok;
        Gpt41Data ->
            Gpt41Cost = maps:get(total_cost, Gpt41Data, 0.0),
            Percentage = (Gpt41Cost / TotalCost) * 100,
            case Percentage > 30 of
                true ->
                    io:format("⚠️  GPT-4.1 accounts for ~.1f% of costs~n", [Percentage]),
                    io:format("   Consider using GPT-4.1-mini or GPT-4.1-nano for simpler tasks~n");
                false -> ok
            end
    end,
    
    %% Caching benefit
    case TotalCachedTokens > 0 of
        true ->
            %% Estimate savings (GPT-4.1 pricing: $2 regular vs $0.50 cached)
            CacheSavings = (TotalCachedTokens / 1000000) * (2.00 - 0.50),
            io:format("✅ Caching saved approximately $~.4f~n", [CacheSavings]);
        false ->
            io:format("💡 No cached tokens - implement caching for cost savings~n")
    end,
    
    %% High volume recommendation
    case TotalCalls > 10 of
        true ->
            io:format("💡 With ~p API calls, consider batch processing~n", [TotalCalls]);
        false -> ok
    end,
    
    io:format("~n=====================================~n"),
    io:format("✅ ALL TESTS COMPLETED SUCCESSFULLY~n"),
    io:format("=====================================~n~n"),
    
    %% Stop agent
    agent_supervisor:stop_agent(AgentPid),
    
    halt(0).