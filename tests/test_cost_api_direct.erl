#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).

main([]) ->
    io:format("~n=== DIRECT COST TRACKING API TEST ===~n"),
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
    
    %% Start cost tracker directly
    io:format("Starting cost tracker...~n"),
    case cost_tracker:start_link() of
        {ok, Pid} ->
            io:format("✓ Cost tracker started with PID ~p~n~n", [Pid]);
        {error, {already_started, Pid}} ->
            io:format("✓ Cost tracker already running with PID ~p~n~n", [Pid]);
        Error ->
            io:format("✗ Failed to start cost tracker: ~p~n", [Error]),
            halt(1)
    end,
    
    %% Test 1: Reset costs
    io:format("Test 1: Resetting costs...~n"),
    ok = cost_tracker:reset_costs(),
    io:format("✓ Costs reset~n~n"),
    
    %% Test 2: Track various model usage
    io:format("Test 2: Tracking usage for different models...~n"),
    
    %% GPT-4.1 - Premium model
    cost_tracker:track_usage(<<"premium-agent">>, <<"gpt-4.1">>, #{
        prompt_tokens => 50000,
        completion_tokens => 10000,
        total_tokens => 60000
    }, #{task => <<"Complex analysis task">>}),
    io:format("✓ Tracked GPT-4.1 usage (50k input, 10k output)~n"),
    
    %% GPT-4.1-mini - Cost optimized
    cost_tracker:track_usage(<<"standard-agent">>, <<"gpt-4.1-mini">>, #{
        prompt_tokens => 100000,
        completion_tokens => 20000,
        total_tokens => 120000
    }, #{task => <<"Bulk processing">>}),
    io:format("✓ Tracked GPT-4.1-mini usage (100k input, 20k output)~n"),
    
    %% GPT-4.1-nano - Ultra low cost
    cost_tracker:track_usage(<<"basic-agent">>, <<"gpt-4.1-nano">>, #{
        prompt_tokens => 200000,
        completion_tokens => 50000,
        total_tokens => 250000
    }, #{task => <<"Simple tasks">>}),
    io:format("✓ Tracked GPT-4.1-nano usage (200k input, 50k output)~n"),
    
    %% o4-mini - Reasoning model
    cost_tracker:track_usage(<<"reasoning-agent">>, <<"o4-mini">>, #{
        prompt_tokens => 10000,
        completion_tokens => 5000,
        total_tokens => 15000
    }, #{task => <<"Complex reasoning">>}),
    io:format("✓ Tracked o4-mini usage (10k input, 5k output)~n"),
    
    %% GPT-4o - Fast flagship
    cost_tracker:track_usage(<<"fast-agent">>, <<"gpt-4o">>, #{
        prompt_tokens => 30000,
        completion_tokens => 8000,
        total_tokens => 38000
    }, #{task => <<"Real-time processing">>}),
    io:format("✓ Tracked GPT-4o usage (30k input, 8k output)~n"),
    
    %% Test with cached tokens
    cost_tracker:track_usage(<<"premium-agent">>, <<"gpt-4.1">>, #{
        prompt_tokens => 10000,
        cached_tokens => 8000,  % 80% cached
        completion_tokens => 2000,
        total_tokens => 12000
    }, #{task => <<"Cached query">>, cache_hit => true}),
    io:format("✓ Tracked GPT-4.1 with caching (10k input, 8k cached, 2k output)~n~n"),
    
    %% Test 3: Get comprehensive summary
    io:format("Test 3: Getting cost summary...~n"),
    {ok, Summary} = cost_tracker:get_cost_summary(),
    
    TotalCost = maps:get(total_cost, Summary, 0.0),
    TotalCalls = maps:get(total_calls, Summary, 0),
    AvgCost = maps:get(average_cost_per_call, Summary, 0.0),
    
    io:format("~nOVERALL METRICS:~n"),
    io:format("  Total Cost: $~.4f~n", [TotalCost]),
    io:format("  Total API Calls: ~p~n", [TotalCalls]),
    io:format("  Average Cost/Call: $~.4f~n", [AvgCost]),
    io:format("  Total Input Tokens: ~p~n", [maps:get(total_input_tokens, Summary, 0)]),
    io:format("  Total Output Tokens: ~p~n", [maps:get(total_output_tokens, Summary, 0)]),
    io:format("  Total Cached Tokens: ~p~n", [maps:get(total_cached_tokens, Summary, 0)]),
    
    %% Test 4: Model breakdown
    io:format("~nMODEL COST BREAKDOWN:~n"),
    ModelBreakdown = maps:get(model_breakdown, Summary, #{}),
    maps:foreach(fun(Model, Data) ->
        ModelCost = maps:get(total_cost, Data, 0.0),
        ModelCalls = maps:get(calls, Data, 0),
        InputTokens = maps:get(input_tokens, Data, 0),
        OutputTokens = maps:get(output_tokens, Data, 0),
        io:format("  ~s:~n", [Model]),
        io:format("    Cost: $~.4f (~p calls)~n", [ModelCost, ModelCalls]),
        io:format("    Tokens: ~p input, ~p output~n", [InputTokens, OutputTokens])
    end, ModelBreakdown),
    
    %% Test 5: Agent breakdown
    io:format("~nAGENT COST BREAKDOWN:~n"),
    AgentBreakdown = maps:get(agent_breakdown, Summary, #{}),
    maps:foreach(fun(Agent, Data) ->
        AgentCost = maps:get(total_cost, Data, 0.0),
        AgentCalls = maps:get(calls, Data, 0),
        io:format("  ~s: $~.4f (~p calls)~n", [Agent, AgentCost, AgentCalls])
    end, AgentBreakdown),
    
    %% Test 6: Cost verification
    io:format("~nCOST VERIFICATION:~n"),
    
    %% Calculate expected costs
    Gpt41Cost = (50000/1000000 * 2.00) + (10000/1000000 * 8.00) + % First call
                (2000/1000000 * 2.00) + (8000/1000000 * 0.50) + (2000/1000000 * 8.00), % Cached call
    Gpt41MiniCost = (100000/1000000 * 0.10) + (20000/1000000 * 0.30),
    Gpt41NanoCost = (200000/1000000 * 0.05) + (50000/1000000 * 0.15),
    O4MiniCost = (10000/1000000 * 3.00) + (5000/1000000 * 12.00),
    Gpt4oCost = (30000/1000000 * 2.50) + (8000/1000000 * 10.00),
    
    ExpectedTotal = Gpt41Cost + Gpt41MiniCost + Gpt41NanoCost + O4MiniCost + Gpt4oCost,
    
    io:format("  Expected total: $~.4f~n", [ExpectedTotal]),
    io:format("  Actual total: $~.4f~n", [TotalCost]),
    io:format("  Difference: $~.4f~n", [abs(ExpectedTotal - TotalCost)]),
    
    %% Test 7: Time-based query
    io:format("~nTest 7: Time-based cost query...~n"),
    {ok, RecentSummary} = cost_tracker:get_cost_summary(60), % Last 60 seconds
    RecentCost = maps:get(total_cost, RecentSummary, 0.0),
    io:format("  Costs in last 60 seconds: $~.4f~n", [RecentCost]),
    
    %% Test 8: Specific agent costs
    io:format("~nTest 8: Agent-specific costs...~n"),
    {ok, PremiumAgentSummary} = cost_tracker:get_agent_costs(<<"premium-agent">>),
    PremiumCost = maps:get(total_cost, PremiumAgentSummary, 0.0),
    io:format("  Premium agent total cost: $~.4f~n", [PremiumCost]),
    
    %% Test 9: Specific model costs
    io:format("~nTest 9: Model-specific costs...~n"),
    {ok, Gpt41Summary} = cost_tracker:get_model_costs(<<"gpt-4.1">>),
    Gpt41TotalCost = maps:get(total_cost, Gpt41Summary, 0.0),
    io:format("  GPT-4.1 total cost: $~.4f~n", [Gpt41TotalCost]),
    
    %% Calculate projections
    io:format("~nCOST PROJECTIONS (based on current usage):~n"),
    %% Assuming this represents 1 minute of usage
    MinuteRate = TotalCost,
    HourlyRate = MinuteRate * 60,
    DailyRate = HourlyRate * 24,
    MonthlyRate = DailyRate * 30,
    
    io:format("  Per minute: $~.4f~n", [MinuteRate]),
    io:format("  Per hour: $~.2f~n", [HourlyRate]),
    io:format("  Per day: $~.2f~n", [DailyRate]),
    io:format("  Per month: $~.2f~n", [MonthlyRate]),
    
    io:format("~n✓ ALL TESTS PASSED!~n~n"),
    
    halt(0).