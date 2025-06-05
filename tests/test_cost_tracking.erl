#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).

main([]) ->
    io:format("Testing Cost Tracking System~n"),
    io:format("==========================~n~n"),
    
    %% Start necessary applications
    application:start(crypto),
    application:start(jsx),
    
    %% Add our code paths
    code:add_paths(["_build/default/lib/openai/ebin",
                    "_build/default/lib/agents/ebin",
                    "_build/default/lib/agent_web/ebin"]),
    
    %% Start cost tracker
    io:format("Starting cost tracker...~n"),
    {ok, _} = cost_tracker:start_link(),
    
    %% Test 1: Track some usage
    io:format("~nTest 1: Tracking usage for different models~n"),
    
    %% GPT-4.1 usage
    cost_tracker:track_usage(<<"agent-1">>, <<"gpt-4.1">>, #{
        prompt_tokens => 1000,
        completion_tokens => 500,
        total_tokens => 1500
    }),
    
    %% GPT-4.1-mini usage
    cost_tracker:track_usage(<<"agent-2">>, <<"gpt-4.1-mini">>, #{
        prompt_tokens => 5000,
        completion_tokens => 2000,
        total_tokens => 7000
    }),
    
    %% o4-mini usage
    cost_tracker:track_usage(<<"agent-3">>, <<"o4-mini">>, #{
        prompt_tokens => 2000,
        completion_tokens => 1000,
        total_tokens => 3000
    }),
    
    timer:sleep(100),
    
    %% Test 2: Get cost summary
    io:format("~nTest 2: Getting cost summary~n"),
    {ok, Summary} = cost_tracker:get_cost_summary(),
    io:format("Total cost: $~.4f~n", [maps:get(total_cost, Summary)]),
    io:format("Total calls: ~p~n", [maps:get(total_calls, Summary)]),
    io:format("Average cost per call: $~.4f~n", [maps:get(average_cost_per_call, Summary)]),
    
    %% Test 3: Get agent costs
    io:format("~nTest 3: Getting costs for agent-1~n"),
    {ok, AgentSummary} = cost_tracker:get_agent_costs(<<"agent-1">>),
    io:format("Agent-1 total cost: $~.4f~n", [maps:get(total_cost, AgentSummary)]),
    
    %% Test 4: Get model breakdown
    io:format("~nTest 4: Model breakdown~n"),
    ModelBreakdown = maps:get(model_breakdown, Summary),
    maps:foreach(fun(Model, Data) ->
        Cost = maps:get(total_cost, Data),
        Calls = maps:get(calls, Data),
        io:format("  ~s: $~.4f (~p calls)~n", [Model, Cost, Calls])
    end, ModelBreakdown),
    
    %% Test 5: Calculate expected costs
    io:format("~nTest 5: Cost calculation verification~n"),
    
    %% GPT-4.1: $2/1M input, $8/1M output
    Gpt41InputCost = (1000 / 1000000) * 2.00,
    Gpt41OutputCost = (500 / 1000000) * 8.00,
    Gpt41Total = Gpt41InputCost + Gpt41OutputCost,
    io:format("Expected GPT-4.1 cost: $~.4f~n", [Gpt41Total]),
    
    %% GPT-4.1-mini: $0.10/1M input, $0.30/1M output
    MiniInputCost = (5000 / 1000000) * 0.10,
    MiniOutputCost = (2000 / 1000000) * 0.30,
    MiniTotal = MiniInputCost + MiniOutputCost,
    io:format("Expected GPT-4.1-mini cost: $~.4f~n", [MiniTotal]),
    
    %% o4-mini: $3.00/1M input, $12.00/1M output
    O4InputCost = (2000 / 1000000) * 3.00,
    O4OutputCost = (1000 / 1000000) * 12.00,
    O4Total = O4InputCost + O4OutputCost,
    io:format("Expected o4-mini cost: $~.4f~n", [O4Total]),
    
    ExpectedTotal = Gpt41Total + MiniTotal + O4Total,
    io:format("~nExpected total: $~.4f~n", [ExpectedTotal]),
    io:format("Actual total: $~.4f~n", [maps:get(total_cost, Summary)]),
    
    io:format("~nCost tracking test completed!~n"),
    
    halt(0).