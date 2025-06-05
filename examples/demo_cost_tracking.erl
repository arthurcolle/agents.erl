#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).

main([]) ->
    io:format("~n=== COMPREHENSIVE COST TRACKING DEMO ===~n"),
    io:format("Using latest OpenAI models with accurate pricing~n~n"),
    
    %% Start necessary applications
    application:start(crypto),
    application:start(jsx),
    
    %% Add our code paths
    code:add_paths(["_build/default/lib/openai/ebin",
                    "_build/default/lib/agents/ebin"]),
    
    %% Start cost tracker
    {ok, _} = cost_tracker:start_link(),
    
    %% Simulate realistic usage patterns
    io:format("Simulating AI agent usage with different models...~n"),
    io:format("================================================~n~n"),
    
    %% Agent 1: Complex tasks with GPT-4.1
    io:format("Agent 1 (Research Bot) - Using GPT-4.1 for complex analysis~n"),
    lists:foreach(fun(I) ->
        Tokens = 5000 + rand:uniform(3000),
        OutputTokens = 1000 + rand:uniform(500),
        cost_tracker:track_usage(<<"research-bot">>, <<"gpt-4.1">>, #{
            prompt_tokens => Tokens,
            completion_tokens => OutputTokens,
            total_tokens => Tokens + OutputTokens
        }, #{task => <<"Complex research query ", (integer_to_binary(I))/binary>>}),
        io:format("  - Query ~p: ~p input, ~p output tokens~n", [I, Tokens, OutputTokens])
    end, lists:seq(1, 3)),
    
    %% Agent 2: High-volume with GPT-4.1-mini
    io:format("~nAgent 2 (Chat Bot) - Using GPT-4.1-mini for customer support~n"),
    lists:foreach(fun(I) ->
        Tokens = 500 + rand:uniform(500),
        OutputTokens = 200 + rand:uniform(300),
        cost_tracker:track_usage(<<"chat-bot">>, <<"gpt-4.1-mini">>, #{
            prompt_tokens => Tokens,
            completion_tokens => OutputTokens,
            total_tokens => Tokens + OutputTokens
        }, #{task => <<"Customer query ", (integer_to_binary(I))/binary>>}),
        io:format("  - Query ~p: ~p input, ~p output tokens~n", [I, Tokens, OutputTokens])
    end, lists:seq(1, 10)),
    
    %% Agent 3: Reasoning tasks with o4-mini
    io:format("~nAgent 3 (Logic Solver) - Using o4-mini for reasoning~n"),
    lists:foreach(fun(I) ->
        Tokens = 2000 + rand:uniform(1000),
        OutputTokens = 1500 + rand:uniform(500),
        cost_tracker:track_usage(<<"logic-solver">>, <<"o4-mini">>, #{
            prompt_tokens => Tokens,
            completion_tokens => OutputTokens,
            total_tokens => Tokens + OutputTokens
        }, #{task => <<"Logic problem ", (integer_to_binary(I))/binary>>}),
        io:format("  - Problem ~p: ~p input, ~p output tokens~n", [I, Tokens, OutputTokens])
    end, lists:seq(1, 2)),
    
    timer:sleep(100),
    
    %% Get comprehensive cost report
    io:format("~n~n=== COST ANALYSIS REPORT ===~n"),
    io:format("===========================~n~n"),
    
    {ok, Summary} = cost_tracker:get_cost_summary(),
    
    io:format("OVERALL SUMMARY:~n"),
    io:format("  Total Cost: $~.4f~n", [maps:get(total_cost, Summary)]),
    io:format("  Total API Calls: ~p~n", [maps:get(total_calls, Summary)]),
    io:format("  Average Cost per Call: $~.4f~n", [maps:get(average_cost_per_call, Summary)]),
    io:format("  Total Tokens Used: ~p~n", [
        maps:get(total_input_tokens, Summary) + maps:get(total_output_tokens, Summary)
    ]),
    
    io:format("~nMODEL BREAKDOWN:~n"),
    ModelBreakdown = maps:get(model_breakdown, Summary),
    maps:foreach(fun(Model, Data) ->
        io:format("  ~s:~n", [Model]),
        io:format("    - Total Cost: $~.4f~n", [maps:get(total_cost, Data)]),
        io:format("    - Calls: ~p~n", [maps:get(calls, Data)]),
        io:format("    - Input Tokens: ~p~n", [maps:get(input_tokens, Data)]),
        io:format("    - Output Tokens: ~p~n", [maps:get(output_tokens, Data)])
    end, ModelBreakdown),
    
    io:format("~nAGENT BREAKDOWN:~n"),
    AgentBreakdown = maps:get(agent_breakdown, Summary),
    maps:foreach(fun(Agent, Data) ->
        io:format("  ~s:~n", [Agent]),
        io:format("    - Total Cost: $~.4f~n", [maps:get(total_cost, Data)]),
        io:format("    - API Calls: ~p~n", [maps:get(calls, Data)])
    end, AgentBreakdown),
    
    %% Calculate projections
    io:format("~nCOST PROJECTIONS:~n"),
    TotalCost = maps:get(total_cost, Summary),
    TotalCalls = maps:get(total_calls, Summary),
    
    %% Assuming this represents 1 hour of usage
    HourlyRate = TotalCost,
    DailyProjection = HourlyRate * 24,
    MonthlyProjection = DailyProjection * 30,
    
    io:format("  Hourly Rate: $~.4f~n", [HourlyRate]),
    io:format("  Daily Projection: $~.2f~n", [DailyProjection]),
    io:format("  Monthly Projection: $~.2f~n", [MonthlyProjection]),
    
    io:format("~nRECOMMENDATIONS:~n"),
    
    %% Check for optimization opportunities
    case maps:get(<<"gpt-4.1">>, ModelBreakdown, undefined) of
        undefined -> ok;
        Gpt41Data ->
            Gpt41Cost = maps:get(total_cost, Gpt41Data),
            case Gpt41Cost > TotalCost * 0.5 of
                true ->
                    io:format("  ⚠️  GPT-4.1 accounts for >50% of costs. Consider:~n"),
                    io:format("     - Using GPT-4.1-mini for simpler tasks~n"),
                    io:format("     - Implementing prompt caching for repeated queries~n");
                false -> ok
            end
    end,
    
    case TotalCalls > 50 of
        true ->
            io:format("  💡 High API usage detected. Consider:~n"),
            io:format("     - Batch processing similar requests~n"),
            io:format("     - Implementing response caching~n");
        false -> ok
    end,
    
    io:format("~n=== END OF REPORT ===~n~n"),
    
    halt(0).