%% Quick test for deep reflection system
-module(quick_reflection_test).
-export([test/0, simple_test/0]).

%% Simple test that works without full agent infrastructure
simple_test() ->
    io:format("~n=== Simple Deep Reflection Test ===~n~n"),
    
    %% Test 1: Deep Code Reflection Engine
    io:format("1. Testing Deep Code Reflection Engine...~n"),
    case deep_code_reflection_engine:start_link() of
        {ok, Pid1} ->
            io:format("   ✓ Started reflection engine: ~p~n", [Pid1]),
            
            %% Enable reflection for this process
            deep_code_reflection_engine:enable_deep_reflection(self()),
            
            %% Test reflecting on a simple line
            TestLine = <<"fibonacci(N) -> fibonacci(N-1) + fibonacci(N-2).">>,
            case deep_code_reflection_engine:reflect_on_line(self(), TestLine) of
                {ok, Reflection} ->
                    io:format("   ✓ Reflection successful!~n"),
                    io:format("     Beauty Score: ~p~n", [maps:get(beauty_score, Reflection, 0)]),
                    io:format("     Purpose Score: ~p~n", [maps:get(purpose_score, Reflection, 0)]),
                    io:format("     Impact Score: ~p~n", [maps:get(impact_score, Reflection, 0)]);
                Error ->
                    io:format("   ✗ Reflection failed: ~p~n", [Error])
            end;
        {error, {already_started, Pid1}} ->
            io:format("   ℹ Reflection engine already running: ~p~n", [Pid1]);
        Error1 ->
            io:format("   ✗ Failed to start: ~p~n", [Error1])
    end,
    
    %% Test 2: Philosophical Code Evaluator
    io:format("~n2. Testing Philosophical Code Evaluator...~n"),
    case philosophical_code_evaluator:start_link() of
        {ok, Pid2} ->
            io:format("   ✓ Started philosophical evaluator: ~p~n", [Pid2]),
            
            %% Test philosophical evaluation
            HelloWorld = <<"hello_world() -> io:format(\"Hello, World!~n\").">>,
            case philosophical_code_evaluator:evaluate_code_philosophy(self(), HelloWorld) of
                {ok, Philosophy} ->
                    io:format("   ✓ Philosophical evaluation successful!~n"),
                    Synthesis = maps:get(synthesis, Philosophy, <<"No synthesis">>),
                    io:format("     Synthesis: ~s~n", [Synthesis]);
                Error2 ->
                    io:format("   ✗ Evaluation failed: ~p~n", [Error2])
            end;
        {error, {already_started, Pid2}} ->
            io:format("   ℹ Philosophical evaluator already running: ~p~n", [Pid2]);
        Error2 ->
            io:format("   ✗ Failed to start: ~p~n", [Error2])
    end,
    
    %% Test 3: Metacognitive Code Analyzer
    io:format("~n3. Testing Metacognitive Code Analyzer...~n"),
    case metacognitive_code_analyzer:start_link() of
        {ok, Pid3} ->
            io:format("   ✓ Started metacognitive analyzer: ~p~n", [Pid3]),
            
            %% Enable for this process
            metacognitive_code_analyzer:enable_for_agent(self()),
            
            %% Test decision analysis
            Context = #{purpose => <<"Sort a list efficiently">>},
            Decision = #{
                choice => <<"Use quicksort algorithm">>,
                reasoning => <<"Efficient average case, familiar pattern">>,
                confidence => 0.8
            },
            
            case metacognitive_code_analyzer:analyze_decision(self(), Context, Decision) of
                {ok, Analysis} ->
                    io:format("   ✓ Metacognitive analysis successful!~n"),
                    Confidence = maps:get(confidence, Analysis, 0),
                    io:format("     Adjusted Confidence: ~p~n", [Confidence]);
                Error3 ->
                    io:format("   ✗ Analysis failed: ~p~n", [Error3])
            end;
        {error, {already_started, Pid3}} ->
            io:format("   ℹ Metacognitive analyzer already running: ~p~n", [Pid3]);
        Error3 ->
            io:format("   ✗ Failed to start: ~p~n", [Error3])
    end,
    
    io:format("~n=== Test Complete ===~n").

%% Full test with agent creation
test() ->
    io:format("~n=== Full Deep Reflection Test with Agent ===~n~n"),
    
    %% Make sure all applications are started
    ensure_apps_started(),
    
    %% Create a reflective agent
    io:format("Creating reflective agent with gpt-4.1-mini...~n"),
    Config = #{
        name => <<"Test Reflective Agent">>,
        model => <<"gpt-4.1-mini">>,  % Using cheap model!
        tools => [],
        system_prompt => <<"I am a deeply reflective agent that contemplates every line of code.">>
    },
    
    case agent:create_reflective_agent(Config) of
        {ok, Agent} ->
            io:format("✓ Agent created: ~p~n~n", [Agent]),
            
            %% Test reflection capabilities
            test_agent_reflection(Agent);
            
        Error ->
            io:format("✗ Failed to create agent: ~p~n", [Error]),
            io:format("~nTrying simple test instead...~n"),
            simple_test()
    end.

test_agent_reflection(Agent) ->
    %% Test 1: Line reflection
    io:format("Testing line reflection...~n"),
    TestCode = <<"factorial(0) -> 1; factorial(N) -> N * factorial(N-1).">>,
    
    case deep_code_reflection_engine:reflect_on_line(Agent, TestCode) of
        {ok, Reflection} ->
            io:format("✓ Line reflection successful~n"),
            display_reflection(Reflection);
        Error ->
            io:format("✗ Line reflection failed: ~p~n", [Error])
    end,
    
    %% Test 2: Get reflection report
    io:format("~nGetting reflection report...~n"),
    case agent:get_reflection_report(Agent) of
        {ok, Report} ->
            io:format("✓ Got reflection report~n"),
            Summary = maps:get(summary, Report, <<"No summary">>),
            io:format("  Summary: ~s~n", [Summary]);
        Error2 ->
            io:format("✗ Failed to get report: ~p~n", [Error2])
    end,
    
    io:format("~n=== Test Complete ===~n").

display_reflection(Reflection) ->
    io:format("  Dimensions analyzed: ~p~n", [maps:keys(maps:get(dimensions, Reflection, #{}))]),
    io:format("  Beauty Score: ~.2f~n", [maps:get(beauty_score, Reflection, 0)]),
    io:format("  Purpose Score: ~.2f~n", [maps:get(purpose_score, Reflection, 0)]),
    io:format("  Impact Score: ~.2f~n", [maps:get(impact_score, Reflection, 0)]),
    
    case maps:get(insights, Reflection, []) of
        [] -> ok;
        [Insight | _] -> io:format("  Insight: ~s~n", [Insight])
    end.

ensure_apps_started() ->
    Apps = [crypto, ssl, inets, ranch, cowlib, cowboy, jsx, openai, agents],
    lists:foreach(fun(App) ->
        case application:ensure_all_started(App) of
            {ok, _} -> ok;
            {error, {already_started, _}} -> ok;
            Error -> io:format("Warning: Failed to start ~p: ~p~n", [App, Error])
        end
    end, Apps).