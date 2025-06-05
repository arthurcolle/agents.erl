%%%-------------------------------------------------------------------
%%% @doc Test module for deep reflection system
%%% This demonstrates agents deeply reflecting on code generation
%%% @end
%%%-------------------------------------------------------------------
-module(test_deep_reflection).

-export([demo/0,
         create_reflective_agent/0,
         test_code_generation/1,
         test_line_reflection/1,
         test_philosophical_evaluation/1,
         show_reflection_report/1]).

%% @doc Run complete demonstration
demo() ->
    io:format("~n=== Deep Reflection System Demo ===~n~n"),
    
    %% Create a reflective agent
    io:format("1. Creating reflective agent...~n"),
    {ok, Agent} = create_reflective_agent(),
    io:format("   ✓ Agent created: ~p~n", [Agent]),
    
    %% Test simple line reflection
    io:format("~n2. Testing line-by-line reflection...~n"),
    test_line_reflection(Agent),
    
    %% Test code generation with reflection
    io:format("~n3. Testing code generation with deep reflection...~n"),
    test_code_generation(Agent),
    
    %% Test philosophical evaluation
    io:format("~n4. Testing philosophical evaluation...~n"),
    test_philosophical_evaluation(Agent),
    
    %% Show comprehensive report
    io:format("~n5. Generating reflection report...~n"),
    show_reflection_report(Agent),
    
    io:format("~n=== Demo Complete ===~n").

%% @doc Create a reflective agent
create_reflective_agent() ->
    Config = #{
        name => <<"Deeply Reflective Agent">>,
        model => <<"gpt-4.1-mini">>,
        tools => [generate_code, reflect_on_code],
        system_prompt => <<"You are an agent that deeply reflects on every line of code you generate, 
                          considering its syntactic beauty, semantic meaning, philosophical implications, 
                          and ethical responsibilities. You contemplate the existence of each function, 
                          the purpose of each variable, and the impact of each operation.">>
    },
    
    reflective_agent_instance:start_link(Config).

%% @doc Test line-by-line reflection
test_line_reflection(Agent) ->
    %% Test reflecting on individual lines
    Lines = [
        <<"-module(example).">>,
        <<"fibonacci(0) -> 0;">>,
        <<"fibonacci(1) -> 1;">>,
        <<"fibonacci(N) when N > 1 -> fibonacci(N-1) + fibonacci(N-2).">>
    ],
    
    lists:foreach(fun(Line) ->
        io:format("~n   Reflecting on: ~s~n", [Line]),
        
        %% Direct reflection
        {ok, Reflection} = deep_code_reflection_engine:reflect_on_line(Agent, Line),
        
        io:format("   Beauty Score: ~.2f~n", [Reflection#reflection.beauty_score]),
        io:format("   Purpose Score: ~.2f~n", [Reflection#reflection.purpose_score]),
        io:format("   Impact Score: ~.2f~n", [Reflection#reflection.impact_score]),
        
        %% Show one insight
        case Reflection#reflection.insights of
            [Insight | _] -> io:format("   Insight: ~s~n", [Insight]);
            [] -> ok
        end
    end, Lines).

%% @doc Test code generation with deep reflection
test_code_generation(Agent) ->
    %% Request code generation
    CodeSpec = #{
        type => function,
        purpose => <<"Calculate factorial with memoization">>,
        requirements => [
            <<"Must handle large numbers efficiently">>,
            <<"Should use memoization for performance">>,
            <<"Must be tail-recursive">>
        ],
        constraints => [
            <<"Memory usage should be bounded">>,
            <<"Must handle negative inputs gracefully">>
        ]
    },
    
    io:format("~n   Requesting code generation with spec:~n   ~p~n", [CodeSpec]),
    
    %% This would trigger deep reflection during generation
    case gen_server:call(Agent, {generate_code, CodeSpec}, infinity) of
        {ok, Code} ->
            io:format("~n   Generated Code:~n~s~n", [Code]);
        Error ->
            io:format("~n   Error: ~p~n", [Error])
    end.

%% @doc Test philosophical evaluation
test_philosophical_evaluation(Agent) ->
    %% Sample code for philosophical evaluation
    SampleCode = <<
        "merge_sort([]) -> [];\n",
        "merge_sort([X]) -> [X];\n",
        "merge_sort(List) ->\n",
        "    {Left, Right} = lists:split(length(List) div 2, List),\n",
        "    merge(merge_sort(Left), merge_sort(Right)).\n",
        "\n",
        "merge([], Right) -> Right;\n",
        "merge(Left, []) -> Left;\n",
        "merge([H1|T1] = Left, [H2|T2] = Right) ->\n",
        "    if H1 =< H2 -> [H1 | merge(T1, Right)];\n",
        "       true -> [H2 | merge(Left, T2)]\n",
        "    end."
    >>,
    
    io:format("~n   Evaluating merge sort implementation philosophically...~n"),
    
    %% Philosophical evaluation
    {ok, Philosophy} = philosophical_code_evaluator:evaluate_code_philosophy(Agent, SampleCode),
    
    %% Show key philosophical insights
    io:format("~n   Existence: ~s~n", [get_reflection(Philosophy, [existence, reflection])]),
    io:format("~n   Ethics: ~s~n", [get_reflection(Philosophy, [ethics, reflection])]),
    io:format("~n   Aesthetics: ~s~n", [get_reflection(Philosophy, [aesthetics, reflection])]),
    io:format("~n   Metaphysics: ~s~n", [get_reflection(Philosophy, [metaphysics, reflection])]),
    io:format("~n   Synthesis: ~s~n", [maps:get(synthesis, Philosophy, <<"No synthesis">>)]).

%% @doc Show comprehensive reflection report
show_reflection_report(Agent) ->
    {ok, Report} = reflective_agent_instance:get_reflection_report(Agent),
    
    io:format("~n   === Reflection Report ===~n"),
    
    %% Show metrics
    case maps:get(reflection_metrics, Report, undefined) of
        undefined -> ok;
        Metrics ->
            io:format("~n   Metrics:~n"),
            io:format("     Lines Reflected: ~p~n", [maps:get(lines_reflected, Metrics)]),
            io:format("     Total Time: ~p microseconds~n", [maps:get(total_time, Metrics)]),
            io:format("     Avg Time/Line: ~p microseconds~n", [maps:get(avg_time_per_line, Metrics)])
    end,
    
    %% Show metacognitive report
    case maps:get(metacognitive_report, Report, undefined) of
        undefined -> ok;
        #{thinking_style := Style, confidence_metrics := Confidence} ->
            io:format("~n   Metacognitive Analysis:~n"),
            io:format("     Thinking Style: ~p~n", [Style]),
            io:format("     Average Confidence: ~.2f~n", [maps:get(average, Confidence, 0.5)])
    end,
    
    %% Show summary
    io:format("~n   Summary: ~s~n", [maps:get(summary, Report, <<"No summary available">>)]).

%% Helper to extract nested values from philosophy maps
get_reflection(Philosophy, Path) ->
    try
        lists:foldl(fun(Key, Map) ->
            maps:get(Key, Map)
        end, Philosophy, Path)
    catch
        _:_ -> <<"Not found">>
    end.