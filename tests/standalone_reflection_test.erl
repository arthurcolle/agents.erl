#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin -Wall

main(_) ->
    io:format("~n=== Standalone Deep Reflection Test ===~n~n"),
    
    %% Test the reflection without full agent infrastructure
    test_basic_reflection(),
    
    io:format("~n=== Test Complete ===~n").

test_basic_reflection() ->
    %% Test 1: Test code reflection scoring
    io:format("1. Testing reflection scoring...~n"),
    
    %% Simulate what the reflection engine would do
    TestCode1 = <<"fibonacci(0) -> 0;\nfibonacci(1) -> 1;\nfibonacci(N) -> fibonacci(N-1) + fibonacci(N-2).">>,
    TestCode2 = <<"hello() -> io:format(\"Hello, World!~n\").">>,
    
    Score1 = analyze_code_beauty(TestCode1),
    Score2 = analyze_code_beauty(TestCode2),
    
    io:format("   Fibonacci beauty score: ~.2f~n", [Score1]),
    io:format("   Hello World beauty score: ~.2f~n", [Score2]),
    
    %% Test 2: Philosophical analysis
    io:format("~n2. Testing philosophical analysis...~n"),
    
    Philosophy1 = analyze_philosophy(TestCode1),
    Philosophy2 = analyze_philosophy(TestCode2),
    
    io:format("   Fibonacci: ~s~n", [Philosophy1]),
    io:format("   Hello World: ~s~n", [Philosophy2]),
    
    %% Test 3: Metacognitive simulation
    io:format("~n3. Testing metacognitive analysis...~n"),
    
    Decision = #{
        context => <<"Implementing factorial function">>,
        choice => <<"Use tail recursion for efficiency">>,
        alternatives => [<<"Simple recursion">>, <<"Iterative approach">>, <<"Memoization">>],
        reasoning => <<"Tail recursion prevents stack overflow for large inputs">>
    },
    
    Confidence = analyze_decision_confidence(Decision),
    io:format("   Decision confidence: ~.2f~n", [Confidence]),
    io:format("   Reasoning quality: ~s~n", [evaluate_reasoning(Decision)]).

%% Simplified analysis functions
analyze_code_beauty(Code) ->
    Lines = binary:split(Code, <<"\n">>, [global]),
    LineCount = length(Lines),
    
    %% Simple heuristics
    BaseScore = 0.5,
    
    %% Bonus for pattern matching
    PatternBonus = case binary:match(Code, <<"->">>) of
        nomatch -> 0;
        _ -> 0.2
    end,
    
    %% Bonus for reasonable length
    LengthBonus = if
        LineCount >= 3, LineCount =< 10 -> 0.2;
        true -> 0
    end,
    
    %% Penalty for complexity
    ComplexityPenalty = if
        LineCount > 20 -> -0.1;
        true -> 0
    end,
    
    min(1.0, max(0.0, BaseScore + PatternBonus + LengthBonus + ComplexityPenalty)).

analyze_philosophy(Code) ->
    case binary:match(Code, <<"fibonacci">>) of
        nomatch ->
            case binary:match(Code, <<"hello">>) of
                nomatch -> <<"This code embodies pure computation">>;
                _ -> <<"A greeting - the fundamental act of communication and connection">>
            end;
        _ ->
            <<"The Fibonacci sequence - nature's algorithm, embodying recursive beauty and mathematical truth">>
    end.

analyze_decision_confidence(Decision) ->
    %% Base confidence
    Base = 0.5,
    
    %% Bonus for considering alternatives
    AlternativeBonus = length(maps:get(alternatives, Decision, [])) * 0.1,
    
    %% Bonus for clear reasoning
    ReasoningBonus = case maps:get(reasoning, Decision, <<>>) of
        <<>> -> 0;
        R when byte_size(R) > 20 -> 0.2;
        _ -> 0.1
    end,
    
    min(1.0, Base + AlternativeBonus + ReasoningBonus).

evaluate_reasoning(Decision) ->
    Reasoning = maps:get(reasoning, Decision, <<>>),
    Alternatives = maps:get(alternatives, Decision, []),
    
    case {byte_size(Reasoning), length(Alternatives)} of
        {R, A} when R > 30, A >= 3 -> <<"Thorough and well-considered">>;
        {R, _} when R > 20 -> <<"Good reasoning provided">>;
        {_, A} when A >= 2 -> <<"Multiple alternatives considered">>;
        _ -> <<"Basic reasoning">>
    end.