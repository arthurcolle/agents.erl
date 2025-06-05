%% Shell commands to demonstrate deep reflection system
%% Run these in the rebar3 shell

-module(demo_deep_reflection).
-export([demo/0]).

demo() ->
    io:format("~n=== Deep Reflection Demo Commands ===~n~n"),
    
    io:format("%% 1. Create a deeply reflective agent:~n"),
    io:format("Config = #{~n"),
    io:format("    name => <<\"Philosophical Coder\">>,~n"),
    io:format("    model => <<\"gpt-4.1-mini\">>,~n"),
    io:format("    tools => [],~n"),
    io:format("    system_prompt => <<\"You deeply reflect on code existence\">>~n"),
    io:format("}.~n"),
    io:format("{ok, Agent} = agent:create_reflective_agent(Config).~n~n"),
    
    io:format("%% 2. Test line reflection:~n"),
    io:format("Line = <<\"fibonacci(N) -> fibonacci(N-1) + fibonacci(N-2).\">>.~n"),
    io:format("deep_code_reflection_engine:reflect_on_line(Agent, Line).~n~n"),
    
    io:format("%% 3. Test philosophical evaluation:~n"),
    io:format("Code = <<\"~n"),
    io:format("hello_world() ->~n"),
    io:format("    io:format(\\\"Hello, World!~\\\\n\\\").~n"),
    io:format("\">>.~n"),
    io:format("philosophical_code_evaluator:evaluate_code_philosophy(Agent, Code).~n~n"),
    
    io:format("%% 4. Test metacognitive analysis:~n"),
    io:format("Context = #{purpose => <<\"Sort a list efficiently\">>}.~n"),
    io:format("Decision = #{~n"),
    io:format("    choice => <<\"Use quicksort algorithm\">>,~n"),
    io:format("    reasoning => <<\"Efficient average case, familiar pattern\">>,~n"),
    io:format("    confidence => 0.8~n"),
    io:format("}.~n"),
    io:format("metacognitive_code_analyzer:analyze_decision(Agent, Context, Decision).~n~n"),
    
    io:format("%% 5. Get reflection report:~n"),
    io:format("agent:get_reflection_report(Agent).~n~n"),
    
    io:format("%% 6. Test code generation with reflection:~n"),
    io:format("Spec = #{~n"),
    io:format("    type => function,~n"),
    io:format("    purpose => <<\"Calculate prime numbers\">>,~n"),
    io:format("    requirements => [<<\"Efficient algorithm\">>]~n"),
    io:format("}.~n"),
    io:format("gen_server:call(Agent, {generate_code, Spec}, infinity).~n~n"),
    
    io:format("=== End of Demo Commands ===~n~n").

%% Quick test function that can be run directly
test() ->
    %% Start required processes if not already started
    ensure_started(),
    
    %% Create agent
    Config = #{
        name => <<"Test Reflective Agent">>,
        model => <<"gpt-4.1-mini">>,
        tools => [],
        system_prompt => <<"I reflect deeply on code">>
    },
    
    {ok, Agent} = agent:create_reflective_agent(Config),
    
    %% Test simple reflection
    TestCode = <<"factorial(0) -> 1; factorial(N) -> N * factorial(N-1).">>,
    
    io:format("~nTesting reflection on: ~s~n", [TestCode]),
    
    %% Get philosophical evaluation
    case philosophical_code_evaluator:evaluate_code_philosophy(Agent, TestCode) of
        {ok, Philosophy} ->
            Synthesis = maps:get(synthesis, Philosophy, <<"No synthesis">>),
            io:format("~nPhilosophical Synthesis:~n~s~n", [Synthesis]);
        Error ->
            io:format("Error: ~p~n", [Error])
    end,
    
    %% Get reflection report
    case agent:get_reflection_report(Agent) of
        {ok, Report} ->
            io:format("~nReflection Report Summary:~n~s~n", 
                [maps:get(summary, Report, <<"No summary">>)]);
        _ ->
            ok
    end,
    
    Agent.

ensure_started() ->
    %% Ensure reflection engines are started
    Apps = [deep_code_reflection_engine, metacognitive_code_analyzer, philosophical_code_evaluator],
    lists:foreach(fun(App) ->
        case whereis(App) of
            undefined ->
                io:format("Starting ~p...~n", [App]),
                App:start_link();
            _ ->
                ok
        end
    end, Apps).