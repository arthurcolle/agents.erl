-module(test_training_simple).
-export([test/0]).

test() ->
    % Start required applications
    application:start(crypto),
    application:start(sasl),
    application:start(jsx),
    
    % Start our applications
    application:start(openai),
    application:start(agents),
    application:start(agent_web),
    
    % Wait a moment for initialization
    timer:sleep(2000),
    
    io:format("Testing timeline access...~n"),
    
    % Test direct function calls
    try
        case timeline_handler:get_all_events() of
            Events when is_list(Events) ->
                io:format("✓ Timeline has ~p events~n", [length(Events)]);
            Error ->
                io:format("✗ Timeline error: ~p~n", [Error])
        end
    catch
        _:Error ->
            io:format("✗ Timeline crashed: ~p~n", [Error])
    end,
    
    % Test training data generation
    try
        case training_data_generator:generate_training_data() of
            {ok, Count} ->
                io:format("✓ Generated ~p training samples~n", [Count]);
            Error ->
                io:format("✗ Training generation error: ~p~n", [Error])
        end
    catch
        _:Error2 ->
            io:format("✗ Training generation crashed: ~p~n", [Error2])
    end.