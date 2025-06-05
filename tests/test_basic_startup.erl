#!/usr/bin/env escript
%% Test basic startup

main(_) ->
    io:format("Starting basic test...~n"),
    
    %% Add library paths
    code:add_paths(filelib:wildcard("_build/default/lib/*/ebin")),
    
    %% Start basic apps
    io:format("Starting mnesia...~n"),
    application:start(mnesia),
    
    io:format("Starting gproc...~n"), 
    application:start(gproc),
    
    io:format("Starting openai app...~n"),
    case application:ensure_all_started(openai) of
        {ok, Started1} -> io:format("OpenAI started: ~p~n", [Started1]);
        {error, Reason1} -> io:format("Failed to start openai: ~p~n", [Reason1])
    end,
    
    io:format("Test complete.~n"),
    halt(0).