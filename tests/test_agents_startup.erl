#!/usr/bin/env escript
%% Test agents startup

main(_) ->
    io:format("Starting agents test...~n"),
    
    %% Add library paths
    code:add_paths(filelib:wildcard("_build/default/lib/*/ebin")),
    
    %% Start prerequisites
    application:start(crypto),
    application:start(ssl),
    application:start(mnesia),
    application:start(gproc),
    
    io:format("Starting openai app...~n"),
    application:ensure_all_started(openai),
    
    io:format("Starting agents app...~n"),
    case application:ensure_all_started(agents) of
        {ok, Started} -> 
            io:format("Agents started successfully: ~p~n", [Started]),
            io:format("Waiting 3 seconds...~n"),
            timer:sleep(3000),
            io:format("Checking registered processes...~n"),
            Registered = erlang:registered(),
            io:format("Registered processes: ~p~n", [lists:sublist(Registered, 10)]);
        {error, Reason} -> 
            io:format("Failed to start agents: ~p~n", [Reason])
    end,
    
    io:format("Test complete.~n"),
    halt(0).