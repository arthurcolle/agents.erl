#!/usr/bin/env escript
%% Test super agent startup after model_selection_strategy fix

main(_) ->
    % Start required applications
    ok = application:start(gproc),
    ok = application:start(mnesia),
    ok = application:start(jsx),
    ok = application:start(hackney),
    ok = application:start(cowlib),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(color),
    ok = application:start(openai),
    ok = application:start(agents),
    
    % Try to start agent_web (which should start super_agent)
    case application:start(agent_web) of
        ok ->
            io:format("SUCCESS: Super agent started successfully!~n"),
            application:stop(agent_web),
            halt(0);
        {error, Reason} ->
            io:format("FAILED: Super agent failed to start: ~p~n", [Reason]),
            halt(1)
    end.