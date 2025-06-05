#!/usr/bin/env escript

main(_) ->
    io:format("Testing Erlang agent system...~n"),
    
    % Check if we can start a basic shell
    case os:cmd("cd /Users/agent/agents.erl && ./rebar3 shell --eval \"io:format('System loaded~n'), halt(0).\"") of
        Result ->
            io:format("Rebar3 result: ~s~n", [Result])
    end,
    
    % Test the start script
    io:format("Testing start script...~n"),
    case os:cmd("cd /Users/agent/agents.erl && timeout 5s ./scripts/start_web.sh 2>&1 || echo 'Timeout reached'") of
        StartResult ->
            io:format("Start script result: ~s~n", [StartResult])
    end.