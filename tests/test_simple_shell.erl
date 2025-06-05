#!/usr/bin/env escript

main(_) ->
    io:format("Starting Erlang shell test...~n"),
    
    % Start a very basic shell session
    case erl_eval:expr(erl_parse:abstract(ok), []) of
        {value, ok, _} ->
            io:format("Basic eval works~n");
        Error ->
            io:format("Basic eval failed: ~p~n", [Error])
    end,
    
    % Test application loading
    case code:which(application) of
        non_existing ->
            io:format("Application module not found~n");
        Path ->
            io:format("Application module found at: ~s~n", [Path])
    end,
    
    ok.