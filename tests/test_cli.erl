#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

main([]) ->
    io:format("Testing application loading...~n"),
    
    % Show code paths
    io:format("Code paths:~n"),
    [io:format("  ~s~n", [Path]) || Path <- code:get_path()],
    
    % Try to load openai app
    io:format("~nLoading openai app...~n"),
    case application:load(openai) of
        ok -> io:format("Loaded successfully~n");
        {error, {already_loaded, _}} -> io:format("Already loaded~n");
        Error -> io:format("Load error: ~p~n", [Error])
    end,
    
    % Check for openai.app
    case code:where_is_file("openai.app") of
        non_existing -> io:format("openai.app not found in code path~n");
        Path -> io:format("openai.app found at: ~s~n", [Path])
    end.