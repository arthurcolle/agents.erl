#!/usr/bin/env escript

main(_) ->
    % Add all library paths
    add_paths(),
    
    % Start required applications
    start_applications(),
    
    % Start the web server
    start_web_server(),
    
    % Keep running
    io:format("Agent system started successfully!~n"),
    io:format("Web interface available at http://localhost:8080~n"),
    io:format("Press Ctrl+C to stop~n"),
    
    % Wait indefinitely
    receive
        stop -> ok
    end.

add_paths() ->
    LibDir = "_build/default/lib",
    Libs = [
        "agents", "openai", "agent_web", "jsx", "gproc", "cowboy", 
        "cowlib", "ranch", "gun", "hackney", "ssl_verify_fun",
        "certifi", "idna", "unicode_util_compat", "mimerl"
    ],
    lists:foreach(fun(Lib) ->
        Path = LibDir ++ "/" ++ Lib ++ "/ebin",
        code:add_path(Path)
    end, Libs).

start_applications() ->
    Apps = [kernel, stdlib, crypto, ssl, inets, jsx, gproc, mnesia, ranch, cowlib, cowboy],
    lists:foreach(fun(App) ->
        case application:start(App) of
            ok -> io:format("Started ~p~n", [App]);
            {error, {already_started, _}} -> ok;
            Error -> io:format("Failed to start ~p: ~p~n", [App, Error])
        end
    end, Apps).

start_web_server() ->
    % Start the main applications
    case application:start(openai) of
        ok -> io:format("Started openai~n");
        {error, {already_started, _}} -> ok;
        Error1 -> io:format("Failed to start openai: ~p~n", [Error1])
    end,
    
    case application:start(agents) of
        ok -> io:format("Started agents~n");
        {error, {already_started, _}} -> ok;
        Error2 -> io:format("Failed to start agents: ~p~n", [Error2])
    end,
    
    case application:start(agent_web) of
        ok -> io:format("Started agent_web~n");
        {error, {already_started, _}} -> ok;
        Error3 -> io:format("Failed to start agent_web: ~p~n", [Error3])
    end.