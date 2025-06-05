#!/usr/bin/env escript

main(_) ->
    io:format("Starting test startup...~n"),
    
    % Add code paths
    code:add_paths(filelib:wildcard("_build/default/lib/*/ebin")),
    
    % Try to start applications in order
    Apps = [crypto, public_key, ssl, inets, gun, yamerl, gproc, mnesia, jsx, cowlib, cowboy],
    
    lists:foreach(fun(App) ->
        io:format("Starting ~p... ", [App]),
        case application:ensure_started(App) of
            ok -> io:format("OK~n");
            {error, Reason} -> io:format("ERROR: ~p~n", [Reason])
        end
    end, Apps),
    
    io:format("Trying to start openai...~n"),
    case application:start(openai) of
        ok -> io:format("OpenAI started OK~n");
        {error, Reason1} -> io:format("OpenAI error: ~p~n", [Reason1])
    end,
    
    io:format("Trying to start agents...~n"),
    case application:start(agents) of
        ok -> io:format("Agents started OK~n");
        {error, Reason2} -> io:format("Agents error: ~p~n", [Reason2])
    end,
    
    io:format("Trying to start agent_web...~n"),
    case application:start(agent_web) of
        ok -> io:format("Agent_web started OK~n");
        {error, Reason3} -> io:format("Agent_web error: ~p~n", [Reason3])
    end,
    
    io:format("Test complete~n").