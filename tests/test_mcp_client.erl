#!/usr/bin/env escript

main(_) ->
    % Add the built applications to the code path
    code:add_paths(filelib:wildcard("_build/default/lib/*/ebin")),
    
    % Start required applications
    application:ensure_all_started(ssl),
    application:ensure_all_started(gun),
    
    io:format("Testing MCP client URL parsing and connection...~n"),
    
    % Test the MCP client URL parsing function directly
    TestUrl = <<"https://mcp.zapier.com/">>,
    
    io:format("Testing URL: ~p~n", [TestUrl]),
    
    case uri_string:parse(TestUrl) of
        #{scheme := Scheme, host := Host} = ParsedUrl ->
            Port = maps:get(port, ParsedUrl, case Scheme of
                <<"https">> -> 443;
                <<"http">> -> 80;
                <<"wss">> -> 443;
                <<"ws">> -> 80
            end),
            Path = maps:get(path, ParsedUrl, <<"/">>),
            io:format("Parsed successfully:~n"),
            io:format("  Scheme: ~p~n", [Scheme]),
            io:format("  Host: ~p~n", [Host]),
            io:format("  Port: ~p~n", [Port]),
            io:format("  Path: ~p~n", [Path]),
            
            % Test if this would trigger the unsupported_scheme error
            if 
                Scheme =:= <<"http">> orelse Scheme =:= <<"https">> ->
                    io:format("URL scheme is supported for SSE~n");
                Scheme =:= <<"ws">> orelse Scheme =:= <<"wss">> ->
                    io:format("URL scheme is supported for WebSocket~n");
                true ->
                    io:format("URL scheme ~p would cause unsupported_scheme error~n", [Scheme])
            end;
        ParseResult ->
            io:format("Failed to parse URL: ~p~n", [ParseResult])
    end.