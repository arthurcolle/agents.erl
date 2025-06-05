#!/usr/bin/env escript

main(_) ->
    % Start required applications
    application:ensure_all_started(ssl),
    application:ensure_all_started(gun),
    
    io:format("Testing HTTPS MCP connection...~n"),
    
    % Test basic HTTPS connectivity with gun
    case gun:open("mcp.zapier.com", 443, #{protocols => [http], transport => tls}) of
        {ok, ConnPid} ->
            io:format("Successfully opened HTTPS connection to mcp.zapier.com~n"),
            case gun:await_up(ConnPid, 5000) of
                {ok, Protocol} ->
                    io:format("Connection established with protocol: ~p~n", [Protocol]),
                    gun:close(ConnPid),
                    io:format("HTTPS support is working!~n");
                {error, Reason} ->
                    io:format("Failed to establish connection: ~p~n", [Reason]),
                    gun:close(ConnPid)
            end;
        {error, Reason} ->
            io:format("Failed to open HTTPS connection: ~p~n", [Reason])
    end,
    
    % Test URL parsing
    Url = <<"https://mcp.zapier.com/">>,
    case uri_string:parse(Url) of
        #{scheme := Scheme, host := Host, port := Port} ->
            io:format("Successfully parsed URL: ~p://~p:~p~n", [Scheme, Host, Port]);
        ParseResult ->
            io:format("URL parsing result: ~p~n", [ParseResult])
    end.