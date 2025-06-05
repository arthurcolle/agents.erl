#!/usr/bin/env escript

main(_) ->
    % Add the built applications to the code path
    code:add_paths(filelib:wildcard("_build/default/lib/*/ebin")),
    
    % Start required applications  
    application:ensure_all_started(ssl),
    application:ensure_all_started(gun),
    application:ensure_all_started(jsx),
    application:ensure_all_started(uuid),
    application:ensure_all_started(cowboy),
    
    io:format("Testing MCP HTTPS connection capabilities...~n"),
    
    % Test gun HTTPS connectivity to verify our fix works
    io:format("Testing HTTPS connectivity with gun...~n"),
    case gun:open("mcp.zapier.com", 443, #{protocols => [http], transport => tls}) of
        {ok, ConnPid} ->
            io:format("✓ Successfully opened HTTPS connection~n"),
            case gun:await_up(ConnPid, 10000) of
                {ok, Protocol} ->
                    io:format("✓ Connection established with protocol: ~p~n", [Protocol]),
                    
                    % Try to make an actual HTTP request
                    StreamRef = gun:get(ConnPid, "/"),
                    case gun:await(ConnPid, StreamRef, 5000) of
                        {response, nofin, Status, Headers} ->
                            io:format("✓ HTTP request successful: Status ~p~n", [Status]),
                            case gun:await_body(ConnPid, StreamRef, 5000) of
                                {ok, Body} ->
                                    io:format("✓ Received response body (~p bytes)~n", [byte_size(Body)]);
                                {error, timeout} ->
                                    io:format("⚠ Response body timeout~n")
                            end;
                        {response, fin, Status, _Headers} ->
                            io:format("✓ HTTP request completed: Status ~p~n", [Status]);
                        {error, Reason} ->
                            io:format("✗ HTTP request failed: ~p~n", [Reason])
                    end,
                    gun:close(ConnPid);
                {error, Reason} ->
                    io:format("✗ Failed to establish connection: ~p~n", [Reason]),
                    gun:close(ConnPid)
            end;
        {error, Reason} ->
            io:format("✗ Failed to open HTTPS connection: ~p~n", [Reason])
    end,
    
    io:format("~n=== HTTPS Support Test Complete ===~n"),
    io:format("If you see ✓ marks above, HTTPS MCP servers should work now!~n").