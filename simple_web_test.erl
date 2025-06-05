#!/usr/bin/env escript

main(_) ->
    code:add_paths(filelib:wildcard("_build/default/lib/*/ebin")),
    
    % Start required apps
    application:ensure_started(crypto),
    application:ensure_started(asn1),
    application:ensure_started(public_key),
    application:ensure_started(ssl),
    application:ensure_started(ranch),
    application:ensure_started(cowlib),
    application:ensure_started(jsx),
    
    % Create simple handler for testing
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/mcp/servers", simple_mcp_handler, []},
            {"/api/mcp/local/servers", simple_mcp_handler, []},
            {"/api/test", simple_mcp_handler, []}
        ]}
    ]),
    
    % Start Cowboy
    {ok, _} = cowboy:start_clear(simple_test_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("Simple web test server started on http://localhost:8080~n"),
    io:format("Test with: curl http://localhost:8080/api/test~n"),
    
    % Keep running
    timer:sleep(60000). % 1 minute