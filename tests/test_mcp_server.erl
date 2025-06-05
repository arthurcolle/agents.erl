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
    application:ensure_started(gproc),
    application:ensure_started(mnesia),
    
    % Start MCP components
    {ok, _} = mcp_registry:start_link(),
    {ok, _} = mcp_manager:start_link(),
    {ok, _} = mcp_connection_manager:start_link(),
    
    % Setup basic routing
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/mcp/servers", mcp_api_handler, []},
            {"/api/mcp/servers/:id", mcp_api_handler, []},
            {"/api/mcp/local/[...]", mcp_management_handler, []},
            {"/api/mcp/status", test_status_handler, []}
        ]}
    ]),
    
    % Start Cowboy
    {ok, _} = cowboy:start_clear(test_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("Test MCP server started on http://localhost:8080~n"),
    io:format("Available endpoints:~n"),
    io:format("  GET /api/mcp/servers~n"),
    io:format("  GET /api/mcp/local/servers~n"),
    io:format("  GET /api/mcp/status~n"),
    
    % Keep running
    timer:sleep(300000). % 5 minutes