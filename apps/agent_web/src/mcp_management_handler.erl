-module(mcp_management_handler).
-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    
    colored_logger:info("[MCP_MGMT] 🔗 ~s ~s", [Method, Path]),
    
    Result = case {Method, Path} of
        {<<"GET">>, <<"/api/mcp/local/servers">>} ->
            handle_get_local_servers(Req0, State);
        {<<"POST">>, <<"/api/mcp/local/servers">>} ->
            handle_create_local_server(Req0, State);
        {<<"GET">>, <<"/api/mcp/remote/servers">>} ->
            handle_get_remote_servers(Req0, State);
        {<<"POST">>, <<"/api/mcp/remote/servers">>} ->
            handle_create_remote_server(Req0, State);
        {<<"POST">>, <<"/api/mcp/discover">>} ->
            handle_discover_servers(Req0, State);
        {<<"GET">>, <<"/api/mcp/status">>} ->
            handle_get_mcp_status(Req0, State);
        {<<"POST">>, <<"/api/mcp/auto_connect">>} ->
            handle_auto_connect(Req0, State);
        {<<"POST">>, <<"/api/mcp/execute_tool">>} ->
            handle_execute_tool(Req0, State);
        {<<"POST">>, <<"/api/mcp/read_resource">>} ->
            handle_read_resource(Req0, State);
        {<<"POST">>, <<"/api/mcp/get_prompt">>} ->
            handle_get_prompt(Req0, State);
        _ ->
            handle_not_found(Req0, State)
    end,
    
    Result.

%% Handle GET /api/mcp/local/servers
handle_get_local_servers(Req0, State) ->
    try
        %% Get local MCP servers from the manager
        LocalServers = case whereis(mcp_manager) of
            undefined ->
                colored_logger:warning("[MCP_MGMT] ⚠️ MCP manager not running"),
                [];
            Pid ->
                case catch gen_server:call(Pid, get_local_servers, 5000) of
                    {'EXIT', _} ->
                        colored_logger:warning("[MCP_MGMT] ⚠️ Failed to get local servers"),
                        [];
                    Servers when is_list(Servers) ->
                        Servers;
                    _ ->
                        []
                end
        end,
        
        %% Format servers for JSON response
        FormattedServers = lists:map(fun format_server_info/1, LocalServers),
        
        Response = jsx:encode(#{
            success => true,
            servers => FormattedServers,
            count => length(FormattedServers)
        }),
        
        Req = cowboy_req:reply(200, #{
            <<"content-type">> => <<"application/json">>,
            <<"access-control-allow-origin">> => <<"*">>
        }, Response, Req0),
        
        colored_logger:success("[MCP_MGMT] ✅ Returned ~p local servers", [length(FormattedServers)]),
        {ok, Req, State}
    catch
        Error:Reason ->
            colored_logger:fire(bright, "[MCP_MGMT] ❌ Error getting local servers: ~p:~p", [Error, Reason]),
            handle_error(Req0, State, <<"Failed to get local servers">>)
    end.

%% Handle GET /api/mcp/remote/servers
handle_get_remote_servers(Req0, State) ->
    try
        %% Mock remote servers for now
        RemoteServers = [
            #{
                id => <<"remote_1">>,
                name => <<"Example Remote MCP Server">>,
                url => <<"https://api.example.com/mcp">>,
                type => <<"remote">>,
                status => <<"connected">>,
                capabilities => [<<"tools">>, <<"resources">>, <<"prompts">>],
                last_seen => erlang:system_time(millisecond)
            }
        ],
        
        Response = jsx:encode(#{
            success => true,
            servers => RemoteServers,
            count => length(RemoteServers)
        }),
        
        Req = cowboy_req:reply(200, #{
            <<"content-type">> => <<"application/json">>,
            <<"access-control-allow-origin">> => <<"*">>
        }, Response, Req0),
        
        colored_logger:success("[MCP_MGMT] ✅ Returned ~p remote servers", [length(RemoteServers)]),
        {ok, Req, State}
    catch
        Error:Reason ->
            colored_logger:fire(bright, "[MCP_MGMT] ❌ Error getting remote servers: ~p:~p", [Error, Reason]),
            handle_error(Req0, State, <<"Failed to get remote servers">>)
    end.

%% Handle POST /api/mcp/discover
handle_discover_servers(Req0, State) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        
        %% Start discovery process
        colored_logger:info("[MCP_MGMT] 🔍 Starting MCP server discovery"),
        
        %% Mock discovery for now - in real implementation would scan network
        DiscoveredServers = [
            #{
                id => <<"discovered_1">>,
                name => <<"Local MCP Server">>,
                url => <<"http://localhost:8765">>,
                type => <<"local">>,
                status => <<"discovered">>,
                capabilities => [<<"tools">>, <<"resources">>]
            },
            #{
                id => <<"discovered_2">>,
                name => <<"Remote MCP Server">>,
                url => <<"https://api.example.com/mcp">>,
                type => <<"remote">>,
                status => <<"discovered">>,
                capabilities => [<<"prompts">>, <<"tools">>]
            }
        ],
        
        Response = jsx:encode(#{
            success => true,
            discovered => DiscoveredServers,
            count => length(DiscoveredServers)
        }),
        
        Req = cowboy_req:reply(200, #{
            <<"content-type">> => <<"application/json">>,
            <<"access-control-allow-origin">> => <<"*">>
        }, Response, Req1),
        
        colored_logger:success("[MCP_MGMT] ✅ Discovery completed, found ~p servers", [length(DiscoveredServers)]),
        {ok, Req, State}
    catch
        Error:Reason ->
            colored_logger:fire(bright, "[MCP_MGMT] ❌ Discovery error: ~p:~p", [Error, Reason]),
            handle_error(Req0, State, <<"Discovery failed">>)
    end.

%% Handle GET /api/mcp/status
handle_get_mcp_status(Req0, State) ->
    try
        %% Get overall MCP status
        Status = #{
            manager_running => whereis(mcp_manager) =/= undefined,
            local_servers => 1,
            remote_clients => 1,
            auto_connect_enabled => true,
            discovery_active => false
        },
        
        Response = jsx:encode(#{
            success => true,
            status => Status
        }),
        
        Req = cowboy_req:reply(200, #{
            <<"content-type">> => <<"application/json">>,
            <<"access-control-allow-origin">> => <<"*">>
        }, Response, Req0),
        
        {ok, Req, State}
    catch
        Error:Reason ->
            colored_logger:fire(bright, "[MCP_MGMT] ❌ Status error: ~p:~p", [Error, Reason]),
            handle_error(Req0, State, <<"Failed to get status">>)
    end.

%% Handle POST /api/mcp/auto_connect
handle_auto_connect(Req0, State) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        
        colored_logger:info("[MCP_MGMT] 🔗 Auto-connecting to available MCP servers"),
        
        %% Mock auto-connect process
        ConnectedCount = 2, % Would be actual connection count
        
        Response = jsx:encode(#{
            success => true,
            connected => ConnectedCount,
            message => <<"Auto-connect completed">>
        }),
        
        Req = cowboy_req:reply(200, #{
            <<"content-type">> => <<"application/json">>,
            <<"access-control-allow-origin">> => <<"*">>
        }, Response, Req1),
        
        colored_logger:success("[MCP_MGMT] ✅ Auto-connected to ~p servers", [ConnectedCount]),
        {ok, Req, State}
    catch
        Error:Reason ->
            colored_logger:fire(bright, "[MCP_MGMT] ❌ Auto-connect error: ~p:~p", [Error, Reason]),
            handle_error(Req0, State, <<"Auto-connect failed">>)
    end.

%% Handle POST /api/mcp/execute_tool
handle_execute_tool(Req0, State) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        RequestData = jsx:decode(Body, [return_maps]),
        
        ToolName = maps:get(<<"tool">>, RequestData, <<"unknown">>),
        Arguments = maps:get(<<"arguments">>, RequestData, #{}),
        ServerId = maps:get(<<"server_id">>, RequestData, <<"default">>),
        
        colored_logger:info("[MCP_MGMT] 🔧 Executing tool ~s on server ~s", [ToolName, ServerId]),
        
        %% Mock tool execution
        Result = #{
            tool => ToolName,
            server_id => ServerId,
            result => <<"Tool executed successfully">>,
            execution_time => 42
        },
        
        Response = jsx:encode(#{
            success => true,
            result => Result
        }),
        
        Req = cowboy_req:reply(200, #{
            <<"content-type">> => <<"application/json">>,
            <<"access-control-allow-origin">> => <<"*">>
        }, Response, Req1),
        
        {ok, Req, State}
    catch
        Error:Reason ->
            colored_logger:fire(bright, "[MCP_MGMT] ❌ Tool execution error: ~p:~p", [Error, Reason]),
            handle_error(Req0, State, <<"Tool execution failed">>)
    end.

%% Handle POST /api/mcp/read_resource
handle_read_resource(Req0, State) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        RequestData = jsx:decode(Body, [return_maps]),
        
        ResourceUri = maps:get(<<"uri">>, RequestData, <<"unknown">>),
        ServerId = maps:get(<<"server_id">>, RequestData, <<"default">>),
        
        colored_logger:info("[MCP_MGMT] 📄 Reading resource ~s from server ~s", [ResourceUri, ServerId]),
        
        %% Mock resource reading
        Resource = #{
            uri => ResourceUri,
            server_id => ServerId,
            content => <<"Mock resource content">>,
            mime_type => <<"text/plain">>,
            size => 123
        },
        
        Response = jsx:encode(#{
            success => true,
            resource => Resource
        }),
        
        Req = cowboy_req:reply(200, #{
            <<"content-type">> => <<"application/json">>,
            <<"access-control-allow-origin">> => <<"*">>
        }, Response, Req1),
        
        {ok, Req, State}
    catch
        Error:Reason ->
            colored_logger:fire(bright, "[MCP_MGMT] ❌ Resource read error: ~p:~p", [Error, Reason]),
            handle_error(Req0, State, <<"Resource read failed">>)
    end.

%% Handle POST /api/mcp/get_prompt
handle_get_prompt(Req0, State) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        RequestData = jsx:decode(Body, [return_maps]),
        
        PromptName = maps:get(<<"prompt">>, RequestData, <<"unknown">>),
        Arguments = maps:get(<<"arguments">>, RequestData, #{}),
        ServerId = maps:get(<<"server_id">>, RequestData, <<"default">>),
        
        colored_logger:info("[MCP_MGMT] 💬 Getting prompt ~s from server ~s", [PromptName, ServerId]),
        
        %% Mock prompt retrieval
        Prompt = #{
            name => PromptName,
            server_id => ServerId,
            content => <<"This is a mock prompt template">>,
            arguments => Arguments
        },
        
        Response = jsx:encode(#{
            success => true,
            prompt => Prompt
        }),
        
        Req = cowboy_req:reply(200, #{
            <<"content-type">> => <<"application/json">>,
            <<"access-control-allow-origin">> => <<"*">>
        }, Response, Req1),
        
        {ok, Req, State}
    catch
        Error:Reason ->
            colored_logger:fire(bright, "[MCP_MGMT] ❌ Prompt get error: ~p:~p", [Error, Reason]),
            handle_error(Req0, State, <<"Prompt retrieval failed">>)
    end.

%% Handle POST requests for server creation (stub implementations)
handle_create_local_server(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    colored_logger:info("[MCP_MGMT] 🆕 Creating local MCP server"),
    
    Response = jsx:encode(#{
        success => true,
        message => <<"Local server creation not yet implemented">>
    }),
    
    Req = cowboy_req:reply(501, #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, Response, Req1),
    
    {ok, Req, State}.

handle_create_remote_server(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    colored_logger:info("[MCP_MGMT] 🆕 Creating remote MCP server connection"),
    
    Response = jsx:encode(#{
        success => true,
        message => <<"Remote server creation not yet implemented">>
    }),
    
    Req = cowboy_req:reply(501, #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, Response, Req1),
    
    {ok, Req, State}.

%% Handle unsupported endpoints
handle_not_found(Req0, State) ->
    Path = cowboy_req:path(Req0),
    Method = cowboy_req:method(Req0),
    
    colored_logger:warning("[MCP_MGMT] ⚠️ Unsupported endpoint: ~s ~s", [Method, Path]),
    
    Response = jsx:encode(#{
        success => false,
        error => <<"Endpoint not supported">>,
        path => Path,
        method => Method
    }),
    
    Req = cowboy_req:reply(404, #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, Response, Req0),
    
    {ok, Req, State}.

%% Error handling
handle_error(Req0, State, ErrorMessage) ->
    Response = jsx:encode(#{
        success => false,
        error => ErrorMessage
    }),
    
    Req = cowboy_req:reply(500, #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, Response, Req0),
    
    {ok, Req, State}.

%% Helper to format server info for JSON
format_server_info(ServerInfo) when is_map(ServerInfo) ->
    ServerInfo;
format_server_info({Id, Config}) ->
    #{
        id => ensure_binary(Id),
        name => maps:get(name, Config, Id),
        status => maps:get(status, Config, <<"unknown">>),
        type => maps:get(type, Config, <<"local">>),
        config => Config
    };
format_server_info(Other) ->
    #{
        id => ensure_binary(Other),
        status => <<"unknown">>,
        type => <<"unknown">>
    }.

%% Ensure value is binary
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) -> iolist_to_binary(io_lib:format("~p", [Value])).