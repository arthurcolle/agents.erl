%%%-------------------------------------------------------------------
%%% @doc
%%% HTTP handler for MCP server operations
%%% Enhanced with comprehensive logging and error reporting
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_server_handler).

-export([init/2]).

%% Logging utility
-define(LOG(Level, Format, Args), 
    io:format("[~s:~p] [~s] " ++ Format ++ "~n", 
              [?MODULE, ?LINE, Level | Args])).

-define(LOG_INFO(Format, Args), ?LOG("INFO", Format, Args)).
-define(LOG_WARN(Format, Args), ?LOG("WARN", Format, Args)).
-define(LOG_ERROR(Format, Args), ?LOG("ERROR", Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG("DEBUG", Format, Args)).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    ?LOG_INFO("Handling request ~s ~s", [Method, Path]),
    
    Req = case {Method, Path} of
        {<<"GET">>, <<"/api/mcp-servers">>} ->
            handle_list_servers(Req0);
        {<<"POST">>, <<"/api/mcp-servers">>} ->
            handle_add_server(Req0);
        {<<"GET">>, <<"/api/mcp-servers/", ServerId/binary>>} ->
            handle_get_server(ServerId, Req0);
        {<<"PUT">>, <<"/api/mcp-servers/", ServerId/binary>>} ->
            handle_update_server(ServerId, Req0);
        {<<"DELETE">>, <<"/api/mcp-servers/", ServerId/binary>>} ->
            handle_delete_server(ServerId, Req0);
        {<<"POST">>, <<"/api/mcp-servers/", Rest/binary>>} ->
            case binary:split(Rest, <<"/">>) of
                [ServerId, <<"discover">>] ->
                    handle_discover_capabilities(ServerId, Req0);
                _ ->
                    error_response(404, <<"Not found">>, Req0)
            end;
        {<<"GET">>, <<"/api/agents/", Rest/binary>>} ->
            case binary:split(Rest, <<"/">>) of
                [AgentId, <<"mcp-servers">>] ->
                    handle_get_agent_servers(AgentId, Req0);
                _ ->
                    error_response(404, <<"Not found">>, Req0)
            end;
        {<<"PUT">>, <<"/api/agents/", Rest/binary>>} ->
            case binary:split(Rest, <<"/">>) of
                [AgentId, <<"mcp-servers">>] ->
                    handle_assign_servers_to_agent(AgentId, Req0);
                _ ->
                    error_response(404, <<"Not found">>, Req0)
            end;
        _ ->
            error_response(404, <<"Not found">>, Req0)
    end,
    {ok, Req, State}.

handle_list_servers(Req) ->
    ?LOG_INFO("Fetching all MCP servers", []),
    case mcp_server_config:get_all_servers() of
        {ok, Servers} ->
            ?LOG_INFO("Successfully retrieved ~p servers", [length(Servers)]),
            Json = jsx:encode(lists:map(fun server_to_json/1, Servers)),
            ?LOG_DEBUG("Server list JSON response length: ~p bytes", [byte_size(Json)]),
            cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Json, Req);
        {error, Reason} ->
            ?LOG_ERROR("Failed to retrieve servers: ~p", [Reason]),
            error_response(500, Reason, Req)
    end.

handle_add_server(Req0) ->
    ?LOG_INFO("Adding new MCP server", []),
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req} ->
            ?LOG_DEBUG("Received request body: ~s", [Body]),
            try
                ServerMap = jsx:decode(Body, [return_maps]),
                ?LOG_INFO("Parsed server config: ~p", [maps:without([<<"description">>], ServerMap)]), % Exclude description to reduce log noise
                case mcp_server_config:add_server(ServerMap) of
                    {ok, Server} ->
                        ServerId = element(2, Server), % Extract server ID from record
                        ?LOG_INFO("Successfully added server: ~s", [ServerId]),
                        Json = jsx:encode(server_to_json(Server)),
                        ?LOG_DEBUG("Add server response JSON: ~s", [Json]),
                        cowboy_req:reply(201, #{
                            <<"content-type">> => <<"application/json">>
                        }, Json, Req);
                    {error, Reason} ->
                        ?LOG_ERROR("Failed to add server: ~p", [Reason]),
                        error_response(400, Reason, Req)
                end
            catch
                Class:Error:Stack ->
                    ?LOG_ERROR("JSON parsing failed: ~p:~p~nStack: ~p~nBody: ~s", [Class, Error, Stack, Body]),
                    error_response(400, <<"Invalid JSON">>, Req)
            end;
        {error, ReadError} ->
            ?LOG_ERROR("Failed to read request body: ~p", [ReadError]),
            error_response(400, <<"Could not read body">>, Req0);
        _ ->
            ?LOG_ERROR("Unexpected body read result", []),
            error_response(400, <<"Could not read body">>, Req0)
    end.

handle_get_server(ServerId, Req) ->
    ?LOG_INFO("Getting server details for: ~s", [ServerId]),
    case mcp_server_config:get_server(ServerId) of
        {ok, Server} ->
            ?LOG_DEBUG("Successfully retrieved server: ~s", [ServerId]),
            Json = jsx:encode(server_to_json(Server)),
            cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Json, Req);
        {error, not_found} ->
            ?LOG_WARN("Server not found: ~s", [ServerId]),
            error_response(404, <<"Server not found">>, Req);
        {error, Reason} ->
            ?LOG_ERROR("Failed to get server ~s: ~p", [ServerId, Reason]),
            error_response(500, Reason, Req)
    end.

handle_update_server(ServerId, Req0) ->
    ?LOG_INFO("Updating server: ~s", [ServerId]),
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req} ->
            ?LOG_DEBUG("Update request body: ~s", [Body]),
            try
                Updates = jsx:decode(Body, [return_maps]),
                ?LOG_INFO("Update fields for server ~s: ~p", [ServerId, maps:keys(Updates)]),
                case mcp_server_config:update_server(ServerId, Updates) of
                    {ok, Server} ->
                        ?LOG_INFO("Successfully updated server: ~s", [ServerId]),
                        Json = jsx:encode(server_to_json(Server)),
                        cowboy_req:reply(200, #{
                            <<"content-type">> => <<"application/json">>
                        }, Json, Req);
                    {error, not_found} ->
                        ?LOG_WARN("Cannot update - server not found: ~s", [ServerId]),
                        error_response(404, <<"Server not found">>, Req);
                    {error, Reason} ->
                        ?LOG_ERROR("Failed to update server ~s: ~p", [ServerId, Reason]),
                        error_response(400, Reason, Req)
                end
            catch
                Class:Error:Stack ->
                    ?LOG_ERROR("JSON parsing failed for update ~s: ~p:~p~nBody: ~s", [ServerId, Class, Error, Body]),
                    error_response(400, <<"Invalid JSON">>, Req)
            end;
        {error, ReadError} ->
            ?LOG_ERROR("Failed to read update body for ~s: ~p", [ServerId, ReadError]),
            error_response(400, <<"Could not read body">>, Req0);
        _ ->
            ?LOG_ERROR("Unexpected body read result for update ~s", [ServerId]),
            error_response(400, <<"Could not read body">>, Req0)
    end.

handle_delete_server(ServerId, Req) ->
    ?LOG_INFO("Deleting server: ~s", [ServerId]),
    case mcp_server_config:delete_server(ServerId) of
        ok ->
            ?LOG_INFO("Successfully deleted server: ~s", [ServerId]),
            cowboy_req:reply(204, #{}, <<>>, Req);
        {error, not_found} ->
            ?LOG_WARN("Cannot delete - server not found: ~s", [ServerId]),
            error_response(404, <<"Server not found">>, Req);
        {error, Reason} ->
            ?LOG_ERROR("Failed to delete server ~s: ~p", [ServerId, Reason]),
            error_response(500, Reason, Req)
    end.

handle_discover_capabilities(ServerId, Req) ->
    ?LOG_INFO("Discovering capabilities for server: ~s", [ServerId]),
    case mcp_server_config:discover_capabilities(ServerId) of
        {ok, Server} ->
            Capabilities = element(9, Server), % Extract capabilities from record
            ?LOG_INFO("Successfully discovered ~p capabilities for server: ~s", [length(Capabilities), ServerId]),
            ?LOG_DEBUG("Discovered capabilities: ~p", [Capabilities]),
            Json = jsx:encode(server_to_json(Server)),
            cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Json, Req);
        {error, not_found} ->
            ?LOG_WARN("Cannot discover capabilities - server not found: ~s", [ServerId]),
            error_response(404, <<"Server not found">>, Req);
        {error, Reason} ->
            ?LOG_ERROR("Failed to discover capabilities for server ~s: ~p", [ServerId, Reason]),
            error_response(500, Reason, Req)
    end.

handle_get_agent_servers(AgentId, Req) ->
    ?LOG_INFO("Getting servers for agent: ~s", [AgentId]),
    case mcp_server_config:get_agent_servers(AgentId) of
        {ok, Servers} ->
            ?LOG_INFO("Found ~p servers for agent ~s", [length(Servers), AgentId]),
            Json = jsx:encode(lists:map(fun server_to_json/1, Servers)),
            cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Json, Req);
        {error, Reason} ->
            ?LOG_ERROR("Failed to get servers for agent ~s: ~p", [AgentId, Reason]),
            error_response(500, Reason, Req)
    end.

handle_assign_servers_to_agent(AgentId, Req0) ->
    ?LOG_INFO("Assigning servers to agent: ~s", [AgentId]),
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req} ->
            ?LOG_DEBUG("Assignment request body: ~s", [Body]),
            try
                #{<<"server_ids">> := ServerIds} = jsx:decode(Body, [return_maps]),
                ?LOG_INFO("Assigning ~p servers to agent ~s: ~p", [length(ServerIds), AgentId, ServerIds]),
                case mcp_server_config:assign_server_to_agent(AgentId, ServerIds) of
                    ok ->
                        ?LOG_INFO("Successfully assigned servers to agent: ~s", [AgentId]),
                        cowboy_req:reply(200, #{
                            <<"content-type">> => <<"application/json">>
                        }, <<"{}">>, Req);
                    {error, Reason} ->
                        ?LOG_ERROR("Failed to assign servers to agent ~s: ~p", [AgentId, Reason]),
                        error_response(400, Reason, Req)
                end
            catch
                Class:Error:Stack ->
                    ?LOG_ERROR("JSON parsing failed for agent assignment ~s: ~p:~p~nBody: ~s", [AgentId, Class, Error, Body]),
                    error_response(400, <<"Invalid JSON">>, Req)
            end;
        {error, ReadError} ->
            ?LOG_ERROR("Failed to read assignment body for agent ~s: ~p", [AgentId, ReadError]),
            error_response(400, <<"Could not read body">>, Req0);
        _ ->
            ?LOG_ERROR("Unexpected body read result for agent assignment ~s", [AgentId]),
            error_response(400, <<"Could not read body">>, Req0)
    end.

server_to_json(Server) ->
    #{
        id => element(2, Server),
        name => element(3, Server),
        category => element(4, Server),
        url => element(5, Server),
        auth_type => element(6, Server),
        maintainer => element(7, Server),
        description => element(8, Server),
        capabilities => element(9, Server),
        status => element(10, Server),
        last_checked => format_datetime(element(11, Server))
    }.

format_datetime({{Y, M, D}, {H, Min, S}}) ->
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ", 
                  [Y, M, D, H, Min, S]).

error_response(Code, Message, Req) ->
    ?LOG_WARN("Sending error response ~p: ~p", [Code, Message]),
    Json = jsx:encode(#{<<"error">> => Message}),
    cowboy_req:reply(Code, #{
        <<"content-type">> => <<"application/json">>
    }, Json, Req).