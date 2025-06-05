-module(mcp_github_registry_handler).

-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([handle_get/2, handle_post/2]).

%% Enhanced colorful logging
-define(LOG(Level, Format, Args), 
    colored_logger:log(Level, "MCP_GH_API", "[~s:~p] " ++ Format, [?MODULE, ?LINE | Args])).

-define(LOG_INFO(Format, Args), ?LOG(info, Format, Args)).
-define(LOG_WARN(Format, Args), ?LOG(warning, Format, Args)).
-define(LOG_ERROR(Format, Args), ?LOG(error, Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG(debug, Format, Args)).
-define(LOG_SUCCESS(Format, Args), ?LOG(success, Format, Args)).
-define(LOG_NETWORK(Format, Args), colored_logger:network(connected, io_lib:format("[~s:~p] " ++ Format, [?MODULE, ?LINE | Args]))).

init(Req, State) ->
    ?LOG_NETWORK("🌐 MCP GitHub registry API request: ~s", [cowboy_req:path(Req)]),
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_get}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, handle_post}
    ], Req, State}.

handle_get(Req, State) ->
    Path = cowboy_req:path(Req),
    ?LOG_INFO("Processing GET request: ~s", [Path]),
    
    Response = case cowboy_req:path_info(Req) of
        [] ->
            % GET /api/mcp-registry - List all servers
            handle_list_servers();
        [<<"stats">>] ->
            % GET /api/mcp-registry/stats - Get registry statistics
            handle_get_stats();
        [<<"categories">>] ->
            % GET /api/mcp-registry/categories - Get servers grouped by category
            handle_get_categories();
        [<<"search">>] ->
            % GET /api/mcp-registry/search?q=term - Search servers
            Query = cowboy_req:qs_val(<<"q">>, Req, <<"">>),
            handle_search_servers(Query);
        [Category] ->
            % GET /api/mcp-registry/{category} - Get servers by category
            handle_get_by_category(Category);
        _ ->
            ?LOG_WARN("Unknown endpoint requested: ~s", [Path]),
            {error, not_found}
    end,
    
    case Response of
        {ok, Data} ->
            ResponseBody = jsx:encode(Data),
            ?LOG_SUCCESS("Successfully processed GET request", []),
            {ResponseBody, Req, State};
        {error, Reason} ->
            ?LOG_ERROR("Error processing GET request: ~p", [Reason]),
            handle_error_response(Reason, Req, State, 400)
    end.

handle_post(Req, State) ->
    ?LOG_INFO("Processing POST request for registry operations", []),
    
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    case jsx:decode(Body, [return_maps]) of
        #{<<"action">> := <<"force_update">>} ->
            ?LOG_INFO("Forcing registry update", []),
            case mcp_github_registry_updater:force_update() of
                ok ->
                    Response = jsx:encode(#{
                        success => true,
                        message => <<"Registry update initiated">>
                    }),
                    ?LOG_SUCCESS("Registry update initiated successfully", []),
                    {true, cowboy_req:set_resp_body(Response, Req1), State};
                {error, Reason} ->
                    ?LOG_ERROR("Failed to initiate registry update: ~p", [Reason]),
                    handle_error_response(update_failed, Req1, State, 500)
            end;
        #{<<"action">> := Action} ->
            ?LOG_WARN("Unknown action requested: ~s", [Action]),
            handle_error_response(unknown_action, Req1, State, 400);
        _ ->
            ?LOG_WARN("Invalid request body format", []),
            handle_error_response(invalid_format, Req1, State, 400)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

handle_error_response(Reason, Req, State, StatusCode) ->
    ErrorResponse = jsx:encode(#{
        error => atom_to_binary(Reason, utf8),
        message => case Reason of
            not_found -> <<"Resource not found">>;
            update_failed -> <<"Failed to initiate registry update">>;
            unknown_action -> <<"Unknown action requested">>;
            invalid_format -> <<"Invalid request body format">>;
            _ -> <<"Request failed">>
        end
    }),
    Req2 = cowboy_req:reply(StatusCode, #{<<"content-type">> => <<"application/json">>}, 
                           ErrorResponse, Req),
    {stop, Req2, State}.

handle_list_servers() ->
    ?LOG_DEBUG("Fetching all MCP servers from registry", []),
    
    case mcp_github_registry_updater:get_registry_stats() of
        #{registry_data := RegistryData, last_update := LastUpdate} when is_map(RegistryData) ->
            Servers = maps:values(RegistryData),
            ?LOG_SUCCESS("Retrieved ~p servers from registry", [length(Servers)]),
            {ok, #{
                servers => Servers,
                total_count => length(Servers),
                last_update => LastUpdate,
                generated_at => erlang:system_time(second)
            }};
        Stats when is_map(Stats) ->
            % Fallback - return stats if registry data not available
            ?LOG_WARN("Registry data not available, returning stats only", []),
            {ok, #{
                servers => [],
                total_count => 0,
                stats => Stats,
                generated_at => erlang:system_time(second)
            }};
        _ ->
            ?LOG_ERROR("Failed to get registry data", []),
            {error, registry_unavailable}
    end.

handle_get_stats() ->
    ?LOG_DEBUG("Fetching registry statistics", []),
    
    case mcp_github_registry_updater:get_registry_stats() of
        Stats when is_map(Stats) ->
            ?LOG_SUCCESS("Retrieved registry statistics", []),
            {ok, maps:merge(Stats, #{
                generated_at => erlang:system_time(second)
            })};
        _ ->
            ?LOG_ERROR("Failed to get registry statistics", []),
            {error, stats_unavailable}
    end.

handle_get_categories() ->
    ?LOG_DEBUG("Fetching servers grouped by category", []),
    
    case mcp_github_registry_updater:get_registry_stats() of
        #{registry_data := RegistryData} when is_map(RegistryData) ->
            Categories = lists:foldl(fun({_Id, Server}, Acc) ->
                Category = maps:get(category, Server, unknown),
                CategoryBin = atom_to_binary(Category, utf8),
                Current = maps:get(CategoryBin, Acc, []),
                maps:put(CategoryBin, [Server | Current], Acc)
            end, #{}, maps:to_list(RegistryData)),
            
            ?LOG_SUCCESS("Grouped servers into ~p categories", [maps:size(Categories)]),
            {ok, #{
                categories => Categories,
                total_categories => maps:size(Categories),
                generated_at => erlang:system_time(second)
            }};
        _ ->
            ?LOG_ERROR("Failed to get registry data for categorization", []),
            {error, registry_unavailable}
    end.

handle_search_servers(Query) ->
    ?LOG_DEBUG("Searching servers with query: ~s", [Query]),
    
    case mcp_github_registry_updater:get_registry_stats() of
        #{registry_data := RegistryData} when is_map(RegistryData) ->
            QueryLower = string:to_lower(binary_to_list(Query)),
            
            MatchingServers = lists:filter(fun({_Id, Server}) ->
                Name = string:to_lower(maps:get(name, Server, "")),
                Description = string:to_lower(maps:get(description, Server, "")),
                
                string:str(Name, QueryLower) > 0 orelse
                string:str(Description, QueryLower) > 0
            end, maps:to_list(RegistryData)),
            
            Servers = [Server || {_Id, Server} <- MatchingServers],
            ?LOG_SUCCESS("Found ~p servers matching query: ~s", [length(Servers), Query]),
            
            {ok, #{
                servers => Servers,
                query => Query,
                total_matches => length(Servers),
                generated_at => erlang:system_time(second)
            }};
        _ ->
            ?LOG_ERROR("Failed to get registry data for search", []),
            {error, registry_unavailable}
    end.

handle_get_by_category(CategoryBin) ->
    ?LOG_DEBUG("Fetching servers for category: ~s", [CategoryBin]),
    
    case mcp_github_registry_updater:get_registry_stats() of
        #{registry_data := RegistryData} when is_map(RegistryData) ->
            try
                % Create atom if it doesn't exist
                Category = try
                    binary_to_existing_atom(CategoryBin, utf8)
                catch
                    error:badarg ->
                        binary_to_atom(CategoryBin, utf8)
                end,
                
                CategoryServers = lists:filtermap(fun({_Id, Server}) ->
                    case maps:get(category, Server, unknown) of
                        Category -> {true, Server};
                        _ -> false
                    end
                end, maps:to_list(RegistryData)),
                
                ?LOG_SUCCESS("Found ~p servers in category: ~s", [length(CategoryServers), CategoryBin]),
                
                {ok, #{
                    servers => CategoryServers,
                    category => CategoryBin,
                    total_count => length(CategoryServers),
                    generated_at => erlang:system_time(second)
                }}
            catch
                _:_ ->
                    ?LOG_ERROR("Invalid category requested: ~s", [CategoryBin]),
                    {error, invalid_category}
            end;
        _ ->
            ?LOG_ERROR("Failed to get registry data for category filter", []),
            {error, registry_unavailable}
    end.