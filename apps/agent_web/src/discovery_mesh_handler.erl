-module(discovery_mesh_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([content_types_provided/2, content_types_accepted/2]).
-export([handle_get/2, handle_post/2]).
-export([allowed_methods/2]).

-define(DISCOVERY_CACHE_TTL, 300). % 5 minutes

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_post}], Req, State}.

handle_get(Req, State) ->
    Path = cowboy_req:path_info(Req),
    Response = case Path of
        [<<"mesh">>, <<"topology">>] ->
            handle_get_mesh_topology(Req);
        [<<"history">>] ->
            handle_get_discovery_history(Req);
        [<<"mesh">>, <<"statistics">>] ->
            handle_get_mesh_statistics(Req);
        [<<"resources">>, <<"fleet">>] ->
            handle_get_fleet_resources(Req);
        _ ->
            jsx:encode(#{error => <<"Invalid endpoint">>})
    end,
    {Response, Req, State}.

handle_post(Req, State) ->
    Path = cowboy_req:path_info(Req),
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    
    Response = try
        RequestData = jsx:decode(Body, [return_maps]),
        case Path of
            [<<"search">>] ->
                handle_discovery_search(RequestData, Req2);
            [<<"mesh">>, <<"optimize">>] ->
                handle_mesh_optimization(RequestData, Req2);
            [<<"cache">>, <<"clear">>] ->
                handle_cache_clear(RequestData, Req2);
            [<<"config">>, <<"update">>] ->
                handle_config_update(RequestData, Req2);
            _ ->
                jsx:encode(#{error => <<"Invalid endpoint">>})
        end
    catch
        Error:Reason:Stack ->
            logger:error("Discovery mesh handler error: ~p:~p~n~p", [Error, Reason, Stack]),
            jsx:encode(#{error => <<"Internal server error">>, details => list_to_binary(io_lib:format("~p", [Reason]))})
    end,
    
    {Response, Req2, State}.

%% Get mesh topology with nodes, connections and statistics
handle_get_mesh_topology(Req) ->
    try
        {ok, MeshStats} = dynamic_discovery_engine:get_mesh_statistics(),
        {ok, FleetInfo} = dynamic_discovery_engine:get_fleet_info(),
        
        Nodes = lists:map(fun(Agent) ->
            AgentId = maps:get(id, Agent),
            {ok, Capabilities} = dynamic_agent_router:get_agent_capabilities(AgentId),
            {ok, ResourceCount} = model_construct_registry:get_agent_resource_count(AgentId),
            
            #{
                id => AgentId,
                type => maps:get(type, Agent, <<"agent">>),
                capabilities => Capabilities,
                resource_count => ResourceCount,
                connection_count => maps:get(connection_count, Agent, 0),
                status => maps:get(status, Agent, <<"active">>),
                last_activity => maps:get(last_activity, Agent, erlang:system_time(second))
            }
        end, maps:get(agents, FleetInfo, [])),
        
        Connections = maps:get(connections, FleetInfo, []),
        
        Topology = #{
            nodes => Nodes,
            connections => Connections,
            statistics => MeshStats
        },
        
        jsx:encode(Topology)
    catch
        Error:Reason:Stack ->
            logger:error("Failed to get mesh topology: ~p:~p~n~p", [Error, Reason, Stack]),
            jsx:encode(#{
                error => <<"Failed to retrieve mesh topology">>,
                nodes => [],
                connections => [],
                statistics => #{
                    total_nodes => 0,
                    total_connections => 0,
                    average_path_length => 0.0,
                    cluster_coefficient => 0.0,
                    discovery_efficiency => 0.0
                }
            })
    end.

%% Get discovery history with pagination
handle_get_discovery_history(Req) ->
    QsVals = cowboy_req:parse_qs(Req),
    Limit = case proplists:get_value(<<"limit">>, QsVals) of
        undefined -> 20;
        LimitBin -> binary_to_integer(LimitBin)
    end,
    
    try
        History = get_discovery_history_from_cache(Limit),
        jsx:encode(History)
    catch
        Error:Reason:Stack ->
            logger:error("Failed to get discovery history: ~p:~p~n~p", [Error, Reason, Stack]),
            jsx:encode([])
    end.

%% Get mesh statistics
handle_get_mesh_statistics(Req) ->
    try
        {ok, Stats} = dynamic_discovery_engine:get_mesh_statistics(),
        jsx:encode(Stats)
    catch
        Error:Reason:Stack ->
            logger:error("Failed to get mesh statistics: ~p:~p~n~p", [Error, Reason, Stack]),
            jsx:encode(#{
                total_nodes => 0,
                total_connections => 0,
                average_path_length => 0.0,
                cluster_coefficient => 0.0,
                discovery_efficiency => 0.0
            })
    end.

%% Get fleet-wide resources
handle_get_fleet_resources(Req) ->
    try
        {ok, FleetResources} = dynamic_discovery_engine:discover_all_fleet_resources(#{
            include_types => [tool, memory, file, conversation, message],
            include_capabilities => true,
            include_topology => false
        }),
        jsx:encode(FleetResources)
    catch
        Error:Reason:Stack ->
            logger:error("Failed to get fleet resources: ~p:~p~n~p", [Error, Reason, Stack]),
            jsx:encode(#{
                resources_by_type => #{},
                agent_capabilities => #{},
                total_resources => 0
            })
    end.

%% Handle discovery search request
handle_discovery_search(RequestData, Req) ->
    Query = maps:get(<<"query">>, RequestData, <<"">>),
    ResourceType = maps:get(<<"resource_type">>, RequestData, <<"all">>),
    MaxResults = maps:get(<<"max_results">>, RequestData, 50),
    
    StartTime = erlang:system_time(millisecond),
    
    try
        DiscoveryResults = case ResourceType of
            <<"all">> ->
                perform_unified_discovery(Query, MaxResults);
            SpecificType ->
                perform_typed_discovery(Query, SpecificType, MaxResults)
        end,
        
        EndTime = erlang:system_time(millisecond),
        DiscoveryTimeMs = EndTime - StartTime,
        
        DiscoveryRecord = #{
            query => Query,
            resource_type => ResourceType,
            results => DiscoveryResults,
            total_results => length(DiscoveryResults),
            discovery_time_ms => DiscoveryTimeMs,
            timestamp => erlang:system_time(second)
        },
        
        %% Cache the discovery result
        cache_discovery_result(DiscoveryRecord),
        
        jsx:encode(DiscoveryRecord)
    catch
        Error:Reason:Stack ->
            logger:error("Discovery search failed: ~p:~p~n~p", [Error, Reason, Stack]),
            jsx:encode(#{
                error => <<"Discovery search failed">>,
                query => Query,
                resource_type => ResourceType,
                results => [],
                total_results => 0,
                discovery_time_ms => 0
            })
    end.

%% Handle mesh optimization request
handle_mesh_optimization(RequestData, Req) ->
    try
        %% Trigger mesh optimization
        ok = dynamic_discovery_engine:optimize_mesh_topology(),
        
        %% Get updated statistics
        {ok, UpdatedStats} = dynamic_discovery_engine:get_mesh_statistics(),
        
        jsx:encode(#{
            success => true,
            message => <<"Mesh optimization completed">>,
            updated_statistics => UpdatedStats
        })
    catch
        Error:Reason:Stack ->
            logger:error("Mesh optimization failed: ~p:~p~n~p", [Error, Reason, Stack]),
            jsx:encode(#{
                success => false,
                error => <<"Mesh optimization failed">>,
                details => list_to_binary(io_lib:format("~p", [Reason]))
            })
    end.

%% Handle cache clear request
handle_cache_clear(RequestData, Req) ->
    try
        %% Clear discovery cache
        ok = dynamic_discovery_engine:clear_discovery_cache(),
        
        %% Clear local history cache
        clear_discovery_history_cache(),
        
        jsx:encode(#{
            success => true,
            message => <<"Discovery cache cleared successfully">>
        })
    catch
        Error:Reason:Stack ->
            logger:error("Cache clear failed: ~p:~p~n~p", [Error, Reason, Stack]),
            jsx:encode(#{
                success => false,
                error => <<"Cache clear failed">>,
                details => list_to_binary(io_lib:format("~p", [Reason]))
            })
    end.

%% Handle configuration update
handle_config_update(RequestData, Req) ->
    try
        Config = #{
            discovery_timeout => maps:get(<<"discovery_timeout">>, RequestData, 30000),
            max_hops => maps:get(<<"max_hops">>, RequestData, 5),
            cache_ttl => maps:get(<<"cache_ttl">>, RequestData, 300),
            optimization_interval => maps:get(<<"optimization_interval">>, RequestData, 3600)
        },
        
        %% Update discovery engine configuration
        ok = dynamic_discovery_engine:update_configuration(Config),
        
        jsx:encode(#{
            success => true,
            message => <<"Configuration updated successfully">>,
            config => Config
        })
    catch
        Error:Reason:Stack ->
            logger:error("Config update failed: ~p:~p~n~p", [Error, Reason, Stack]),
            jsx:encode(#{
                success => false,
                error => <<"Configuration update failed">>,
                details => list_to_binary(io_lib:format("~p", [Reason]))
            })
    end.

%% Perform unified discovery across all resource types
perform_unified_discovery(Query, MaxResults) ->
    ResourceTypes = [tool, memory, file, conversation, message],
    MaxPerType = MaxResults div length(ResourceTypes),
    
    AllResults = lists:foldl(fun(Type, Acc) ->
        try
            TypeResults = perform_typed_discovery(Query, atom_to_binary(Type), MaxPerType),
            Acc ++ TypeResults
        catch
            _:_ -> Acc
        end
    end, [], ResourceTypes),
    
    %% Limit total results and sort by discovery time
    SortedResults = lists:sort(fun(A, B) ->
        maps:get(discovery_time_ms, A, 0) =< maps:get(discovery_time_ms, B, 0)
    end, AllResults),
    
    lists:sublist(SortedResults, MaxResults).

%% Perform discovery for specific resource type
perform_typed_discovery(Query, ResourceType, MaxResults) ->
    StartTime = erlang:system_time(millisecond),
    
    Results = case ResourceType of
        <<"tool">> ->
            discover_tools(Query, MaxResults);
        <<"memory">> ->
            discover_memories(Query, MaxResults);
        <<"file">> ->
            discover_files(Query, MaxResults);
        <<"conversation">> ->
            discover_conversations(Query, MaxResults);
        <<"message">> ->
            discover_messages(Query, MaxResults);
        _ ->
            []
    end,
    
    EndTime = erlang:system_time(millisecond),
    DiscoveryTime = EndTime - StartTime,
    
    %% Add discovery metadata to each result
    lists:map(fun(Result) ->
        Result#{
            discovery_time_ms => DiscoveryTime,
            discovery_path => maps:get(discovery_path, Result, [<<"direct">>])
        }
    end, Results).

%% Discover tools using agent retrieval system
discover_tools(Query, MaxResults) ->
    try
        {ok, ToolResults} = agent_retrieval_system:retrieve_tools(
            <<"discovery_mesh_handler">>,
            #{query => Query, limit => MaxResults}
        ),
        
        lists:map(fun(Tool) ->
            #{
                id => maps:get(id, Tool),
                name => maps:get(name, Tool),
                owner_agent => maps:get(owner_agent, Tool),
                resource_type => <<"tool">>,
                access_level => maps:get(access_level, Tool, <<"public">>),
                description => maps:get(description, Tool, <<"">>)
            }
        end, ToolResults)
    catch
        _:_ -> []
    end.

%% Discover memories using agent retrieval system
discover_memories(Query, MaxResults) ->
    try
        {ok, MemoryResults} = agent_retrieval_system:retrieve_memories(
            <<"discovery_mesh_handler">>,
            #{query => Query, limit => MaxResults}
        ),
        
        lists:map(fun(Memory) ->
            #{
                id => maps:get(id, Memory),
                name => maps:get(id, Memory), % Use ID as name for memories
                owner_agent => maps:get(owner_agent, Memory),
                resource_type => <<"memory">>,
                access_level => maps:get(access_level, Memory, <<"private">>),
                tags => maps:get(tags, Memory, [])
            }
        end, MemoryResults)
    catch
        _:_ -> []
    end.

%% Discover files using agent retrieval system
discover_files(Query, MaxResults) ->
    try
        {ok, FileResults} = agent_retrieval_system:retrieve_files(
            <<"discovery_mesh_handler">>,
            #{query => Query, limit => MaxResults}
        ),
        
        lists:map(fun(File) ->
            #{
                id => maps:get(id, File),
                name => maps:get(path, File, maps:get(id, File)),
                owner_agent => maps:get(owner_agent, File),
                resource_type => <<"file">>,
                access_level => maps:get(access_level, File, <<"private">>),
                content_type => maps:get(content_type, File, <<"application/octet-stream">>)
            }
        end, FileResults)
    catch
        _:_ -> []
    end.

%% Discover conversations using agent retrieval system
discover_conversations(Query, MaxResults) ->
    try
        {ok, ConversationResults} = agent_retrieval_system:retrieve_conversations(
            <<"discovery_mesh_handler">>,
            #{query => Query, limit => MaxResults}
        ),
        
        lists:map(fun(Conversation) ->
            #{
                id => maps:get(id, Conversation),
                name => maps:get(topic, Conversation, maps:get(id, Conversation)),
                owner_agent => maps:get(owner_agent, Conversation),
                resource_type => <<"conversation">>,
                access_level => maps:get(access_level, Conversation, <<"private">>),
                participants => maps:get(participants, Conversation, [])
            }
        end, ConversationResults)
    catch
        _:_ -> []
    end.

%% Discover messages using agent retrieval system
discover_messages(Query, MaxResults) ->
    try
        {ok, MessageResults} = agent_retrieval_system:retrieve_messages(
            <<"discovery_mesh_handler">>,
            #{query => Query, limit => MaxResults}
        ),
        
        lists:map(fun(Message) ->
            #{
                id => maps:get(id, Message),
                name => truncate_content(maps:get(content, Message, <<"">>), 50),
                owner_agent => maps:get(owner_agent, Message),
                resource_type => <<"message">>,
                access_level => maps:get(access_level, Message, <<"private">>),
                timestamp => maps:get(timestamp, Message, 0)
            }
        end, MessageResults)
    catch
        _:_ -> []
    end.

%% Helper function to truncate content for display
truncate_content(Content, MaxLen) when byte_size(Content) > MaxLen ->
    <<Truncated:MaxLen/binary, _/binary>> = Content,
    <<Truncated/binary, "...">>;
truncate_content(Content, _MaxLen) ->
    Content.

%% Cache discovery result for history
cache_discovery_result(DiscoveryRecord) ->
    try
        HistoryKey = {discovery_history, node()},
        
        %% Get existing history
        ExistingHistory = case persistent_term:get(HistoryKey, []) of
            History when is_list(History) -> History;
            _ -> []
        end,
        
        %% Add new record and keep only last 100 entries
        UpdatedHistory = [DiscoveryRecord | lists:sublist(ExistingHistory, 99)],
        
        %% Store updated history
        persistent_term:put(HistoryKey, UpdatedHistory),
        
        ok
    catch
        _:_ -> ok % Ignore cache errors
    end.

%% Get discovery history from cache
get_discovery_history_from_cache(Limit) ->
    try
        HistoryKey = {discovery_history, node()},
        History = persistent_term:get(HistoryKey, []),
        lists:sublist(History, Limit)
    catch
        _:_ -> []
    end.

%% Clear discovery history cache
clear_discovery_history_cache() ->
    try
        HistoryKey = {discovery_history, node()},
        persistent_term:put(HistoryKey, []),
        ok
    catch
        _:_ -> ok
    end.