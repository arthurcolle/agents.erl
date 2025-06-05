%% dynamic_agent_router.erl
%% Dynamic routing system for cross-agent communication and resource discovery
-module(dynamic_agent_router).
-behaviour(gen_server).

-export([
    start_link/0,
    register_agent/3,
    unregister_agent/1,
    route_request/3,
    discover_resource/2,
    find_agent_by_capability/1,
    get_routing_table/0,
    broadcast_discovery/1,
    register_resource/4,
    query_resources/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(agent_info, {
    id :: binary(),
    pid :: pid(),
    capabilities :: [binary()],
    resources :: map(), % resource_type => [resource_info]
    load :: integer(),
    last_seen :: integer(),
    metadata :: map()
}).

-record(resource_info, {
    id :: binary(),
    type :: atom(), % tool | memory | file | conversation | message
    owner_agent :: binary(),
    metadata :: map(),
    access_level :: atom(), % public | private | shared
    created_at :: integer(),
    last_accessed :: integer()
}).

-record(state, {
    agents = #{} :: #{binary() => #agent_info{}},
    resources = #{} :: #{binary() => #resource_info{}},
    routing_table = #{} :: map(),
    discovery_cache = #{} :: map(),
    cache_ttl = 300000 :: integer() % 5 minutes
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Register an agent with its capabilities and resources
register_agent(AgentId, AgentPid, Capabilities) when is_binary(AgentId), is_pid(AgentPid), is_list(Capabilities) ->
    gen_server:call(?SERVER, {register_agent, AgentId, AgentPid, Capabilities}).

%% Unregister an agent
unregister_agent(AgentId) when is_binary(AgentId) ->
    gen_server:call(?SERVER, {unregister_agent, AgentId}).

%% Route a request to the most appropriate agent
route_request(RequestType, Request, RequesterAgentId) ->
    gen_server:call(?SERVER, {route_request, RequestType, Request, RequesterAgentId}, 30000).

%% Discover resources of a specific type across the fleet
discover_resource(ResourceType, Query) ->
    gen_server:call(?SERVER, {discover_resource, ResourceType, Query}).

%% Find agents by capability
find_agent_by_capability(Capability) when is_binary(Capability) ->
    gen_server:call(?SERVER, {find_agent_by_capability, Capability}).

%% Get current routing table
get_routing_table() ->
    gen_server:call(?SERVER, get_routing_table).

%% Broadcast discovery request to all agents
broadcast_discovery(Query) ->
    gen_server:call(?SERVER, {broadcast_discovery, Query}).

%% Register a resource owned by an agent
register_resource(ResourceId, ResourceType, OwnerAgent, Metadata) ->
    gen_server:call(?SERVER, {register_resource, ResourceId, ResourceType, OwnerAgent, Metadata}).

%% Query resources with filters
query_resources(Filters, Options) ->
    gen_server:call(?SERVER, {query_resources, Filters, Options}).

%% gen_server callbacks
init([]) ->
    % Schedule periodic cleanup and discovery
    timer:send_interval(60000, cleanup_stale_agents),
    timer:send_interval(120000, refresh_discovery_cache),
    
    {ok, #state{}}.

handle_call({register_agent, AgentId, AgentPid, Capabilities}, _From, State) ->
    % Monitor the agent process
    monitor(process, AgentPid),
    
    % Create agent info record
    AgentInfo = #agent_info{
        id = AgentId,
        pid = AgentPid,
        capabilities = ensure_binary_list(Capabilities),
        resources = #{},
        load = 0,
        last_seen = erlang:system_time(millisecond),
        metadata = #{}
    },
    
    % Update agents map
    NewAgents = maps:put(AgentId, AgentInfo, State#state.agents),
    
    % Update routing table
    NewRoutingTable = update_routing_table(AgentInfo, State#state.routing_table),
    
    % Broadcast agent registration to fleet
    spawn(fun() -> broadcast_agent_registration(AgentInfo, NewAgents) end),
    
    {reply, ok, State#state{agents = NewAgents, routing_table = NewRoutingTable}};

handle_call({unregister_agent, AgentId}, _From, State) ->
    case maps:get(AgentId, State#state.agents, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        AgentInfo ->
            % Remove agent from all structures
            NewAgents = maps:remove(AgentId, State#state.agents),
            NewRoutingTable = remove_from_routing_table(AgentInfo, State#state.routing_table),
            
            % Remove agent's resources
            NewResources = maps:filter(fun(_, #resource_info{owner_agent = Owner}) ->
                Owner =/= AgentId
            end, State#state.resources),
            
            {reply, ok, State#state{
                agents = NewAgents,
                routing_table = NewRoutingTable,
                resources = NewResources
            }}
    end;

handle_call({route_request, RequestType, Request, RequesterAgentId}, _From, State) ->
    Result = route_request_internal(RequestType, Request, RequesterAgentId, State),
    {reply, Result, State};

handle_call({discover_resource, ResourceType, Query}, _From, State) ->
    Result = discover_resource_internal(ResourceType, Query, State),
    {reply, Result, State};

handle_call({find_agent_by_capability, Capability}, _From, State) ->
    MatchingAgents = maps:fold(fun(AgentId, #agent_info{capabilities = Caps}, Acc) ->
        case lists:member(Capability, Caps) of
            true -> [AgentId | Acc];
            false -> Acc
        end
    end, [], State#state.agents),
    {reply, {ok, MatchingAgents}, State};

handle_call(get_routing_table, _From, State) ->
    {reply, State#state.routing_table, State};

handle_call({broadcast_discovery, Query}, _From, State) ->
    Results = broadcast_discovery_internal(Query, State),
    {reply, Results, State};

handle_call({register_resource, ResourceId, ResourceType, OwnerAgent, Metadata}, _From, State) ->
    ResourceInfo = #resource_info{
        id = ResourceId,
        type = ResourceType,
        owner_agent = OwnerAgent,
        metadata = Metadata,
        access_level = maps:get(access_level, Metadata, public),
        created_at = erlang:system_time(millisecond),
        last_accessed = erlang:system_time(millisecond)
    },
    
    NewResources = maps:put(ResourceId, ResourceInfo, State#state.resources),
    {reply, ok, State#state{resources = NewResources}};

handle_call({query_resources, Filters, Options}, _From, State) ->
    Results = query_resources_internal(Filters, Options, State),
    {reply, Results, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % Handle agent process termination
    DeadAgentId = find_agent_by_pid(Pid, State#state.agents),
    case DeadAgentId of
        undefined -> {noreply, State};
        AgentId -> 
            {reply, _, NewState} = handle_call({unregister_agent, AgentId}, undefined, State),
            {noreply, NewState}
    end;

handle_info(cleanup_stale_agents, State) ->
    NewState = cleanup_stale_agents_internal(State),
    {noreply, NewState};

handle_info(refresh_discovery_cache, State) ->
    NewState = refresh_discovery_cache_internal(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

route_request_internal(RequestType, Request, RequesterAgentId, State) ->
    case RequestType of
        tool_execution ->
            route_tool_request(Request, RequesterAgentId, State);
        memory_retrieval ->
            route_memory_request(Request, RequesterAgentId, State);
        file_access ->
            route_file_request(Request, RequesterAgentId, State);
        conversation_query ->
            route_conversation_request(Request, RequesterAgentId, State);
        message_delivery ->
            route_message_request(Request, RequesterAgentId, State);
        _ ->
            route_generic_request(RequestType, Request, RequesterAgentId, State)
    end.

route_tool_request(#{tool_name := ToolName} = Request, RequesterAgentId, State) ->
    % Find agents with the specific tool capability
    ToolCapability = <<"tool:", ToolName/binary>>,
    case find_agents_with_capability(ToolCapability, State) of
        [] ->
            % Fallback to agents with general tool execution capability
            case find_agents_with_capability(<<"tool_execution">>, State) of
                [] -> {error, no_capable_agents};
                Agents -> route_to_best_agent(Agents, Request, RequesterAgentId, State)
            end;
        Agents ->
            route_to_best_agent(Agents, Request, RequesterAgentId, State)
    end.

route_memory_request(#{memory_type := MemoryType} = Request, RequesterAgentId, State) ->
    % Find agents with memory capabilities
    MemoryCapability = <<"memory:", MemoryType/binary>>,
    case find_agents_with_capability(MemoryCapability, State) of
        [] ->
            case find_agents_with_capability(<<"memory_management">>, State) of
                [] -> {error, no_memory_agents};
                Agents -> route_to_best_agent(Agents, Request, RequesterAgentId, State)
            end;
        Agents ->
            route_to_best_agent(Agents, Request, RequesterAgentId, State)
    end.

route_file_request(#{file_path := FilePath} = Request, RequesterAgentId, State) ->
    % Find agents with file system access
    case find_agents_with_capability(<<"file_system">>, State) of
        [] -> {error, no_file_agents};
        Agents ->
            % Prefer agents that have accessed similar file paths
            BestAgent = select_file_agent(Agents, FilePath, State),
            route_to_agent(BestAgent, Request, RequesterAgentId, State)
    end.

route_conversation_request(#{conversation_id := ConvId} = Request, RequesterAgentId, State) ->
    % Find agents with conversation management capability
    case find_agents_with_capability(<<"conversation_management">>, State) of
        [] -> {error, no_conversation_agents};
        Agents ->
            % Prefer agents that have handled this conversation before
            BestAgent = select_conversation_agent(Agents, ConvId, State),
            route_to_agent(BestAgent, Request, RequesterAgentId, State)
    end.

route_message_request(#{target_agent := TargetAgent, message := Message} = Request, RequesterAgentId, State) ->
    case maps:get(TargetAgent, State#state.agents, undefined) of
        undefined -> {error, target_agent_not_found};
        #agent_info{pid = TargetPid} ->
            % Direct message delivery
            try
                agent_instance:self_message(TargetPid, Message),
                {ok, #{delivered_to => TargetAgent, from => RequesterAgentId}}
            catch
                E:R -> {error, {delivery_failed, E, R}}
            end
    end.

route_generic_request(RequestType, Request, RequesterAgentId, State) ->
    % Generic routing based on agent load and capabilities
    AvailableAgents = get_available_agents(State),
    case AvailableAgents of
        [] -> {error, no_available_agents};
        Agents ->
            BestAgent = select_least_loaded_agent(Agents),
            route_to_agent(BestAgent, #{
                type => RequestType,
                request => Request,
                from => RequesterAgentId
            }, RequesterAgentId, State)
    end.

discover_resource_internal(ResourceType, Query, State) ->
    % Search resources by type and query criteria
    MatchingResources = maps:fold(fun(ResourceId, #resource_info{type = Type} = Resource, Acc) ->
        case Type =:= ResourceType andalso matches_query(Resource, Query) of
            true -> [format_resource_result(ResourceId, Resource) | Acc];
            false -> Acc
        end
    end, [], State#state.resources),
    
    % Also query agents for dynamic discovery
    AgentDiscovery = discover_from_agents(ResourceType, Query, State),
    
    AllResults = MatchingResources ++ AgentDiscovery,
    {ok, AllResults}.

find_agents_with_capability(Capability, State) ->
    maps:fold(fun(AgentId, #agent_info{capabilities = Caps}, Acc) ->
        case lists:member(Capability, Caps) of
            true -> [AgentId | Acc];
            false -> Acc
        end
    end, [], State#state.agents).

route_to_best_agent(Agents, Request, RequesterAgentId, State) ->
    case select_best_agent(Agents, Request, State) of
        undefined -> {error, no_suitable_agent};
        BestAgent -> route_to_agent(BestAgent, Request, RequesterAgentId, State)
    end.

route_to_agent(AgentId, Request, RequesterAgentId, State) ->
    case maps:get(AgentId, State#state.agents, undefined) of
        undefined -> {error, agent_not_found};
        #agent_info{pid = AgentPid} ->
            try
                % Route request to agent via self-message
                RouteMessage = jsx:encode(#{
                    type => <<"routed_request">>,
                    request => Request,
                    from => RequesterAgentId,
                    routed_at => erlang:system_time(millisecond)
                }),
                agent_instance:self_message(AgentPid, RouteMessage),
                {ok, #{routed_to => AgentId, from => RequesterAgentId}}
            catch
                E:R -> {error, {routing_failed, E, R}}
            end
    end.

select_best_agent(Agents, _Request, State) ->
    % Simple load-based selection for now
    AgentLoads = lists:map(fun(AgentId) ->
        case maps:get(AgentId, State#state.agents, undefined) of
            undefined -> {AgentId, infinity};
            #agent_info{load = Load} -> {AgentId, Load}
        end
    end, Agents),
    
    case lists:keysort(2, AgentLoads) of
        [{BestAgent, _} | _] -> BestAgent;
        [] -> undefined
    end.

select_least_loaded_agent(Agents) ->
    case Agents of
        [Agent | _] -> Agent; % Simple selection for now
        [] -> undefined
    end.

select_file_agent(Agents, _FilePath, _State) ->
    % For now, select first available agent
    % TODO: Implement path affinity logic
    case Agents of
        [Agent | _] -> Agent;
        [] -> undefined
    end.

select_conversation_agent(Agents, _ConvId, _State) ->
    % For now, select first available agent
    % TODO: Implement conversation history affinity
    case Agents of
        [Agent | _] -> Agent;
        [] -> undefined
    end.

get_available_agents(State) ->
    maps:fold(fun(AgentId, #agent_info{}, Acc) ->
        [AgentId | Acc]
    end, [], State#state.agents).

update_routing_table(#agent_info{id = AgentId, capabilities = Capabilities}, RoutingTable) ->
    % Update routing table with agent capabilities
    lists:foldl(fun(Capability, Table) ->
        ExistingAgents = maps:get(Capability, Table, []),
        maps:put(Capability, [AgentId | lists:delete(AgentId, ExistingAgents)], Table)
    end, RoutingTable, Capabilities).

remove_from_routing_table(#agent_info{id = AgentId}, RoutingTable) ->
    % Remove agent from all capability entries
    maps:map(fun(_, AgentList) ->
        lists:delete(AgentId, AgentList)
    end, RoutingTable).

broadcast_agent_registration(#agent_info{id = AgentId, capabilities = Capabilities}, Agents) ->
    % Notify other agents about new agent registration
    RegistrationMessage = jsx:encode(#{
        type => <<"agent_registered">>,
        agent_id => AgentId,
        capabilities => Capabilities,
        timestamp => erlang:system_time(millisecond)
    }),
    
    maps:fold(fun(OtherAgentId, #agent_info{pid = Pid}, _) ->
        case OtherAgentId =/= AgentId of
            true ->
                try agent_instance:self_message(Pid, RegistrationMessage)
                catch _:_ -> ok
                end;
            false -> ok
        end
    end, ok, Agents).

broadcast_discovery_internal(Query, State) ->
    % Broadcast discovery request to all agents
    DiscoveryMessage = jsx:encode(#{
        type => <<"discovery_request">>,
        query => Query,
        timestamp => erlang:system_time(millisecond)
    }),
    
    Results = maps:fold(fun(AgentId, #agent_info{pid = Pid}, Acc) ->
        try
            agent_instance:self_message(Pid, DiscoveryMessage),
            [#{agent_id => AgentId, status => <<"sent">>} | Acc]
        catch
            _:_ ->
                [#{agent_id => AgentId, status => <<"failed">>} | Acc]
        end
    end, [], State#state.agents),
    
    {ok, Results}.

discover_from_agents(ResourceType, Query, State) ->
    % Query agents for resources they might have
    % This would typically involve async communication
    % For now, return empty list as placeholder
    [].

query_resources_internal(Filters, Options, State) ->
    % Apply filters to resources
    FilteredResources = maps:fold(fun(ResourceId, Resource, Acc) ->
        case apply_filters(Resource, Filters) of
            true -> [format_resource_result(ResourceId, Resource) | Acc];
            false -> Acc
        end
    end, [], State#state.resources),
    
    % Apply options (sorting, limiting, etc.)
    FinalResults = apply_query_options(FilteredResources, Options),
    {ok, FinalResults}.

apply_filters(#resource_info{type = Type, owner_agent = Owner, access_level = Access}, Filters) ->
    TypeMatch = case maps:get(type, Filters, undefined) of
        undefined -> true;
        FilterType -> Type =:= FilterType
    end,
    
    OwnerMatch = case maps:get(owner, Filters, undefined) of
        undefined -> true;
        FilterOwner -> Owner =:= FilterOwner
    end,
    
    AccessMatch = case maps:get(access_level, Filters, undefined) of
        undefined -> true;
        FilterAccess -> Access =:= FilterAccess
    end,
    
    TypeMatch andalso OwnerMatch andalso AccessMatch.

apply_query_options(Results, Options) ->
    % Apply sorting
    SortedResults = case maps:get(sort_by, Options, undefined) of
        undefined -> Results;
        created_at -> lists:sort(fun(A, B) -> 
            maps:get(created_at, A) =< maps:get(created_at, B) 
        end, Results);
        _ -> Results
    end,
    
    % Apply limit
    case maps:get(limit, Options, undefined) of
        undefined -> SortedResults;
        Limit -> lists:sublist(SortedResults, Limit)
    end.

format_resource_result(ResourceId, #resource_info{type = Type, owner_agent = Owner, metadata = Metadata, access_level = Access}) ->
    #{
        id => ResourceId,
        type => Type,
        owner => Owner,
        access_level => Access,
        metadata => Metadata
    }.

matches_query(#resource_info{metadata = Metadata}, Query) ->
    % Simple metadata matching
    QueryKeys = maps:keys(Query),
    lists:all(fun(Key) ->
        maps:get(Key, Metadata, undefined) =:= maps:get(Key, Query)
    end, QueryKeys).

find_agent_by_pid(Pid, Agents) ->
    case maps:fold(fun(AgentId, #agent_info{pid = AgentPid}, Acc) ->
        case AgentPid =:= Pid of
            true -> AgentId;
            false -> Acc
        end
    end, undefined, Agents) of
        undefined -> undefined;
        AgentId -> AgentId
    end.

cleanup_stale_agents_internal(State) ->
    % Remove agents that haven't been seen recently
    Now = erlang:system_time(millisecond),
    StaleThreshold = 5 * 60 * 1000, % 5 minutes
    
    {StaleAgents, ActiveAgents} = maps:fold(fun(AgentId, #agent_info{last_seen = LastSeen} = Agent, {Stale, Active}) ->
        case Now - LastSeen > StaleThreshold of
            true -> {[AgentId | Stale], Active};
            false -> {Stale, maps:put(AgentId, Agent, Active)}
        end
    end, {[], #{}}, State#state.agents),
    
    % Clean up routing table for stale agents
    NewRoutingTable = lists:foldl(fun(StaleAgent, Table) ->
        case maps:get(StaleAgent, State#state.agents, undefined) of
            undefined -> Table;
            AgentInfo -> remove_from_routing_table(AgentInfo, Table)
        end
    end, State#state.routing_table, StaleAgents),
    
    State#state{agents = ActiveAgents, routing_table = NewRoutingTable}.

refresh_discovery_cache_internal(State) ->
    % Refresh discovery cache by querying active agents
    % This is a placeholder for more sophisticated caching
    State#state{discovery_cache = #{}}.

ensure_binary_list(List) ->
    lists:map(fun ensure_binary/1, List).

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) -> iolist_to_binary(io_lib:format("~p", [Value])).