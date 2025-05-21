%% agent_discovery.erl
%% Enhanced registry for agent discovery and capabilities
-module(agent_discovery).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    register_agent/3,
    unregister_agent/1,
    find_agent/1,
    find_by_capability/1,
    find_all/0,
    get_agent_info/1,
    update_agent_status/2,
    update_agent_capabilities/2,
    broadcast_heartbeat/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_HEARTBEAT_INTERVAL, 30000). % 30 seconds

-record(state, {
    agents = #{} :: map(),             % AgentId -> {Pid, Timestamp, Capabilities, Status}
    capabilities_index = #{} :: map(),  % Capability -> [AgentId]
    heartbeat_interval = ?DEFAULT_HEARTBEAT_INTERVAL :: non_neg_integer(),
    heartbeat_ref :: reference() | undefined
}).

%% API Functions

%% Start the agent discovery service
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Register an agent with capabilities
-spec register_agent(AgentId, Pid, Capabilities) -> Result when
    AgentId :: term(),
    Pid :: pid(),
    Capabilities :: map(),
    Result :: ok | {error, Reason},
    Reason :: term().
register_agent(AgentId, Pid, Capabilities) ->
    gen_server:call(?SERVER, {register_agent, AgentId, Pid, Capabilities}).

%% Unregister an agent
-spec unregister_agent(AgentId) -> Result when
    AgentId :: term(),
    Result :: ok | {error, Reason},
    Reason :: term().
unregister_agent(AgentId) ->
    gen_server:call(?SERVER, {unregister_agent, AgentId}).

%% Find an agent by ID
-spec find_agent(AgentId) -> Result when
    AgentId :: term(),
    Result :: {ok, AgentInfo} | {error, Reason},
    AgentInfo :: map(),
    Reason :: term().
find_agent(AgentId) ->
    gen_server:call(?SERVER, {find_agent, AgentId}).

%% Find agents by capability
-spec find_by_capability(Capability) -> Result when
    Capability :: term(),
    Result :: {ok, [AgentInfo]} | {error, Reason},
    AgentInfo :: map(),
    Reason :: term().
find_by_capability(Capability) ->
    gen_server:call(?SERVER, {find_by_capability, Capability}).

%% Get all registered agents
-spec find_all() -> Result when
    Result :: {ok, [AgentInfo]} | {error, Reason},
    AgentInfo :: map(),
    Reason :: term().
find_all() ->
    gen_server:call(?SERVER, find_all).

%% Get detailed information about an agent
-spec get_agent_info(AgentId) -> Result when
    AgentId :: term(),
    Result :: {ok, AgentInfo} | {error, Reason},
    AgentInfo :: map(),
    Reason :: term().
get_agent_info(AgentId) ->
    gen_server:call(?SERVER, {get_agent_info, AgentId}).

%% Update agent status
-spec update_agent_status(AgentId, Status) -> Result when
    AgentId :: term(),
    Status :: atom(),
    Result :: ok | {error, Reason},
    Reason :: term().
update_agent_status(AgentId, Status) ->
    gen_server:call(?SERVER, {update_agent_status, AgentId, Status}).

%% Update agent capabilities
-spec update_agent_capabilities(AgentId, Capabilities) -> Result when
    AgentId :: term(),
    Capabilities :: map(),
    Result :: ok | {error, Reason},
    Reason :: term().
update_agent_capabilities(AgentId, Capabilities) ->
    gen_server:call(?SERVER, {update_agent_capabilities, AgentId, Capabilities}).

%% Broadcast a heartbeat to check all agents
broadcast_heartbeat() ->
    gen_server:cast(?SERVER, broadcast_heartbeat).

%% gen_server callbacks

init(Options) ->
    % Start periodic heartbeat
    HeartbeatInterval = maps:get(heartbeat_interval, Options, ?DEFAULT_HEARTBEAT_INTERVAL),
    HeartbeatRef = schedule_heartbeat(HeartbeatInterval),
    
    {ok, #state{
        heartbeat_interval = HeartbeatInterval,
        heartbeat_ref = HeartbeatRef
    }}.

handle_call({register_agent, AgentId, Pid, Capabilities}, _From, State) ->
    % Monitor the agent process to detect crashes
    MonitorRef = erlang:monitor(process, Pid),
    
    % Store agent information
    Now = os:timestamp(),
    Status = active,
    AgentInfo = #{
        pid => Pid,
        monitor_ref => MonitorRef,
        timestamp => Now,
        capabilities => Capabilities,
        status => Status
    },
    
    % Update agents map
    NewAgents = maps:put(AgentId, AgentInfo, State#state.agents),
    
    % Update capabilities index
    NewCapabilitiesIndex = update_capabilities_index(AgentId, Capabilities, State#state.capabilities_index),
    
    {reply, ok, State#state{
        agents = NewAgents,
        capabilities_index = NewCapabilitiesIndex
    }};

handle_call({unregister_agent, AgentId}, _From, State) ->
    % Remove agent and update capabilities index
    {NewAgents, NewCapabilitiesIndex} = case maps:find(AgentId, State#state.agents) of
        {ok, AgentInfo} ->
            % Demonitor the agent process
            erlang:demonitor(maps:get(monitor_ref, AgentInfo, make_ref()), [flush]),
            
            % Remove from agents map
            NextAgents = maps:remove(AgentId, State#state.agents),
            
            % Remove from capabilities index
            NextCapabilitiesIndex = remove_from_capabilities_index(
                AgentId,
                maps:get(capabilities, AgentInfo, #{}),
                State#state.capabilities_index
            ),
            
            {NextAgents, NextCapabilitiesIndex};
        error ->
            {State#state.agents, State#state.capabilities_index}
    end,
    
    {reply, ok, State#state{
        agents = NewAgents,
        capabilities_index = NewCapabilitiesIndex
    }};

handle_call({find_agent, AgentId}, _From, State) ->
    % Find an agent by ID
    Result = case maps:find(AgentId, State#state.agents) of
        {ok, AgentInfo} -> {ok, format_agent_info(AgentId, AgentInfo)};
        error -> {error, {agent_not_found, AgentId}}
    end,
    
    {reply, Result, State};

handle_call({find_by_capability, Capability}, _From, State) ->
    % Find agents by capability
    AgentIds = case maps:find(Capability, State#state.capabilities_index) of
        {ok, Ids} -> Ids;
        error -> []
    end,
    
    % Gather agent information
    AgentInfoList = [
        format_agent_info(Id, Info)
        || Id <- AgentIds,
           {ok, Info} <- [maps:find(Id, State#state.agents)]
    ],
    
    {reply, {ok, AgentInfoList}, State};

handle_call(find_all, _From, State) ->
    % Return all registered agents
    AgentInfoList = [
        format_agent_info(Id, Info)
        || {Id, Info} <- maps:to_list(State#state.agents)
    ],
    
    {reply, {ok, AgentInfoList}, State};

handle_call({get_agent_info, AgentId}, _From, State) ->
    % Get detailed agent information
    Result = case maps:find(AgentId, State#state.agents) of
        {ok, AgentInfo} -> {ok, format_agent_info(AgentId, AgentInfo)};
        error -> {error, {agent_not_found, AgentId}}
    end,
    
    {reply, Result, State};

handle_call({update_agent_status, AgentId, Status}, _From, State) ->
    % Update an agent's status
    {Result, NewState} = case maps:find(AgentId, State#state.agents) of
        {ok, AgentInfo} ->
            % Update the status
            UpdatedInfo = AgentInfo#{status => Status},
            NewAgents = maps:put(AgentId, UpdatedInfo, State#state.agents),
            {ok, State#state{agents = NewAgents}};
        error ->
            {{error, {agent_not_found, AgentId}}, State}
    end,
    
    {reply, Result, NewState};

handle_call({update_agent_capabilities, AgentId, NewCapabilities}, _From, State) ->
    % Update an agent's capabilities
    {Result, NewState} = case maps:find(AgentId, State#state.agents) of
        {ok, AgentInfo} ->
            % Get the old capabilities
            OldCapabilities = maps:get(capabilities, AgentInfo, #{}),
            
            % Update the capabilities
            UpdatedInfo = AgentInfo#{capabilities => NewCapabilities},
            NewAgents = maps:put(AgentId, UpdatedInfo, State#state.agents),
            
            % Update capabilities index
            NewIndex = update_capabilities_index_for_change(
                AgentId,
                OldCapabilities,
                NewCapabilities,
                State#state.capabilities_index
            ),
            
            {ok, State#state{
                agents = NewAgents,
                capabilities_index = NewIndex
            }};
        error ->
            {{error, {agent_not_found, AgentId}}, State}
    end,
    
    {reply, Result, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(broadcast_heartbeat, State) ->
    % Check all agents for liveness
    {NewAgents, NewCapabilitiesIndex} = check_agent_liveness(
        State#state.agents,
        State#state.capabilities_index
    ),
    
    % Schedule next heartbeat
    NewHeartbeatRef = schedule_heartbeat(State#state.heartbeat_interval),
    
    {noreply, State#state{
        agents = NewAgents,
        capabilities_index = NewCapabilitiesIndex,
        heartbeat_ref = NewHeartbeatRef
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    % Find the agent with this monitor reference
    AgentId = find_agent_by_monitor_ref(MonitorRef, State#state.agents),
    
    % Remove the agent if found
    {NewAgents, NewCapabilitiesIndex} = case AgentId of
        undefined ->
            {State#state.agents, State#state.capabilities_index};
        Id ->
            % Get agent info
            AgentInfo = maps:get(Id, State#state.agents),
            
            % Remove from agents map
            NextAgents = maps:remove(Id, State#state.agents),
            
            % Remove from capabilities index
            NextCapabilitiesIndex = remove_from_capabilities_index(
                Id,
                maps:get(capabilities, AgentInfo, #{}),
                State#state.capabilities_index
            ),
            
            {NextAgents, NextCapabilitiesIndex}
    end,
    
    {noreply, State#state{
        agents = NewAgents,
        capabilities_index = NewCapabilitiesIndex
    }};

handle_info(cleanup_stale_agents, State) ->
    % Check and remove stale agents
    {NewAgents, NewCapabilitiesIndex} = cleanup_stale_agents(
        State#state.agents,
        State#state.capabilities_index
    ),
    
    {noreply, State#state{
        agents = NewAgents,
        capabilities_index = NewCapabilitiesIndex
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{heartbeat_ref = HeartbeatRef}) ->
    % Cancel the heartbeat timer if it exists
    case HeartbeatRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(HeartbeatRef)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

%% Format agent information for external use
format_agent_info(AgentId, AgentInfo) ->
    #{
        id => AgentId,
        pid => maps:get(pid, AgentInfo),
        capabilities => maps:get(capabilities, AgentInfo, #{}),
        status => maps:get(status, AgentInfo, unknown),
        last_seen => maps:get(timestamp, AgentInfo)
    }.

%% Update capabilities index when adding an agent
update_capabilities_index(AgentId, Capabilities, Index) ->
    CapabilityKeys = case Capabilities of
        Cap when is_map(Cap) -> maps:keys(Cap);
        Cap when is_list(Cap) -> Cap;
        _ -> []
    end,
    
    lists:foldl(
        fun(Capability, AccIndex) ->
            AgentIds = case maps:find(Capability, AccIndex) of
                {ok, Ids} -> Ids;
                error -> []
            end,
            maps:put(Capability, [AgentId | AgentIds], AccIndex)
        end,
        Index,
        CapabilityKeys
    ).

%% Remove an agent from the capabilities index
remove_from_capabilities_index(AgentId, Capabilities, Index) ->
    CapabilityKeys = case Capabilities of
        Cap when is_map(Cap) -> maps:keys(Cap);
        Cap when is_list(Cap) -> Cap;
        _ -> []
    end,
    
    lists:foldl(
        fun(Capability, AccIndex) ->
            case maps:find(Capability, AccIndex) of
                {ok, Ids} ->
                    NewIds = lists:delete(AgentId, Ids),
                    case NewIds of
                        [] -> maps:remove(Capability, AccIndex);
                        _ -> maps:put(Capability, NewIds, AccIndex)
                    end;
                error ->
                    AccIndex
            end
        end,
        Index,
        CapabilityKeys
    ).

%% Update capabilities index when an agent's capabilities change
update_capabilities_index_for_change(AgentId, OldCapabilities, NewCapabilities, Index) ->
    % Convert capabilities to lists of keys
    OldKeys = case OldCapabilities of
        Old when is_map(Old) -> maps:keys(Old);
        Old when is_list(Old) -> Old;
        _ -> []
    end,
    
    NewKeys = case NewCapabilities of
        New when is_map(New) -> maps:keys(New);
        New when is_list(New) -> New;
        _ -> []
    end,
    
    % Find keys to remove and add
    KeysToRemove = OldKeys -- NewKeys,
    KeysToAdd = NewKeys -- OldKeys,
    
    % Remove agent from old capabilities
    IndexAfterRemove = lists:foldl(
        fun(Capability, AccIndex) ->
            case maps:find(Capability, AccIndex) of
                {ok, Ids} ->
                    NewIds = lists:delete(AgentId, Ids),
                    case NewIds of
                        [] -> maps:remove(Capability, AccIndex);
                        _ -> maps:put(Capability, NewIds, AccIndex)
                    end;
                error ->
                    AccIndex
            end
        end,
        Index,
        KeysToRemove
    ),
    
    % Add agent to new capabilities
    lists:foldl(
        fun(Capability, AccIndex) ->
            AgentIds = case maps:find(Capability, AccIndex) of
                {ok, Ids} -> Ids;
                error -> []
            end,
            maps:put(Capability, [AgentId | AgentIds], AccIndex)
        end,
        IndexAfterRemove,
        KeysToAdd
    ).

%% Find an agent by monitor reference
find_agent_by_monitor_ref(MonitorRef, Agents) ->
    maps:fold(
        fun(AgentId, Info, Acc) ->
            case maps:get(monitor_ref, Info, undefined) of
                MonitorRef -> AgentId;
                _ -> Acc
            end
        end,
        undefined,
        Agents
    ).

%% Check agent liveness and update status
check_agent_liveness(Agents, CapabilitiesIndex) ->
    % Current timestamp
    Now = os:timestamp(),
    Threshold = 60 * 1000000, % 60 seconds in microseconds
    
    % Identify agents to mark as inactive
    {ActiveAgents, InactiveAgentIds} = maps:fold(
        fun(AgentId, Info, {ActiveAcc, InactiveAcc}) ->
            Pid = maps:get(pid, Info),
            TimeDiff = timer:now_diff(Now, maps:get(timestamp, Info)),
            
            % Check if the process is alive
            IsAlive = case is_process_alive(Pid) of
                true -> TimeDiff < Threshold;
                false -> false
            end,
            
            case IsAlive of
                true -> 
                    % Update timestamp for active agents
                    NewInfo = Info#{timestamp => Now},
                    {maps:put(AgentId, NewInfo, ActiveAcc), InactiveAcc};
                false ->
                    % Mark for removal
                    {ActiveAcc, [AgentId | InactiveAcc]}
            end
        end,
        {#{}, []},
        Agents
    ),
    
    % Remove inactive agents from capabilities index
    NewCapabilitiesIndex = lists:foldl(
        fun(AgentId, Index) ->
            case maps:find(AgentId, Agents) of
                {ok, Info} ->
                    Capabilities = maps:get(capabilities, Info, #{}),
                    remove_from_capabilities_index(AgentId, Capabilities, Index);
                error ->
                    Index
            end
        end,
        CapabilitiesIndex,
        InactiveAgentIds
    ),
    
    {ActiveAgents, NewCapabilitiesIndex}.

%% Cleanup stale agents
cleanup_stale_agents(Agents, CapabilitiesIndex) ->
    % Current timestamp
    Now = os:timestamp(),
    Threshold = 600 * 1000000, % 10 minutes in microseconds
    
    % Identify agents to remove
    {RemainingAgents, StaleAgentIds} = maps:fold(
        fun(AgentId, Info, {RemAcc, StaleAcc}) ->
            TimeDiff = timer:now_diff(Now, maps:get(timestamp, Info)),
            
            case TimeDiff < Threshold of
                true -> {maps:put(AgentId, Info, RemAcc), StaleAcc};
                false -> {RemAcc, [AgentId | StaleAcc]}
            end
        end,
        {#{}, []},
        Agents
    ),
    
    % Remove stale agents from capabilities index
    NewCapabilitiesIndex = lists:foldl(
        fun(AgentId, Index) ->
            case maps:find(AgentId, Agents) of
                {ok, Info} ->
                    Capabilities = maps:get(capabilities, Info, #{}),
                    remove_from_capabilities_index(AgentId, Capabilities, Index);
                error ->
                    Index
            end
        end,
        CapabilitiesIndex,
        StaleAgentIds
    ),
    
    {RemainingAgents, NewCapabilitiesIndex}.

%% Schedule the next heartbeat
schedule_heartbeat(Interval) ->
    erlang:send_after(Interval, self(), broadcast_heartbeat).