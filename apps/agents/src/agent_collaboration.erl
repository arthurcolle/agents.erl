%% agent_collaboration.erl
%% Module for managing agent collaboration and communication
-module(agent_collaboration).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    create_collaboration/3,
    send_message/3,
    broadcast_to_group/2,
    join_group/2,
    leave_group/2,
    get_collaborations/1,
    get_group_members/1
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

-record(state, {
    collaborations = #{} :: map(), % collaboration_id => #{agents, messages, created_at}
    groups = #{} :: map(),         % group_id => [agent_ids]
    agent_groups = #{} :: map()    % agent_id => [group_ids]
}).

-define(SERVER, ?MODULE).

%% API Functions

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_collaboration(AgentId1, AgentId2, Purpose) ->
    gen_server:call(?SERVER, {create_collaboration, AgentId1, AgentId2, Purpose}).

send_message(CollaborationId, FromAgent, Message) ->
    gen_server:call(?SERVER, {send_message, CollaborationId, FromAgent, Message}).

broadcast_to_group(GroupId, Message) ->
    gen_server:cast(?SERVER, {broadcast, GroupId, Message}).

join_group(AgentId, GroupId) ->
    gen_server:call(?SERVER, {join_group, AgentId, GroupId}).

leave_group(AgentId, GroupId) ->
    gen_server:call(?SERVER, {leave_group, AgentId, GroupId}).

get_collaborations(AgentId) ->
    gen_server:call(?SERVER, {get_collaborations, AgentId}).

get_group_members(GroupId) ->
    gen_server:call(?SERVER, {get_group_members, GroupId}).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({create_collaboration, AgentId1, AgentId2, Purpose}, _From, State) ->
    CollabId = generate_collaboration_id(),
    Collaboration = #{
        id => CollabId,
        agents => [AgentId1, AgentId2],
        purpose => Purpose,
        messages => [],
        created_at => erlang:timestamp(),
        status => active
    },
    
    NewCollaborations = maps:put(CollabId, Collaboration, State#state.collaborations),
    
    % Notify both agents
    notify_agents([AgentId1, AgentId2], {collaboration_created, CollabId, Purpose}),
    
    {reply, {ok, CollabId}, State#state{collaborations = NewCollaborations}};

handle_call({send_message, CollaborationId, FromAgent, Message}, _From, State) ->
    case maps:get(CollaborationId, State#state.collaborations, undefined) of
        undefined ->
            {reply, {error, collaboration_not_found}, State};
        Collaboration ->
            % Check if agent is part of collaboration
            case lists:member(FromAgent, maps:get(agents, Collaboration)) of
                true ->
                    EnrichedMessage = #{
                        from => FromAgent,
                        content => Message,
                        timestamp => erlang:timestamp()
                    },
                    
                    UpdatedMessages = [EnrichedMessage | maps:get(messages, Collaboration)],
                    UpdatedCollab = maps:put(messages, UpdatedMessages, Collaboration),
                    NewCollaborations = maps:put(CollaborationId, UpdatedCollab, State#state.collaborations),
                    
                    % Notify other agents in collaboration
                    OtherAgents = lists:delete(FromAgent, maps:get(agents, Collaboration)),
                    notify_agents(OtherAgents, {collaboration_message, CollaborationId, EnrichedMessage}),
                    
                    {reply, ok, State#state{collaborations = NewCollaborations}};
                false ->
                    {reply, {error, agent_not_in_collaboration}, State}
            end
    end;

handle_call({join_group, AgentId, GroupId}, _From, State) ->
    Groups = State#state.groups,
    AgentGroups = State#state.agent_groups,
    
    % Add agent to group
    UpdatedGroup = case maps:get(GroupId, Groups, undefined) of
        undefined -> [AgentId];
        Members -> [AgentId | Members]
    end,
    
    % Update agent's group list
    UpdatedAgentGroups = case maps:get(AgentId, AgentGroups, undefined) of
        undefined -> [GroupId];
        CurrentGroups -> [GroupId | CurrentGroups]
    end,
    
    NewState = State#state{
        groups = maps:put(GroupId, UpdatedGroup, Groups),
        agent_groups = maps:put(AgentId, UpdatedAgentGroups, AgentGroups)
    },
    
    % Notify group members
    notify_agents(UpdatedGroup, {agent_joined_group, GroupId, AgentId}),
    
    {reply, ok, NewState};

handle_call({leave_group, AgentId, GroupId}, _From, State) ->
    Groups = State#state.groups,
    AgentGroups = State#state.agent_groups,
    
    % Remove agent from group
    UpdatedGroup = case maps:get(GroupId, Groups, undefined) of
        undefined -> [];
        Members -> lists:delete(AgentId, Members)
    end,
    
    % Update agent's group list
    UpdatedAgentGroups = case maps:get(AgentId, AgentGroups, undefined) of
        undefined -> [];
        CurrentGroups -> lists:delete(GroupId, CurrentGroups)
    end,
    
    NewState = State#state{
        groups = maps:put(GroupId, UpdatedGroup, Groups),
        agent_groups = maps:put(AgentId, UpdatedAgentGroups, AgentGroups)
    },
    
    % Notify remaining group members
    notify_agents(UpdatedGroup, {agent_left_group, GroupId, AgentId}),
    
    {reply, ok, NewState};

handle_call({get_collaborations, AgentId}, _From, State) ->
    AgentCollabs = maps:fold(fun(CollabId, Collab, Acc) ->
        case lists:member(AgentId, maps:get(agents, Collab)) of
            true -> [CollabId | Acc];
            false -> Acc
        end
    end, [], State#state.collaborations),
    
    {reply, AgentCollabs, State};

handle_call({get_group_members, GroupId}, _From, State) ->
    Members = maps:get(GroupId, State#state.groups, []),
    {reply, Members, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({broadcast, GroupId, Message}, State) ->
    case maps:get(GroupId, State#state.groups, undefined) of
        undefined ->
            {noreply, State};
        Members ->
            BroadcastMessage = #{
                type => group_broadcast,
                group_id => GroupId,
                content => Message,
                timestamp => erlang:timestamp()
            },
            notify_agents(Members, BroadcastMessage),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

generate_collaboration_id() ->
    list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

notify_agents(AgentIds, Message) ->
    lists:foreach(fun(AgentId) ->
        case agent_registry:find_agent(AgentId) of
            {ok, Pid} ->
                gen_server:cast(Pid, {collaboration_event, Message});
            _ ->
                ok
        end
    end, AgentIds).