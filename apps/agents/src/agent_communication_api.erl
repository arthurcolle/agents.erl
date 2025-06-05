-module(agent_communication_api).

-export([
    send_message/3,
    broadcast_message/2,
    create_collaboration/2,
    join_collaboration/2,
    leave_collaboration/2,
    get_agent_messages/1,
    get_collaborations/0,
    % Budget system exports
    check_budget/1,
    get_remaining_budget/1,
    reset_budget/1,
    get_budget_info/1
]).

%% Send direct message between agents (with budget checking)
send_message(FromAgentId, ToAgentId, Message) ->
    % Check budget first
    case check_budget(FromAgentId) of
        {ok, remaining} ->
            case {agent_registry:find_agent(FromAgentId), agent_registry:find_agent(ToAgentId)} of
                {{ok, FromPid}, {ok, ToPid}} ->
                    % Deduct from budget
                    decrement_budget(FromAgentId),
                    
                    % Log the communication
                    Event = #{
                        type => agent_communication,
                        from => FromAgentId,
                        to => ToAgentId,
                        messageType => direct,
                        content => Message,
                        timestamp => erlang:system_time(millisecond),
                        budgetUsed => true,
                        remainingBudget => get_remaining_budget(FromAgentId)
                    },
                    broadcast_to_websockets(Event),
                    
                    % Actually send the message using agent collaboration
                    case agent_collaboration:send_message(FromPid, ToPid, Message) of
                        ok -> {ok, Event};
                        Error -> Error
                    end;
                _ ->
                    {error, agent_not_found}
            end;
        {error, budget_exhausted} ->
            {error, {budget_exhausted, "Agent has used up hourly communication budget (2-3 messages per hour)"}}
    end.

%% Broadcast message to all agents (with budget checking)
broadcast_message(FromAgentId, Message) ->
    % Check budget first (broadcast counts as 1 message regardless of recipient count)
    case check_budget(FromAgentId) of
        {ok, remaining} ->
            case agent_registry:find_agent(FromAgentId) of
                {ok, FromPid} ->
                    % Deduct from budget
                    decrement_budget(FromAgentId),
                    
                    AllAgents = agent_registry:list_agents(),
                    Recipients = [Pid || {_Id, Pid, _Meta} <- AllAgents, Pid =/= FromPid],
                    
                    Event = #{
                        type => agent_communication,
                        from => FromAgentId,
                        to => broadcast,
                        messageType => broadcast,
                        content => Message,
                        timestamp => erlang:system_time(millisecond),
                        recipientCount => length(Recipients),
                        budgetUsed => true,
                        remainingBudget => get_remaining_budget(FromAgentId)
                    },
                    broadcast_to_websockets(Event),
                    
                    % Send to all agents
                    lists:foreach(fun(Pid) ->
                        agent_collaboration:send_message(FromPid, Pid, Message)
                    end, Recipients),
                    
                    {ok, Event};
                _ ->
                    {error, agent_not_found}
            end;
        {error, budget_exhausted} ->
            {error, {budget_exhausted, "Agent has used up hourly communication budget (2-3 messages per hour)"}}
    end.

%% Create a collaboration group
create_collaboration(AgentIds, Topic) ->
    Agents = lists:filtermap(fun(Id) ->
        case agent_registry:find_agent(Id) of
            {ok, Pid} -> {true, {Id, Pid}};
            _ -> false
        end
    end, AgentIds),
    
    case Agents of
        [] -> {error, no_valid_agents};
        _ ->
            CollabId = generate_collaboration_id(),
            case agent_collaboration:create_collaboration(CollabId, [Pid || {_, Pid} <- Agents]) of
                {ok, _} ->
                    Event = #{
                        type => agent_communication,
                        messageType => collaboration,
                        action => created,
                        collaborationId => CollabId,
                        topic => Topic,
                        participants => [Id || {Id, _} <- Agents],
                        timestamp => erlang:system_time(millisecond)
                    },
                    broadcast_to_websockets(Event),
                    {ok, CollabId};
                Error ->
                    Error
            end
    end.

%% Join existing collaboration
join_collaboration(AgentId, CollaborationId) ->
    case agent_registry:find_agent(AgentId) of
        {ok, Pid} ->
            case agent_collaboration:join_collaboration(CollaborationId, Pid) of
                ok ->
                    Event = #{
                        type => agent_communication,
                        messageType => collaboration,
                        action => joined,
                        collaborationId => CollaborationId,
                        agentId => AgentId,
                        timestamp => erlang:system_time(millisecond)
                    },
                    broadcast_to_websockets(Event),
                    ok;
                Error ->
                    Error
            end;
        _ ->
            {error, agent_not_found}
    end.

%% Leave collaboration
leave_collaboration(AgentId, CollaborationId) ->
    case agent_registry:find_agent(AgentId) of
        {ok, Pid} ->
            case agent_collaboration:leave_collaboration(CollaborationId, Pid) of
                ok ->
                    Event = #{
                        type => agent_communication,
                        messageType => collaboration,
                        action => left,
                        collaborationId => CollaborationId,
                        agentId => AgentId,
                        timestamp => erlang:system_time(millisecond)
                    },
                    broadcast_to_websockets(Event),
                    ok;
                Error ->
                    Error
            end;
        _ ->
            {error, agent_not_found}
    end.

%% Get messages for a specific agent
get_agent_messages(AgentId) ->
    % This would typically query a message store
    % For now, return empty list
    {ok, []}.

%% Get all active collaborations
get_collaborations() ->
    % Get from agent_collaboration registry
    case catch agent_collaboration:list_collaborations() of
        {'EXIT', _} -> {ok, []};
        Collabs -> {ok, Collabs}
    end.

%% Private functions
generate_collaboration_id() ->
    list_to_binary(io_lib:format("collab_~p_~p", [
        erlang:system_time(millisecond),
        rand:uniform(10000)
    ])).

broadcast_to_websockets(Event) ->
    % Send to all connected websocket clients
    case catch gproc:send({p, l, websocket}, {broadcast, jsx:encode(Event)}) of
        {'EXIT', _} -> ok;
        _ -> ok
    end.

%% Budget System Implementation
%% Each agent gets 2-3 messages per hour for research and communication

-define(HOURLY_BUDGET, 3).  % Default 3 messages per hour
-define(BUDGET_RESET_INTERVAL, 3600).  % 1 hour in seconds

%% Check if agent has budget remaining
check_budget(AgentId) ->
    case get_budget_state(AgentId) of
        {MessagesUsed, LastReset} ->
            CurrentTime = erlang:system_time(second),
            
            % Check if we need to reset the budget (hour has passed)
            if 
                CurrentTime - LastReset >= ?BUDGET_RESET_INTERVAL ->
                    reset_budget_internal(AgentId),
                    {ok, ?HOURLY_BUDGET};
                MessagesUsed >= ?HOURLY_BUDGET ->
                    {error, budget_exhausted};
                true ->
                    {ok, ?HOURLY_BUDGET - MessagesUsed}
            end;
        not_found ->
            % First time - initialize budget
            init_budget(AgentId),
            {ok, ?HOURLY_BUDGET}
    end.

%% Get remaining budget for an agent
get_remaining_budget(AgentId) ->
    case check_budget(AgentId) of
        {ok, Remaining} -> Remaining;
        {error, budget_exhausted} -> 0
    end.

%% Manually reset budget (for admin purposes)
reset_budget(AgentId) ->
    reset_budget_internal(AgentId),
    ok.

%% Get comprehensive budget information
get_budget_info(AgentId) ->
    case get_budget_state(AgentId) of
        {MessagesUsed, LastReset} ->
            CurrentTime = erlang:system_time(second),
            TimeUntilReset = ?BUDGET_RESET_INTERVAL - (CurrentTime - LastReset),
            #{
                agentId => AgentId,
                messagesUsed => MessagesUsed,
                totalBudget => ?HOURLY_BUDGET,
                remainingBudget => max(0, ?HOURLY_BUDGET - MessagesUsed),
                lastReset => LastReset,
                timeUntilReset => max(0, TimeUntilReset),
                budgetExhausted => MessagesUsed >= ?HOURLY_BUDGET
            };
        not_found ->
            CurrentTime = erlang:system_time(second),
            #{
                agentId => AgentId,
                messagesUsed => 0,
                totalBudget => ?HOURLY_BUDGET,
                remainingBudget => ?HOURLY_BUDGET,
                lastReset => CurrentTime,
                timeUntilReset => ?BUDGET_RESET_INTERVAL,
                budgetExhausted => false
            }
    end.

%% Private budget functions

%% Initialize budget for new agent
init_budget(AgentId) ->
    CurrentTime = erlang:system_time(second),
    BudgetKey = {agent_budget, AgentId},
    ets:insert(agent_budget_table, {BudgetKey, 0, CurrentTime}).

%% Decrement budget after successful message send
decrement_budget(AgentId) ->
    case get_budget_state(AgentId) of
        {MessagesUsed, LastReset} ->
            BudgetKey = {agent_budget, AgentId},
            ets:insert(agent_budget_table, {BudgetKey, MessagesUsed + 1, LastReset});
        not_found ->
            init_budget(AgentId),
            decrement_budget(AgentId)
    end.

%% Reset budget to zero (new hour started)
reset_budget_internal(AgentId) ->
    CurrentTime = erlang:system_time(second),
    BudgetKey = {agent_budget, AgentId},
    ets:insert(agent_budget_table, {BudgetKey, 0, CurrentTime}).

%% Get current budget state from ETS
get_budget_state(AgentId) ->
    ensure_budget_table(),
    BudgetKey = {agent_budget, AgentId},
    case ets:lookup(agent_budget_table, BudgetKey) of
        [{BudgetKey, MessagesUsed, LastReset}] ->
            {MessagesUsed, LastReset};
        [] ->
            not_found
    end.

%% Ensure the budget ETS table exists
ensure_budget_table() ->
    case ets:whereis(agent_budget_table) of
        undefined ->
            ets:new(agent_budget_table, [named_table, public, set, {read_concurrency, true}]);
        _ ->
            ok
    end.