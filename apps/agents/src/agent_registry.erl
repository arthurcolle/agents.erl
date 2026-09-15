%% agent_registry.erl
%% Registry for tracking active agent processes
-module(agent_registry).
-behaviour(gen_server).

-export([
    start_link/1,
    register_agent/2,
    register_agent/3,
    unregister_agent/1,
    list_agents/0,
    get_agent/1,
    find_agent/1,
    send_to_agent/2
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
-define(CALL_TIMEOUT, 5000).
-define(CLEANUP_INTERVAL, 60000).

-record(state, {
    agents = #{} :: map(),    % SessionId -> {Pid, Timestamp, Meta, MonitorRef}
    cleanup_timer :: reference() | undefined
}).

%% API Functions
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Register an agent process
register_agent(SessionId, Pid) ->
    register_agent(SessionId, Pid, #{}).

register_agent(SessionId, Pid, Meta) ->
    case {is_pid(Pid), is_map(Meta)} of
        {true, true} ->
            gen_server:call(?SERVER, {register, SessionId, Pid, Meta}, ?CALL_TIMEOUT);
        _ ->
            {error, invalid_agent_registration}
    end.

%% Unregister an agent process
unregister_agent(SessionId) ->
    gen_server:call(?SERVER, {unregister, SessionId}, ?CALL_TIMEOUT).

%% List all active agents
list_agents() ->
    gen_server:call(?SERVER, list_agents, ?CALL_TIMEOUT).

%% Get a specific agent
get_agent(SessionId) ->
    gen_server:call(?SERVER, {get_agent, SessionId}, ?CALL_TIMEOUT).

%% Find a specific agent (alias for get_agent for compatibility)
find_agent(SessionId) ->
    get_agent(SessionId).

%% Send a message to a specific agent
send_to_agent(SessionId, Message) ->
    gen_server:call(?SERVER, {send_to_agent, SessionId, Message}, ?CALL_TIMEOUT).

%% gen_server callbacks
init(_Options) ->
    % Start periodic cleanup of stale agents
    Timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_stale_agents),
    {ok, #state{cleanup_timer = Timer}}.

handle_call(list_agents, _From, State) ->
    AgentList = maps:fold(fun(K, V, Acc) ->
        [{K, agent_pid(V), agent_meta(V)} | Acc]
    end, [], State#state.agents),
    {reply, AgentList, State};

handle_call({get_agent, SessionId}, _From, State) ->
    Result = case maps:find(SessionId, State#state.agents) of
        {ok, Entry} -> {ok, agent_pid(Entry)};
        error -> {error, agent_not_found}
    end,
    {reply, Result, State};

handle_call({send_to_agent, SessionId, Message}, _From, State) ->
    Result = case maps:find(SessionId, State#state.agents) of
        {ok, Entry} ->
            Pid = agent_pid(Entry),
            Pid ! Message,
            ok;
        error ->
            {error, agent_not_found}
    end,
    {reply, Result, State};

handle_call({register, SessionId, Pid, Meta}, _From, State) ->
    case register_entry(SessionId, Pid, Meta, State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call({unregister, SessionId}, _From, State) ->
    {NewAgents, _Removed} = remove_entry(SessionId, State#state.agents),
    {reply, ok, State#state{agents = NewAgents}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% Retain cast handling for compatibility with older callers.
handle_cast({register, SessionId, Pid, Meta}, State) ->
    case {is_pid(Pid), is_map(Meta)} of
        {true, true} ->
            case register_entry(SessionId, Pid, Meta, State) of
                {ok, NewState} -> {noreply, NewState};
                {error, _} -> {noreply, State}
            end;
        _ ->
            {noreply, State}
    end;

handle_cast({unregister, SessionId}, State) ->
    {NewAgents, _Removed} = remove_entry(SessionId, State#state.agents),
    {noreply, State#state{agents = NewAgents}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State) ->
    %% A process can be registered again under the same session before an old
    %% DOWN message is delivered.  Match both the pid and monitor reference so
    %% an old lifecycle event cannot remove the replacement.
    NewAgents = maps:filter(
        fun(_SessionId, Entry) ->
            not (agent_pid(Entry) =:= Pid andalso
                 agent_monitor(Entry) =:= MonitorRef)
        end,
        State#state.agents
    ),
    {noreply, State#state{agents = NewAgents}};

handle_info(cleanup_stale_agents, State) ->
    % Registration age is not liveness. Keep healthy long-running agents
    % and remove only processes that are no longer alive.
    NewAgents = maps:fold(fun(SessionId, Entry, Acc) ->
        Pid = agent_pid(Entry),
        case is_process_alive(Pid) of
            true -> maps:put(SessionId, Entry, Acc);
            false ->
                demonitor_entry(Entry),
                Acc
        end
    end, #{}, State#state.agents),

    % Schedule next cleanup
    Timer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_stale_agents),
    {noreply, State#state{agents = NewAgents, cleanup_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    maybe_cancel_timer(State#state.cleanup_timer),
    maps:foreach(fun(_SessionId, Entry) -> demonitor_entry(Entry) end,
                 State#state.agents),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

register_entry(SessionId, Pid, Meta, State) ->
    case is_process_alive(Pid) of
        false -> {error, process_not_alive};
        true ->
            Existing = maps:get(SessionId, State#state.agents, undefined),
            MonitorRef = monitor_replacement(Pid, Existing),
            Entry = {Pid, erlang:system_time(microsecond), Meta, MonitorRef},
            {ok, State#state{agents = maps:put(SessionId, Entry,
                                                State#state.agents)}}
    end.

monitor_replacement(Pid, {Pid, _Timestamp, _Meta, MonitorRef}) ->
    MonitorRef;
monitor_replacement(Pid, Existing) ->
    demonitor_entry(Existing),
    erlang:monitor(process, Pid).

remove_entry(SessionId, Agents) ->
    case maps:take(SessionId, Agents) of
        {Entry, Remaining} ->
            demonitor_entry(Entry),
            {Remaining, true};
        error ->
            {Agents, false}
    end.

demonitor_entry({_Pid, _Timestamp, _Meta, MonitorRef}) when is_reference(MonitorRef) ->
    erlang:demonitor(MonitorRef, [flush]),
    ok;
demonitor_entry(_) -> ok.

agent_pid({Pid, _Timestamp, _Meta, _MonitorRef}) -> Pid;
agent_pid({Pid, _Timestamp, _Meta}) -> Pid.

agent_meta({_Pid, _Timestamp, Meta, _MonitorRef}) -> Meta;
agent_meta({_Pid, _Timestamp, Meta}) -> Meta.

agent_monitor({_Pid, _Timestamp, _Meta, MonitorRef}) -> MonitorRef;
agent_monitor(_) -> undefined.

maybe_cancel_timer(Timer) when is_reference(Timer) ->
    _ = erlang:cancel_timer(Timer),
    ok;
maybe_cancel_timer(_) -> ok.
