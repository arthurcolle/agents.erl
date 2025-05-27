%% agent_registry.erl
%% Registry for tracking active agent processes
-module(agent_registry).
-behaviour(gen_server).

-export([
    start_link/1,
    register_agent/2,
    unregister_agent/1,
    list_agents/0,
    get_agent/1,
    send_to_agent/2,
    clear_all/0
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

-record(state, {
    agents = #{} :: map()     % SessionId -> {Pid, Timestamp} mapping
}).

%% API Functions
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Register an agent process
register_agent(SessionId, Pid) ->
    gen_server:cast(?SERVER, {register, SessionId, Pid}).

%% Unregister an agent process
unregister_agent(SessionId) ->
    gen_server:cast(?SERVER, {unregister, SessionId}).

%% List all active agents
list_agents() ->
    gen_server:call(?SERVER, list_agents).

%% Get a specific agent
get_agent(SessionId) ->
    gen_server:call(?SERVER, {get_agent, SessionId}).

%% Send a message to a specific agent
send_to_agent(SessionId, Message) ->
    gen_server:call(?SERVER, {send_to_agent, SessionId, Message}).

%% Clear all registered agents
clear_all() ->
    gen_server:call(?SERVER, clear_all).

%% gen_server callbacks
init(_Options) ->
    % Start periodic cleanup of stale agents
    erlang:send_after(60000, self(), cleanup_stale_agents),
    {ok, #state{}}.

handle_call(list_agents, _From, State) ->
    {reply, maps:keys(State#state.agents), State};

handle_call({get_agent, SessionId}, _From, State) ->
    Result = case maps:find(SessionId, State#state.agents) of
        {ok, {Pid, _Timestamp}} -> {ok, Pid};
        error -> {error, agent_not_found}
    end,
    {reply, Result, State};

handle_call({send_to_agent, SessionId, Message}, _From, State) ->
    Result = case maps:find(SessionId, State#state.agents) of
        {ok, {Pid, _Timestamp}} ->
            Pid ! Message,
            ok;
        error ->
            {error, agent_not_found}
    end,
    {reply, Result, State};

handle_call(clear_all, _From, _State) ->
    NewState = #state{agents = #{}},
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({register, SessionId, Pid}, State) ->
    % Monitor the process to detect crashes
    erlang:monitor(process, Pid),
    
    % Store the agent with timestamp
    NewAgents = maps:put(SessionId, {Pid, os:timestamp()}, State#state.agents),
    {noreply, State#state{agents = NewAgents}};

handle_cast({unregister, SessionId}, State) ->
    % Remove the agent
    NewAgents = maps:remove(SessionId, State#state.agents),
    {noreply, State#state{agents = NewAgents}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    % Process crashed or exited, remove it from the registry
    NewAgents = maps:filter(
        fun(_SessionId, {AgentPid, _Timestamp}) -> AgentPid =/= Pid end,
        State#state.agents
    ),
    {noreply, State#state{agents = NewAgents}};

handle_info(cleanup_stale_agents, State) ->
    % Remove agents that haven't been active for more than 10 minutes
    Now = os:timestamp(),
    Threshold = 600 * 1000000, % 10 minutes in microseconds
    
    NewAgents = maps:filter(
        fun(_SessionId, {_Pid, Timestamp}) ->
            timer:now_diff(Now, Timestamp) < Threshold
        end,
        State#state.agents
    ),
    
    % Schedule next cleanup
    erlang:send_after(60000, self(), cleanup_stale_agents),
    {noreply, State#state{agents = NewAgents}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.