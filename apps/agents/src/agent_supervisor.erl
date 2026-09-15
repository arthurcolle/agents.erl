%%%-------------------------------------------------------------------
%%% @doc Dynamic supervisor and lifecycle API for agent instances.
%%%
%%% This supervisor owns agent processes only. Long-lived runtime
%%% services belong under agents_sup.
%%% @end
%%%-------------------------------------------------------------------
-module(agent_supervisor).
-behaviour(supervisor).

-export([
    start_link/0,
    start_agent/1,
    start_agent/2,
    create_agent/1,
    stop_agent/1,
    terminate_agent/1,
    restart_child/1,
    get_all_agents/0
]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_agent(Config0) when is_map(Config0) ->
    Config = normalize_config(Config0),
    AgentId = maps:get(id, Config, maps:get(agent_id, Config, new_agent_id())),
    start_agent(AgentId, Config).

start_agent(AgentId, Config0) when is_map(Config0) ->
    Config = (normalize_config(Config0))#{id => AgentId, agent_id => AgentId},
    case capacity_available() of
        true ->
            ChildSpec = #{
                id => child_id(AgentId),
                start => {agent_instance, start_link, [Config]},
                restart => transient,
                shutdown => 5000,
                type => worker,
                modules => [agent_instance]
            },
            case supervisor:start_child(?SERVER, ChildSpec) of
                {ok, Pid} = Result ->
                    ok = maybe_register(AgentId, Pid, metadata(Config)),
                    Result;
                {ok, Pid, _Info} = Result ->
                    ok = maybe_register(AgentId, Pid, metadata(Config)),
                    Result;
                Error ->
                    Error
            end;
        false ->
            {error, max_agents_reached}
    end.

%% Command-oriented API used by super_agent.
create_agent(Config0) when is_map(Config0) ->
    Config = normalize_config(Config0),
    AgentId = maps:get(id, Config, maps:get(agent_id, Config, new_agent_id())),
    case start_agent(AgentId, Config) of
        {ok, _Pid} -> {ok, AgentId};
        {ok, _Pid, _Info} -> {ok, AgentId};
        Error -> Error
    end.

stop_agent(Target) ->
    terminate_agent(Target).

terminate_agent(Target) ->
    case resolve_child(Target) of
        {ok, ChildId, AgentId} ->
            case supervisor:terminate_child(?SERVER, ChildId) of
                ok ->
                    DeleteResult = supervisor:delete_child(?SERVER, ChildId),
                    ok = maybe_unregister(AgentId),
                    normalize_delete_result(DeleteResult);
                {error, not_found} ->
                    {error, agent_not_found};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

restart_child(Target) ->
    case resolve_child(Target) of
        {ok, ChildId, _AgentId} ->
            supervisor:restart_child(?SERVER, ChildId);
        Error ->
            Error
    end.

get_all_agents() ->
    Children = supervisor:which_children(?SERVER),
    Agents = [
        agent_info(AgentId, Pid)
        || {{agent, AgentId}, Pid, worker, _Modules} <- Children
    ],
    {ok, Agents}.

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => application:get_env(agents, agent_restart_intensity, 10),
        period => application:get_env(agents, agent_restart_period, 60)
    },
    {ok, {SupFlags, []}}.

capacity_available() ->
    MaxAgents = application:get_env(agents, max_agents, 10000),
    Counts = supervisor:count_children(?SERVER),
    Active = case Counts of
        #{active := Count} -> Count;
        CountList when is_list(CountList) ->
            proplists:get_value(active, CountList, 0)
    end,
    Active < MaxAgents.

resolve_child(Pid) when is_pid(Pid) ->
    case lists:keyfind(Pid, 2, supervisor:which_children(?SERVER)) of
        {{agent, AgentId} = ChildId, Pid, worker, _Modules} ->
            {ok, ChildId, AgentId};
        false ->
            {error, agent_not_found}
    end;
resolve_child({agent, AgentId} = ChildId) ->
    case lists:keymember(ChildId, 1, supervisor:which_children(?SERVER)) of
        true -> {ok, ChildId, AgentId};
        false -> {error, agent_not_found}
    end;
resolve_child(AgentId) ->
    resolve_child(child_id(AgentId)).

child_id(AgentId) ->
    {agent, AgentId}.

agent_info(AgentId, Pid) when is_pid(Pid) ->
    State = try agent_instance:get_state(Pid) of
        AgentState when is_map(AgentState) -> AgentState;
        _ -> #{}
    catch
        _:_ -> #{}
    end,
    maps:merge(State, #{
        id => AgentId,
        pid => Pid,
        status => running
    });
agent_info(AgentId, restarting) ->
    #{id => AgentId, pid => restarting, status => restarting};
agent_info(AgentId, undefined) ->
    #{id => AgentId, pid => undefined, status => stopped}.

metadata(Config) ->
    Base = maps:with(
        [name, type, model, tools, capabilities, system_prompt],
        Config
    ),
    Base#{
        created_at => erlang:system_time(millisecond),
        status => active
    }.

maybe_register(AgentId, Pid, Meta) ->
    case whereis(agent_registry) of
        undefined ->
            ok;
        _ ->
            try agent_registry:register_agent(AgentId, Pid, Meta) of
                _ -> ok
            catch
                _:_ -> ok
            end
    end.

maybe_unregister(AgentId) ->
    case whereis(agent_registry) of
        undefined ->
            ok;
        _ ->
            try agent_registry:unregister_agent(AgentId) of
                _ -> ok
            catch
                _:_ -> ok
            end
    end.

normalize_delete_result(ok) -> ok;
normalize_delete_result({error, not_found}) -> ok;
normalize_delete_result(Error) -> Error.

normalize_config(Config0) ->
    KeyPairs = [
        {<<"id">>, id},
        {<<"agent_id">>, agent_id},
        {<<"name">>, name},
        {<<"type">>, type},
        {<<"model">>, model},
        {<<"tools">>, tools},
        {<<"capabilities">>, capabilities},
        {<<"system_prompt">>, system_prompt},
        {<<"autonomous_mode">>, autonomous_mode},
        {<<"max_autonomous_turns">>, max_autonomous_turns},
        {<<"api_preference">>, api_preference}
    ],
    lists:foldl(fun({BinaryKey, AtomKey}, Config) ->
        case {maps:is_key(AtomKey, Config), maps:find(BinaryKey, Config)} of
            {false, {ok, Value}} -> Config#{AtomKey => Value};
            _ -> Config
        end
    end, Config0, KeyPairs).

new_agent_id() ->
    list_to_binary(agent_uuid:to_string(agent_uuid:uuid4())).
