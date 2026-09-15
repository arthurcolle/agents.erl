%%%-------------------------------------------------------------------
%%% @doc Dynamic lifecycle owner for the optional super-agent controller.
%%% @end
%%%-------------------------------------------------------------------
-module(super_agent_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    start_super_agent/1,
    stop_super_agent/0,
    init/1
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_super_agent(Config) when is_map(Config) ->
    ChildSpec = #{
        id => super_agent,
        start => {super_agent, start_link, [Config]},
        restart => transient,
        shutdown => 5000,
        type => worker,
        modules => [super_agent]
    },
    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, _Info} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {error, already_present} ->
            supervisor:restart_child(?MODULE, super_agent);
        Error ->
            Error
    end.

stop_super_agent() ->
    case supervisor:terminate_child(?MODULE, super_agent) of
        ok ->
            case supervisor:delete_child(?MODULE, super_agent) of
                ok -> ok;
                {error, not_found} -> ok;
                Error -> Error
            end;
        {error, not_found} ->
            {error, not_found};
        Error ->
            Error
    end.

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },
    {ok, {SupFlags, []}}.
