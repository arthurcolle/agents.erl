%%%-------------------------------------------------------------------
%%% @doc Storage subtree for the web application.
%%%
%%% rest_for_one preserves the dependency that conversation storage is
%%% initialized only after the persistent table manager is available.
%%% @end
%%%-------------------------------------------------------------------
-module(agent_web_storage_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => rest_for_one, intensity => 5, period => 30},
    Children = [
        worker(persistent_table_manager),
        worker(conversation_initializer)
    ],
    {ok, {SupFlags, Children}}.

worker(Module) ->
    #{
        id => Module,
        start => {Module, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [Module]
    }.
