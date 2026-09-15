%%%-------------------------------------------------------------------
%%% @doc Dynamic host for runtime-created supervisors.
%%%
%%% This process is the sole OTP owner of supervisors created through
%%% dynamic_supervisor_manager.
%%% @end
%%%-------------------------------------------------------------------
-module(dynamic_supervisors_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    {ok, {SupFlags, []}}.
