%%%-------------------------------------------------------------------
%%% @doc OTP application callback for the core agent runtime.
%%%
%%% Dependency startup is intentionally left to the application
%%% controller via agents.app.src. This module owns only the root
%%% supervisor lifecycle.
%%% @end
%%%-------------------------------------------------------------------
-module(agents_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    agents_sup:start_link().

stop(_State) ->
    ok.
