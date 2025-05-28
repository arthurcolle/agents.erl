-module(agent_web_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = agent_web_sup:start_link(),
    %% Initialize default agents after supervisor starts
    spawn(fun() ->
        timer:sleep(1000), %% Give the system time to fully initialize
        agent_initializer:init_default_agents()
    end),
    {ok, Pid}.

stop(_State) ->
    ok.