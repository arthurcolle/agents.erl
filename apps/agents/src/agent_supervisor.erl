%% agent_supervisor.erl
%% Supervisor for individual agent instances
-module(agent_supervisor).
-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_agent/1,
    stop_agent/1
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% API Functions

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Start a new agent with configuration
start_agent(Config) ->
    ChildSpec = #{
        id => make_ref(),
        start => {agent_instance, start_link, [Config]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [agent_instance]
    },
    supervisor:start_child(?SERVER, ChildSpec).

%% Stop an agent
stop_agent(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

%% Supervisor callback
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    ChildSpecs = [],
    
    {ok, {SupFlags, ChildSpecs}}.