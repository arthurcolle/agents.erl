%%%-------------------------------------------------------------------
%% @doc myapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(myapp_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [
        % Agent registry
        #{
            id => agent_registry,
            start => {agent_registry, start_link, [#{}]},
            restart => permanent,
            type => worker
        },
        % Agent discovery
        #{
            id => agent_discovery,
            start => {agent_discovery, start_link, [#{}]},
            restart => permanent,
            type => worker
        },
        % Agent tools
        #{
            id => agent_tools,
            start => {agent_tools, start_link, [#{}]},
            restart => permanent,
            type => worker
        },
        % Agent messenger
        #{
            id => agent_messenger,
            start => {agent_messenger, start_link, [#{}]},
            restart => permanent,
            type => worker
        },
        % Agent supervisor
        #{
            id => agent,
            start => {agent, start_link, []},
            restart => permanent,
            type => supervisor
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
