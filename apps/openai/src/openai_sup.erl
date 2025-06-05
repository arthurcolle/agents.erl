%% openai_sup.erl
%% Top-level supervisor for the OpenAI API client
-module(openai_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    start_link/1,
    init/1,
    start_api_client/2,
    stop_api_client/1
]).

-define(SERVER, ?MODULE).

%% Start the top-level supervisor
start_link() ->
    start_link(#{}).

start_link(Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Options).

%% Dynamically start a new API client under supervision
start_api_client(ApiGroup, Options) ->
    ModuleName = list_to_atom("openai_" ++ atom_to_list(ApiGroup)),
    ChildSpec = #{
        id => ModuleName,
        start => {ModuleName, start_link, [Options]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [ModuleName]
    },
    supervisor:start_child(?SERVER, ChildSpec).

%% Dynamically stop an API client
stop_api_client(ApiGroup) ->
    ModuleName = list_to_atom("openai_" ++ atom_to_list(ApiGroup)),
    supervisor:terminate_child(?SERVER, ModuleName),
    supervisor:delete_child(?SERVER, ModuleName).

%% Supervisor callback
init(Options) ->
    % Supervisor flags
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    % Initialize library dependencies
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    
    % Define top-level children specs
    ChildSpecs = [
        % Config manager process (start first)
        #{
            id => openai_config,
            start => {openai_config, start_link, [Options]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [openai_config]
        },
        
        % Cost tracking service
        #{
            id => cost_tracker,
            start => {cost_tracker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [cost_tracker]
        },
        
        % Request rate limiter service
        #{
            id => openai_rate_limiter,
            start => {openai_rate_limiter, start_link, [Options]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [openai_rate_limiter]
        },
        
        % API generator supervisor
        #{
            id => openai_generator_sup,
            start => {openai_generator_sup, start_link, [Options]},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [openai_generator_sup]
        },
        
        % API clients supervisor
        #{
            id => openai_clients_sup,
            start => {openai_clients_sup, start_link, [Options]},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [openai_clients_sup]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.