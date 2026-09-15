%% openai_clients_sup.erl
%% Supervisor for all the individual API client processes
-module(openai_clients_sup).
-behaviour(supervisor).

-export([
    start_link/1,
    init/1,
    start_client/2,
    stop_client/1,
    start_all_clients/1,
    list_active_clients/0
]).

-define(SERVER, ?MODULE).

%% Start the clients supervisor
start_link(Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Options).

%% Start a specific API client
start_client(ApiGroup, Options) ->
    case openai_sup:resolve_client_module(ApiGroup) of
        {ok, ModuleName} ->
            start_client_module(ModuleName, Options);
        Error ->
            Error
    end.

%% Stop a specific API client
stop_client(ApiGroup) ->
    case openai_sup:resolve_client_module(ApiGroup) of
        {ok, ModuleName} ->
            case supervisor:terminate_child(?SERVER, ModuleName) of
                ok -> supervisor:delete_child(?SERVER, ModuleName);
                {error, _} = Error -> Error
            end;
        Error ->
            Error
    end.

%% Start all API clients
start_all_clients(Options) ->
    ApiGroups = openai_api_structure:get_api_groups(),
    Results = lists:map(
        fun(ApiGroup) ->
            {ApiGroup, start_client(ApiGroup, Options)}
        end,
        ApiGroups
    ),
    {ok, Results}.

%% List all active clients
list_active_clients() ->
    [Id || {Id, _, _, _} <- supervisor:which_children(?SERVER)].

%% Supervisor callback
init(Options) ->
    % Supervisor flags - use simple_one_for_one for dynamic children
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    % Optionally start predefined clients
    AutoStartGroups = maps:get(auto_start_groups, Options, []),
    ChildSpecs = lists:filtermap(
        fun(ApiGroup) ->
            case openai_sup:resolve_client_module(ApiGroup) of
                {ok, ModuleName} ->
                    {true, client_spec(ModuleName, Options)};
                {error, _} ->
                    false
            end
        end,
        AutoStartGroups
    ),
    
    {ok, {SupFlags, ChildSpecs}}.

start_client_module(ModuleName, Options) ->
    case code:ensure_loaded(ModuleName) of
        {module, _} ->
            supervisor:start_child(?SERVER, client_spec(ModuleName, Options));
        {error, Reason} ->
            {error, {client_module_not_loaded, ModuleName, Reason}}
    end.

client_spec(ModuleName, Options) ->
    #{
        id => ModuleName,
        start => {ModuleName, start_link, [Options]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [ModuleName]
    }.
