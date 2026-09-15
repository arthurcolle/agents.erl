%% openai_generator_sup.erl
%% Supervisor for the API client module generator services
-module(openai_generator_sup).
-behaviour(supervisor).

-export([
    start_link/1,
    init/1,
    generate_client/2
]).

-define(SERVER, ?MODULE).

%% Start the generator supervisor
start_link(Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Options).

%% Generate a client module and notify the clients supervisor to start it
generate_client(ApiGroup, OutputDir) ->
    % First, check if the module already exists
    case openai_sup:resolve_client_module(ApiGroup) of
        {ok, ModuleName} ->
            case code:is_loaded(ModuleName) of
                {file, _} ->
                    % Module is already loaded
                    {ok, already_loaded};
                false ->
                    % Generate the module
                    gen_server:call(openai_generator, {generate_client, ApiGroup, OutputDir})
            end;
        Error ->
            Error
    end.

%% Supervisor callback
init(Options) ->
    % Supervisor flags
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 5
    },
    
    % Define children specs
    ChildSpecs = [
        % Module generator service
        #{
            id => openai_generator,
            start => {openai_generator, start_link, [Options]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [openai_generator]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
