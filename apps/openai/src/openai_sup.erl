%% openai_sup.erl
%% Top-level supervisor for the OpenAI API client
-module(openai_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    start_link/1,
    init/1,
    start_api_client/2,
    stop_api_client/1,
    resolve_client_module/1
]).

-define(SERVER, ?MODULE).

%% Start the top-level supervisor
start_link() ->
    start_link(#{}).

start_link(Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Options).

%% Dynamically start a new API client under supervision
start_api_client(ApiGroup, Options) ->
    case resolve_client_module(ApiGroup) of
        {ok, ModuleName} ->
            start_client_module(ModuleName, Options);
        Error ->
            Error
    end.

%% Dynamically stop an API client
stop_api_client(ApiGroup) ->
    case resolve_client_module(ApiGroup) of
        {ok, ModuleName} ->
            case supervisor:terminate_child(?SERVER, ModuleName) of
                ok -> supervisor:delete_child(?SERVER, ModuleName);
                {error, not_found} = Error -> Error;
                {error, _} = Error -> Error
            end;
        Error ->
            Error
    end.

resolve_client_module(models) -> {ok, openai_models};
resolve_client_module(completions) -> {ok, openai_completions};
resolve_client_module(chat) -> {ok, openai_chat};
resolve_client_module(embeddings) -> {ok, openai_embeddings};
resolve_client_module(files) -> {ok, openai_files};
resolve_client_module(fine_tuning) -> {ok, openai_fine_tuning};
resolve_client_module(images) -> {ok, openai_images};
resolve_client_module(audio) -> {ok, openai_audio};
resolve_client_module(moderations) -> {ok, openai_moderations};
resolve_client_module(assistants) -> {ok, openai_assistants};
resolve_client_module(threads) -> {ok, openai_threads};
resolve_client_module(messages) -> {ok, openai_messages};
resolve_client_module(runs) -> {ok, openai_runs};
resolve_client_module(batch) -> {ok, openai_batch};
resolve_client_module(responses) -> {ok, openai_responses};
resolve_client_module(<<"models">>) -> {ok, openai_models};
resolve_client_module(<<"completions">>) -> {ok, openai_completions};
resolve_client_module(<<"chat">>) -> {ok, openai_chat};
resolve_client_module(<<"embeddings">>) -> {ok, openai_embeddings};
resolve_client_module(<<"files">>) -> {ok, openai_files};
resolve_client_module(<<"fine_tuning">>) -> {ok, openai_fine_tuning};
resolve_client_module(<<"images">>) -> {ok, openai_images};
resolve_client_module(<<"audio">>) -> {ok, openai_audio};
resolve_client_module(<<"moderations">>) -> {ok, openai_moderations};
resolve_client_module(<<"assistants">>) -> {ok, openai_assistants};
resolve_client_module(<<"threads">>) -> {ok, openai_threads};
resolve_client_module(<<"messages">>) -> {ok, openai_messages};
resolve_client_module(<<"runs">>) -> {ok, openai_runs};
resolve_client_module(<<"batch">>) -> {ok, openai_batch};
resolve_client_module(<<"responses">>) -> {ok, openai_responses};
resolve_client_module(_) -> {error, invalid_api_group}.

start_client_module(ModuleName, Options) ->
    case code:ensure_loaded(ModuleName) of
        {module, _} ->
            ChildSpec = #{
                id => ModuleName,
                start => {ModuleName, start_link, [Options]},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [ModuleName]
            },
            supervisor:start_child(?SERVER, ChildSpec);
        {error, Reason} ->
            {error, {client_module_not_loaded, ModuleName, Reason}}
    end.

%% Supervisor callback
init(Options) ->
    % Supervisor flags
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
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
