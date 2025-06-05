%% openai_config.erl
%% Module for managing configuration for OpenAI API clients
-module(openai_config).
-behaviour(gen_server).

-export([
    start_link/1,
    get_api_key/0,
    set_api_key/1,
    get_organization/0,
    set_organization/1,
    get_config/1,
    get_all_config/0,
    set_config/2,
    get_available_models/0,
    get_model_info/1,
    validate_model/1,
    get_models_by_category/1,
    get_models_by_capability/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_CONFIG, #{
    api_key => undefined,
    organization => undefined,
    base_url => "https://api.openai.com/v1",
    timeout => 30000,  % 30 seconds
    retry_count => 3,
    retry_delay => 1000 % 1 second
}).

-record(state, {
    config = ?DEFAULT_CONFIG :: map()
}).

%% API Functions
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Get the API key
get_api_key() ->
    gen_server:call(?SERVER, {get_config, api_key}).

%% Set the API key
set_api_key(ApiKey) ->
    gen_server:call(?SERVER, {set_config, api_key, ApiKey}).

%% Get the organization ID
get_organization() ->
    gen_server:call(?SERVER, {get_config, organization}).

%% Set the organization ID
set_organization(Organization) ->
    gen_server:call(?SERVER, {set_config, organization, Organization}).

%% Get a specific configuration value
get_config(Key) ->
    gen_server:call(?SERVER, {get_config, Key}).

%% Get all configuration
get_all_config() ->
    gen_server:call(?SERVER, get_all_config).

%% Set a specific configuration value
set_config(Key, Value) ->
    gen_server:call(?SERVER, {set_config, Key, Value}).

%% Get available models
get_available_models() ->
    model_registry:get_all_models().

%% Get information about a specific model
get_model_info(ModelId) ->
    model_registry:get_model_info(ModelId).

%% Validate if a model exists
validate_model(ModelId) ->
    model_registry:validate_model(ModelId).

%% Get models by category
get_models_by_category(Category) ->
    model_registry:get_models_by_category(Category).

%% Get models by capability
get_models_by_capability(Capability) ->
    model_registry:get_models_by_capability(Capability).

%% gen_server callbacks
init(Options) ->
    % Initialize with default config and override with provided options
    Config = maps:merge(?DEFAULT_CONFIG, Options),
    
    % Check environment variables if config values are undefined
    ConfigWithEnv = apply_env_vars(Config),
    
    {ok, #state{config = ConfigWithEnv}}.

handle_call({get_config, Key}, _From, State) ->
    Value = maps:get(Key, State#state.config, undefined),
    {reply, Value, State};

handle_call(get_all_config, _From, State) ->
    {reply, State#state.config, State};

handle_call({set_config, Key, Value}, _From, State) ->
    NewConfig = maps:put(Key, Value, State#state.config),
    
    % Notify clients of config change
    notify_config_change(Key, Value),
    
    {reply, ok, State#state{config = NewConfig}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

%% Apply environment variables to config if values are undefined
apply_env_vars(Config) ->
    % Check for API key in environment
    ConfigWithApiKey = case maps:get(api_key, Config) of
        undefined ->
            case os:getenv("OPENAI_API_KEY") of
                false -> Config;
                ApiKey -> maps:put(api_key, ApiKey, Config)
            end;
        _ ->
            Config
    end,
    
    % Check for organization in environment
    ConfigWithOrg = case maps:get(organization, ConfigWithApiKey) of
        undefined ->
            case os:getenv("OPENAI_ORGANIZATION") of
                false -> ConfigWithApiKey;
                Org -> maps:put(organization, Org, ConfigWithApiKey)
            end;
        _ ->
            ConfigWithApiKey
    end,
    
    ConfigWithOrg.

%% Notify all clients of config change
notify_config_change(Key, Value) ->
    % Get all active clients
    Clients = try openai_clients_sup:list_active_clients() of
        ClientList -> ClientList
    catch
        _:_ -> []
    end,
    
    % Send config update to each client
    lists:foreach(
        fun(Client) ->
            gen_server:cast(Client, {config_update, Key, Value})
        end,
        Clients
    ).