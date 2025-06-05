-module(api_key_manager).
-behaviour(gen_server).

-export([start_link/0, get_key/1, get_key/2, set_key/2, save_key/2, 
         get_all_keys/0, check_required_keys/0, load_from_env/0,
         get_api_requirements/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    keys = #{} :: map(),
    config_file :: string()
}).

%% API Requirements Definition
-define(API_REQUIREMENTS, #{
    openai => #{
        name => <<"OpenAI">>,
        required_fields => [
            #{key => api_key, 
              env => "OPENAI_API_KEY", 
              description => <<"API key for OpenAI (GPT models)">>,
              pattern => <<"sk-">>}
        ],
        optional_fields => [
            #{key => organization, 
              env => "OPENAI_ORG_ID",
              description => <<"OpenAI Organization ID">>}
        ],
        documentation_url => <<"https://platform.openai.com/api-keys">>
    },
    anthropic => #{
        name => <<"Anthropic">>,
        required_fields => [
            #{key => api_key,
              env => "ANTHROPIC_API_KEY",
              description => <<"API key for Anthropic (Claude models)">>,
              pattern => <<"sk-ant-">>}
        ],
        documentation_url => <<"https://console.anthropic.com/account/keys">>
    },
    slack => #{
        name => <<"Slack">>,
        required_fields => [
            #{key => bot_token,
              env => "SLACK_BOT_TOKEN",
              description => <<"Bot User OAuth Token">>,
              pattern => <<"xoxb-">>},
            #{key => team_id,
              env => "SLACK_TEAM_ID",
              description => <<"Slack Team/Workspace ID">>,
              pattern => <<"T">>}
        ],
        optional_fields => [
            #{key => app_id, env => "SLACK_APP_ID"},
            #{key => client_id, env => "SLACK_CLIENT_ID"},
            #{key => client_secret, env => "SLACK_CLIENT_SECRET"},
            #{key => signing_secret, env => "SLACK_SIGNING_SECRET"}
        ],
        documentation_url => <<"https://api.slack.com/apps">>
    },
    linear => #{
        name => <<"Linear">>,
        required_fields => [
            #{key => api_key,
              env => "LINEAR_API_KEY",
              description => <<"Linear API Key">>,
              pattern => <<"lin_api_">>}
        ],
        documentation_url => <<"https://linear.app/settings/api">>
    },
    plaid => #{
        name => <<"Plaid">>,
        required_fields => [
            #{key => client_id,
              env => "PLAID_CLIENT_ID",
              description => <<"Plaid Client ID">>},
            #{key => secret,
              env => "PLAID_SECRET",
              description => <<"Plaid Secret Key">>}
        ],
        optional_fields => [
            #{key => environment,
              env => "PLAID_ENV",
              description => <<"Plaid Environment (sandbox/development/production)">>,
              default => <<"sandbox">>}
        ],
        documentation_url => <<"https://dashboard.plaid.com/developers/keys">>
    },
    graphlit => #{
        name => <<"Graphlit">>,
        required_fields => [
            #{key => api_key,
              env => "GRAPHLIT_API_KEY",
              description => <<"Graphlit API Key">>}
        ],
        documentation_url => <<"https://graphlit.com/docs">>
    },
    github => #{
        name => <<"GitHub">>,
        required_fields => [
            #{key => token,
              env => "GITHUB_TOKEN",
              description => <<"GitHub Personal Access Token">>,
              pattern => <<"ghp_">>}
        ],
        documentation_url => <<"https://github.com/settings/tokens">>
    },
    jina => #{
        name => <<"Jina AI">>,
        required_fields => [
            #{key => api_key,
              env => "JINA_API_KEY",
              description => <<"Jina AI API Key">>,
              pattern => <<"jina_">>}
        ],
        documentation_url => <<"https://jina.ai/api">>
    }
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_key(Service) ->
    get_key(Service, api_key).

get_key(Service, Field) ->
    gen_server:call(?MODULE, {get_key, Service, Field}).

set_key(Service, Value) when is_map(Value) ->
    gen_server:call(?MODULE, {set_keys, Service, Value});
set_key(Service, Value) ->
    gen_server:call(?MODULE, {set_key, Service, api_key, Value}).

save_key(Service, KeyData) ->
    gen_server:call(?MODULE, {save_key, Service, KeyData}).

get_all_keys() ->
    gen_server:call(?MODULE, get_all_keys).

check_required_keys() ->
    gen_server:call(?MODULE, check_required_keys).

get_api_requirements() ->
    ?API_REQUIREMENTS.

load_from_env() ->
    gen_server:call(?MODULE, load_from_env).

%% gen_server callbacks
init([]) ->
    ConfigFile = filename:join([code:priv_dir(agent_web), "..", "..", "config", "api_keys.config"]),
    State = #state{config_file = ConfigFile},
    
    %% Load from config file
    NewState = load_config(State),
    
    %% Override with environment variables
    FinalState = load_env_vars(NewState),
    
    %% Schedule periodic save
    timer:send_interval(300000, save_config),  % Save every 5 minutes
    
    {ok, FinalState}.

handle_call({get_key, Service, Field}, _From, State) ->
    Reply = case maps:get(Service, State#state.keys, undefined) of
        undefined -> {error, service_not_found};
        ServiceKeys -> 
            case maps:get(Field, ServiceKeys, undefined) of
                undefined -> {error, key_not_found};
                <<>> -> {error, key_not_set};
                "" -> {error, key_not_set};
                Value -> {ok, Value}
            end
    end,
    {reply, Reply, State};

handle_call({set_key, Service, Field, Value}, _From, State) ->
    ServiceKeys = maps:get(Service, State#state.keys, #{}),
    UpdatedServiceKeys = maps:put(Field, Value, ServiceKeys),
    NewKeys = maps:put(Service, UpdatedServiceKeys, State#state.keys),
    NewState = State#state{keys = NewKeys},
    {reply, ok, NewState};

handle_call({set_keys, Service, KeyMap}, _From, State) ->
    ServiceKeys = maps:get(Service, State#state.keys, #{}),
    UpdatedServiceKeys = maps:merge(ServiceKeys, KeyMap),
    NewKeys = maps:put(Service, UpdatedServiceKeys, State#state.keys),
    NewState = State#state{keys = NewKeys},
    {reply, ok, NewState};

handle_call({save_key, Service, KeyData}, _From, State) ->
    NewState = case save_service_config(Service, KeyData, State) of
        {ok, UpdatedState} -> UpdatedState;
        {error, _} -> State
    end,
    {reply, ok, NewState};

handle_call(get_all_keys, _From, State) ->
    %% Return sanitized keys (mask sensitive data)
    SanitizedKeys = maps:map(fun(_Service, ServiceKeys) ->
        maps:map(fun(Field, Value) ->
            case is_sensitive_field(Field) of
                true -> mask_value(Value);
                false -> Value
            end
        end, ServiceKeys)
    end, State#state.keys),
    {reply, {ok, SanitizedKeys}, State};

handle_call(check_required_keys, _From, State) ->
    Missing = check_missing_keys(State#state.keys),
    {reply, {ok, Missing}, State};

handle_call(load_from_env, _From, State) ->
    NewState = load_env_vars(State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(save_config, State) ->
    save_config_file(State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    save_config_file(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
load_config(State) ->
    case file:consult(State#state.config_file) of
        {ok, [Config]} ->
            Keys = lists:foldl(fun({Service, Fields}, Acc) ->
                ServiceMap = lists:foldl(fun({Key, Value}, SAcc) ->
                    maps:put(Key, to_binary(Value), SAcc)
                end, #{}, Fields),
                maps:put(Service, ServiceMap, Acc)
            end, #{}, Config),
            State#state{keys = Keys};
        {error, enoent} ->
            %% File doesn't exist, create it
            save_config_file(State),
            State;
        {error, Reason} ->
            colored_logger:warning("Failed to load API config: ~p", [Reason]),
            State
    end.

load_env_vars(State) ->
    maps:fold(fun(Service, Requirements, AccState) ->
        RequiredFields = maps:get(required_fields, Requirements, []),
        OptionalFields = maps:get(optional_fields, Requirements, []),
        AllFields = RequiredFields ++ OptionalFields,
        
        lists:foldl(fun(FieldSpec, InnerState) ->
            EnvVar = maps:get(env, FieldSpec),
            FieldKey = maps:get(key, FieldSpec),
            
            case os:getenv(EnvVar) of
                false -> InnerState;
                Value when Value =/= "" ->
                    ServiceKeys = maps:get(Service, InnerState#state.keys, #{}),
                    UpdatedServiceKeys = maps:put(FieldKey, list_to_binary(Value), ServiceKeys),
                    NewKeys = maps:put(Service, UpdatedServiceKeys, InnerState#state.keys),
                    InnerState#state{keys = NewKeys}
            end
        end, AccState, AllFields)
    end, State, ?API_REQUIREMENTS).

save_config_file(State) ->
    Config = maps:fold(fun(Service, ServiceKeys, Acc) ->
        Fields = maps:fold(fun(Key, Value, FAcc) ->
            [{Key, binary_to_list(Value)} | FAcc]
        end, [], ServiceKeys),
        [{Service, Fields} | Acc]
    end, [], State#state.keys),
    
    ConfigStr = io_lib:format("%% API Keys Configuration File~n"
                             "%% This file contains sensitive API keys and credentials~n"
                             "%% DO NOT COMMIT THIS FILE TO VERSION CONTROL~n~n"
                             "~p.", [Config]),
    
    case file:write_file(State#state.config_file, ConfigStr) of
        ok -> ok;
        {error, Reason} ->
            colored_logger:error("Failed to save API config: ~p", [Reason])
    end.

save_service_config(Service, KeyData, State) ->
    try
        ValidatedData = validate_service_data(Service, KeyData),
        ServiceKeys = maps:get(Service, State#state.keys, #{}),
        UpdatedServiceKeys = maps:merge(ServiceKeys, ValidatedData),
        NewKeys = maps:put(Service, UpdatedServiceKeys, State#state.keys),
        NewState = State#state{keys = NewKeys},
        save_config_file(NewState),
        {ok, NewState}
    catch
        error:Reason ->
            {error, Reason}
    end.

validate_service_data(Service, KeyData) ->
    case maps:get(Service, ?API_REQUIREMENTS, undefined) of
        undefined -> throw({unknown_service, Service});
        Requirements ->
            RequiredFields = maps:get(required_fields, Requirements, []),
            
            %% Validate required fields
            lists:foreach(fun(FieldSpec) ->
                FieldKey = maps:get(key, FieldSpec),
                case maps:get(atom_to_binary(FieldKey, utf8), KeyData, undefined) of
                    undefined -> throw({missing_required_field, FieldKey});
                    <<>> -> throw({empty_required_field, FieldKey});
                    Value ->
                        case maps:get(pattern, FieldSpec, undefined) of
                            undefined -> ok;
                            Pattern ->
                                case binary:match(Value, Pattern) of
                                    {0, _} -> ok;
                                    _ -> throw({invalid_format, FieldKey, Pattern})
                                end
                        end
                end
            end, RequiredFields),
            
            KeyData
    end.

check_missing_keys(Keys) ->
    maps:fold(fun(Service, Requirements, Acc) ->
        RequiredFields = maps:get(required_fields, Requirements, []),
        ServiceKeys = maps:get(Service, Keys, #{}),
        
        Missing = lists:filtermap(fun(FieldSpec) ->
            FieldKey = maps:get(key, FieldSpec),
            case maps:get(FieldKey, ServiceKeys, <<>>) of
                <<>> -> {true, FieldKey};
                _ -> false
            end
        end, RequiredFields),
        
        case Missing of
            [] -> Acc;
            _ -> [{Service, Missing} | Acc]
        end
    end, [], ?API_REQUIREMENTS).

is_sensitive_field(Field) ->
    lists:member(Field, [api_key, secret, client_secret, token, 
                        signing_secret, bot_token, password]).

mask_value(Value) when is_binary(Value) ->
    case byte_size(Value) of
        0 -> <<"(not set)">>;
        Len when Len =< 8 -> <<"****">>;
        Len ->
            Prefix = binary:part(Value, 0, 4),
            <<Prefix/binary, "...", "****">>
    end;
mask_value(_) -> <<"(not set)">>.

to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_list(Value) -> list_to_binary(Value);
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
to_binary(_) -> <<>>.