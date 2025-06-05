-module(mcp_env_checker).
-export([check_and_fix_servers/0, check_server_requirements/1]).

%% Record definition from mcp_server_config
-record(mcp_server, {
    id :: binary(),
    name :: binary(),
    category :: binary(),
    url :: binary(),
    auth_type :: oauth2 | api_key | open,
    maintainer :: binary(),
    description :: binary(),
    capabilities :: list(),
    status :: active | inactive,
    last_checked :: calendar:datetime(),
    metadata :: map()
}).

%% Check all registered MCP servers and fix missing environment variables
check_and_fix_servers() ->
    %% First try to load from API key manager
    try
        api_key_manager:load_from_env()
    catch
        _:_ -> ok
    end,
    
    Servers = mcp_server_config:get_all_servers(),
    Results = lists:map(fun(Server) ->
        {Server#mcp_server.id, check_server_requirements(Server)}
    end, Servers),
    
    %% Log results
    lists:foreach(fun({Id, Result}) ->
        case Result of
            {ok, _} -> 
                colored_logger:success("MCP Server ~s: All requirements met", [Id]);
            {missing_env, Vars} ->
                colored_logger:warning("MCP Server ~s: Missing environment variables: ~p", [Id, Vars]),
                %% Try to load from API key manager first
                case load_from_api_manager(Id, Vars) of
                    ok -> 
                        colored_logger:info("Loaded keys from API manager for ~s", [Id]);
                    {error, _} ->
                        %% Fall back to dummy values
                        set_dummy_env_vars(Id, Vars)
                end;
            {error, Reason} ->
                colored_logger:error("MCP Server ~s: Error - ~p", [Id, Reason])
        end
    end, Results),
    
    Results.

%% Check specific server requirements
check_server_requirements(#mcp_server{id = <<"slack_mcp">>, auth_type = oauth2}) ->
    Required = [{"SLACK_BOT_TOKEN", "xoxb-dummy-token"}, 
                {"SLACK_TEAM_ID", "T00000000"}],
    check_env_vars(Required);

check_server_requirements(#mcp_server{id = <<"linear">>, auth_type = oauth2}) ->
    Required = [{"LINEAR_API_KEY", "lin_api_dummy_key"}],
    check_env_vars(Required);

check_server_requirements(#mcp_server{id = <<"plaid">>, auth_type = api_key}) ->
    Required = [{"PLAID_CLIENT_ID", "dummy_client_id"},
                {"PLAID_SECRET", "dummy_secret"},
                {"PLAID_ENV", "sandbox"}],
    check_env_vars(Required);

check_server_requirements(#mcp_server{id = <<"graphlit">>, auth_type = api_key}) ->
    Required = [{"GRAPHLIT_API_KEY", "dummy_graphlit_key"}],
    check_env_vars(Required);

check_server_requirements(#mcp_server{id = <<"github_mcp">>, auth_type = api_key}) ->
    Required = [{"GITHUB_TOKEN", "ghp_dummy_token"}],
    check_env_vars(Required);

check_server_requirements(#mcp_server{auth_type = open}) ->
    {ok, no_requirements};

check_server_requirements(_) ->
    {ok, unknown_requirements}.

%% Check if environment variables exist
check_env_vars(VarPairs) ->
    Missing = lists:filtermap(fun({Var, _Default}) ->
        case os:getenv(Var) of
            false -> {true, Var};
            _ -> false
        end
    end, VarPairs),
    
    case Missing of
        [] -> {ok, all_present};
        _ -> {missing_env, Missing}
    end.

%% Set dummy environment variables for development/testing
set_dummy_env_vars(ServerId, MissingVars) ->
    %% Get the default values for this server
    Defaults = get_server_defaults(ServerId),
    
    lists:foreach(fun(Var) ->
        case lists:keyfind(Var, 1, Defaults) of
            {Var, Default} ->
                os:putenv(Var, Default),
                colored_logger:info("Set dummy value for ~s: ~s", [Var, Default]);
            false ->
                colored_logger:warning("No default value for ~s", [Var])
        end
    end, MissingVars).

%% Get default values for each server
get_server_defaults(<<"slack_mcp">>) ->
    [{"SLACK_BOT_TOKEN", "xoxb-dummy-token"},
     {"SLACK_TEAM_ID", "T00000000"}];

get_server_defaults(<<"linear">>) ->
    [{"LINEAR_API_KEY", "lin_api_dummy_key"}];

get_server_defaults(<<"plaid">>) ->
    [{"PLAID_CLIENT_ID", "dummy_client_id"},
     {"PLAID_SECRET", "dummy_secret"},
     {"PLAID_ENV", "sandbox"}];

get_server_defaults(<<"graphlit">>) ->
    [{"GRAPHLIT_API_KEY", "dummy_graphlit_key"}];

get_server_defaults(<<"github_mcp">>) ->
    [{"GITHUB_TOKEN", "ghp_dummy_token"}];

get_server_defaults(_) ->
    [].

%% Load keys from API manager
load_from_api_manager(ServerId, MissingVars) ->
    try
        %% Convert server ID to atom for API manager
        ServiceAtom = case ServerId of
            <<"slack_mcp">> -> slack;
            <<"linear">> -> linear;
            <<"plaid">> -> plaid;
            <<"graphlit">> -> graphlit;
            <<"github_mcp">> -> github;
            _ -> binary_to_atom(ServerId, utf8)
        end,
        
        %% Get field mappings
        FieldMappings = get_field_mappings(ServiceAtom),
        
        %% Try to get each missing variable from API manager
        lists:foreach(fun(Var) ->
            case lists:keyfind(Var, 1, FieldMappings) of
                {Var, Field} ->
                    case api_key_manager:get_key(ServiceAtom, Field) of
                        {ok, Value} when is_binary(Value) ->
                            os:putenv(Var, binary_to_list(Value));
                        _ -> ok
                    end;
                false -> ok
            end
        end, MissingVars),
        
        ok
    catch
        _:_ -> {error, failed}
    end.

%% Map environment variables to API manager fields
get_field_mappings(slack) ->
    [{"SLACK_BOT_TOKEN", bot_token},
     {"SLACK_TEAM_ID", team_id},
     {"SLACK_APP_ID", app_id},
     {"SLACK_CLIENT_ID", client_id},
     {"SLACK_CLIENT_SECRET", client_secret},
     {"SLACK_SIGNING_SECRET", signing_secret}];
get_field_mappings(linear) ->
    [{"LINEAR_API_KEY", api_key}];
get_field_mappings(plaid) ->
    [{"PLAID_CLIENT_ID", client_id},
     {"PLAID_SECRET", secret},
     {"PLAID_ENV", environment}];
get_field_mappings(graphlit) ->
    [{"GRAPHLIT_API_KEY", api_key}];
get_field_mappings(github) ->
    [{"GITHUB_TOKEN", token}];
get_field_mappings(_) ->
    [].