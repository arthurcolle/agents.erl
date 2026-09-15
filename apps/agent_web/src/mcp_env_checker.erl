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

%% Check all registered MCP servers.
%%
%% This module is deliberately read-mostly.  Missing credentials are a normal
%% deployment state for optional integrations and must never be replaced with
%% synthetic values or written to the VM environment.
check_and_fix_servers() ->
    %% First try to load real values from the API key manager.  The manager is
    %% allowed to be unavailable during a partial application start.
    try
        api_key_manager:load_from_env()
    catch
        _:_ -> ok
    end,

    case mcp_server_config:get_all_servers() of
        {ok, Servers} when is_list(Servers) ->
            Results = lists:map(fun(Server) ->
                Id = Server#mcp_server.id,
                Result = check_server_requirements(Server),
                FinalResult = case Result of
                    {missing_env, Vars} ->
                        %% Existing, user-supplied values may live in the
                        %% manager's encrypted/configured store.  Copy only
                        %% real values, then verify; never invent a value.
                        _ = load_from_api_manager(Id, Vars),
                        check_server_requirements(Server);
                    _ ->
                        Result
                end,
                {Id, FinalResult}
            end, Servers),
            log_results(Results),
            {ok, Results};
        {error, Reason} ->
            colored_logger:error("Unable to inspect MCP server configuration: ~p", [Reason]),
            {error, {server_config_unavailable, Reason}};
        Other ->
            colored_logger:error("Invalid MCP server configuration response: ~p", [Other]),
            {error, {invalid_server_config_response, Other}}
    end.

%% Check specific server requirements
check_server_requirements(#mcp_server{id = <<"slack_mcp">>, auth_type = oauth2}) ->
    Required = ["SLACK_BOT_TOKEN", "SLACK_TEAM_ID"],
    check_env_vars(Required);

check_server_requirements(#mcp_server{id = <<"linear">>, auth_type = oauth2}) ->
    Required = ["LINEAR_API_KEY"],
    check_env_vars(Required);

check_server_requirements(#mcp_server{id = <<"plaid">>, auth_type = api_key}) ->
    Required = ["PLAID_CLIENT_ID", "PLAID_SECRET"],
    check_env_vars(Required);

check_server_requirements(#mcp_server{auth_type = open}) ->
    {ok, no_requirements};

check_server_requirements(#mcp_server{metadata = Metadata}) when is_map(Metadata) ->
    Required = maps:get(<<"required_env">>, Metadata,
        maps:get(required_env, Metadata, [])),
    case normalize_env_names(Required) of
        [] -> {ok, unknown_requirements};
        Names -> check_env_vars(Names)
    end;

check_server_requirements(_) -> {ok, unknown_requirements}.

%% Check if environment variables exist
check_env_vars(VarPairs) ->
    Missing = lists:filtermap(fun(VarPair) ->
        Var = case VarPair of
            {Name, _Default} -> Name;
            Name -> Name
        end,
        case os:getenv(Var) of
            false -> {true, Var};
            [] -> {true, Var};
            _ -> false
        end
    end, VarPairs),
    
    case Missing of
        [] -> {ok, all_present};
        _ -> {missing_env, Missing}
    end.

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
            _ -> undefined
        end,

        case ServiceAtom of
            undefined -> {error, unsupported_service};
            _ -> load_from_api_manager_values(ServiceAtom, MissingVars)
        end
    catch
        _:_ -> {error, failed}
    end.

load_from_api_manager_values(ServiceAtom, MissingVars) ->
    try
        
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

normalize_env_names(Names) when is_list(Names) ->
    [Name || Name <- [normalize_env_name(Value) || Value <- Names], Name =/= undefined];
normalize_env_names(_) -> [].

normalize_env_name(Name) when is_binary(Name) -> binary_to_list(Name);
normalize_env_name(Name) when is_list(Name) -> Name;
normalize_env_name(_) -> undefined.

log_results(Results) ->
    lists:foreach(fun({Id, Result}) ->
        case Result of
            {ok, _} ->
                colored_logger:success("MCP Server ~s: requirements met", [Id]);
            {missing_env, Vars} ->
                %% Names are safe to report; values are intentionally never
                %% logged or synthesized.
                colored_logger:warning(
                    "MCP Server ~s: credentials unavailable for ~p",
                    [Id, Vars]
                );
            {error, Reason} ->
                colored_logger:error("MCP Server ~s: Error - ~p", [Id, Reason])
        end
    end, Results).

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
