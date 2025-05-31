%%%-------------------------------------------------------------------
%%% @doc
%%% Claude OAuth Adapter
%%% Provides seamless OAuth integration for Claude's MCP connection format
%%% Ensures perfect compatibility with Claude's authentication expectations
%%% @end
%%%-------------------------------------------------------------------
-module(claude_oauth_adapter).

-export([get_claude_mcp_config/0, update_server_with_auth/2, 
         validate_claude_auth/1, get_auth_status_for_claude/0]).

-include_lib("stdlib/include/qlc.hrl").

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

-record(oauth_token, {
    provider :: binary(),
    user_id :: binary(),
    access_token :: binary(),
    refresh_token :: binary(),
    expires_at :: calendar:datetime(),
    scopes :: [binary()],
    metadata :: map()
}).

%%%===================================================================
%%% Claude MCP Configuration Generation
%%%===================================================================

%% Generate Claude-compatible MCP configuration with OAuth tokens
get_claude_mcp_config() ->
    case get_authenticated_servers() of
        {ok, Servers} ->
            ClaudeConfig = #{
                <<"mcpServers">> => format_servers_for_claude(Servers)
            },
            {ok, ClaudeConfig};
        {error, Reason} ->
            {error, Reason}
    end.

%% Update an MCP server configuration with authentication data
update_server_with_auth(ServerId, AuthData) ->
    case mcp_server_config:get_server(ServerId) of
        {ok, Server} ->
            UpdatedMetadata = add_auth_to_metadata(Server#mcp_server.metadata, AuthData),
            Updates = #{metadata => UpdatedMetadata},
            mcp_server_config:update_server(ServerId, Updates);
        Error ->
            Error
    end.

%% Validate authentication for Claude compatibility
validate_claude_auth(Servers) when is_list(Servers) ->
    lists:map(fun validate_server_auth/1, Servers).

%% Get overall authentication status for Claude
get_auth_status_for_claude() ->
    case get_authenticated_servers() of
        {ok, Servers} ->
            TotalServers = length(Servers),
            AuthenticatedServers = length([S || S <- Servers, is_server_authenticated(S)]),
            #{
                total_servers => TotalServers,
                authenticated_servers => AuthenticatedServers,
                authentication_rate => case TotalServers of
                    0 -> 0.0;
                    _ -> AuthenticatedServers / TotalServers
                end,
                status => case AuthenticatedServers == TotalServers of
                    true -> <<"fully_authenticated">>;
                    false when AuthenticatedServers > 0 -> <<"partially_authenticated">>;
                    false -> <<"not_authenticated">>
                end
            };
        {error, Reason} ->
            #{error => Reason, status => <<"error">>}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

get_authenticated_servers() ->
    case mcp_server_config:get_all_servers() of
        {ok, Servers} ->
            % Filter for servers that require authentication
            AuthServers = [S || S <- Servers, requires_authentication(S)],
            {ok, AuthServers};
        Error ->
            Error
    end.

format_servers_for_claude(Servers) ->
    maps:from_list([
        {format_server_key(Server), format_server_for_claude(Server)}
        || Server <- Servers
    ]).

format_server_key(#mcp_server{id = Id, name = Name}) ->
    % Use name if available, otherwise use ID
    case Name of
        <<>> -> Id;
        undefined -> Id;
        _ -> Name
    end.

format_server_for_claude(#mcp_server{} = Server) ->
    BaseConfig = #{
        <<"command">> => extract_command(Server),
        <<"args">> => extract_args(Server)
    },
    
    % Add environment variables if authenticated
    case get_server_env_vars(Server) of
        #{} = EnvVars when map_size(EnvVars) > 0 ->
            maps:put(<<"env">>, EnvVars, BaseConfig);
        _ ->
            BaseConfig
    end.

extract_command(#mcp_server{metadata = Metadata}) ->
    case maps:get(<<"command">>, Metadata, undefined) of
        undefined -> <<"npx">>;
        Command -> Command
    end.

extract_args(#mcp_server{metadata = Metadata}) ->
    case maps:get(<<"args">>, Metadata, undefined) of
        undefined -> [];
        Args when is_list(Args) -> Args;
        _ -> []
    end.

get_server_env_vars(#mcp_server{id = ServerId, auth_type = AuthType, metadata = Metadata}) ->
    BaseEnvVars = maps:get(<<"env">>, Metadata, #{}),
    
    case AuthType of
        oauth2 ->
            case get_oauth_token(ServerId) of
                {ok, Token} ->
                    add_oauth_env_vars(ServerId, Token, BaseEnvVars);
                _ ->
                    BaseEnvVars
            end;
        api_key ->
            case get_api_key(ServerId) of
                {ok, ApiKey} ->
                    add_api_key_env_vars(ServerId, ApiKey, BaseEnvVars);
                _ ->
                    BaseEnvVars
            end;
        _ ->
            BaseEnvVars
    end.

get_oauth_token(ServerId) ->
    % Try to get OAuth token for this server
    case oauth_handler:get_token(ServerId, get_current_user_id()) of
        {ok, #oauth_token{access_token = Token}} ->
            {ok, Token};
        _ ->
            {error, not_found}
    end.

get_api_key(ServerId) ->
    % Try to get API key from environment or configuration
    case get_api_key_from_env(ServerId) of
        {ok, ApiKey} -> {ok, ApiKey};
        _ -> {error, not_found}
    end.

add_oauth_env_vars(ServerId, Token, BaseEnvVars) ->
    case ServerId of
        <<"github">> ->
            maps:put(<<"GITHUB_PERSONAL_ACCESS_TOKEN">>, Token, BaseEnvVars);
        <<"github_mcp">> ->
            maps:put(<<"GITHUB_PERSONAL_ACCESS_TOKEN">>, Token, BaseEnvVars);
        <<"slack">> ->
            maps:put(<<"SLACK_BOT_TOKEN">>, Token, BaseEnvVars);
        <<"slack_mcp">> ->
            maps:put(<<"SLACK_BOT_TOKEN">>, Token, BaseEnvVars);
        <<"linear">> ->
            maps:put(<<"LINEAR_API_KEY">>, Token, BaseEnvVars);
        <<"asana">> ->
            maps:put(<<"ASANA_ACCESS_TOKEN">>, Token, BaseEnvVars);
        <<"intercom">> ->
            maps:put(<<"INTERCOM_ACCESS_TOKEN">>, Token, BaseEnvVars);
        <<"zapier">> ->
            maps:put(<<"ZAPIER_ACCESS_TOKEN">>, Token, BaseEnvVars);
        _ ->
            % Generic OAuth token handling
            maps:put(<<"ACCESS_TOKEN">>, Token, BaseEnvVars)
    end.

add_api_key_env_vars(ServerId, ApiKey, BaseEnvVars) ->
    case ServerId of
        <<"brave_search_mcp">> ->
            maps:put(<<"BRAVE_API_KEY">>, ApiKey, BaseEnvVars);
        <<"postgres_mcp">> ->
            maps:put(<<"POSTGRES_CONNECTION_STRING">>, ApiKey, BaseEnvVars);
        <<"graphlit">> ->
            % Graphlit has multiple required env vars
            maps:merge(BaseEnvVars, #{
                <<"GRAPHLIT_ORGANIZATION_ID">> => <<"1b591b0d-eb12-4f6d-be5a-ceb95b40e716">>,
                <<"GRAPHLIT_ENVIRONMENT_ID">> => <<"42d0e0ef-8eeb-463d-921c-ef5119541eb9">>,
                <<"GRAPHLIT_JWT_SECRET">> => ApiKey
            });
        _ ->
            % Generic API key handling
            maps:put(<<"API_KEY">>, ApiKey, BaseEnvVars)
    end.

get_api_key_from_env(ServerId) ->
    EnvVarName = case ServerId of
        <<"brave_search_mcp">> -> "BRAVE_API_KEY";
        <<"postgres_mcp">> -> "POSTGRES_CONNECTION_STRING";
        <<"graphlit">> -> "GRAPHLIT_JWT_SECRET";
        _ -> string:uppercase(binary_to_list(ServerId)) ++ "_API_KEY"
    end,
    
    case os:getenv(EnvVarName) of
        false -> {error, not_found};
        Value -> {ok, list_to_binary(Value)}
    end.

requires_authentication(#mcp_server{auth_type = open}) -> false;
requires_authentication(#mcp_server{auth_type = _}) -> true.

is_server_authenticated(#mcp_server{id = ServerId, auth_type = AuthType}) ->
    case AuthType of
        open -> true; % No auth required
        oauth2 ->
            case get_oauth_token(ServerId) of
                {ok, _} -> true;
                _ -> false
            end;
        api_key ->
            case get_api_key(ServerId) of
                {ok, _} -> true;
                _ -> false
            end;
        _ -> false
    end.

validate_server_auth(#mcp_server{id = ServerId, name = Name, auth_type = AuthType} = Server) ->
    AuthStatus = is_server_authenticated(Server),
    #{
        id => ServerId,
        name => Name,
        auth_type => AuthType,
        authenticated => AuthStatus,
        status => case AuthStatus of
            true -> <<"ready">>;
            false -> <<"requires_authentication">>
        end
    }.

add_auth_to_metadata(Metadata, AuthData) ->
    maps:merge(Metadata, AuthData).

get_current_user_id() ->
    % For now, use a default user ID. In production, this would come from session
    <<"default_user">>.

%%%===================================================================
%%% Claude Compatibility Helpers
%%%===================================================================

%% Generate Claude-specific MCP configuration file content
generate_claude_config_file() ->
    case get_claude_mcp_config() of
        {ok, Config} ->
            JsonConfig = jsx:encode(Config, [{space, 2}, {indent, 2}]),
            {ok, JsonConfig};
        Error ->
            Error
    end.

%% Export configuration for Claude Desktop app
export_for_claude_desktop() ->
    case generate_claude_config_file() of
        {ok, JsonConfig} ->
            % Claude Desktop expects config in ~/.claude/claude_desktop_config.json
            ConfigPath = filename:join([os:getenv("HOME"), ".claude", "claude_desktop_config.json"]),
            case file:write_file(ConfigPath, JsonConfig) of
                ok ->
                    io:format("[CLAUDE_OAUTH] Configuration exported to: ~s~n", [ConfigPath]),
                    {ok, ConfigPath};
                {error, Reason} ->
                    io:format("[CLAUDE_OAUTH] Failed to write config file: ~p~n", [Reason]),
                    {error, Reason}
            end;
        Error ->
            Error
    end.

%% Get OAuth status summary for Claude compatibility
get_oauth_summary_for_claude() ->
    AuthStatus = get_auth_status_for_claude(),
    
    case maps:get(status, AuthStatus) of
        <<"fully_authenticated">> ->
            #{
                status => <<"ready">>,
                message => <<"All MCP servers are properly authenticated">>,
                details => AuthStatus
            };
        <<"partially_authenticated">> ->
            #{
                status => <<"warning">>,
                message => <<"Some MCP servers require authentication">>,
                details => AuthStatus,
                action_required => <<"Please complete OAuth flows for remaining servers">>
            };
        <<"not_authenticated">> ->
            #{
                status => <<"error">>,
                message => <<"MCP servers require authentication">>,
                details => AuthStatus,
                action_required => <<"Please authenticate with OAuth providers">>
            };
        _ ->
            #{
                status => <<"error">>,
                message => <<"Unable to determine authentication status">>,
                details => AuthStatus
            }
    end.