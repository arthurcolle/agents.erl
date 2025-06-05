%%%-------------------------------------------------------------------
%%% @doc
%%% MCP OAuth Integration with Popup Authentication
%%% Enhanced OAuth flow specifically for MCP server authentication
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_oauth_integration).

-export([init_oauth_for_mcp/0, 
         start_oauth_flow/2, 
         handle_oauth_popup_callback/3,
         get_oauth_url/2,
         is_oauth_required/1,
         connect_with_oauth/2,
         refresh_oauth_token/2]).

-include_lib("stdlib/include/qlc.hrl").

%% OAuth configurations for MCP servers
-define(MCP_OAUTH_CONFIG, #{
    <<"linear">> => #{
        client_id => get_env_or_default("LINEAR_CLIENT_ID", <<"lin_api_demo">>),
        client_secret => get_env_or_default("LINEAR_CLIENT_SECRET", <<"demo_secret">>),
        auth_url => <<"https://linear.app/oauth/authorize">>,
        token_url => <<"https://api.linear.app/oauth/token">>,
        scopes => <<"read,write">>,
        redirect_uri => <<"http://localhost:8080/api/mcp/oauth/callback/linear">>,
        server_id => <<"linear">>,
        mcp_url => <<"https://mcp.linear.app/sse">>
    },
    <<"slack">> => #{
        client_id => get_env_or_default("SLACK_CLIENT_ID", <<"demo_client">>),
        client_secret => get_env_or_default("SLACK_CLIENT_SECRET", <<"demo_secret">>),
        auth_url => <<"https://slack.com/oauth/v2/authorize">>,
        token_url => <<"https://slack.com/api/oauth.v2.access">>,
        scopes => <<"channels:read,chat:write,users:read,files:read">>,
        redirect_uri => <<"http://localhost:8080/api/mcp/oauth/callback/slack">>,
        server_id => <<"slack_mcp">>,
        mcp_url => <<"npx -y @modelcontextprotocol/server-slack">>
    },
    <<"asana">> => #{
        client_id => get_env_or_default("ASANA_CLIENT_ID", <<"demo_client">>),
        client_secret => get_env_or_default("ASANA_CLIENT_SECRET", <<"demo_secret">>),
        auth_url => <<"https://app.asana.com/-/oauth_authorize">>,
        token_url => <<"https://app.asana.com/-/oauth_token">>,
        scopes => <<"default">>,
        redirect_uri => <<"http://localhost:8080/api/mcp/oauth/callback/asana">>,
        server_id => <<"asana">>,
        mcp_url => <<"https://mcp.asana.com/sse">>
    },
    <<"intercom">> => #{
        client_id => get_env_or_default("INTERCOM_CLIENT_ID", <<"demo_client">>),
        client_secret => get_env_or_default("INTERCOM_CLIENT_SECRET", <<"demo_secret">>),
        auth_url => <<"https://app.intercom.com/oauth">>,
        token_url => <<"https://api.intercom.io/auth/eagle/token">>,
        scopes => <<"read_conversations,write_conversations,read_users">>,
        redirect_uri => <<"http://localhost:8080/api/mcp/oauth/callback/intercom">>,
        server_id => <<"intercom">>,
        mcp_url => <<"https://mcp.intercom.com/sse">>
    },
    <<"paypal">> => #{
        client_id => get_env_or_default("PAYPAL_CLIENT_ID", <<"demo_client">>),
        client_secret => get_env_or_default("PAYPAL_CLIENT_SECRET", <<"demo_secret">>),
        auth_url => <<"https://www.paypal.com/signin/authorize">>,
        token_url => <<"https://api.paypal.com/v1/oauth2/token">>,
        scopes => <<"openid profile">>,
        redirect_uri => <<"http://localhost:8080/api/mcp/oauth/callback/paypal">>,
        server_id => <<"paypal">>,
        mcp_url => <<"https://mcp.paypal.com/sse">>
    },
    <<"workato">> => #{
        client_id => get_env_or_default("WORKATO_CLIENT_ID", <<"demo_client">>),
        client_secret => get_env_or_default("WORKATO_CLIENT_SECRET", <<"demo_secret">>),
        auth_url => <<"https://www.workato.com/oauth/authorize">>,
        token_url => <<"https://www.workato.com/oauth/token">>,
        scopes => <<"read,write">>,
        redirect_uri => <<"http://localhost:8080/api/mcp/oauth/callback/workato">>,
        server_id => <<"workato">>,
        mcp_url => <<"https://mcp.workato.com/sse">>
    }
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% Initialize OAuth system for MCP servers
init_oauth_for_mcp() ->
    io:format("[MCP_OAUTH] Initializing OAuth system for MCP servers~n"),
    
    % Ensure oauth_handler is initialized
    case oauth_handler:init_oauth_system() of
        ok -> 
            io:format("[MCP_OAUTH] OAuth system initialized successfully~n"),
            register_mcp_oauth_routes();
        {error, Reason} ->
            io:format("[MCP_OAUTH] Failed to initialize OAuth: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Start OAuth flow for an MCP server
start_oauth_flow(ServerId, UserId) ->
    case maps:get(ServerId, ?MCP_OAUTH_CONFIG, undefined) of
        undefined ->
            {error, {unsupported_server, ServerId}};
        Config ->
            AuthUrl = build_oauth_url(Config, UserId),
            {ok, #{
                auth_url => AuthUrl,
                provider => ServerId,
                popup_required => true,
                redirect_uri => maps:get(redirect_uri, Config)
            }}
    end.

%% Get OAuth authorization URL for popup
get_oauth_url(ServerId, UserId) ->
    case maps:get(ServerId, ?MCP_OAUTH_CONFIG, undefined) of
        undefined ->
            {error, {unsupported_server, ServerId}};
        Config ->
            AuthUrl = build_oauth_url(Config, UserId),
            {ok, AuthUrl}
    end.

%% Check if OAuth is required for a server
is_oauth_required(ServerId) ->
    case mcp_registry:get_server(ServerId) of
        {ok, ServerConfig} ->
            AuthType = maps:get(auth_type, ServerConfig, open),
            AuthType =:= oauth2;
        _ ->
            false
    end.

%% Connect to MCP server using OAuth token
connect_with_oauth(ServerId, AuthCode) ->
    case maps:get(ServerId, ?MCP_OAUTH_CONFIG, undefined) of
        undefined ->
            {error, {unsupported_server, ServerId}};
        Config ->
            case exchange_code_for_token(Config, AuthCode) of
                {ok, TokenData} ->
                    % Store token and attempt MCP connection
                    store_mcp_token(ServerId, TokenData),
                    attempt_mcp_connection(ServerId, TokenData);
                {error, Reason} ->
                    {error, {token_exchange_failed, Reason}}
            end
    end.

%% Handle OAuth popup callback
handle_oauth_popup_callback(ServerId, AuthCode, State) ->
    case connect_with_oauth(ServerId, AuthCode) of
        {ok, ConnectionResult} ->
            % Send success message to frontend via WebSocket
            broadcast_oauth_success(ServerId, ConnectionResult),
            generate_popup_close_response();
        {error, Reason} ->
            % Send error message to frontend via WebSocket
            broadcast_oauth_error(ServerId, Reason),
            generate_popup_error_response(Reason)
    end.

%% Refresh OAuth token
refresh_oauth_token(ServerId, RefreshToken) ->
    case maps:get(ServerId, ?MCP_OAUTH_CONFIG, undefined) of
        undefined ->
            {error, {unsupported_server, ServerId}};
        Config ->
            refresh_token_request(Config, RefreshToken)
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Build OAuth authorization URL
build_oauth_url(Config, UserId) ->
    BaseUrl = maps:get(auth_url, Config),
    ClientId = maps:get(client_id, Config),
    RedirectUri = maps:get(redirect_uri, Config),
    Scopes = maps:get(scopes, Config),
    State = generate_oauth_state(UserId),
    
    Params = [
        {<<"client_id">>, ClientId},
        {<<"redirect_uri">>, RedirectUri},
        {<<"scope">>, Scopes},
        {<<"state">>, State},
        {<<"response_type">>, <<"code">>}
    ],
    
    QueryString = build_query_string(Params),
    <<BaseUrl/binary, "?", QueryString/binary>>.

%% Exchange authorization code for access token
exchange_code_for_token(Config, AuthCode) ->
    TokenUrl = maps:get(token_url, Config),
    ClientId = maps:get(client_id, Config),
    ClientSecret = maps:get(client_secret, Config),
    RedirectUri = maps:get(redirect_uri, Config),
    
    Body = build_query_string([
        {<<"grant_type">>, <<"authorization_code">>},
        {<<"code">>, AuthCode},
        {<<"client_id">>, ClientId},
        {<<"client_secret">>, ClientSecret},
        {<<"redirect_uri">>, RedirectUri}
    ]),
    
    Headers = [
        {"Content-Type", "application/x-www-form-urlencoded"},
        {"Accept", "application/json"}
    ],
    
    case httpc:request(post, {binary_to_list(TokenUrl), Headers, "application/x-www-form-urlencoded", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                TokenData when is_map(TokenData) ->
                    {ok, TokenData};
                _ ->
                    {error, invalid_token_response}
            end;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {http_error, Status, ErrorBody}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

%% Attempt to connect to MCP server with OAuth token
attempt_mcp_connection(ServerId, TokenData) ->
    AccessToken = maps:get(<<"access_token">>, TokenData),
    
    % Update server configuration with OAuth token
    case mcp_registry:get_server(ServerId) of
        {ok, ServerConfig} ->
            UpdatedConfig = ServerConfig#{
                oauth_token => AccessToken,
                oauth_configured => true
            },
            mcp_registry:update_server(ServerId, UpdatedConfig),
            
            % Attempt connection
            case mcp_manager:connect_remote_server(ServerId, UpdatedConfig) of
                {ok, _} ->
                    {ok, #{status => connected, server_id => ServerId}};
                {error, Reason} ->
                    {error, {connection_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {server_not_found, Reason}}
    end.

%% Store MCP OAuth token
store_mcp_token(ServerId, TokenData) ->
    AccessToken = maps:get(<<"access_token">>, TokenData),
    RefreshToken = maps:get(<<"refresh_token">>, TokenData, undefined),
    ExpiresIn = maps:get(<<"expires_in">>, TokenData, 3600),
    
    ExpiresAt = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(calendar:local_time()) + ExpiresIn
    ),
    
    oauth_handler:store_token(ServerId, <<"mcp_user">>, #{
        access_token => AccessToken,
        refresh_token => RefreshToken,
        expires_at => ExpiresAt,
        scope => maps:get(<<"scope">>, TokenData, <<"">>),
        metadata => #{server_type => mcp}
    }).

%% Generate OAuth state parameter
generate_oauth_state(UserId) ->
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    Nonce = integer_to_binary(rand:uniform(1000000)),
    <<UserId/binary, "_", Timestamp/binary, "_", Nonce/binary>>.

%% Build query string from parameters
build_query_string(Params) ->
    EncodedParams = lists:map(fun({Key, Value}) ->
        EncodedKey = http_uri:encode(binary_to_list(Key)),
        EncodedValue = http_uri:encode(binary_to_list(Value)),
        list_to_binary(EncodedKey ++ "=" ++ EncodedValue)
    end, Params),
    iolist_to_binary(lists:join("&", EncodedParams)).

%% Broadcast OAuth success to frontend
broadcast_oauth_success(ServerId, Result) ->
    Message = #{
        type => <<"mcp_oauth_success">>,
        server_id => ServerId,
        result => Result,
        timestamp => erlang:system_time(millisecond)
    },
    % Use existing WebSocket broadcasting system
    case whereis(agent_ws_handler) of
        undefined -> ok;
        _ -> agent_ws_handler:broadcast(jsx:encode(Message))
    end.

%% Broadcast OAuth error to frontend
broadcast_oauth_error(ServerId, Error) ->
    Message = #{
        type => <<"mcp_oauth_error">>,
        server_id => ServerId,
        error => format_error(Error),
        timestamp => erlang:system_time(millisecond)
    },
    % Use existing WebSocket broadcasting system
    case whereis(agent_ws_handler) of
        undefined -> ok;
        _ -> agent_ws_handler:broadcast(jsx:encode(Message))
    end.

%% Generate popup close response
generate_popup_close_response() ->
    Html = <<"<!DOCTYPE html>
<html>
<head>
    <title>OAuth Success</title>
    <style>
        body { 
            font-family: Arial, sans-serif; 
            text-align: center; 
            padding: 50px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
        }
        .success { 
            background: rgba(255,255,255,0.1); 
            padding: 30px; 
            border-radius: 10px; 
            margin: 20px auto; 
            max-width: 400px;
        }
        .icon { font-size: 48px; margin-bottom: 20px; }
    </style>
    <script>
        // Close popup window
        setTimeout(function() {
            if (window.opener) {
                window.opener.postMessage({type: 'oauth_success'}, '*');
            }
            window.close();
        }, 2000);
    </script>
</head>
<body>
    <div class='success'>
        <div class='icon'>✅</div>
        <h2>Authentication Successful!</h2>
        <p>You can now close this window.</p>
        <p><small>This window will close automatically in 2 seconds.</small></p>
    </div>
</body>
</html>">>,
    {200, #{<<"content-type">> => <<"text/html">>}, Html}.

%% Generate popup error response
generate_popup_error_response(Error) ->
    ErrorMsg = format_error(Error),
    Html = <<"<!DOCTYPE html>
<html>
<head>
    <title>OAuth Error</title>
    <style>
        body { 
            font-family: Arial, sans-serif; 
            text-align: center; 
            padding: 50px;
            background: linear-gradient(135deg, #f56565 0%, #e53e3e 100%);
            color: white;
        }
        .error { 
            background: rgba(255,255,255,0.1); 
            padding: 30px; 
            border-radius: 10px; 
            margin: 20px auto; 
            max-width: 400px;
        }
        .icon { font-size: 48px; margin-bottom: 20px; }
    </style>
    <script>
        setTimeout(function() {
            if (window.opener) {
                window.opener.postMessage({type: 'oauth_error', error: '", ErrorMsg/binary, "'}, '*');
            }
            window.close();
        }, 3000);
    </script>
</head>
<body>
    <div class='error'>
        <div class='icon'>❌</div>
        <h2>Authentication Failed</h2>
        <p>", ErrorMsg/binary, "</p>
        <p><small>This window will close automatically in 3 seconds.</small></p>
    </div>
</body>
</html>">>,
    {200, #{<<"content-type">> => <<"text/html">>}, Html}.

%% Register OAuth routes for MCP
register_mcp_oauth_routes() ->
    io:format("[MCP_OAUTH] Registering MCP OAuth routes~n"),
    ok.

%% Format error for display
format_error({token_exchange_failed, Reason}) ->
    iolist_to_binary(io_lib:format("Token exchange failed: ~p", [Reason]));
format_error({connection_failed, Reason}) ->
    iolist_to_binary(io_lib:format("Connection failed: ~p", [Reason]));
format_error({unsupported_server, ServerId}) ->
    iolist_to_binary(io_lib:format("Unsupported server: ~s", [ServerId]));
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% Refresh token request
refresh_token_request(Config, RefreshToken) ->
    TokenUrl = maps:get(token_url, Config),
    ClientId = maps:get(client_id, Config),
    ClientSecret = maps:get(client_secret, Config),
    
    Body = build_query_string([
        {<<"grant_type">>, <<"refresh_token">>},
        {<<"refresh_token">>, RefreshToken},
        {<<"client_id">>, ClientId},
        {<<"client_secret">>, ClientSecret}
    ]),
    
    Headers = [
        {"Content-Type", "application/x-www-form-urlencoded"},
        {"Accept", "application/json"}
    ],
    
    case httpc:request(post, {binary_to_list(TokenUrl), Headers, "application/x-www-form-urlencoded", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                TokenData when is_map(TokenData) ->
                    {ok, TokenData};
                _ ->
                    {error, invalid_token_response}
            end;
        {ok, {{_, Status, _}, _, ErrorBody}} ->
            {error, {http_error, Status, ErrorBody}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

%% Get environment variable or default
get_env_or_default(VarName, Default) ->
    case os:getenv(VarName) of
        false -> Default;
        Value -> list_to_binary(Value)
    end.