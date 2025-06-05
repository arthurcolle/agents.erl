%%%-------------------------------------------------------------------
%%% @doc
%%% OAuth Handler for MCP Server Authentication
%%% Handles OAuth flows for various MCP providers with popup support
%%% Compatible with Claude's authentication patterns
%%% @end
%%%-------------------------------------------------------------------
-module(oauth_handler).

-export([init/2, handle_oauth_callback/3, 
         store_token/3, get_token/2, revoke_token/2, init_oauth_system/0]).

-include_lib("stdlib/include/qlc.hrl").

-record(oauth_token, {
    provider :: binary(),
    user_id :: binary(),
    access_token :: binary(),
    refresh_token :: binary(),
    expires_at :: calendar:datetime(),
    scope :: binary(),
    metadata :: map()
}).

%% OAuth provider configurations
-define(OAUTH_PROVIDERS, #{
    <<"github">> => #{
        client_id => <<"Ov23li8QpUE3VQn8Xr2t">>,
        client_secret => get_env_var("GITHUB_CLIENT_SECRET"),
        auth_url => <<"https://github.com/login/oauth/authorize">>,
        token_url => <<"https://github.com/login/oauth/access_token">>,
        scopes => <<"repo,user:email,read:org">>,
        redirect_uri => <<"http://localhost:8081/api/oauth/callback/github">>
    },
    <<"slack">> => #{
        client_id => get_env_var("SLACK_CLIENT_ID"),
        client_secret => get_env_var("SLACK_CLIENT_SECRET"),
        auth_url => <<"https://slack.com/oauth/v2/authorize">>,
        token_url => <<"https://slack.com/api/oauth.v2.access">>,
        scopes => <<"channels:read,chat:write,users:read,files:read">>,
        redirect_uri => <<"http://localhost:8081/api/oauth/callback/slack">>
    },
    <<"linear">> => #{
        client_id => get_env_var("LINEAR_CLIENT_ID"),
        client_secret => get_env_var("LINEAR_CLIENT_SECRET"),
        auth_url => <<"https://linear.app/oauth/authorize">>,
        token_url => <<"https://api.linear.app/oauth/token">>,
        scopes => <<"read,write">>,
        redirect_uri => <<"http://localhost:8081/api/oauth/callback/linear">>
    },
    <<"asana">> => #{
        client_id => get_env_var("ASANA_CLIENT_ID"),
        client_secret => get_env_var("ASANA_CLIENT_SECRET"),
        auth_url => <<"https://app.asana.com/-/oauth_authorize">>,
        token_url => <<"https://app.asana.com/-/oauth_token">>,
        scopes => <<"default">>,
        redirect_uri => <<"http://localhost:8081/api/oauth/callback/asana">>
    },
    <<"intercom">> => #{
        client_id => get_env_var("INTERCOM_CLIENT_ID"),
        client_secret => get_env_var("INTERCOM_CLIENT_SECRET"),
        auth_url => <<"https://app.intercom.com/oauth">>,
        token_url => <<"https://api.intercom.io/auth/eagle/token">>,
        scopes => <<"read_conversations,write_conversations,read_users">>,
        redirect_uri => <<"http://localhost:8081/api/oauth/callback/intercom">>
    },
    <<"zapier">> => #{
        client_id => get_env_var("ZAPIER_CLIENT_ID"),
        client_secret => get_env_var("ZAPIER_CLIENT_SECRET"),
        auth_url => <<"https://zapier.com/oauth/authorize/">>,
        token_url => <<"https://zapier.com/oauth/access-token/">>,
        scopes => <<"read,write">>,
        redirect_uri => <<"http://localhost:8081/api/oauth/callback/zapier">>
    }
}).

%%%===================================================================
%%% Cowboy Handler Implementation
%%%===================================================================

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path_info(Req0),
    handle_request(Method, Path, Req0, State).

handle_request(<<"GET">>, [<<"oauth">>, <<"callback">>, Provider], Req0, State) ->
    handle_oauth_callback(Provider, Req0, State);

handle_request(<<"GET">>, [<<"oauth">>, <<"authorize">>, Provider], Req0, State) ->
    handle_oauth_authorize(Provider, Req0, State);

handle_request(<<"POST">>, [<<"oauth">>, <<"revoke">>, Provider], Req0, State) ->
    handle_oauth_revoke(Provider, Req0, State);

handle_request(<<"GET">>, [<<"oauth">>, <<"status">>, Provider], Req0, State) ->
    handle_oauth_status(Provider, Req0, State);

handle_request(_, _, Req0, State) ->
    Req1 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>},
                           jsx:encode(#{error => <<"not_found">>}), Req0),
    {ok, Req1, State}.

%%%===================================================================
%%% OAuth Flow Handlers
%%%===================================================================

handle_oauth_authorize(Provider, Req0, State) ->
    io:format("[OAUTH] Starting authorization for provider: ~s~n", [Provider]),
    
    case maps:get(Provider, ?OAUTH_PROVIDERS, undefined) of
        undefined ->
            Req1 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>},
                                   jsx:encode(#{error => <<"unsupported_provider">>}), Req0),
            {ok, Req1, State};
        Config ->
            UserId = get_user_id_from_request(Req0),
            StateParam = generate_and_store_state(Provider, UserId),
            AuthUrl = build_auth_url(Config, StateParam),
            
            % Return JSON response for popup handling
            ResponseData = #{
                auth_url => AuthUrl,
                provider => Provider,
                state => StateParam,
                popup_features => <<"width=600,height=700,scrollbars=yes,resizable=yes">>
            },
            
            Req1 = cowboy_req:reply(200, 
                #{<<"content-type">> => <<"application/json">>,
                  <<"access-control-allow-origin">> => <<"*">>},
                jsx:encode(ResponseData), Req0),
            {ok, Req1, State}
    end.

handle_oauth_callback(Provider, Req0, _State) ->
    io:format("[OAUTH] Handling callback for provider: ~s~n", [Provider]),
    
    QsVals = cowboy_req:parse_qs(Req0),
    Code = proplists:get_value(<<"code">>, QsVals),
    StateParam = proplists:get_value(<<"state">>, QsVals),
    Error = proplists:get_value(<<"error">>, QsVals),
    
    case Error of
        undefined when Code =/= undefined, StateParam =/= undefined ->
            case validate_state(StateParam) of
                {ok, UserId} ->
                    case exchange_code_for_token(Provider, Code) of
                        {ok, TokenData} ->
                            store_token(Provider, UserId, TokenData),
                            send_success_popup_response(Req0, Provider, TokenData);
                        {error, Reason} ->
                            send_error_popup_response(Req0, Provider, Reason)
                    end;
                {error, invalid_state} ->
                    send_error_popup_response(Req0, Provider, <<"invalid_state">>)
            end;
        undefined ->
            send_error_popup_response(Req0, Provider, <<"missing_parameters">>);
        ErrorCode ->
            send_error_popup_response(Req0, Provider, ErrorCode)
    end.

handle_oauth_revoke(Provider, Req0, State) ->
    UserId = get_user_id_from_request(Req0),
    case revoke_token(Provider, UserId) of
        ok ->
            Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                                   jsx:encode(#{success => true}), Req0),
            {ok, Req1, State};
        {error, Reason} ->
            Req1 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>},
                                   jsx:encode(#{error => Reason}), Req0),
            {ok, Req1, State}
    end.

handle_oauth_status(Provider, Req0, State) ->
    UserId = get_user_id_from_request(Req0),
    case get_token(Provider, UserId) of
        {ok, _Token} ->
            ResponseData = #{
                authenticated => true,
                provider => Provider,
                user_id => UserId
            },
            Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                                   jsx:encode(ResponseData), Req0),
            {ok, Req1, State};
        {error, not_found} ->
            ResponseData = #{
                authenticated => false,
                provider => Provider
            },
            Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                                   jsx:encode(ResponseData), Req0),
            {ok, Req1, State}
    end.

%%%===================================================================
%%% Token Management
%%%===================================================================

store_token(Provider, UserId, TokenData) ->
    Token = #oauth_token{
        provider = Provider,
        user_id = UserId,
        access_token = maps:get(access_token, TokenData),
        refresh_token = maps:get(refresh_token, TokenData, <<>>),
        expires_at = calculate_expiry(maps:get(expires_in, TokenData, 3600)),
        scope = maps:get(scope, TokenData, <<>>),
        metadata = maps:get(metadata, TokenData, #{})
    },
    
    F = fun() -> mnesia:write(Token) end,
    case mnesia:transaction(F) of
        {atomic, ok} -> 
            io:format("[OAUTH] Token stored for ~s:~s~n", [Provider, UserId]),
            {ok, Token};
        {aborted, Reason} -> 
            io:format("[OAUTH] Failed to store token: ~p~n", [Reason]),
            {error, Reason}
    end.

get_token(Provider, UserId) ->
    F = fun() -> mnesia:read(oauth_token, {Provider, UserId}) end,
    case mnesia:transaction(F) of
        {atomic, [Token]} -> 
            case is_token_expired(Token) of
                false -> {ok, Token};
                true -> refresh_token_if_possible(Token)
            end;
        {atomic, []} -> {error, not_found};
        {aborted, Reason} -> {error, Reason}
    end.

revoke_token(Provider, UserId) ->
    F = fun() -> mnesia:delete({oauth_token, {Provider, UserId}}) end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% OAuth Protocol Implementation
%%%===================================================================

build_auth_url(Config, State) ->
    BaseUrl = maps:get(auth_url, Config),
    Params = [
        {<<"client_id">>, maps:get(client_id, Config)},
        {<<"redirect_uri">>, maps:get(redirect_uri, Config)},
        {<<"scope">>, maps:get(scopes, Config)},
        {<<"state">>, State},
        {<<"response_type">>, <<"code">>}
    ],
    QueryString = cow_qs:qs(Params),
    <<BaseUrl/binary, "?", QueryString/binary>>.

exchange_code_for_token(Provider, Code) ->
    case maps:get(Provider, ?OAUTH_PROVIDERS, undefined) of
        undefined -> {error, unsupported_provider};
        Config ->
            TokenUrl = maps:get(token_url, Config),
            
            Body = cow_qs:qs([
                {<<"client_id">>, maps:get(client_id, Config)},
                {<<"client_secret">>, maps:get(client_secret, Config)},
                {<<"code">>, Code},
                {<<"redirect_uri">>, maps:get(redirect_uri, Config)},
                {<<"grant_type">>, <<"authorization_code">>}
            ]),
            
            case httpc:request(post, {binary_to_list(TokenUrl), 
                                     [{"content-type", "application/x-www-form-urlencoded"},
                                      {"accept", "application/json"}], 
                                     "application/x-www-form-urlencoded", 
                                     binary_to_list(Body)}, [], []) of
                {ok, {{_, 200, _}, _Headers, ResponseBody}} ->
                    try jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                        TokenData -> {ok, TokenData}
                    catch
                        _:_ -> {error, invalid_response}
                    end;
                {ok, {{_, StatusCode, _}, _Headers, ErrorBody}} ->
                    io:format("[OAUTH] Token exchange failed: ~p ~s~n", [StatusCode, ErrorBody]),
                    {error, {http_error, StatusCode}};
                {error, Reason} ->
                    io:format("[OAUTH] HTTP request failed: ~p~n", [Reason]),
                    {error, http_request_failed}
            end
    end.

%%%===================================================================
%%% Popup Response Handlers
%%%===================================================================

send_success_popup_response(Req0, Provider, TokenData) ->
    % HTML page that sends success message to parent window and closes popup
    Html = <<"<!DOCTYPE html>
<html>
<head>
    <title>Authorization Successful</title>
    <script>
        window.onload = function() {
            const message = {
                type: 'oauth_success',
                provider: '", Provider/binary, "',
                data: ", (jsx:encode(TokenData))/binary, "
            };
            if (window.opener) {
                window.opener.postMessage(message, '*');
                window.close();
            } else {
                document.getElementById('message').innerHTML = 
                    '<h2>Authorization Successful!</h2><p>You can close this window.</p>';
            }
        };
    </script>
</head>
<body>
    <div id=\"message\">
        <h2>Processing...</h2>
        <p>Please wait while we complete the authorization.</p>
    </div>
</body>
</html>">>,
    
    Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req1, []}.

send_error_popup_response(Req0, Provider, Error) ->
    % HTML page that sends error message to parent window and closes popup
    Html = <<"<!DOCTYPE html>
<html>
<head>
    <title>Authorization Failed</title>
    <script>
        window.onload = function() {
            const message = {
                type: 'oauth_error',
                provider: '", Provider/binary, "',
                error: '", Error/binary, "'
            };
            if (window.opener) {
                window.opener.postMessage(message, '*');
                window.close();
            } else {
                document.getElementById('message').innerHTML = 
                    '<h2>Authorization Failed</h2><p>Error: ", Error/binary, "</p>';
            }
        };
    </script>
</head>
<body>
    <div id=\"message\">
        <h2>Processing...</h2>
        <p>Please wait...</p>
    </div>
</body>
</html>">>,
    
    Req1 = cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, Html, Req0),
    {ok, Req1, []}.

%%%===================================================================
%%% Utility Functions
%%%===================================================================

get_env_var(VarName) ->
    case os:getenv(VarName) of
        false -> <<"">>;
        Value -> list_to_binary(Value)
    end.

get_user_id_from_request(Req) ->
    % Extract user ID from session, JWT, or generate temporary ID
    case cowboy_req:header(<<"x-user-id">>, Req) of
        undefined -> 
            % Generate temporary user ID for demo purposes
            list_to_binary(agent_uuid:to_string(agent_uuid:uuid4()));
        UserId -> UserId
    end.

generate_and_store_state(Provider, UserId) ->
    State = base64:encode(crypto:strong_rand_bytes(32)),
    % Store state in ETS for verification (expires in 10 minutes)
    ets:insert(oauth_states, {State, Provider, UserId, 
                             calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 600}),
    State.

validate_state(State) ->
    case ets:lookup(oauth_states, State) of
        [{State, _Provider, UserId, ExpiresAt}] ->
            Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
            if ExpiresAt > Now ->
                ets:delete(oauth_states, State),
                {ok, UserId};
               true ->
                ets:delete(oauth_states, State),
                {error, expired_state}
            end;
        [] ->
            {error, invalid_state}
    end.

calculate_expiry(ExpiresIn) when is_integer(ExpiresIn) ->
    Seconds = calendar:datetime_to_gregorian_seconds(calendar:local_time()) + ExpiresIn,
    calendar:gregorian_seconds_to_datetime(Seconds);
calculate_expiry(_) ->
    calculate_expiry(3600). % Default 1 hour

is_token_expired(#oauth_token{expires_at = ExpiresAt}) ->
    Now = calendar:local_time(),
    calendar:datetime_to_gregorian_seconds(ExpiresAt) =< 
        calendar:datetime_to_gregorian_seconds(Now).

refresh_token_if_possible(#oauth_token{refresh_token = <<>>}) ->
    {error, token_expired};
refresh_token_if_possible(_Token) ->
    % TODO: Implement refresh token logic
    {error, token_expired}.

%%%===================================================================
%%% Initialization
%%%===================================================================

init_oauth_system() ->
    % Create ETS table for state management
    ets:new(oauth_states, [named_table, public, set]),
    
    % Create Mnesia table for token storage
    TableType = case node() of
        nonode@nohost -> ram_copies;
        _ -> disc_copies
    end,
    
    mnesia:create_table(oauth_token,
        [{attributes, record_info(fields, oauth_token)},
         {TableType, [node()]},
         {index, [provider, user_id]}]),
    
    io:format("[OAUTH] OAuth system initialized~n").