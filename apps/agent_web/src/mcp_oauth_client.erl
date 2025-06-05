-module(mcp_oauth_client).
-behaviour(gen_server).

%% API
-export([start_link/0, 
         authenticate_server/2,
         get_auth_token/1,
         refresh_token/1,
         revoke_token/1,
         check_auth_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
    auth_tokens = #{}, % ServerId -> {AccessToken, RefreshToken, ExpiresAt}
    pending_auth = #{}, % ServerId -> {From, StartTime}
    oauth_configs = #{} % ServerId -> OAuthConfig
}).

-record(oauth_config, {
    client_id,
    client_secret,
    auth_url,
    token_url,
    redirect_uri,
    scopes
}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

authenticate_server(ServerId, AuthConfig) ->
    gen_server:call(?MODULE, {authenticate, ServerId, AuthConfig}).

get_auth_token(ServerId) ->
    gen_server:call(?MODULE, {get_token, ServerId}).

refresh_token(ServerId) ->
    gen_server:call(?MODULE, {refresh_token, ServerId}).

revoke_token(ServerId) ->
    gen_server:call(?MODULE, {revoke_token, ServerId}).

check_auth_status(ServerId) ->
    gen_server:call(?MODULE, {check_status, ServerId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call({authenticate, ServerId, AuthConfig}, From, State) ->
    case maps:find(ServerId, State#state.auth_tokens) of
        {ok, {_AccessToken, _RefreshToken, ExpiresAt}} ->
            % Check if token is still valid
            Now = erlang:system_time(second),
            if Now < ExpiresAt ->
                {reply, {ok, already_authenticated}, State};
            true ->
                % Token expired, need to refresh
                handle_token_refresh(ServerId, From, State)
            end;
        error ->
            % No token, start OAuth flow
            start_oauth_flow(ServerId, AuthConfig, From, State)
    end;

handle_call({get_token, ServerId}, _From, State) ->
    case maps:find(ServerId, State#state.auth_tokens) of
        {ok, {AccessToken, _RefreshToken, ExpiresAt}} ->
            Now = erlang:system_time(second),
            if Now < ExpiresAt ->
                {reply, {ok, AccessToken}, State};
            true ->
                {reply, {error, token_expired}, State}
            end;
        error ->
            {reply, {error, not_authenticated}, State}
    end;

handle_call({refresh_token, ServerId}, From, State) ->
    handle_token_refresh(ServerId, From, State);

handle_call({revoke_token, ServerId}, _From, State) ->
    case maps:find(ServerId, State#state.auth_tokens) of
        {ok, {AccessToken, _RefreshToken, _ExpiresAt}} ->
            case maps:find(ServerId, State#state.oauth_configs) of
                {ok, Config} ->
                    % Send revoke request to OAuth provider
                    spawn(fun() -> revoke_oauth_token(AccessToken, Config) end),
                    NewTokens = maps:remove(ServerId, State#state.auth_tokens),
                    {reply, ok, State#state{auth_tokens = NewTokens}};
                error ->
                    {reply, {error, no_config}, State}
            end;
        error ->
            {reply, {error, not_authenticated}, State}
    end;

handle_call({check_status, ServerId}, _From, State) ->
    case maps:find(ServerId, State#state.auth_tokens) of
        {ok, {_AccessToken, _RefreshToken, ExpiresAt}} ->
            Now = erlang:system_time(second),
            Status = if Now < ExpiresAt -> authenticated;
                       true -> expired
                    end,
            {reply, {ok, Status}, State};
        error ->
            case maps:find(ServerId, State#state.pending_auth) of
                {ok, _} -> {reply, {ok, authenticating}, State};
                error -> {reply, {ok, not_authenticated}, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({auth_callback, ServerId, AuthCode}, State) ->
    case maps:find(ServerId, State#state.pending_auth) of
        {ok, {From, _StartTime}} ->
            case maps:find(ServerId, State#state.oauth_configs) of
                {ok, Config} ->
                    % Exchange auth code for tokens
                    case exchange_auth_code(AuthCode, Config) of
                        {ok, AccessToken, RefreshToken, ExpiresIn} ->
                            ExpiresAt = erlang:system_time(second) + ExpiresIn,
                            NewTokens = maps:put(ServerId, {AccessToken, RefreshToken, ExpiresAt}, 
                                               State#state.auth_tokens),
                            NewPending = maps:remove(ServerId, State#state.pending_auth),
                            gen_server:reply(From, {ok, AccessToken}),
                            {noreply, State#state{
                                auth_tokens = NewTokens,
                                pending_auth = NewPending
                            }};
                        {error, Reason} ->
                            NewPending = maps:remove(ServerId, State#state.pending_auth),
                            gen_server:reply(From, {error, Reason}),
                            {noreply, State#state{pending_auth = NewPending}}
                    end;
                error ->
                    {noreply, State}
            end;
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({auth_timeout, ServerId}, State) ->
    case maps:find(ServerId, State#state.pending_auth) of
        {ok, {From, _StartTime}} ->
            NewPending = maps:remove(ServerId, State#state.pending_auth),
            gen_server:reply(From, {error, auth_timeout}),
            {noreply, State#state{pending_auth = NewPending}};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

start_oauth_flow(ServerId, AuthConfig, From, State) ->
    % Parse OAuth configuration
    Config = parse_oauth_config(AuthConfig),
    
    % Generate state parameter for security
    StateParam = base64:encode(crypto:strong_rand_bytes(32)),
    
    % Build authorization URL
    AuthUrl = build_auth_url(Config, StateParam),
    
    % Store pending auth request
    NewPending = maps:put(ServerId, {From, erlang:system_time(second)}, State#state.pending_auth),
    NewConfigs = maps:put(ServerId, Config, State#state.oauth_configs),
    
    % Set timeout for auth flow (5 minutes)
    erlang:send_after(300000, self(), {auth_timeout, ServerId}),
    
    % Return auth URL to caller who will handle the browser redirect
    gen_server:reply(From, {oauth_redirect, AuthUrl}),
    
    {noreply, State#state{
        pending_auth = NewPending,
        oauth_configs = NewConfigs
    }}.

handle_token_refresh(ServerId, From, State) ->
    case maps:find(ServerId, State#state.auth_tokens) of
        {ok, {_AccessToken, RefreshToken, _ExpiresAt}} ->
            case maps:find(ServerId, State#state.oauth_configs) of
                {ok, Config} ->
                    % Perform token refresh
                    case refresh_oauth_token(RefreshToken, Config) of
                        {ok, NewAccessToken, NewRefreshToken, ExpiresIn} ->
                            ExpiresAt = erlang:system_time(second) + ExpiresIn,
                            NewTokens = maps:put(ServerId, {NewAccessToken, NewRefreshToken, ExpiresAt}, 
                                               State#state.auth_tokens),
                            {reply, {ok, NewAccessToken}, State#state{auth_tokens = NewTokens}};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                error ->
                    {reply, {error, no_config}, State}
            end;
        error ->
            {reply, {error, not_authenticated}, State}
    end.

parse_oauth_config(#{auth_type := oauth2} = Config) ->
    #oauth_config{
        client_id = maps:get(client_id, Config, undefined),
        client_secret = maps:get(client_secret, Config, undefined),
        auth_url = maps:get(auth_url, Config, undefined),
        token_url = maps:get(token_url, Config, undefined),
        redirect_uri = maps:get(redirect_uri, Config, <<"http://localhost:8081/api/oauth/callback">>),
        scopes = maps:get(scopes, Config, <<>>)
    }.

build_auth_url(#oauth_config{auth_url = AuthUrl, client_id = ClientId, 
                            redirect_uri = RedirectUri, scopes = Scopes}, StateParam) ->
    Params = [
        {<<"client_id">>, ClientId},
        {<<"redirect_uri">>, RedirectUri},
        {<<"response_type">>, <<"code">>},
        {<<"state">>, StateParam}
    ],
    
    ParamsWithScope = case Scopes of
        <<>> -> Params;
        _ -> [{<<"scope">>, Scopes} | Params]
    end,
    
    QueryString = uri_string:compose_query(ParamsWithScope),
    <<AuthUrl/binary, "?", QueryString/binary>>.

exchange_auth_code(AuthCode, #oauth_config{token_url = TokenUrl, 
                                          client_id = ClientId,
                                          client_secret = ClientSecret,
                                          redirect_uri = RedirectUri}) ->
    % Make POST request to token endpoint
    Headers = [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    Body = uri_string:compose_query([
        {<<"grant_type">>, <<"authorization_code">>},
        {<<"code">>, AuthCode},
        {<<"client_id">>, ClientId},
        {<<"client_secret">>, ClientSecret},
        {<<"redirect_uri">>, RedirectUri}
    ]),
    
    case httpc:request(post, {binary_to_list(TokenUrl), Headers, 
                            "application/x-www-form-urlencoded", Body}, [], []) of
        {ok, {{_, 200, _}, _Headers, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"access_token">> := AccessToken} = Response ->
                    RefreshToken = maps:get(<<"refresh_token">>, Response, undefined),
                    ExpiresIn = maps:get(<<"expires_in">>, Response, 3600),
                    {ok, AccessToken, RefreshToken, ExpiresIn};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _Headers, ResponseBody}} ->
            error_logger:error_msg("[MCP_OAUTH] Token exchange failed with status ~p: ~s~n", 
                                 [StatusCode, ResponseBody]),
            {error, {oauth_error, StatusCode}};
        {error, Reason} ->
            error_logger:error_msg("[MCP_OAUTH] Token exchange request failed: ~p~n", [Reason]),
            {error, Reason}
    end.

refresh_oauth_token(RefreshToken, #oauth_config{token_url = TokenUrl,
                                              client_id = ClientId,
                                              client_secret = ClientSecret}) ->
    Headers = [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    Body = uri_string:compose_query([
        {<<"grant_type">>, <<"refresh_token">>},
        {<<"refresh_token">>, RefreshToken},
        {<<"client_id">>, ClientId},
        {<<"client_secret">>, ClientSecret}
    ]),
    
    case httpc:request(post, {binary_to_list(TokenUrl), Headers, 
                            "application/x-www-form-urlencoded", Body}, [], []) of
        {ok, {{_, 200, _}, _Headers, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"access_token">> := AccessToken} = Response ->
                    NewRefreshToken = maps:get(<<"refresh_token">>, Response, RefreshToken),
                    ExpiresIn = maps:get(<<"expires_in">>, Response, 3600),
                    {ok, AccessToken, NewRefreshToken, ExpiresIn};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _Headers, _ResponseBody}} ->
            {error, {oauth_error, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.

revoke_oauth_token(AccessToken, #oauth_config{token_url = TokenUrl,
                                            client_id = ClientId,
                                            client_secret = ClientSecret}) ->
    % Build revoke URL - this varies by provider
    RevokeUrl = case binary:match(TokenUrl, <<"token">>) of
        {Start, _} ->
            <<(binary:part(TokenUrl, 0, Start))/binary, "revoke">>;
        nomatch ->
            TokenUrl
    end,
    
    Headers = [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    Body = uri_string:compose_query([
        {<<"token">>, AccessToken},
        {<<"client_id">>, ClientId},
        {<<"client_secret">>, ClientSecret}
    ]),
    
    httpc:request(post, {binary_to_list(RevokeUrl), Headers, 
                        "application/x-www-form-urlencoded", Body}, [], []).