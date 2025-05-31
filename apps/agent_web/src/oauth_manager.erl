-module(oauth_manager).
-behaviour(gen_server).

%% API
-export([start_link/0,
         store_token/3,
         get_token/2,
         refresh_token/2,
         revoke_token/2,
         get_all_tokens/1,
         init_oauth_system/0,
         cleanup_expired_tokens/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% OAuth provider callbacks
-export([handle_oauth_callback/3]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(SERVER, ?MODULE).
-define(TOKEN_REFRESH_INTERVAL, 300000). % 5 minutes
-define(TOKEN_TABLE, oauth_tokens).
-define(ENCRYPTION_KEY, <<"agents_erl_oauth_encryption_key_2025">>).

-record(oauth_token, {
    key,                    % {server_id, provider}
    access_token,           % encrypted
    refresh_token,          % encrypted
    expires_at,
    token_type,
    scope,
    created_at,
    updated_at,
    metadata = #{}
}).

-record(state, {
    refresh_timer,
    providers = #{}
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

store_token(ServerId, Provider, TokenData) ->
    gen_server:call(?SERVER, {store_token, ServerId, Provider, TokenData}).

get_token(ServerId, Provider) ->
    gen_server:call(?SERVER, {get_token, ServerId, Provider}).

refresh_token(ServerId, Provider) ->
    gen_server:call(?SERVER, {refresh_token, ServerId, Provider}).

revoke_token(ServerId, Provider) ->
    gen_server:call(?SERVER, {revoke_token, ServerId, Provider}).

get_all_tokens(ServerId) ->
    gen_server:call(?SERVER, {get_all_tokens, ServerId}).

init_oauth_system() ->
    gen_server:call(?SERVER, init_oauth_system).

cleanup_expired_tokens() ->
    gen_server:cast(?SERVER, cleanup_expired_tokens).

handle_oauth_callback(Provider, Code, State) ->
    gen_server:call(?SERVER, {oauth_callback, Provider, Code, State}).

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    
    % Create ETS table for token storage
    ets:new(?TOKEN_TABLE, [named_table, set, protected, 
                          {keypos, #oauth_token.key}]),
    
    % Initialize OAuth providers
    Providers = init_providers(),
    
    % Start token refresh timer
    Timer = erlang:send_after(?TOKEN_REFRESH_INTERVAL, self(), refresh_tokens),
    
    State = #state{
        refresh_timer = Timer,
        providers = Providers
    },
    
    {ok, State}.

handle_call({store_token, ServerId, Provider, TokenData}, _From, State) ->
    Key = {ServerId, Provider},
    
    % Encrypt sensitive tokens
    EncryptedAccess = encrypt_token(maps:get(access_token, TokenData)),
    EncryptedRefresh = encrypt_token(maps:get(refresh_token, TokenData, <<>>)),
    
    Token = #oauth_token{
        key = Key,
        access_token = EncryptedAccess,
        refresh_token = EncryptedRefresh,
        expires_at = calculate_expiry(TokenData),
        token_type = maps:get(token_type, TokenData, <<"Bearer">>),
        scope = maps:get(scope, TokenData, <<>>),
        created_at = erlang:system_time(second),
        updated_at = erlang:system_time(second),
        metadata = maps:get(metadata, TokenData, #{})
    },
    
    ets:insert(?TOKEN_TABLE, Token),
    {reply, ok, State};

handle_call({get_token, ServerId, Provider}, _From, State) ->
    Key = {ServerId, Provider},
    case ets:lookup(?TOKEN_TABLE, Key) of
        [#oauth_token{expires_at = ExpiresAt} = Token] ->
            Now = erlang:system_time(second),
            if
                ExpiresAt > Now ->
                    % Token is valid, decrypt and return
                    DecryptedToken = Token#oauth_token{
                        access_token = decrypt_token(Token#oauth_token.access_token),
                        refresh_token = decrypt_token(Token#oauth_token.refresh_token)
                    },
                    {reply, {ok, DecryptedToken}, State};
                true ->
                    % Token expired, try to refresh
                    case auto_refresh_token(ServerId, Provider, Token, State) of
                        {ok, NewToken} ->
                            {reply, {ok, NewToken}, State};
                        {error, Reason} ->
                            {reply, {error, {expired, Reason}}, State}
                    end
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({refresh_token, ServerId, Provider}, _From, State) ->
    Key = {ServerId, Provider},
    case ets:lookup(?TOKEN_TABLE, Key) of
        [Token] ->
            case auto_refresh_token(ServerId, Provider, Token, State) of
                {ok, NewToken} ->
                    {reply, {ok, NewToken}, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({revoke_token, ServerId, Provider}, _From, State) ->
    Key = {ServerId, Provider},
    case ets:lookup(?TOKEN_TABLE, Key) of
        [Token] ->
            % Call provider's revoke endpoint
            case revoke_with_provider(Provider, Token, State) of
                ok ->
                    ets:delete(?TOKEN_TABLE, Key),
                    {reply, ok, State};
                {error, Reason} ->
                    % Still delete local token even if revoke fails
                    ets:delete(?TOKEN_TABLE, Key),
                    {reply, {ok, {warning, Reason}}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_all_tokens, ServerId}, _From, State) ->
    Pattern = ets:fun2ms(fun(#oauth_token{key = {S, _}} = T) when S =:= ServerId -> T end),
    Tokens = ets:select(?TOKEN_TABLE, Pattern),
    DecryptedTokens = [Token#oauth_token{
        access_token = decrypt_token(Token#oauth_token.access_token),
        refresh_token = decrypt_token(Token#oauth_token.refresh_token)
    } || Token <- Tokens],
    {reply, {ok, DecryptedTokens}, State};

handle_call(init_oauth_system, _From, State) ->
    try
        % Initialize OAuth provider configurations
        ok = init_provider_configs(),
        % Set up OAuth endpoints
        ok = setup_oauth_endpoints(),
        {reply, ok, State}
    catch
        Class:Reason:Stacktrace ->
            io:format("[OAUTH] Initialization error: ~p:~p~n~p~n", 
                     [Class, Reason, Stacktrace]),
            {reply, {error, {Class, Reason}}, State}
    end;

handle_call({oauth_callback, Provider, Code, CallbackState}, _From, State) ->
    case exchange_code_for_token(Provider, Code, CallbackState, State) of
        {ok, TokenData} ->
            % Extract server_id from callback state
            ServerId = maps:get(server_id, CallbackState, Provider),
            store_token(ServerId, Provider, TokenData),
            {reply, {ok, TokenData}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(cleanup_expired_tokens, State) ->
    Now = erlang:system_time(second),
    Pattern = ets:fun2ms(fun(#oauth_token{expires_at = E}) when E =< Now -> true end),
    ets:select_delete(?TOKEN_TABLE, Pattern),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh_tokens, State) ->
    % Cancel old timer
    erlang:cancel_timer(State#state.refresh_timer),
    
    % Refresh tokens that are close to expiry
    refresh_expiring_tokens(),
    
    % Schedule next refresh
    NewTimer = erlang:send_after(?TOKEN_REFRESH_INTERVAL, self(), refresh_tokens),
    {noreply, State#state{refresh_timer = NewTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    erlang:cancel_timer(State#state.refresh_timer),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
init_providers() ->
    #{
        <<"github">> => #{
            auth_url => <<"https://github.com/login/oauth/authorize">>,
            token_url => <<"https://github.com/login/oauth/access_token">>,
            revoke_url => <<"https://api.github.com/applications/{client_id}/token">>,
            scopes => [<<"repo">>, <<"user">>]
        },
        <<"google">> => #{
            auth_url => <<"https://accounts.google.com/o/oauth2/v2/auth">>,
            token_url => <<"https://oauth2.googleapis.com/token">>,
            revoke_url => <<"https://oauth2.googleapis.com/revoke">>,
            scopes => [<<"openid">>, <<"email">>, <<"profile">>]
        },
        <<"slack">> => #{
            auth_url => <<"https://slack.com/oauth/v2/authorize">>,
            token_url => <<"https://slack.com/api/oauth.v2.access">>,
            revoke_url => <<"https://slack.com/api/auth.revoke">>,
            scopes => [<<"chat:write">>, <<"channels:read">>]
        }
    }.

encrypt_token(Token) when is_binary(Token) ->
    % Simple XOR encryption - in production use proper crypto
    crypto:crypto_one_time(aes_256_ctr, 
                          crypto:hash(sha256, ?ENCRYPTION_KEY), 
                          <<0:128>>, Token, true);
encrypt_token(_) ->
    <<>>.

decrypt_token(EncryptedToken) when is_binary(EncryptedToken) ->
    crypto:crypto_one_time(aes_256_ctr, 
                          crypto:hash(sha256, ?ENCRYPTION_KEY), 
                          <<0:128>>, EncryptedToken, false);
decrypt_token(_) ->
    <<>>.

calculate_expiry(TokenData) ->
    ExpiresIn = maps:get(expires_in, TokenData, 3600),
    erlang:system_time(second) + ExpiresIn.

auto_refresh_token(ServerId, Provider, Token, State) ->
    RefreshToken = decrypt_token(Token#oauth_token.refresh_token),
    case RefreshToken of
        <<>> ->
            {error, no_refresh_token};
        _ ->
            ProviderConfig = maps:get(Provider, State#state.providers, #{}),
            refresh_with_provider(Provider, RefreshToken, ProviderConfig)
    end.

refresh_with_provider(Provider, RefreshToken, Config) ->
    % Implementation would make HTTP request to provider's token endpoint
    % This is a placeholder
    {error, not_implemented}.

revoke_with_provider(Provider, Token, State) ->
    % Implementation would make HTTP request to provider's revoke endpoint
    % This is a placeholder
    ok.

exchange_code_for_token(Provider, Code, CallbackState, State) ->
    % Implementation would exchange authorization code for access token
    % This is a placeholder
    {error, not_implemented}.

refresh_expiring_tokens() ->
    % Refresh tokens expiring in next 10 minutes
    ThresholdTime = erlang:system_time(second) + 600,
    Pattern = ets:fun2ms(fun(#oauth_token{expires_at = E, key = K}) 
                           when E =< ThresholdTime -> K end),
    
    Keys = ets:select(?TOKEN_TABLE, Pattern),
    lists:foreach(fun({ServerId, Provider}) ->
        spawn(fun() -> refresh_token(ServerId, Provider) end)
    end, Keys).

init_provider_configs() ->
    % Initialize provider-specific configurations
    ok.

setup_oauth_endpoints() ->
    % Set up OAuth callback endpoints
    ok.