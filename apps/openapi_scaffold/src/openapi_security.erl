%%%-------------------------------------------------------------------
%%% @doc Security Features for OpenAPI Scaffold
%%% OAuth2/JWT authentication, API key rotation, request signing,
%%% rate limiting, and automatic security scanning.
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_security).
-behaviour(gen_server).

-export([
    start_link/0,
    authenticate_request/2,
    authorize_request/3,
    sign_request/2,
    verify_signature/2,
    rotate_api_key/1,
    rate_limit_check/2,
    scan_request/2,
    configure_security/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    jwt_keys :: map(),
    api_keys :: ets:tid(),
    rate_limiter :: ets:tid(),
    security_rules :: map(),
    oauth_providers :: map()
}).

-record(api_key, {
    key :: binary(),
    client_id :: binary(),
    created_at :: integer(),
    expires_at :: integer(),
    permissions :: [binary()],
    rate_limit :: map(),
    status :: active | revoked | expired
}).

-record(rate_limit_entry, {
    key :: {binary(), binary()}, % {client_id, endpoint}
    window_start :: integer(),
    request_count :: integer(),
    tokens :: float()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Authenticate incoming request
authenticate_request(Request, Options) ->
    gen_server:call(?MODULE, {authenticate, Request, Options}).

%% @doc Authorize request for specific resource
authorize_request(Identity, Resource, Action) ->
    gen_server:call(?MODULE, {authorize, Identity, Resource, Action}).

%% @doc Sign outgoing request
sign_request(Request, Options) ->
    gen_server:call(?MODULE, {sign_request, Request, Options}).

%% @doc Verify request signature
verify_signature(Request, Options) ->
    gen_server:call(?MODULE, {verify_signature, Request, Options}).

%% @doc Rotate API key for client
rotate_api_key(ClientId) ->
    gen_server:call(?MODULE, {rotate_key, ClientId}).

%% @doc Check rate limits
rate_limit_check(ClientId, Endpoint) ->
    gen_server:call(?MODULE, {rate_limit, ClientId, Endpoint}).

%% @doc Scan request for security threats
scan_request(Request, Options) ->
    gen_server:call(?MODULE, {scan_request, Request, Options}).

%% @doc Configure security settings
configure_security(Component, Config) ->
    gen_server:call(?MODULE, {configure, Component, Config}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize JWT key storage
    JWTKeys = load_jwt_keys(),
    
    %% Initialize API key storage
    APIKeys = ets:new(api_keys, [
        set,
        private,
        {keypos, #api_key.key}
    ]),
    
    %% Initialize rate limiter
    RateLimiter = ets:new(rate_limiter, [
        set,
        public,
        {keypos, #rate_limit_entry.key},
        {write_concurrency, true}
    ]),
    
    %% Load security rules
    SecurityRules = load_security_rules(),
    
    %% Configure OAuth providers
    OAuthProviders = configure_oauth_providers(),
    
    %% Schedule key rotation check
    timer:send_interval(3600000, check_key_rotation), % Every hour
    
    %% Schedule rate limit cleanup
    timer:send_interval(60000, cleanup_rate_limits), % Every minute
    
    State = #state{
        jwt_keys = JWTKeys,
        api_keys = APIKeys,
        rate_limiter = RateLimiter,
        security_rules = SecurityRules,
        oauth_providers = OAuthProviders
    },
    
    {ok, State}.

handle_call({authenticate, Request, Options}, _From, State) ->
    Result = perform_authentication(Request, Options, State),
    {reply, Result, State};

handle_call({authorize, Identity, Resource, Action}, _From, State) ->
    Result = perform_authorization(Identity, Resource, Action, State),
    {reply, Result, State};

handle_call({sign_request, Request, Options}, _From, State) ->
    SignedRequest = sign_request_internal(Request, Options, State),
    {reply, {ok, SignedRequest}, State};

handle_call({verify_signature, Request, Options}, _From, State) ->
    Result = verify_signature_internal(Request, Options, State),
    {reply, Result, State};

handle_call({rotate_key, ClientId}, _From, State) ->
    NewKey = perform_key_rotation(ClientId, State),
    {reply, {ok, NewKey}, State};

handle_call({rate_limit, ClientId, Endpoint}, _From, State) ->
    Result = check_rate_limit(ClientId, Endpoint, State),
    {reply, Result, State};

handle_call({scan_request, Request, Options}, _From, State) ->
    ScanResult = perform_security_scan(Request, Options, State),
    {reply, ScanResult, State};

handle_call({configure, Component, Config}, _From, State) ->
    NewState = update_security_config(Component, Config, State),
    {reply, ok, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_key_rotation, State) ->
    %% Check for keys that need rotation
    perform_scheduled_rotation(State),
    {noreply, State};

handle_info(cleanup_rate_limits, State) ->
    %% Clean up old rate limit entries
    cleanup_old_rate_limits(State#state.rate_limiter),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Authentication
%%====================================================================

perform_authentication(Request, Options, State) ->
    %% Extract authentication credentials
    AuthHeader = maps:get(<<"authorization">>, maps:get(headers, Request, #{}), undefined),
    
    case AuthHeader of
        undefined ->
            %% Try API key authentication
            authenticate_api_key(Request, State);
        <<"Bearer ", Token/binary>> ->
            %% JWT authentication
            authenticate_jwt(Token, State);
        <<"Basic ", Credentials/binary>> ->
            %% Basic authentication
            authenticate_basic(Credentials, State);
        _ ->
            %% Try OAuth2
            authenticate_oauth(Request, Options, State)
    end.

authenticate_jwt(Token, State) ->
    %% Decode and verify JWT
    case jwt:decode(Token) of
        {ok, Claims} ->
            %% Verify signature
            case verify_jwt_signature(Token, Claims, State#state.jwt_keys) of
                ok ->
                    %% Check expiration
                    case check_jwt_expiration(Claims) of
                        ok -> {ok, #{type => jwt, claims => Claims}};
                        expired -> {error, token_expired}
                    end;
                error ->
                    {error, invalid_signature}
            end;
        error ->
            {error, invalid_token}
    end.

authenticate_api_key(Request, State) ->
    %% Extract API key from header or query param
    APIKey = extract_api_key(Request),
    
    case APIKey of
        undefined ->
            {error, no_credentials};
        Key ->
            case ets:lookup(State#state.api_keys, Key) of
                [#api_key{status = active, expires_at = ExpiresAt} = KeyRecord] ->
                    Now = erlang:system_time(second),
                    if
                        ExpiresAt > Now ->
                            {ok, #{type => api_key, key_info => KeyRecord}};
                        true ->
                            %% Mark as expired
                            UpdatedKey = KeyRecord#api_key{status = expired},
                            ets:insert(State#state.api_keys, UpdatedKey),
                            {error, key_expired}
                    end;
                [#api_key{status = Status}] ->
                    {error, {key_status, Status}};
                [] ->
                    {error, invalid_api_key}
            end
    end.

authenticate_basic(Credentials, State) ->
    %% Decode base64 credentials
    case base64:decode(Credentials) of
        {ok, Decoded} ->
            case binary:split(Decoded, <<":">>) of
                [Username, Password] ->
                    %% Verify against user database
                    verify_basic_auth(Username, Password, State);
                _ ->
                    {error, invalid_credentials}
            end;
        error ->
            {error, invalid_credentials}
    end.

authenticate_oauth(Request, Options, State) ->
    %% Extract OAuth token
    Provider = maps:get(oauth_provider, Options, default),
    Token = extract_oauth_token(Request),
    
    case Token of
        undefined ->
            {error, no_oauth_token};
        _ ->
            %% Validate with OAuth provider
            validate_oauth_token(Token, Provider, State#state.oauth_providers)
    end.

%%====================================================================
%% Internal functions - Authorization
%%====================================================================

perform_authorization(Identity, Resource, Action, State) ->
    %% Get user permissions
    Permissions = extract_permissions(Identity),
    
    %% Check against security rules
    Rules = maps:get(Resource, State#state.security_rules, []),
    
    %% Evaluate rules
    case evaluate_rules(Rules, Action, Permissions, Identity) of
        allow -> {ok, authorized};
        deny -> {error, forbidden};
        undefined -> 
            %% Default deny
            {error, forbidden}
    end.

extract_permissions(#{type := jwt, claims := Claims}) ->
    maps:get(<<"permissions">>, Claims, []);
extract_permissions(#{type := api_key, key_info := #api_key{permissions = Perms}}) ->
    Perms;
extract_permissions(_) ->
    [].

evaluate_rules([], _Action, _Permissions, _Identity) ->
    undefined;
evaluate_rules([Rule | Rest], Action, Permissions, Identity) ->
    case evaluate_single_rule(Rule, Action, Permissions, Identity) of
        allow -> allow;
        deny -> deny;
        continue -> evaluate_rules(Rest, Action, Permissions, Identity)
    end.

evaluate_single_rule(#{actions := Actions, effect := Effect} = Rule, Action, Permissions, Identity) ->
    %% Check if action matches
    ActionMatches = lists:member(Action, Actions) orelse lists:member(<<"*">>, Actions),
    
    if
        not ActionMatches -> continue;
        true ->
            %% Check conditions
            case check_rule_conditions(Rule, Permissions, Identity) of
                true -> Effect;
                false -> continue
            end
    end.

check_rule_conditions(#{conditions := Conditions}, Permissions, Identity) ->
    lists:all(fun(Condition) ->
        check_single_condition(Condition, Permissions, Identity)
    end, Conditions);
check_rule_conditions(_, _, _) ->
    true. % No conditions means rule applies

check_single_condition(#{type := <<"permission">>, value := Required}, Permissions, _) ->
    lists:member(Required, Permissions);
check_single_condition(#{type := <<"time">>, start := Start, end_ := End}, _, _) ->
    Now = erlang:system_time(second),
    Now >= Start andalso Now =< End;
check_single_condition(_, _, _) ->
    false.

%%====================================================================
%% Internal functions - Request Signing
%%====================================================================

sign_request_internal(Request, Options, State) ->
    %% Get signing method
    Method = maps:get(signing_method, Options, hmac_sha256),
    
    %% Get signing key
    Key = get_signing_key(Options, State),
    
    %% Create signature
    Signature = case Method of
        hmac_sha256 -> sign_hmac_sha256(Request, Key);
        rsa_sha256 -> sign_rsa_sha256(Request, Key);
        aws_v4 -> sign_aws_v4(Request, Key, Options)
    end,
    
    %% Add signature to request
    add_signature_to_request(Request, Signature, Method).

sign_hmac_sha256(Request, Key) ->
    %% Create canonical request
    CanonicalRequest = create_canonical_request(Request),
    
    %% Sign with HMAC-SHA256
    crypto:mac(hmac, sha256, Key, CanonicalRequest).

sign_rsa_sha256(Request, PrivateKey) ->
    %% Create canonical request
    CanonicalRequest = create_canonical_request(Request),
    
    %% Sign with RSA-SHA256
    public_key:sign(CanonicalRequest, sha256, PrivateKey).

sign_aws_v4(Request, Credentials, Options) ->
    %% Implement AWS Signature Version 4
    Region = maps:get(region, Options, <<"us-east-1">>),
    Service = maps:get(service, Options, <<"execute-api">>),
    
    %% Create canonical request
    CanonicalRequest = create_aws_canonical_request(Request),
    
    %% Create string to sign
    StringToSign = create_aws_string_to_sign(CanonicalRequest, Region, Service),
    
    %% Calculate signature
    SigningKey = derive_aws_signing_key(Credentials, Region, Service),
    crypto:mac(hmac, sha256, SigningKey, StringToSign).

create_canonical_request(#{method := Method, path := Path, headers := Headers, body := Body}) ->
    %% Sort headers
    SortedHeaders = lists:sort(maps:to_list(Headers)),
    HeaderString = lists:foldl(fun({K, V}, Acc) ->
        [Acc, K, <<":">>, V, <<"\n">>]
    end, <<>>, SortedHeaders),
    
    %% Create canonical representation
    iolist_to_binary([
        Method, <<"\n">>,
        Path, <<"\n">>,
        HeaderString, <<"\n">>,
        crypto:hash(sha256, Body)
    ]).

%%====================================================================
%% Internal functions - Rate Limiting
%%====================================================================

check_rate_limit(ClientId, Endpoint, State) ->
    Key = {ClientId, Endpoint},
    Now = erlang:system_time(second),
    
    %% Get rate limit configuration
    {Limit, Window, Algorithm} = get_rate_limit_config(ClientId, Endpoint, State),
    
    case Algorithm of
        token_bucket ->
            check_token_bucket(Key, Limit, Window, Now, State#state.rate_limiter);
        sliding_window ->
            check_sliding_window(Key, Limit, Window, Now, State#state.rate_limiter);
        fixed_window ->
            check_fixed_window(Key, Limit, Window, Now, State#state.rate_limiter)
    end.

check_token_bucket(Key, Limit, Window, Now, Table) ->
    %% Token bucket with refill
    RefillRate = Limit / Window,
    
    case ets:lookup(Table, Key) of
        [] ->
            %% First request, initialize bucket
            Entry = #rate_limit_entry{
                key = Key,
                window_start = Now,
                tokens = Limit - 1.0
            },
            ets:insert(Table, Entry),
            {ok, #{remaining => Limit - 1, reset => Now + Window}};
        
        [#rate_limit_entry{window_start = LastUpdate, tokens = Tokens} = Entry] ->
            %% Calculate tokens added since last request
            ElapsedTime = Now - LastUpdate,
            NewTokens = min(Limit, Tokens + (ElapsedTime * RefillRate)),
            
            if
                NewTokens >= 1.0 ->
                    %% Allow request
                    UpdatedEntry = Entry#rate_limit_entry{
                        window_start = Now,
                        tokens = NewTokens - 1.0
                    },
                    ets:insert(Table, UpdatedEntry),
                    {ok, #{remaining => trunc(NewTokens - 1), reset => Now + Window}};
                true ->
                    %% Deny request
                    TimeToNextToken = (1.0 - NewTokens) / RefillRate,
                    {error, #{
                        limit_exceeded => true,
                        retry_after => ceil(TimeToNextToken),
                        reset => Now + ceil(TimeToNextToken)
                    }}
            end
    end.

check_sliding_window(Key, Limit, Window, Now, Table) ->
    %% Sliding window log algorithm
    WindowStart = Now - Window,
    
    case ets:lookup(Table, Key) of
        [] ->
            %% First request
            Entry = #rate_limit_entry{
                key = Key,
                window_start = Now,
                request_count = 1
            },
            ets:insert(Table, Entry),
            {ok, #{remaining => Limit - 1, reset => Now + Window}};
        
        [Entry] ->
            %% Count requests in current window
            %% In production, would maintain a list of timestamps
            CurrentCount = Entry#rate_limit_entry.request_count,
            
            if
                CurrentCount < Limit ->
                    UpdatedEntry = Entry#rate_limit_entry{
                        request_count = CurrentCount + 1
                    },
                    ets:insert(Table, UpdatedEntry),
                    {ok, #{remaining => Limit - CurrentCount - 1, reset => Now + Window}};
                true ->
                    {error, #{
                        limit_exceeded => true,
                        retry_after => Window,
                        reset => Now + Window
                    }}
            end
    end.

check_fixed_window(Key, Limit, Window, Now, Table) ->
    %% Fixed window counter
    WindowNumber = Now div Window,
    
    case ets:lookup(Table, Key) of
        [] ->
            %% First request
            Entry = #rate_limit_entry{
                key = Key,
                window_start = WindowNumber,
                request_count = 1
            },
            ets:insert(Table, Entry),
            {ok, #{remaining => Limit - 1, reset => (WindowNumber + 1) * Window}};
        
        [#rate_limit_entry{window_start = WinNum, request_count = Count} = Entry] ->
            if
                WinNum < WindowNumber ->
                    %% New window, reset counter
                    UpdatedEntry = Entry#rate_limit_entry{
                        window_start = WindowNumber,
                        request_count = 1
                    },
                    ets:insert(Table, UpdatedEntry),
                    {ok, #{remaining => Limit - 1, reset => (WindowNumber + 1) * Window}};
                
                Count < Limit ->
                    %% Within limit
                    UpdatedEntry = Entry#rate_limit_entry{
                        request_count = Count + 1
                    },
                    ets:insert(Table, UpdatedEntry),
                    {ok, #{remaining => Limit - Count - 1, reset => (WindowNumber + 1) * Window}};
                
                true ->
                    %% Limit exceeded
                    ResetTime = (WindowNumber + 1) * Window,
                    {error, #{
                        limit_exceeded => true,
                        retry_after => ResetTime - Now,
                        reset => ResetTime
                    }}
            end
    end.

get_rate_limit_config(ClientId, Endpoint, State) ->
    %% Get client-specific or default rate limit
    %% Returns {requests_per_window, window_seconds, algorithm}
    Default = {100, 60, token_bucket},
    
    %% Check for client-specific limits
    case lookup_client_limits(ClientId, State) of
        undefined -> Default;
        ClientLimits ->
            %% Check for endpoint-specific limits
            maps:get(Endpoint, ClientLimits, maps:get(default, ClientLimits, Default))
    end.

%%====================================================================
%% Internal functions - Security Scanning
%%====================================================================

perform_security_scan(Request, Options, State) ->
    %% Run multiple security checks
    Checks = [
        {sql_injection, fun check_sql_injection/2},
        {xss, fun check_xss/2},
        {path_traversal, fun check_path_traversal/2},
        {command_injection, fun check_command_injection/2},
        {xxe, fun check_xxe/2},
        {csrf, fun check_csrf/2}
    ],
    
    %% Run checks in parallel
    Results = lists:map(fun({Name, CheckFun}) ->
        {Name, CheckFun(Request, State)}
    end, Checks),
    
    %% Aggregate results
    Threats = [Name || {Name, {threat, _}} <- Results],
    
    case Threats of
        [] -> 
            {ok, safe};
        _ ->
            Details = [{Name, Details} || {Name, {threat, Details}} <- Results],
            {error, {security_threats, Details}}
    end.

check_sql_injection(Request, _State) ->
    %% Check for SQL injection patterns
    Patterns = [
        <<"union.*select">>,
        <<"drop.*table">>,
        <<"insert.*into">>,
        <<"delete.*from">>,
        <<"update.*set">>,
        <<"script.*>">>,
        <<"'.*or.*'.*=.*'">>
    ],
    
    %% Check all request fields
    Fields = extract_all_fields(Request),
    
    case find_pattern_match(Fields, Patterns) of
        {match, Field, Pattern} ->
            {threat, #{field => Field, pattern => Pattern}};
        nomatch ->
            safe
    end.

check_xss(Request, _State) ->
    %% Check for XSS patterns
    Patterns = [
        <<"<script">>,
        <<"javascript:">>,
        <<"onerror=">>,
        <<"onload=">>,
        <<"<iframe">>,
        <<"<object">>,
        <<"<embed">>
    ],
    
    Fields = extract_all_fields(Request),
    
    case find_pattern_match(Fields, Patterns) of
        {match, Field, Pattern} ->
            {threat, #{field => Field, pattern => Pattern}};
        nomatch ->
            safe
    end.

check_path_traversal(Request, _State) ->
    %% Check for path traversal attempts
    PathPatterns = [
        <<"../">>,
        <<"..\\">>,
        <<"%2e%2e%2f">>,
        <<"%2e%2e%5c">>
    ],
    
    Path = maps:get(path, Request, <<>>),
    
    case find_single_pattern_match(Path, PathPatterns) of
        {match, Pattern} ->
            {threat, #{path => Path, pattern => Pattern}};
        nomatch ->
            safe
    end.

check_command_injection(Request, _State) ->
    %% Check for command injection patterns
    Patterns = [
        <<"; ">>,
        <<"&&">>,
        <<"||">>,
        <<"`">>,
        <<"$(">>,
        <<"|">>
    ],
    
    Fields = extract_all_fields(Request),
    
    case find_pattern_match(Fields, Patterns) of
        {match, Field, Pattern} ->
            {threat, #{field => Field, pattern => Pattern}};
        nomatch ->
            safe
    end.

check_xxe(Request, _State) ->
    %% Check for XXE patterns in XML content
    Body = maps:get(body, Request, <<>>),
    ContentType = maps:get(<<"content-type">>, maps:get(headers, Request, #{}), <<>>),
    
    case binary:match(ContentType, <<"xml">>) of
        nomatch -> safe;
        _ ->
            XXEPatterns = [
                <<"<!ENTITY">>,
                <<"<!DOCTYPE">>,
                <<"SYSTEM">>,
                <<"file://">>,
                <<"http://">>
            ],
            
            case find_single_pattern_match(Body, XXEPatterns) of
                {match, Pattern} ->
                    {threat, #{body => Body, pattern => Pattern}};
                nomatch ->
                    safe
            end
    end.

check_csrf(Request, State) ->
    %% Check for CSRF protection
    Method = maps:get(method, Request),
    
    case lists:member(Method, [<<"POST">>, <<"PUT">>, <<"DELETE">>, <<"PATCH">>]) of
        false -> safe;
        true ->
            %% Check for CSRF token
            Token = extract_csrf_token(Request),
            case Token of
                undefined ->
                    {threat, #{reason => missing_csrf_token}};
                _ ->
                    %% Verify token
                    case verify_csrf_token(Token, State) of
                        ok -> safe;
                        error -> {threat, #{reason => invalid_csrf_token}}
                    end
            end
    end.

%%====================================================================
%% Internal functions - Utilities
%%====================================================================

load_jwt_keys() ->
    %% Load JWT signing keys from configuration
    #{
        <<"default">> => load_key_file("priv/jwt/public_key.pem"),
        <<"rsa">> => load_key_file("priv/jwt/rsa_public.pem")
    }.

load_security_rules() ->
    %% Load security rules from configuration
    #{
        <<"/api/admin/*">> => [
            #{
                actions => [<<"*">>],
                effect => allow,
                conditions => [
                    #{type => <<"permission">>, value => <<"admin">>}
                ]
            }
        ],
        <<"/api/users/*">> => [
            #{
                actions => [<<"GET">>],
                effect => allow,
                conditions => []
            },
            #{
                actions => [<<"POST">>, <<"PUT">>, <<"DELETE">>],
                effect => allow,
                conditions => [
                    #{type => <<"permission">>, value => <<"user:write">>}
                ]
            }
        ]
    }.

configure_oauth_providers() ->
    %% Configure OAuth providers
    #{
        google => #{
            token_endpoint => <<"https://oauth2.googleapis.com/tokeninfo">>,
            userinfo_endpoint => <<"https://www.googleapis.com/oauth2/v2/userinfo">>,
            client_id => os:getenv("GOOGLE_CLIENT_ID"),
            client_secret => os:getenv("GOOGLE_CLIENT_SECRET")
        },
        github => #{
            token_endpoint => <<"https://api.github.com/user">>,
            client_id => os:getenv("GITHUB_CLIENT_ID"),
            client_secret => os:getenv("GITHUB_CLIENT_SECRET")
        }
    }.

extract_api_key(Request) ->
    Headers = maps:get(headers, Request, #{}),
    QueryParams = maps:get(query_params, Request, #{}),
    
    %% Check header first
    case maps:get(<<"x-api-key">>, Headers, undefined) of
        undefined ->
            %% Check query parameter
            maps:get(<<"api_key">>, QueryParams, undefined);
        Key ->
            Key
    end.

extract_all_fields(Request) ->
    %% Extract all text fields from request for scanning
    Headers = maps:get(headers, Request, #{}),
    QueryParams = maps:get(query_params, Request, #{}),
    Body = maps:get(body, Request, #{}),
    
    %% Flatten all fields
    HeaderFields = maps:to_list(Headers),
    QueryFields = maps:to_list(QueryParams),
    BodyFields = case Body of
        Map when is_map(Map) -> flatten_map(Map);
        Binary when is_binary(Binary) -> [{body, Binary}];
        _ -> []
    end,
    
    HeaderFields ++ QueryFields ++ BodyFields.

flatten_map(Map) ->
    flatten_map(Map, []).

flatten_map(Map, Path) ->
    maps:fold(fun(K, V, Acc) ->
        NewPath = Path ++ [K],
        case V of
            M when is_map(M) ->
                flatten_map(M, NewPath) ++ Acc;
            L when is_list(L) ->
                [{NewPath, iolist_to_binary(io_lib:format("~p", [L]))} | Acc];
            B when is_binary(B) ->
                [{NewPath, B} | Acc];
            Other ->
                [{NewPath, iolist_to_binary(io_lib:format("~p", [Other]))} | Acc]
        end
    end, [], Map).

find_pattern_match(Fields, Patterns) ->
    lists:foldl(fun({FieldName, FieldValue}, Acc) ->
        case Acc of
            {match, _, _} -> Acc;
            nomatch ->
                case find_single_pattern_match(FieldValue, Patterns) of
                    {match, Pattern} -> {match, FieldName, Pattern};
                    nomatch -> nomatch
                end
        end
    end, nomatch, Fields).

find_single_pattern_match(Value, Patterns) ->
    lists:foldl(fun(Pattern, Acc) ->
        case Acc of
            {match, _} -> Acc;
            nomatch ->
                case re:run(Value, Pattern, [caseless]) of
                    {match, _} -> {match, Pattern};
                    nomatch -> nomatch
                end
        end
    end, nomatch, Patterns).

verify_jwt_signature(Token, Claims, Keys) ->
    %% Get key ID from token header
    Alg = maps:get(<<"alg">>, Claims, <<"RS256">>),
    Kid = maps:get(<<"kid">>, Claims, <<"default">>),
    
    %% Get corresponding key
    case maps:get(Kid, Keys, undefined) of
        undefined -> error;
        Key ->
            %% Verify based on algorithm
            case Alg of
                <<"RS256">> -> verify_rsa_signature(Token, Key);
                <<"HS256">> -> verify_hmac_signature(Token, Key);
                _ -> error
            end
    end.

check_jwt_expiration(Claims) ->
    Now = erlang:system_time(second),
    Exp = maps:get(<<"exp">>, Claims, 0),
    
    if
        Exp > Now -> ok;
        true -> expired
    end.

perform_key_rotation(ClientId, State) ->
    %% Generate new API key
    NewKey = generate_secure_key(),
    
    %% Get existing key info
    OldKeys = ets:match_object(State#state.api_keys, #api_key{client_id = ClientId, _ = '_'}),
    
    %% Create new key record
    NewKeyRecord = case OldKeys of
        [OldKey | _] ->
            #api_key{
                key = NewKey,
                client_id = ClientId,
                created_at = erlang:system_time(second),
                expires_at = erlang:system_time(second) + (365 * 24 * 60 * 60), % 1 year
                permissions = OldKey#api_key.permissions,
                rate_limit = OldKey#api_key.rate_limit,
                status = active
            };
        [] ->
            #api_key{
                key = NewKey,
                client_id = ClientId,
                created_at = erlang:system_time(second),
                expires_at = erlang:system_time(second) + (365 * 24 * 60 * 60),
                permissions = [],
                rate_limit = #{},
                status = active
            }
    end,
    
    %% Insert new key
    ets:insert(State#state.api_keys, NewKeyRecord),
    
    %% Mark old keys as revoked
    [ets:update_element(State#state.api_keys, K#api_key.key, 
                       {#api_key.status, revoked}) || K <- OldKeys],
    
    NewKey.

generate_secure_key() ->
    %% Generate cryptographically secure API key
    Bytes = crypto:strong_rand_bytes(32),
    base64:encode(Bytes).

cleanup_old_rate_limits(Table) ->
    %% Remove rate limit entries older than 1 hour
    CutoffTime = erlang:system_time(second) - 3600,
    
    MatchSpec = [{#rate_limit_entry{window_start = '$1', _ = '_'},
                  [{'<', '$1', CutoffTime}],
                  [true]}],
    
    ets:select_delete(Table, MatchSpec).

load_key_file(Path) ->
    case file:read_file(Path) of
        {ok, KeyData} -> KeyData;
        {error, _} -> undefined
    end.

verify_basic_auth(_Username, _Password, _State) ->
    %% In production, would verify against user database
    {error, not_implemented}.

extract_oauth_token(Request) ->
    Headers = maps:get(headers, Request, #{}),
    
    case maps:get(<<"authorization">>, Headers, undefined) of
        <<"Bearer ", Token/binary>> -> Token;
        _ -> undefined
    end.

validate_oauth_token(Token, Provider, Providers) ->
    case maps:get(Provider, Providers, undefined) of
        undefined -> {error, unknown_provider};
        ProviderConfig ->
            %% Call provider's token validation endpoint
            validate_with_provider(Token, ProviderConfig)
    end.

validate_with_provider(_Token, _Config) ->
    %% In production, would make HTTP request to provider
    {error, not_implemented}.

get_signing_key(Options, _State) ->
    %% Get signing key from options or configuration
    maps:get(signing_key, Options, <<"default-signing-key">>).

add_signature_to_request(Request, Signature, Method) ->
    Headers = maps:get(headers, Request, #{}),
    
    SignatureHeader = case Method of
        hmac_sha256 -> <<"X-Signature">>;
        rsa_sha256 -> <<"X-RSA-Signature">>;
        aws_v4 -> <<"Authorization">>
    end,
    
    UpdatedHeaders = maps:put(SignatureHeader, base64:encode(Signature), Headers),
    maps:put(headers, UpdatedHeaders, Request).

create_aws_canonical_request(_Request) ->
    %% Simplified - would implement full AWS canonical request format
    <<"canonical-request">>.

create_aws_string_to_sign(_CanonicalRequest, _Region, _Service) ->
    %% Simplified - would implement full AWS string to sign
    <<"string-to-sign">>.

derive_aws_signing_key(_Credentials, _Region, _Service) ->
    %% Simplified - would implement full AWS key derivation
    <<"signing-key">>.

verify_signature_internal(_Request, _Options, _State) ->
    %% Would implement signature verification
    {ok, verified}.

update_security_config(Component, Config, State) ->
    %% Update security configuration for specific component
    case Component of
        jwt_keys ->
            State#state{jwt_keys = maps:merge(State#state.jwt_keys, Config)};
        security_rules ->
            State#state{security_rules = maps:merge(State#state.security_rules, Config)};
        oauth_providers ->
            State#state{oauth_providers = maps:merge(State#state.oauth_providers, Config)};
        _ ->
            State
    end.

perform_scheduled_rotation(State) ->
    %% Check all API keys for rotation need
    Now = erlang:system_time(second),
    RotationPeriod = 90 * 24 * 60 * 60, % 90 days
    
    AllKeys = ets:tab2list(State#state.api_keys),
    
    lists:foreach(fun(#api_key{created_at = CreatedAt, client_id = ClientId}) ->
        Age = Now - CreatedAt,
        if
            Age > RotationPeriod ->
                %% Trigger rotation notification
                notify_key_rotation_needed(ClientId);
            true ->
                ok
        end
    end, AllKeys).

notify_key_rotation_needed(ClientId) ->
    %% Send notification about key rotation
    error_logger:info_msg("API key rotation needed for client: ~p~n", [ClientId]).

lookup_client_limits(_ClientId, _State) ->
    %% Would look up client-specific rate limits
    undefined.

extract_csrf_token(Request) ->
    Headers = maps:get(headers, Request, #{}),
    maps:get(<<"x-csrf-token">>, Headers, undefined).

verify_csrf_token(_Token, _State) ->
    %% Would verify CSRF token
    ok.

verify_rsa_signature(_Token, _Key) ->
    %% Would verify RSA signature
    ok.

verify_hmac_signature(_Token, _Key) ->
    %% Would verify HMAC signature
    ok.