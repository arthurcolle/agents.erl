%%%-------------------------------------------------------------------
%%% @doc Bearer-token and same-origin session gate for the HTTP control plane.
%%%
%%% The dashboard and liveness endpoint remain public.  All API, websocket,
%%% and MCP control-plane traffic requires AGENTS_WEB_API_TOKEN unless an
%%% operator explicitly disables this middleware for a local development
%%% deployment with agent_web.api_auth_enabled = false.
%%% @end
%%%-------------------------------------------------------------------
-module(agent_web_auth_middleware).
-behaviour(cowboy_middleware).

-export([execute/2, validate_request/1, establish_session/1, clear_session/1]).

-define(SESSION_COOKIE, <<"agents_api_session">>).

execute(Req, Env) ->
    Req1 = security_headers(Req),
    case application:get_env(agent_web, api_auth_enabled, true) of
        false ->
            {ok, Req1, Env};
        _ ->
            case protected_path(cowboy_req:path(Req1)) of
                false ->
                    {ok, Req1, Env};
                true ->
                    authorize(Req1, Env)
            end
    end.

authorize(Req, Env) ->
    case validate_request(Req) of
        ok ->
            {ok, Req, Env};
        {error, unauthorized} ->
            unauthorized(Req);
        {error, unavailable} ->
            %% A missing token is a deployment error, not an invitation to
            %% run the control plane anonymously.
            unavailable(Req)
    end.

%% @doc Validate a request without sending a response.
%%
%% This is also used by the browser session bootstrap handler.  Keeping the
%% comparison in one place prevents the UI session path from becoming a
%% weaker authentication path than direct API clients.
validate_request(Req) ->
    case auth_enabled() of
        false ->
            ok;
        true ->
            case configured_token() of
                {ok, ExpectedToken} ->
                    case supplied_token(Req) of
                        {ok, SuppliedToken} ->
                            case secure_equal(ExpectedToken, SuppliedToken) of
                                true -> ok;
                                false -> {error, unauthorized}
                            end;
                        _ ->
                            {error, unauthorized}
                    end;
                error ->
                    {error, unavailable}
            end
    end.

%% @doc Validate credentials and establish the browser session cookie.
%%
%% The cookie contains a base64 representation of the configured bearer
%% token so that browser fetches and WebSocket handshakes can authenticate
%% without putting the token in URLs, localStorage, or JavaScript bundles.
establish_session(Req) ->
    case auth_enabled() of
        false ->
            {ok, Req};
        true ->
            case configured_token() of
                {ok, ExpectedToken} ->
                    case supplied_token(Req) of
                        {ok, SuppliedToken} ->
                            case secure_equal(ExpectedToken, SuppliedToken) of
                                true -> {ok, issue_session_cookie(ExpectedToken, Req)};
                                false -> {error, unauthorized}
                            end;
                        _ ->
                            {error, unauthorized}
                    end;
                error ->
                    {error, unavailable}
            end
    end.

clear_session(Req) ->
    cowboy_req:set_resp_cookie(
        ?SESSION_COOKIE,
        <<>>,
        Req,
        (cookie_options())#{max_age => 0}
    ).

auth_enabled() ->
    application:get_env(agent_web, api_auth_enabled, true) =/= false.

protected_path(<<"/api/system/health">>) -> false;
protected_path(<<"/api/system/session">>) -> false;
protected_path(<<"/api/oauth/callback/", _/binary>>) -> false;
protected_path(<<"/api/mcp/oauth/callback/", _/binary>>) -> false;
protected_path(<<"/api/", _/binary>>) -> true;
protected_path(<<"/ws", _/binary>>) -> true;
protected_path(<<"/mcp", _/binary>>) -> true;
protected_path(_) -> false.

configured_token() ->
    case application:get_env(agent_web, api_token) of
        {ok, Value} -> normalize_token(Value);
        undefined ->
            case os:getenv("AGENTS_WEB_API_TOKEN") of
                false -> error;
                Value -> normalize_token(Value)
            end
    end.

normalize_token(Value) when is_binary(Value), byte_size(Value) >= 16 ->
    {ok, Value};
normalize_token(Value) when is_list(Value), length(Value) >= 16 ->
    {ok, list_to_binary(Value)};
normalize_token(_) ->
    error.

supplied_token(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer ", Token/binary>> when byte_size(Token) > 0 ->
            {ok, Token};
        <<"bearer ", Token/binary>> when byte_size(Token) > 0 ->
            {ok, Token};
        _ ->
            case cowboy_req:header(<<"x-api-key">>, Req) of
                Token when is_binary(Token), byte_size(Token) > 0 ->
                    {ok, Token};
                _ ->
                    session_cookie_token(Req)
            end
    end.

session_cookie_token(Req) ->
    try
        case lists:keyfind(?SESSION_COOKIE, 1, cowboy_req:parse_cookies(Req)) of
            {?SESSION_COOKIE, EncodedToken} when is_binary(EncodedToken) ->
                try base64:decode(EncodedToken) of
                    Token when is_binary(Token) -> {ok, Token};
                    _ -> error
                catch
                    _:_ -> error
                end;
            _ ->
                error
        end
    catch
        _:_ -> error
    end.

issue_session_cookie(ExpectedToken, Req) ->
    cowboy_req:set_resp_cookie(
        ?SESSION_COOKIE,
        base64:encode(ExpectedToken),
        Req,
        cookie_options()
    ).

cookie_options() ->
    #{
        path => <<"/">>,
        http_only => true,
        max_age => session_max_age(),
        same_site => strict,
        secure => application:get_env(agent_web, secure_cookies, true) =:= true
    }.

session_max_age() ->
    case application:get_env(agent_web, session_max_age, 28800) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> 28800
    end.

secure_equal(Expected, Actual) when is_binary(Expected), is_binary(Actual) ->
    %% Hashing normalizes the comparison to a fixed-size value and avoids
    %% comparing a secret directly in a variable-length branch.
    crypto:hash_equals(
        crypto:hash(sha256, Expected),
        crypto:hash(sha256, Actual)
    );
secure_equal(_, _) ->
    false.

unauthorized(Req) ->
    Req1 = cowboy_req:reply(401,
        #{
            <<"content-type">> => <<"application/json">>,
            <<"www-authenticate">> => <<"Bearer realm=agents.erl">>,
            <<"cache-control">> => <<"no-store">>
        },
        jsx:encode(#{error => <<"Authentication required">>}),
        Req),
    {stop, Req1}.

unavailable(Req) ->
    Req1 = cowboy_req:reply(503,
        #{
            <<"content-type">> => <<"application/json">>,
            <<"cache-control">> => <<"no-store">>
        },
        jsx:encode(#{error => <<"API authentication is not configured">>}),
        Req),
    {stop, Req1}.

security_headers(Req0) ->
    cowboy_req:set_resp_headers(
        #{
            <<"x-content-type-options">> => <<"nosniff">>,
            <<"x-frame-options">> => <<"DENY">>,
            <<"referrer-policy">> => <<"no-referrer">>,
            <<"permissions-policy">> => <<"camera=(), microphone=(), geolocation=()">>
        },
        Req0
    ).
