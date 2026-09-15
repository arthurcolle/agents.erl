-module(agent_web_auth_middleware_tests).

-include_lib("eunit/include/eunit.hrl").

valid_bearer_is_accepted_test() ->
    with_auth(fun(Token) ->
        Req = request(#{<<"authorization">> => <<"Bearer ", Token/binary>>}),
        ?assertEqual(ok, agent_web_auth_middleware:validate_request(Req))
    end).

invalid_bearer_is_rejected_test() ->
    with_auth(fun(_Token) ->
        Req = request(#{<<"authorization">> => <<"Bearer wrong-token">>}),
        ?assertEqual(
            {error, unauthorized},
            agent_web_auth_middleware:validate_request(Req)
        )
    end).

session_cookie_is_accepted_after_exchange_test() ->
    with_auth(fun(Token) ->
        BearerReq = request(#{<<"authorization">> => <<"Bearer ", Token/binary>>}),
        {ok, ReqWithCookie} = agent_web_auth_middleware:establish_session(BearerReq),
        RespCookies = maps:get(resp_cookies, ReqWithCookie),
        SetCookie = maps:get(<<"agents_api_session">>, RespCookies),
        EncodedToken = base64:encode(Token),
        ?assert(is_list(SetCookie) orelse is_binary(SetCookie)),
        CookieReq = request(#{
            <<"cookie">> => <<"agents_api_session=", EncodedToken/binary>>
        }),
        ?assertEqual(ok, agent_web_auth_middleware:validate_request(CookieReq))
    end).

auth_can_be_disabled_for_local_development_test() ->
    application:set_env(agent_web, api_auth_enabled, false),
    application:unset_env(agent_web, api_token),
    try
        ?assertEqual(ok, agent_web_auth_middleware:validate_request(request(#{})))
    after
        application:set_env(agent_web, api_auth_enabled, true)
    end.

with_auth(Test) ->
    Token = <<"auth-test-token-20260831">>,
    application:set_env(agent_web, api_auth_enabled, true),
    application:set_env(agent_web, api_token, Token),
    application:set_env(agent_web, secure_cookies, false),
    try
        Test(Token)
    after
        application:unset_env(agent_web, api_token),
        application:set_env(agent_web, api_auth_enabled, true)
    end.

request(Headers) ->
    #{
        headers => Headers,
        path => <<"/api/agents">>
    }.
