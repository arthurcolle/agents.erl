%%%-------------------------------------------------------------------
%%% @doc Same-origin browser session bootstrap for the API bearer token.
%%%
%%% A browser cannot attach arbitrary Authorization headers to a WebSocket
%%% handshake.  The dashboard therefore exchanges a bearer token supplied by
%%% the operator for a short-lived, HttpOnly, SameSite=Strict cookie.  Direct
%%% API clients can continue to use Authorization or x-api-key headers.
%%% @end
%%%-------------------------------------------------------------------
-module(agent_web_session_handler).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> ->
            establish_session(Req0, State);
        <<"GET">> ->
            validate_session(Req0, State);
        <<"DELETE">> ->
            Req1 = agent_web_auth_middleware:clear_session(Req0),
            reply(204, #{}, <<>>, Req1, State);
        _ ->
            reply(405, #{<<"allow">> => <<"GET, POST, DELETE">>},
                <<"Method not allowed">>, Req0, State)
    end.

establish_session(Req0, State) ->
    case agent_web_auth_middleware:establish_session(Req0) of
        {ok, Req1} ->
            reply(204, #{<<"cache-control">> => <<"no-store">>}, <<>>, Req1, State);
        {error, unauthorized} ->
            unauthorized(Req0, State);
        {error, unavailable} ->
            unavailable(Req0, State)
    end.

validate_session(Req0, State) ->
    case agent_web_auth_middleware:validate_request(Req0) of
        ok ->
            reply(204, #{<<"cache-control">> => <<"no-store">>}, <<>>, Req0, State);
        {error, unauthorized} ->
            unauthorized(Req0, State);
        {error, unavailable} ->
            unavailable(Req0, State)
    end.

unauthorized(Req, State) ->
    reply(401,
        #{
            <<"content-type">> => <<"application/json">>,
            <<"www-authenticate">> => <<"Bearer realm=agents.erl">>,
            <<"cache-control">> => <<"no-store">>
        },
        jsx:encode(#{error => <<"Authentication required">>}),
        Req,
        State).

unavailable(Req, State) ->
    reply(503,
        #{
            <<"content-type">> => <<"application/json">>,
            <<"cache-control">> => <<"no-store">>
        },
        jsx:encode(#{error => <<"API authentication is not configured">>}),
        Req,
        State).

reply(Status, Headers, Body, Req, State) ->
    {ok, cowboy_req:reply(Status, Headers, Body, Req), State}.
