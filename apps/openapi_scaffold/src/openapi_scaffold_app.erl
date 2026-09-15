%%%-------------------------------------------------------------------
%%% @doc OpenAPI Scaffold Application
%%% Advanced OpenAPI scaffolder with AI, multi-language support,
%%% monitoring, and developer tools.
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_scaffold_app).
-behaviour(application).

-export([start/2, stop/1, start_http_server/0]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    %% Start the main supervisor
    openapi_scaffold_sup:start_link().

stop(_State) ->
    _ = openapi_http_listener:stop(),
    try cowboy:stop_listener(openapi_http_listener) of
        _ -> ok
    catch
        _:_ -> ok
    end,
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_http_server() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", openapi_health_handler, []},
            {"/openapi/[...]", openapi_introspection_handler, []},
            {"/api/playground", openapi_playground_handler, []},
            {"/api/graphql", openapi_graphql_handler, []},
            {"/api/mock/[...]", openapi_mock_handler, []},
            {"/api/metrics", prometheus_cowboy2_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, openapi_scaffold, "static"}},
            {"/", cowboy_static, {priv_file, openapi_scaffold, "static/playground.html"}}
        ]}
    ]),
    Port = configured_port(),
    Ip = configured_ip(),
    {ok, _} = cowboy:start_clear(openapi_http_listener,
        [{ip, Ip}, {port, Port}],
        #{env => #{dispatch => Dispatch}}
    ).

configured_port() ->
    case application:get_env(openapi_scaffold, port, 8090) of
        Port when is_integer(Port), Port > 0, Port =< 65535 -> Port;
        _ -> 8090
    end.

configured_ip() ->
    case application:get_env(openapi_scaffold, host, "127.0.0.1") of
        Ip when is_tuple(Ip) -> Ip;
        Ip when is_binary(Ip) -> parse_ip(binary_to_list(Ip));
        Ip when is_list(Ip) -> parse_ip(Ip);
        _ -> {127, 0, 0, 1}
    end.

parse_ip(Ip) ->
    case inet:parse_address(Ip) of
        {ok, Parsed} -> Parsed;
        {error, _} -> {127, 0, 0, 1}
    end.
