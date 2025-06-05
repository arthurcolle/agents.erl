%%%-------------------------------------------------------------------
%%% @doc OpenAPI Scaffold Application
%%% Advanced OpenAPI scaffolder with AI, multi-language support,
%%% monitoring, and developer tools.
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_scaffold_app).
-behaviour(application).

-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    %% Start Prometheus metrics
    prometheus_httpd:start(),
    
    %% Start HTTP server for playground
    start_http_server(),
    
    %% Start the main supervisor
    openapi_scaffold_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_http_server() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/playground", openapi_playground_handler, []},
            {"/api/graphql", openapi_graphql_handler, []},
            {"/api/mock/[...]", openapi_mock_handler, []},
            {"/api/metrics", prometheus_cowboy2_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, openapi_scaffold, "static"}},
            {"/", cowboy_static, {priv_file, openapi_scaffold, "static/playground.html"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(openapi_http_listener,
        [{port, 8090}],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.