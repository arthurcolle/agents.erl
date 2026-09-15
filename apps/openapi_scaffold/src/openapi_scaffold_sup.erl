-module(openapi_scaffold_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 30
    },
    %% These processes are the actual ownership boundary for the OpenAPI
    %% service. Keeping them here makes health checks meaningful and ensures
    %% router/codegen calls never target an unstarted process.
    ChildSpecs = [
        worker(openapi_parser),
        worker(openapi_codegen),
        worker(openapi_router),
        worker(openapi_validator),
        worker(openapi_proxy),
        worker(openapi_introspection),
        worker(openapi_playground),
        worker(openapi_graphql_bridge),
        listener()
    ],
    {ok, {SupFlags, ChildSpecs}}.

worker(Module) ->
    #{
        id => Module,
        start => {Module, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [Module]
    }.

listener() ->
    #{
        id => openapi_http_listener,
        start => {openapi_http_listener, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [openapi_http_listener]
    }.
