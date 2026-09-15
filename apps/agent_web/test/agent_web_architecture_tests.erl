-module(agent_web_architecture_tests).

-include_lib("eunit/include/eunit.hrl").

supervision_boundaries_test() ->
    {ok, {Flags, ChildSpecs}} = agent_web_sup:init([]),
    ?assertEqual(rest_for_one, maps:get(strategy, Flags)),
    ?assertEqual(
        [agent_web_services_sup, agent_web_http_listener, agent_web_bootstrap],
        [maps:get(id, Spec) || Spec <- ChildSpecs]
    ).

static_agent_route_precedes_identifier_route_test() ->
    Dispatch = web_dispatch(),
    Req0 = #{host => <<"localhost">>, path => <<"/api/agents/health">>},
    {ok, Req, Env} = cowboy_router:execute(Req0, #{dispatch => Dispatch}),
    ?assertEqual(agent_api_handler, maps:get(handler, Env)),
    ?assertEqual(#{}, maps:get(bindings, Req)).

static_interpretation_route_precedes_identifier_route_test() ->
    Dispatch = web_dispatch(),
    Req0 = #{
        host => <<"localhost">>,
        path => <<"/api/interpretations/interpret">>
    },
    {ok, Req, Env} = cowboy_router:execute(Req0, #{dispatch => Dispatch}),
    ?assertEqual(error_interpretation_api_handler, maps:get(handler, Env)),
    ?assertEqual(#{}, maps:get(bindings, Req)).

web_dispatch() ->
    {ok, {_Flags, ChildSpecs}} = agent_web_sup:init([]),
    ListenerSpec = lists:keyfind(
        agent_web_http_listener,
        1,
        [{maps:get(id, Spec), Spec} || Spec <- ChildSpecs]
    ),
    {agent_web_http_listener, #{
        start := {cowboy, start_clear, [
            agent_web_http_listener,
            _TransportOptions,
            #{env := #{dispatch := Dispatch}}
        ]}
    }} = ListenerSpec,
    Dispatch.
