-module(agents_architecture_tests).

-include_lib("eunit/include/eunit.hrl").

application_callback_test() ->
    LoadResult = application:load(agents),
    ?assert(lists:member(LoadResult, [ok, {error, {already_loaded, agents}}])),
    ?assertEqual({ok, {agents_app, []}}, application:get_key(agents, mod)).

root_supervisor_owns_platform_services_test() ->
    {ok, {_Flags, ChildSpecs}} = agents_sup:init([]),
    ChildIds = [maps:get(id, Spec) || Spec <- ChildSpecs],
    ?assert(lists:member(agent_time_authority, ChildIds)),
    ?assert(lists:member(agent_tools, ChildIds)),
    ?assert(lists:member(agent_dynamic_system, ChildIds)),
    ?assert(lists:member(agent_supervisor, ChildIds)),
    ?assert(lists:member(super_agent_sup, ChildIds)),
    ?assert(lists:member(dynamic_runtime_sup, ChildIds)),
    ?assert(lists:member(system_introspection, ChildIds)).

dynamic_supervisor_owns_agents_only_test() ->
    {ok, {_Flags, ChildSpecs}} = agent_supervisor:init([]),
    ?assertEqual([], ChildSpecs),
    ?assert(erlang:function_exported(agent_supervisor, start_agent, 2)),
    ?assert(erlang:function_exported(agent_supervisor, terminate_agent, 1)),
    ?assert(erlang:function_exported(agent_supervisor, get_all_agents, 0)).

dynamic_runtime_has_ordered_ownership_test() ->
    {ok, {Flags, ChildSpecs}} = dynamic_runtime_sup:init([]),
    ?assertEqual(rest_for_one, maps:get(strategy, Flags)),
    ?assertEqual(
        [dynamic_supervisors_sup, dynamic_supervisor_manager],
        [maps:get(id, Spec) || Spec <- ChildSpecs]
    ),
    {ok, {_DynamicFlags, DynamicChildren}} = dynamic_supervisors_sup:init([]),
    ?assertEqual([], DynamicChildren).

optional_super_agent_has_a_lifecycle_owner_test() ->
    {ok, {_Flags, ChildSpecs}} = super_agent_sup:init([]),
    ?assertEqual([], ChildSpecs),
    ?assert(erlang:function_exported(super_agent, start_link, 1)).
