%% agent_registry_tests.erl
%% Unit tests for the agent registry module
-module(agent_registry_tests).
-include_lib("eunit/include/eunit.hrl").

-import(test_helpers, [
    setup_test_env/0,
    cleanup_test_env/0,
    generate_test_id/0,
    eventually/1,
    with_timeout/2
]).

%% Test fixture setup
setup() ->
    setup_test_env(),
    % Ensure registry is started
    case whereis(agent_registry) of
        undefined -> {ok, _} = agent_registry:start_link(#{});
        _ -> ok
    end.

cleanup(_) ->
    cleanup_test_env().

agent_registry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Basic registration tests", fun test_basic_registration/0},
      {"Multiple agent tests", fun test_multiple_agents/0},
      {"Process monitoring tests", fun test_process_monitoring/0},
      {"Registry state tests", fun test_registry_state/0},
      {"Concurrent registration tests", fun test_concurrent_registration/0},
      {"Error handling tests", fun test_error_handling/0},
      {"Registry cleanup tests", fun test_registry_cleanup/0}
     ]
    }.

%% Test basic registration
test_basic_registration() ->
    AgentId = generate_test_id(),
    Pid = self(),
    
    % Test registration
    ?assertEqual(ok, agent_registry:register_agent(AgentId, Pid)),
    
    % Test retrieval
    ?assertMatch({ok, #{pid := Pid}}, agent_registry:get_agent(AgentId)),
    
    % Test listing
    {ok, Agents} = agent_registry:list_agents(),
    ?assert(lists:any(fun(#{id := Id}) -> Id =:= AgentId end, Agents)),
    
    % Test unregistration
    ?assertEqual(ok, agent_registry:unregister_agent(AgentId)),
    
    % Verify removal
    ?assertEqual({error, not_found}, agent_registry:get_agent(AgentId)).

%% Test multiple agents
test_multiple_agents() ->
    % Register multiple agents
    AgentIds = [generate_test_id() || _ <- lists:seq(1, 5)],
    Pids = [spawn_link(fun() -> receive stop -> ok end end) || _ <- AgentIds],
    
    lists:foreach(
        fun({Id, Pid}) ->
            ?assertEqual(ok, agent_registry:register_agent(Id, Pid))
        end,
        lists:zip(AgentIds, Pids)
    ),
    
    % Verify all registered
    {ok, AllAgents} = agent_registry:list_agents(),
    RegisteredIds = [Id || #{id := Id} <- AllAgents],
    
    lists:foreach(
        fun(Id) ->
            ?assert(lists:member(Id, RegisteredIds))
        end,
        AgentIds
    ),
    
    % Unregister some agents
    [Id1, Id2 | Rest] = AgentIds,
    ?assertEqual(ok, agent_registry:unregister_agent(Id1)),
    ?assertEqual(ok, agent_registry:unregister_agent(Id2)),
    
    % Verify partial removal
    {ok, RemainingAgents} = agent_registry:list_agents(),
    RemainingIds = [Id || #{id := Id} <- RemainingAgents],
    
    ?assertNot(lists:member(Id1, RemainingIds)),
    ?assertNot(lists:member(Id2, RemainingIds)),
    lists:foreach(
        fun(Id) ->
            ?assert(lists:member(Id, RemainingIds))
        end,
        Rest
    ),
    
    % Cleanup
    lists:foreach(fun(Pid) -> Pid ! stop end, Pids).

%% Test process monitoring
test_process_monitoring() ->
    AgentId = generate_test_id(),
    
    % Spawn a process that we can kill
    AgentPid = spawn_link(fun() ->
        receive
            stop -> ok
        end
    end),
    
    % Register the agent
    ?assertEqual(ok, agent_registry:register_agent(AgentId, AgentPid)),
    
    % Verify registration
    ?assertMatch({ok, #{pid := AgentPid}}, agent_registry:get_agent(AgentId)),
    
    % Kill the process
    unlink(AgentPid),
    exit(AgentPid, kill),
    
    % Wait for monitor to trigger
    timer:sleep(100),
    
    % Verify agent was automatically removed
    ?assertEqual({error, not_found}, agent_registry:get_agent(AgentId)).

%% Test registry state
test_registry_state() ->
    % Get initial state
    InitialState = sys:get_state(agent_registry),
    ?assertMatch({state, #{agents := _}}, InitialState),
    
    % Register an agent
    AgentId = generate_test_id(),
    Pid = self(),
    ?assertEqual(ok, agent_registry:register_agent(AgentId, Pid)),
    
    % Check state was updated
    UpdatedState = sys:get_state(agent_registry),
    {state, #{agents := Agents}} = UpdatedState,
    ?assert(maps:is_key(AgentId, Agents)),
    
    % Verify agent info in state
    AgentInfo = maps:get(AgentId, Agents),
    ?assertMatch(#{pid := Pid, monitor_ref := Ref} when is_reference(Ref), AgentInfo).

%% Test concurrent registration
test_concurrent_registration() ->
    NumAgents = 100,
    Parent = self(),
    
    % Spawn processes to register agents concurrently
    Pids = [
        spawn_link(fun() ->
            Id = generate_test_id(),
            Result = agent_registry:register_agent(Id, self()),
            Parent ! {registered, Id, Result}
        end)
        || _ <- lists:seq(1, NumAgents)
    ],
    
    % Collect results
    Results = [
        receive
            {registered, Id, Result} -> {Id, Result}
        after 5000 ->
            timeout
        end
        || _ <- lists:seq(1, NumAgents)
    ],
    
    % Verify all succeeded
    ?assertEqual(NumAgents, length(Results)),
    lists:foreach(
        fun({_Id, Result}) ->
            ?assertEqual(ok, Result)
        end,
        Results
    ),
    
    % Verify all agents are in registry
    {ok, AllAgents} = agent_registry:list_agents(),
    ?assert(length(AllAgents) >= NumAgents).

%% Test error handling
test_error_handling() ->
    AgentId = generate_test_id(),
    Pid = self(),
    
    % Test double registration
    ?assertEqual(ok, agent_registry:register_agent(AgentId, Pid)),
    ?assertEqual({error, already_registered}, agent_registry:register_agent(AgentId, Pid)),
    
    % Test unregistering non-existent agent
    NonExistentId = generate_test_id(),
    ?assertEqual({error, not_found}, agent_registry:unregister_agent(NonExistentId)),
    
    % Test getting non-existent agent
    ?assertEqual({error, not_found}, agent_registry:get_agent(NonExistentId)),
    
    % Cleanup
    ?assertEqual(ok, agent_registry:unregister_agent(AgentId)).

%% Test registry cleanup
test_registry_cleanup() ->
    % Register multiple agents with different types of processes
    
    % 1. Normal process
    NormalId = generate_test_id(),
    NormalPid = spawn_link(fun() -> receive stop -> ok end end),
    ?assertEqual(ok, agent_registry:register_agent(NormalId, NormalPid)),
    
    % 2. Process that will exit
    ExitId = generate_test_id(),
    ExitPid = spawn_link(fun() -> 
        receive
            exit_now -> exit(normal)
        end
    end),
    ?assertEqual(ok, agent_registry:register_agent(ExitId, ExitPid)),
    
    % 3. Process that will crash
    CrashId = generate_test_id(),
    CrashPid = spawn_link(fun() ->
        receive
            crash_now -> error(crash_test)
        end
    end),
    ?assertEqual(ok, agent_registry:register_agent(CrashId, CrashPid)),
    
    % Verify all registered
    {ok, InitialAgents} = agent_registry:list_agents(),
    InitialCount = length(InitialAgents),
    ?assert(InitialCount >= 3),
    
    % Trigger exits
    unlink(ExitPid),
    ExitPid ! exit_now,
    
    unlink(CrashPid),
    CrashPid ! crash_now,
    
    % Wait for cleanup
    timer:sleep(200),
    
    % Verify cleanup
    ?assertMatch({ok, #{pid := NormalPid}}, agent_registry:get_agent(NormalId)),
    ?assertEqual({error, not_found}, agent_registry:get_agent(ExitId)),
    ?assertEqual({error, not_found}, agent_registry:get_agent(CrashId)),
    
    % Cleanup
    NormalPid ! stop.

%% Additional test cases

registry_persistence_test() ->
    % Test that registry maintains state across calls
    Id1 = generate_test_id(),
    Id2 = generate_test_id(),
    
    ?assertEqual(ok, agent_registry:register_agent(Id1, self())),
    
    % Make several calls
    {ok, _} = agent_registry:list_agents(),
    {ok, _} = agent_registry:get_agent(Id1),
    
    ?assertEqual(ok, agent_registry:register_agent(Id2, self())),
    
    % Verify both still exist
    {ok, Agents} = agent_registry:list_agents(),
    Ids = [Id || #{id := Id} <- Agents],
    ?assert(lists:member(Id1, Ids)),
    ?assert(lists:member(Id2, Ids)).

monitor_ref_uniqueness_test() ->
    % Test that each registration gets a unique monitor ref
    Pids = [spawn_link(fun() -> receive stop -> ok end end) || _ <- lists:seq(1, 3)],
    Ids = [generate_test_id() || _ <- lists:seq(1, 3)],
    
    lists:foreach(
        fun({Id, Pid}) ->
            ?assertEqual(ok, agent_registry:register_agent(Id, Pid))
        end,
        lists:zip(Ids, Pids)
    ),
    
    % Get monitor refs from state
    {state, #{agents := Agents}} = sys:get_state(agent_registry),
    MonitorRefs = [
        maps:get(monitor_ref, maps:get(Id, Agents))
        || Id <- Ids
    ],
    
    % Verify all unique
    ?assertEqual(length(MonitorRefs), length(lists:usort(MonitorRefs))),
    
    % Cleanup
    lists:foreach(fun(Pid) -> Pid ! stop end, Pids).

registry_call_timeout_test() ->
    % Test that registry handles call timeouts gracefully
    meck:new(gen_server, [unstick, passthrough]),
    meck:expect(gen_server, call,
        fun(Server, Request, Timeout) when Server =:= agent_registry ->
            timer:sleep(100),
            meck:passthrough([Server, Request, Timeout])
        end
    ),
    
    % This should still work despite the delay
    Id = generate_test_id(),
    ?assertEqual(ok, agent_registry:register_agent(Id, self())),
    
    meck:unload(gen_server).

large_agent_count_test() ->
    % Test registry with many agents
    NumAgents = 1000,
    
    % Register many agents
    RegisterStart = erlang:system_time(millisecond),
    Ids = lists:map(
        fun(N) ->
            Id = list_to_atom("large_test_" ++ integer_to_list(N)),
            Pid = spawn_link(fun() -> receive stop -> ok end end),
            ok = agent_registry:register_agent(Id, Pid),
            {Id, Pid}
        end,
        lists:seq(1, NumAgents)
    ),
    RegisterTime = erlang:system_time(millisecond) - RegisterStart,
    
    % Registration should be reasonably fast
    ?assert(RegisterTime < 5000, "Registration took too long"),
    
    % List all agents
    ListStart = erlang:system_time(millisecond),
    {ok, AllAgents} = agent_registry:list_agents(),
    ListTime = erlang:system_time(millisecond) - ListStart,
    
    ?assert(length(AllAgents) >= NumAgents),
    ?assert(ListTime < 1000, "Listing took too long"),
    
    % Cleanup
    lists:foreach(
        fun({Id, Pid}) ->
            unlink(Pid),
            exit(Pid, kill),
            agent_registry:unregister_agent(Id)
        end,
        Ids
    ).