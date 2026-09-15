-module(agent_registry_tests).
-include_lib("eunit/include/eunit.hrl").

agent_registry_test_() ->
    {setup,
     fun() -> 
         {ok, _} = agent_registry:start_link([]),
         ok
     end,
     fun(_) -> gen_server:stop(agent_registry) end,
     fun(_) ->
        [
          ?_test(begin
            % Create a test agent process
            SessionId = <<"test_session">>,
            Self = self(),
            
            % Register agent
            agent_registry:register_agent(SessionId, Self),
            
            % List agents should include our session
            Agents = agent_registry:list_agents(),
            SessionIds = [Id || {Id, _Pid, _Meta} <- Agents],
            ?assert(lists:member(SessionId, SessionIds)),
            
            % Get agent should return our pid
            ?assertMatch({ok, Self}, agent_registry:get_agent(SessionId)),
            
            % Send message to agent
            ?assertEqual(ok, agent_registry:send_to_agent(SessionId, test)),
            receive
                test -> ok
            after 1000 ->
                ?assert(false, "Did not receive message")
            end,
            
            % Unregister agent
            ok = agent_registry:unregister_agent(SessionId),
            Agents2 = agent_registry:list_agents(),
            SessionIds2 = [Id || {Id, _Pid, _Meta} <- Agents2],
            ?assertNot(lists:member(SessionId, SessionIds2)),
            
            % get_agent now returns error
            ?assertMatch({error, agent_not_found}, agent_registry:get_agent(SessionId)),
            
            % send_to_agent on missing agent returns error
            ?assertMatch({error, agent_not_found}, agent_registry:send_to_agent(SessionId, test)),
            
            % Query another missing
            ?assertMatch({error, agent_not_found}, agent_registry:get_agent(<<"missing">>)),
            ok
          end),
          ?_test(begin
            P1 = spawn(fun() -> receive stop -> ok end end),
            P2 = spawn(fun() -> receive stop -> ok end end),
            SessionId = <<"replacement_session">>,
            ok = agent_registry:register_agent(SessionId, P1, #{}),
            ok = agent_registry:register_agent(SessionId, P2, #{}),
            exit(P1, kill),
            timer:sleep(25),
            ?assertEqual({ok, P2}, agent_registry:get_agent(SessionId)),
            exit(P2, kill),
            timer:sleep(25),
            ?assertMatch({error, agent_not_found}, agent_registry:get_agent(SessionId))
          end)
        ]
     end
    }.
