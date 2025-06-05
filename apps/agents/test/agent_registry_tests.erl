-module(agent_registry_tests).
-include_lib("eunit/include/eunit.hrl").

agent_registry_test_() ->
    {setup,
     fun() -> 
         {ok, _} = agent_registry:start_link([]),
         ok
     end,
     fun(_) -> ok end,
     fun(_) ->
        [
          ?_test(begin
            % Create a test agent process
            SessionId = <<"test_session">>,
            Self = self(),
            
            % Register agent
            agent_registry:register_agent(SessionId, Self),
            timer:sleep(100), % Give time for registration
            
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
            timer:sleep(100),
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
          end)
        ]
     end
    }.