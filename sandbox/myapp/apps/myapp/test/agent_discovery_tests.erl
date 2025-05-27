%%%-------------------------------------------------------------------
%%% @doc
%%% Agent Discovery Tests
%%% Tests for agent discovery with capability-based lookup
%%% @end
%%%-------------------------------------------------------------------
-module(agent_discovery_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 5000).

%%%===================================================================
%%% Setup and Teardown
%%%===================================================================

setup() ->
    % Start required applications
    application:ensure_all_started(myapp),
    % Clear any existing registrations
    agent_discovery:clear_all(),
    ok.

teardown(_) ->
    agent_discovery:clear_all(),
    application:stop(myapp),
    ok.

%%%===================================================================
%%% Test Generators
%%%===================================================================

agent_discovery_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Basic registration and discovery", fun test_basic_registration/0},
      {"Multiple capability registration", fun test_multiple_capabilities/0},
      {"Capability-based lookup", fun test_capability_lookup/0},
      {"Agent unregistration", fun test_unregistration/0},
      {"Concurrent registrations", fun test_concurrent_registrations/0},
      {"Edge cases", fun test_edge_cases/0},
      {"Error handling", fun test_error_handling/0},
      {"Performance", fun test_performance/0}
     ]}.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_basic_registration() ->
    AgentId = <<"test_agent_1">>,
    Capabilities = [<<"chat">>, <<"translate">>],
    Metadata = #{name => <<"Test Agent">>, version => <<"1.0">>},
    
    % Register agent
    ?assertEqual(ok, agent_discovery:register_agent(AgentId, Capabilities, Metadata)),
    
    % Verify registration
    {ok, Info} = agent_discovery:get_agent_info(AgentId),
    ?assertEqual(Capabilities, maps:get(capabilities, Info)),
    ?assertEqual(Metadata, maps:get(metadata, Info)),
    
    % List all agents
    Agents = agent_discovery:list_agents(),
    ?assert(lists:member(AgentId, Agents)).

test_multiple_capabilities() ->
    AgentId1 = <<"multi_cap_agent_1">>,
    AgentId2 = <<"multi_cap_agent_2">>,
    AgentId3 = <<"multi_cap_agent_3">>,
    
    % Register agents with overlapping capabilities
    ?assertEqual(ok, agent_discovery:register_agent(AgentId1, [<<"chat">>, <<"translate">>], #{})),
    ?assertEqual(ok, agent_discovery:register_agent(AgentId2, [<<"translate">>, <<"search">>], #{})),
    ?assertEqual(ok, agent_discovery:register_agent(AgentId3, [<<"search">>, <<"analyze">>], #{})),
    
    % Find agents by capability
    ChatAgents = agent_discovery:find_agents_by_capability(<<"chat">>),
    ?assertEqual([AgentId1], ChatAgents),
    
    TranslateAgents = agent_discovery:find_agents_by_capability(<<"translate">>),
    ?assertEqual(lists:sort([AgentId1, AgentId2]), lists:sort(TranslateAgents)),
    
    SearchAgents = agent_discovery:find_agents_by_capability(<<"search">>),
    ?assertEqual(lists:sort([AgentId2, AgentId3]), lists:sort(SearchAgents)).

test_capability_lookup() ->
    % Register agents with various capabilities
    Agents = [
        {<<"agent_1">>, [<<"nlp">>, <<"sentiment">>], #{priority => high}},
        {<<"agent_2">>, [<<"nlp">>, <<"translation">>], #{priority => medium}},
        {<<"agent_3">>, [<<"vision">>, <<"ocr">>], #{priority => high}},
        {<<"agent_4">>, [<<"audio">>, <<"transcription">>], #{priority => low}}
    ],
    
    lists:foreach(fun({Id, Caps, Meta}) ->
        ?assertEqual(ok, agent_discovery:register_agent(Id, Caps, Meta))
    end, Agents),
    
    % Test various lookups
    NlpAgents = agent_discovery:find_agents_by_capability(<<"nlp">>),
    ?assertEqual(2, length(NlpAgents)),
    ?assert(lists:member(<<"agent_1">>, NlpAgents)),
    ?assert(lists:member(<<"agent_2">>, NlpAgents)),
    
    % Test non-existent capability
    NoAgents = agent_discovery:find_agents_by_capability(<<"non_existent">>),
    ?assertEqual([], NoAgents),
    
    % Test multiple capability search
    MultiCaps = agent_discovery:find_agents_with_all_capabilities([<<"nlp">>, <<"sentiment">>]),
    ?assertEqual([<<"agent_1">>], MultiCaps).

test_unregistration() ->
    AgentId = <<"unreg_test_agent">>,
    Capabilities = [<<"test_cap">>],
    
    % Register and verify
    ?assertEqual(ok, agent_discovery:register_agent(AgentId, Capabilities, #{})),
    ?assertEqual([AgentId], agent_discovery:find_agents_by_capability(<<"test_cap">>)),
    
    % Unregister
    ?assertEqual(ok, agent_discovery:unregister_agent(AgentId)),
    
    % Verify removal
    ?assertEqual([], agent_discovery:find_agents_by_capability(<<"test_cap">>)),
    ?assertEqual({error, not_found}, agent_discovery:get_agent_info(AgentId)),
    
    % Test double unregistration
    ?assertEqual({error, not_found}, agent_discovery:unregister_agent(AgentId)).

test_concurrent_registrations() ->
    NumAgents = 100,
    
    % Spawn concurrent registration processes
    Parent = self(),
    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            AgentId = list_to_binary("concurrent_agent_" ++ integer_to_list(N)),
            Cap = list_to_binary("cap_" ++ integer_to_list(N rem 10)),
            Result = agent_discovery:register_agent(AgentId, [Cap], #{index => N}),
            Parent ! {self(), Result}
        end)
    end, lists:seq(1, NumAgents)),
    
    % Collect results
    Results = lists:map(fun(Pid) ->
        receive
            {Pid, Result} -> Result
        after ?TIMEOUT ->
            timeout
        end
    end, Pids),
    
    % Verify all succeeded
    ?assertEqual(NumAgents, length([R || R <- Results, R =:= ok])),
    
    % Verify all agents are registered
    AllAgents = agent_discovery:list_agents(),
    ?assertEqual(NumAgents, length([A || A <- AllAgents, 
                                        binary:match(A, <<"concurrent_agent_">>) =/= nomatch])).

test_edge_cases() ->
    % Empty capability list
    ?assertEqual({error, invalid_capabilities}, 
                 agent_discovery:register_agent(<<"empty_cap_agent">>, [], #{})),
    
    % Duplicate capabilities
    ?assertEqual(ok, agent_discovery:register_agent(<<"dup_cap_agent">>, 
                                                  [<<"cap1">>, <<"cap1">>, <<"cap2">>], #{})),
    {ok, Info} = agent_discovery:get_agent_info(<<"dup_cap_agent">>),
    Caps = maps:get(capabilities, Info),
    ?assertEqual([<<"cap1">>, <<"cap2">>], lists:usort(Caps)),
    
    % Very long agent ID
    LongId = list_to_binary(lists:duplicate(1000, $a)),
    ?assertEqual(ok, agent_discovery:register_agent(LongId, [<<"test">>], #{})),
    
    % Special characters in capabilities
    SpecialCaps = [<<"cap-with-dash">>, <<"cap_with_underscore">>, <<"cap.with.dot">>],
    ?assertEqual(ok, agent_discovery:register_agent(<<"special_agent">>, SpecialCaps, #{})),
    
    % Large metadata
    LargeMeta = maps:from_list([{list_to_atom("key_" ++ integer_to_list(N)), 
                                list_to_binary("value_" ++ integer_to_list(N))} 
                               || N <- lists:seq(1, 100)]),
    ?assertEqual(ok, agent_discovery:register_agent(<<"large_meta_agent">>, [<<"test">>], LargeMeta)).

test_error_handling() ->
    % Invalid agent ID types
    ?assertError(badarg, agent_discovery:register_agent(atom_id, [<<"cap">>], #{})),
    ?assertError(badarg, agent_discovery:register_agent(123, [<<"cap">>], #{})),
    
    % Invalid capability types
    ?assertError(badarg, agent_discovery:register_agent(<<"agent">>, [atom_cap], #{})),
    ?assertError(badarg, agent_discovery:register_agent(<<"agent">>, [123], #{})),
    
    % Invalid metadata
    ?assertError(badarg, agent_discovery:register_agent(<<"agent">>, [<<"cap">>], not_a_map)),
    
    % Re-registration attempt
    ?assertEqual(ok, agent_discovery:register_agent(<<"dup_agent">>, [<<"cap">>], #{})),
    ?assertEqual({error, already_registered}, 
                 agent_discovery:register_agent(<<"dup_agent">>, [<<"cap2">>], #{})).

test_performance() ->
    NumAgents = 1000,
    NumCapabilities = 50,
    
    % Generate test data
    Agents = [{list_to_binary("perf_agent_" ++ integer_to_list(N)),
               [list_to_binary("cap_" ++ integer_to_list(rand:uniform(NumCapabilities))) 
                || _ <- lists:seq(1, rand:uniform(5))],
               #{index => N}}
              || N <- lists:seq(1, NumAgents)],
    
    % Measure registration time
    RegStart = erlang:monotonic_time(millisecond),
    lists:foreach(fun({Id, Caps, Meta}) ->
        agent_discovery:register_agent(Id, Caps, Meta)
    end, Agents),
    RegTime = erlang:monotonic_time(millisecond) - RegStart,
    
    ?assert(RegTime < 5000), % Should complete within 5 seconds
    io:format("Registration of ~p agents took ~p ms~n", [NumAgents, RegTime]),
    
    % Measure lookup time
    LookupStart = erlang:monotonic_time(millisecond),
    lists:foreach(fun(N) ->
        Cap = list_to_binary("cap_" ++ integer_to_list(N)),
        agent_discovery:find_agents_by_capability(Cap)
    end, lists:seq(1, NumCapabilities)),
    LookupTime = erlang:monotonic_time(millisecond) - LookupStart,
    
    ?assert(LookupTime < 1000), % Should complete within 1 second
    io:format("Lookup of ~p capabilities took ~p ms~n", [NumCapabilities, LookupTime]).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

% Add any helper functions here if needed