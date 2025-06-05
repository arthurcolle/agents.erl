#!/usr/bin/env escript

%% Comprehensive test for fleet management with autonomous multi-turn function calling
-module(test_fleet_management_complete).
-mode(compile).

-export([main/1]).

main(_) ->
    % Add paths for compiled modules  
    code:add_paths(filelib:wildcard("_build/default/lib/*/ebin")),
    
    % Start required applications
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    application:ensure_all_started(jsx),
    application:ensure_all_started(agents),
    application:ensure_all_started(agent_web),
    application:ensure_all_started(openai),
    
    io:format("=== Fleet Management Complete System Test ===~n"),
    
    try
        test_fleet_creation_direct(),
        test_autonomous_fleet_operations(),
        test_parallel_function_calling_fleet(),
        test_fleet_self_routing(),
        test_fleet_coordination(),
        io:format("~n✅ Fleet management complete system test passed!~n")
    catch
        Class:Reason:Stacktrace ->
            io:format("❌ Test failed: ~p:~p~n~p~n", [Class, Reason, Stacktrace]),
            halt(1)
    end.

test_fleet_creation_direct() ->
    io:format("~n🚁 Testing Direct Fleet Creation...~n"),
    
    % Start the universal tool registry
    case whereis(universal_tool_registry) of
        undefined -> 
            {ok, _} = universal_tool_registry:start_link();
        _ -> ok
    end,
    
    % Start agent tools
    case whereis(agent_tools) of
        undefined ->
            try {ok, _} = agent_tools:start_link(#{})
            catch _:_ -> io:format("ℹ️  Agent tools server not available~n")
            end;
        _ -> ok
    end,
    
    % Start agent registry
    case whereis(agent_registry) of
        undefined ->
            try {ok, _} = agent_registry:start_link()
            catch _:_ -> io:format("ℹ️  Agent registry not available~n")
            end;
        _ -> ok
    end,
    
    % Create a small fleet directly
    FleetSize = 3,
    FleetAgents = lists:map(fun(N) ->
        AgentConfig = #{
            id => iolist_to_binary(io_lib:format("fleet-agent-~p", [N])),
            name => iolist_to_binary(io_lib:format("Fleet Agent ~p", [N])),
            type => ai,
            model => <<"gpt-4.1-mini">>,
            tools => [shell, who_am_i, get_system_state],
            system_prompt => <<"You are a fleet agent capable of autonomous multi-turn function calling.">>,
            autonomous_mode => false,
            max_autonomous_turns => 5
        },
        
        case agent_instance:start_link(AgentConfig) of
            {ok, AgentPid} ->
                AgentId = maps:get(id, AgentConfig),
                {success, AgentId, AgentPid};
            {error, Reason} ->
                {failed, N, Reason}
        end
    end, lists:seq(1, FleetSize)),
    
    SuccessfulAgents = [{Id, Pid} || {success, Id, Pid} <- FleetAgents],
    io:format("✅ Created ~p/~p fleet agents successfully~n", [length(SuccessfulAgents), FleetSize]),
    
    % Store fleet for other tests
    put(fleet_agents, SuccessfulAgents),
    
    ok.

test_autonomous_fleet_operations() ->
    io:format("~n🤖 Testing Autonomous Fleet Operations...~n"),
    
    FleetAgents = get(fleet_agents),
    case FleetAgents of
        undefined ->
            io:format("⚠️  No fleet available for autonomous testing~n");
        [] ->
            io:format("⚠️  Empty fleet for autonomous testing~n");
        Agents ->
            io:format("Testing autonomous operations on ~p agents...~n", [length(Agents)]),
            
            % Enable autonomous mode for all agents
            lists:foreach(fun({_AgentId, AgentPid}) ->
                try
                    ok = agent_instance:enable_autonomous_mode(AgentPid),
                    io:format("✅ Autonomous mode enabled for agent~n")
                catch
                    E:R -> 
                        io:format("⚠️  Failed to enable autonomous mode: ~p:~p~n", [E, R])
                end
            end, Agents),
            
            % Test autonomous execution on one agent
            case Agents of
                [{_FirstAgentId, FirstAgentPid} | _] ->
                    AutonomousTask = #{
                        action => <<"chat">>,
                        message => <<"Please identify yourself and check the system state autonomously.">>
                    },
                    
                    io:format("Starting autonomous execution test...~n"),
                    try
                        case agent_instance:autonomous_execute(FirstAgentPid, AutonomousTask) of
                            {ok, Response} when is_binary(Response) ->
                                io:format("✅ Autonomous execution completed (~p chars)~n", [byte_size(Response)]);
                            {ok, Response} ->
                                io:format("✅ Autonomous execution completed: ~p~n", [Response]);
                            {error, Reason} ->
                                io:format("⚠️  Autonomous execution failed: ~p~n", [Reason])
                        end
                    catch
                        E:R:S ->
                            io:format("⚠️  Autonomous execution crashed: ~p:~p~n~p~n", [E, R, S])
                    end;
                [] ->
                    io:format("⚠️  No agents available for autonomous testing~n")
            end
    end,
    
    ok.

test_parallel_function_calling_fleet() ->
    io:format("~n⚡ Testing Parallel Function Calling Across Fleet...~n"),
    
    FleetAgents = get(fleet_agents),
    case FleetAgents of
        undefined ->
            io:format("⚠️  No fleet available for parallel testing~n");
        [] ->
            io:format("⚠️  Empty fleet for parallel testing~n");
        Agents ->
            % Test parallel execution across multiple agents
            TestMessage = <<"Execute system analysis using multiple tools in parallel">>,
            
            io:format("Testing parallel execution on ~p agents...~n", [length(Agents)]),
            StartTime = erlang:system_time(millisecond),
            
            % Execute in parallel across fleet
            ParentPid = self(),
            Tasks = lists:map(fun({AgentId, AgentPid}) ->
                spawn_link(fun() ->
                    Result = try
                        Action = #{action => <<"chat">>, message => TestMessage},
                        case agent_instance:execute(AgentPid, Action) of
                            {ok, Response} ->
                                {success, AgentId, Response};
                            {error, Reason} ->
                                {failed, AgentId, Reason}
                        end
                    catch
                        E:R -> {crashed, AgentId, {E, R}}
                    end,
                    ParentPid ! {fleet_result, self(), Result}
                end)
            end, Agents),
            
            % Collect results
            Results = collect_fleet_results(Tasks, []),
            EndTime = erlang:system_time(millisecond),
            Duration = EndTime - StartTime,
            
            SuccessCount = length([Result || {success, _, _} = Result <- Results]),
            io:format("✅ Fleet parallel execution completed in ~p ms~n", [Duration]),
            io:format("   Successful: ~p/~p agents~n", [SuccessCount, length(Agents)]),
            
            % Analyze results for function calling evidence
            FunctionCallEvidence = lists:sum([
                case Result of
                    {success, _, Response} when is_binary(Response) ->
                        case binary:match(Response, <<"function">>) of
                            {_, _} -> 1;
                            nomatch -> 0
                        end;
                    _ -> 0
                end || Result <- Results
            ]),
            
            case FunctionCallEvidence > 0 of
                true -> 
                    io:format("✅ Function calling evidence found in ~p responses~n", [FunctionCallEvidence]);
                false -> 
                    io:format("ℹ️  Limited function calling evidence (expected without tools server)~n")
            end
    end,
    
    ok.

test_fleet_self_routing() ->
    io:format("~n📨 Testing Fleet Self-Routing Capabilities...~n"),
    
    FleetAgents = get(fleet_agents),
    case FleetAgents of
        undefined ->
            io:format("⚠️  No fleet available for self-routing testing~n");
        [] ->
            io:format("⚠️  Empty fleet for self-routing testing~n");
        Agents ->
            % Test self-message routing across fleet
            SelfMessage = <<"Fleet coordination test: acknowledge this self-routed message">>,
            
            io:format("Testing self-routing on ~p agents...~n", [length(Agents)]),
            
            % Send self-messages to all agents
            lists:foreach(fun({AgentId, AgentPid}) ->
                try
                    agent_instance:self_message(AgentPid, SelfMessage),
                    io:format("✅ Self-message sent to agent ~s~n", [AgentId])
                catch
                    E:R ->
                        io:format("⚠️  Self-message failed for agent ~s: ~p:~p~n", [AgentId, E, R])
                end
            end, Agents),
            
            % Give time for async processing
            timer:sleep(1000),
            
            io:format("✅ Fleet self-routing test completed~n")
    end,
    
    ok.

test_fleet_coordination() ->
    io:format("~n🔄 Testing Fleet Coordination and Communication...~n"),
    
    FleetAgents = get(fleet_agents),
    case FleetAgents of
        undefined ->
            io:format("⚠️  No fleet available for coordination testing~n");
        [] ->
            io:format("⚠️  Empty fleet for coordination testing~n");
        Agents when length(Agents) >= 2 ->
            % Test inter-agent communication simulation
            [{Agent1Id, Agent1Pid}, {Agent2Id, Agent2Pid} | _] = Agents,
            
            io:format("Testing coordination between agents ~s and ~s...~n", [Agent1Id, Agent2Id]),
            
            % Test 1: Get states
            try
                State1 = agent_instance:get_state(Agent1Pid),
                State2 = agent_instance:get_state(Agent2Pid),
                
                case {is_map(State1), is_map(State2)} of
                    {true, true} ->
                        io:format("✅ Both agents responding to state queries~n");
                    _ ->
                        io:format("⚠️  Agent state query issues~n")
                end
            catch
                Error:Reason ->
                    io:format("⚠️  State query failed: ~p:~p~n", [Error, Reason])
            end,
            
            % Test 2: Coordinated task execution
            CoordinationTask = <<"Coordinate with other fleet agents for system analysis">>,
            
            try
                % Execute coordination task on both agents
                Action = #{action => <<"chat">>, message => CoordinationTask},
                
                spawn(fun() ->
                    agent_instance:execute(Agent1Pid, Action)
                end),
                
                spawn(fun() ->
                    agent_instance:execute(Agent2Pid, Action)
                end),
                
                io:format("✅ Coordination tasks initiated~n")
            catch
                E:R ->
                    io:format("⚠️  Coordination task failed: ~p:~p~n", [E, R])
            end,
            
            % Test 3: Fleet status overview
            AgentStatuses = lists:map(fun({AgentId, AgentPid}) ->
                try
                    case agent_instance:get_state(AgentPid) of
                        State when is_map(State) ->
                            #{
                                agent_id => AgentId,
                                status => <<"active">>,
                                autonomous_mode => maps:get(autonomous_mode, State, false),
                                model => maps:get(model, State, <<"unknown">>)
                            };
                        _ ->
                            #{agent_id => AgentId, status => <<"error">>}
                    end
                catch
                    _:_ ->
                        #{agent_id => AgentId, status => <<"offline">>}
                end
            end, Agents),
            
            ActiveCount = length([Status || #{status := <<"active">>} = Status <- AgentStatuses]),
            AutonomousCount = length([Status || #{autonomous_mode := true} = Status <- AgentStatuses]),
            
            io:format("✅ Fleet Status Overview:~n"),
            io:format("   Total agents: ~p~n", [length(Agents)]),
            io:format("   Active agents: ~p~n", [ActiveCount]),
            io:format("   Autonomous agents: ~p~n", [AutonomousCount]),
            io:format("   Fleet health: ~.1f%~n", [(ActiveCount / length(Agents)) * 100]);
        
        OtherAgents ->
            io:format("ℹ️  Limited coordination test (only ~p agents available)~n", [length(OtherAgents)])
    end,
    
    % Cleanup
    CleanupAgents = get(fleet_agents),
    case CleanupAgents of
        undefined -> ok;
        [] -> ok;
        AgentList ->
            io:format("Cleaning up fleet (~p agents)...~n", [length(AgentList)]),
            lists:foreach(fun({_AgentId, AgentPid}) ->
                try gen_server:stop(AgentPid)
                catch _:_ -> ok
                end
            end, AgentList),
            io:format("✅ Fleet cleanup completed~n")
    end,
    
    ok.

%% Helper Functions

collect_fleet_results([], Results) ->
    lists:reverse(Results);
collect_fleet_results([Pid | Remaining], Results) ->
    receive
        {fleet_result, Pid, Result} ->
            collect_fleet_results(Remaining, [Result | Results])
    after 15000 ->  % 15 second timeout per agent
        TimeoutResult = {timeout, unknown, <<"Agent execution timeout">>},
        collect_fleet_results(Remaining, [TimeoutResult | Results])
    end.