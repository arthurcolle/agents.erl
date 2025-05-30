-module(runtime_test).
-export([test_component_startup/0]).

%% Test that components can actually start and respond
test_component_startup() ->
    io:format("Testing component startup and basic operations...~n"),
    
    % Test 1: Autonomous Agency
    io:format("1. Testing autonomous_agency startup...~n"),
    Config1 = #{agent_id => <<"test_agent_1">>},
    case autonomous_agency:start_link(Config1) of
        {ok, AgencyPid} ->
            io:format("   ✓ Autonomous agency started: ~p~n", [AgencyPid]),
            
            % Test a simple operation
            TestInput = #{
                type => test_perception,
                content => <<"test environmental input">>,
                timestamp => erlang:system_time(second)
            },
            
            try
                Result = autonomous_agency:perceive_environment(AgencyPid, TestInput),
                io:format("   ✓ Perception test result: ~p~n", [Result])
            catch
                Error:Reason ->
                    io:format("   ! Perception test error: ~p:~p~n", [Error, Reason])
            end,
            
            % Clean shutdown
            exit(AgencyPid, shutdown),
            io:format("   ✓ Agency shutdown completed~n");
        {error, Reason} ->
            io:format("   ✗ Failed to start agency: ~p~n", [Reason])
    end,
    
    % Test 2: Knowledge Graph  
    io:format("2. Testing dynamic_knowledge_graph startup...~n"),
    Config2 = #{agent_id => <<"test_agent_2">>},
    case dynamic_knowledge_graph:start_link(Config2) of
        {ok, GraphPid} ->
            io:format("   ✓ Knowledge graph started: ~p~n", [GraphPid]),
            
            % Test adding a node
            TestNodeData = #{
                type => concept,
                name => <<"test_concept">>,
                properties => #{test => true}
            },
            
            try
                Result2 = dynamic_knowledge_graph:add_node(GraphPid, test_node_1, TestNodeData),
                io:format("   ✓ Node addition result: ~p~n", [Result2])
            catch
                Error2:Reason2 ->
                    io:format("   ! Node addition error: ~p:~p~n", [Error2, Reason2])
            end,
            
            % Clean shutdown
            exit(GraphPid, shutdown),
            io:format("   ✓ Graph shutdown completed~n");
        {error, Reason2} ->
            io:format("   ✗ Failed to start graph: ~p~n", [Reason2])
    end,
    
    % Test 3: Goal Planner
    io:format("3. Testing autonomous_goal_planner startup...~n"),
    Config3 = #{agent_id => <<"test_agent_3">>},
    case autonomous_goal_planner:start_link(Config3) of
        {ok, PlannerPid} ->
            io:format("   ✓ Goal planner started: ~p~n", [PlannerPid]),
            
            % Test goal formation
            GoalContext = #{
                domain => learning,
                urgency => medium,
                resources => available
            },
            
            try
                Result3 = autonomous_goal_planner:form_autonomous_goals(PlannerPid, GoalContext),
                io:format("   ✓ Goal formation result: ~p~n", [Result3])
            catch
                Error3:Reason3 ->
                    io:format("   ! Goal formation error: ~p:~p~n", [Error3, Reason3])
            end,
            
            % Clean shutdown
            exit(PlannerPid, shutdown),
            io:format("   ✓ Planner shutdown completed~n");
        {error, Reason3} ->
            io:format("   ✗ Failed to start planner: ~p~n", [Reason3])
    end,
    
    % Test 4: Advanced Agent (quick test)
    io:format("4. Testing advanced_autonomous_agent startup...~n"),
    Config4 = #{agent_id => <<"advanced_test_agent">>},
    case catch advanced_autonomous_agent:start_link(Config4) of
        {ok, AdvancedPid} ->
            io:format("   ✓ Advanced agent started: ~p~n", [AdvancedPid]),
            
            % Let it run briefly
            timer:sleep(1000),
            
            % Clean shutdown
            exit(AdvancedPid, shutdown),
            io:format("   ✓ Advanced agent shutdown completed~n");
        Error4 ->
            io:format("   ! Advanced agent startup issue: ~p~n", [Error4])
    end,
    
    io:format("Component startup testing completed!~n"),
    ok.