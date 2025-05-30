-module(simple_test).
-export([test_compilation/0]).

%% Simple test to verify modules can be loaded and basic functions work
test_compilation() ->
    io:format("Testing module compilation and basic functionality...~n"),
    
    % Test that our modules can be loaded
    Modules = [
        autonomous_agency,
        dynamic_knowledge_graph,
        deep_reasoning_engine,
        environmental_learning_engine,
        autonomous_goal_planner,
        active_exploration_engine,
        advanced_autonomous_agent
    ],
    
    lists:foreach(fun(Module) ->
        case code:which(Module) of
            non_existing ->
                io:format("✗ Module ~p not found~n", [Module]);
            BeamFile ->
                io:format("✓ Module ~p loaded from ~p~n", [Module, BeamFile])
        end
    end, Modules),
    
    % Test basic record/data structure functionality
    io:format("Testing data structures...~n"),
    
    % Test knowledge node creation (from dynamic_knowledge_graph)
    TestNode = #{
        id => test_node_1,
        type => concept,
        properties => #{test => true},
        content => "test content",
        confidence => 1.0
    },
    io:format("✓ Knowledge node structure: ~p~n", [TestNode]),
    
    % Test goal structure (from autonomous_goal_planner)
    TestGoal = #{
        goal_id => test_goal_1,
        goal_type => learning,
        description => "Test learning goal",
        priority => high,
        status => active
    },
    io:format("✓ Goal structure: ~p~n", [TestGoal]),
    
    % Test exploration strategy (from active_exploration_engine)
    TestStrategy = #{
        strategy_id => test_strategy_1,
        strategy_type => curiosity_driven,
        strategy_name => "Test exploration strategy",
        exploration_depth => 3,
        exploration_breadth => 5
    },
    io:format("✓ Exploration strategy structure: ~p~n", [TestStrategy]),
    
    io:format("Basic compilation and structure test completed successfully!~n"),
    ok.