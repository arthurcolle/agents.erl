-module(test_autonomous_system).
-export([test_basic_functionality/0, test_advanced_agent/0]).

%% Test basic functionality of the autonomous system
test_basic_functionality() ->
    io:format("=== Testing Basic Autonomous System Functionality ===~n"),
    
    % Test 1: Start autonomous agency
    io:format("1. Testing autonomous agency startup...~n"),
    Config1 = #{agent_id => test_agent_1},
    case autonomous_agency:start_link(Config1) of
        {ok, AgencyPid} ->
            io:format("   ✓ Autonomous agency started successfully: ~p~n", [AgencyPid]),
            
            % Test environmental perception
            io:format("2. Testing environmental perception...~n"),
            TestInput = #{
                type => environmental_change,
                content => "Temperature increased by 5 degrees",
                timestamp => erlang:system_time(second)
            },
            
            case autonomous_agency:perceive_environment(AgencyPid, TestInput) of
                {ok, PerceptionResult} ->
                    io:format("   ✓ Environmental perception successful: ~p~n", [PerceptionResult]);
                {error, Reason} ->
                    io:format("   ✗ Environmental perception failed: ~p~n", [Reason])
            end,
            
            % Test goal formation
            io:format("3. Testing autonomous goal formation...~n"),
            GoalContext = #{
                domain => environment,
                urgency => medium,
                resources => available
            },
            
            case autonomous_agency:form_autonomous_goals(AgencyPid, GoalContext) of
                {ok, Goals} ->
                    io:format("   ✓ Goal formation successful: ~p~n", [Goals]);
                {error, Reason} ->
                    io:format("   ✗ Goal formation failed: ~p~n", [Reason])
            end,
            
            % Clean up
            exit(AgencyPid, shutdown),
            io:format("   ✓ Agency shutdown completed~n");
        {error, Reason} ->
            io:format("   ✗ Failed to start autonomous agency: ~p~n", [Reason])
    end,
    
    % Test 2: Start knowledge graph
    io:format("4. Testing dynamic knowledge graph...~n"),
    Config2 = #{agent_id => test_agent_2},
    case dynamic_knowledge_graph:start_link(Config2) of
        {ok, GraphPid} ->
            io:format("   ✓ Knowledge graph started successfully: ~p~n", [GraphPid]),
            
            % Test adding knowledge
            TestConcept = #{
                type => concept,
                name => "autonomous_learning",
                properties => #{domain => "AI", complexity => high}
            },
            
            case dynamic_knowledge_graph:add_concept(GraphPid, autonomous_learning, TestConcept) of
                {ok, ConceptResult} ->
                    io:format("   ✓ Knowledge addition successful: ~p~n", [ConceptResult]);
                {error, Reason} ->
                    io:format("   ✗ Knowledge addition failed: ~p~n", [Reason])
            end,
            
            % Clean up
            exit(GraphPid, shutdown),
            io:format("   ✓ Knowledge graph shutdown completed~n");
        {error, Reason} ->
            io:format("   ✗ Failed to start knowledge graph: ~p~n", [Reason])
    end,
    
    io:format("=== Basic functionality test completed ===~n").

%% Test the advanced autonomous agent
test_advanced_agent() ->
    io:format("=== Testing Advanced Autonomous Agent ===~n"),
    
    % Test advanced agent startup
    io:format("1. Testing advanced agent startup...~n"),
    Config = #{
        agent_id => advanced_test_agent,
        cognitive_components => all,
        consciousness_simulation => enabled,
        meta_cognition => enabled
    },
    
    case advanced_autonomous_agent:start_link(Config) of
        {ok, AgentPid} ->
            io:format("   ✓ Advanced agent started successfully: ~p~n", [AgentPid]),
            
            % Test environmental input processing
            io:format("2. Testing environmental input processing...~n"),
            EnvironmentalInput = #{
                type => complex_environmental_change,
                data => #{
                    temperature => 25,
                    humidity => 60,
                    new_objects => [car, tree, building],
                    social_presence => 3
                },
                novelty_level => 0.7,
                timestamp => erlang:system_time(second)
            },
            
            case advanced_autonomous_agent:process_environmental_input(AgentPid, EnvironmentalInput) of
                {ok, ProcessingResult} ->
                    io:format("   ✓ Environmental processing successful~n"),
                    io:format("     - Processing result keys: ~p~n", [maps:keys(ProcessingResult)]);
                {error, Reason} ->
                    io:format("   ✗ Environmental processing failed: ~p~n", [Reason])
            end,
            
            % Test autonomous decision making
            io:format("3. Testing autonomous decision making...~n"),
            DecisionContext = #{
                decision_type => strategic,
                available_options => [explore, exploit, wait, collaborate],
                constraints => #{time => limited, resources => moderate},
                stakes => medium
            },
            
            case advanced_autonomous_agent:autonomous_decision_making(AgentPid, DecisionContext) of
                {ok, DecisionResult} ->
                    io:format("   ✓ Decision making successful~n"),
                    io:format("     - Selected decision: ~p~n", [maps:get(selected_decision, DecisionResult, undefined)]),
                    io:format("     - Decision confidence: ~p~n", [maps:get(decision_confidence, DecisionResult, undefined)]);
                {error, Reason} ->
                    io:format("   ✗ Decision making failed: ~p~n", [Reason])
            end,
            
            % Test meta-cognitive reflection
            io:format("4. Testing meta-cognitive reflection...~n"),
            Experience = #{
                type => learning_experience,
                content => "Successfully adapted to new environment",
                outcome => positive,
                insights => ["adaptation is key", "curiosity drives learning"],
                timestamp => erlang:system_time(second)
            },
            
            case advanced_autonomous_agent:reflect_on_experience(AgentPid, Experience) of
                {ok, ReflectionResult} ->
                    io:format("   ✓ Meta-cognitive reflection successful~n"),
                    io:format("     - Reflection insights: ~p~n", [maps:get(reflection_insights, ReflectionResult, [])]);
                {error, Reason} ->
                    io:format("   ✗ Meta-cognitive reflection failed: ~p~n", [Reason])
            end,
            
            % Test creative problem solving
            io:format("5. Testing creative problem solving...~n"),
            Problem = #{
                type => adaptation_challenge,
                description => "How to learn efficiently in a rapidly changing environment",
                constraints => [limited_time, uncertain_data, changing_goals],
                desired_outcome => "robust learning strategy"
            },
            CreativityContext = #{
                novelty_encouragement => high,
                risk_tolerance => medium,
                exploration_bias => 0.7
            },
            
            case advanced_autonomous_agent:creative_problem_solving(AgentPid, Problem, CreativityContext) of
                {ok, CreativeResult} ->
                    io:format("   ✓ Creative problem solving successful~n"),
                    io:format("     - Creative solutions generated: ~p~n", [length(maps:get(creative_solutions, CreativeResult, []))]);
                {error, Reason} ->
                    io:format("   ✗ Creative problem solving failed: ~p~n", [Reason])
            end,
            
            % Let the agent run for a bit to test periodic cycles
            io:format("6. Testing periodic cognitive cycles (waiting 5 seconds)...~n"),
            timer:sleep(5000),
            io:format("   ✓ Agent ran periodic cycles successfully~n"),
            
            % Clean up
            exit(AgentPid, shutdown),
            io:format("   ✓ Advanced agent shutdown completed~n");
        {error, Reason} ->
            io:format("   ✗ Failed to start advanced agent: ~p~n", [Reason])
    end,
    
    io:format("=== Advanced agent test completed ===~n").