%% run_advanced_examples.erl
%% Runner module for executing all advanced agent examples
-module(run_advanced_examples).

%% API exports
-export([
    run_all/0,
    run_distributed/0,
    run_tool_composition/0,
    run_streaming/0,
    run_specific/1,
    demo_menu/0
]).

%% @doc Run all advanced examples
run_all() ->
    io:format("~n=== Running All Advanced Agent Examples ===~n~n"),
    
    Results = [
        {distributed, run_distributed()},
        {tool_composition, run_tool_composition()},
        {streaming, run_streaming()}
    ],
    
    print_summary(Results),
    Results.

%% @doc Run distributed agent examples
run_distributed() ->
    io:format("~n--- Distributed Agent Examples ---~n"),
    
    % Example 1: Start distributed cluster
    io:format("~n1. Starting distributed agent cluster...~n"),
    Nodes = [node()], % Use current node for demo
    ClusterResult = advanced_distributed_agents:start_distributed_cluster(Nodes),
    io:format("Cluster started: ~p~n", [maps:get(cluster_id, ClusterResult)]),
    
    % Example 2: Collaborative research
    io:format("~n2. Running collaborative research on Quantum Computing...~n"),
    ResearchResult = advanced_distributed_agents:collaborative_research(
        <<"Quantum Computing Applications">>, 
        [web_research, analysis, summarization]
    ),
    io:format("Research completed with ~p agents~n", [length(maps:get(research_agents, ResearchResult, []))]),
    
    % Example 3: Cross-node communication
    io:format("~n3. Demonstrating cross-node agent communication...~n"),
    CommResult = advanced_distributed_agents:cross_node_agent_communication(node(), node()),
    io:format("Communication established: ~p~n", [maps:is_key(channel, CommResult)]),
    
    % Example 4: Agent swarm computation
    io:format("~n4. Running agent swarm computation...~n"),
    SwarmResult = advanced_distributed_agents:agent_swarm_computation(
        <<"Optimize resource allocation">>, 
        5, 
        #{strategy => genetic}
    ),
    io:format("Swarm size: ~p, Convergence: ~p~n", 
        [maps:get(swarm_size, SwarmResult), 
         maps:get(convergence_metrics, SwarmResult)]),
    
    % Example 5: Hierarchical decision network
    io:format("~n5. Building hierarchical agent network...~n"),
    HierarchyResult = advanced_distributed_agents:hierarchical_agent_network(
        <<"Should we migrate to microservices?">>,
        #{depth => 3}
    ),
    io:format("Decision tree depth: ~p levels~n", [length(maps:get(decision_tree, HierarchyResult, #{}))]),
    
    % Example 6: Fault-tolerant group
    io:format("~n6. Creating fault-tolerant agent group...~n"),
    FaultResult = advanced_distributed_agents:fault_tolerant_agent_group(
        <<"Process critical data">>,
        3
    ),
    io:format("Availability: ~p%~n", 
        [maps:get(availability_metrics, FaultResult, #{uptime_percentage => 0})]),
    
    #{
        cluster => ClusterResult,
        research => ResearchResult,
        communication => CommResult,
        swarm => SwarmResult,
        hierarchy => HierarchyResult,
        fault_tolerance => FaultResult
    }.

%% @doc Run tool composition examples
run_tool_composition() ->
    io:format("~n--- Tool Composition Examples ---~n"),
    
    % Example 1: Code analysis pipeline
    io:format("~n1. Running code analysis pipeline...~n"),
    AnalysisResult = advanced_tool_composition:code_analysis_pipeline(
        <<"/Users/agent/agents.erl">>,
        #{include_performance => true}
    ),
    io:format("Analysis completed: Found ~p recommendations~n", 
        [length(maps:get(recommendations, AnalysisResult, []))]),
    
    % Example 2: Autonomous debugging
    io:format("~n2. Starting autonomous debugging session...~n"),
    DebugResult = advanced_tool_composition:autonomous_debugging_session(
        <<"undefined function clause matching error in agent:handle_call/3">>,
        <<"/Users/agent/agents.erl/apps/agent/src">>
    ),
    io:format("Debug tools used: ~p~n", [maps:get(debug_tools_used, DebugResult, [])]),
    
    % Example 3: Data science workflow
    io:format("~n3. Executing data science workflow...~n"),
    DataSciResult = advanced_tool_composition:data_science_workflow(
        <<"sample_dataset.csv">>,
        #{model_types => [regression, classification]}
    ),
    io:format("Models trained: ~p~n", [maps:get(models, DataSciResult, [])]),
    
    % Example 4: Security audit
    io:format("~n4. Running security audit chain...~n"),
    SecurityResult = advanced_tool_composition:security_audit_chain(
        <<"localhost:8080">>,
        #{compliance => [owasp, pci]}
    ),
    io:format("Risk score: ~p~n", [maps:get(risk_score, SecurityResult, unknown)]),
    
    % Example 5: Infrastructure automation
    io:format("~n5. Running infrastructure automation...~n"),
    InfraResult = advanced_tool_composition:infrastructure_automation(
        <<"Deploy new microservice">>,
        #{environment => dev}
    ),
    io:format("Deployment status: ~p~n", [maps:get(deployment, InfraResult, #{})]),
    
    % Example 6: Knowledge extraction
    io:format("~n6. Running knowledge extraction pipeline...~n"),
    KnowledgeResult = advanced_tool_composition:knowledge_extraction_pipeline(
        <<"technical_docs/">>,
        #{extract => [entities, relationships, topics]}
    ),
    io:format("Knowledge graph nodes: ~p~n", [maps:get(knowledge_graph, KnowledgeResult, #{})]),
    
    #{
        analysis => AnalysisResult,
        debugging => DebugResult,
        data_science => DataSciResult,
        security => SecurityResult,
        infrastructure => InfraResult,
        knowledge => KnowledgeResult
    }.

%% @doc Run streaming and async examples
run_streaming() ->
    io:format("~n--- Streaming & Async Examples ---~n"),
    
    % Example 1: Streaming data pipeline
    io:format("~n1. Starting streaming data pipeline...~n"),
    {ok, StreamPid} = advanced_streaming_async:streaming_data_pipeline(
        <<"data_stream">>,
        #{batch_size => 100}
    ),
    io:format("Stream processor started: ~p~n", [StreamPid]),
    
    % Example 2: Async agent orchestra
    io:format("~n2. Starting async agent orchestra...~n"),
    OrchestraResult = advanced_streaming_async:async_agent_orchestra(
        <<"Compose a solution for distributed computing">>,
        [
            <<"Design the architecture">>,
            <<"Implement core components">>,
            <<"Create test suite">>,
            <<"Write documentation">>
        ],
        #{parallelism => 4}
    ),
    io:format("Orchestra performance completed in ~p ms~n", 
        [maps:get(duration, maps:get(results, OrchestraResult, #{}), 0)]),
    
    % Example 3: Real-time chat processor
    io:format("~n3. Starting real-time chat processor...~n"),
    ClientPid = self(),
    {ok, ChatPid} = advanced_streaming_async:real_time_chat_processor(
        ClientPid,
        #{model => <<"gpt-4">>, streaming => true}
    ),
    io:format("Chat processor ready: ~p~n", [ChatPid]),
    
    % Example 4: Streaming code generator
    io:format("~n4. Starting streaming code generator...~n"),
    {ok, GenPid} = advanced_streaming_async:streaming_code_generator(
        <<"REST API server with authentication">>,
        #{language => erlang, streaming => true}
    ),
    io:format("Code generator started: ~p~n", [GenPid]),
    
    % Example 5: Async batch processor
    io:format("~n5. Running async batch processor...~n"),
    Items = lists:seq(1, 100),
    BatchResult = advanced_streaming_async:async_batch_processor(
        Items,
        fun(X) -> X * X end,
        #{batch_size => 10, parallelism => 4}
    ),
    io:format("Processed ~p items in ~p batches~n", 
        [maps:get(total_items, BatchResult), maps:get(batches_processed, BatchResult)]),
    
    % Example 6: Event-driven system
    io:format("~n6. Creating event-driven agent system...~n"),
    {ok, EventPid} = advanced_streaming_async:event_driven_agent_system(
        <<"AgentEventSystem">>,
        #{event_types => [user, system, data]}
    ),
    io:format("Event system monitor: ~p~n", [EventPid]),
    
    % Example 7: Streaming analysis engine
    io:format("~n7. Starting streaming analysis engine...~n"),
    {ok, AnalysisPid} = advanced_streaming_async:streaming_analysis_engine(
        <<"metrics_stream">>,
        #{window_size => 1000, alert_threshold => 0.95}
    ),
    io:format("Analysis engine started: ~p~n", [AnalysisPid]),
    
    % Cleanup streaming processes
    timer:sleep(1000),
    lists:foreach(fun(Pid) -> 
        catch Pid ! stop 
    end, [StreamPid, ChatPid, GenPid, EventPid, AnalysisPid]),
    
    #{
        streaming_pipeline => StreamPid,
        orchestra => OrchestraResult,
        chat_processor => ChatPid,
        code_generator => GenPid,
        batch_processor => BatchResult,
        event_system => EventPid,
        analysis_engine => AnalysisPid
    }.

%% @doc Run a specific example by name
run_specific(ExampleName) ->
    case ExampleName of
        distributed_cluster ->
            advanced_distributed_agents:start_distributed_cluster([node()]);
            
        collaborative_research ->
            advanced_distributed_agents:collaborative_research(
                <<"AI Ethics">>, [research, analysis]);
                
        agent_swarm ->
            advanced_distributed_agents:agent_swarm_computation(
                <<"Solve optimization problem">>, 10, #{});
                
        code_analysis ->
            advanced_tool_composition:code_analysis_pipeline(
                <<".">>, #{});
                
        autonomous_debug ->
            advanced_tool_composition:autonomous_debugging_session(
                <<"Sample error">>, <<".">>);
                
        streaming_pipeline ->
            advanced_streaming_async:streaming_data_pipeline(
                <<"stream">>, #{});
                
        async_orchestra ->
            advanced_streaming_async:async_agent_orchestra(
                <<"Main task">>, [<<"Sub1">>, <<"Sub2">>], #{});
                
        _ ->
            {error, unknown_example}
    end.

%% @doc Interactive demo menu
demo_menu() ->
    io:format("~n=== Advanced Agent Examples Demo Menu ===~n"),
    io:format("~n1. Distributed Agent Examples"),
    io:format("~n2. Tool Composition Examples"),
    io:format("~n3. Streaming & Async Examples"),
    io:format("~n4. Run All Examples"),
    io:format("~n5. Exit~n"),
    io:format("~nSelect option (1-5): "),
    
    case io:get_line("") of
        "1\n" -> run_distributed(), demo_menu();
        "2\n" -> run_tool_composition(), demo_menu();
        "3\n" -> run_streaming(), demo_menu();
        "4\n" -> run_all(), demo_menu();
        "5\n" -> io:format("Exiting demo...~n");
        _ -> io:format("Invalid option~n"), demo_menu()
    end.

%% Internal helpers

print_summary(Results) ->
    io:format("~n=== Summary of All Examples ===~n"),
    lists:foreach(fun({Category, Result}) ->
        io:format("~n~p: ", [Category]),
        case Result of
            #{} -> io:format("Completed successfully with ~p examples~n", [maps:size(Result)]);
            _ -> io:format("Result: ~p~n", [Result])
        end
    end, Results).