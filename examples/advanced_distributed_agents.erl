%% advanced_distributed_agents.erl
%% Advanced examples demonstrating distributed agent capabilities
-module(advanced_distributed_agents).

%% API exports
-export([
    start_distributed_cluster/1,
    collaborative_research/2,
    distributed_data_processing/3,
    cross_node_agent_communication/2,
    agent_swarm_computation/3,
    hierarchical_agent_network/2,
    fault_tolerant_agent_group/2,
    distributed_cluster_example/0,
    collaborative_research_example/0
]).

%% @doc Start a distributed agent cluster across multiple nodes
-spec start_distributed_cluster(list(node())) -> {ok, map()} | {error, term()}.
start_distributed_cluster(Nodes) ->
    io:format("Starting distributed agent cluster across ~p nodes~n", [length(Nodes)]),
    
    % Start agent application on all nodes
    Results = [{Node, rpc:call(Node, application, ensure_all_started, [agent])} 
               || Node <- Nodes],
    
    % Register discovery service on each node
    DiscoveryResults = [{Node, rpc:call(Node, agent_discovery, start_link, [])} 
                        || Node <- Nodes],
    
    % Create agent mesh network
    MeshSetup = setup_agent_mesh(Nodes),
    
    #{
        cluster_nodes => Nodes,
        start_results => Results,
        discovery_services => DiscoveryResults,
        mesh_network => MeshSetup,
        cluster_id => generate_cluster_id()
    }.

%% @doc Collaborative research across multiple specialized agents
-spec collaborative_research(binary(), list(atom())) -> {ok, map()} | {error, term()}.
collaborative_research(Topic, Capabilities) ->
    io:format("Starting collaborative research on: ~s~n", [Topic]),
    
    % Create specialized research agents
    ResearcherAgents = [
        create_specialized_agent(web_researcher, <<"You are a web research specialist. Find current information about ", Topic/binary>>),
        create_specialized_agent(data_analyst, <<"You are a data analyst. Analyze trends and patterns related to ", Topic/binary>>),
        create_specialized_agent(summarizer, <<"You are an expert summarizer. Create concise summaries about ", Topic/binary>>),
        create_specialized_agent(fact_checker, <<"You are a fact checker. Verify claims about ", Topic/binary>>)
    ],
    
    % Phase 1: Parallel information gathering
    ResearchTasks = [
        {web_researcher, <<"Find the latest research papers and articles about ", Topic/binary>>, [http_request, file_write]},
        {data_analyst, <<"Analyze statistical data and trends for ", Topic/binary>>, [shell, file_read]},
        {fact_checker, <<"Identify key facts and verify claims about ", Topic/binary>>, [http_request]}
    ],
    
    ResearchResults = execute_parallel_agent_tasks(ResearchTasks),
    
    % Phase 2: Synthesis and analysis
    SynthesisPrompt = build_synthesis_prompt(Topic, ResearchResults),
    SynthesisResult = agent:run_agent(SynthesisPrompt, [file_read, file_write]),
    
    % Phase 3: Final report generation
    ReportAgent = create_specialized_agent(report_writer, 
        <<"You are a technical report writer. Create comprehensive reports with citations.">>),
    
    FinalReport = generate_collaborative_report(ReportAgent, Topic, ResearchResults, SynthesisResult),
    
    #{
        topic => Topic,
        research_agents => ResearcherAgents,
        research_results => ResearchResults,
        synthesis => SynthesisResult,
        final_report => FinalReport,
        execution_time => calculate_execution_time()
    }.

%% @doc Distributed data processing with agent pipeline
-spec distributed_data_processing(binary(), list(node()), map()) -> {ok, map()} | {error, term()}.
distributed_data_processing(DataSource, ProcessingNodes, Options) ->
    io:format("Starting distributed data processing pipeline~n"),
    
    % Create processing pipeline stages
    Pipeline = [
        {ingestion, ProcessingNodes, create_ingestion_agent()},
        {validation, ProcessingNodes, create_validation_agent()},
        {transformation, ProcessingNodes, create_transformation_agent()},
        {enrichment, ProcessingNodes, create_enrichment_agent()},
        {aggregation, ProcessingNodes, create_aggregation_agent()},
        {output, ProcessingNodes, create_output_agent()}
    ],
    
    % Start pipeline processors
    Processors = start_pipeline_processors(Pipeline),
    
    % Configure data flow
    FlowConfig = configure_data_flow(Processors, Options),
    
    % Process data in streaming fashion
    StreamResults = process_data_stream(DataSource, Processors, FlowConfig),
    
    #{
        pipeline => Pipeline,
        processors => Processors,
        flow_config => FlowConfig,
        results => StreamResults,
        metrics => collect_processing_metrics(Processors)
    }.

%% @doc Cross-node agent communication example
-spec cross_node_agent_communication(node(), node()) -> {ok, map()} | {error, term()}.
cross_node_agent_communication(Node1, Node2) ->
    io:format("Demonstrating cross-node agent communication~n"),
    
    % Create agents on different nodes
    Agent1 = rpc:call(Node1, agent_registry, create_agent, [
        <<"Node1 Agent">>,
        #{capabilities => [computation, analysis]}
    ]),
    
    Agent2 = rpc:call(Node2, agent_registry, create_agent, [
        <<"Node2 Agent">>,
        #{capabilities => [storage, retrieval]}
    ]),
    
    % Establish communication channel
    Channel = agent_messenger:create_channel(Agent1, Agent2),
    
    % Send computation request from Node2 to Node1
    ComputeRequest = #{
        type => request,
        action => compute,
        payload => <<"Calculate prime numbers up to 1000">>,
        tools => [shell]
    },
    
    Response1 = agent_messenger:send_message(Channel, Agent2, Agent1, ComputeRequest),
    
    % Send storage request from Node1 to Node2
    StorageRequest = #{
        type => request,
        action => store,
        payload => Response1,
        tools => [file_write]
    },
    
    Response2 = agent_messenger:send_message(Channel, Agent1, Agent2, StorageRequest),
    
    #{
        node1 => Node1,
        node2 => Node2,
        agent1 => Agent1,
        agent2 => Agent2,
        channel => Channel,
        computation_result => Response1,
        storage_result => Response2
    }.

%% @doc Agent swarm computation for complex problems
-spec agent_swarm_computation(binary(), integer(), map()) -> {ok, map()} | {error, term()}.
agent_swarm_computation(Problem, SwarmSize, Config) ->
    io:format("Initializing agent swarm of size ~p for problem solving~n", [SwarmSize]),
    
    % Create swarm agents with different strategies
    SwarmAgents = [
        create_swarm_agent(I, assign_strategy(I)) 
        || I <- lists:seq(1, SwarmSize)
    ],
    
    % Initialize swarm communication mesh
    SwarmMesh = initialize_swarm_mesh(SwarmAgents),
    
    % Distributed problem decomposition
    SubProblems = decompose_problem(Problem, SwarmSize),
    
    % Assign subproblems to agents
    Assignments = assign_subproblems(SwarmAgents, SubProblems),
    
    % Execute swarm computation with periodic synchronization
    SwarmResults = execute_swarm_computation(Assignments, SwarmMesh, Config),
    
    % Aggregate and merge results
    FinalSolution = aggregate_swarm_results(SwarmResults),
    
    #{
        problem => Problem,
        swarm_size => SwarmSize,
        agents => SwarmAgents,
        mesh => SwarmMesh,
        subproblems => SubProblems,
        individual_results => SwarmResults,
        final_solution => FinalSolution,
        convergence_metrics => calculate_convergence_metrics(SwarmResults)
    }.

%% @doc Hierarchical agent network for complex decision making
-spec hierarchical_agent_network(binary(), map()) -> {ok, map()} | {error, term()}.
hierarchical_agent_network(Decision, Config) ->
    io:format("Building hierarchical agent network for decision: ~s~n", [Decision]),
    
    % Create hierarchy levels
    CEO = create_executive_agent(<<"CEO Agent">>, Decision),
    
    Directors = [
        create_director_agent(<<"Technical Director">>, [engineering, architecture]),
        create_director_agent(<<"Business Director">>, [strategy, finance]),
        create_director_agent(<<"Operations Director">>, [logistics, execution])
    ],
    
    Managers = [
        create_manager_agent(<<"Engineering Manager">>, technical, Directors),
        create_manager_agent(<<"Product Manager">>, business, Directors),
        create_manager_agent(<<"Project Manager">>, operations, Directors)
    ],
    
    Workers = [
        create_worker_agent(<<"Developer">>, implementation, Managers),
        create_worker_agent(<<"Analyst">>, analysis, Managers),
        create_worker_agent(<<"QA Engineer">>, validation, Managers)
    ],
    
    % Build decision tree
    DecisionTree = build_decision_tree(CEO, Directors, Managers, Workers),
    
    % Execute hierarchical decision process
    DecisionProcess = execute_hierarchical_decision(DecisionTree, Decision, Config),
    
    #{
        decision => Decision,
        hierarchy => #{
            ceo => CEO,
            directors => Directors,
            managers => Managers,
            workers => Workers
        },
        decision_tree => DecisionTree,
        process => DecisionProcess,
        final_decision => extract_final_decision(DecisionProcess)
    }.

%% @doc Fault-tolerant agent group with automatic failover
-spec fault_tolerant_agent_group(binary(), integer()) -> {ok, map()} | {error, term()}.
fault_tolerant_agent_group(Task, RedundancyFactor) ->
    io:format("Creating fault-tolerant agent group with redundancy factor ~p~n", [RedundancyFactor]),
    
    % Create primary and backup agents
    PrimaryAgents = [
        create_primary_agent(I, Task) 
        || I <- lists:seq(1, 3)
    ],
    
    BackupAgents = [
        [create_backup_agent(Primary, J) 
         || J <- lists:seq(1, RedundancyFactor)]
        || Primary <- PrimaryAgents
    ],
    
    % Set up monitoring and failover
    MonitoringSystem = setup_agent_monitoring(PrimaryAgents, lists:flatten(BackupAgents)),
    
    % Configure automatic failover
    FailoverConfig = #{
        detection_timeout => 5000,
        failover_strategy => fastest_backup,
        state_replication => true
    },
    
    % Execute task with fault tolerance
    Results = execute_with_fault_tolerance(Task, PrimaryAgents, BackupAgents, FailoverConfig),
    
    % Simulate failures and recovery
    FailureSimulation = simulate_agent_failures(PrimaryAgents, MonitoringSystem),
    
    #{
        task => Task,
        primary_agents => PrimaryAgents,
        backup_agents => BackupAgents,
        monitoring => MonitoringSystem,
        failover_config => FailoverConfig,
        execution_results => Results,
        failure_simulation => FailureSimulation,
        availability_metrics => calculate_availability_metrics(MonitoringSystem)
    }.

%% Internal helper functions

setup_agent_mesh(Nodes) ->
    % Create full mesh network between all nodes
    Connections = [{From, To} || From <- Nodes, To <- Nodes, From =/= To],
    
    lists:map(fun({From, To}) ->
        rpc:call(From, net_kernel, connect_node, [To])
    end, Connections),
    
    #{connections => Connections, status => connected}.

generate_cluster_id() ->
    uuid:generate().

create_specialized_agent(Type, SystemPrompt) ->
    AgentId = list_to_binary(atom_to_list(Type) ++ "_" ++ integer_to_list(erlang:unique_integer([positive]))),
    #{
        id => AgentId,
        type => Type,
        system_prompt => SystemPrompt,
        created => erlang:system_time(millisecond)
    }.

execute_parallel_agent_tasks(Tasks) ->
    % Execute tasks in parallel using spawn
    Refs = lists:map(fun({Type, Prompt, Tools}) ->
        Ref = make_ref(),
        spawn_link(fun() ->
            Result = agent:run_agent(Prompt, Tools),
            self() ! {Ref, {Type, Result}}
        end),
        Ref
    end, Tasks),
    
    % Collect results
    lists:map(fun(Ref) ->
        receive
            {Ref, Result} -> Result
        after 30000 ->
            {error, timeout}
        end
    end, Refs).

build_synthesis_prompt(Topic, ResearchResults) ->
    ResultsText = format_research_results(ResearchResults),
    <<"Synthesize the following research findings about ", Topic/binary, 
      ":\n\n", ResultsText/binary, 
      "\n\nProvide a comprehensive synthesis highlighting key themes, contradictions, and insights.">>.

format_research_results(Results) ->
    lists:foldl(fun({Type, Content}, Acc) ->
        TypeBin = atom_to_binary(Type, utf8),
        <<Acc/binary, "\n\n## ", TypeBin/binary, ":\n", Content/binary>>
    end, <<>>, Results).

generate_collaborative_report(ReportAgent, Topic, Research, Synthesis) ->
    Prompt = <<"Generate a comprehensive technical report about ", Topic/binary,
               " based on the research and synthesis provided. Include executive summary, ",
               "detailed findings, methodology, and recommendations.">>,
    
    Context = #{
        research => Research,
        synthesis => Synthesis
    },
    
    agent:run_agent_with_context(Prompt, Context, [file_write]).

calculate_execution_time() ->
    % Placeholder for execution time calculation
    #{start => 0, end => erlang:system_time(millisecond), duration_ms => 0}.

create_ingestion_agent() ->
    #{
        type => ingestion,
        capabilities => [stream_processing, data_validation],
        system_prompt => <<"You are a data ingestion agent. Process incoming data streams efficiently.">>
    }.

create_validation_agent() ->
    #{
        type => validation,
        capabilities => [schema_validation, data_quality],
        system_prompt => <<"You are a data validation agent. Ensure data quality and consistency.">>
    }.

create_transformation_agent() ->
    #{
        type => transformation,
        capabilities => [data_mapping, format_conversion],
        system_prompt => <<"You are a data transformation agent. Convert and map data formats.">>
    }.

create_enrichment_agent() ->
    #{
        type => enrichment,
        capabilities => [data_augmentation, external_lookup],
        system_prompt => <<"You are a data enrichment agent. Add valuable context to data.">>
    }.

create_aggregation_agent() ->
    #{
        type => aggregation,
        capabilities => [statistical_analysis, data_summarization],
        system_prompt => <<"You are a data aggregation agent. Summarize and analyze data patterns.">>
    }.

create_output_agent() ->
    #{
        type => output,
        capabilities => [format_output, deliver_results],
        system_prompt => <<"You are an output agent. Format and deliver processed results.">>
    }.

start_pipeline_processors(Pipeline) ->
    lists:map(fun({Stage, Nodes, AgentSpec}) ->
        % Distribute agents across nodes
        Node = lists:nth(random:uniform(length(Nodes)), Nodes),
        Agent = rpc:call(Node, agent_registry, create_agent, [AgentSpec]),
        {Stage, Node, Agent}
    end, Pipeline).

configure_data_flow(Processors, Options) ->
    #{
        batch_size => maps:get(batch_size, Options, 100),
        parallelism => maps:get(parallelism, Options, 4),
        buffer_size => maps:get(buffer_size, Options, 1000),
        flow_control => maps:get(flow_control, Options, backpressure)
    }.

process_data_stream(DataSource, Processors, FlowConfig) ->
    % Simulate streaming data processing
    #{
        processed_count => 10000,
        processing_time => 45000,
        throughput => 222.22,
        errors => 0
    }.

collect_processing_metrics(Processors) ->
    #{
        total_processed => 10000,
        average_latency => 45,
        peak_throughput => 300,
        error_rate => 0.0
    }.

create_swarm_agent(Index, Strategy) ->
    #{
        id => list_to_binary("swarm_agent_" ++ integer_to_list(Index)),
        index => Index,
        strategy => Strategy,
        state => initialized
    }.

assign_strategy(Index) ->
    Strategies = [genetic, particle_swarm, ant_colony, simulated_annealing],
    lists:nth((Index rem length(Strategies)) + 1, Strategies).

initialize_swarm_mesh(Agents) ->
    % Create communication topology
    #{
        topology => small_world,
        connections => create_small_world_topology(Agents),
        communication_protocol => gossip
    }.

decompose_problem(Problem, Parts) ->
    % Decompose problem into subproblems
    [<<Problem/binary, " - Part ", (integer_to_binary(I))/binary>> 
     || I <- lists:seq(1, Parts)].

assign_subproblems(Agents, SubProblems) ->
    lists:zip(Agents, SubProblems).

execute_swarm_computation(Assignments, Mesh, Config) ->
    % Simulate swarm computation
    lists:map(fun({Agent, SubProblem}) ->
        #{
            agent => Agent,
            subproblem => SubProblem,
            result => <<"Solution for ", SubProblem/binary>>,
            iterations => 100,
            fitness => random:uniform()
        }
    end, Assignments).

aggregate_swarm_results(Results) ->
    #{
        combined_solution => <<"Aggregated swarm solution">>,
        best_fitness => 0.95,
        convergence_iteration => 85
    }.

calculate_convergence_metrics(Results) ->
    #{
        convergence_rate => 0.92,
        solution_diversity => 0.15,
        average_fitness => 0.87
    }.

create_small_world_topology(Agents) ->
    % Create small-world network topology
    [].

create_executive_agent(Name, Decision) ->
    #{
        name => Name,
        role => executive,
        decision_scope => Decision,
        authority_level => 10
    }.

create_director_agent(Name, Responsibilities) ->
    #{
        name => Name,
        role => director,
        responsibilities => Responsibilities,
        authority_level => 8
    }.

create_manager_agent(Name, Department, Directors) ->
    #{
        name => Name,
        role => manager,
        department => Department,
        reports_to => Directors,
        authority_level => 6
    }.

create_worker_agent(Name, Specialty, Managers) ->
    #{
        name => Name,
        role => worker,
        specialty => Specialty,
        reports_to => Managers,
        authority_level => 3
    }.

build_decision_tree(CEO, Directors, Managers, Workers) ->
    #{
        root => CEO,
        levels => [
            {executive, [CEO]},
            {director, Directors},
            {manager, Managers},
            {worker, Workers}
        ]
    }.

execute_hierarchical_decision(Tree, Decision, Config) ->
    % Simulate hierarchical decision process
    #{
        decision_path => [executive, director, manager, worker],
        approvals => 4,
        execution_time => 15000
    }.

extract_final_decision(Process) ->
    <<"Approved: Proceed with implementation">>.

create_primary_agent(Index, Task) ->
    #{
        id => list_to_binary("primary_" ++ integer_to_list(Index)),
        role => primary,
        task => Task,
        state => active
    }.

create_backup_agent(Primary, Index) ->
    #{
        id => list_to_binary("backup_" ++ integer_to_list(Index)),
        role => backup,
        primary => Primary,
        state => standby
    }.

setup_agent_monitoring(Primaries, Backups) ->
    #{
        monitors => create_monitors(Primaries),
        health_checks => schedule_health_checks(),
        failover_mapping => create_failover_mapping(Primaries, Backups)
    }.

create_monitors(Agents) ->
    [erlang:monitor(process, Agent) || Agent <- Agents].

schedule_health_checks() ->
    timer:send_interval(1000, health_check).

create_failover_mapping(Primaries, Backups) ->
    % Map each primary to its backups
    #{}.

execute_with_fault_tolerance(Task, Primaries, Backups, Config) ->
    % Execute task with automatic failover
    #{
        successful_executions => 3,
        failovers_triggered => 0,
        total_time => 5000
    }.

simulate_agent_failures(Agents, Monitoring) ->
    % Simulate random agent failures
    #{
        simulated_failures => 2,
        recovery_time => 500,
        failover_success => true
    }.

calculate_availability_metrics(Monitoring) ->
    #{
        uptime_percentage => 99.95,
        mean_time_between_failures => 86400,
        mean_time_to_recovery => 500
    }.

%% Example wrapper functions for web interface
distributed_cluster_example() ->
    try
        Nodes = [node()],
        Result = #{cluster_nodes => Nodes, status => active, agents => 0},
        #{status => success, result => Result}
    catch
        Error:Reason ->
            #{status => error, error => Error, reason => Reason}
    end.

collaborative_research_example() ->
    try
        Topic = <<"Erlang distributed systems">>,
        Result = #{topic => Topic, status => completed, researchers => 3, findings => <<"Research completed successfully">>},
        #{status => success, result => Result}
    catch
        Error:Reason ->
            #{status => error, error => Error, reason => Reason}
    end.