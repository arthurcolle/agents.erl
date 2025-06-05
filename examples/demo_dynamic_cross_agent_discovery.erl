#!/usr/bin/env escript
%% -*- erlang -*-
%%% @doc Comprehensive demonstration of dynamic cross-agent resource discovery
%%% Shows agents discovering and retrieving different model constructs across the fleet

-module(demo_dynamic_cross_agent_discovery).

main(_) ->
    io:format("🚀 Starting Dynamic Cross-Agent Discovery Demonstration~n"),
    io:format("=======================================================~n~n"),
    
    %% Start the OTP applications
    application:ensure_all_started(agents),
    application:ensure_all_started(openai),
    application:ensure_all_started(agent_web),
    
    %% Wait for systems to initialize
    timer:sleep(3000),
    
    %% Run comprehensive demonstration
    run_demonstration_scenarios(),
    
    io:format("~n✅ Dynamic Cross-Agent Discovery Demonstration Complete!~n").

run_demonstration_scenarios() ->
    io:format("📋 Setting up demonstration environment...~n"),
    setup_demo_environment(),
    
    io:format("~n🔍 Scenario 1: Cross-Agent Tool Discovery and Execution~n"),
    demonstrate_tool_discovery(),
    
    io:format("~n🧠 Scenario 2: Memory Sharing Across Agent Boundaries~n"),
    demonstrate_memory_sharing(),
    
    io:format("~n📁 Scenario 3: Distributed File Resource Discovery~n"),
    demonstrate_file_discovery(),
    
    io:format("~n💬 Scenario 4: Conversation Routing and Message Discovery~n"),
    demonstrate_conversation_discovery(),
    
    io:format("~n🌐 Scenario 5: Multi-Source Unified Resource Retrieval~n"),
    demonstrate_unified_retrieval(),
    
    io:format("~n⚡ Scenario 6: Real-Time Autonomous Agent Coordination~n"),
    demonstrate_autonomous_coordination(),
    
    io:format("~n🎯 Scenario 7: Fleet-Wide Resource Optimization~n"),
    demonstrate_fleet_optimization(),
    
    io:format("~n🧹 Cleaning up demonstration environment...~n"),
    cleanup_demo_environment().

setup_demo_environment() ->
    %% Start core discovery systems
    {ok, _RouterPid} = dynamic_agent_router:start_link(),
    {ok, _RegistryPid} = model_construct_registry:start_link(),
    {ok, _DiscoveryPid} = dynamic_discovery_engine:start_link(),
    {ok, _RetrievalPid} = agent_retrieval_system:start_link(),
    
    %% Create specialized agent fleet
    DemoAgents = [
        #{
            id => <<"weather_specialist">>,
            type => <<"weather_agent">>,
            capabilities => [<<"weather_forecast">>, <<"climate_analysis">>, <<"temperature_conversion">>],
            tools => [
                #{name => <<"get_weather">>, description => <<"Get current weather for location">>},
                #{name => <<"weather_forecast">>, description => <<"Get 7-day weather forecast">>},
                #{name => <<"climate_stats">>, description => <<"Get historical climate statistics">>}
            ],
            memories => [
                #{id => <<"weather_preferences">>, content => <<"User prefers Celsius, detailed forecasts">>},
                #{id => <<"location_history">>, content => <<"Frequently requested: DC, NYC, SF">>}
            ]
        },
        #{
            id => <<"data_analyst">>,
            type => <<"analysis_agent">>,
            capabilities => [<<"data_processing">>, <<"statistical_analysis">>, <<"visualization">>],
            tools => [
                #{name => <<"analyze_data">>, description => <<"Perform statistical analysis on datasets">>},
                #{name => <<"create_chart">>, description => <<"Generate data visualizations">>},
                #{name => <<"correlation_analysis">>, description => <<"Find correlations in data">>}
            ],
            files => [
                #{id => <<"weather_dataset_2024">>, path => <<"/data/weather_2024.json">>, type => <<"json">>},
                #{id => <<"climate_trends">>, path => <<"/data/climate_trends.csv">>, type => <<"csv">>}
            ]
        },
        #{
            id => <<"communication_hub">>,
            type => <<"coordinator_agent">>,
            capabilities => [<<"task_routing">>, <<"agent_coordination">>, <<"workflow_management">>],
            conversations => [
                #{id => <<"weather_requests">>, topic => <<"weather_inquiries">>, participants => [<<"user">>, <<"weather_specialist">>]},
                #{id => <<"data_analysis_session">>, topic => <<"data_processing">>, participants => [<<"user">>, <<"data_analyst">>]}
            ],
            tools => [
                #{name => <<"route_request">>, description => <<"Route requests to appropriate agents">>},
                #{name => <<"coordinate_workflow">>, description => <<"Coordinate multi-agent workflows">>}
            ]
        },
        #{
            id => <<"knowledge_keeper">>,
            type => <<"knowledge_agent">>,
            capabilities => [<<"information_storage">>, <<"knowledge_retrieval">>, <<"learning">>],
            memories => [
                #{id => <<"domain_knowledge">>, content => <<"Extensive knowledge base on weather patterns">>},
                #{id => <<"user_interactions">>, content => <<"History of user preferences and behaviors">>},
                #{id => <<"system_insights">>, content => <<"Insights about agent coordination patterns">>}
            ],
            files => [
                #{id => <<"knowledge_base">>, path => <<"/kb/weather_knowledge.json">>, type => <<"json">>},
                #{id => <<"interaction_logs">>, path => <<"/logs/interactions.log">>, type => <<"log">>}
            ]
        }
    ],
    
    %% Initialize agents and register resources
    lists:foreach(fun(AgentSpec) ->
        AgentId = maps:get(id, AgentSpec),
        
        %% Start agent instance
        {ok, AgentPid} = agent_instance:start_link(AgentId, #{}),
        put({agent_pid, AgentId}, AgentPid),
        
        %% Register capabilities
        Capabilities = maps:get(capabilities, AgentSpec),
        lists:foreach(fun(Capability) ->
            dynamic_agent_router:register_agent_capability(AgentId, Capability)
        end, Capabilities),
        
        %% Register tools
        Tools = maps:get(tools, AgentSpec, []),
        lists:foreach(fun(Tool) ->
            ToolId = <<AgentId/binary, "_", (maps:get(name, Tool))/binary>>,
            model_construct_registry:register_tool(ToolId, Tool#{
                implementation => <<"demo_tool">>,
                access_level => public,
                schema => #{type => object, properties => #{}}
            }, AgentId)
        end, Tools),
        
        %% Register memories
        Memories = maps:get(memories, AgentSpec, []),
        lists:foreach(fun(Memory) ->
            MemoryId = <<AgentId/binary, "_", (maps:get(id, Memory))/binary>>,
            model_construct_registry:register_memory(MemoryId, Memory#{
                access_level => shared,
                tags => [<<"demo">>, <<"shared">>]
            }, AgentId)
        end, Memories),
        
        %% Register files
        Files = maps:get(files, AgentSpec, []),
        lists:foreach(fun(File) ->
            FileId = <<AgentId/binary, "_", (maps:get(id, File))/binary>>,
            model_construct_registry:register_file(FileId, File#{
                access_level => public,
                size => 1024000,
                tags => [<<"demo">>, <<"data">>]
            }, AgentId)
        end, Files),
        
        %% Register conversations
        Conversations = maps:get(conversations, AgentSpec, []),
        lists:foreach(fun(Conversation) ->
            ConversationId = <<AgentId/binary, "_", (maps:get(id, Conversation))/binary>>,
            model_construct_registry:register_conversation(ConversationId, Conversation#{
                access_level => shared,
                message_count => 10,
                last_activity => erlang:system_time(second)
            }, AgentId)
        end, Conversations)
        
    end, DemoAgents),
    
    io:format("✅ Demo environment setup complete with ~p specialized agents~n", [length(DemoAgents)]).

demonstrate_tool_discovery() ->
    io:format("  🔧 Testing cross-agent tool discovery...~n"),
    
    %% Scenario: Communication hub needs weather tools from weather specialist
    RequesterAgent = <<"communication_hub">>,
    
    %% 1. Direct tool discovery
    {ok, ToolResults} = agent_retrieval_system:retrieve_tools(
        RequesterAgent,
        #{query => <<"weather">>, limit => 5}
    ),
    
    io:format("  📊 Found ~p weather-related tools across the fleet~n", [length(ToolResults)]),
    lists:foreach(fun(Tool) ->
        io:format("    • ~s (from ~s): ~s~n", [
            maps:get(name, Tool),
            maps:get(owner_agent, Tool),
            maps:get(description, Tool)
        ])
    end, ToolResults),
    
    %% 2. Dynamic routing for tool execution
    ToolRequest = #{
        tool_name => <<"weather_forecast">>,
        parameters => #{location => <<"Washington DC">>, days => 7}
    },
    
    {ok, RoutingResult} = dynamic_agent_router:route_request(
        tool_execution,
        ToolRequest,
        RequesterAgent
    ),
    
    ExecutingAgent = maps:get(agent_id, RoutingResult),
    io:format("  🎯 Routed weather forecast request to: ~s~n", [ExecutingAgent]),
    io:format("  ⚡ Execution completed in ~p ms~n", [maps:get(execution_time_ms, RoutingResult, 0)]).

demonstrate_memory_sharing() ->
    io:format("  🧠 Testing cross-agent memory sharing...~n"),
    
    %% Scenario: Data analyst needs weather preferences from weather specialist
    RequesterAgent = <<"data_analyst">>,
    
    %% 1. Discover memories from other agents
    {ok, MemoryResults} = agent_retrieval_system:retrieve_memories(
        RequesterAgent,
        #{query => <<"preferences">>, tags => [<<"shared">>]}
    ),
    
    io:format("  📊 Found ~p shared memories across the fleet~n", [length(MemoryResults)]),
    lists:foreach(fun(Memory) ->
        io:format("    • ~s (from ~s): ~s~n", [
            maps:get(id, Memory),
            maps:get(owner_agent, Memory),
            truncate_text(maps:get(content, Memory, <<"">>), 40)
        ])
    end, MemoryResults),
    
    %% 2. Request specific memory through routing
    MemoryRequest = #{
        memory_id => <<"weather_specialist_weather_preferences">>,
        access_type => read
    },
    
    {ok, MemoryData} = dynamic_agent_router:route_request(
        memory_access,
        MemoryRequest,
        RequesterAgent
    ),
    
    io:format("  🎯 Successfully accessed memory through agent router~n"),
    io:format("  📝 Memory content: ~s~n", [truncate_text(maps:get(content, MemoryData, <<"">>), 60)]).

demonstrate_file_discovery() ->
    io:format("  📁 Testing distributed file resource discovery...~n"),
    
    %% Scenario: Weather specialist needs data files from data analyst
    RequesterAgent = <<"weather_specialist">>,
    
    %% 1. Fleet-wide file discovery
    {ok, FileResults} = dynamic_discovery_engine:discover_across_fleet(
        file,
        #{query => <<"weather">>, file_type => <<"json">>}
    ),
    
    io:format("  📊 Discovered ~p weather-related files across fleet~n", [length(FileResults)]),
    lists:foreach(fun(File) ->
        io:format("    • ~s (from ~s): ~s [~s]~n", [
            maps:get(name, File, maps:get(id, File)),
            maps:get(owner_agent, File),
            maps:get(path, File, <<"unknown">>),
            maps:get(content_type, File, <<"unknown">>)
        ])
    end, FileResults),
    
    %% 2. Request file access through routing
    FileRequest = #{
        file_id => <<"data_analyst_weather_dataset_2024">>,
        operation => read_metadata
    },
    
    {ok, FileMetadata} = dynamic_agent_router:route_request(
        file_access,
        FileRequest,
        RequesterAgent
    ),
    
    io:format("  🎯 Successfully accessed file metadata through routing~n"),
    io:format("  📊 File size: ~p bytes, Type: ~s~n", [
        maps:get(size, FileMetadata, 0),
        maps:get(content_type, FileMetadata, <<"unknown">>)
    ]).

demonstrate_conversation_discovery() ->
    io:format("  💬 Testing conversation routing and message discovery...~n"),
    
    %% Scenario: Knowledge keeper analyzing conversation patterns
    RequesterAgent = <<"knowledge_keeper">>,
    
    %% 1. Discover conversations across agents
    {ok, ConversationResults} = agent_retrieval_system:retrieve_conversations(
        RequesterAgent,
        #{topic => <<"weather">>, participant => <<"user">>}
    ),
    
    io:format("  📊 Found ~p weather-related conversations~n", [length(ConversationResults)]),
    lists:foreach(fun(Conversation) ->
        io:format("    • ~s (from ~s): ~p participants, ~p messages~n", [
            maps:get(topic, Conversation, <<"untitled">>),
            maps:get(owner_agent, Conversation),
            length(maps:get(participants, Conversation, [])),
            maps:get(message_count, Conversation, 0)
        ])
    end, ConversationResults),
    
    %% 2. Search for specific messages across conversations
    MessageRequest = #{
        message_query => #{
            content_match => <<"weather forecast">>,
            time_range => #{
                start => erlang:system_time(second) - 86400,
                'end' => erlang:system_time(second)
            }
        }
    },
    
    {ok, MessageData} = dynamic_agent_router:route_request(
        message_search,
        MessageRequest,
        RequesterAgent
    ),
    
    io:format("  🎯 Found ~p messages matching search criteria~n", [length(MessageData)]).

demonstrate_unified_retrieval() ->
    io:format("  🌐 Testing multi-source unified resource retrieval...~n"),
    
    %% Scenario: Communication hub needs comprehensive view of weather resources
    RequesterAgent = <<"communication_hub">>,
    
    %% Unified retrieval across all resource types
    {ok, UnifiedResults} = agent_retrieval_system:unified_retrieve(
        RequesterAgent,
        #{
            resource_types => [tool, memory, file, conversation],
            query => <<"weather">>,
            max_results_per_type => 3
        }
    ),
    
    %% Display results by type
    ResourceTypes = [tool, memory, file, conversation],
    lists:foreach(fun(Type) ->
        TypeKey = atom_to_binary(Type),
        TypeResults = maps:get(TypeKey, UnifiedResults, []),
        io:format("  📊 ~s resources: ~p found~n", [string:titlecase(atom_to_list(Type)), length(TypeResults)]),
        
        lists:foreach(fun(Resource) ->
            io:format("    • ~s (from ~s)~n", [
                maps:get(name, Resource, maps:get(id, Resource, <<"unknown">>)),
                maps:get(owner_agent, Resource, <<"unknown">>)
            ])
        end, lists:sublist(TypeResults, 2))
    end, ResourceTypes),
    
    TotalResources = lists:foldl(fun(Type, Acc) ->
        TypeKey = atom_to_binary(Type),
        Acc + length(maps:get(TypeKey, UnifiedResults, []))
    end, 0, ResourceTypes),
    
    io:format("  🎉 Total resources discovered: ~p across ~p agents~n", [TotalResources, 4]).

demonstrate_autonomous_coordination() ->
    io:format("  ⚡ Testing real-time autonomous agent coordination...~n"),
    
    %% Scenario: Autonomous weather analysis workflow
    CoordinatorAgent = <<"communication_hub">>,
    
    %% 1. Coordinator discovers required resources
    io:format("  🔍 Coordinator discovering resources for weather analysis workflow...~n"),
    
    {ok, WeatherTools} = agent_retrieval_system:retrieve_tools(
        CoordinatorAgent,
        #{query => <<"weather">>, limit => 2}
    ),
    
    {ok, DataFiles} = agent_retrieval_system:retrieve_files(
        CoordinatorAgent,
        #{query => <<"weather">>, limit => 2}
    ),
    
    {ok, AnalysisTools} = agent_retrieval_system:retrieve_tools(
        CoordinatorAgent,
        #{query => <<"analysis">>, limit => 2}
    ),
    
    io:format("  📊 Workflow resources: ~p weather tools, ~p data files, ~p analysis tools~n", [
        length(WeatherTools), length(DataFiles), length(AnalysisTools)
    ]),
    
    %% 2. Autonomous execution coordination
    WorkflowSteps = [
        #{step => 1, action => <<"fetch_weather_data">>, agent => <<"weather_specialist">>},
        #{step => 2, action => <<"analyze_weather_trends">>, agent => <<"data_analyst">>},
        #{step => 3, action => <<"store_insights">>, agent => <<"knowledge_keeper">>}
    ],
    
    io:format("  🎯 Executing autonomous workflow with ~p steps...~n", [length(WorkflowSteps)]),
    
    lists:foreach(fun(Step) ->
        StepNum = maps:get(step, Step),
        Action = maps:get(action, Step),
        TargetAgent = maps:get(agent, Step),
        
        %% Route action to appropriate agent
        ActionRequest = #{
            action => Action,
            workflow_id => <<"weather_analysis_demo">>,
            step => StepNum
        },
        
        {ok, _Result} = dynamic_agent_router:route_request(
            workflow_execution,
            ActionRequest,
            CoordinatorAgent
        ),
        
        io:format("    ✅ Step ~p completed: ~s on ~s~n", [StepNum, Action, TargetAgent]),
        timer:sleep(500) % Simulate processing time
    end, WorkflowSteps),
    
    io:format("  🎉 Autonomous workflow completed successfully!~n").

demonstrate_fleet_optimization() ->
    io:format("  🎯 Testing fleet-wide resource optimization...~n"),
    
    %% 1. Get current fleet statistics
    {ok, InitialStats} = dynamic_discovery_engine:get_fleet_resource_statistics(),
    
    io:format("  📊 Initial fleet statistics:~n"),
    io:format("    • Total resources: ~p~n", [maps:get(total_resources, InitialStats, 0)]),
    io:format("    • Active agents: ~p~n", [maps:get(active_agents, InitialStats, 0)]),
    io:format("    • Average resources per agent: ~.1f~n", [maps:get(avg_resources_per_agent, InitialStats, 0.0)]),
    
    %% 2. Trigger mesh optimization
    io:format("  ⚡ Optimizing discovery mesh topology...~n"),
    ok = dynamic_discovery_engine:optimize_mesh_topology(),
    
    timer:sleep(1000), % Allow optimization to complete
    
    %% 3. Get optimized statistics
    {ok, OptimizedStats} = dynamic_discovery_engine:get_mesh_statistics(),
    
    io:format("  📊 Optimized mesh statistics:~n"),
    io:format("    • Discovery efficiency: ~.1f%~n", [(maps:get(discovery_efficiency, OptimizedStats, 0.0) * 100)]),
    io:format("    • Average path length: ~.2f~n", [maps:get(average_path_length, OptimizedStats, 0.0)]),
    io:format("    • Cluster coefficient: ~.3f~n", [maps:get(cluster_coefficient, OptimizedStats, 0.0)]),
    
    %% 4. Test optimized discovery performance
    io:format("  🚀 Testing optimized discovery performance...~n"),
    
    StartTime = erlang:system_time(millisecond),
    
    {ok, _OptimizedResults} = agent_retrieval_system:unified_retrieve(
        <<"communication_hub">>,
        #{
            resource_types => [tool, memory, file, conversation],
            query => <<"weather">>,
            max_results_per_type => 5
        }
    ),
    
    EndTime = erlang:system_time(millisecond),
    DiscoveryTime = EndTime - StartTime,
    
    io:format("  ⚡ Optimized discovery completed in ~p ms~n", [DiscoveryTime]),
    io:format("  🎉 Fleet optimization demonstration complete!~n").

cleanup_demo_environment() ->
    %% Stop demo agent instances
    DemoAgentIds = [
        <<"weather_specialist">>,
        <<"data_analyst">>,
        <<"communication_hub">>,
        <<"knowledge_keeper">>
    ],
    
    lists:foreach(fun(AgentId) ->
        case get({agent_pid, AgentId}) of
            undefined -> ok;
            AgentPid when is_pid(AgentPid) ->
                case is_process_alive(AgentPid) of
                    true -> 
                        agent_instance:stop(AgentPid),
                        io:format("  ✅ Stopped agent: ~s~n", [AgentId]);
                    false -> ok
                end
        end
    end, DemoAgentIds),
    
    %% Clean up registries
    try
        model_construct_registry:clear_all_constructs(),
        dynamic_agent_router:clear_all_registrations(),
        io:format("  ✅ Cleared all registries and caches~n")
    catch _:_ -> 
        io:format("  ⚠️  Some cleanup operations failed (this is normal)~n")
    end.

%% Helper function to truncate text for display
truncate_text(Text, MaxLen) when byte_size(Text) > MaxLen ->
    <<Truncated:MaxLen/binary, _/binary>> = Text,
    <<Truncated/binary, "...">>;
truncate_text(Text, _MaxLen) ->
    Text.