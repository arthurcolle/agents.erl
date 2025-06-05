#!/usr/bin/env escript
%% -*- erlang -*-
%%% @doc Comprehensive integration test for dynamic routing and discovery system
%%% Tests cross-agent resource retrieval for Tools, Memory, Files, Conversations

-module(test_dynamic_routing_integration).

main(_) ->
    io:format("Starting Dynamic Routing and Discovery Integration Tests~n"),
    
    %% Start the OTP application
    application:ensure_all_started(agents),
    application:ensure_all_started(openai),
    
    %% Wait for systems to initialize
    timer:sleep(2000),
    
    %% Initialize test environment
    setup_test_environment(),
    
    %% Run comprehensive test suite
    TestResults = [
        test_tool_discovery_and_routing(),
        test_memory_sharing_across_agents(),
        test_file_resource_discovery(),
        test_conversation_routing(),
        test_message_cross_agent_retrieval(),
        test_multi_source_retrieval(),
        test_discovery_mesh_optimization(),
        test_fleet_wide_resource_discovery()
    ],
    
    %% Report results
    report_test_results(TestResults),
    
    %% Cleanup
    cleanup_test_environment(),
    
    io:format("Dynamic Routing Integration Tests Complete!~n").

setup_test_environment() ->
    io:format("Setting up test environment...~n"),
    
    %% Start core discovery systems
    {ok, _RouterPid} = dynamic_agent_router:start_link(),
    {ok, _RegistryPid} = model_construct_registry:start_link(),
    {ok, _DiscoveryPid} = dynamic_discovery_engine:start_link(),
    {ok, _RetrievalPid} = agent_retrieval_system:start_link(),
    
    %% Create test agent fleet
    TestAgents = [
        #{id => <<"agent_tools_specialist">>, type => <<"tool_specialist">>, 
          capabilities => [<<"weather_tools">>, <<"calculation_tools">>, <<"web_tools">>]},
        #{id => <<"agent_memory_keeper">>, type => <<"memory_specialist">>, 
          capabilities => [<<"long_term_memory">>, <<"conversation_history">>, <<"knowledge_base">>]},
        #{id => <<"agent_file_manager">>, type => <<"file_specialist">>, 
          capabilities => [<<"file_operations">>, <<"document_processing">>, <<"data_analysis">>]},
        #{id => <<"agent_coordinator">>, type => <<"coordination_specialist">>, 
          capabilities => [<<"fleet_management">>, <<"task_routing">>, <<"cross_agent_communication">>]}
    ],
    
    %% Register test agents
    lists:foreach(fun(AgentSpec) ->
        AgentId = maps:get(id, AgentSpec),
        Capabilities = maps:get(capabilities, AgentSpec),
        
        %% Start agent instance
        {ok, AgentPid} = agent_instance:start_link(AgentId, #{}),
        
        %% Register capabilities
        lists:foreach(fun(Capability) ->
            dynamic_agent_router:register_agent_capability(AgentId, Capability)
        end, Capabilities),
        
        %% Store agent reference
        put({agent_pid, AgentId}, AgentPid)
    end, TestAgents),
    
    %% Populate test resources
    populate_test_resources(),
    
    io:format("Test environment setup complete~n").

populate_test_resources() ->
    %% Register test tools
    model_construct_registry:register_tool(
        <<"weather_forecast">>,
        #{
            name => <<"weather_forecast">>,
            description => <<"Get weather forecast for a location">>,
            schema => #{
                type => object,
                properties => #{
                    location => #{type => string, description => <<"Location name">>}
                },
                required => [location]
            },
            implementation => <<"weather_service:get_forecast/1">>,
            access_level => public
        },
        <<"agent_tools_specialist">>
    ),
    
    %% Register test memories
    model_construct_registry:register_memory(
        <<"user_preferences">>,
        #{
            content => #{
                preferred_temperature_unit => <<"celsius">>,
                preferred_language => <<"english">>,
                timezone => <<"UTC">>
            },
            tags => [<<"preferences">>, <<"user_data">>],
            access_level => shared
        },
        <<"agent_memory_keeper">>
    ),
    
    %% Register test files
    model_construct_registry:register_file(
        <<"weather_data_2024">>,
        #{
            path => <<"/data/weather/2024_climate_data.json">>,
            size => 1048576,
            content_type => <<"application/json">>,
            access_level => public,
            tags => [<<"weather">>, <<"climate">>, <<"2024">>]
        },
        <<"agent_file_manager">>
    ),
    
    %% Register test conversations
    model_construct_registry:register_conversation(
        <<"weather_discussion_thread">>,
        #{
            participants => [<<"user_123">>, <<"agent_tools_specialist">>],
            message_count => 15,
            topic => <<"weather_forecasting">>,
            last_activity => erlang:system_time(second),
            access_level => shared
        },
        <<"agent_coordinator">>
    ).

test_tool_discovery_and_routing() ->
    io:format("Testing tool discovery and routing...~n"),
    
    try
        %% Test 1: Direct tool discovery
        {ok, ToolResults} = agent_retrieval_system:retrieve_tools(
            <<"agent_coordinator">>, 
            #{query => <<"weather">>, limit => 5}
        ),
        
        true = length(ToolResults) > 0,
        
        %% Test 2: Cross-agent tool execution routing
        ToolRequest = #{
            tool_name => <<"weather_forecast">>,
            parameters => #{location => <<"Washington DC">>}
        },
        
        {ok, RoutingResult} = dynamic_agent_router:route_request(
            tool_execution, 
            ToolRequest, 
            <<"agent_coordinator">>
        ),
        
        true = maps:is_key(agent_id, RoutingResult),
        true = maps:is_key(execution_result, RoutingResult),
        
        io:format("✓ Tool discovery and routing successful~n"),
        {tool_discovery_routing, success}
        
    catch
        Error:Reason:Stack ->
            io:format("✗ Tool discovery and routing failed: ~p:~p~n", [Error, Reason]),
            {tool_discovery_routing, {failure, Error, Reason}}
    end.

test_memory_sharing_across_agents() ->
    io:format("Testing memory sharing across agents...~n"),
    
    try
        %% Test memory retrieval from different agent
        {ok, MemoryResults} = agent_retrieval_system:retrieve_memories(
            <<"agent_tools_specialist">>,
            #{query => <<"preferences">>, tags => [<<"user_data">>]}
        ),
        
        true = length(MemoryResults) > 0,
        
        %% Test memory routing request
        MemoryRequest = #{
            memory_id => <<"user_preferences">>,
            access_type => read
        },
        
        {ok, MemoryData} = dynamic_agent_router:route_request(
            memory_access,
            MemoryRequest,
            <<"agent_tools_specialist">>
        ),
        
        true = maps:is_key(content, MemoryData),
        
        io:format("✓ Memory sharing across agents successful~n"),
        {memory_sharing, success}
        
    catch
        Error:Reason:Stack ->
            io:format("✗ Memory sharing failed: ~p:~p~n", [Error, Reason]),
            {memory_sharing, {failure, Error, Reason}}
    end.

test_file_resource_discovery() ->
    io:format("Testing file resource discovery...~n"),
    
    try
        %% Test file discovery across fleet
        {ok, FileResults} = dynamic_discovery_engine:discover_across_fleet(
            file,
            #{query => <<"weather">>, file_type => <<"json">>}
        ),
        
        true = length(FileResults) > 0,
        
        %% Test file access routing
        FileRequest = #{
            file_id => <<"weather_data_2024">>,
            operation => read_metadata
        },
        
        {ok, FileMetadata} = dynamic_agent_router:route_request(
            file_access,
            FileRequest,
            <<"agent_coordinator">>
        ),
        
        true = maps:is_key(path, FileMetadata),
        true = maps:is_key(size, FileMetadata),
        
        io:format("✓ File resource discovery successful~n"),
        {file_discovery, success}
        
    catch
        Error:Reason:Stack ->
            io:format("✗ File resource discovery failed: ~p:~p~n", [Error, Reason]),
            {file_discovery, {failure, Error, Reason}}
    end.

test_conversation_routing() ->
    io:format("Testing conversation routing...~n"),
    
    try
        %% Test conversation discovery
        {ok, ConversationResults} = agent_retrieval_system:retrieve_conversations(
            <<"agent_memory_keeper">>,
            #{topic => <<"weather">>, participant => <<"user_123">>}
        ),
        
        true = length(ConversationResults) > 0,
        
        %% Test conversation access routing
        ConversationRequest = #{
            conversation_id => <<"weather_discussion_thread">>,
            operation => get_messages,
            limit => 10
        },
        
        {ok, ConversationData} = dynamic_agent_router:route_request(
            conversation_access,
            ConversationRequest,
            <<"agent_memory_keeper">>
        ),
        
        true = maps:is_key(messages, ConversationData),
        
        io:format("✓ Conversation routing successful~n"),
        {conversation_routing, success}
        
    catch
        Error:Reason:Stack ->
            io:format("✗ Conversation routing failed: ~p:~p~n", [Error, Reason]),
            {conversation_routing, {failure, Error, Reason}}
    end.

test_message_cross_agent_retrieval() ->
    io:format("Testing message cross-agent retrieval...~n"),
    
    try
        %% Test message retrieval across agents
        {ok, MessageResults} = agent_retrieval_system:retrieve_messages(
            <<"agent_file_manager">>,
            #{
                query => <<"weather forecast">>,
                conversation_id => <<"weather_discussion_thread">>,
                limit => 5
            }
        ),
        
        %% Test message routing
        MessageRequest = #{
            message_query => #{
                content_match => <<"temperature">>,
                from_agent => <<"agent_tools_specialist">>,
                time_range => #{
                    start => erlang:system_time(second) - 86400,
                    'end' => erlang:system_time(second)
                }
            }
        },
        
        {ok, MessageData} = dynamic_agent_router:route_request(
            message_search,
            MessageRequest,
            <<"agent_file_manager">>
        ),
        
        true = is_list(MessageData),
        
        io:format("✓ Message cross-agent retrieval successful~n"),
        {message_retrieval, success}
        
    catch
        Error:Reason:Stack ->
            io:format("✗ Message cross-agent retrieval failed: ~p:~p~n", [Error, Reason]),
            {message_retrieval, {failure, Error, Reason}}
    end.

test_multi_source_retrieval() ->
    io:format("Testing multi-source retrieval...~n"),
    
    try
        %% Test unified multi-source tool retrieval
        {ok, UnifiedResults} = agent_retrieval_system:unified_retrieve(
            <<"agent_coordinator">>,
            #{
                resource_types => [tool, memory, file],
                query => <<"weather">>,
                max_results_per_type => 3
            }
        ),
        
        true = maps:is_key(tools, UnifiedResults),
        true = maps:is_key(memories, UnifiedResults),
        true = maps:is_key(files, UnifiedResults),
        
        %% Verify each type has results
        ToolCount = length(maps:get(tools, UnifiedResults, [])),
        MemoryCount = length(maps:get(memories, UnifiedResults, [])),
        FileCount = length(maps:get(files, UnifiedResults, [])),
        
        true = (ToolCount + MemoryCount + FileCount) > 0,
        
        io:format("✓ Multi-source retrieval successful (Tools: ~p, Memories: ~p, Files: ~p)~n", 
                 [ToolCount, MemoryCount, FileCount]),
        {multi_source_retrieval, success}
        
    catch
        Error:Reason:Stack ->
            io:format("✗ Multi-source retrieval failed: ~p:~p~n", [Error, Reason]),
            {multi_source_retrieval, {failure, Error, Reason}}
    end.

test_discovery_mesh_optimization() ->
    io:format("Testing discovery mesh optimization...~n"),
    
    try
        %% Test mesh topology optimization
        {ok, MeshStats} = dynamic_discovery_engine:get_mesh_statistics(),
        
        true = maps:is_key(node_count, MeshStats),
        true = maps:is_key(connection_count, MeshStats),
        true = maps:is_key(average_path_length, MeshStats),
        
        %% Test optimized discovery path
        {ok, OptimizedPath} = dynamic_discovery_engine:find_optimal_discovery_path(
            <<"agent_coordinator">>,
            <<"agent_tools_specialist">>,
            tool
        ),
        
        true = is_list(OptimizedPath),
        true = length(OptimizedPath) > 0,
        
        io:format("✓ Discovery mesh optimization successful~n"),
        {mesh_optimization, success}
        
    catch
        Error:Reason:Stack ->
            io:format("✗ Discovery mesh optimization failed: ~p:~p~n", [Error, Reason]),
            {mesh_optimization, {failure, Error, Reason}}
    end.

test_fleet_wide_resource_discovery() ->
    io:format("Testing fleet-wide resource discovery...~n"),
    
    try
        %% Test comprehensive fleet discovery
        {ok, FleetResources} = dynamic_discovery_engine:discover_all_fleet_resources(
            #{
                include_types => [tool, memory, file, conversation],
                include_capabilities => true,
                include_topology => true
            }
        ),
        
        true = maps:is_key(resources_by_type, FleetResources),
        true = maps:is_key(agent_capabilities, FleetResources),
        true = maps:is_key(mesh_topology, FleetResources),
        
        %% Test fleet resource statistics
        {ok, FleetStats} = dynamic_discovery_engine:get_fleet_resource_statistics(),
        
        true = maps:is_key(total_resources, FleetStats),
        true = maps:is_key(resources_per_agent, FleetStats),
        true = maps:is_key(capability_distribution, FleetStats),
        
        io:format("✓ Fleet-wide resource discovery successful~n"),
        {fleet_discovery, success}
        
    catch
        Error:Reason:Stack ->
            io:format("✗ Fleet-wide resource discovery failed: ~p:~p~n", [Error, Reason]),
            {fleet_discovery, {failure, Error, Reason}}
    end.

report_test_results(TestResults) ->
    io:format("~n=== DYNAMIC ROUTING INTEGRATION TEST RESULTS ===~n"),
    
    SuccessCount = length([Result || {_, Result} <- TestResults, Result =:= success]),
    TotalCount = length(TestResults),
    
    io:format("Tests Passed: ~p/~p~n", [SuccessCount, TotalCount]),
    
    lists:foreach(fun({TestName, Result}) ->
        Status = case Result of
            success -> "✓ PASS";
            {failure, _, _} -> "✗ FAIL"
        end,
        io:format("  ~s: ~s~n", [TestName, Status])
    end, TestResults),
    
    case SuccessCount of
        TotalCount ->
            io:format("~n🎉 ALL TESTS PASSED! Dynamic routing and discovery system is fully functional.~n");
        _ ->
            io:format("~n⚠️  Some tests failed. Please review the implementation.~n")
    end.

cleanup_test_environment() ->
    io:format("Cleaning up test environment...~n"),
    
    %% Stop test agent instances
    TestAgentIds = [
        <<"agent_tools_specialist">>, 
        <<"agent_memory_keeper">>, 
        <<"agent_file_manager">>, 
        <<"agent_coordinator">>
    ],
    
    lists:foreach(fun(AgentId) ->
        case get({agent_pid, AgentId}) of
            undefined -> ok;
            AgentPid when is_pid(AgentPid) ->
                case is_process_alive(AgentPid) of
                    true -> agent_instance:stop(AgentPid);
                    false -> ok
                end
        end
    end, TestAgentIds),
    
    %% Clean up registries
    try
        model_construct_registry:clear_all_constructs()
    catch _:_ -> ok end,
    
    try
        dynamic_agent_router:clear_all_registrations()
    catch _:_ -> ok end,
    
    io:format("Test environment cleanup complete~n").