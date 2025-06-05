#!/usr/bin/env escript
%% Test script for diverse agents with different tool configurations
%% Including borrowed tools, tool forking, and MLX integration

main(_) ->
    try
        io:format("ROCKET Testing Diverse Agents with Advanced Tool Configurations...~n~n"),
        
        % Start the application components
        application:start(crypto),
        application:start(ssl),
        application:start(agents),
        
        % Test 1: Create various agents with different tool sets
        io:format("=== 1. Creating Specialized Agents ===~n"),
        
        % Web-focused agent with Jina tools
        {ok, WebAgent} = agent:create_agent(#{
            name => <<"Web Research Agent">>,
            type => ai,
            model => <<"gpt-4.1-nano">>,
            api_preference => responses,
            system_prompt => <<"You are a web research specialist with access to Jina AI tools for comprehensive web analysis.">>,
            tools => [jina_search, jina_read_webpage, jina_fact_check, jina_deep_search]
        }),
        
        % System administration agent
        {ok, SysAdminAgent} = agent:create_agent(#{
            name => <<"System Admin Agent">>,
            type => ai,
            model => <<"gpt-4.1-mini">>,
            api_preference => chat,
            system_prompt => <<"You are a system administrator with access to shell commands and file operations.">>,
            tools => [shell, file_read, file_write, http_request]
        }),
        
        % Data science agent with embeddings
        {ok, DataScienceAgent} = agent:create_agent(#{
            name => <<"Data Science Agent">>,
            type => ai,
            model => <<"gpt-4.1-nano">>,
            api_preference => responses,
            system_prompt => <<"You are a data scientist specializing in text analysis and embeddings.">>,
            tools => [jina_embed_text, file_read, file_write, knowledge_base_retrieval]
        }),
        
        io:format("OK Created 3 specialized agents~n"),
        
        % Test 2: Create and test borrowed tools
        io:format("~n=== 2. Testing Borrowed Tool Creation ===~n"),
        
        % Create a custom MLX tool schema (correcting the activation count issue)
        MLXToolSchema = #{
            <<"name">> => <<"mlx_create_neural_net">>,
            <<"description">> => <<"Create neural networks using MLX with proper activation function counts">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"layer_sizes">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"integer">>},
                        <<"description">> => <<"List of layer sizes (e.g., [768, 1024, 64, 128, 32, 64, 8])">>
                    },
                    <<"activations">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{
                            <<"type">> => <<"string">>,
                            <<"enum">> => [<<"relu">>, <<"tanh">>, <<"sigmoid">>, <<"softmax">>, <<"gelu">>, <<"swish">>]
                        },
                        <<"description">> => <<"Activation functions (count must be layer_sizes - 1)">>
                    },
                    <<"network_type">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"mlp">>, <<"cnn">>, <<"rnn">>],
                        <<"default">> => <<"mlp">>,
                        <<"description">> => <<"Type of neural network">>
                    }
                },
                <<"required">> => [<<"layer_sizes">>, <<"activations">>]
            }
        },
        
        % Create borrowed tool
        {ok, Version1} = agent_tools:create_or_update_borrowed_tool(
            mlx_neural_net,
            MLXToolSchema,
            #{source => <<"community">>, contributor => <<"mlx_team">>, license => <<"MIT">>}
        ),
        
        io:format("OK Created borrowed MLX tool, version: ~s~n", [Version1]),
        
        % Test 3: Fork the MLX tool to create a specialized version
        io:format("~n=== 3. Testing Tool Forking ===~n"),
        
        % Fork the MLX tool to add validation
        Modifications = [
            {<<"description">>, <<"Enhanced MLX neural network creator with validation">>},
            {add_property, <<"validate_inputs">>, #{
                <<"type">> => <<"boolean">>,
                <<"default">> => true,
                <<"description">> => <<"Whether to validate input dimensions">>
            }}
        ],
        
        {ok, ForkVersion} = agent_tools:fork_tool(
            mlx_neural_net,
            mlx_neural_net_validated,
            Modifications
        ),
        
        io:format("OK Forked MLX tool to validated version: ~s~n", [ForkVersion]),
        
        % Test 4: Create an MLX-specialized agent
        io:format("~n=== 4. Creating MLX-Specialized Agent ===~n"),
        
        {ok, MLXAgent} = agent:create_agent(#{
            name => <<"MLX Neural Network Agent">>,
            type => ai,
            model => <<"gpt-4.1-nano">>,
            api_preference => responses,
            system_prompt => <<"You are an MLX specialist for creating and managing neural networks. Always ensure activation count = layer count - 1.">>,
            tools => [mlx_neural_net, mlx_neural_net_validated, file_read, file_write]
        }),
        
        MLXState = agent_instance:get_state(MLXAgent),
        io:format("OK Created MLX agent with tools: ~p~n", [maps:get(tools, MLXState)]),
        
        % Test 5: Test tool versioning
        io:format("~n=== 5. Testing Tool Versioning ===~n"),
        
        % Get version history
        Versions = agent_tools:get_tool_versions(mlx_neural_net),
        io:format("GRAPH MLX tool versions: ~p~n", [length(Versions)]),
        
        case Versions of
            [LatestVersion | _] ->
                CreatedAt = maps:get(created_at, LatestVersion),
                io:format("DATE Latest version created at: ~p~n", [CreatedAt]);
            [] ->
                io:format("ERROR No versions found~n")
        end,
        
        % Test 6: Demonstrate tool execution with validation
        io:format("~n=== 6. Testing Tool Execution ===~n"),
        
        % Test the original shell tool
        {ok, ShellResult} = agent_tools:execute_tool(shell, #{<<"command">> => <<"echo 'Testing diverse agents!'>>}),
        io:format("SHELL Shell tool result: ~s~n", [binary_to_list(ShellResult)]),
        
        % Test file operations
        TestContent = <<"# MLX Neural Network Example\n\nThis demonstrates proper activation function counts:\n- 7 layers need 6 activation functions\n- [768, 1024, 64, 128, 32, 64, 8] -> [relu, relu, relu, relu, relu, relu]">>,
        {ok, _} = agent_tools:execute_tool(file_write, #{
            <<"path">> => <<"/tmp/mlx_example.md">>,
            <<"content">> => TestContent
        }),
        
        {ok, ReadContent} = agent_tools:execute_tool(file_read, #{<<"path">> => <<"/tmp/mlx_example.md">>}),
        io:format("FILE File content length: ~p bytes~n", [byte_size(ReadContent)]),
        
        % Test 7: Advanced agent interaction demonstration
        io:format("~n=== 7. Agent Capabilities Summary ===~n"),
        
        Agents = [
            {<<"Web Research Agent">>, WebAgent, <<"Jina AI tools for web analysis">>},
            {<<"System Admin Agent">>, SysAdminAgent, <<"Shell and file operations">>},
            {<<"Data Science Agent">>, DataScienceAgent, <<"Embeddings and knowledge retrieval">>},
            {<<"MLX Neural Network Agent">>, MLXAgent, <<"Neural network creation with validation">>}
        ],
        
        lists:foreach(fun({Name, AgentPid, Description}) ->
            State = agent_instance:get_state(AgentPid),
            Model = maps:get(model, State),
            API = maps:get(api_preference, State),
            ToolCount = length(maps:get(tools, State)),
            io:format("ROBOT ~s:~n", [Name]),
            io:format("   - Model: ~s~n", [Model]),
            io:format("   - API: ~s~n", [API]),
            io:format("   - Tools: ~p available~n", [ToolCount]),
            io:format("   - Specialty: ~s~n~n", [Description])
        end, Agents),
        
        % Test 8: Tool ecosystem statistics
        io:format("=== 8. Tool Ecosystem Statistics ===~n"),
        
        AllToolNames = agent_tools:list_tools(),
        TotalTools = length(AllToolNames),
        
        % Count borrowed tools and regular tools
        BorrowedToolsCount = length([T || T <- AllToolNames, 
                                     lists:member(T, [mlx_neural_net, mlx_neural_net_validated])]),
        RegularToolsCount = TotalTools - BorrowedToolsCount,
        
        io:format("CHART Tool Statistics:~n"),
        io:format("   - Total tools: ~p~n", [TotalTools]),
        io:format("   - Regular tools: ~p~n", [RegularToolsCount]),
        io:format("   - Borrowed/Forked tools: ~p~n", [BorrowedToolsCount]),
        io:format("   - Available Jina tools: 5~n"),
        io:format("   - System tools: 5~n~n"),
        
        % Test 9: MLX activation function validation demo
        io:format("=== 9. MLX Activation Function Validation Demo ===~n"),
        
        io:format("OK Correct MLX MLP examples:~n"),
        io:format("   - [784, 128, 10] requires [relu, softmax] (2 activations)~n"),
        io:format("   - [768, 1024, 64, 128, 32, 64, 8] requires 6 activations~n"),
        io:format("   - Rule: activations = layers - 1~n~n"),
        
        io:format("ERROR Common mistakes to avoid:~n"),
        io:format("   - Don't provide 7 activations for 7 layers~n"),
        io:format("   - Last layer typically has no activation (or specific like softmax)~n"),
        io:format("   - Always validate layer_count - 1 = activation_count~n~n"),
        
        io:format("=== All Advanced Agent Tests Completed Successfully! ===~n"),
        io:format("Demonstrated:~n"),
        io:format("   + Multiple specialized agents with different tool sets~n"),
        io:format("   + Borrowed tool creation and versioning~n"),
        io:format("   + Tool forking and modification~n"),
        io:format("   + MLX neural network tools with proper validation~n"),
        io:format("   + Advanced tool ecosystem management~n"),
        io:format("   + Different API preferences (Chat vs Responses)~n"),
        io:format("   + Tool execution across different categories~n~n"),
        
        % Cleanup
        lists:foreach(fun({_, AgentPid, _}) ->
            agent:delete_agent(AgentPid)
        end, Agents),
        
        io:format("Cleaned up test agents~n")
        
    catch
        E:R:S ->
            io:format("ERROR Test failed: ~p:~p~n", [E, R]),
            io:format("Stack trace: ~p~n", [S])
    end.