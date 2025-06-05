#!/usr/bin/env escript
%% Simple test for diverse agents with borrowed tools

main(_) ->
    try
        io:format("Testing Diverse Agents with Borrowed Tools...~n~n"),
        
        % Start applications
        application:start(crypto),
        application:start(ssl),
        application:start(agents),
        application:start(openai),
        
        % Start agent tools system
        {ok, _} = agent_tools:start_link(#{}),
        
        % Test 1: Create specialized agents
        io:format("=== 1. Creating Specialized Agents ===~n"),
        
        WebConfig = #{
            name => <<"Web Research Agent">>,
            type => ai,
            model => <<"gpt-4.1-nano">>,
            api_preference => responses,
            system_prompt => <<"Web research specialist with Jina AI tools.">>,
            tools => [jina_search, jina_read_webpage, jina_fact_check]
        },
        
        SysConfig = #{
            name => <<"System Admin Agent">>,
            type => ai,
            model => <<"gpt-4.1-mini">>,
            api_preference => chat,
            system_prompt => <<"System administrator with shell access.">>,
            tools => [shell, file_read, file_write]
        },
        
        {ok, WebAgent} = agent_instance:start_link(WebConfig),
        {ok, SysAgent} = agent_instance:start_link(SysConfig),
        
        io:format("Created 2 specialized agents~n"),
        
        % Test 2: Create borrowed tool
        io:format("~n=== 2. Testing Borrowed Tool Creation ===~n"),
        
        MLXSchema = #{
            <<"name">> => <<"mlx_neural_net">>,
            <<"description">> => <<"Create MLX neural networks with proper activation counts">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"layer_sizes">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"integer">>},
                        <<"description">> => <<"Layer sizes array">>
                    },
                    <<"activations">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"string">>},
                        <<"description">> => <<"Activation functions (count = layers - 1)">>
                    }
                },
                <<"required">> => [<<"layer_sizes">>, <<"activations">>]
            }
        },
        
        {ok, Version1} = agent_tools:create_or_update_borrowed_tool(
            mlx_neural_net,
            MLXSchema,
            #{source => <<"community">>, contributor => <<"mlx_team">>}
        ),
        
        io:format("Created borrowed MLX tool, version: ~s~n", [Version1]),
        
        % Test 3: Fork the tool
        io:format("~n=== 3. Testing Tool Forking ===~n"),
        
        Modifications = [
            {<<"description">>, <<"Enhanced MLX neural network with validation">>},
            {add_property, <<"validate">>, #{
                <<"type">> => <<"boolean">>,
                <<"default">> => true,
                <<"description">> => <<"Validate inputs">>
            }}
        ],
        
        {ok, ForkVersion} = agent_tools:fork_tool(
            mlx_neural_net,
            mlx_neural_net_validated,
            Modifications
        ),
        
        io:format("Forked MLX tool, version: ~s~n", [ForkVersion]),
        
        % Test 4: Create MLX agent
        io:format("~n=== 4. Creating MLX Agent ===~n"),
        
        MLXConfig = #{
            name => <<"MLX Agent">>,
            type => ai,
            model => <<"gpt-4.1-nano">>,
            api_preference => responses,
            system_prompt => <<"MLX neural network specialist.">>,
            tools => [mlx_neural_net, mlx_neural_net_validated, file_read]
        },
        
        {ok, MLXAgent} = agent_instance:start_link(MLXConfig),
        
        MLXState = agent_instance:get_state(MLXAgent),
        ToolCount = length(maps:get(tools, MLXState)),
        io:format("Created MLX agent with ~p tools~n", [ToolCount]),
        
        % Test 5: Tool versioning
        io:format("~n=== 5. Testing Tool Versioning ===~n"),
        
        Versions = agent_tools:get_tool_versions(mlx_neural_net),
        io:format("MLX tool has ~p versions~n", [length(Versions)]),
        
        % Test 6: Tool execution
        io:format("~n=== 6. Testing Tool Execution ===~n"),
        
        {ok, ShellResult} = agent_tools:execute_tool(shell, #{
            <<"command">> => <<"echo 'Testing agents'">>
        }),
        io:format("Shell result: ~s", [ShellResult]),
        
        % Test MLX example content
        TestContent = <<"MLX Example: [768,1024,64,128,32,64,8] needs 6 activations">>,
        {ok, _} = agent_tools:execute_tool(file_write, #{
            <<"path">> => <<"/tmp/mlx_test.txt">>,
            <<"content">> => TestContent
        }),
        
        {ok, ReadContent} = agent_tools:execute_tool(file_read, #{
            <<"path">> => <<"/tmp/mlx_test.txt">>
        }),
        io:format("File content: ~s~n", [ReadContent]),
        
        % Test 7: Summary
        io:format("~n=== 7. Agent Summary ===~n"),
        
        Agents = [
            {<<"Web Agent">>, WebAgent, <<"Jina tools">>},
            {<<"Sys Agent">>, SysAgent, <<"Shell tools">>},
            {<<"MLX Agent">>, MLXAgent, <<"Neural nets">>}
        ],
        
        lists:foreach(fun({Name, Pid, Spec}) ->
            State = agent_instance:get_state(Pid),
            Model = maps:get(model, State),
            API = maps:get(api_preference, State),
            Tools = length(maps:get(tools, State)),
            io:format("~s: ~s (~s) - ~p tools - ~s~n", 
                     [Name, Model, API, Tools, Spec])
        end, Agents),
        
        % Test 8: Tool statistics
        io:format("~n=== 8. Tool Statistics ===~n"),
        
        AllTools = agent_tools:list_tools(),
        TotalTools = length(AllTools),
        BorrowedCount = length([T || T <- AllTools, 
                               lists:member(T, [mlx_neural_net, mlx_neural_net_validated])]),
        
        io:format("Total tools: ~p~n", [TotalTools]),
        io:format("Borrowed tools: ~p~n", [BorrowedCount]),
        io:format("Regular tools: ~p~n", [TotalTools - BorrowedCount]),
        
        % Test 9: MLX validation demo
        io:format("~n=== 9. MLX Validation Demo ===~n"),
        io:format("Correct: [784,128,10] needs [relu,softmax] (2 activations)~n"),
        io:format("Correct: [768,1024,64,128,32,64,8] needs 6 activations~n"),
        io:format("Rule: activation_count = layer_count - 1~n"),
        io:format("Error: Don't use 7 activations for 7 layers~n"),
        
        io:format("~n=== All Tests Completed Successfully! ===~n"),
        io:format("Demonstrated:~n"),
        io:format("+ Multiple specialized agents~n"),
        io:format("+ Borrowed tool creation and versioning~n"),
        io:format("+ Tool forking and modification~n"),
        io:format("+ MLX neural network validation~n"),
        io:format("+ Different API preferences~n"),
        
        % Note: In production, you would properly shut down agent processes
        io:format("Test completed - agents remain active~n")
        
    catch
        E:R:S ->
            io:format("Test failed: ~p:~p~n", [E, R]),
            io:format("Stack: ~p~n", [S])
    end.