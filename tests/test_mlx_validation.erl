#!/usr/bin/env escript
%% Test MLX activation function validation and demonstrate tool concepts

main(_) ->
    try
        io:format("Testing MLX Neural Network Validation and Tool Concepts...~n~n"),
        
        % Test 1: MLX Activation Function Validation Demo
        io:format("=== 1. MLX Activation Function Validation ===~n"),
        
        % Demonstrate correct examples
        io:format("CORRECT Examples:~n"),
        
        Example1 = #{
            layers => [784, 128, 10],
            activations => [relu, softmax],
            description => "Simple MNIST classifier"
        },
        
        Example2 = #{
            layers => [768, 1024, 64, 128, 32, 64, 8],
            activations => [relu, relu, relu, relu, relu, relu],
            description => "Deep MLP for embeddings"
        },
        
        Example3 = #{
            layers => [512, 256, 128, 64, 32, 1],
            activations => [relu, relu, relu, relu, sigmoid],
            description => "Regression network"
        },
        
        Examples = [Example1, Example2, Example3],
        
        lists:foreach(fun(#{layers := Layers, activations := Activations, description := Desc}) ->
            LayerCount = length(Layers),
            ActivationCount = length(Activations),
            ExpectedActivations = LayerCount - 1,
            Status = case ActivationCount =:= ExpectedActivations of
                true -> "VALID";
                false -> "INVALID"
            end,
            io:format("  ~s: ~s~n", [Status, Desc]),
            io:format("    Layers: ~p (count: ~p)~n", [Layers, LayerCount]),
            io:format("    Activations: ~p (count: ~p)~n", [Activations, ActivationCount]),
            io:format("    Expected activations: ~p~n~n", [ExpectedActivations])
        end, Examples),
        
        % Test 2: Common Mistakes Demo
        io:format("=== 2. Common MLX Mistakes to Avoid ===~n"),
        
        BadExample1 = #{
            layers => [768, 1024, 64, 128, 32, 64, 8],
            activations => [relu, relu, relu, relu, relu, relu, relu], % TOO MANY!
            error => "Too many activations - should be 6, not 7"
        },
        
        BadExample2 = #{
            layers => [784, 128, 10],
            activations => [relu], % TOO FEW!
            error => "Too few activations - should be 2, not 1"
        },
        
        BadExamples = [BadExample1, BadExample2],
        
        lists:foreach(fun(#{layers := Layers, activations := Activations, error := Error}) ->
            LayerCount = length(Layers),
            ActivationCount = length(Activations),
            ExpectedActivations = LayerCount - 1,
            io:format("  ERROR: ~s~n", [Error]),
            io:format("    Layers: ~p (count: ~p)~n", [Layers, LayerCount]),
            io:format("    Activations: ~p (count: ~p)~n", [Activations, ActivationCount]),
            io:format("    Should be: ~p activations~n~n", [ExpectedActivations])
        end, BadExamples),
        
        % Test 3: Tool Schema Design for MLX
        io:format("=== 3. MLX Tool Schema Design ===~n"),
        
        MLXToolSchema = #{
            <<"name">> => <<"mlx_create_neural_net">>,
            <<"description">> => <<"Create neural networks using MLX with proper activation validation">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"layer_sizes">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"integer">>, <<"minimum">> => 1},
                        <<"description">> => <<"Array of layer sizes (e.g., [768, 1024, 64, 128, 32, 64, 8])">>,
                        <<"minItems">> => 2
                    },
                    <<"activations">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{
                            <<"type">> => <<"string">>,
                            <<"enum">> => [<<"relu">>, <<"tanh">>, <<"sigmoid">>, <<"softmax">>, <<"gelu">>, <<"swish">>, <<"leaky_relu">>]
                        },
                        <<"description">> => <<"Activation functions (MUST be exactly layer_count - 1)">>
                    },
                    <<"validate_inputs">> => #{
                        <<"type">> => <<"boolean">>,
                        <<"default">> => true,
                        <<"description">> => <<"Whether to validate activation count matches layer count - 1">>
                    },
                    <<"network_type">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"mlp">>, <<"cnn">>, <<"rnn">>],
                        <<"default">> => <<"mlp">>,
                        <<"description">> => <<"Type of neural network architecture">>
                    }
                },
                <<"required">> => [<<"layer_sizes">>, <<"activations">>]
            }
        },
        
        io:format("MLX Tool Schema:~n"),
        io:format("  Name: ~s~n", [maps:get(<<"name">>, MLXToolSchema)]),
        io:format("  Description: ~s~n", [maps:get(<<"description">>, MLXToolSchema)]),
        io:format("  Required params: layer_sizes, activations~n"),
        io:format("  Validation: activation_count = layer_count - 1~n~n"),
        
        % Test 4: Borrowed Tool Concept Demo
        io:format("=== 4. Borrowed Tool Versioning Concept ===~n"),
        
        % Simulate tool versions
        BorrowedToolVersions = [
            #{
                version => "v1.0.0",
                created_at => "2024-01-15",
                contributor => "mlx_team",
                changes => "Initial MLX tool implementation",
                validation => false
            },
            #{
                version => "v1.1.0", 
                created_at => "2024-02-01",
                contributor => "community_dev",
                changes => "Added activation function validation",
                validation => true
            },
            #{
                version => "v1.2.0",
                created_at => "2024-02-15", 
                contributor => "neural_net_expert",
                changes => "Added support for CNN and RNN architectures",
                validation => true
            }
        ],
        
        io:format("Tool Evolution History:~n"),
        lists:foreach(fun(#{version := V, created_at := Date, contributor := Contrib, changes := Changes, validation := Valid}) ->
            ValidStr = case Valid of true -> "YES"; false -> "NO" end,
            io:format("  ~s (~s) by ~s~n", [V, Date, Contrib]),
            io:format("    Changes: ~s~n", [Changes]),
            io:format("    Validation: ~s~n~n", [ValidStr])
        end, BorrowedToolVersions),
        
        % Test 5: Tool Forking Concept
        io:format("=== 5. Tool Forking Concept Demo ===~n"),
        
        BaseTool = <<"mlx_neural_net">>,
        ForkedTool = <<"mlx_neural_net_validated">>,
        
        Modifications = [
            "Added input validation for layer/activation count mismatch",
            "Added support for custom activation functions",
            "Added network architecture recommendations",
            "Added performance optimization hints"
        ],
        
        io:format("Base Tool: ~s~n", [BaseTool]),
        io:format("Forked Tool: ~s~n", [ForkedTool]),
        io:format("Modifications:~n"),
        lists:foreach(fun(Mod) ->
            io:format("  + ~s~n", [Mod])
        end, Modifications),
        
        % Test 6: Agent Specialization Concepts
        io:format("~n=== 6. Agent Specialization Concepts ===~n"),
        
        AgentTypes = [
            #{
                name => "Web Research Agent",
                model => "gpt-4.1-nano",
                api => "responses",
                tools => ["jina_search", "jina_read_webpage", "jina_fact_check", "jina_deep_search"],
                specialty => "Web analysis and fact-checking"
            },
            #{
                name => "System Admin Agent", 
                model => "gpt-4.1-mini",
                api => "chat",
                tools => ["shell", "file_read", "file_write", "http_request"],
                specialty => "System operations and file management"
            },
            #{
                name => "MLX Neural Network Agent",
                model => "gpt-4.1-nano", 
                api => "responses",
                tools => ["mlx_neural_net", "mlx_neural_net_validated", "file_read", "file_write"],
                specialty => "Neural network creation with validation"
            },
            #{
                name => "Data Science Agent",
                model => "gpt-4.1-nano",
                api => "responses", 
                tools => ["jina_embed_text", "file_read", "file_write", "knowledge_base_retrieval"],
                specialty => "Text analysis and embeddings"
            }
        ],
        
        lists:foreach(fun(#{name := Name, model := Model, api := API, tools := Tools, specialty := Spec}) ->
            ToolCount = length(Tools),
            io:format("~s:~n", [Name]),
            io:format("  Model: ~s~n", [Model]),
            io:format("  API: ~s~n", [API]),
            io:format("  Tools: ~p (~p available)~n", [ToolCount, Tools]),
            io:format("  Specialty: ~s~n~n", [Spec])
        end, AgentTypes),
        
        % Test 7: Summary and Best Practices
        io:format("=== 7. MLX Best Practices Summary ===~n"),
        
        BestPractices = [
            "Always validate: activation_count = layer_count - 1",
            "Use appropriate activations for each layer type",
            "Consider softmax for classification output layers",
            "Use relu/gelu for hidden layers in most cases",
            "Validate input dimensions before network creation",
            "Version your tool schemas for reproducibility",
            "Fork tools to add domain-specific enhancements",
            "Specialize agents for specific task domains"
        ],
        
        lists:foreach(fun(Practice) ->
            io:format("  + ~s~n", [Practice])
        end, BestPractices),
        
        io:format("~n=== Test Completed Successfully! ===~n"),
        io:format("Demonstrated:~n"),
        io:format("  + MLX activation function validation~n"),
        io:format("  + Common mistakes and how to avoid them~n"),
        io:format("  + Tool schema design with validation~n"),
        io:format("  + Borrowed tool versioning concepts~n"),
        io:format("  + Tool forking and modification concepts~n"),
        io:format("  + Agent specialization strategies~n"),
        io:format("  + Best practices for neural network tools~n")
        
    catch
        E:R:S ->
            io:format("Test failed: ~p:~p~n", [E, R]),
            io:format("Stack: ~p~n", [S])
    end.