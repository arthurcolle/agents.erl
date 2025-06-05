#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

%%%-------------------------------------------------------------------
%%% @doc Advanced OpenAPI Scaffold Demonstration
%%% Shows all features of the advanced scaffolding system
%%% @end
%%%-------------------------------------------------------------------

-module(advanced_scaffold_demo).
-mode(compile).

main(_Args) ->
    io:format("~n=== OpenAPI Advanced Scaffold Demo ===~n~n"),
    
    %% Start required applications
    application:ensure_all_started(hackney),
    application:ensure_all_started(jsx),
    
    %% Start the scaffold application
    {ok, _} = application:ensure_all_started(openapi_scaffold),
    
    %% Demo all features
    demo_code_generation(),
    demo_ai_transformations(),
    demo_smart_caching(),
    demo_security_features(),
    demo_graphql_bridge(),
    demo_playground(),
    demo_metrics_monitoring(),
    demo_time_travel(),
    demo_test_generation(),
    
    io:format("~n=== Demo Complete! ===~n"),
    io:format("~nPlayground available at: http://localhost:8090/~n"),
    io:format("GraphQL endpoint at: http://localhost:8090/api/graphql~n"),
    io:format("Metrics at: http://localhost:8090/api/metrics~n~n"),
    
    %% Keep running
    receive
        stop -> ok
    end.

%%====================================================================
%% Demo Functions
%%====================================================================

demo_code_generation() ->
    io:format("~n--- Multi-Language Code Generation ---~n"),
    
    %% Sample OpenAPI spec
    Spec = #{
        <<"openapi">> => <<"3.0.0">>,
        <<"info">> => #{
            <<"title">> => <<"Pet Store API">>,
            <<"version">> => <<"1.0.0">>
        },
        <<"servers">> => [
            #{<<"url">> => <<"https://api.petstore.com/v1">>}
        ],
        <<"paths">> => #{
            <<"/pets">> => #{
                <<"get">> => #{
                    <<"operationId">> => <<"listPets">>,
                    <<"summary">> => <<"List all pets">>,
                    <<"parameters">> => [
                        #{
                            <<"name">> => <<"limit">>,
                            <<"in">> => <<"query">>,
                            <<"schema">> => #{<<"type">> => <<"integer">>}
                        }
                    ],
                    <<"responses">> => #{
                        <<"200">> => #{
                            <<"description">> => <<"A list of pets">>,
                            <<"content">> => #{
                                <<"application/json">> => #{
                                    <<"schema">> => #{
                                        <<"type">> => <<"array">>,
                                        <<"items">> => #{
                                            <<"$ref">> => <<"#/components/schemas/Pet">>
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                <<"post">> => #{
                    <<"operationId">> => <<"createPet">>,
                    <<"summary">> => <<"Create a pet">>,
                    <<"requestBody">> => #{
                        <<"required">> => true,
                        <<"content">> => #{
                            <<"application/json">> => #{
                                <<"schema">> => #{
                                    <<"$ref">> => <<"#/components/schemas/Pet">>
                                }
                            }
                        }
                    },
                    <<"responses">> => #{
                        <<"201">> => #{
                            <<"description">> => <<"Pet created">>
                        }
                    }
                }
            }
        },
        <<"components">> => #{
            <<"schemas">> => #{
                <<"Pet">> => #{
                    <<"type">> => <<"object">>,
                    <<"required">> => [<<"name">>],
                    <<"properties">> => #{
                        <<"id">> => #{<<"type">> => <<"integer">>},
                        <<"name">> => #{<<"type">> => <<"string">>},
                        <<"tag">> => #{<<"type">> => <<"string">>}
                    }
                }
            }
        }
    },
    
    %% Generate TypeScript client
    {ok, TSCode} = openapi_advanced_codegen:generate_typescript_client(Spec, #{
        class_name => "PetStoreClient"
    }),
    io:format("TypeScript client generated (~p bytes)~n", [byte_size(TSCode)]),
    
    %% Generate Python client
    {ok, PyCode} = openapi_advanced_codegen:generate_python_client(Spec, #{}),
    io:format("Python client generated (~p bytes)~n", [byte_size(PyCode)]),
    
    %% Generate Erlang client
    {ok, ErlCode} = openapi_advanced_codegen:generate_erlang_client(Spec, #{}),
    io:format("Erlang client generated (~p bytes)~n", [byte_size(ErlCode)]),
    
    %% Generate GraphQL schema
    {ok, GraphQLSchema} = openapi_advanced_codegen:generate_graphql_schema(Spec),
    io:format("GraphQL schema generated (~p bytes)~n", [byte_size(GraphQLSchema)]),
    
    %% Generate property tests
    {ok, ProperTests} = openapi_advanced_codegen:generate_property_tests(Spec, #{
        module_name => "pet_store_tests"
    }),
    io:format("PropEr tests generated (~p bytes)~n", [byte_size(ProperTests)]).

demo_ai_transformations() ->
    io:format("~n--- AI-Powered Transformations ---~n"),
    
    %% Sample malformed request
    MalformedRequest = #{
        <<"user_name">> => <<"john">>,  % Should be 'username'
        <<"emial">> => <<"john@example">>,  % Typo and missing .com
        <<"age">> => <<"25">>  % Should be integer
    },
    
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"username">> => #{<<"type">> => <<"string">>},
            <<"email">> => #{<<"type">> => <<"string">>, <<"format">> => <<"email">>},
            <<"age">> => #{<<"type">> => <<"integer">>}
        },
        <<"required">> => [<<"username">>, <<"email">>]
    },
    
    %% Auto-fix request
    case openapi_ai_transform:auto_fix_request(MalformedRequest, Schema) of
        {ok, FixedRequest} ->
            io:format("AI fixed request: ~p~n", [FixedRequest]);
        {error, Reason} ->
            io:format("AI fix failed: ~p~n", [Reason])
    end,
    
    %% Semantic validation
    TestData = #{
        <<"startDate">> => <<"2024-01-01">>,
        <<"endDate">> => <<"2023-12-01">>  % Invalid: end before start
    },
    
    case openapi_ai_transform:semantic_validation(TestData, Schema) of
        {ok, valid} ->
            io:format("Semantic validation passed~n");
        {error, {validation_errors, Errors}} ->
            io:format("Semantic validation errors: ~p~n", [Errors])
    end.

demo_smart_caching() ->
    io:format("~n--- Smart Caching System ---~n"),
    
    %% Configure cache
    openapi_smart_cache:configure(#{
        max_size => 50 * 1024 * 1024,  % 50MB
        max_entries => 5000,
        enable_compression => true,
        eviction_policy => lru
    }),
    
    %% Cache some data with TTL from headers
    Headers1 = #{
        <<"cache-control">> => <<"max-age=300">>,  % 5 minutes
        <<"content-type">> => <<"application/json">>
    },
    
    TestData = #{<<"users">> => lists:map(fun(I) ->
        #{<<"id">> => I, <<"name">> => <<"User", (integer_to_binary(I))/binary>>}
    end, lists:seq(1, 100))},
    
    Key1 = <<"api:users:page1">>,
    openapi_smart_cache:put(Key1, TestData, Headers1, #{}),
    io:format("Cached data with 5-minute TTL~n"),
    
    %% Test cache hit
    case openapi_smart_cache:get(Key1, #{}) of
        {ok, CachedData} ->
            io:format("Cache HIT - Retrieved ~p bytes~n", [byte_size(term_to_binary(CachedData))]);
        miss ->
            io:format("Cache MISS~n")
    end,
    
    %% Test deduplication
    spawn_link(fun() ->
        io:format("Request 1 starting...~n"),
        openapi_smart_cache:get(<<"slow-key">>, #{}),
        io:format("Request 1 complete~n")
    end),
    
    timer:sleep(10),
    
    spawn_link(fun() ->
        io:format("Request 2 starting (should be deduplicated)...~n"),
        openapi_smart_cache:get(<<"slow-key">>, #{}),
        io:format("Request 2 complete~n")
    end),
    
    %% Get cache stats
    {ok, Stats} = openapi_smart_cache:get_stats(),
    io:format("Cache stats: ~p~n", [Stats]).

demo_security_features() ->
    io:format("~n--- Security Features ---~n"),
    
    %% Test API key authentication
    Request1 = #{
        headers => #{
            <<"x-api-key">> => <<"test-api-key-123">>
        }
    },
    
    case openapi_security:authenticate_request(Request1, #{}) of
        {ok, Identity} ->
            io:format("Authentication successful: ~p~n", [Identity]);
        {error, Reason} ->
            io:format("Authentication failed: ~p~n", [Reason])
    end,
    
    %% Test rate limiting
    ClientId = <<"client-123">>,
    Endpoint = <<"/api/users">>,
    
    lists:foreach(fun(I) ->
        case openapi_security:rate_limit_check(ClientId, Endpoint) of
            {ok, Info} ->
                io:format("Request ~p allowed. Remaining: ~p~n", 
                         [I, maps:get(remaining, Info)]);
            {error, #{limit_exceeded := true} = Info} ->
                io:format("Request ~p RATE LIMITED. Retry after: ~p seconds~n",
                         [I, maps:get(retry_after, Info)])
        end,
        timer:sleep(100)
    end, lists:seq(1, 10)),
    
    %% Test security scanning
    MaliciousRequest = #{
        path => <<"/api/users">>,
        query_params => #{
            <<"search">> => <<"' OR '1'='1">>  % SQL injection attempt
        },
        headers => #{
            <<"user-input">> => <<"<script>alert('XSS')</script>">>  % XSS attempt
        }
    },
    
    case openapi_security:scan_request(MaliciousRequest, #{}) of
        {ok, safe} ->
            io:format("Request passed security scan~n");
        {error, {security_threats, Threats}} ->
            io:format("Security threats detected: ~p~n", [Threats])
    end.

demo_graphql_bridge() ->
    io:format("~n--- GraphQL Bridge ---~n"),
    
    %% Register OpenAPI spec as GraphQL API
    Spec = load_sample_spec(),
    ok = openapi_graphql_bridge:register_api(<<"petstore">>, Spec),
    io:format("OpenAPI spec registered as GraphQL API~n"),
    
    %% Execute GraphQL query
    Query = <<"
        query {
            listPets(limit: 10) {
                id
                name
                tag
            }
        }
    ">>,
    
    case openapi_graphql_bridge:execute_query(<<"petstore">>, Query, #{}) of
        {ok, Result} ->
            io:format("GraphQL query result: ~p~n", [Result]);
        {error, Error} ->
            io:format("GraphQL query error: ~p~n", [Error])
    end,
    
    %% Execute GraphQL mutation
    Mutation = <<"
        mutation CreatePet($input: PetInput!) {
            createPet(input: $input) {
                id
                name
            }
        }
    ">>,
    
    Variables = #{
        <<"input">> => #{
            <<"name">> => <<"Fluffy">>,
            <<"tag">> => <<"cat">>
        }
    },
    
    case openapi_graphql_bridge:execute_mutation(<<"petstore">>, Mutation, Variables) of
        {ok, Result} ->
            io:format("GraphQL mutation result: ~p~n", [Result]);
        {error, Error} ->
            io:format("GraphQL mutation error: ~p~n", [Error])
    end,
    
    %% Introspect schema
    {ok, Schema} = openapi_graphql_bridge:introspect(<<"petstore">>),
    io:format("GraphQL schema has ~p types~n", 
             [length(maps:get(<<"types">>, maps:get(<<"__schema">>, Schema, #{}), []))]).

demo_playground() ->
    io:format("~n--- Interactive Playground ---~n"),
    
    %% Register spec for playground
    Spec = load_sample_spec(),
    ok = openapi_playground:register_spec(<<"petstore">>, Spec),
    io:format("Spec registered in playground~n"),
    
    %% Generate example request
    {ok, Example} = openapi_playground:generate_example(<<"petstore">>, <<"listPets">>),
    io:format("Generated example: ~p~n", [Example]),
    
    %% Validate request
    TestRequest = #{
        <<"path">> => <<"/pets">>,
        <<"method">> => <<"GET">>,
        <<"queryParams">> => #{
            <<"limit">> => <<"not-a-number">>  % Invalid
        }
    },
    
    case openapi_playground:validate_request(<<"petstore">>, TestRequest) of
        {ok, valid} ->
            io:format("Request validation passed~n");
        {error, Errors} ->
            io:format("Request validation errors: ~p~n", [Errors])
    end,
    
    io:format("Playground UI available at: http://localhost:8090/~n").

demo_metrics_monitoring() ->
    io:format("~n--- Metrics & Monitoring ---~n"),
    
    %% Record some API activity
    lists:foreach(fun(I) ->
        Endpoint = case I rem 3 of
            0 -> <<"/api/users">>;
            1 -> <<"/api/products">>;
            2 -> <<"/api/orders">>
        end,
        
        Method = case I rem 2 of
            0 -> <<"GET">>;
            1 -> <<"POST">>
        end,
        
        %% Record request
        openapi_metrics:record_request(Endpoint, Method, #{}, <<>>),
        
        %% Simulate processing
        Latency = 10 + rand:uniform(200),
        timer:sleep(Latency),
        
        %% Record response
        Status = case rand:uniform(10) of
            1 -> 500;  % 10% error rate
            2 -> 404;
            _ -> 200
        end,
        
        openapi_metrics:record_response(Endpoint, Status, Latency, 
                                       rand:uniform(10000), #{})
    end, lists:seq(1, 50)),
    
    %% Get endpoint metrics
    {ok, Metrics} = openapi_metrics:get_endpoint_metrics(<<"/api/users">>),
    io:format("Endpoint metrics: ~p~n", [Metrics]),
    
    %% Check for anomalies
    {ok, Anomalies} = openapi_metrics:get_anomalies(),
    io:format("Detected anomalies: ~p~n", [Anomalies]),
    
    %% Get SLA report
    {ok, SLAReport} = openapi_metrics:get_sla_report(300), % Last 5 minutes
    io:format("SLA Report: ~p~n", [SLAReport]).

demo_time_travel() ->
    io:format("~n--- Time Travel Debugging ---~n"),
    
    SessionId = <<"debug-session-1">>,
    
    %% Enable recording
    ok = openapi_time_travel:enable_recording(SessionId),
    io:format("Recording enabled for session~n"),
    
    %% Record some requests
    lists:foreach(fun(I) ->
        RequestData = #{
            method => <<"GET">>,
            path => <<"/api/users/", (integer_to_binary(I))/binary>>,
            headers => #{<<"authorization">> => <<"Bearer token123">>},
            status => 200,
            response_body => #{<<"id">> => I, <<"name">> => <<"User ", (integer_to_binary(I))/binary>>},
            duration => rand:uniform(100)
        },
        
        openapi_time_travel:record_request(SessionId, RequestData),
        timer:sleep(100)
    end, lists:seq(1, 5)),
    
    %% Get timeline
    {ok, Timeline} = openapi_time_travel:get_timeline(SessionId),
    io:format("Recorded ~p events~n", [length(Timeline)]),
    
    %% Travel to specific point
    case Timeline of
        [_, _, Event3 | _] ->
            Timestamp = maps:get(timestamp, Event3),
            {ok, State} = openapi_time_travel:travel_to(SessionId, Timestamp),
            io:format("Time traveled to: ~p~n", [maps:get(timestamp, State)]);
        _ ->
            ok
    end,
    
    %% Export session
    {ok, Export} = openapi_time_travel:export_session(SessionId, json),
    io:format("Session exported (~p bytes)~n", [byte_size(Export)]),
    
    %% Replay session
    {ok, ReplayInfo} = openapi_time_travel:replay_session(SessionId, #{
        speed => 2.0,  % 2x speed
        start_time => 0,
        end_time => erlang:system_time(microsecond)
    }),
    io:format("Replaying ~p recordings...~n", [maps:get(recording_count, ReplayInfo)]).

demo_test_generation() ->
    io:format("~n--- Test Generation ---~n"),
    
    Spec = load_sample_spec(),
    
    %% Generate property-based tests
    {ok, ProperTests} = openapi_test_generator:generate_proper_tests(Spec, #{
        module_name => "petstore_proper_tests"
    }),
    io:format("Generated PropEr tests (~p bytes)~n", [byte_size(ProperTests)]),
    
    %% Generate EUnit tests
    {ok, EUnitTests} = openapi_test_generator:generate_eunit_tests(Spec, #{
        module_name => "petstore_eunit_tests"
    }),
    io:format("Generated EUnit tests (~p bytes)~n", [byte_size(EUnitTests)]),
    
    %% Generate load tests
    {ok, LoadTests} = openapi_test_generator:generate_load_tests(Spec, #{
        module_name => "petstore_load_tests"
    }),
    io:format("Generated load tests (~p bytes)~n", [byte_size(LoadTests)]),
    
    %% Generate security tests
    {ok, SecurityTests} = openapi_test_generator:generate_security_tests(Spec, #{
        module_name => "petstore_security_tests"
    }),
    io:format("Generated security tests (~p bytes)~n", [byte_size(SecurityTests)]),
    
    %% Save generated tests
    file:write_file("petstore_tests.erl", ProperTests),
    io:format("Tests saved to petstore_tests.erl~n").

%%====================================================================
%% Helper Functions
%%====================================================================

load_sample_spec() ->
    #{
        <<"openapi">> => <<"3.0.0">>,
        <<"info">> => #{
            <<"title">> => <<"Pet Store API">>,
            <<"version">> => <<"1.0.0">>,
            <<"description">> => <<"A sample Pet Store Server based on OpenAPI 3.0">>
        },
        <<"servers">> => [
            #{<<"url">> => <<"https://api.petstore.com/v1">>}
        ],
        <<"paths">> => #{
            <<"/pets">> => #{
                <<"get">> => #{
                    <<"operationId">> => <<"listPets">>,
                    <<"summary">> => <<"List all pets">>,
                    <<"tags">> => [<<"pets">>],
                    <<"parameters">> => [
                        #{
                            <<"name">> => <<"limit">>,
                            <<"in">> => <<"query">>,
                            <<"description">> => <<"How many items to return">>,
                            <<"required">> => false,
                            <<"schema">> => #{
                                <<"type">> => <<"integer">>,
                                <<"format">> => <<"int32">>,
                                <<"minimum">> => 1,
                                <<"maximum">> => 100
                            }
                        }
                    ],
                    <<"responses">> => #{
                        <<"200">> => #{
                            <<"description">> => <<"A paged array of pets">>,
                            <<"content">> => #{
                                <<"application/json">> => #{
                                    <<"schema">> => #{
                                        <<"type">> => <<"array">>,
                                        <<"items">> => #{
                                            <<"$ref">> => <<"#/components/schemas/Pet">>
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                <<"post">> => #{
                    <<"operationId">> => <<"createPet">>,
                    <<"summary">> => <<"Create a pet">>,
                    <<"tags">> => [<<"pets">>],
                    <<"requestBody">> => #{
                        <<"required">> => true,
                        <<"content">> => #{
                            <<"application/json">> => #{
                                <<"schema">> => #{
                                    <<"$ref">> => <<"#/components/schemas/Pet">>
                                }
                            }
                        }
                    },
                    <<"responses">> => #{
                        <<"201">> => #{
                            <<"description">> => <<"Pet created">>
                        }
                    }
                }
            },
            <<"/pets/{petId}">> => #{
                <<"get">> => #{
                    <<"operationId">> => <<"getPetById">>,
                    <<"summary">> => <<"Info for a specific pet">>,
                    <<"tags">> => [<<"pets">>],
                    <<"parameters">> => [
                        #{
                            <<"name">> => <<"petId">>,
                            <<"in">> => <<"path">>,
                            <<"required">> => true,
                            <<"description">> => <<"The id of the pet to retrieve">>,
                            <<"schema">> => #{
                                <<"type">> => <<"string">>
                            }
                        }
                    ],
                    <<"responses">> => #{
                        <<"200">> => #{
                            <<"description">> => <<"Expected response to a valid request">>,
                            <<"content">> => #{
                                <<"application/json">> => #{
                                    <<"schema">> => #{
                                        <<"$ref">> => <<"#/components/schemas/Pet">>
                                    }
                                }
                            }
                        }
                    }
                }
            }
        },
        <<"components">> => #{
            <<"schemas">> => #{
                <<"Pet">> => #{
                    <<"type">> => <<"object">>,
                    <<"required">> => [<<"id">>, <<"name">>],
                    <<"properties">> => #{
                        <<"id">> => #{
                            <<"type">> => <<"integer">>,
                            <<"format">> => <<"int64">>
                        },
                        <<"name">> => #{
                            <<"type">> => <<"string">>
                        },
                        <<"tag">> => #{
                            <<"type">> => <<"string">>
                        }
                    }
                },
                <<"Error">> => #{
                    <<"type">> => <<"object">>,
                    <<"required">> => [<<"code">>, <<"message">>],
                    <<"properties">> => #{
                        <<"code">> => #{
                            <<"type">> => <<"integer">>,
                            <<"format">> => <<"int32">>
                        },
                        <<"message">> => #{
                            <<"type">> => <<"string">>
                        }
                    }
                }
            }
        },
        <<"tags">> => [
            #{
                <<"name">> => <<"pets">>,
                <<"description">> => <<"Everything about your Pets">>
            }
        ]
    }.