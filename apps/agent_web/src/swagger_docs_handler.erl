-module(swagger_docs_handler).
-export([init/2]).

-include_lib("kernel/include/file.hrl").

init(Req0 = #{method := <<"GET">>}, State) ->
    Path = cowboy_req:path(Req0),
    case Path of
        <<"/docs">> ->
            serve_swagger_ui(Req0, State);
        <<"/docs/swagger.json">> ->
            serve_swagger_spec(Req0, State);
        _ ->
            cowboy_req:reply(404, #{}, <<"Not Found">>, Req0),
            {ok, Req0, State}
    end;

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{error => <<"Method not allowed">>}), Req0),
    {ok, Req, State}.

serve_swagger_ui(Req0, State) ->
    Html = <<"<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <title>Agents.erl API Documentation</title>
    <link rel=\"stylesheet\" type=\"text/css\" href=\"https://unpkg.com/swagger-ui-dist@4.15.5/swagger-ui.css\" />
    <style>
        html { box-sizing: border-box; overflow: -moz-scrollbars-vertical; overflow-y: scroll; }
        *, *:before, *:after { box-sizing: inherit; }
        body { margin:0; background: #fafafa; }
    </style>
</head>
<body>
    <div id=\"swagger-ui\"></div>
    <script src=\"https://unpkg.com/swagger-ui-dist@4.15.5/swagger-ui-bundle.js\"></script>
    <script src=\"https://unpkg.com/swagger-ui-dist@4.15.5/swagger-ui-standalone-preset.js\"></script>
    <script>
        window.onload = function() {
            const ui = SwaggerUIBundle({
                url: '/docs/swagger.json',
                dom_id: '#swagger-ui',
                deepLinking: true,
                presets: [
                    SwaggerUIBundle.presets.apis,
                    SwaggerUIStandalonePreset
                ],
                plugins: [
                    SwaggerUIBundle.plugins.DownloadUrl
                ],
                layout: \"StandaloneLayout\"
            });
        };
    </script>
</body>
</html>">>,
    
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html">>
    }, Html, Req0),
    {ok, Req, State}.

serve_swagger_spec(Req0, State) ->
    SwaggerSpec = generate_swagger_spec(),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(SwaggerSpec), Req0),
    {ok, Req, State}.

generate_swagger_spec() ->
    #{
        <<"openapi">> => <<"3.0.3">>,
        <<"info">> => #{
            <<"title">> => <<"Agents.erl API">>,
            <<"description">> => <<"Distributed Multi-Agent System with OpenAI/Anthropic Integration, Quantum-Inspired Coordination, and Modern Web Interface">>,
            <<"version">> => <<"1.0.0">>,
            <<"contact">> => #{
                <<"name">> => <<"Agents.erl">>,
                <<"url">> => <<"https://github.com/agents-erl/agents.erl">>
            }
        },
        <<"servers">> => [
            #{
                <<"url">> => <<"http://localhost:8080">>,
                <<"description">> => <<"Development server">>
            }
        ],
        <<"tags">> => [
            #{<<"name">> => <<"agents">>, <<"description">> => <<"Agent management operations">>},
            #{<<"name">> => <<"mcp">>, <<"description">> => <<"Model Context Protocol management">>},
            #{<<"name">> => <<"system">>, <<"description">> => <<"System health and metrics">>},
            #{<<"name">> => <<"workflows">>, <<"description">> => <<"Workflow orchestration">>},
            #{<<"name">> => <<"auth">>, <<"description">> => <<"Authentication and API keys">>},
            #{<<"name">> => <<"monitoring">>, <<"description">> => <<"Error tracking and monitoring">>},
            #{<<"name">> => <<"fleet">>, <<"description">> => <<"Fleet management operations">>},
            #{<<"name">> => <<"costs">>, <<"description">> => <<"Cost tracking and analytics">>},
            #{<<"name">> => <<"bulk">>, <<"description">> => <<"Bulk operations">>},
            #{<<"name">> => <<"conversations">>, <<"description">> => <<"Conversation management">>},
            #{<<"name">> => <<"timeline">>, <<"description">> => <<"Timeline events">>},
            #{<<"name">> => <<"models">>, <<"description">> => <<"Model management">>},
            #{<<"name">> => <<"pipedream">>, <<"description">> => <<"Pipedream integration">>},
            #{<<"name">> => <<"logs">>, <<"description">> => <<"Log management">>},
            #{<<"name">> => <<"stats">>, <<"description">> => <<"Statistics and metrics">>},
            #{<<"name">> => <<"examples">>, <<"description">> => <<"Code examples">>},
            #{<<"name">> => <<"knowledge">>, <<"description">> => <<"Knowledge base management">>},
            #{<<"name">> => <<"templates">>, <<"description">> => <<"Agent templates">>},
            #{<<"name">> => <<"upload">>, <<"description">> => <<"File upload services">>},
            #{<<"name">> => <<"claude">>, <<"description">> => <<"Claude CLI integration">>},
            #{<<"name">> => <<"discovery">>, <<"description">> => <<"Service discovery">>},
            #{<<"name">> => <<"websockets">>, <<"description">> => <<"WebSocket connections">>},
            #{<<"name">> => <<"api">>, <<"description">> => <<"General API operations">>}
        ],
        <<"paths">> => generate_paths(),
        <<"components">> => generate_components()
    }.

generate_paths() ->
    %% Get routes from the running cowboy router
    Routes = get_cowboy_routes(),
    %% Add debug info about route count
    colored_logger:info("[SWAGGER] Generating docs for ~p routes", [length(Routes)]),
    Paths = generate_paths_from_routes(Routes),
    colored_logger:info("[SWAGGER] Generated ~p API endpoints", [maps:size(Paths)]),
    Paths.

%% Get all routes from Cowboy router
get_cowboy_routes() ->
    try
        %% Get the ranch listener info for agent_web_http_listener
        case ranch:get_protocol_options(agent_web_http_listener) of
            #{env := #{dispatch := Dispatch}} ->
                %% Extract routes from the dispatch table
                extract_routes_from_dispatch(Dispatch);
            _ ->
                %% Fallback to default routes if we can't get them dynamically
                get_default_routes()
        end
    catch
        _:_ ->
            %% If anything fails, use default routes
            get_default_routes()
    end.

%% Extract routes from Cowboy dispatch table
extract_routes_from_dispatch([{_Host, _HostMatch, PathList}|_]) ->
    extract_paths_from_pathlist(PathList);
extract_routes_from_dispatch(_) ->
    get_default_routes().

%% Extract paths from Cowboy path list
extract_paths_from_pathlist(PathList) ->
    lists:foldl(fun(PathInfo, Acc) ->
        case PathInfo of
            {PathMatch, [], Handler, _Opts} ->
                Path = cowboy_path_to_openapi_path(PathMatch),
                [{Path, Handler}|Acc];
            _ ->
                Acc
        end
    end, [], PathList).

%% Convert Cowboy path patterns to OpenAPI paths
cowboy_path_to_openapi_path(PathMatch) when is_list(PathMatch) ->
    %% Handle path segments
    PathStr = lists:foldl(fun
        (Segment, Acc) when is_binary(Segment) ->
            Acc ++ "/" ++ binary_to_list(Segment);
        (Segment, Acc) when is_atom(Segment) ->
            Acc ++ "/{" ++ atom_to_list(Segment) ++ "}";
        ('...', Acc) ->
            Acc ++ "/*";
        (_, Acc) ->
            Acc
    end, "", PathMatch),
    list_to_binary(PathStr);
cowboy_path_to_openapi_path([]) ->
    <<"/">>;
cowboy_path_to_openapi_path(_) ->
    <<"/">>.

%% Generate OpenAPI paths from extracted routes
generate_paths_from_routes(Routes) ->
    lists:foldl(fun({Path, Handler}, PathsAcc) ->
        case should_include_in_api_docs(Path, Handler) of
            true ->
                Methods = get_supported_methods_for_handler(Handler),
                PathSpec = generate_path_spec_for_handler(Path, Handler, Methods),
                maps:put(Path, PathSpec, PathsAcc);
            false ->
                PathsAcc
        end
    end, #{}, Routes).

%% Check if path should be included in API documentation
should_include_in_api_docs(Path, _Handler) ->
    %% Include all /api/ paths and exclude static content
    case Path of
        <<"/api/", _/binary>> -> true;
        <<"/ws/", _/binary>> -> true;  % Include WebSocket endpoints
        _ -> false
    end.

%% Get supported HTTP methods for a handler
get_supported_methods_for_handler(Handler) ->
    %% Default to GET and POST for most handlers
    %% This could be enhanced by introspecting the handler module
    case Handler of
        agent_ws_handler -> [<<"GET">>];  % WebSocket upgrade
        swagger_docs_handler -> [<<"GET">>];
        cowboy_static -> [<<"GET">>];
        _ -> [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>]  % Assume REST endpoints
    end.

%% Generate OpenAPI path specification for a handler
generate_path_spec_for_handler(Path, Handler, Methods) ->
    lists:foldl(fun(Method, MethodsAcc) ->
        MethodLower = string:lowercase(binary_to_list(Method)),
        MethodAtom = list_to_atom(MethodLower),
        MethodSpec = generate_method_spec(Path, Handler, MethodAtom),
        maps:put(list_to_binary(MethodLower), MethodSpec, MethodsAcc)
    end, #{}, Methods).

%% Generate method specification based on path and handler
generate_method_spec(Path, Handler, Method) ->
    {Tag, Summary, Description} = get_endpoint_info(Path, Handler, Method),
    
    BaseSpec = #{
        <<"tags">> => [Tag],
        <<"summary">> => Summary,
        <<"description">> => Description,
        <<"responses">> => #{
            <<"200">> => #{
                <<"description">> => <<"Success">>,
                <<"content">> => #{
                    <<"application/json">> => #{
                        <<"schema">> => #{<<"type">> => <<"object">>}
                    }
                }
            },
            <<"400">> => #{<<"description">> => <<"Bad Request">>},
            <<"500">> => #{<<"description">> => <<"Internal Server Error">>}
        }
    },
    
    %% Add parameters for path variables
    Parameters = extract_path_parameters(Path),
    SpecWithParams = case Parameters of
        [] -> BaseSpec;
        _ -> maps:put(<<"parameters">>, Parameters, BaseSpec)
    end,
    
    %% Add request body for POST/PUT methods
    case Method of
        post -> add_request_body(SpecWithParams);
        put -> add_request_body(SpecWithParams);
        _ -> SpecWithParams
    end.

%% Extract path parameters from OpenAPI path
extract_path_parameters(Path) ->
    PathStr = binary_to_list(Path),
    case re:run(PathStr, "\\{([^}]+)\\}", [global, {capture, all_but_first, list}]) of
        {match, Matches} ->
            lists:map(fun([ParamName]) ->
                #{
                    <<"name">> => list_to_binary(ParamName),
                    <<"in">> => <<"path">>,
                    <<"required">> => true,
                    <<"schema">> => #{<<"type">> => <<"string">>}
                }
            end, Matches);
        _ ->
            []
    end.

%% Add request body specification
add_request_body(Spec) ->
    RequestBody = #{
        <<"required">> => true,
        <<"content">> => #{
            <<"application/json">> => #{
                <<"schema">> => #{<<"type">> => <<"object">>}
            }
        }
    },
    maps:put(<<"requestBody">>, RequestBody, Spec).

%% Get endpoint information (tag, summary, description) based on path and handler
get_endpoint_info(Path, Handler, Method) ->
    PathStr = binary_to_list(Path),
    MethodStr = atom_to_list(Method),
    
    {Tag, Category} = case PathStr of
        "/api/agents" ++ _ -> {<<"agents">>, "Agent management"};
        "/api/mcp" ++ _ -> {<<"mcp">>, "Model Context Protocol"};
        "/api/system" ++ _ -> {<<"system">>, "System operations"};
        "/api/fleet" ++ _ -> {<<"fleet">>, "Fleet management"};
        "/api/costs" ++ _ -> {<<"costs">>, "Cost tracking"};
        "/api/keys" ++ _ -> {<<"auth">>, "Authentication"};
        "/api/oauth" ++ _ -> {<<"auth">>, "OAuth authentication"};
        "/api/crashes" ++ _ -> {<<"monitoring">>, "Error monitoring"};
        "/api/errors" ++ _ -> {<<"monitoring">>, "Error tracking"};
        "/api/interpretations" ++ _ -> {<<"monitoring">>, "Error interpretation"};
        "/api/monitoring" ++ _ -> {<<"monitoring">>, "System monitoring"};
        "/api/workflow" ++ _ -> {<<"workflows">>, "Workflow orchestration"};
        "/api/bulk" ++ _ -> {<<"bulk">>, "Bulk operations"};
        "/api/conversations" ++ _ -> {<<"conversations">>, "Conversation management"};
        "/api/timeline" ++ _ -> {<<"timeline">>, "Timeline events"};
        "/api/models" ++ _ -> {<<"models">>, "Model management"};
        "/api/pipedream" ++ _ -> {<<"pipedream">>, "Pipedream integration"};
        "/api/mcp-registry" ++ _ -> {<<"mcp">>, "MCP Registry"};
        "/api/reload" ++ _ -> {<<"system">>, "Hot code reloading"};
        "/api/logs" ++ _ -> {<<"logs">>, "Log management"};
        "/api/stats" ++ _ -> {<<"stats">>, "Statistics"};
        "/api/examples" ++ _ -> {<<"examples">>, "Examples"};
        "/api/knowledge" ++ _ -> {<<"knowledge">>, "Knowledge base"};
        "/api/templates" ++ _ -> {<<"templates">>, "Agent templates"};
        "/api/upload" ++ _ -> {<<"upload">>, "File upload"};
        "/api/claude-cli" ++ _ -> {<<"claude">>, "Claude CLI"};
        "/api/discovery" ++ _ -> {<<"discovery">>, "Service discovery"};
        "/ws" ++ _ -> {<<"websockets">>, "WebSocket connections"};
        _ -> {<<"api">>, "API operations"}
    end,
    
    Summary = list_to_binary(title_case(MethodStr) ++ " " ++ PathStr),
    Description = list_to_binary(Category ++ " - " ++ PathStr),
    
    {Tag, Summary, Description}.

%% Helper function to capitalize first letter
title_case([]) -> [];
title_case([H|T]) when H >= $a, H =< $z -> [H - 32|T];
title_case(Str) -> Str.

%% Fallback to manually defined routes if dynamic extraction fails
get_default_routes() ->
    [
        {<<"/api/agents">>, agent_api_handler},
        {<<"/api/agents/{id}">>, agent_api_handler},
        {<<"/api/mcp/servers">>, mcp_api_handler},
        {<<"/api/system/health">>, system_health_handler}
    ].

generate_components() ->
    #{
        <<"schemas">> => #{
            <<"Agent">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"id">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Unique agent identifier">>},
                    <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Agent name">>},
                    <<"type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"simple">>, <<"ai">>, <<"template">>]},
                    <<"status">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"active">>, <<"idle">>, <<"error">>]},
                    <<"model">> => #{<<"type">> => <<"string">>, <<"description">> => <<"AI model being used">>},
                    <<"created_at">> => #{<<"type">> => <<"string">>, <<"format">> => <<"date-time">>},
                    <<"memory">> => #{<<"type">> => <<"integer">>, <<"description">> => <<"Memory usage in bytes">>},
                    <<"message_queue_len">> => #{<<"type">> => <<"integer">>, <<"description">> => <<"Message queue length">>},
                    <<"health_score">> => #{<<"type">> => <<"integer">>, <<"minimum">> => 0, <<"maximum">> => 100}
                },
                <<"required">> => [<<"id">>, <<"type">>, <<"status">>]
            },
            <<"AgentsResponse">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"agents">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"$ref">> => <<"#/components/schemas/Agent">>}
                    },
                    <<"total">> => #{<<"type">> => <<"integer">>},
                    <<"page">> => #{<<"type">> => <<"integer">>},
                    <<"page_size">> => #{<<"type">> => <<"integer">>}
                }
            },
            <<"CreateAgentRequest">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"type">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"simple">>, <<"ai">>, <<"template">>]},
                    <<"name">> => #{<<"type">> => <<"string">>},
                    <<"model">> => #{<<"type">> => <<"string">>},
                    <<"tools">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"string">>}
                    }
                },
                <<"required">> => [<<"type">>]
            },
            <<"ChatRequest">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"message">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Chat message content">>},
                    <<"stream">> => #{<<"type">> => <<"boolean">>, <<"default">> => false}
                },
                <<"required">> => [<<"message">>]
            },
            <<"ChatResponse">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"response">> => #{<<"type">> => <<"string">>},
                    <<"agent_id">> => #{<<"type">> => <<"string">>},
                    <<"timestamp">> => #{<<"type">> => <<"string">>, <<"format">> => <<"date-time">>}
                }
            },
            <<"MCPServer">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"id">> => #{<<"type">> => <<"string">>},
                    <<"name">> => #{<<"type">> => <<"string">>},
                    <<"description">> => #{<<"type">> => <<"string">>},
                    <<"status">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"connected">>, <<"disconnected">>, <<"error">>]},
                    <<"capabilities">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"string">>}
                    }
                }
            },
            <<"MCPServersResponse">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"servers">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"$ref">> => <<"#/components/schemas/MCPServer">>}
                    }
                }
            },
            <<"SystemHealth">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"status">> => #{<<"type">> => <<"string">>, <<"enum">> => [<<"healthy">>, <<"degraded">>, <<"critical">>]},
                    <<"uptime">> => #{<<"type">> => <<"integer">>, <<"description">> => <<"Uptime in milliseconds">>},
                    <<"memory_usage">> => #{<<"type">> => <<"integer">>, <<"description">> => <<"Memory usage percentage">>},
                    <<"cpu_usage">> => #{<<"type">> => <<"integer">>, <<"description">> => <<"CPU usage percentage">>},
                    <<"process_count">> => #{<<"type">> => <<"integer">>},
                    <<"agents_count">> => #{<<"type">> => <<"integer">>}
                }
            },
            <<"FleetStatus">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"total_agents">> => #{<<"type">> => <<"integer">>},
                    <<"active_agents">> => #{<<"type">> => <<"integer">>},
                    <<"idle_agents">> => #{<<"type">> => <<"integer">>},
                    <<"error_agents">> => #{<<"type">> => <<"integer">>},
                    <<"metrics">> => #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"total_messages">> => #{<<"type">> => <<"integer">>},
                            <<"messages_per_second">> => #{<<"type">> => <<"number">>},
                            <<"avg_response_time">> => #{<<"type">> => <<"number">>}
                        }
                    }
                }
            },
            <<"ApiKeyRequirements">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"requirements">> => #{
                        <<"type">> => <<"object">>,
                        <<"additionalProperties">> => #{
                            <<"type">> => <<"object">>,
                            <<"properties">> => #{
                                <<"required">> => #{<<"type">> => <<"boolean">>},
                                <<"description">> => #{<<"type">> => <<"string">>}
                            }
                        }
                    },
                    <<"missing">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"string">>}
                    }
                }
            },
            <<"CrashReports">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"crashes">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{
                            <<"type">> => <<"object">>,
                            <<"properties">> => #{
                                <<"id">> => #{<<"type">> => <<"string">>},
                                <<"timestamp">> => #{<<"type">> => <<"string">>, <<"format">> => <<"date-time">>},
                                <<"module">> => #{<<"type">> => <<"string">>},
                                <<"function">> => #{<<"type">> => <<"string">>},
                                <<"reason">> => #{<<"type">> => <<"string">>},
                                <<"stack_trace">> => #{<<"type">> => <<"string">>}
                            }
                        }
                    },
                    <<"total">> => #{<<"type">> => <<"integer">>}
                }
            }
        }
    }.