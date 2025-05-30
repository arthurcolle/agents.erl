-module(mcp_server).
-behaviour(gen_server).

%% MCP Server implementation that can host local tools, resources, and prompts
%% Supports both WebSocket and stdio transports

-export([start_link/1, stop/1, register_tool/3, register_resource/3, register_prompt/3,
         unregister_tool/2, unregister_resource/2, unregister_prompt/2,
         get_capabilities/1, send_notification/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PROTOCOL_VERSION, <<"2024-11-05">>).
-define(SERVER_NAME, <<"agents.erl MCP Server">>).
-define(SERVER_VERSION, <<"1.0.0">>).

-record(state, {
    server_id,
    config = #{},
    capabilities = #{},
    tools = #{},
    resources = #{},
    prompts = #{},
    clients = #{}, % ClientId -> {TransportPid, ClientInfo}
    transport_type = websocket, % websocket | stdio
    transport_pid,
    port = 8765,
    initialized = false
}).

-record(mcp_tool, {
    name,
    description,
    input_schema,
    handler_fun
}).

-record(mcp_resource, {
    uri,
    name,
    description,
    mime_type,
    content_fun % Function to get content: fun() -> {ok, Content} | {error, Reason}
}).

-record(mcp_prompt, {
    name,
    description,
    arguments = [],
    template_fun % Function to render: fun(Args) -> {ok, Messages} | {error, Reason}
}).

%%====================================================================
%% API
%%====================================================================

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

stop(Pid) ->
    gen_server:stop(Pid).

register_tool(Pid, Name, ToolDef) ->
    gen_server:call(Pid, {register_tool, Name, ToolDef}).

register_resource(Pid, URI, ResourceDef) ->
    gen_server:call(Pid, {register_resource, URI, ResourceDef}).

register_prompt(Pid, Name, PromptDef) ->
    gen_server:call(Pid, {register_prompt, Name, PromptDef}).

unregister_tool(Pid, Name) ->
    gen_server:call(Pid, {unregister_tool, Name}).

unregister_resource(Pid, URI) ->
    gen_server:call(Pid, {unregister_resource, URI}).

unregister_prompt(Pid, Name) ->
    gen_server:call(Pid, {unregister_prompt, Name}).

get_capabilities(Pid) ->
    gen_server:call(Pid, get_capabilities).

send_notification(Pid, Method, Params) ->
    gen_server:cast(Pid, {send_notification, Method, Params}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Config) ->
    process_flag(trap_exit, true),
    
    ServerId = maps:get(server_id, Config, generate_server_id()),
    TransportType = maps:get(transport, Config, websocket),
    Port = maps:get(port, Config, 8765),
    
    % Initialize with default capabilities
    Capabilities = #{
        <<"tools">> => #{
            <<"listChanged">> => true
        },
        <<"resources">> => #{
            <<"subscribe">> => true,
            <<"listChanged">> => true
        },
        <<"prompts">> => #{
            <<"listChanged">> => true
        },
        <<"logging">> => #{}
    },
    
    State = #state{
        server_id = ServerId,
        config = Config,
        capabilities = Capabilities,
        transport_type = TransportType,
        port = Port
    },
    
    % Register default tools and resources
    NewState = register_default_tools_and_resources(State),
    
    % Start transport
    case start_server_transport(NewState) of
        {ok, TransportPid} ->
            {ok, NewState#state{transport_pid = TransportPid}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({register_tool, Name, ToolDef}, _From, State) ->
    Tool = create_tool_record(Name, ToolDef),
    NewTools = maps:put(Name, Tool, State#state.tools),
    NewState = State#state{tools = NewTools},
    
    % Notify clients of tool list change
    broadcast_notification(<<"notifications/tools/list_changed">>, #{}, NewState),
    
    {reply, ok, NewState};

handle_call({register_resource, URI, ResourceDef}, _From, State) ->
    Resource = create_resource_record(URI, ResourceDef),
    NewResources = maps:put(URI, Resource, State#state.resources),
    NewState = State#state{resources = NewResources},
    
    % Notify clients of resource list change
    broadcast_notification(<<"notifications/resources/list_changed">>, #{}, NewState),
    
    {reply, ok, NewState};

handle_call({register_prompt, Name, PromptDef}, _From, State) ->
    Prompt = create_prompt_record(Name, PromptDef),
    NewPrompts = maps:put(Name, Prompt, State#state.prompts),
    NewState = State#state{prompts = NewPrompts},
    
    % Notify clients of prompt list change
    broadcast_notification(<<"notifications/prompts/list_changed">>, #{}, NewState),
    
    {reply, ok, NewState};

handle_call({unregister_tool, Name}, _From, State) ->
    NewTools = maps:remove(Name, State#state.tools),
    NewState = State#state{tools = NewTools},
    broadcast_notification(<<"notifications/tools/list_changed">>, #{}, NewState),
    {reply, ok, NewState};

handle_call({unregister_resource, URI}, _From, State) ->
    NewResources = maps:remove(URI, State#state.resources),
    NewState = State#state{resources = NewResources},
    broadcast_notification(<<"notifications/resources/list_changed">>, #{}, NewState),
    {reply, ok, NewState};

handle_call({unregister_prompt, Name}, _From, State) ->
    NewPrompts = maps:remove(Name, State#state.prompts),
    NewState = State#state{prompts = NewPrompts},
    broadcast_notification(<<"notifications/prompts/list_changed">>, #{}, NewState),
    {reply, ok, NewState};

handle_call(get_capabilities, _From, State) ->
    {reply, State#state.capabilities, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({send_notification, Method, Params}, State) ->
    broadcast_notification(Method, Params, State),
    {noreply, State};

handle_cast({client_message, ClientId, Message}, State) ->
    NewState = handle_client_message(ClientId, Message, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mcp_client_connected, ClientId, TransportPid, ClientInfo}, State) ->
    NewClients = maps:put(ClientId, {TransportPid, ClientInfo}, State#state.clients),
    {noreply, State#state{clients = NewClients}};

handle_info({mcp_client_disconnected, ClientId}, State) ->
    NewClients = maps:remove(ClientId, State#state.clients),
    {noreply, State#state{clients = NewClients}};

handle_info({'EXIT', Pid, Reason}, State) ->
    case Pid of
        TransportPid when TransportPid =:= State#state.transport_pid ->
            error_logger:error_msg("MCP Server transport died: ~p~n", [Reason]),
            {stop, {transport_died, Reason}, State};
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    cleanup_transport(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

generate_server_id() ->
    iolist_to_binary(io_lib:format("mcp_server_~p", [erlang:system_time(microsecond)])).

start_server_transport(#state{transport_type = websocket, port = Port} = State) ->
    % Start WebSocket server using cowboy
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/mcp", mcp_websocket_handler, [State#state.server_id]}
        ]}
    ]),
    
    case cowboy:start_clear(
        list_to_atom("mcp_server_" ++ binary_to_list(State#state.server_id)),
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ) of
        {ok, _} ->
            io:format("[MCP_SRV] Successfully started MCP server on port ~p~n", [Port]);
        {error, eaddrinuse} ->
            io:format("[MCP_SRV] WARNING: Port ~p already in use, MCP server not started~n", [Port]);
        {error, Reason} ->
            io:format("[MCP_SRV] ERROR: Failed to start MCP server on port ~p: ~p~n", [Port, Reason])
    end,
    
    {ok, self()};

start_server_transport(#state{transport_type = stdio}) ->
    % For stdio, we're already connected via stdin/stdout
    {ok, self()}.

create_tool_record(Name, #{description := Description, input_schema := Schema, handler := Handler}) ->
    #mcp_tool{
        name = Name,
        description = Description,
        input_schema = Schema,
        handler_fun = Handler
    }.

create_resource_record(URI, #{name := Name, description := Description, 
                             mime_type := MimeType, content_provider := ContentFun}) ->
    #mcp_resource{
        uri = URI,
        name = Name,
        description = Description,
        mime_type = MimeType,
        content_fun = ContentFun
    }.

create_prompt_record(Name, #{description := Description, arguments := Args, 
                            template := TemplateFun}) ->
    #mcp_prompt{
        name = Name,
        description = Description,
        arguments = Args,
        template_fun = TemplateFun
    }.

get_jina_tools() ->
    % Check if Jina API key is available
    case os:getenv("JINA_API_KEY") of
        false ->
            logger:info("JINA_API_KEY not found, skipping Jina AI tools registration"),
            #{};
        _ ->
            logger:info("JINA_API_KEY found, registering Jina AI tools"),
            #{
                <<"jina_search">> => #mcp_tool{
                    name = <<"jina_search">>,
                    description = <<"Search the web using Jina AI Search API with structured results">>,
                    input_schema = #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"query">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"The search query">>
                            },
                            <<"num_results">> => #{
                                <<"type">> => <<"integer">>,
                                <<"description">> => <<"Maximum number of results to return (default: 5)">>,
                                <<"default">> => 5
                            },
                            <<"site">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"Optional domain to restrict search to">>
                            }
                        },
                        <<"required">> => [<<"query">>]
                    },
                    handler_fun = fun(Args) -> jina_tools:jina_search(Args) end
                },
                
                <<"jina_read_webpage">> => #mcp_tool{
                    name = <<"jina_read_webpage">>,
                    description = <<"Extract and read content from a webpage using Jina AI Reader API">>,
                    input_schema = #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"url">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"URL of the webpage to read">>
                            },
                            <<"no_cache">> => #{
                                <<"type">> => <<"boolean">>,
                                <<"description">> => <<"Bypass cache for fresh retrieval">>,
                                <<"default">> => false
                            }
                        },
                        <<"required">> => [<<"url">>]
                    },
                    handler_fun = fun(Args) -> jina_tools:jina_read_webpage(Args) end
                },
                
                <<"jina_fact_check">> => #mcp_tool{
                    name = <<"jina_fact_check">>,
                    description = <<"Fact-check a query using Jina AI grounding capabilities">>,
                    input_schema = #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"query">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"The query or statement to fact-check">>
                            }
                        },
                        <<"required">> => [<<"query">>]
                    },
                    handler_fun = fun(Args) -> jina_tools:jina_fact_check(Args) end
                },
                
                <<"jina_embed_text">> => #mcp_tool{
                    name = <<"jina_embed_text">>,
                    description = <<"Create embeddings for text using Jina AI embedding models">>,
                    input_schema = #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"text">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"Text to create embeddings for">>
                            },
                            <<"model">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"Embedding model to use">>,
                                <<"default">> => <<"jina-embeddings-v3">>
                            }
                        },
                        <<"required">> => [<<"text">>]
                    },
                    handler_fun = fun(Args) -> jina_tools:jina_embed_text(Args) end
                },
                
                <<"jina_rerank">> => #mcp_tool{
                    name = <<"jina_rerank">>,
                    description = <<"Rerank documents based on relevance to a query using Jina AI Reranker">>,
                    input_schema = #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"query">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"The search query">>
                            },
                            <<"documents">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"Documents separated by '---'">>
                            },
                            <<"top_n">> => #{
                                <<"type">> => <<"integer">>,
                                <<"description">> => <<"Number of top results to return">>,
                                <<"default">> => 5
                            }
                        },
                        <<"required">> => [<<"query">>, <<"documents">>]
                    },
                    handler_fun = fun(Args) -> jina_tools:jina_rerank(Args) end
                },
                
                <<"jina_classify">> => #mcp_tool{
                    name = <<"jina_classify">>,
                    description = <<"Classify text or images into predefined categories using Jina AI">>,
                    input_schema = #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"inputs">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"Items to classify, separated by '---'">>
                            },
                            <<"labels">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"Comma-separated classification labels">>
                            },
                            <<"is_image">> => #{
                                <<"type">> => <<"boolean">>,
                                <<"description">> => <<"Whether inputs are images">>,
                                <<"default">> => false
                            }
                        },
                        <<"required">> => [<<"inputs">>, <<"labels">>]
                    },
                    handler_fun = fun(Args) -> jina_tools:jina_classify(Args) end
                },
                
                <<"jina_segment">> => #mcp_tool{
                    name = <<"jina_segment">>,
                    description = <<"Segment text into manageable chunks using Jina AI Segmenter">>,
                    input_schema = #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"content">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"Text content to segment">>
                            },
                            <<"max_chunk_length">> => #{
                                <<"type">> => <<"integer">>,
                                <<"description">> => <<"Maximum characters per chunk">>,
                                <<"default">> => 1000
                            }
                        },
                        <<"required">> => [<<"content">>]
                    },
                    handler_fun = fun(Args) -> jina_tools:jina_segment(Args) end
                },
                
                <<"jina_deep_search">> => #mcp_tool{
                    name = <<"jina_deep_search">>,
                    description = <<"Perform comprehensive search with reasoning using Jina AI DeepSearch">>,
                    input_schema = #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"query">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"The search query">>
                            },
                            <<"reasoning_effort">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"Level of reasoning effort">>,
                                <<"default">> => <<"medium">>,
                                <<"enum">> => [<<"low">>, <<"medium">>, <<"high">>]
                            }
                        },
                        <<"required">> => [<<"query">>]
                    },
                    handler_fun = fun(Args) -> jina_tools:jina_deep_search(Args) end
                }
            }
    end.

register_default_tools_and_resources(State) ->
    % Register built-in tools
    Tools = #{
        <<"get_system_info">> => #mcp_tool{
            name = <<"get_system_info">>,
            description = <<"Get system information and metrics">>,
            input_schema = #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"required">> => []
            },
            handler_fun = fun(_Args) -> 
                {ok, #{
                    <<"content">> => [#{
                        <<"type">> => <<"text">>,
                        <<"text">> => iolist_to_binary(io_lib:format("System: ~p, OTP: ~s", 
                                                                     [erlang:system_info(system_architecture),
                                                                      erlang:system_info(otp_release)]))
                    }]
                }}
            end
        },
        
        <<"list_agents">> => #mcp_tool{
            name = <<"list_agents">>,
            description = <<"List all active agents in the system">>,
            input_schema = #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"required">> => []
            },
            handler_fun = fun(_Args) ->
                % Get agents from registry
                try
                    Agents = agent_registry:list_agents(),
                    AgentList = lists:map(fun({Id, Info}) ->
                        iolist_to_binary(io_lib:format("~p: ~p", [Id, Info]))
                    end, Agents),
                    {ok, #{
                        <<"content">> => [#{
                            <<"type">> => <<"text">>,
                            <<"text">> => iolist_to_binary(string:join(AgentList, "\n"))
                        }]
                    }}
                catch
                    _:_ ->
                        {ok, #{
                            <<"content">> => [#{
                                <<"type">> => <<"text">>,
                                <<"text">> => <<"No agents found or registry unavailable">>
                            }]
                        }}
                end
            end
        },
        
        <<"execute_shell_command">> => #mcp_tool{
            name = <<"execute_shell_command">>,
            description = <<"Execute a shell command and return the result">>,
            input_schema = #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"command">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The shell command to execute">>
                    }
                },
                <<"required">> => [<<"command">>]
            },
            handler_fun = fun(#{<<"command">> := Command}) ->
                try
                    Port = open_port({spawn, binary_to_list(Command)}, 
                                   [exit_status, stderr_to_stdout, {line, 1000}]),
                    Result = collect_shell_output(Port, []),
                    {ok, #{
                        <<"content">> => [#{
                            <<"type">> => <<"text">>,
                            <<"text">> => Result
                        }]
                    }}
                catch
                    _:Error ->
                        {error, #{
                            <<"content">> => [#{
                                <<"type">> => <<"text">>,
                                <<"text">> => iolist_to_binary(io_lib:format("Error: ~p", [Error]))
                            }],
                            <<"isError">> => true
                        }}
                end
            end
        }
    },
    
    % Register Jina AI tools
    JinaTools = get_jina_tools(),
    AllTools = maps:merge(Tools, JinaTools),
    
    % Register built-in resources
    Resources = #{
        <<"file://system_config">> => #mcp_resource{
            uri = <<"file://system_config">>,
            name = <<"System Configuration">>,
            description = <<"Current system configuration and settings">>,
            mime_type = <<"application/json">>,
            content_fun = fun() ->
                Config = #{
                    otp_release => list_to_binary(erlang:system_info(otp_release)),
                    system_architecture => erlang:system_info(system_architecture),
                    logical_processors => erlang:system_info(logical_processors),
                    process_count => erlang:system_info(process_count),
                    memory_total => erlang:memory(total)
                },
                {ok, jsx:encode(Config)}
            end
        },
        
        <<"file://agent_logs">> => #mcp_resource{
            uri = <<"file://agent_logs">>,
            name = <<"Agent System Logs">>,
            description = <<"Recent log entries from the agent system">>,
            mime_type = <<"text/plain">>,
            content_fun = fun() ->
                % This would typically read from a log file
                {ok, <<"Recent agent system activity...\nNo specific logs configured.">>}
            end
        }
    },
    
    % Register built-in prompts
    Prompts = #{
        <<"analyze_system">> => #mcp_prompt{
            name = <<"analyze_system">>,
            description = <<"Analyze system performance and provide recommendations">>,
            arguments = [
                #{
                    <<"name">> => <<"focus">>,
                    <<"description">> => <<"What aspect to focus on (performance, security, resources)">>,
                    <<"required">> => false
                }
            ],
            template_fun = fun(Args) ->
                Focus = maps:get(<<"focus">>, Args, <<"general">>),
                {ok, [
                    #{
                        <<"role">> => <<"user">>,
                        <<"content">> => #{
                            <<"type">> => <<"text">>,
                            <<"text">> => iolist_to_binary(io_lib:format(
                                "Please analyze the system with focus on ~s. "
                                "Use the available tools to gather system information "
                                "and provide actionable recommendations.", [Focus]))
                        }
                    }
                ]}
            end
        }
    },
    
    State#state{
        tools = AllTools,
        resources = Resources,
        prompts = Prompts
    }.

handle_client_message(ClientId, #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Message, State) ->
    case Message of
        #{<<"id">> := RequestId} ->
            % This is a request
            handle_mcp_request(ClientId, RequestId, Method, maps:get(<<"params">>, Message, #{}), State);
        _ ->
            % This is a notification
            handle_mcp_notification(ClientId, Method, maps:get(<<"params">>, Message, #{}), State)
    end.

handle_mcp_request(ClientId, RequestId, <<"initialize">>, Params, State) ->
    #{<<"protocolVersion">> := Version, <<"clientInfo">> := ClientInfo} = Params,
    
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"result">> => #{
            <<"protocolVersion">> => ?PROTOCOL_VERSION,
            <<"capabilities">> => State#state.capabilities,
            <<"serverInfo">> => #{
                <<"name">> => ?SERVER_NAME,
                <<"version">> => ?SERVER_VERSION
            },
            <<"instructions">> => <<"This is an Erlang-based MCP server providing access to agent system tools and resources.">>
        }
    },
    
    send_to_client(ClientId, Response, State),
    State#state{initialized = true};

handle_mcp_request(ClientId, RequestId, <<"tools/list">>, _Params, State) ->
    Tools = lists:map(fun({_Name, #mcp_tool{name = N, description = D, input_schema = S}}) ->
        #{
            <<"name">> => N,
            <<"description">> => D,
            <<"inputSchema">> => S
        }
    end, maps:to_list(State#state.tools)),
    
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"result">> => #{
            <<"tools">> => Tools
        }
    },
    
    send_to_client(ClientId, Response, State),
    State;

handle_mcp_request(ClientId, RequestId, <<"tools/call">>, #{<<"name">> := ToolName, <<"arguments">> := Args}, State) ->
    case maps:find(ToolName, State#state.tools) of
        {ok, #mcp_tool{handler_fun = Handler}} ->
            try
                Result = Handler(Args),
                Response = #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => RequestId,
                    <<"result">> => Result
                },
                send_to_client(ClientId, Response, State)
            catch
                _:Error ->
                    ErrorResponse = #{
                        <<"jsonrpc">> => <<"2.0">>,
                        <<"id">> => RequestId,
                        <<"error">> => #{
                            <<"code">> => -32603,
                            <<"message">> => <<"Tool execution failed">>,
                            <<"data">> => iolist_to_binary(io_lib:format("~p", [Error]))
                        }
                    },
                    send_to_client(ClientId, ErrorResponse, State)
            end;
        error ->
            ErrorResponse = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => RequestId,
                <<"error">> => #{
                    <<"code">> => -32602,
                    <<"message">> => <<"Unknown tool">>,
                    <<"data">> => ToolName
                }
            },
            send_to_client(ClientId, ErrorResponse, State)
    end,
    State;

handle_mcp_request(ClientId, RequestId, <<"resources/list">>, _Params, State) ->
    Resources = lists:map(fun({URI, #mcp_resource{name = N, description = D, mime_type = M}}) ->
        #{
            <<"uri">> => URI,
            <<"name">> => N,
            <<"description">> => D,
            <<"mimeType">> => M
        }
    end, maps:to_list(State#state.resources)),
    
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"result">> => #{
            <<"resources">> => Resources
        }
    },
    
    send_to_client(ClientId, Response, State),
    State;

handle_mcp_request(ClientId, RequestId, <<"resources/read">>, #{<<"uri">> := URI}, State) ->
    case maps:find(URI, State#state.resources) of
        {ok, #mcp_resource{content_fun = ContentFun, mime_type = MimeType}} ->
            try
                {ok, Content} = ContentFun(),
                ContentItem = case MimeType of
                    <<"application/json">> ->
                        #{<<"uri">> => URI, <<"mimeType">> => MimeType, <<"text">> => Content};
                    <<"text/", _/binary>> ->
                        #{<<"uri">> => URI, <<"mimeType">> => MimeType, <<"text">> => Content};
                    _ ->
                        #{<<"uri">> => URI, <<"mimeType">> => MimeType, <<"blob">> => base64:encode(Content)}
                end,
                
                Response = #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => RequestId,
                    <<"result">> => #{
                        <<"contents">> => [ContentItem]
                    }
                },
                send_to_client(ClientId, Response, State)
            catch
                _:Error ->
                    ErrorResponse = #{
                        <<"jsonrpc">> => <<"2.0">>,
                        <<"id">> => RequestId,
                        <<"error">> => #{
                            <<"code">> => -32603,
                            <<"message">> => <<"Resource read failed">>,
                            <<"data">> => iolist_to_binary(io_lib:format("~p", [Error]))
                        }
                    },
                    send_to_client(ClientId, ErrorResponse, State)
            end;
        error ->
            ErrorResponse = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => RequestId,
                <<"error">> => #{
                    <<"code">> => -32602,
                    <<"message">> => <<"Resource not found">>,
                    <<"data">> => URI
                }
            },
            send_to_client(ClientId, ErrorResponse, State)
    end,
    State;

handle_mcp_request(ClientId, RequestId, <<"prompts/list">>, _Params, State) ->
    Prompts = lists:map(fun({Name, #mcp_prompt{description = D, arguments = A}}) ->
        #{
            <<"name">> => Name,
            <<"description">> => D,
            <<"arguments">> => A
        }
    end, maps:to_list(State#state.prompts)),
    
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"result">> => #{
            <<"prompts">> => Prompts
        }
    },
    
    send_to_client(ClientId, Response, State),
    State;

handle_mcp_request(ClientId, RequestId, <<"prompts/get">>, #{<<"name">> := PromptName} = Params, State) ->
    case maps:find(PromptName, State#state.prompts) of
        {ok, #mcp_prompt{template_fun = TemplateFun, description = Description}} ->
            try
                Args = maps:get(<<"arguments">>, Params, #{}),
                {ok, Messages} = TemplateFun(Args),
                
                Response = #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => RequestId,
                    <<"result">> => #{
                        <<"description">> => Description,
                        <<"messages">> => Messages
                    }
                },
                send_to_client(ClientId, Response, State)
            catch
                _:Error ->
                    ErrorResponse = #{
                        <<"jsonrpc">> => <<"2.0">>,
                        <<"id">> => RequestId,
                        <<"error">> => #{
                            <<"code">> => -32603,
                            <<"message">> => <<"Prompt rendering failed">>,
                            <<"data">> => iolist_to_binary(io_lib:format("~p", [Error]))
                        }
                    },
                    send_to_client(ClientId, ErrorResponse, State)
            end;
        error ->
            ErrorResponse = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => RequestId,
                <<"error">> => #{
                    <<"code">> => -32602,
                    <<"message">> => <<"Prompt not found">>,
                    <<"data">> => PromptName
                }
            },
            send_to_client(ClientId, ErrorResponse, State)
    end,
    State;

handle_mcp_request(ClientId, RequestId, <<"ping">>, _Params, State) ->
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"result">> => #{}
    },
    send_to_client(ClientId, Response, State),
    State;

handle_mcp_request(ClientId, RequestId, Method, _Params, State) ->
    ErrorResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"error">> => #{
            <<"code">> => -32601,
            <<"message">> => <<"Method not found">>,
            <<"data">> => Method
        }
    },
    send_to_client(ClientId, ErrorResponse, State),
    State.

handle_mcp_notification(ClientId, <<"notifications/initialized">>, _Params, State) ->
    % Client has completed initialization
    error_logger:info_msg("MCP Client ~p initialized~n", [ClientId]),
    State;

handle_mcp_notification(_ClientId, Method, _Params, State) ->
    error_logger:warning_msg("Unhandled notification: ~s~n", [Method]),
    State.

send_to_client(ClientId, Message, State) ->
    case maps:find(ClientId, State#state.clients) of
        {ok, {TransportPid, _ClientInfo}} ->
            mcp_transport:send_message(TransportPid, Message);
        error ->
            error_logger:warning_msg("Attempt to send to unknown client: ~p~n", [ClientId])
    end.

broadcast_notification(Method, Params, State) ->
    Notification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method,
        <<"params">> => Params
    },
    
    maps:fold(fun(ClientId, {TransportPid, _}, _) ->
        mcp_transport:send_message(TransportPid, Notification)
    end, ok, State#state.clients).

collect_shell_output(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            collect_shell_output(Port, [Line, "\n" | Acc]);
        {Port, {data, {noeol, Line}}} ->
            collect_shell_output(Port, [Line | Acc]);
        {Port, {exit_status, 0}} ->
            list_to_binary(lists:reverse(Acc));
        {Port, {exit_status, Status}} ->
            Result = lists:reverse(Acc),
            iolist_to_binary([Result, "\nExit status: ", integer_to_list(Status)])
    after 5000 ->
        list_to_binary(lists:reverse(["Command timeout" | Acc]))
    end.

cleanup_transport(#state{transport_type = websocket}) ->
    % Cowboy will handle cleanup
    ok;
cleanup_transport(_State) ->
    ok.