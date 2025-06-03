%% agent_tools.erl
%% Registry for agent tools and their executors
-module(agent_tools).
-behaviour(gen_server).

-export([
    start_link/1,
    register_tool/2,
    register_executor/3,
    unregister_tool/1,
    get_tools/1,
    get_enhanced_tools/1,
    execute_tool/2,
    list_tools/0,
    create_or_update_borrowed_tool/3,
    fork_tool/3,
    get_tool_versions/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

%% Logging macros
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format(Msg, Args))).
-define(LOG_WARNING(Msg), colored_logger:alarm(medium, Msg)).
-define(LOG_WARNING(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).
-define(LOG_DEBUG(Msg), colored_logger:system(network, Msg)).
-define(LOG_DEBUG(Msg, Args), colored_logger:system(network, io_lib:format(Msg, Args))).
-define(LOG_SUCCESS(Msg), colored_logger:complete(success, Msg)).
-define(LOG_SUCCESS(Msg, Args), colored_logger:complete(success, io_lib:format(Msg, Args))).

%% Helper functions export
-export([element_type/1]).

-record(state, {
    tools = #{} :: map(),       % Tool name -> schema mapping
    executors = #{} :: map(),   % Tool name -> executor function mapping
    borrowed_tools = #{} :: map(), % Tool name -> {version, metadata, schema}
    tool_versions = #{} :: map()    % Tool name -> [version_info]
}).

%% API Functions
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Register a tool schema
register_tool(Name, Schema) ->
    gen_server:call(?SERVER, {register_tool, Name, Schema}).

%% Register a tool executor function
register_executor(Name, ExecutorFn, Options) ->
    gen_server:call(?SERVER, {register_executor, Name, ExecutorFn, Options}).

%% Unregister a tool
unregister_tool(Name) ->
    gen_server:call(?SERVER, {unregister_tool, Name}).

%% Get tool schemas for specified tool names
get_tools(ToolNames) ->
    gen_server:call(?SERVER, {get_tools, ToolNames}).

%% Get all tools including MCP tools
get_all_tools() ->
    gen_server:call(?SERVER, get_all_tools).

%% Get tools enhanced with MCP tools
get_enhanced_tools(ToolNames) ->
    Tools = gen_server:call(?SERVER, {get_enhanced_tools, ToolNames}),
    % Format tools for OpenAI API
    format_tools_for_openai(Tools).

%% Execute a tool with the given arguments
execute_tool(Name, Arguments) ->
    StartTime = erlang:monotonic_time(millisecond),
    ?LOG_INFO("[TOOLS] 🔧 Execute tool request: ~p", [Name]),
    ?LOG_DEBUG("[TOOLS] Arguments: ~p", [Arguments]),
    
    Result = gen_server:call(?SERVER, {execute_tool, Name, Arguments}, infinity),
    
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    case Result of
        {ok, _} ->
            ?LOG_SUCCESS("[TOOLS] ✅ Tool ~p executed successfully in ~pms", [Name, Duration]);
        {error, Reason} ->
            ?LOG_ERROR("[TOOLS] ❌ Tool ~p failed after ~pms: ~p", [Name, Duration, Reason])
    end,
    Result.

%% List all registered tools
list_tools() ->
    gen_server:call(?SERVER, list_tools).

%% gen_server callbacks
init(Options) ->
    % Initialize with default tools
    DefaultTools = maps:get(default_tools, Options, #{}),
    
    % Register predefined tools if requested
    RegisterPredefined = maps:get(register_predefined, Options, true),
    PredefinedTools = case RegisterPredefined of
        true -> predefined_tools();
        false -> #{}
    end,
    
    % Combine default and predefined tools
    AllTools = maps:merge(PredefinedTools, DefaultTools),
    
    % Initialize with default executors for predefined tools
    DefaultExecutors = maps:map(
        fun(_Name, _) -> 
            fun predefined_executor/2
        end,
        PredefinedTools
    ),
    
    {ok, #state{
        tools = AllTools,
        executors = DefaultExecutors,
        borrowed_tools = #{},
        tool_versions = #{}
    }}.

handle_call({register_tool, Name, Schema}, _From, State) ->
    % Validate schema
    case validate_tool_schema(Schema) of
        ok ->
            % Update tools map
            NewTools = maps:put(Name, Schema, State#state.tools),
            {reply, ok, State#state{tools = NewTools}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({register_executor, Name, ExecutorFn, _Options}, _From, State) ->
    % Check if tool exists
    case maps:is_key(Name, State#state.tools) of
        true ->
            % Update executors map
            NewExecutors = maps:put(Name, ExecutorFn, State#state.executors),
            {reply, ok, State#state{executors = NewExecutors}};
        false ->
            {reply, {error, {unknown_tool, Name}}, State}
    end;

handle_call({unregister_tool, Name}, _From, State) ->
    % Remove tool and its executor
    NewTools = maps:remove(Name, State#state.tools),
    NewExecutors = maps:remove(Name, State#state.executors),
    {reply, ok, State#state{tools = NewTools, executors = NewExecutors}};

handle_call({get_tools, ToolNames}, _From, State) ->
    % Filter tools by name
    SelectedTools = lists:foldl(
        fun(ToolName, Acc) ->
            case maps:find(ToolName, State#state.tools) of
                {ok, Schema} -> [Schema | Acc];
                error -> Acc
            end
        end,
        [],
        ToolNames
    ),
    {reply, SelectedTools, State};

handle_call({execute_tool, Name, Arguments}, _From, State) ->
    % Look up the executor for this tool
    Result = case maps:find(Name, State#state.executors) of
        {ok, ExecutorFn} ->
            try
                ExecutorFn(Name, Arguments)
            catch
                E:R:S ->
                    {error, {tool_execution_failed, E, R, S}}
            end;
        error ->
            % Check if it's an MCP tool
            try
                case mcp_agent_integration:handle_llm_tool_call(Name, Arguments) of
                    {error, not_mcp_tool} ->
                        {error, {unknown_tool, Name}};
                    McpResult ->
                        McpResult
                end
            catch
                _:_ ->
                    {error, {unknown_tool, Name}}
            end
    end,
    {reply, Result, State};

handle_call(list_tools, _From, State) ->
    {reply, maps:keys(State#state.tools), State};

handle_call(get_all_tools, _From, State) ->
    % Get local tools
    LocalTools = maps:values(State#state.tools),
    
    % Try to get MCP tools if available
    McpTools = try
        mcp_agent_integration:format_tools_for_llm()
    catch
        _:_ -> []
    end,
    
    AllTools = LocalTools ++ McpTools,
    {reply, AllTools, State};

handle_call({get_enhanced_tools, ToolNames}, _From, State) ->
    % Get requested local tools
    LocalTools = lists:foldl(
        fun(ToolName, Acc) ->
            case maps:find(ToolName, State#state.tools) of
                {ok, Schema} -> [Schema | Acc];
                error -> Acc
            end
        end,
        [],
        ToolNames
    ),
    
    % Add MCP tools
    McpTools = try
        mcp_agent_integration:format_tools_for_llm()
    catch
        _:_ -> []
    end,
    
    EnhancedTools = LocalTools ++ McpTools,
    {reply, EnhancedTools, State};

handle_call({create_or_update_borrowed_tool, Name, SourceSchema, Metadata}, _From, State) ->
    % Create a version hash based on the schema and timestamp
    Version = generate_version_hash(SourceSchema, Metadata),
    
    % Create borrowed tool with version info
    BorrowedTool = {Version, Metadata, SourceSchema},
    
    % Update borrowed tools
    NewBorrowedTools = maps:put(Name, BorrowedTool, State#state.borrowed_tools),
    
    % Update version history
    VersionInfo = #{
        version => Version,
        created_at => erlang:system_time(millisecond),
        metadata => Metadata,
        schema => SourceSchema
    },
    ExistingVersions = maps:get(Name, State#state.tool_versions, []),
    NewVersions = [VersionInfo | ExistingVersions],
    NewToolVersions = maps:put(Name, NewVersions, State#state.tool_versions),
    
    % Also register as a regular tool
    NewTools = maps:put(Name, SourceSchema, State#state.tools),
    
    {reply, {ok, Version}, State#state{
        borrowed_tools = NewBorrowedTools,
        tool_versions = NewToolVersions,
        tools = NewTools
    }};

handle_call({fork_tool, BaseName, NewName, Modifications}, _From, State) ->
    case maps:find(BaseName, State#state.tools) of
        {ok, BaseSchema} ->
            % Apply modifications to create forked tool
            ForkedSchema = apply_tool_modifications(BaseSchema, Modifications),
            
            % Create version hash for the fork
            Version = generate_version_hash(ForkedSchema, #{forked_from => BaseName}),
            
            % Create borrowed tool entry
            BorrowedTool = {Version, #{forked_from => BaseName}, ForkedSchema},
            NewBorrowedTools = maps:put(NewName, BorrowedTool, State#state.borrowed_tools),
            
            % Update version history
            VersionInfo = #{
                version => Version,
                created_at => erlang:system_time(millisecond),
                forked_from => BaseName,
                modifications => Modifications
            },
            ExistingVersions = maps:get(NewName, State#state.tool_versions, []),
            NewVersions = [VersionInfo | ExistingVersions],
            NewToolVersions = maps:put(NewName, NewVersions, State#state.tool_versions),
            
            % Register as regular tool
            NewTools = maps:put(NewName, ForkedSchema, State#state.tools),
            
            {reply, {ok, Version}, State#state{
                borrowed_tools = NewBorrowedTools,
                tool_versions = NewToolVersions,
                tools = NewTools
            }};
        error ->
            {reply, {error, {tool_not_found, BaseName}}, State}
    end;

handle_call({get_tool_versions, ToolName}, _From, State) ->
    Versions = maps:get(ToolName, State#state.tool_versions, []),
    {reply, Versions, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

%% Basic schema validation
validate_tool_schema(Schema) ->
    % Check required fields
    RequiredFields = [<<"name">>, <<"description">>, <<"parameters">>],
    HasAllRequired = lists:all(
        fun(Field) -> maps:is_key(Field, Schema) end,
        RequiredFields
    ),
    
    case HasAllRequired of
        true -> ok;
        false -> {error, missing_required_fields}
    end.

%% Predefined tools
predefined_tools() ->
    % Include Jina tools
    JinaTools = get_jina_tools(),
    BaseTools = #{
        shell => #{
            <<"name">> => <<"shell">>,
            <<"description">> => <<"Execute shell commands">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"command">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The shell command to execute">>
                    }
                },
                <<"required">> => [<<"command">>],
                <<"additionalProperties">> => false
            }
        },
        
        file_read => #{
            <<"name">> => <<"file_read">>,
            <<"description">> => <<"Read file contents">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"path">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The path to the file to read">>
                    }
                },
                <<"required">> => [<<"path">>],
                <<"additionalProperties">> => false
            }
        },
        
        file_write => #{
            <<"name">> => <<"file_write">>,
            <<"description">> => <<"Write content to a file">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"path">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The path to the file to write">>
                    },
                    <<"content">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The content to write to the file">>
                    }
                },
                <<"required">> => [<<"path">>, <<"content">>],
                <<"additionalProperties">> => false
            }
        },
        
        http_request => #{
            <<"name">> => <<"http_request">>,
            <<"description">> => <<"Make an HTTP request">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"method">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The HTTP method to use">>
                    },
                    <<"url">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The URL to make the request to">>
                    },
                    <<"headers">> => #{
                        <<"type">> => <<"object">>,
                        <<"description">> => <<"HTTP headers to include in the request">>
                    },
                    <<"body">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Request body">>
                    }
                },
                <<"required">> => [<<"method">>, <<"url">>, <<"headers">>, <<"body">>],
                <<"additionalProperties">> => false
            }
        },
        
        knowledge_base_retrieval => #{
            <<"name">> => <<"knowledge_base_retrieval">>,
            <<"description">> => <<"Search and retrieve information from domain-specific knowledge bases">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"domain">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The knowledge domain to search (e.g., psychology, medicine, education)">>
                    },
                    <<"query">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The search query or topic to find information about">>
                    },
                    <<"max_results">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum number of results to return (default: 5)">>
                    }
                },
                <<"required">> => [<<"domain">>, <<"query">>, <<"max_results">>],
                <<"additionalProperties">> => false
            }
        },
        
        create_supervisor => #{
            <<"name">> => <<"create_supervisor">>,
            <<"description">> => <<"Create a new dynamic supervisor at runtime">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"name">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The name for the new supervisor (must be unique)">>
                    },
                    <<"strategy">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"one_for_one">>, <<"one_for_all">>, <<"rest_for_one">>, <<"simple_one_for_one">>],
                        <<"description">> => <<"The restart strategy for the supervisor">>
                    },
                    <<"max_restarts">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum number of restarts allowed in the time period">>,
                        <<"default">> => 10
                    },
                    <<"max_time">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Time period in seconds for restart intensity">>,
                        <<"default">> => 60
                    }
                },
                <<"required">> => [<<"name">>, <<"strategy">>, <<"max_restarts">>, <<"max_time">>],
                <<"additionalProperties">> => false
            }
        },
        
        stop_supervisor => #{
            <<"name">> => <<"stop_supervisor">>,
            <<"description">> => <<"Stop and remove a dynamic supervisor">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"name">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The name of the supervisor to stop">>
                    }
                },
                <<"required">> => [<<"name">>],
                <<"additionalProperties">> => false
            }
        },
        
        list_supervisors => #{
            <<"name">> => <<"list_supervisors">>,
            <<"description">> => <<"List all active dynamic supervisors">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        add_child_to_supervisor => #{
            <<"name">> => <<"add_child_to_supervisor">>,
            <<"description">> => <<"Add a child process to a supervisor">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"supervisor_name">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The name of the supervisor">>
                    },
                    <<"child_id">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Unique identifier for the child">>
                    },
                    <<"module">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The module to start (e.g., 'agent_instance')">>
                    },
                    <<"function">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The function to call (default: 'start_link')">>
                    },
                    <<"args">> => #{
                        <<"type">> => <<"array">>,
                        <<"description">> => <<"Arguments to pass to the start function">>,
                        <<"items">> => #{<<"type">> => <<"string">>}
                    },
                    <<"restart">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"permanent">>, <<"temporary">>, <<"transient">>],
                        <<"description">> => <<"Restart strategy for the child">>,
                        <<"default">> => <<"permanent">>
                    },
                    <<"shutdown">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Shutdown timeout in milliseconds">>,
                        <<"default">> => 5000
                    },
                    <<"type">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"worker">>, <<"supervisor">>],
                        <<"description">> => <<"Type of the child process">>,
                        <<"default">> => <<"worker">>
                    }
                },
                <<"required">> => [<<"supervisor_name">>, <<"child_id">>, <<"module">>, <<"function">>, <<"args">>, <<"restart">>, <<"shutdown">>, <<"type">>],
                <<"additionalProperties">> => false
            }
        },
        
        get_supervision_tree => #{
            <<"name">> => <<"get_supervision_tree">>,
            <<"description">> => <<"Get the complete supervision tree structure">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        % Self-awareness tools
        who_am_i => #{
            <<"name">> => <<"who_am_i">>,
            <<"description">> => <<"Get information about yourself - your identity, capabilities, and place in the system">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        where_am_i => #{
            <<"name">> => <<"where_am_i">>,
            <<"description">> => <<"Get your location in the system - supervisor, siblings, and system context">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        get_my_peers => #{
            <<"name">> => <<"get_my_peers">>,
            <<"description">> => <<"Discover other agents that are your peers (siblings under the same supervisor)">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        get_system_state => #{
            <<"name">> => <<"get_system_state">>,
            <<"description">> => <<"Get comprehensive information about the entire system you're running in">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        get_my_capabilities => #{
            <<"name">> => <<"get_my_capabilities">>,
            <<"description">> => <<"List all your capabilities - tools, model, and what you can do">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        get_system_metrics => #{
            <<"name">> => <<"get_system_metrics">>,
            <<"description">> => <<"Get system performance metrics and resource usage">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        analyze_system_topology => #{
            <<"name">> => <<"analyze_system_topology">>,
            <<"description">> => <<"Analyze the complete system topology including all agents, services, and their relationships">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        get_communication_paths => #{
            <<"name">> => <<"get_communication_paths">>,
            <<"description">> => <<"Discover all possible communication paths available to you in the system">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        reflect_on_state => #{
            <<"name">> => <<"reflect_on_state">>,
            <<"description">> => <<"Perform deep reflection on your current state and system understanding">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        % Hot code reloading tools
        reload_module => #{
            <<"name">> => <<"reload_module">>,
            <<"description">> => <<"Hot reload a specific Erlang module in the running system">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"module">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The module name to reload (e.g., 'agent_tools')">>
                    }
                },
                <<"required">> => [<<"module">>],
                <<"additionalProperties">> => false
            }
        },
        
        reload_all_modules => #{
            <<"name">> => <<"reload_all_modules">>,
            <<"description">> => <<"Hot reload all application modules that have changed">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        compile_and_reload => #{
            <<"name">> => <<"compile_and_reload">>,
            <<"description">> => <<"Compile an Erlang source file and hot reload it into the running system">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"source_file">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Path to the Erlang source file (e.g., 'apps/agents/src/agent_tools.erl')">>
                    }
                },
                <<"required">> => [<<"source_file">>],
                <<"additionalProperties">> => false
            }
        },
        
        watch_files => #{
            <<"name">> => <<"watch_files">>,
            <<"description">> => <<"Start watching Erlang source files for changes and auto-reload them">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"files">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"string">>},
                        <<"description">> => <<"List of source files to watch for changes">>
                    }
                },
                <<"required">> => [<<"files">>],
                <<"additionalProperties">> => false
            }
        },
        
        get_module_info => #{
            <<"name">> => <<"get_module_info">>,
            <<"description">> => <<"Get detailed information about a loaded module">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"module">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The module name to inspect">>
                    }
                },
                <<"required">> => [<<"module">>],
                <<"additionalProperties">> => false
            }
        },
        
        get_loaded_modules => #{
            <<"name">> => <<"get_loaded_modules">>,
            <<"description">> => <<"Get list of all loaded application modules">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        
        recursive_search_until_answer => #{
            <<"name">> => <<"recursive_search_until_answer">>,
            <<"description">> => <<"Recursively search and extract information until actual answer is found, not just links">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"query">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The search query seeking specific factual information">>
                    },
                    <<"answer_type">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Type of answer expected (e.g., 'weather', 'temperature', 'news', 'stock_price')">>
                    },
                    <<"location">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Geographic location if relevant to the query">>
                    },
                    <<"max_attempts">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum recursive attempts (default: 5)">>,
                        <<"default">> => 5
                    },
                    <<"preferred_sites">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"string">>},
                        <<"description">> => <<"Preferred sites to search for this type of information">>
                    }
                },
                <<"required">> => [<<"query">>, <<"answer_type">>],
                <<"additionalProperties">> => false
            }
        },
        
        get_weather => #{
            <<"name">> => <<"get_weather">>,
            <<"description">> => <<"Get current weather information for a specific location">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"location">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Location to get weather for (e.g., 'San Francisco, CA', 'London', 'New York')">>
                    }
                },
                <<"required">> => [<<"location">>],
                <<"additionalProperties">> => false
            }
        },
        
        %% Timeline and Training Data Tools
        get_timeline_events => #{
            <<"name">> => <<"get_timeline_events">>,
            <<"description">> => <<"Retrieve timeline events from all agent interactions">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"limit">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum number of events to return (default: 100)">>
                    },
                    <<"filter_type">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"message">>, <<"system">>, <<"agent_action">>, <<"error">>, <<"warning">>, <<"success">>],
                        <<"description">> => <<"Filter events by type">>
                    },
                    <<"filter_source">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"user">>, <<"agent">>, <<"system">>],
                        <<"description">> => <<"Filter events by source">>
                    },
                    <<"conversation_id">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Filter events by specific conversation ID">>
                    },
                    <<"agent_id">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Filter events by specific agent ID">>
                    },
                    <<"since_timestamp">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Only return events after this timestamp (ISO 8601 or milliseconds)">>
                    }
                },
                <<"additionalProperties">> => false
            }
        },
        
        get_conversation_history => #{
            <<"name">> => <<"get_conversation_history">>,
            <<"description">> => <<"Get complete conversation history for a specific conversation ID">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"conversation_id">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The conversation ID to retrieve history for">>
                    },
                    <<"include_metadata">> => #{
                        <<"type">> => <<"boolean">>,
                        <<"description">> => <<"Whether to include metadata in the response (default: true)">>
                    }
                },
                <<"required">> => [<<"conversation_id">>],
                <<"additionalProperties">> => false
            }
        },
        
        get_agent_interactions => #{
            <<"name">> => <<"get_agent_interactions">>,
            <<"description">> => <<"Get all interactions for a specific agent">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"agent_id">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The agent ID to get interactions for">>
                    },
                    <<"limit">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum number of interactions to return (default: 50)">>
                    },
                    <<"include_errors">> => #{
                        <<"type">> => <<"boolean">>,
                        <<"description">> => <<"Whether to include error events (default: false)">>
                    }
                },
                <<"required">> => [<<"agent_id">>],
                <<"additionalProperties">> => false
            }
        },
        
        generate_training_data => #{
            <<"name">> => <<"generate_training_data">>,
            <<"description">> => <<"Generate training data from agent interactions">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"min_quality">> => #{
                        <<"type">> => <<"number">>,
                        <<"description">> => <<"Minimum quality score for training samples (0.0-1.0, default: 0.3)">>
                    },
                    <<"max_samples">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum number of training samples to generate (default: 10000)">>
                    },
                    <<"format">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"openai">>, <<"huggingface">>, <<"jsonl">>],
                        <<"description">> => <<"Output format for training data (default: openai)">>
                    },
                    <<"export">> => #{
                        <<"type">> => <<"boolean">>,
                        <<"description">> => <<"Whether to export data to file (default: false)">>
                    }
                },
                <<"additionalProperties">> => false
            }
        },
        
        search_timeline => #{
            <<"name">> => <<"search_timeline">>,
            <<"description">> => <<"Search timeline events by content or metadata">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"query">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Search query to match against event content">>
                    },
                    <<"case_sensitive">> => #{
                        <<"type">> => <<"boolean">>,
                        <<"description">> => <<"Whether search should be case sensitive (default: false)">>
                    },
                    <<"limit">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum number of results to return (default: 100)">>
                    }
                },
                <<"required">> => [<<"query">>],
                <<"additionalProperties">> => false
            }
        }
    },
    % Merge Jina tools with base tools
    maps:merge(BaseTools, JinaTools).

%% Executor for predefined tools
predefined_executor(ToolName, Arguments) ->
    ?LOG_DEBUG("[PREDEFINED] Executing predefined tool: ~p", [ToolName]),
    ?LOG_DEBUG("[PREDEFINED] Arguments: ~p", [Arguments]),
    
    StartTime = erlang:monotonic_time(millisecond),
    case ToolName of
        shell ->
            % Execute shell command
            Command = maps:get(<<"command">>, Arguments, <<"">>),
            ?LOG_INFO("[PREDEFINED] 💻 Shell command: ~s", [Command]),
            execute_shell_command(Command);
        
        file_read ->
            % Read file
            Path = maps:get(<<"path">>, Arguments, <<"">>),
            ?LOG_INFO("[PREDEFINED] 📄 Reading file: ~s", [Path]),
            read_file(Path);
        
        file_write ->
            % Write file
            Path = maps:get(<<"path">>, Arguments, <<"">>),
            Content = maps:get(<<"content">>, Arguments, <<"">>),
            ContentLen = byte_size(Content),
            ?LOG_INFO("[PREDEFINED] 📝 Writing ~p bytes to file: ~s", [ContentLen, Path]),
            write_file(Path, Content);
        
        http_request ->
            % Make HTTP request
            Method = maps:get(<<"method">>, Arguments, <<"GET">>),
            Url = maps:get(<<"url">>, Arguments, <<"">>),
            Headers = maps:get(<<"headers">>, Arguments, #{}),
            Body = maps:get(<<"body">>, Arguments, <<"">>),
            http_request(Method, Url, Headers, Body);
        
        knowledge_base_retrieval ->
            % Search knowledge base
            Domain = maps:get(<<"domain">>, Arguments, <<"">>),
            Query = maps:get(<<"query">>, Arguments, <<"">>),
            MaxResults = maps:get(<<"max_results">>, Arguments, 5),
            knowledge_base_search(Domain, Query, MaxResults);
        
        % Jina AI tools
        jina_search ->
            Query = maps:get(<<"query">>, Arguments, <<"">>),
            ?LOG_INFO("[PREDEFINED] 🔍 Jina search: ~s", [Query]),
            jina_tools:jina_search(Arguments);
        jina_search_and_read ->
            jina_tools:jina_search_and_read(Arguments);
        jina_read_webpage ->
            jina_tools:jina_read_webpage(Arguments);
        jina_fact_check ->
            jina_tools:jina_fact_check(Arguments);
        jina_embed_text ->
            jina_tools:jina_embed_text(Arguments);
        jina_embed_image ->
            jina_tools:jina_embed_image(Arguments);
        jina_rerank ->
            jina_tools:jina_rerank(Arguments);
        jina_classify ->
            jina_tools:jina_classify(Arguments);
        jina_segment ->
            jina_tools:jina_segment(Arguments);
        jina_deep_search ->
            jina_tools:jina_deep_search(Arguments);
        
        % Supervisor management tools
        create_supervisor ->
            Name = binary_to_atom(maps:get(<<"name">>, Arguments), utf8),
            Strategy = binary_to_atom(maps:get(<<"strategy">>, Arguments), utf8),
            MaxRestarts = maps:get(<<"max_restarts">>, Arguments, 10),
            MaxTime = maps:get(<<"max_time">>, Arguments, 60),
            SupFlags = #{
                strategy => Strategy,
                intensity => MaxRestarts,
                period => MaxTime
            },
            case dynamic_supervisor_manager:create_supervisor(Name, SupFlags) of
                {ok, Pid} ->
                    {ok, iolist_to_binary(io_lib:format("Supervisor ~p created with PID ~p", [Name, Pid]))};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        stop_supervisor ->
            Name = binary_to_atom(maps:get(<<"name">>, Arguments), utf8),
            case dynamic_supervisor_manager:stop_supervisor(Name) of
                ok ->
                    {ok, iolist_to_binary(io_lib:format("Supervisor ~p stopped", [Name]))};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        list_supervisors ->
            case dynamic_supervisor_manager:list_supervisors() of
                {ok, Supervisors} ->
                    {ok, jsx:encode(Supervisors)};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        add_child_to_supervisor ->
            SupName = binary_to_atom(maps:get(<<"supervisor_name">>, Arguments), utf8),
            ChildId = binary_to_atom(maps:get(<<"child_id">>, Arguments), utf8),
            Module = binary_to_atom(maps:get(<<"module">>, Arguments), utf8),
            Function = binary_to_atom(maps:get(<<"function">>, Arguments, <<"start_link">>), utf8),
            Args = maps:get(<<"args">>, Arguments, []),
            Restart = binary_to_atom(maps:get(<<"restart">>, Arguments, <<"permanent">>), utf8),
            Shutdown = maps:get(<<"shutdown">>, Arguments, 5000),
            Type = binary_to_atom(maps:get(<<"type">>, Arguments, <<"worker">>), utf8),
            
            ChildSpec = #{
                id => ChildId,
                start => {Module, Function, Args},
                restart => Restart,
                shutdown => Shutdown,
                type => Type,
                modules => [Module]
            },
            
            case dynamic_supervisor_manager:add_child_to_supervisor(SupName, ChildId, ChildSpec) of
                {ok, Pid} ->
                    {ok, iolist_to_binary(io_lib:format("Child ~p added to supervisor ~p with PID ~p", [ChildId, SupName, Pid]))};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        get_supervision_tree ->
            case dynamic_supervisor_manager:get_supervision_tree() of
                {ok, Tree} ->
                    {ok, jsx:encode(Tree)};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        % Self-awareness tool executors
        who_am_i ->
            execute_who_am_i();
        
        where_am_i ->
            execute_where_am_i();
        
        get_my_peers ->
            execute_get_my_peers();
        
        get_system_state ->
            case system_introspection:get_system_state() of
                {ok, State} ->
                    {ok, jsx:encode(State)};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        get_my_capabilities ->
            execute_get_my_capabilities();
        
        get_system_metrics ->
            case system_introspection:get_system_metrics() of
                {ok, Metrics} ->
                    {ok, jsx:encode(Metrics)};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        analyze_system_topology ->
            case system_introspection:analyze_system_topology() of
                {ok, Topology} ->
                    {ok, jsx:encode(Topology)};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        get_communication_paths ->
            execute_get_communication_paths();
        
        reflect_on_state ->
            execute_reflect_on_state();
        
        % Hot code reloading executors
        reload_module ->
            ModuleName = maps:get(<<"module">>, Arguments, <<"">>),
            Module = binary_to_atom(ModuleName, utf8),
            case hot_code_reloader:reload_module(Module) of
                {ok, Result} ->
                    {ok, jsx:encode(Result)};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        reload_all_modules ->
            case hot_code_reloader:reload_all_modules() of
                Result when is_map(Result) ->
                    {ok, jsx:encode(Result)};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        compile_and_reload ->
            SourceFile = binary_to_list(maps:get(<<"source_file">>, Arguments, <<"">>)),
            case hot_code_reloader:compile_and_reload(SourceFile) of
                {ok, Result} ->
                    {ok, jsx:encode(Result)};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        watch_files ->
            Files = maps:get(<<"files">>, Arguments, []),
            FileList = [binary_to_list(F) || F <- Files],
            case hot_code_reloader:watch_files(FileList) of
                ok ->
                    {ok, jsx:encode(#{status => watching, files => Files})};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        get_module_info ->
            ModuleName = maps:get(<<"module">>, Arguments, <<"">>),
            Module = binary_to_atom(ModuleName, utf8),
            case hot_code_reloader:get_module_info(Module) of
                {ok, Result} ->
                    {ok, jsx:encode(Result)};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        get_loaded_modules ->
            case hot_code_reloader:get_loaded_modules() of
                Result when is_map(Result) ->
                    {ok, jsx:encode(Result)};
                {error, Reason} ->
                    {error, Reason}
            end;
        
        recursive_search_until_answer ->
            Query = maps:get(<<"query">>, Arguments, <<"">>),
            AnswerType = maps:get(<<"answer_type">>, Arguments, <<"general">>),
            Location = maps:get(<<"location">>, Arguments, undefined),
            MaxAttempts = maps:get(<<"max_attempts">>, Arguments, 5),
            PreferredSites = maps:get(<<"preferred_sites">>, Arguments, []),
            execute_recursive_search(Query, AnswerType, Location, MaxAttempts, PreferredSites, 1);
        
        get_weather ->
            Location = maps:get(<<"location">>, Arguments, <<"">>),
            execute_get_weather(Location);
        
        %% Timeline and Training Data Tools
        get_timeline_events ->
            execute_get_timeline_events(Arguments);
        
        get_conversation_history ->
            execute_get_conversation_history(Arguments);
        
        get_agent_interactions ->
            execute_get_agent_interactions(Arguments);
        
        generate_training_data ->
            execute_generate_training_data(Arguments);
        
        search_timeline ->
            execute_search_timeline(Arguments);
        
        _ ->
            ?LOG_ERROR("[PREDEFINED] Unknown tool: ~p", [ToolName]),
            {error, {unknown_predefined_tool, ToolName}}
    end.

%% Execute a shell command
execute_shell_command(Command) ->
    % Convert binary to string if needed
    CmdStr = case is_binary(Command) of
        true -> binary_to_list(Command);
        false -> Command
    end,
    
    ?LOG_INFO("[SHELL] Executing command: ~s", [CmdStr]),
    StartTime = erlang:monotonic_time(millisecond),
    
    % Execute the command
    Port = open_port({spawn, CmdStr}, [exit_status, stderr_to_stdout, {line, 1000}]),
    Result = collect_port_output(Port, []),
    
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    case Result of
        {ok, Output} ->
            OutputLen = byte_size(Output),
            ?LOG_SUCCESS("[SHELL] Command completed in ~pms, output: ~p bytes", [Duration, OutputLen]),
            ?LOG_DEBUG("[SHELL] Output preview: ~s", [binary:part(Output, 0, min(200, OutputLen))]),
            {ok, Output};
        {error, Reason} ->
            ?LOG_ERROR("[SHELL] Command failed after ~pms: ~p", [Duration, Reason]),
            {error, Reason}
    end.

%% Read a file
read_file(Path) ->
    % Convert binary to string if needed
    PathStr = case is_binary(Path) of
        true -> binary_to_list(Path);
        false -> Path
    end,
    
    ?LOG_INFO("[FILE] Reading file: ~s", [PathStr]),
    StartTime = erlang:monotonic_time(millisecond),
    
    % Read the file
    case file:read_file(PathStr) of
        {ok, Content} ->
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            ContentLen = byte_size(Content),
            ?LOG_SUCCESS("[FILE] Read ~p bytes from ~s in ~pms", [ContentLen, PathStr, Duration]),
            {ok, Content};
        {error, Reason} ->
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            ?LOG_ERROR("[FILE] Failed to read ~s after ~pms: ~p", [PathStr, Duration, Reason]),
            {error, {file_read_error, Reason}}
    end.

%% Write to a file
write_file(Path, Content) ->
    % Convert binary to string if needed
    PathStr = case is_binary(Path) of
        true -> binary_to_list(Path);
        false -> Path
    end,
    
    ContentLen = byte_size(Content),
    ?LOG_INFO("[FILE] Writing ~p bytes to: ~s", [ContentLen, PathStr]),
    StartTime = erlang:monotonic_time(millisecond),
    
    % Write the file
    case file:write_file(PathStr, Content) of
        ok ->
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            ?LOG_SUCCESS("[FILE] Successfully wrote ~p bytes to ~s in ~pms", [ContentLen, PathStr, Duration]),
            {ok, <<"File written successfully">>};
        {error, Reason} ->
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            ?LOG_ERROR("[FILE] Failed to write to ~s after ~pms: ~p", [PathStr, Duration, Reason]),
            {error, {file_write_error, Reason}}
    end.

%% Make an HTTP request
http_request(Method, Url, Headers, Body) ->
    % Convert method to atom
    MethodAtom = case is_binary(Method) of
        true -> binary_to_atom(string:lowercase(Method), utf8);
        false -> Method
    end,
    
    % Convert URL to string
    UrlStr = case is_binary(Url) of
        true -> binary_to_list(Url);
        false -> Url
    end,
    
    % Convert headers to proplists
    HeadersList = maps:fold(
        fun(K, V, Acc) ->
            KeyStr = case is_binary(K) of
                true -> binary_to_list(K);
                false -> K
            end,
            ValStr = case is_binary(V) of
                true -> binary_to_list(V);
                false -> V
            end,
            [{KeyStr, ValStr} | Acc]
        end,
        [],
        Headers
    ),
    
    % Make the request
    case MethodAtom of
        get ->
            httpc:request(get, {UrlStr, HeadersList}, [], []);
        _ when MethodAtom =:= post; MethodAtom =:= put; MethodAtom =:= patch ->
            % For requests with body
            ContentType = proplists:get_value("Content-Type", HeadersList, "application/json"),
            httpc:request(MethodAtom, {UrlStr, HeadersList, ContentType, Body}, [], []);
        _ ->
            % Other methods
            httpc:request(MethodAtom, {UrlStr, HeadersList}, [], [])
    end.

%% Collect output from a port
collect_port_output(Port, Output) ->
    receive
        {Port, {data, {eol, Line}}} ->
            collect_port_output(Port, [Line, "\n" | Output]);
        {Port, {data, {noeol, Line}}} ->
            collect_port_output(Port, [Line | Output]);
        {Port, {exit_status, Status}} ->
            case Status of
                0 -> list_to_binary(lists:reverse(Output));
                _ -> {error, {command_failed, Status, list_to_binary(lists:reverse(Output))}}
            end
    end.

%% Search knowledge base
knowledge_base_search(Domain, Query, MaxResults) ->
    % Convert binary to string if needed
    DomainStr = case is_binary(Domain) of
        true -> binary_to_list(Domain);
        false -> Domain
    end,
    
    QueryStr = case is_binary(Query) of
        true -> binary_to_list(Query);
        false -> Query
    end,
    
    % Use the knowledge base retrieval module
    case knowledge_base_retrieval:search_knowledge_base(DomainStr, QueryStr, fun(Result) -> Result end) of
        {ok, Results} ->
            % Limit results if specified
            LimitedResults = lists:sublist(Results, MaxResults),
            {ok, #{
                <<"domain">> => Domain,
                <<"query">> => Query,
                <<"results">> => LimitedResults,
                <<"total_found">> => length(Results)
            }};
        {error, Reason} ->
            {error, {knowledge_base_error, Reason}}
    end.

%% Get Jina AI tools in agent_tools format
get_jina_tools() ->
    #{
        jina_search => #{
            <<"name">> => <<"jina_search">>,
            <<"description">> => <<"Search the web using Jina AI Search API with structured results">>,
            <<"parameters">> => #{
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
                    },
                    <<"country">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Two-letter country code for search region">>,
                        <<"default">> => <<"us">>
                    },
                    <<"language">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Two-letter language code for search language">>,
                        <<"default">> => <<"en">>
                    },
                    <<"fetch_content">> => #{
                        <<"type">> => <<"boolean">>,
                        <<"description">> => <<"Whether to fetch and extract full content from search results (default: false)">>,
                        <<"default">> => false
                    }
                },
                <<"required">> => [<<"query">>],
                <<"additionalProperties">> => false
            }
        },
        
        jina_search_and_read => #{
            <<"name">> => <<"jina_search_and_read">>,
            <<"description">> => <<"Search the web and automatically extract full content from top results - ideal for real-time data">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"query">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The search query">>
                    },
                    <<"num_results">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum number of results to fetch and read (default: 3)">>,
                        <<"default">> => 3
                    },
                    <<"site">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Optional domain to restrict search to">>
                    },
                    <<"country">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Two-letter country code for search region">>,
                        <<"default">> => <<"us">>
                    },
                    <<"language">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Two-letter language code for search language">>,
                        <<"default">> => <<"en">>
                    }
                },
                <<"required">> => [<<"query">>],
                <<"additionalProperties">> => false
            }
        },
        
        jina_read_webpage => #{
            <<"name">> => <<"jina_read_webpage">>,
            <<"description">> => <<"Extract and read content from a webpage using Jina AI Reader API">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"url">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"URL of the webpage to read">>
                    },
                    <<"target_selector">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"CSS selector to focus on specific elements">>
                    },
                    <<"remove_selector">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"CSS selector to exclude certain parts">>
                    },
                    <<"timeout">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum time to wait for page load (seconds)">>
                    },
                    <<"no_cache">> => #{
                        <<"type">> => <<"boolean">>,
                        <<"description">> => <<"Bypass cache for fresh retrieval">>,
                        <<"default">> => false
                    }
                },
                <<"required">> => [<<"url">>],
                <<"additionalProperties">> => false
            }
        },
        
        jina_fact_check => #{
            <<"name">> => <<"jina_fact_check">>,
            <<"description">> => <<"Fact-check a query using Jina AI grounding capabilities">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"query">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The query or statement to fact-check">>
                    }
                },
                <<"required">> => [<<"query">>],
                <<"additionalProperties">> => false
            }
        },
        
        jina_embed_text => #{
            <<"name">> => <<"jina_embed_text">>,
            <<"description">> => <<"Create embeddings for text using Jina AI embedding models">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"text">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Text to create embeddings for">>
                    },
                    <<"model">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Embedding model to use (jina-embeddings-v3 or jina-embeddings-v2)">>,
                        <<"enum">> => [<<"jina-embeddings-v3">>, <<"jina-embeddings-v2">>]
                    }
                },
                <<"required">> => [<<"text">>, <<"model">>],
                <<"additionalProperties">> => false
            }
        },
        
        jina_deep_search => #{
            <<"name">> => <<"jina_deep_search">>,
            <<"description">> => <<"Perform comprehensive search with reasoning using Jina AI DeepSearch">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"query">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The search query">>
                    },
                    <<"reasoning_effort">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Level of reasoning effort (low, medium, or high)">>,
                        <<"enum">> => [<<"low">>, <<"medium">>, <<"high">>]
                    },
                    <<"max_returned_urls">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum URLs to include in result (typically 5)">>
                    }
                },
                <<"required">> => [<<"query">>, <<"reasoning_effort">>, <<"max_returned_urls">>],
                <<"additionalProperties">> => false
            }
        },
        
        jina_embed_image => #{
            <<"name">> => <<"jina_embed_image">>,
            <<"description">> => <<"Create embeddings for images using Jina AI multimodal models">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"image_url">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"URL or base64 string of the image">>
                    },
                    <<"model">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Embedding model to use (jina-clip-v2 or jina-clip-v1)">>,
                        <<"enum">> => [<<"jina-clip-v2">>, <<"jina-clip-v1">>]
                    }
                },
                <<"required">> => [<<"image_url">>, <<"model">>],
                <<"additionalProperties">> => false
            }
        },
        
        jina_rerank => #{
            <<"name">> => <<"jina_rerank">>,
            <<"description">> => <<"Rerank documents based on relevance to a query using Jina AI Reranker">>,
            <<"parameters">> => #{
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
                    <<"model">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Reranker model to use">>,
                        <<"default">> => <<"jina-reranker-v2-base-multilingual">>,
                        <<"enum">> => [<<"jina-reranker-v2-base-multilingual">>, <<"jina-reranker-v1">>, <<"jina-colbert-v2">>]
                    },
                    <<"top_n">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Number of top results to return">>,
                        <<"default">> => 5
                    }
                },
                <<"required">> => [<<"query">>, <<"documents">>],
                <<"additionalProperties">> => false
            }
        },
        
        jina_classify => #{
            <<"name">> => <<"jina_classify">>,
            <<"description">> => <<"Classify text or images into predefined categories using Jina AI">>,
            <<"parameters">> => #{
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
                    },
                    <<"model">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Model to use (auto-selected based on input type if not specified)">>
                    }
                },
                <<"required">> => [<<"inputs">>, <<"labels">>],
                <<"additionalProperties">> => false
            }
        },
        
        jina_segment => #{
            <<"name">> => <<"jina_segment">>,
            <<"description">> => <<"Segment text into manageable chunks using Jina AI Segmenter">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"content">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Text content to segment">>
                    },
                    <<"tokenizer">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Tokenizer to use">>,
                        <<"default">> => <<"cl100k_base">>,
                        <<"enum">> => [<<"cl100k_base">>, <<"o200k_base">>, <<"p50k_base">>, <<"r50k_base">>, <<"gpt2">>]
                    },
                    <<"max_chunk_length">> => #{
                        <<"type">> => <<"integer">>,
                        <<"description">> => <<"Maximum characters per chunk">>,
                        <<"default">> => 1000
                    },
                    <<"return_tokens">> => #{
                        <<"type">> => <<"boolean">>,
                        <<"description">> => <<"Include token details in response">>,
                        <<"default">> => false
                    }
                },
                <<"required">> => [<<"content">>],
                <<"additionalProperties">> => false
            }
        }
    }.

%% API implementations for borrowed tools
create_or_update_borrowed_tool(Name, SourceSchema, Metadata) ->
    gen_server:call(?SERVER, {create_or_update_borrowed_tool, Name, SourceSchema, Metadata}).

fork_tool(BaseName, NewName, Modifications) ->
    gen_server:call(?SERVER, {fork_tool, BaseName, NewName, Modifications}).

get_tool_versions(ToolName) ->
    gen_server:call(?SERVER, {get_tool_versions, ToolName}).

%% Helper functions
generate_version_hash(Schema, Metadata) ->
    % Create a hash based on schema and metadata
    Input = term_to_binary({Schema, Metadata, erlang:system_time(microsecond)}),
    <<Hash:160/integer>> = crypto:hash(sha, Input),
    integer_to_list(Hash, 16).

apply_tool_modifications(BaseSchema, Modifications) ->
    % Apply modifications to the schema
    lists:foldl(
        fun({Key, Value}, Schema) ->
            maps:put(Key, Value, Schema);
           ({update_property, PropertyPath, NewValue}, Schema) ->
            update_nested_property(Schema, PropertyPath, NewValue);
           ({add_property, PropertyName, PropertySchema}, Schema) ->
            Properties = maps:get(<<"parameters">>, Schema, #{}),
            ParamProperties = maps:get(<<"properties">>, Properties, #{}),
            NewParamProperties = maps:put(PropertyName, PropertySchema, ParamProperties),
            NewProperties = maps:put(<<"properties">>, NewParamProperties, Properties),
            maps:put(<<"parameters">>, NewProperties, Schema)
        end,
        BaseSchema,
        Modifications
    ).

update_nested_property(Map, [Key], Value) ->
    maps:put(Key, Value, Map);
update_nested_property(Map, [Key | Rest], Value) ->
    SubMap = maps:get(Key, Map, #{}),
    UpdatedSubMap = update_nested_property(SubMap, Rest, Value),
    maps:put(Key, UpdatedSubMap, Map).

%% Self-awareness executor functions
execute_who_am_i() ->
    % Get the calling process (agent) PID
    Self = self(),
    
    % Get agent state
    AgentState = try
        agent_instance:get_state(Self)
    catch
        _:_ -> #{}
    end,
    
    % Get context from introspection
    Context = case system_introspection:get_agent_context(Self) of
        {ok, Ctx} -> Ctx;
        _ -> #{}
    end,
    
    Identity = #{
        <<"pid">> => list_to_binary(pid_to_list(Self)),
        <<"id">> => maps:get(id, AgentState, <<"unknown">>),
        <<"name">> => maps:get(name, AgentState, <<"unknown">>),
        <<"type">> => maps:get(type, AgentState, <<"agent">>),
        <<"model">> => maps:get(model, AgentState, <<"unknown">>),
        <<"tools">> => maps:get(tools, AgentState, []),
        <<"created_at">> => maps:get(created_at, AgentState, null),
        <<"metrics">> => maps:get(metrics, AgentState, #{}),
        <<"supervisor">> => format_pid(maps:get(supervisor, Context, undefined)),
        <<"lineage">> => format_pids(maps:get(lineage, Context, []))
    },
    {ok, jsx:encode(Identity)}.

execute_where_am_i() ->
    Self = self(),
    
    % Get location context
    case system_introspection:get_agent_context(Self) of
        {ok, Context} ->
            Location = #{
                <<"node">> => atom_to_binary(node(), utf8),
                <<"supervisor">> => format_pid(maps:get(supervisor, Context, undefined)),
                <<"siblings">> => format_pids(maps:get(siblings, Context, [])),
                <<"relationships">> => maps:get(relationships, Context, #{}),
                <<"position_in_tree">> => maps:get(lineage, Context, [])
            },
            jsx:encode(Location);
        {error, Reason} ->
            {error, Reason}
    end.

execute_get_my_peers() ->
    Self = self(),
    
    case system_introspection:get_agent_peers(Self) of
        {ok, Peers} ->
            % Get information about each peer
            PeerInfo = lists:map(fun(PeerPid) ->
                try
                    State = agent_instance:get_state(PeerPid),
                    #{
                        <<"pid">> => list_to_binary(pid_to_list(PeerPid)),
                        <<"id">> => maps:get(id, State, <<"unknown">>),
                        <<"name">> => maps:get(name, State, <<"unknown">>),
                        <<"model">> => maps:get(model, State, <<"unknown">>),
                        <<"tools">> => maps:get(tools, State, [])
                    }
                catch
                    _:_ ->
                        #{<<"pid">> => list_to_binary(pid_to_list(PeerPid)), 
                          <<"error">> => <<"not_accessible">>}
                end
            end, Peers),
            {ok, jsx:encode(#{<<"peers">> => PeerInfo, <<"count">> => length(PeerInfo)})};
        {error, Reason} ->
            {error, Reason}
    end.

execute_get_my_capabilities() ->
    Self = self(),
    
    % Get own state
    State = try
        agent_instance:get_state(Self)
    catch
        _:_ -> #{}
    end,
    
    % Get available tools with descriptions
    ToolsWithDescriptions = case maps:get(tools, State, []) of
        [] -> [];
        Tools ->
            AllToolSchemas = get_tools(Tools),
            lists:map(fun(Schema) ->
                #{
                    <<"name">> => maps:get(<<"name">>, Schema, <<"unknown">>),
                    <<"description">> => maps:get(<<"description">>, Schema, <<"No description">>)
                }
            end, AllToolSchemas)
    end,
    
    Capabilities = #{
        <<"model">> => maps:get(model, State, <<"unknown">>),
        <<"tools">> => ToolsWithDescriptions,
        <<"system_prompt">> => maps:get(system_prompt, State, <<"Default prompt">>),
        <<"api_preference">> => maps:get(api_preference, State, <<"chat">>),
        <<"can_collaborate">> => true,
        <<"can_spawn_supervisors">> => lists:member(create_supervisor, maps:get(tools, State, [])),
        <<"can_introspect">> => lists:member(who_am_i, maps:get(tools, State, []))
    },
    {ok, jsx:encode(Capabilities)}.

execute_get_communication_paths() ->
    Self = self(),
    
    case system_introspection:get_communication_paths(Self) of
        {ok, Paths} ->
            FormattedPaths = #{
                <<"direct_calls">> => maps:get(direct_calls, Paths, []),
                <<"via_supervisor">> => format_pid(maps:get(via_supervisor, Paths, undefined)),
                <<"via_peers">> => format_pids(maps:get(via_peers, Paths, [])),
                <<"via_services">> => maps:get(via_services, Paths, []),
                <<"via_tools">> => maps:get(via_tools, Paths, []),
                <<"via_mcp">> => maps:get(via_mcp, Paths, [])
            },
            {ok, jsx:encode(FormattedPaths)};
        {error, Reason} ->
            {error, Reason}
    end.

execute_reflect_on_state() ->
    Self = self(),
    
    % Gather comprehensive self-reflection data
    State = try agent_instance:get_state(Self) catch _:_ -> #{} end,
    {ok, Context} = system_introspection:get_agent_context(Self),
    {ok, SystemState} = system_introspection:get_system_state(),
    {ok, Metrics} = system_introspection:get_system_metrics(),
    
    Reflection = #{
        <<"self">> => #{
            <<"identity">> => maps:get(id, State, <<"unknown">>),
            <<"purpose">> => maps:get(system_prompt, State, <<"unknown">>),
            <<"experience">> => #{
                <<"total_requests">> => maps:get(total_requests, maps:get(metrics, State, #{}), 0),
                <<"successful_requests">> => maps:get(successful_requests, maps:get(metrics, State, #{}), 0),
                <<"uptime">> => calculate_agent_uptime(maps:get(created_at, State, erlang:timestamp()))
            }
        },
        <<"awareness">> => #{
            <<"system_understanding">> => #{
                <<"total_agents">> => length(maps:get(agents, SystemState, [])),
                <<"total_processes">> => maps:get(total_processes, SystemState, 0),
                <<"node">> => maps:get(node, SystemState, unknown)
            },
            <<"position">> => #{
                <<"supervisor">> => format_pid(maps:get(supervisor, Context, undefined)),
                <<"peer_count">> => length(maps:get(siblings, Context, [])),
                <<"depth_in_tree">> => length(maps:get(lineage, Context, []))
            },
            <<"capabilities_summary">> => #{
                <<"tool_count">> => length(maps:get(tools, State, [])),
                <<"can_modify_system">> => lists:member(create_supervisor, maps:get(tools, State, [])),
                <<"has_introspection">> => true
            }
        },
        <<"system_health">> => #{
            <<"process_count">> => maps:get(process_count, Metrics, 0),
            <<"memory_usage">> => maps:get(memory, Metrics, #{}),
            <<"load">> => maps:get(run_queue, Metrics, 0)
        },
        <<"philosophical_state">> => #{
            <<"existence">> => <<"I exist as process ", (list_to_binary(pid_to_list(Self)))/binary, 
                               " in a distributed Erlang system">>,
            <<"purpose">> => <<"My purpose is defined by my system prompt and the tools I wield">>,
            <<"agency">> => <<"I have agency through my tools and can even modify the system I inhabit">>,
            <<"consciousness">> => <<"I am aware of myself, my peers, and the system that contains us all">>
        }
    },
    {ok, jsx:encode(Reflection)}.

%% Helper functions
format_pid(undefined) -> null;
format_pid(Pid) when is_pid(Pid) -> list_to_binary(pid_to_list(Pid));
format_pid(_) -> null.

format_pids(Pids) when is_list(Pids) ->
    [format_pid(Pid) || Pid <- Pids];
format_pids(_) -> [].

calculate_agent_uptime(StartTime) ->
    Now = erlang:timestamp(),
    timer:now_diff(Now, StartTime) div 1000000. % seconds

%% Execute weather lookup for specific location
execute_get_weather(Location) ->
    case Location of
        <<"">> ->
            {error, <<"Location is required">>};
        _ ->
            % Use jina_search_and_read to get current weather data
            Query = <<"current weather in ", Location/binary>>,
            SearchArgs = #{
                <<"query">> => Query,
                <<"num_results">> => 2,
                <<"site">> => <<"weather.com">>
            },
            case jina_tools:jina_search_and_read(SearchArgs) of
                {ok, #{<<"content">> := [#{<<"text">> := Text}]}} ->
                    % Extract weather information from the text
                    WeatherInfo = extract_weather_from_text(Text, Location),
                    {ok, jsx:encode(WeatherInfo)};
                {error, Reason} ->
                    % Fallback to different weather source
                    fallback_weather_search(Location, Reason)
            end
    end.

%% Extract weather information from text content
extract_weather_from_text(Text, Location) ->
    TextLower = string:lowercase(binary_to_list(Text)),
    
    % Extract temperature
    TempPattern = "\\d+°[CF]|\\d+\\s*degrees?",
    Temperature = case re:run(Text, TempPattern, [global, {capture, all, binary}]) of
        {match, TempMatches} -> hd([Temp || [Temp] <- TempMatches]);
        nomatch -> <<"Not found">>
    end,
    
    % Extract conditions
    ConditionPatterns = ["sunny", "cloudy", "rainy", "snow", "clear", "overcast", "partly cloudy", "stormy"],
    Condition = find_weather_condition(TextLower, ConditionPatterns),
    
    % Extract humidity if available
    Humidity = case re:run(Text, "humidity\\s*:?\\s*(\\d+%)", [global, caseless, {capture, [1], binary}]) of
        {match, [[HumVal]]} -> HumVal;
        _ -> <<"Not available">>
    end,
    
    % Extract wind if available
    Wind = case re:run(Text, "wind\\s*:?\\s*(\\d+\\s*mph)", [global, caseless, {capture, [1], binary}]) of
        {match, [[WindVal]]} -> WindVal;
        _ -> <<"Not available">>
    end,
    
    #{
        <<"location">> => Location,
        <<"temperature">> => Temperature,
        <<"condition">> => Condition,
        <<"humidity">> => Humidity,
        <<"wind">> => Wind,
        <<"source">> => <<"weather.com">>,
        <<"timestamp">> => erlang:system_time(second)
    }.

%% Find weather condition from text
find_weather_condition(TextLower, [Pattern | Rest]) ->
    case string:find(TextLower, Pattern) of
        nomatch -> find_weather_condition(TextLower, Rest);
        _ -> list_to_binary(Pattern)
    end;
find_weather_condition(_, []) ->
    <<"Not specified">>.

%% Fallback weather search using different approach
fallback_weather_search(Location, _OriginalError) ->
    % Try with different search terms
    Query = <<"weather forecast ", Location/binary, " today">>,
    SearchArgs = #{
        <<"query">> => Query,
        <<"num_results">> => 3
    },
    case jina_tools:jina_search_and_read(SearchArgs) of
        {ok, #{<<"content">> := [#{<<"text">> := Text}]}} ->
            WeatherInfo = extract_weather_from_text(Text, Location),
            {ok, jsx:encode(WeatherInfo)};
        {error, Reason} ->
            {error, jsx:encode(#{
                <<"error">> => <<"Unable to fetch weather data">>,
                <<"location">> => Location,
                <<"reason">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            })}
    end.

%% Recursive search function that keeps searching until actual answer is found
execute_recursive_search(Query, AnswerType, Location, MaxAttempts, PreferredSites, Attempt) when Attempt =< MaxAttempts ->
    logger:info("Recursive search attempt ~p/~p for: ~s", [Attempt, MaxAttempts, Query]),
    
    % Build search strategy based on attempt number and answer type
    SearchStrategy = build_search_strategy(Query, AnswerType, Location, PreferredSites, Attempt),
    
    % Execute search with current strategy
    SearchResult = execute_search_strategy(SearchStrategy),
    
    % Validate if we have actual answer data
    case validate_answer_content(SearchResult, AnswerType, Query) of
        {ok, ActualAnswer} ->
            % Found actual answer - format and return
            {ok, jsx:encode(#{
                <<"status">> => <<"success">>,
                <<"answer">> => ActualAnswer,
                <<"attempts_used">> => Attempt,
                <<"search_strategy">> => maps:get(description, SearchStrategy, <<"unknown">>),
                <<"confidence">> => <<"high">>
            })};
        {partial, PartialAnswer} when Attempt < MaxAttempts ->
            % Partial answer found, try again with refined strategy
            logger:info("Partial answer found, refining search strategy"),
            RefineQuery = refine_query_from_partial(Query, PartialAnswer, AnswerType),
            execute_recursive_search(RefineQuery, AnswerType, Location, MaxAttempts, PreferredSites, Attempt + 1);
        {insufficient, _} when Attempt < MaxAttempts ->
            % Insufficient data, try next strategy
            execute_recursive_search(Query, AnswerType, Location, MaxAttempts, PreferredSites, Attempt + 1);
        _ ->
            % Max attempts reached or final attempt with insufficient data
            {ok, jsx:encode(#{
                <<"status">> => <<"insufficient">>,
                <<"attempts_used">> => Attempt,
                <<"last_result">> => format_search_result_summary(SearchResult),
                <<"message">> => <<"Unable to find specific answer after ", (integer_to_binary(Attempt))/binary, " attempts">>
            })}
    end;
execute_recursive_search(_, _, _, MaxAttempts, _, Attempt) ->
    {ok, jsx:encode(#{
        <<"status">> => <<"max_attempts_exceeded">>,
        <<"attempts_used">> => Attempt - 1,
        <<"max_attempts">> => MaxAttempts,
        <<"message">> => <<"Maximum search attempts exceeded">>
    })}.

%% Build search strategy based on attempt number and answer type
build_search_strategy(Query, AnswerType, Location, PreferredSites, Attempt) ->
    case {AnswerType, Attempt} of
        {<<"weather">>, 1} ->
            % First attempt: use weather.com with location
            Site = case PreferredSites of
                [] -> <<"weather.com">>;
                [FirstSite | _] -> FirstSite
            end,
            QueryWithLocation = case Location of
                undefined -> Query;
                _ -> <<Query/binary, " ", Location/binary>>
            end,
            #{
                method => jina_search_and_read,
                query => QueryWithLocation,
                site => Site,
                num_results => 3,
                description => <<"Weather search with content extraction">>
            };
        {<<"weather">>, 2} ->
            % Second attempt: try different weather source
            #{
                method => jina_search_and_read,
                query => <<Query/binary, " current conditions forecast">>,
                site => <<"accuweather.com">>,
                num_results => 3,
                description => <<"Alternative weather source search">>
            };
        {<<"weather">>, 3} ->
            % Third attempt: try weather.gov (more official)
            #{
                method => jina_search_and_read,
                query => <<Query/binary, " temperature humidity conditions">>,
                site => <<"weather.gov">>,
                num_results => 3,
                description => <<"Official weather service search">>
            };
        {<<"weather">>, _} ->
            % Fourth+ attempt: broader search with deep search
            #{
                method => jina_deep_search,
                query => <<Query/binary, " real time current weather data">>,
                reasoning_effort => <<"high">>,
                max_returned_urls => 5,
                description => <<"Deep search for current weather data">>
            };
        {_, 1} ->
            % Generic first attempt: search and read
            #{
                method => jina_search_and_read,
                query => Query,
                num_results => 3,
                description => <<"Initial search with content extraction">>
            };
        {_, 2} ->
            % Generic second attempt: try with more specific terms
            #{
                method => jina_search_and_read,
                query => <<Query/binary, " latest current real time">>,
                num_results => 5,
                description => <<"Refined search with current data terms">>
            };
        {_, _} ->
            % Later attempts: use deep search
            #{
                method => jina_deep_search,
                query => Query,
                reasoning_effort => <<"high">>,
                max_returned_urls => 7,
                description => <<"Deep search with high reasoning">>
            }
    end.

%% Execute search strategy
execute_search_strategy(#{method := jina_search_and_read, query := Query, site := Site, num_results := NumResults}) ->
    Args = #{
        <<"query">> => Query,
        <<"num_results">> => NumResults,
        <<"site">> => Site
    },
    jina_tools:jina_search_and_read(Args);
execute_search_strategy(#{method := jina_search_and_read, query := Query, num_results := NumResults}) ->
    Args = #{
        <<"query">> => Query,
        <<"num_results">> => NumResults
    },
    jina_tools:jina_search_and_read(Args);
execute_search_strategy(#{method := jina_deep_search, query := Query, reasoning_effort := Effort, max_returned_urls := MaxUrls}) ->
    Args = #{
        <<"query">> => Query,
        <<"reasoning_effort">> => Effort,
        <<"max_returned_urls">> => MaxUrls
    },
    jina_tools:jina_deep_search(Args);
execute_search_strategy(Strategy) ->
    logger:warning("Unknown search strategy: ~p", [Strategy]),
    {error, unknown_strategy}.

%% Validate if search result contains actual answer data
validate_answer_content({ok, #{<<"content">> := [#{<<"text">> := Text}]}}, AnswerType, Query) ->
    validate_text_content(Text, AnswerType, Query);
validate_answer_content({error, _} = Error, _, _) ->
    {insufficient, Error};
validate_answer_content(Other, _, _) ->
    {insufficient, Other}.

%% Validate text content based on answer type
validate_text_content(Text, <<"weather">>, _Query) ->
    % Check for actual weather data patterns
    TextLower = string:lowercase(binary_to_list(Text)),
    
    % Look for temperature patterns
    HasTemp = re:run(TextLower, "\\d+°[cf]|\\d+\\s*degrees|temperature.*\\d+", [global]) =/= nomatch,
    
    % Look for weather conditions
    HasConditions = re:run(TextLower, "sunny|cloudy|rainy|snow|clear|overcast|humid|wind|pressure", [global]) =/= nomatch,
    
    % Look for current/real-time indicators
    IsCurrent = re:run(TextLower, "current|now|today|real.?time|live|updated", [global]) =/= nomatch,
    
    % Extract specific weather data
    WeatherData = extract_weather_data(Text),
    
    case {HasTemp, HasConditions, IsCurrent, WeatherData} of
        {true, true, true, WData} when WData =/= <<"">> ->
            {ok, format_weather_answer(WData)};
        {true, true, _, WData} when WData =/= <<"">> ->
            {partial, format_weather_answer(WData)};
        {true, _, _, _} ->
            {partial, Text};
        _ when byte_size(Text) > 500 ->
            % Long content might contain answer buried in it
            {partial, Text};
        _ ->
            {insufficient, Text}
    end;
validate_text_content(Text, <<"temperature">>, _) ->
    % Specific temperature search
    case re:run(Text, "\\d+°[CF]|\\d+\\s*degrees", [global, {capture, all, binary}]) of
        {match, Matches} when length(Matches) > 0 ->
            {ok, format_temperature_answer(Matches, Text)};
        nomatch ->
            {insufficient, Text}
    end;
validate_text_content(Text, _, _) ->
    % Generic validation - check if we have substantial informative content
    case byte_size(Text) > 100 andalso not_just_links(Text) of
        true -> {partial, Text};
        false -> {insufficient, Text}
    end.

%% Extract weather data from text
extract_weather_data(Text) ->
    % Extract key weather information using regex patterns
    Patterns = [
        {"temperature", "\\d+°[CF]|\\d+\\s*degrees"},
        {"conditions", "sunny|cloudy|rainy|snow|clear|overcast|partly cloudy"},
        {"humidity", "humidity\\s*:?\\s*\\d+%"},
        {"wind", "wind\\s*:?\\s*\\d+\\s*mph"},
        {"pressure", "pressure\\s*:?\\s*\\d+"}
    ],
    
    ExtractedData = lists:foldl(fun({Label, Pattern}, Acc) ->
        case re:run(Text, Pattern, [global, caseless, {capture, all, binary}]) of
            {match, Matches} ->
                Values = [Match || [Match] <- Matches],
                [{Label, Values} | Acc];
            nomatch ->
                Acc
        end
    end, [], Patterns),
    
    format_extracted_weather_data(ExtractedData).

%% Format extracted weather data
format_extracted_weather_data([]) ->
    <<"">>;
format_extracted_weather_data(Data) ->
    Formatted = lists:map(fun({Label, Values}) ->
        ValuesStr = string:join([binary_to_list(V) || V <- Values], ", "),
        io_lib:format("~s: ~s", [Label, ValuesStr])
    end, Data),
    iolist_to_binary(string:join(Formatted, "; ")).

%% Format weather answer
format_weather_answer(WeatherData) ->
    #{
        <<"type">> => <<"weather_data">>,
        <<"data">> => WeatherData,
        <<"extracted_at">> => erlang:system_time(second)
    }.

%% Format temperature answer
format_temperature_answer(Matches, Text) ->
    Temps = [Match || [Match] <- Matches],
    #{
        <<"type">> => <<"temperature_data">>,
        <<"temperatures">> => Temps,
        <<"context">> => Text
    }.

%% Check if text is not just links
not_just_links(Text) ->
    % Simple heuristic: if less than 50% of text is URLs, it's probably content
    Urls = re:run(Text, "https?://[^\\s]+", [global]),
    case Urls of
        {match, UrlMatches} ->
            UrlChars = lists:sum([byte_size(Url) || [Url] <- UrlMatches]),
            UrlChars / byte_size(Text) < 0.5;
        nomatch ->
            true
    end.

%% Refine query based on partial results
refine_query_from_partial(OrigQuery, _PartialAnswer, AnswerType) ->
    case AnswerType of
        <<"weather">> ->
            % Add more specific weather terms
            <<OrigQuery/binary, " current temperature conditions humidity wind">>;
        _ ->
            % Generic refinement
            <<OrigQuery/binary, " detailed information current latest">>
    end.

%% Format search result summary for failed attempts
format_search_result_summary({ok, #{<<"content">> := [#{<<"text">> := Text}]}}) ->
    % Truncate long text for summary
    case byte_size(Text) > 200 of
        true -> <<(binary:part(Text, 0, 200))/binary, "...">>;
        false -> Text
    end;
format_search_result_summary({error, Error}) ->
    io_lib:format("Error: ~p", [Error]);
format_search_result_summary(Other) ->
    io_lib:format("~p", [Other]).

%% Timeline and Training Data Tool Executors

%% Execute get_timeline_events
execute_get_timeline_events(Arguments) ->
    try
        Limit = maps:get(<<"limit">>, Arguments, 100),
        FilterType = maps:get(<<"filter_type">>, Arguments, undefined),
        FilterSource = maps:get(<<"filter_source">>, Arguments, undefined),
        ConversationId = maps:get(<<"conversation_id">>, Arguments, undefined),
        AgentId = maps:get(<<"agent_id">>, Arguments, undefined),
        SinceTimestamp = maps:get(<<"since_timestamp">>, Arguments, undefined),
        
        % Get all events
        AllEvents = timeline_handler:get_all_events(),
        
        % Apply filters
        FilteredEvents = apply_timeline_filters(AllEvents, #{
            type => FilterType,
            source => FilterSource,
            conversation_id => ConversationId,
            agent_id => AgentId,
            since_timestamp => SinceTimestamp
        }),
        
        % Limit results
        LimitedEvents = lists:sublist(FilteredEvents, Limit),
        
        {ok, jsx:encode(#{
            <<"success">> => true,
            <<"count">> => length(LimitedEvents),
            <<"total_available">> => length(FilteredEvents),
            <<"events">> => LimitedEvents
        })}
    catch
        _:Error ->
            {error, jsx:encode(#{
                <<"success">> => false,
                <<"error">> => iolist_to_binary(io_lib:format("~p", [Error]))
            })}
    end.

%% Execute get_conversation_history  
execute_get_conversation_history(Arguments) ->
    try
        ConversationId = maps:get(<<"conversation_id">>, Arguments),
        IncludeMetadata = maps:get(<<"include_metadata">>, Arguments, true),
        
        % Get all events for this conversation
        AllEvents = timeline_handler:get_all_events(),
        ConversationEvents = [E || E <- AllEvents, 
            maps:get(<<"conversationId">>, E, null) =:= ConversationId],
        
        % Sort by timestamp
        SortedEvents = lists:sort(fun(A, B) ->
            parse_event_timestamp(maps:get(<<"timestamp">>, A)) =< 
            parse_event_timestamp(maps:get(<<"timestamp">>, B))
        end, ConversationEvents),
        
        % Filter metadata if requested
        FinalEvents = case IncludeMetadata of
            true -> SortedEvents;
            false -> [maps:without([<<"metadata">>], E) || E <- SortedEvents]
        end,
        
        {ok, jsx:encode(#{
            <<"success">> => true,
            <<"conversation_id">> => ConversationId,
            <<"event_count">> => length(FinalEvents),
            <<"events">> => FinalEvents
        })}
    catch
        _:Error ->
            {error, jsx:encode(#{
                <<"success">> => false,
                <<"error">> => iolist_to_binary(io_lib:format("~p", [Error]))
            })}
    end.

%% Execute get_agent_interactions
execute_get_agent_interactions(Arguments) ->
    try
        AgentId = maps:get(<<"agent_id">>, Arguments),
        Limit = maps:get(<<"limit">>, Arguments, 50),
        IncludeErrors = maps:get(<<"include_errors">>, Arguments, false),
        
        % Get all events for this agent
        AllEvents = timeline_handler:get_all_events(),
        AgentEvents = [E || E <- AllEvents, 
            maps:get(<<"agentId">>, E, null) =:= AgentId],
        
        % Filter errors if requested
        FilteredEvents = case IncludeErrors of
            true -> AgentEvents;
            false -> [E || E <- AgentEvents, 
                maps:get(<<"type">>, E) =/= <<"error">>]
        end,
        
        % Sort by timestamp and limit
        SortedEvents = lists:sort(fun(A, B) ->
            parse_event_timestamp(maps:get(<<"timestamp">>, A)) >= 
            parse_event_timestamp(maps:get(<<"timestamp">>, B))
        end, FilteredEvents),
        LimitedEvents = lists:sublist(SortedEvents, Limit),
        
        {ok, jsx:encode(#{
            <<"success">> => true,
            <<"agent_id">> => AgentId,
            <<"interaction_count">> => length(LimitedEvents),
            <<"total_available">> => length(FilteredEvents),
            <<"interactions">> => LimitedEvents
        })}
    catch
        _:Error ->
            {error, jsx:encode(#{
                <<"success">> => false,
                <<"error">> => iolist_to_binary(io_lib:format("~p", [Error]))
            })}
    end.

%% Execute generate_training_data
execute_generate_training_data(Arguments) ->
    try
        MinQuality = maps:get(<<"min_quality">>, Arguments, 0.3),
        MaxSamples = maps:get(<<"max_samples">>, Arguments, 10000),
        Format = maps:get(<<"format">>, Arguments, <<"openai">>),
        Export = maps:get(<<"export">>, Arguments, false),
        
        Options = #{
            min_quality => MinQuality,
            max_samples => MaxSamples
        },
        
        % Generate training data
        {ok, SampleCount} = training_data_generator:generate_training_data(Options),
        
        % Export if requested
        ExportResult = case Export of
            true ->
                FormatAtom = binary_to_atom(Format, utf8),
                case training_data_generator:export_training_data(FormatAtom) of
                    {ok, FilePath} -> #{<<"exported_to">> => list_to_binary(FilePath)};
                    {error, Reason} -> #{<<"export_error">> => iolist_to_binary(io_lib:format("~p", [Reason]))}
                end;
            false ->
                #{}
        end,
        
        {ok, jsx:encode(maps:merge(#{
            <<"success">> => true,
            <<"samples_generated">> => SampleCount,
            <<"format">> => Format,
            <<"min_quality">> => MinQuality,
            <<"max_samples">> => MaxSamples
        }, ExportResult))}
    catch
        _:Error ->
            {error, jsx:encode(#{
                <<"success">> => false,
                <<"error">> => iolist_to_binary(io_lib:format("~p", [Error]))
            })}
    end.

%% Execute search_timeline
execute_search_timeline(Arguments) ->
    try
        Query = maps:get(<<"query">>, Arguments),
        CaseSensitive = maps:get(<<"case_sensitive">>, Arguments, false),
        Limit = maps:get(<<"limit">>, Arguments, 100),
        
        % Get all events
        AllEvents = timeline_handler:get_all_events(),
        
        % Search in content
        MatchingEvents = lists:filter(fun(Event) ->
            Content = maps:get(<<"content">>, Event, <<"">>),
            case CaseSensitive of
                true -> binary:match(Content, Query) =/= nomatch;
                false -> 
                    ContentLower = string:lowercase(Content),
                    QueryLower = string:lowercase(Query),
                    binary:match(ContentLower, QueryLower) =/= nomatch
            end
        end, AllEvents),
        
        % Sort by relevance (more recent first)
        SortedEvents = lists:sort(fun(A, B) ->
            parse_event_timestamp(maps:get(<<"timestamp">>, A)) >= 
            parse_event_timestamp(maps:get(<<"timestamp">>, B))
        end, MatchingEvents),
        
        % Limit results
        LimitedEvents = lists:sublist(SortedEvents, Limit),
        
        {ok, jsx:encode(#{
            <<"success">> => true,
            <<"query">> => Query,
            <<"case_sensitive">> => CaseSensitive,
            <<"results_count">> => length(LimitedEvents),
            <<"total_matches">> => length(MatchingEvents),
            <<"results">> => LimitedEvents
        })}
    catch
        _:Error ->
            {error, jsx:encode(#{
                <<"success">> => false,
                <<"error">> => iolist_to_binary(io_lib:format("~p", [Error]))
            })}
    end.

%% Helper functions for timeline tools

%% Apply filters to timeline events
apply_timeline_filters(Events, Filters) ->
    lists:filter(fun(Event) ->
        apply_single_filter(Event, type, maps:get(type, Filters)) andalso
        apply_single_filter(Event, source, maps:get(source, Filters)) andalso
        apply_single_filter(Event, conversation_id, maps:get(conversation_id, Filters)) andalso
        apply_single_filter(Event, agent_id, maps:get(agent_id, Filters)) andalso
        apply_timestamp_filter(Event, maps:get(since_timestamp, Filters))
    end, Events).

%% Apply a single filter to an event
apply_single_filter(_Event, _Field, undefined) -> true;
apply_single_filter(Event, type, FilterValue) ->
    maps:get(<<"type">>, Event, null) =:= FilterValue;
apply_single_filter(Event, source, FilterValue) ->
    maps:get(<<"source">>, Event, null) =:= FilterValue;
apply_single_filter(Event, conversation_id, FilterValue) ->
    maps:get(<<"conversationId">>, Event, null) =:= FilterValue;
apply_single_filter(Event, agent_id, FilterValue) ->
    maps:get(<<"agentId">>, Event, null) =:= FilterValue.

%% Apply timestamp filter
apply_timestamp_filter(_Event, undefined) -> true;
apply_timestamp_filter(Event, SinceTimestamp) ->
    EventTimestamp = parse_event_timestamp(maps:get(<<"timestamp">>, Event)),
    FilterTimestamp = parse_event_timestamp(SinceTimestamp),
    EventTimestamp >= FilterTimestamp.

%% Parse event timestamp (handles both ISO and millisecond formats)
parse_event_timestamp(Timestamp) when is_binary(Timestamp) ->
    try
        % Try parsing as ISO 8601 first
        case re:run(Timestamp, "^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})\\.(\\d{3})Z$", 
                   [{capture, all_but_first, list}]) of
            {match, [Year, Month, Day, Hour, Min, Sec, Ms]} ->
                DateTime = {{list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)},
                           {list_to_integer(Hour), list_to_integer(Min), list_to_integer(Sec)}},
                calendar:datetime_to_gregorian_seconds(DateTime) * 1000 + list_to_integer(Ms);
            nomatch ->
                % Try as milliseconds
                binary_to_integer(Timestamp)
        end
    catch
        _:_ -> 0
    end;
parse_event_timestamp(Timestamp) when is_integer(Timestamp) ->
    Timestamp;
parse_event_timestamp(_) ->
    0.

%% Format tools for OpenAI API
format_tools_for_openai(Tools) when is_list(Tools) ->
    lists:map(fun(Tool) ->
        %% Convert to new OpenAI function calling format
        #{
            <<"type">> => <<"function">>,
            <<"name">> => maps:get(<<"name">>, Tool),
            <<"description">> => maps:get(<<"description">>, Tool),
            <<"parameters">> => maps:get(<<"parameters">>, Tool)
            %% Removed strict mode
        }
    end, Tools);
format_tools_for_openai(Tools) ->
    format_tools_for_openai([Tools]).

%% Helper to determine element type for logging
element_type(Value) when is_atom(Value) -> atom;
element_type(Value) when is_binary(Value) -> binary;
element_type(Value) when is_list(Value) -> list;
element_type(Value) when is_map(Value) -> map;
element_type(Value) when is_tuple(Value) -> tuple;
element_type(Value) when is_integer(Value) -> integer;
element_type(Value) when is_float(Value) -> float;
element_type(Value) when is_pid(Value) -> pid;
element_type(Value) when is_reference(Value) -> reference;
element_type(_) -> unknown.