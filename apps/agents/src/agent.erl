%% agent.erl
%% Advanced Erlang agent with distributed architecture and supervision trees
-module(agent).
-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

%% Supervisor callbacks
-export([
    init/1
]).

%% API
-export([
    start/0,
    start_link/0,
    start_link/1,
    start_link/2,
    stop/0,
    stop_agent/1,
    get_info/1,
    chat/2,
    stream_chat/2,
    stream_chat/3,
    subscribe/2,
    process_task/2,
    run_agent/2,
    run_agent/3,
    run_agent_with_context/3,
    run_agent_with_context/4,
    run_agent_with_mcp/3,
    run_agent_with_mcp/4,
    define_tool/2,
    execute_tool/3,
    list_available_endpoints/0,
    ensure_api_client/1,
    ensure_anthropic_client/0,
    
    %% Dynamic system augmentation
    create_supervisor/2,
    create_supervisor/3,
    add_subsystem/3,
    augment_system/2,
    get_system_tree/0,
    
    %% Deep reflection
    create_reflective_agent/1,
    enable_deep_reflection/1,
    get_reflection_report/1
]).

%% Internal exports for spawned processes
-export([
    agent_process/5,
    tool_executor/5
]).

-define(DEFAULT_MODEL, <<"gpt-4.1-mini">>).
-define(DEFAULT_TIMEOUT, 60000).

%% Application callback
start(_StartType, _StartArgs) ->
    % Ensure openai application is started
    case application:start(openai) of
        ok -> ok;
        {error, {already_started, _}} -> ok;
        {error, _Reason} -> ok
    end,
    
    % Start the agent supervisor
    start_link().

stop(_State) ->
    ok.

%% API Functions

%% Start the application
start() ->
    application:ensure_all_started(agent).

%% Start the agent supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Start the agent supervisor with options or create an agent instance
start_link(Options) ->
    % This is for supervisor initialization
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Options]).

%% Start an agent with ID and options
start_link(AgentId, Options) when is_map(Options) ->
    % Add the agent_id to the options
    OptionsWithId = Options#{agent_id => AgentId},
    agent_supervisor:start_agent(OptionsWithId).

%% Stop the agent application
stop() ->
    application:stop(agent).

%% Stop a specific agent process
stop_agent(Pid) when is_pid(Pid) ->
    exit(Pid, normal),
    ok.

%% Get information about an agent process
get_info(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid) of
        undefined ->
            #{status => dead};
        Info ->
            #{
                status => alive,
                pid => Pid,
                memory => proplists:get_value(memory, Info, 0),
                message_queue_len => proplists:get_value(message_queue_len, Info, 0),
                current_function => proplists:get_value(current_function, Info, undefined)
            }
    end.

%% Run an agent with a prompt and available tools
run_agent(Prompt, ToolNames) ->
    run_agent(Prompt, ToolNames, #{}).

%% Run an agent with a prompt, available tools, and options
run_agent(Prompt, ToolNames, Options) ->
    run_agent_with_context(Prompt, #{}, ToolNames, Options).

%% Run an agent with a prompt, context data, tools, and options
run_agent_with_context(Prompt, Context, ToolNames) ->
    run_agent_with_context(Prompt, Context, ToolNames, #{}).

run_agent_with_context(Prompt, Context, ToolNames, Options) when is_map(Context) ->
    % Set defaults
    _Model = maps:get(model, Options, ?DEFAULT_MODEL),
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    
    % Merge context into the prompt if provided
    FinalPrompt = case maps:size(Context) of
        0 -> Prompt;
        _ -> <<Prompt/binary, "\n\nContext: ", (jsx:encode(Context))/binary>>
    end,
    
    % Create a unique reference for this agent session
    SessionId = make_ref(),
    
    % Spawn a dedicated process for this agent session
    AgentPid = spawn_link(?MODULE, agent_process, [self(), SessionId, FinalPrompt, ToolNames, Options]),
    
    % Register the agent process
    register_agent(SessionId, AgentPid),
    
    % Wait for response or timeout
    receive
        {agent_response, SessionId, Response} ->
            % Clean up registration
            unregister_agent(SessionId),
            Response;
        {agent_error, SessionId, Error} ->
            % Clean up registration
            unregister_agent(SessionId),
            {error, Error}
    after Timeout ->
        % Clean up registration
        unregister_agent(SessionId),
        {error, timeout}
    end.

%% Run an agent with MCP connector support
run_agent_with_mcp(Prompt, AgentId, ToolNames) ->
    run_agent_with_mcp(Prompt, AgentId, ToolNames, #{}).

run_agent_with_mcp(Prompt, AgentId, ToolNames, Options) ->
    % Ensure Anthropic client is started
    ensure_anthropic_client(),
    
    % Get MCP servers for this agent
    case mcp_server_config:get_anthropic_mcp_servers([AgentId]) of
        {ok, McpServers} ->
            % Set defaults for Anthropic Claude
            Model = maps:get(model, Options, <<"claude-sonnet-4-20250514">>),
            MaxTokens = maps:get(max_tokens, Options, 4000),
            Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
            
            % Create messages in Anthropic format
            Messages = [#{
                role => <<"user">>,
                content => Prompt
            }],
            
            % Add system prompt if provided
            SystemPrompt = maps:get(system, Options, undefined),
            RequestOptions = case SystemPrompt of
                undefined -> #{};
                System -> #{system => System}
            end,
            
            % Add tools if provided
            Tools = case ToolNames of
                [] -> RequestOptions;
                _ -> 
                    ToolSchemas = get_tool_schemas(ToolNames),
                    maps:put(tools, ToolSchemas, RequestOptions)
            end,
            
            % Create a unique reference for this agent session
            SessionId = make_ref(),
            
            % Spawn a dedicated process for this MCP agent session
            AgentPid = spawn_link(fun() ->
                mcp_agent_process(self(), SessionId, Model, MaxTokens, Messages, McpServers, Tools, Timeout)
            end),
            
            % Register the agent process
            register_agent(SessionId, AgentPid),
            
            % Wait for response or timeout
            receive
                {agent_response, SessionId, Response} ->
                    % Clean up registration
                    unregister_agent(SessionId),
                    Response;
                {agent_error, SessionId, Error} ->
                    % Clean up registration
                    unregister_agent(SessionId),
                    {error, human_error_formatter:format_error(Error)}
            after Timeout ->
                % Clean up registration
                unregister_agent(SessionId),
                {error, human_error_formatter:format_error(timeout)}
            end;
        {error, Reason} ->
            {error, human_error_formatter:format_mcp_error(Reason)}
    end.

%% MCP agent process
mcp_agent_process(Parent, SessionId, Model, MaxTokens, Messages, McpServers, Options, _Timeout) ->
    try
        % Make request to Anthropic with MCP servers
        Result = anthropic_client:create_message_with_mcp(
            Model, MaxTokens, Messages, McpServers, Options
        ),
        
        case Result of
            {ok, Response} ->
                % Check for tool calls (MCP tool calls)
                case extract_mcp_tool_calls(Response) of
                    [] ->
                        % No tool calls, return the message content
                        Content = extract_anthropic_content(Response),
                        Parent ! {agent_response, SessionId, {ok, Content}};
                    ToolCalls ->
                        % Execute MCP tool calls and continue conversation
                        case execute_mcp_tool_calls(ToolCalls) of
                            {ok, ToolResults} ->
                                % Add tool results to conversation and continue
                                UpdatedMessages = Messages ++ [Response] ++ ToolResults,
                                % Recursive call with updated messages
                                FinalResult = anthropic_client:create_message_with_mcp(
                                    Model, MaxTokens, UpdatedMessages, McpServers, Options
                                ),
                                case FinalResult of
                                    {ok, FinalResponse} ->
                                        FinalContent = extract_anthropic_content(FinalResponse),
                                        Parent ! {agent_response, SessionId, {ok, FinalContent}};
                                    {error, FinalError} ->
                                        Parent ! {agent_error, SessionId, {anthropic_error, FinalError}}
                                end;
                            {error, ToolError} ->
                                Parent ! {agent_error, SessionId, {tool_execution_error, ToolError}}
                        end
                end;
            {error, Error} ->
                Parent ! {agent_error, SessionId, {anthropic_error, Error}}
        end
    catch
        E:R:S ->
            Parent ! {agent_error, SessionId, {E, R, S}}
    end.

%% Extract MCP tool calls from Anthropic response
extract_mcp_tool_calls(Response) ->
    case maps:get(<<"content">>, Response, []) of
        Content when is_list(Content) ->
            lists:filtermap(fun(Block) ->
                case maps:get(<<"type">>, Block, undefined) of
                    <<"mcp_tool_use">> -> {true, Block};
                    _ -> false
                end
            end, Content);
        _ -> []
    end.

%% Extract content from Anthropic response
extract_anthropic_content(Response) ->
    case maps:get(<<"content">>, Response, []) of
        Content when is_list(Content) ->
            TextBlocks = lists:filtermap(fun(Block) ->
                case maps:get(<<"type">>, Block, undefined) of
                    <<"text">> -> 
                        {true, maps:get(<<"text">>, Block, <<>>)};
                    _ -> false
                end
            end, Content),
            case TextBlocks of
                [] -> <<"No text content in response">>;
                _ -> iolist_to_binary(lists:join(<<"\n">>, TextBlocks))
            end;
        _ -> <<"Invalid response format">>
    end.

%% Execute MCP tool calls
execute_mcp_tool_calls(ToolCalls) ->
    try
        Results = lists:map(fun(ToolCall) ->
            ToolUseId = maps:get(<<"id">>, ToolCall),
            ToolName = maps:get(<<"name">>, ToolCall),
            ServerName = maps:get(<<"server_name">>, ToolCall),
            Input = maps:get(<<"input">>, ToolCall, #{}),
            
            % Execute the tool call (this would delegate to the appropriate MCP server)
            case execute_mcp_tool(ServerName, ToolName, Input) of
                {ok, Result} ->
                    #{
                        type => <<"mcp_tool_result">>,
                        tool_use_id => ToolUseId,
                        is_error => false,
                        content => [#{type => <<"text">>, text => Result}]
                    };
                {error, Error} ->
                    HumanError = human_error_formatter:format_tool_error(Error),
                    #{
                        type => <<"mcp_tool_result">>,
                        tool_use_id => ToolUseId,
                        is_error => true,
                        content => [#{type => <<"text">>, text => HumanError}]
                    }
            end
        end, ToolCalls),
        {ok, Results}
    catch
        E:R:S ->
            {error, {E, R, S}}
    end.

%% Execute a single MCP tool (placeholder - would integrate with actual MCP servers)
execute_mcp_tool(ServerName, ToolName, Input) ->
    % For now, return a mock response
    % In a real implementation, this would:
    % 1. Look up the server connection
    % 2. Send the tool call to the MCP server
    % 3. Return the result
    {ok, io_lib:format("Executed ~s on ~s with input: ~p", [ToolName, ServerName, Input])}.

%% Ensure Anthropic client is running
ensure_anthropic_client() ->
    case whereis(anthropic_client) of
        undefined ->
            case anthropic_client:start_link() of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok;
                Error -> throw({anthropic_client_start_error, Error})
            end;
        _Pid -> ok
    end.

%% Define a new tool
define_tool(Name, Schema) ->
    agent_tools:register_tool(Name, Schema).

%% Register a function to execute a tool
execute_tool(Name, ExecutionFn, Options) ->
    agent_tools:register_executor(Name, ExecutionFn, Options).

%% List all available API endpoints
list_available_endpoints() ->
    openai_api_structure:get_api_groups().

%% Ensure an API client is running
ensure_api_client(ApiGroup) ->
    case openai_clients_sup:start_client(ApiGroup, #{}) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        Error -> Error
    end.

%% Supervisor callback
init([]) ->
    % Supervisor flags
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    % Define child specifications
    ChildSpecs = [
        % Tools registry
        #{
            id => agent_tools,
            start => {agent_tools, start_link, [#{}]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [agent_tools]
        },
        
        % Agent registry
        #{
            id => agent_registry,
            start => {agent_registry, start_link, [#{}]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [agent_registry]
        },
        
        % Agent supervisor for managing agent instances
        #{
            id => agent_supervisor,
            start => {agent_supervisor, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [agent_supervisor]
        },
        
        % Agent collaboration manager
        #{
            id => agent_collaboration,
            start => {agent_collaboration, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [agent_collaboration]
        },
        
        % Knowledge base retrieval system
        #{
            id => knowledge_base_retrieval,
            start => {knowledge_base_retrieval, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [knowledge_base_retrieval]
        },
        
        % Model Selection Strategy - Dynamic model assignment and load balancing
        #{
            id => model_selection_strategy,
            start => {model_selection_strategy, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [model_selection_strategy]
        },
        
        % Specialized Agent Factory - Creates domain-specific agents
        #{
            id => specialized_agent_factory,
            start => {specialized_agent_factory, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [specialized_agent_factory]
        },
        
        % Meta-System Supervisor - Top-level abstraction layer
        #{
            id => meta_system_sup,
            start => {meta_system_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [meta_system_sup]
        },
        
        % Pipedream Tool Integration - Integrates Pipedream MCP tools with agent system
        #{
            id => pipedream_tool_integration,
            start => {pipedream_tool_integration, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [pipedream_tool_integration]
        },
        
        % Dynamic Supervisor Manager - Manages dynamic supervisor creation
        #{
            id => dynamic_supervisor_manager,
            start => {dynamic_supervisor_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [dynamic_supervisor_manager]
        },
        
        % System Introspection - Provides system-wide introspection capabilities
        #{
            id => system_introspection,
            start => {system_introspection, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [system_introspection]
        },
        
        % Agent Scheduler Engine - Manages scheduled tasks for agents
        #{
            id => agent_scheduler_engine,
            start => {agent_scheduler_engine, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [agent_scheduler_engine]
        },
        
        % Timeline Event Store - Stores and retrieves timeline events
        #{
            id => timeline_event_store,
            start => {timeline_event_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [timeline_event_store]
        },
        
        % Agent Temporal Awareness - Provides temporal context for agents
        #{
            id => agent_temporal_awareness,
            start => {agent_temporal_awareness, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [agent_temporal_awareness]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.

%% Internal Functions

%% Process that runs a single agent session
agent_process(Parent, SessionId, Prompt, ToolNames, Options) ->
    try
        % Resolve tool schemas
        Tools = get_tool_schemas(ToolNames),
        
        % Get model from options
        Model = maps:get(model, Options, ?DEFAULT_MODEL),
        
        % Ensure the chat client is available
        ensure_api_client(chat),
        
        % Prepare messages
        SystemMessage = maps:get(system_message, Options, <<"Erlang agent assistant">>),
        Messages = [
            #{role => <<"system">>, content => SystemMessage},
            #{role => <<"user">>, content => Prompt}
        ],
        
        % Call the OpenAI API
        Result = case openai_chat:create_chat_completion(Model, Messages, #{
            tools => Tools,
            tool_choice => <<"auto">>
        }) of
            {ok, Response} ->
                % Check if the response has tool calls
                case extract_tool_calls(Response) of
                    [] ->
                        % No tool calls, return the assistant's message content
                        {ok, extract_message_content(Response)};
                    ToolCalls ->
                        % Execute tool calls
                        ToolResults = execute_tool_calls(ToolCalls, SessionId),
                        
                        % Create follow-up messages with tool results
                        FollowUpMessages = create_tool_result_messages(ToolCalls, ToolResults),
                        
                        % Make a follow-up request with tool results
                        handle_follow_up(Model, Messages ++ FollowUpMessages, Options)
                end;
            {error, Reason} ->
                {error, Reason}
        end,
        
        % Send the result back to the parent process
        case Result of
            {ok, FinalResponse} ->
                Parent ! {agent_response, SessionId, FinalResponse};
            {error, Error} ->
                Parent ! {agent_error, SessionId, {anthropic_error, Error}}
        end
    catch
        E:R:S ->
            Parent ! {agent_error, SessionId, {E, R, S}}
    end.

%% Get tool schemas for the specified tool names
get_tool_schemas(ToolNames) ->
    agent_tools:get_enhanced_tools(ToolNames).

%% Extract tool calls from the response
extract_tool_calls(Response) ->
    try
        case maps:get(<<"choices">>, Response, []) of
            [] -> [];
            Choices ->
                FirstChoice = hd(Choices),
                case maps:get(<<"message">>, FirstChoice, #{}) of
                    #{<<"tool_calls">> := ToolCalls} -> ToolCalls;
                    _ -> []
                end
        end
    catch
        _:_ -> []
    end.

%% Extract the assistant's message content
extract_message_content(Response) ->
    try
        case maps:get(<<"choices">>, Response, []) of
            [] -> <<"No response generated">>;
            Choices ->
                FirstChoice = hd(Choices),
                case maps:get(<<"message">>, FirstChoice, #{}) of
                    #{<<"content">> := Content} when Content =/= null -> Content;
                    _ -> <<"No content in response">>
                end
        end
    catch
        _:_ -> <<"Error extracting response content">>
    end.

%% Execute tool calls
execute_tool_calls(ToolCalls, SessionId) ->
    % Spawn a process for each tool call
    ToolResults = lists:map(
        fun(ToolCall) ->
            ToolId = maps:get(<<"id">>, ToolCall, <<"">>),
            ToolName = binary_to_atom(maps:get(<<"name">>, ToolCall, <<"">>), utf8),
            ArgumentsJson = maps:get(<<"arguments">>, ToolCall, <<"{}">>),
            
            % Decode arguments
            Arguments = try jsx:decode(ArgumentsJson, [return_maps]) catch _:_ -> #{} end,
            
            % Execute the tool in a separate process
            _ExecutorPid = spawn_link(?MODULE, tool_executor, [self(), SessionId, ToolId, ToolName, Arguments]),
            
            % Wait for the result
            receive
                {tool_result, SessionId, ToolId, Result} -> {ToolId, Result}
            after 30000 ->
                {ToolId, {error, tool_execution_timeout}}
            end
        end,
        ToolCalls
    ),
    
    ToolResults.

%% Process that executes a tool call
tool_executor(Parent, SessionId, ToolId, ToolName, Arguments) ->
    Result = agent_tools:execute_tool(ToolName, Arguments),
    Parent ! {tool_result, SessionId, ToolId, Result}.

%% Create follow-up messages with tool results
create_tool_result_messages(_ToolCalls, ToolResults) ->
    % Create a message for each tool call with its result
    lists:map(
        fun({ToolId, Result}) ->
            ResultStr = case is_binary(Result) of
                true -> Result;
                false -> jsx:encode(Result)
            end,
            
            #{
                role => <<"tool">>,
                tool_call_id => ToolId,
                content => ResultStr
            }
        end,
        ToolResults
    ).

%% Handle follow-up request
handle_follow_up(Model, AllMessages, _Options) ->
    % Make a follow-up request to OpenAI
    case openai_chat:create_chat_completion(Model, AllMessages, #{}) of
        {ok, Response} ->
            % Check if this response also has tool calls
            case extract_tool_calls(Response) of
                [] ->
                    % No more tool calls, return the final answer
                    {ok, extract_message_content(Response)};
                _MoreToolCalls ->
                    % Too many follow-ups, just return what we have
                    {ok, extract_message_content(Response)}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Register an agent process
register_agent(SessionId, Pid) ->
    agent_registry:register_agent(SessionId, Pid).

%% Unregister an agent process
unregister_agent(SessionId) ->
    agent_registry:unregister_agent(SessionId).

%% Chat with an agent
chat(Pid, Message) when is_pid(Pid) ->
    case catch agent_instance:execute(Pid, #{
        action => <<"chat">>,
        message => ensure_binary(Message)
    }) of
        {ok, Response} ->
            case Response of
                #{message := Msg} -> binary_to_list(Msg);
                Msg when is_binary(Msg) -> binary_to_list(Msg);
                _ -> io_lib:format("~p", [Response])
            end;
        {error, Reason} ->
            io_lib:format("Error: ~p", [Reason]);
        Error ->
            io_lib:format("Unexpected error: ~p", [Error])
    end.

%% Stream chat with an agent
stream_chat(Pid, Message) when is_pid(Pid) ->
    stream_chat(Pid, Message, self()).

%% Stream chat with an agent, sending updates to a specific subscriber
stream_chat(Pid, Message, SubscriberPid) when is_pid(Pid), is_pid(SubscriberPid) ->
    % Use the new streaming execute function
    agent_instance:stream_execute(Pid, #{
        action => <<"chat">>,
        message => ensure_binary(Message)
    }, SubscriberPid),
    ok.

%% Subscribe to agent events
subscribe(Pid, SubscriberPid) when is_pid(Pid), is_pid(SubscriberPid) ->
    % For now, just acknowledge the subscription
    % In a real implementation, this would register the subscriber
    ok.

%% Process a task with an agent
process_task(Pid, Task) when is_pid(Pid) ->
    case catch agent_instance:execute(Pid, #{
        action => <<"process">>,
        task => ensure_binary(Task)
    }) of
        {ok, Response} ->
            Response;
        {error, Reason} ->
            {error, Reason};
        Error ->
            {error, Error}
    end.

%% Helper functions
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) -> list_to_binary(io_lib:format("~p", [Value])).


%%%===================================================================
%%% Dynamic System Augmentation Functions
%%%===================================================================

%% @doc Create a new supervisor with default strategy
create_supervisor(Name, Children) ->
    create_supervisor(Name, one_for_one, Children).

%% @doc Create a new supervisor with specified strategy
create_supervisor(Name, Strategy, Children) when is_atom(Name), is_atom(Strategy) ->
    case agent_dynamic_system:create_supervisor(Name, Strategy, Children) of
        {ok, Pid} ->
            colored_logger:quantum(superposition, io_lib:format("🎆 [AGENT] Created supervisor ~p with PID ~p", [Name, Pid])),
            {ok, Pid};
        Error ->
            colored_logger:fire(inferno, io_lib:format("☠️ [AGENT] Failed to create supervisor ~p: ~p", [Name, Error])),
            Error
    end.

%% @doc Add a subsystem (supervisor + children) to the system
add_subsystem(Name, Components, Options) ->
    Strategy = maps:get(strategy, Options, one_for_one),
    Children = build_child_specs(Components),
    
    case create_supervisor(Name, Strategy, Children) of
        {ok, SupPid} ->
            %% Register subsystem for agent access
            register_subsystem(Name, SupPid, Components),
            {ok, SupPid};
        Error ->
            Error
    end.

%% @doc General system augmentation function for agents
augment_system(_AgentPid, Augmentation) ->
    case Augmentation of
        #{type := supervisor} = Spec ->
            Name = maps:get(name, Spec, generate_supervisor_name()),
            Strategy = maps:get(strategy, Spec, one_for_one),
            Children = maps:get(children, Spec, []),
            create_supervisor(Name, Strategy, Children);
            
        #{type := subsystem} = Spec ->
            Name = maps:get(name, Spec, generate_subsystem_name()),
            Components = maps:get(components, Spec, []),
            Options = maps:get(options, Spec, #{}),
            add_subsystem(Name, Components, Options);
            
        #{type := worker, supervisor := SupName} = Spec ->
            WorkerSpec = build_worker_spec(Spec),
            agent_dynamic_system:add_worker(SupName, maps:get(id, WorkerSpec), WorkerSpec);
            
        _ ->
            {error, invalid_augmentation_spec}
    end.

%% @doc Get the current system supervision tree
get_system_tree() ->
    %% Get all supervisors
    Supervisors = agent_dynamic_system:list_supervisors(),
    
    %% Build complete tree
    Tree = lists:map(fun({Name, _Pid}) ->
        case agent_dynamic_system:get_supervisor_tree(Name) of
            {ok, Children} ->
                #{name => Name, children => Children};
            _ ->
                #{name => Name, children => []}
        end
    end, Supervisors),
    
    %% Include the main application supervisors
    MainSupervisors = [
        agent_supervisor,
        agent_web_sup,
        openai_sup
    ],
    
    AllTrees = Tree ++ lists:filtermap(fun(SupName) ->
        case whereis(SupName) of
            undefined -> false;
            Pid ->
                Children = supervisor:which_children(Pid),
                {true, #{name => SupName, children => format_children(Children)}}
        end
    end, MainSupervisors),
    
    {ok, AllTrees}.

%%%===================================================================
%%% Internal functions for dynamic system augmentation
%%%===================================================================

build_child_specs(Components) ->
    lists:map(fun(Component) ->
        case Component of
            #{type := worker} = W ->
                #{
                    id => maps:get(id, W),
                    start => maps:get(start, W),
                    restart => maps:get(restart, W, permanent),
                    shutdown => maps:get(shutdown, W, 5000),
                    type => worker
                };
            #{type := supervisor} = S ->
                #{
                    id => maps:get(id, S),
                    start => {supervisor, start_link, [
                        {local, maps:get(id, S)},
                        ?MODULE,
                        maps:get(children, S, [])
                    ]},
                    restart => permanent,
                    shutdown => infinity,
                    type => supervisor
                }
        end
    end, Components).

build_worker_spec(#{module := Module} = Spec) ->
    #{
        id => maps:get(id, Spec, Module),
        start => {Module, maps:get(function, Spec, start_link), maps:get(args, Spec, [])},
        restart => maps:get(restart, Spec, permanent),
        shutdown => maps:get(shutdown, Spec, 5000),
        type => worker
    }.

register_subsystem(Name, Pid, Components) ->
    %% Could store in ETS or process dictionary for tracking
    put({subsystem, Name}, {Pid, Components}).

generate_supervisor_name() ->
    list_to_atom("dynamic_sup_" ++ integer_to_list(erlang:unique_integer([positive]))).

generate_subsystem_name() ->
    list_to_atom("dynamic_subsystem_" ++ integer_to_list(erlang:unique_integer([positive]))).

format_children(Children) ->
    lists:map(fun({Id, Pid, Type, _Modules}) ->
        #{id => Id, pid => Pid, type => Type}
    end, Children).

%%%===================================================================
%%% Deep Reflection Functions
%%%===================================================================

%% @doc Create an agent with deep reflection capabilities
create_reflective_agent(Config) ->
    %% Start a reflective agent instance
    ChildSpec = #{
        id => make_ref(),
        start => {reflective_agent_instance, start_link, [Config]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [reflective_agent_instance]
    },
    
    case supervisor:start_child(agent_supervisor, ChildSpec) of
        {ok, Pid} ->
            colored_logger:cosmic(supernova, io_lib:format("✨ [AGENT] Created reflective agent ~p", [Pid])),
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc Enable deep reflection for an existing agent
enable_deep_reflection(AgentPid) when is_pid(AgentPid) ->
    %% Check if this is already a reflective agent
    case erlang:function_exported(reflective_agent_instance, enable_deep_reflection, 1) of
        true ->
            reflective_agent_instance:enable_deep_reflection(AgentPid);
        false ->
            %% Regular agent - need to wrap it
            {error, not_reflective_agent}
    end.

%% @doc Get reflection report from an agent
get_reflection_report(AgentPid) when is_pid(AgentPid) ->
    try
        reflective_agent_instance:get_reflection_report(AgentPid)
    catch
        exit:{noproc, _} ->
            {error, agent_not_found};
        _:_ ->
            {error, not_reflective_agent}
    end.