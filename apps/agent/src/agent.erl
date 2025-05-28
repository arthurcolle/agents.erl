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
    run_agent/2,
    run_agent/3,
    define_tool/2,
    execute_tool/3,
    list_available_endpoints/0,
    ensure_api_client/1
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
    % Set defaults
    _Model = maps:get(model, Options, ?DEFAULT_MODEL),
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    
    % Create a unique reference for this agent session
    SessionId = make_ref(),
    
    % Spawn a dedicated process for this agent session
    AgentPid = spawn_link(?MODULE, agent_process, [self(), SessionId, Prompt, ToolNames, Options]),
    
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
                Parent ! {agent_error, SessionId, Error}
        end
    catch
        E:R:S ->
            Parent ! {agent_error, SessionId, {E, R, S}}
    end.

%% Get tool schemas for the specified tool names
get_tool_schemas(ToolNames) ->
    agent_tools:get_tools(ToolNames).

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