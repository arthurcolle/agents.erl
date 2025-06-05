%% agent_instance.erl

%% Colorful logging macros
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format(Msg, Args))).
-define(LOG_WARNING(Msg), colored_logger:alarm(medium, Msg)).
-define(LOG_WARNING(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).
-define(LOG_SUCCESS(Msg), colored_logger:complete(success, Msg)).
-define(LOG_SUCCESS(Msg, Args), colored_logger:complete(success, io_lib:format(Msg, Args))).
-define(LOG_DEBUG(Msg), colored_logger:development(debugging, Msg)).
-define(LOG_DEBUG(Msg, Args), colored_logger:development(debugging, io_lib:format(Msg, Args))).

%% Individual agent process with gen_server behavior
-module(agent_instance).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    execute/2,
    stream_execute/3,
    get_state/1,
    update_config/2,
    self_message/2,
    autonomous_execute/2,
    enable_autonomous_mode/1,
    disable_autonomous_mode/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    id :: binary(),
    name :: binary(),
    type :: atom(),
    model :: binary(),
    tools :: [atom()],
    system_prompt :: binary(),
    conversation_history :: [map()],
    metrics :: map(),
    created_at :: erlang:timestamp(),
    last_activity :: erlang:timestamp(),
    api_preference :: atom(),  % 'chat' or 'responses'
    autonomous_mode :: boolean(), % Enable autonomous multi-turn function calling
    pending_function_calls :: [map()], % Function calls waiting to be processed
    autonomous_context :: map(), % Context for autonomous operations
    max_autonomous_turns :: integer() % Maximum autonomous turns before requiring human input
}).

%% API Functions

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

execute(Pid, Action) ->
    gen_server:call(Pid, {execute, Action}, 60000).

stream_execute(Pid, Action, StreamPid) ->
    gen_server:cast(Pid, {stream_execute, Action, StreamPid}).

get_state(Pid) ->
    gen_server:call(Pid, get_state).

update_config(Pid, Config) ->
    gen_server:call(Pid, {update_config, Config}).

%% Send a message to self for autonomous operation
self_message(Pid, Message) ->
    gen_server:cast(Pid, {self_message, Message}).

%% Execute with autonomous multi-turn function calling
autonomous_execute(Pid, Action) ->
    gen_server:call(Pid, {autonomous_execute, Action}, 300000). % 5 minute timeout for autonomous chains

%% Enable autonomous mode
enable_autonomous_mode(Pid) ->
    gen_server:call(Pid, enable_autonomous_mode).

%% Disable autonomous mode
disable_autonomous_mode(Pid) ->
    gen_server:call(Pid, disable_autonomous_mode).

%% gen_server callbacks

init(Config) ->
    process_flag(trap_exit, true),
    
    % Use model selection strategy to choose optimal model
    AgentType = maps:get(type, Config, ai),
    SelectedModel = case model_selection_strategy:select_model_for_agent(AgentType, Config) of
        {ok, Model} -> Model;
        _ -> maps:get(model, Config, <<"gpt-4.1">>)  % Fallback to specified or default
    end,
    
    State = #state{
        id = maps:get(id, Config, generate_id()),
        name = maps:get(name, Config, <<"Unnamed Agent">>),
        type = AgentType,
        model = SelectedModel,
        tools = maps:get(tools, Config, []),
        system_prompt = maps:get(system_prompt, Config, <<"You are a helpful AI assistant.">>),
        conversation_history = [],
        metrics = #{
            total_requests => 0,
            successful_requests => 0,
            failed_requests => 0,
            total_tokens => 0
        },
        created_at = erlang:timestamp(),
        last_activity = erlang:timestamp(),
        api_preference = maps:get(api_preference, Config, responses),  % Default to Responses API
        autonomous_mode = maps:get(autonomous_mode, Config, false),
        pending_function_calls = [],
        autonomous_context = #{
            turn_count => 0,
            original_message => <<>>,
            function_call_chain => []
        },
        max_autonomous_turns = maps:get(max_autonomous_turns, Config, 10)
    },
    
    % Register model usage with the strategy
    model_selection_strategy:register_model_usage(self(), SelectedModel),
    
    {ok, State}.

handle_call({update_model, NewModel}, _From, State) ->
    case model_registry:validate_model(NewModel) of
        true ->
            {reply, ok, State#state{model = NewModel}};
        false ->
            {reply, {error, invalid_model}, State}
    end;

handle_call({execute, #{action := <<"chat">>, message := Message}}, _From, State) ->
    NewState = State#state{last_activity = erlang:timestamp()},
    case process_chat(Message, NewState) of
        {ok, Response, UpdatedState} ->
            {reply, {ok, Response}, UpdatedState};
        {error, Reason} ->
            FailedState = update_metrics(NewState, failed_request),
            {reply, {error, Reason}, FailedState}
    end;

handle_call({execute, #{action := <<"process">>, task := Task}}, _From, State) ->
    NewState = State#state{last_activity = erlang:timestamp()},
    case process_task(Task, NewState) of
        {ok, Result, UpdatedState} ->
            {reply, {ok, Result}, UpdatedState};
        {error, Reason} ->
            FailedState = update_metrics(NewState, failed_request),
            {reply, {error, Reason}, FailedState}
    end;

handle_call(get_state, _From, State) ->
    StateMap = #{
        id => State#state.id,
        name => State#state.name,
        type => State#state.type,
        model => State#state.model,
        tools => State#state.tools,
        system_prompt => State#state.system_prompt,
        metrics => State#state.metrics,
        created_at => State#state.created_at,
        last_activity => State#state.last_activity,
        conversation_length => length(State#state.conversation_history),
        api_preference => State#state.api_preference,
        autonomous_mode => State#state.autonomous_mode,
        autonomous_context => State#state.autonomous_context,
        max_autonomous_turns => State#state.max_autonomous_turns,
        pending_function_calls => length(State#state.pending_function_calls)
    },
    {reply, StateMap, State};

handle_call({update_config, Config}, _From, State) ->
    NewState = State#state{
        name = maps:get(name, Config, State#state.name),
        model = maps:get(model, Config, State#state.model),
        tools = maps:get(tools, Config, State#state.tools),
        system_prompt = maps:get(system_prompt, Config, State#state.system_prompt),
        api_preference = maps:get(api_preference, Config, State#state.api_preference)
    },
    {reply, ok, NewState};

handle_call({autonomous_execute, Action}, _From, State) ->
    % Execute with autonomous multi-turn function calling enabled
    NewState = State#state{
        last_activity = erlang:timestamp(),
        autonomous_mode = true,
        autonomous_context = #{
            turn_count => 0,
            original_message => maps:get(message, Action, <<>>),
            function_call_chain => []
        }
    },
    case process_autonomous_action(Action, NewState) of
        {ok, FinalResponse, UpdatedState} ->
            {reply, {ok, FinalResponse}, UpdatedState};
        {error, Reason} ->
            FailedState = update_metrics(NewState, failed_request),
            {reply, {error, Reason}, FailedState}
    end;

handle_call(enable_autonomous_mode, _From, State) ->
    NewState = State#state{autonomous_mode = true},
    {reply, ok, NewState};

handle_call(disable_autonomous_mode, _From, State) ->
    NewState = State#state{
        autonomous_mode = false,
        pending_function_calls = [],
        autonomous_context = #{
            turn_count => 0,
            original_message => <<>>,
            function_call_chain => []
        }
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({self_message, Message}, State) ->
    % Handle self-routed messages for autonomous operation
    case State#state.autonomous_mode of
        true ->
            % Process the self-message autonomously
            Action = #{action => <<"chat">>, message => Message},
            spawn(fun() ->
                try
                    case process_autonomous_action(Action, State) of
                        {ok, _Response, _UpdatedState} ->
                            io:format("Agent ~p completed autonomous self-message~n", [State#state.id]);
                        {error, Reason} ->
                            io:format("Agent ~p autonomous self-message failed: ~p~n", [State#state.id, Reason])
                    end
                catch
                    E:R:S ->
                        io:format("Agent ~p autonomous self-message crashed: ~p:~p~n~p~n", [State#state.id, E, R, S])
                end
            end),
            {noreply, State};
        false ->
            io:format("Agent ~p received self-message but autonomous mode disabled~n", [State#state.id]),
            {noreply, State}
    end;

handle_cast({collaboration_event, Event}, State) ->
    % Handle collaboration events
    case Event of
        {collaboration_created, CollabId, Purpose} ->
            io:format("Agent ~p joined collaboration ~p for ~p~n", 
                     [State#state.id, CollabId, Purpose]);
        {collaboration_message, CollabId, Message} ->
            io:format("Agent ~p received message in collaboration ~p: ~p~n", 
                     [State#state.id, CollabId, Message]);
        {agent_joined_group, GroupId, AgentId} ->
            io:format("Agent ~p joined group ~p~n", [AgentId, GroupId]);
        _ ->
            ok
    end,
    {noreply, State};

handle_cast({stream_execute, #{action := <<"chat">>, message := Message}, StreamPid}, State) ->
    ?LOG_INFO("[AGENT_STREAM] 🌊 Starting streaming execution for agent ~p", [State#state.id]),
    ?LOG_INFO("[AGENT_STREAM] Message: ~p, StreamPid: ~p", [Message, StreamPid]),
    NewState = State#state{last_activity = erlang:timestamp()},
    spawn(fun() ->
        ?LOG_INFO("[AGENT_STREAM] 🚀 Spawned streaming worker, processing chat..."),
        case process_streaming_chat(Message, StreamPid, NewState) of
            {ok, _Response, _UpdatedState} -> 
                ?LOG_SUCCESS("[AGENT_STREAM] ✅ Streaming chat completed successfully");
            {error, Reason} ->
                ?LOG_ERROR("[AGENT_STREAM] ❌ Streaming chat failed: ~p", [Reason]),
                StreamPid ! {stream_error, Reason}
        end
    end),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

generate_id() ->
    %% Generate a simple UUID-like identifier using crypto:strong_rand_bytes
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    ID = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", 
                       [A, B, C, D, E]),
    list_to_binary(ID).

validate_and_get_model(Model) ->
    case model_registry:validate_model(Model) of
        true -> Model;
        false -> model_registry:get_default_model()
    end.

process_chat(Message, #state{type = ai} = State) ->
    ?LOG_INFO("[AGENT_CHAT] Processing message: ~p", [Message]),
    ?LOG_INFO("[AGENT_CHAT] Agent ID: ~p, Tools: ~p", [State#state.id, State#state.tools]),
    ?LOG_INFO("[AGENT_CHAT] Conversation history length: ~p", [length(State#state.conversation_history)]),
    case State#state.api_preference of
        responses ->
            ?LOG_INFO("[AGENT_CHAT] Using Responses API"),
            process_chat_with_responses_api(Message, State);
        chat ->
            ?LOG_INFO("[AGENT_CHAT] Using Chat API"),
            process_chat_with_chat_api(Message, State);
        _ ->
            % Default to responses API for better features
            ?LOG_INFO("[AGENT_CHAT] Using default Responses API"),
            process_chat_with_responses_api(Message, State)
    end;

process_chat(Message, #state{type = simple} = State) ->
    Response = #{
        message => <<"Echo: ", Message/binary>>,
        timestamp => erlang:timestamp()
    },
    UpdatedState = update_metrics(State, successful_request),
    {ok, Response, UpdatedState}.

process_task(_Task, #state{type = simple} = State) ->
    Response = #{
        result => <<"Task processed">>,
        timestamp => erlang:timestamp()
    },
    UpdatedState = update_metrics(State, successful_request),
    {ok, Response, UpdatedState};

process_task(Task, #state{type = ai} = State) ->
    process_chat(Task, State).

%% Chat API implementation (legacy)
process_chat_with_chat_api(Message, State) ->
    Messages = build_messages(Message, State),
    Tools = agent_tools:get_enhanced_tools(State#state.tools),
    
    case agent:ensure_api_client(chat) of
        ok ->
            case openai_chat:create_chat_completion(State#state.model, Messages, #{
                tools => Tools,
                tool_choice => <<"auto">>
            }) of
                {ok, Response} ->
                    handle_chat_response(Response, Messages, State);
                {error, Reason} ->
                    {error, Reason}
            end;
        Error ->
            {error, Error}
    end.

%% Responses API implementation (preferred)
process_chat_with_responses_api(Message, State) ->
    % Store the original user message in the state for multi-turn handling
    UserMessageMap = #{role => <<"user">>, content => Message},
    StateWithMessage = State#state{conversation_history = [UserMessageMap | State#state.conversation_history]},
    Input = build_responses_input(Message, State),
    Tools = agent_tools:get_enhanced_tools(State#state.tools),
    
    ?LOG_INFO("[AGENT_CHAT] Built input: ~p", [Input]),
    ?LOG_INFO("[AGENT_CHAT] Available tools: ~p", [Tools]),
    
    case agent:ensure_api_client(responses) of
        ok ->
            Options = #{
                tools => Tools,
                tool_choice => <<"auto">>,
                instructions => State#state.system_prompt,
                parallel_tool_calls => true,
                temperature => 0.7
            },
            ?LOG_INFO("[AGENT_CHAT] Making API call with options: ~p", [Options]),
            case openai_responses:create_response(Input, State#state.model, Options) of
                {ok, Response} ->
                    ?LOG_INFO("[AGENT_CHAT] Got response: ~p", [Response]),
                    handle_responses_response(Response, Input, StateWithMessage);
                {error, Reason} ->
                    ?LOG_ERROR("[AGENT_CHAT] API error: ~p", [Reason]),
                    {error, Reason}
            end;
        Error ->
            ?LOG_ERROR("[AGENT_CHAT] Client error: ~p", [Error]),
            {error, Error}
    end.

%% Autonomous multi-turn function calling implementation
process_autonomous_action(Action, State) ->
    % Initialize or update autonomous context
    AutonomousContext = State#state.autonomous_context,
    CurrentTurn = maps:get(turn_count, AutonomousContext, 0),
    MaxTurns = State#state.max_autonomous_turns,
    
    case CurrentTurn >= MaxTurns of
        true ->
            % Max turns reached, return with summary
            FinalResponse = create_autonomous_summary(AutonomousContext, State),
            {ok, FinalResponse, reset_autonomous_context(State)};
        false ->
            % Process the action autonomously
            execute_autonomous_turn(Action, State)
    end.

execute_autonomous_turn(#{action := <<"chat">>, message := Message}, State) ->
    % Execute one turn of autonomous operation
    AutonomousContext = State#state.autonomous_context,
    CurrentTurn = maps:get(turn_count, AutonomousContext, 0),
    FunctionChain = maps:get(function_call_chain, AutonomousContext, []),
    
    % Update turn count
    UpdatedContext = AutonomousContext#{turn_count => CurrentTurn + 1},
    TurnState = State#state{autonomous_context = UpdatedContext},
    
    % Process the message normally first
    case process_chat(Message, TurnState) of
        {ok, Response, NewState} ->
            % Check if the response contains function calls
            case extract_function_calls_from_response(Response) of
                [] ->
                    % No function calls, we're done with this autonomous chain
                    FinalResponse = create_autonomous_response(Response, NewState#state.autonomous_context),
                    {ok, FinalResponse, reset_autonomous_context(NewState)};
                FunctionCalls ->
                    % Execute function calls and continue autonomously
                    continue_autonomous_chain(Response, FunctionCalls, NewState)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

continue_autonomous_chain(Response, FunctionCalls, State) ->
    % Execute function calls
    ToolResults = execute_tool_calls(FunctionCalls),
    
    % Update function call chain
    AutonomousContext = State#state.autonomous_context,
    ExistingChain = maps:get(function_call_chain, AutonomousContext, []),
    NewChainEntry = #{
        function_calls => FunctionCalls,
        results => ToolResults,
        timestamp => erlang:system_time(millisecond)
    },
    UpdatedChain = [NewChainEntry | ExistingChain],
    
    % Create follow-up message based on function results
    FollowUpMessage = create_autonomous_followup_message(FunctionCalls, ToolResults, State),
    
    % Update context
    NewContext = AutonomousContext#{function_call_chain => UpdatedChain},
    UpdatedState = State#state{autonomous_context = NewContext},
    
    % Self-route the follow-up message for continued autonomous operation
    FollowUpAction = #{action => <<"chat">>, message => FollowUpMessage},
    
    % Continue the autonomous chain recursively
    process_autonomous_action(FollowUpAction, UpdatedState).

extract_function_calls_from_response(Response) ->
    % Extract function calls from different response formats
    case maps:get(<<"output">>, Response, undefined) of
        undefined ->
            % Try chat format
            case maps:get(<<"choices">>, Response, []) of
                [Choice | _] ->
                    Message = maps:get(<<"message">>, Choice, #{}),
                    maps:get(<<"tool_calls">>, Message, []);
                _ -> []
            end;
        Output when is_list(Output) ->
            % Responses API format - filter for function calls
            lists:filter(fun(Item) ->
                maps:get(<<"type">>, Item, undefined) =:= <<"function_call">>
            end, Output);
        _ -> []
    end.

create_autonomous_followup_message(FunctionCalls, ToolResults, State) ->
    % Create a message that incorporates function call results
    % This allows the agent to reason about the results and decide next steps
    
    ResultSummaries = lists:zip(FunctionCalls, ToolResults),
    ResultTexts = lists:map(fun({FuncCall, {_ToolId, Result}}) ->
        FuncName = get_function_name(FuncCall),
        ResultStr = format_tool_result(Result),
        iolist_to_binary(io_lib:format("Function ~s returned: ~s", [FuncName, ResultStr]))
    end, ResultSummaries),
    
    CombinedResults = binary:list_to_bin(lists:join(<<"\n">>, ResultTexts)),
    
    % Create a reasoning prompt for the agent
    FollowUpPrompt = iolist_to_binary(io_lib:format(
        "Based on these function call results:\n~s\n\n"
        "Please analyze the results and determine if any further actions are needed to complete the original task. "
        "If more function calls are required, make them. If the task is complete, provide a final summary.",
        [CombinedResults]
    )),
    
    FollowUpPrompt.

get_function_name(FuncCall) ->
    case maps:get(<<"type">>, FuncCall, undefined) of
        <<"function">> ->
            FunctionInfo = maps:get(<<"function">>, FuncCall, #{}),
            maps:get(<<"name">>, FunctionInfo, <<"unknown">>);
        <<"function_call">> ->
            maps:get(<<"name">>, FuncCall, <<"unknown">>);
        _ -> <<"unknown">>
    end.

create_autonomous_response(Response, AutonomousContext) ->
    % Create a comprehensive response that includes autonomous context
    TurnCount = maps:get(turn_count, AutonomousContext, 0),
    FunctionChain = maps:get(function_call_chain, AutonomousContext, []),
    
    % Extract main response content
    MainContent = extract_response_content(Response),
    
    % Add autonomous operation summary
    Summary = case length(FunctionChain) of
        0 -> <<>>;
        N -> iolist_to_binary(io_lib:format(
            "\n\n[Autonomous Operation Summary: Completed ~p function call chains in ~p turns]",
            [N, TurnCount]
        ))
    end,
    
    <<MainContent/binary, Summary/binary>>.

create_autonomous_summary(AutonomousContext, _State) ->
    TurnCount = maps:get(turn_count, AutonomousContext),
    FunctionChain = maps:get(function_call_chain, AutonomousContext, []),
    OriginalMessage = maps:get(original_message, AutonomousContext, <<>>),
    
    iolist_to_binary(io_lib:format(
        "Autonomous operation completed after ~p turns.\n"
        "Original request: ~s\n"
        "Function calls executed: ~p\n"
        "Maximum autonomous turn limit reached. Human intervention may be required to continue.",
        [TurnCount, OriginalMessage, length(FunctionChain)]
    )).

extract_response_content(Response) ->
    % Extract the main text content from various response formats
    case maps:get(<<"output_text">>, Response, undefined) of
        undefined ->
            % Try other formats
            case maps:get(<<"choices">>, Response, []) of
                [Choice | _] ->
                    Message = maps:get(<<"message">>, Choice, #{}),
                    maps:get(<<"content">>, Message, <<"No content">>);
                _ ->
                    case maps:get(<<"output">>, Response, []) of
                        [FirstItem | _] when is_map(FirstItem) ->
                            maps:get(<<"text">>, FirstItem, <<"No content">>);
                        _ -> <<"No content">>
                    end
            end;
        Text -> Text
    end.

reset_autonomous_context(State) ->
    State#state{
        autonomous_mode = false,
        autonomous_context = #{
            turn_count => 0,
            original_message => <<>>,
            function_call_chain => []
        }
    }.

%% Process streaming chat with an agent
process_streaming_chat(Message, StreamPid, #state{type = ai} = State) ->
    case State#state.api_preference of
        responses ->
            process_streaming_chat_with_responses_api(Message, StreamPid, State);
        chat ->
            process_streaming_chat_with_chat_api(Message, StreamPid, State);
        _ ->
            % Default to responses API for better features
            process_streaming_chat_with_responses_api(Message, StreamPid, State)
    end;

process_streaming_chat(Message, StreamPid, #state{type = simple} = State) ->
    % For simple agents, just echo with fake streaming
    StreamPid ! {stream_start, #{agent_id => State#state.id}},
    
    ResponseText = <<"Echo: ", Message/binary>>,
    Chunks = split_into_chunks(binary_to_list(ResponseText), 10),
    
    lists:foreach(fun(Chunk) ->
        StreamPid ! {stream_token, list_to_binary(Chunk)},
        timer:sleep(100)
    end, Chunks),
    
    Response = #{
        message => ResponseText,
        timestamp => erlang:timestamp()
    },
    StreamPid ! {stream_complete, Response},
    UpdatedState = update_metrics(State, successful_request),
    {ok, Response, UpdatedState}.

%% Streaming implementation for Chat API (legacy)
process_streaming_chat_with_chat_api(Message, StreamPid, State) ->
    Messages = build_messages(Message, State),
    Tools = agent_tools:get_enhanced_tools(State#state.tools),
    
    case agent:ensure_api_client(chat) of
        ok ->
            % Send stream start notification
            StreamPid ! {stream_start, #{agent_id => State#state.id}},
            
            % Use streaming completion
            case openai_chat:create_streaming_completion(State#state.model, Messages, #{
                tools => Tools,
                tool_choice => <<"auto">>
            }) of
                ok ->
                    % Handle streaming chunks
                    handle_streaming_response(StreamPid, Messages, State);
                {error, Reason} ->
                    {error, Reason}
            end;
        Error ->
            {error, Error}
    end.

%% Streaming implementation for Responses API (preferred)
process_streaming_chat_with_responses_api(Message, StreamPid, State) ->
    Input = build_responses_input(Message, State),
    Tools = agent_tools:get_enhanced_tools(State#state.tools),
    
    case agent:ensure_api_client(responses) of
        ok ->
            % Send stream start notification
            StreamPid ! {stream_start, #{agent_id => State#state.id}},
            
            Options = #{
                tools => Tools,
                tool_choice => <<"auto">>,
                instructions => State#state.system_prompt,
                parallel_tool_calls => true,
                temperature => 0.7,
                stream => true
            },
            
            case openai_responses:create_streaming_response(Input, State#state.model, Options) of
                ok ->
                    % Handle streaming events
                    handle_streaming_response_events(StreamPid, Input, State);
                {error, Reason} ->
                    {error, Reason}
            end;
        Error ->
            {error, Error}
    end.

%% Helper functions for Responses API
build_responses_input(UserMessage, State) ->
    % For Responses API, include conversation history in the input
    History = lists:reverse(State#state.conversation_history),
    AllMessages = History ++ [#{role => <<"user">>, content => UserMessage}],
    format_messages_for_responses(AllMessages).

format_messages_for_responses(Messages) ->
    % Convert messages to Responses API format
    FormattedMessages = lists:map(fun(Message) ->
        case Message of
            % Handle function_call_output messages (tool results)
            #{<<"type">> := <<"function_call_output">>, <<"call_id">> := CallId, <<"output">> := Output} ->
                % These are already in the correct format
                Message;
            % Handle regular messages
            _ ->
                Role = maps:get(role, Message, <<"user">>),
                Content = maps:get(content, Message, <<"">>),
                ToolCalls = maps:get(tool_calls, Message, undefined),
                case Role of
                    <<"system">> -> 
                        % System messages are handled as instructions, not input
                        null;
                    <<"assistant">> when ToolCalls =/= undefined ->
                        % Assistant message with tool calls - need to format properly for Responses API
                        % The tool calls should be in the content as output items
                        #{
                            <<"type">> => <<"message">>,
                            <<"role">> => <<"assistant">>,
                            <<"content">> => ToolCalls  % Tool calls are the content
                        };
                    _ ->
                        #{
                            <<"type">> => <<"message">>,
                            <<"role">> => Role,
                            <<"content">> => [#{
                                <<"type">> => <<"text">>,
                                <<"text">> => Content
                            }]
                        }
                end
        end
    end, Messages),
    % Filter out null entries (system messages)
    lists:filter(fun(Msg) -> Msg =/= null end, FormattedMessages).

handle_responses_response(Response, _Input, State) ->
    %% Track costs for Responses API
    track_cost(State, Response),
    
    ?LOG_INFO("[AGENT_CHAT] Handling response, extracting content..."),
    case extract_responses_content(Response) of
        {tool_calls, ToolCalls} ->
            ?LOG_INFO("[AGENT_CHAT] Found tool calls: ~p", [ToolCalls]),
            % For Responses API, execute tools and continue conversation for multi-turn
            execute_tools_and_respond_responses(ToolCalls, Response, State);
        {content, Content} ->
            ?LOG_INFO("[AGENT_CHAT] Found regular content: ~p", [Content]),
            % Regular text response
            AssistantMsg = #{role => <<"assistant">>, content => Content},
            NewHistory = update_history([AssistantMsg], State#state.conversation_history),
            UpdatedState = State#state{
                conversation_history = NewHistory,
                metrics = update_metrics_map_responses(State#state.metrics, Response)
            },
            {ok, #{message => Content, response => Response}, UpdatedState};
        {error, Reason} ->
            {error, Reason}
    end.

extract_responses_content(Response) ->
    case maps:get(<<"output">>, Response, []) of
        [] -> {error, no_output};
        OutputItems ->
            % Check for function calls first
            FunctionCalls = lists:filter(fun(Item) ->
                maps:get(<<"type">>, Item, undefined) =:= <<"function_call">>
            end, OutputItems),
            
            case FunctionCalls of
                [] ->
                    % No function calls, look for text content
                    case find_message_content(OutputItems) of
                        {text, Text} -> {content, Text};
                        {error, Reason} -> {error, Reason}
                    end;
                _ ->
                    % Has function calls
                    {tool_calls, FunctionCalls}
            end
    end.

find_message_content([]) ->
    {error, no_content};
find_message_content([OutputItem | Rest]) ->
    case maps:get(<<"type">>, OutputItem, undefined) of
        <<"message">> ->
            Content = maps:get(<<"content">>, OutputItem, []),
            case extract_message_content_responses(Content) of
                {text, Text} -> {text, Text};
                {error, _} -> find_message_content(Rest)
            end;
        _ ->
            find_message_content(Rest)
    end.

extract_message_content_responses([]) ->
    {text, <<"No content">>};
extract_message_content_responses([ContentItem | Rest]) ->
    case maps:get(<<"type">>, ContentItem, undefined) of
        <<"output_text">> ->
            Text = maps:get(<<"text">>, ContentItem, <<"">>),
            {text, Text};
        _ ->
            % Try next content item
            extract_message_content_responses(Rest)
    end.

execute_tools_and_return_summary(ToolCalls, _Response, State) ->
    ?LOG_INFO("[AGENT_CHAT] Executing ~p tool calls", [length(ToolCalls)]),
    ToolResults = execute_tool_calls(ToolCalls),
    ?LOG_INFO("[AGENT_CHAT] Tool execution complete, formatting results"),
    
    % Format the tool results into a summary message with error protection
    Summary = try
        format_tool_results_summary(ToolCalls, ToolResults)
    catch
        Error:Reason:Stack ->
            ?LOG_ERROR("[AGENT_CHAT] Error formatting tool results: ~p:~p~n~p", [Error, Reason, Stack]),
            % Fallback to simple result display
            iolist_to_binary([
                <<"I found the following information:\n\n">>,
                format_simple_results(ToolResults)
            ])
    end,
    
    ?LOG_INFO("[AGENT_CHAT] Summary formatted, returning response: ~p", [Summary]),
    
    % Add to conversation history
    AssistantMsg = #{role => <<"assistant">>, content => Summary},
    NewHistory = update_history([AssistantMsg], State#state.conversation_history),
    UpdatedState = State#state{
        conversation_history = NewHistory,
        metrics = update_metrics_map_responses(State#state.metrics, #{})
    },
    
    {ok, #{message => Summary, response => #{}}, UpdatedState}.

format_tool_results_summary(ToolCalls, Results) ->
    case Results of
        [{_CallId, {ok, #{<<"content">> := [#{<<"text">> := Text} | _]}}}] ->
            % Single tool call with text result - analyze and interpret the content
            analyze_and_interpret_search_results(ToolCalls, Text);
        _ ->
            % Multiple tools or complex results - create a summary
            iolist_to_binary([
                <<"I found the following information:\n\n">>,
                format_multiple_results(Results)
            ])
    end.

analyze_and_interpret_search_results(ToolCalls, SearchResults) ->
    try
        % Determine what was being searched for based on the tool call
        SearchQuery = extract_search_query(ToolCalls),
        
        % Check if this is a weather query
        IsWeatherQuery = is_weather_query(SearchQuery, SearchResults),
        
        case IsWeatherQuery of
            true ->
                extract_weather_information(SearchResults);
            false ->
                % For non-weather queries, provide a general interpretation
                interpret_general_search_results(SearchQuery, SearchResults)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("[AGENT_CHAT] Error in search result analysis: ~p:~p", [Error, Reason]),
            % Fallback to simple display
            iolist_to_binary([
                <<"I found search results, but encountered an issue analyzing them. Here's the raw information:\n\n">>,
                case byte_size(SearchResults) > 1000 of
                    true -> <<(binary:part(SearchResults, 0, 1000))/binary, "...">>;
                    false -> SearchResults
                end
            ])
    end.

extract_search_query([]) ->
    <<"unknown">>;
extract_search_query([ToolCall | _]) ->
    case maps:get(<<"function">>, ToolCall, #{}) of
        #{<<"arguments">> := ArgsJson} when is_binary(ArgsJson) ->
            try
                Args = jsx:decode(ArgsJson),
                maps:get(<<"query">>, Args, <<"unknown">>)
            catch
                _:_ -> <<"unknown">>
            end;
        #{<<"arguments">> := Args} when is_map(Args) ->
            maps:get(<<"query">>, Args, <<"unknown">>);
        _ ->
            <<"unknown">>
    end.

is_weather_query(Query, SearchResults) ->
    WeatherKeywords = [<<"weather">>, <<"Weather">>, <<"temperature">>, <<"Temperature">>, 
                      <<"forecast">>, <<"Forecast">>, <<"conditions">>, <<"Conditions">>,
                      <<"climate">>, <<"Climate">>, <<"rain">>, <<"Rain">>, <<"snow">>, <<"Snow">>,
                      <<"sunny">>, <<"Sunny">>, <<"cloudy">>, <<"Cloudy">>, <<"humidity">>, <<"Humidity">>,
                      <<"wind">>, <<"Wind">>, <<"precipitation">>, <<"Precipitation">>],
    
    QueryHasWeather = lists:any(fun(Keyword) ->
        binary:match(Query, Keyword) =/= nomatch
    end, WeatherKeywords),
    
    ResultsHaveWeather = lists:any(fun(Keyword) ->
        binary:match(SearchResults, Keyword) =/= nomatch
    end, WeatherKeywords),
    
    QueryHasWeather orelse ResultsHaveWeather.

extract_weather_information(SearchResults) ->
    % Parse the search results to extract actual weather data
    % The search results contain content from weather sites with current conditions
    
    % Try to extract temperature information
    Temperature = extract_temperature(SearchResults),
    
    % Try to extract conditions
    Conditions = extract_conditions(SearchResults),
    
    % Try to extract location
    Location = extract_location(SearchResults),
    
    % Build a comprehensive weather response
    build_weather_response(Location, Temperature, Conditions, SearchResults).

extract_temperature(Text) ->
    % Look for temperature patterns like "72°F", "22°C", "Temperature: 75°F", etc.
    TempPatterns = [
        <<"([0-9]+)°F">>, <<"([0-9]+)°C">>, <<"([0-9]+) °F">>, <<"([0-9]+) °C">>,
        <<"Temperature: ([0-9]+)°">>, <<"Temp: ([0-9]+)°">>, <<"([0-9]+) degrees">>,
        <<"High: ([0-9]+)°">>, <<"Low: ([0-9]+)°">>
    ],
    
    case extract_pattern_match(Text, TempPatterns) of
        {ok, Match} -> Match;
        nomatch -> <<"unknown">>
    end.

extract_conditions(Text) ->
    % Look for weather condition words
    ConditionPatterns = [
        <<"sunny">>, <<"Sunny">>, <<"cloudy">>, <<"Cloudy">>, <<"rainy">>, <<"Rainy">>,
        <<"snowy">>, <<"Snowy">>, <<"clear">>, <<"Clear">>, <<"overcast">>, <<"Overcast">>,
        <<"partly cloudy">>, <<"Partly Cloudy">>, <<"thunderstorms">>, <<"Thunderstorms">>,
        <<"drizzle">>, <<"Drizzle">>, <<"fog">>, <<"Fog">>, <<"windy">>, <<"Windy">>
    ],
    
    Found = lists:filter(fun(Pattern) ->
        binary:match(Text, Pattern) =/= nomatch
    end, ConditionPatterns),
    
    case Found of
        [] -> <<"unknown">>;
        [First | _] -> First
    end.

extract_location(Text) ->
    % Try to find location information in the search results
    % Look for city names, states, etc. - for now, just return a placeholder
    case binary:match(Text, [<<"Washington">>, <<"DC">>, <<"D.C.">>]) of
        nomatch -> <<"location">>;
        _ -> <<"Washington, DC">>
    end.

extract_pattern_match(_Text, []) ->
    nomatch;
extract_pattern_match(Text, [Pattern | Rest]) ->
    case re:run(Text, Pattern, [global, {capture, [1], binary}]) of
        {match, [[Match]]} -> {ok, Match};
        _ -> extract_pattern_match(Text, Rest)
    end.

build_weather_response(Location, Temperature, Conditions, SearchResults) ->
    % Build a natural language response based on extracted data
    case {Temperature, Conditions} of
        {<<"unknown">>, <<"unknown">>} ->
            % No specific weather data found, but provide what we can from the search
            iolist_to_binary([
                <<"Based on my search, I found weather information for ">>, Location, <<". ">>,
                <<"Here's what I found:\n\n">>,
                extract_relevant_weather_snippets(SearchResults)
            ]);
        {Temp, <<"unknown">>} ->
            iolist_to_binary([
                <<"The current temperature in ">>, Location, <<" is ">>, Temp, <<"°. ">>,
                <<"Here are more details from the search:\n\n">>,
                extract_relevant_weather_snippets(SearchResults)
            ]);
        {<<"unknown">>, Cond} ->
            iolist_to_binary([
                <<"The current conditions in ">>, Location, <<" are ">>, Cond, <<". ">>,
                <<"Here are more details from the search:\n\n">>,
                extract_relevant_weather_snippets(SearchResults)
            ]);
        {Temp, Cond} ->
            iolist_to_binary([
                <<"The current weather in ">>, Location, <<" is ">>, Cond, <<" with a temperature of ">>, Temp, <<"°. ">>,
                <<"Here are more details from the search:\n\n">>,
                extract_relevant_weather_snippets(SearchResults)
            ])
    end.

extract_relevant_weather_snippets(SearchResults) ->
    % Extract the most relevant parts of the search results
    % Split by numbered results and take the first few relevant lines from each
    Lines = binary:split(SearchResults, <<"\n">>, [global]),
    RelevantLines = lists:filter(fun(Line) ->
        % Include lines that contain weather information
        WeatherTerms = [<<"temperature">>, <<"Temperature">>, <<"weather">>, <<"Weather">>,
                       <<"conditions">>, <<"Conditions">>, <<"forecast">>, <<"Forecast">>,
                       <<"°F">>, <<"°C">>, <<"degrees">>, <<"sunny">>, <<"cloudy">>, <<"rain">>],
        lists:any(fun(Term) ->
            binary:match(Line, Term) =/= nomatch
        end, WeatherTerms) andalso byte_size(Line) > 10
    end, Lines),
    
    % Take first 5 relevant lines and format them
    TopLines = lists:sublist(RelevantLines, 5),
    case TopLines of
        [] -> <<"Please check the weather sites directly for current conditions.">>;
        _ -> iolist_to_binary(lists:join(<<"\n">>, TopLines))
    end.

interpret_general_search_results(Query, SearchResults) ->
    % For non-weather queries, provide a general interpretation
    iolist_to_binary([
        <<"Based on my search for '">>, Query, <<"', I found the following information:\n\n">>,
        extract_key_information(SearchResults)
    ]).

extract_key_information(SearchResults) ->
    % Extract the most informative parts of any search results
    Lines = binary:split(SearchResults, <<"\n">>, [global]),
    
    % Filter out very short lines and URLs, keep substantial content
    ContentLines = lists:filter(fun(Line) ->
        byte_size(Line) > 20 andalso 
        binary:match(Line, <<"URL:">>) =:= nomatch andalso
        binary:match(Line, <<"http">>) =:= nomatch
    end, Lines),
    
    % Take first 10 content lines
    TopContent = lists:sublist(ContentLines, 10),
    case TopContent of
        [] -> <<"No detailed information could be extracted from the search results.">>;
        _ -> iolist_to_binary(lists:join(<<"\n">>, TopContent))
    end.

format_multiple_results(Results) ->
    lists:map(fun({_CallId, Result}) ->
        case Result of
            {ok, #{<<"content">> := [#{<<"text">> := Text} | _]}} ->
                [Text, <<"\n\n">>];
            {error, Reason} ->
                [<<"Error: ">>, io_lib:format("~p", [Reason]), <<"\n\n">>];
            _ ->
                [io_lib:format("~p", [Result]), <<"\n\n">>]
        end
    end, Results).

format_simple_results(Results) ->
    % Simple fallback formatting for tool results
    lists:map(fun({_CallId, Result}) ->
        case Result of
            {ok, #{<<"content">> := [#{<<"text">> := Text} | _]}} ->
                % Extract just the first part of text to avoid overwhelming output
                FirstPart = case byte_size(Text) > 500 of
                    true -> <<(binary:part(Text, 0, 500))/binary, "...">>;
                    false -> Text
                end,
                [FirstPart, <<"\n\n">>];
            {ok, Data} when is_map(Data) ->
                [<<"Tool returned data">>, <<"\n\n">>];
            {error, Reason} ->
                [<<"Error: ">>, io_lib:format("~p", [Reason]), <<"\n\n">>];
            _ ->
                [<<"Tool execution completed">>, <<"\n\n">>]
        end
    end, Results).

execute_tools_and_respond_responses(ToolCalls, InitialResponse, State) ->
    % For now, just execute tools and return a summary
    % TODO: Implement proper multi-turn function calling for Responses API
    execute_tools_and_return_summary(ToolCalls, InitialResponse, State).

execute_tools_and_respond_responses(ToolCalls, InitialResponse, State, Depth) ->
    % Check recursion depth to prevent infinite loops
    MaxDepth = 5,  % Maximum number of tool call rounds
    case Depth >= MaxDepth of
        true ->
            ?LOG_WARNING("[MULTI_TURN] Max tool call depth (~p) reached, stopping recursion", [MaxDepth]),
            {ok, #{message => <<"Maximum tool call depth reached. Please refine your request.">>, response => #{}}, State};
        false ->
            ?LOG_INFO("[MULTI_TURN] === Starting multi-turn execution ==="),
            ?LOG_INFO("[MULTI_TURN] Depth: ~p/~p", [Depth, MaxDepth]),
            ?LOG_INFO("[MULTI_TURN] Number of tool calls: ~p", [length(ToolCalls)]),
            ?LOG_INFO("[MULTI_TURN] Tool calls: ~p", [lists:map(fun(TC) ->
                case maps:get(<<"type">>, TC, undefined) of
                    <<"function">> ->
                        FunctionInfo = maps:get(<<"function">>, TC, #{}),
                        #{name => maps:get(<<"name">>, FunctionInfo, undefined),
                          id => maps:get(<<"id">>, TC, undefined)};
                    <<"function_call">> ->
                        #{name => maps:get(<<"name">>, TC, undefined),
                          id => maps:get(<<"call_id">>, TC, undefined)};
                    _ -> TC
                end
            end, ToolCalls)]),
            
            ToolResults = execute_tool_calls(ToolCalls),
            ?LOG_INFO("[MULTI_TURN] Tool execution completed"),
            ?LOG_INFO("[MULTI_TURN] Tool results summary: ~p", [lists:map(fun({Id, Result}) ->
                case Result of
                    {ok, Data} when is_binary(Data) -> 
                        #{id => Id, status => ok, data_length => byte_size(Data)};
                    {ok, Data} -> 
                        #{id => Id, status => ok, data_type => element(1, Data)};
                    {error, Reason} -> 
                        #{id => Id, status => error, reason => Reason};
                    _ -> 
                        #{id => Id, status => unknown}
                end
            end, ToolResults)]),
    
    % Add tool results to conversation history for next request
    ToolMessages = create_tool_messages(ToolCalls, ToolResults),
    ?LOG_INFO("[MULTI_TURN] Created ~p tool messages", [length(ToolMessages)]),
    ?LOG_DEBUG("[MULTI_TURN] Tool messages detail: ~p", [ToolMessages]),
    
    % Build proper history: original messages + assistant's tool call + tool results
    % The conversation history in State contains the original user message
    % Get the latest user message
    OriginalHistory = lists:reverse(State#state.conversation_history),
    
    % For Responses API, we don't need to include the assistant's function call message
    % Just send the original conversation + function call results
    % The function call outputs reference the call_ids from the assistant's response
    
    % Format for the API: original messages + tool results
    MessagesForInput = OriginalHistory ++ ToolMessages,
    ?LOG_INFO("[MULTI_TURN] Conversation history length: ~p messages", [length(OriginalHistory)]),
    ?LOG_INFO("[MULTI_TURN] Adding ~p tool messages to history", [length(ToolMessages)]),
    
    FinalInput = format_messages_for_responses(MessagesForInput),
    ?LOG_INFO("[MULTI_TURN] Formatted input for follow-up request"),
    ?LOG_DEBUG("[MULTI_TURN] Final input structure: ~p", [FinalInput]),
    
    Options = #{
        tools => agent_tools:get_enhanced_tools(State#state.tools),
        tool_choice => <<"auto">>,
        instructions => State#state.system_prompt,
        parallel_tool_calls => true,
        temperature => 0.7
    },
    
    ?LOG_INFO("[MULTI_TURN] Sending follow-up request to model: ~p", [State#state.model]),
    case openai_responses:create_response(FinalInput, State#state.model, Options) of
        {ok, FinalResponse} ->
            ?LOG_INFO("[MULTI_TURN] Received response from model"),
            case extract_responses_content(FinalResponse) of
                {content, Content} ->
                    ?LOG_INFO("[MULTI_TURN] Response contains content (conversation complete)"),
                    % Add the final assistant response to history
                    FinalMessage = #{role => <<"assistant">>, content => Content},
                    NewHistory = update_history([FinalMessage], State#state.conversation_history),
                    UpdatedState = State#state{
                        conversation_history = NewHistory,
                        metrics = update_metrics_map_responses(State#state.metrics, FinalResponse)
                    },
                    ?LOG_INFO("[MULTI_TURN] === Multi-turn execution complete ==="),
                    {ok, #{message => Content, response => FinalResponse}, UpdatedState};
                {tool_calls, MoreToolCalls} ->
                    % Handle recursive tool calls in multi-turn scenarios
                    ?LOG_INFO("[MULTI_TURN] Response contains MORE tool calls!"),
                    ?LOG_INFO("[MULTI_TURN] Detected ~p additional tool calls at depth ~p", [length(MoreToolCalls), Depth + 1]),
                    % For multi-turn, we need to update the history properly
                    TempState = State#state{conversation_history = State#state.conversation_history},
                    execute_tools_and_respond_responses(MoreToolCalls, FinalResponse, TempState, Depth + 1);
                {error, Reason} ->
                    ?LOG_ERROR("[MULTI_TURN] Error extracting response content: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG_ERROR("[MULTI_TURN] API request failed: ~p", [Reason]),
            {error, Reason}
    end
    end.

update_metrics_map_responses(Metrics, Response) ->
    Usage = maps:get(<<"usage">>, Response, #{}),
    TotalTokens = maps:get(<<"total_tokens">>, Usage, 0),
    
    Metrics#{
        total_tokens => maps:get(total_tokens, Metrics, 0) + TotalTokens,
        successful_requests => maps:get(successful_requests, Metrics, 0) + 1,
        total_requests => maps:get(total_requests, Metrics, 0) + 1
    }.

%% Helper functions for Chat API (legacy)
build_messages(UserMessage, State) ->
    SystemMessage = #{role => <<"system">>, content => State#state.system_prompt},
    UserMsg = #{role => <<"user">>, content => UserMessage},
    
    History = lists:reverse(State#state.conversation_history),
    [SystemMessage | History] ++ [UserMsg].

handle_chat_response(Response, Messages, State) ->
    %% Track costs
    track_cost(State, Response),
    
    case extract_tool_calls(Response) of
        [] ->
            Content = extract_message_content(Response),
            AssistantMsg = #{role => <<"assistant">>, content => Content},
            
            NewHistory = update_history(Messages ++ [AssistantMsg], State#state.conversation_history),
            UpdatedState = State#state{
                conversation_history = NewHistory,
                metrics = update_metrics_map(State#state.metrics, Response)
            },
            
            {ok, #{message => Content, response => Response}, UpdatedState};
        ToolCalls ->
            execute_tools_and_respond(ToolCalls, Response, Messages, State)
    end.

extract_tool_calls(Response) ->
    case maps:get(<<"choices">>, Response, []) of
        [] -> [];
        [Choice|_] ->
            Message = maps:get(<<"message">>, Choice, #{}),
            maps:get(<<"tool_calls">>, Message, [])
    end.

extract_message_content(Response) ->
    case maps:get(<<"choices">>, Response, []) of
        [] -> <<"No response">>;
        [Choice|_] ->
            Message = maps:get(<<"message">>, Choice, #{}),
            maps:get(<<"content">>, Message, <<"No content">>)
    end.

execute_tools_and_respond(ToolCalls, InitialResponse, Messages, State) ->
    execute_tools_and_respond(ToolCalls, InitialResponse, Messages, State, 0).

execute_tools_and_respond(ToolCalls, _InitialResponse, Messages, State, Depth) ->
    % Check recursion depth to prevent infinite loops
    MaxDepth = 5,  % Maximum number of tool call rounds
    case Depth >= MaxDepth of
        true ->
            ?LOG_WARNING("[AGENT_CHAT] Max tool call depth (~p) reached in Chat API, stopping recursion", [MaxDepth]),
            {ok, #{message => <<"Maximum tool call depth reached. Please refine your request.">>, response => #{}}, State};
        false ->
            ToolResults = execute_tool_calls(ToolCalls),
            
            ToolMessages = create_tool_messages(ToolCalls, ToolResults),
            UpdatedMessages = Messages ++ ToolMessages,
            
            case openai_chat:create_chat_completion(State#state.model, UpdatedMessages, #{}) of
                {ok, FinalResponse} ->
                    case extract_tool_calls(FinalResponse) of
                        [] ->
                            % No more tool calls, return the final response
                            Content = extract_message_content(FinalResponse),
                            NewHistory = update_history(UpdatedMessages, State#state.conversation_history),
                            
                            UpdatedState = State#state{
                                conversation_history = NewHistory,
                                metrics = update_metrics_map(State#state.metrics, FinalResponse)
                            },
                            
                            {ok, #{message => Content, response => FinalResponse}, UpdatedState};
                        MoreToolCalls ->
                            % Handle recursive tool calls
                            ?LOG_INFO("[AGENT_CHAT] Detected more tool calls in Chat API follow-up: ~p", [length(MoreToolCalls)]),
                            execute_tools_and_respond(MoreToolCalls, FinalResponse, UpdatedMessages, State, Depth + 1)
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

execute_tool_calls(ToolCalls) ->
    ?LOG_INFO("[TOOL_EXEC] Starting parallel execution of ~p tool calls", [length(ToolCalls)]),
    % Execute tool calls in parallel for better performance
    ParentPid = self(),
    
    % Spawn parallel execution tasks
    Tasks = lists:map(fun(ToolCall) ->
        spawn_link(fun() ->
            ToolInfo = case maps:get(<<"type">>, ToolCall, undefined) of
                <<"function">> ->
                    FunctionInfo = maps:get(<<"function">>, ToolCall, #{}),
                    #{name => maps:get(<<"name">>, FunctionInfo, undefined),
                      id => maps:get(<<"id">>, ToolCall, undefined)};
                <<"function_call">> ->
                    #{name => maps:get(<<"name">>, ToolCall, undefined),
                      id => maps:get(<<"call_id">>, ToolCall, undefined)};
                _ -> #{}
            end,
            ?LOG_DEBUG("[TOOL_EXEC] Executing tool: ~p", [ToolInfo]),
            StartTime = erlang:monotonic_time(millisecond),
            Result = execute_single_tool_call(ToolCall),
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            ?LOG_INFO("[TOOL_EXEC] Tool ~p completed in ~pms", [maps:get(name, ToolInfo, unknown), Duration]),
            ParentPid ! {tool_result, self(), Result}
        end)
    end, ToolCalls),
    
    % Collect results in order
    Results = collect_tool_results(Tasks, []),
    ?LOG_INFO("[TOOL_EXEC] All tool calls completed"),
    Results.

execute_single_tool_call(ToolCall) ->
    case maps:get(<<"type">>, ToolCall, undefined) of
        <<"function">> ->
            % Standard OpenAI function calling format (Chat API)
            ToolId = maps:get(<<"id">>, ToolCall),
            FunctionInfo = maps:get(<<"function">>, ToolCall),
            ToolName = maps:get(<<"name">>, FunctionInfo),
            ArgsJson = maps:get(<<"arguments">>, FunctionInfo),
            
            ?LOG_DEBUG("[TOOL_EXEC] Executing function: ~s (id: ~s)", [ToolName, ToolId]),
            ?LOG_DEBUG("[TOOL_EXEC] Arguments JSON: ~s", [ArgsJson]),
            
            % Safely decode arguments
            Args = try
                Decoded = jsx:decode(ArgsJson, [return_maps]),
                ?LOG_DEBUG("[TOOL_EXEC] Decoded arguments: ~p", [Decoded]),
                Decoded
            catch
                Type:Error ->
                    ?LOG_ERROR("[TOOL_EXEC] Failed to decode arguments: ~p:~p", [Type, Error]),
                    #{}
            end,
            
            % Convert tool name to atom safely
            ToolNameAtom = try
                binary_to_existing_atom(ToolName, utf8)
            catch
                _:_ -> binary_to_atom(ToolName, utf8)
            end,
            
            ?LOG_INFO("[TOOL_EXEC] Calling agent_tools:execute_tool(~p, ~p)", [ToolNameAtom, Args]),
            Result = agent_tools:execute_tool(ToolNameAtom, Args),
            ?LOG_INFO("[TOOL_EXEC] Tool ~s returned: ~p", [ToolName, case Result of
                {ok, Data} when is_binary(Data) andalso byte_size(Data) > 100 ->
                    {ok, <<(binary:part(Data, 0, 100))/binary, "...">>};
                Other -> Other
            end]),
            {ToolId, Result};
        <<"function_call">> ->
            % Responses API function calling format
            ToolId = maps:get(<<"call_id">>, ToolCall),
            ToolName = maps:get(<<"name">>, ToolCall),
            ArgsJson = maps:get(<<"arguments">>, ToolCall),
            
            ?LOG_DEBUG("[TOOL_EXEC] Executing function_call: ~s (call_id: ~s)", [ToolName, ToolId]),
            ?LOG_DEBUG("[TOOL_EXEC] Arguments JSON: ~s", [ArgsJson]),
            
            % Safely decode arguments
            Args = try
                Decoded = jsx:decode(ArgsJson, [return_maps]),
                ?LOG_DEBUG("[TOOL_EXEC] Decoded arguments: ~p", [Decoded]),
                Decoded
            catch
                Type:Error ->
                    ?LOG_ERROR("[TOOL_EXEC] Failed to decode arguments: ~p:~p", [Type, Error]),
                    #{}
            end,
            
            % Convert tool name to atom safely
            ToolNameAtom = try
                binary_to_existing_atom(ToolName, utf8)
            catch
                _:_ -> binary_to_atom(ToolName, utf8)
            end,
            
            Result = agent_tools:execute_tool(ToolNameAtom, Args),
            {ToolId, Result};
        _ ->
            % Unknown format - log and return error
            io:format("Unknown tool call format: ~p~n", [ToolCall]),
            {maps:get(<<"id">>, ToolCall, <<"unknown">>), {error, unknown_tool_call_format}}
    end.

collect_tool_results([], Results) ->
    lists:reverse(Results);
collect_tool_results([Pid | Remaining], Results) ->
    receive
        {tool_result, Pid, Result} ->
            collect_tool_results(Remaining, [Result | Results])
    after 30000 ->  % 30 second timeout per tool
        TimeoutResult = {<<"timeout">>, {error, tool_execution_timeout}},
        collect_tool_results(Remaining, [TimeoutResult | Results])
    end.

create_tool_messages(ToolCalls, Results) ->
    ?LOG_DEBUG("[TOOL_MSG] Creating tool messages for ~p tool calls and ~p results", [length(ToolCalls), length(Results)]),
    try
        Messages = lists:zipwith(fun(ToolCall, ResultTuple) ->
            % Safely extract ToolId and Result
            {ToolId, Result} = case ResultTuple of
                {Id, Res} -> {ensure_binary(Id), Res};
                Other -> 
                    ?LOG_WARNING("[TOOL_MSG] Unexpected result format: ~p", [Other]),
                    {<<"unknown">>, Other}
            end,
            
            % Safely get tool call type
            ToolType = maps:get(<<"type">>, ToolCall, <<"function">>),
            ?LOG_DEBUG("[TOOL_MSG] Processing tool result - Type: ~s, ID: ~s", [ToolType, ToolId]),
            
            case ToolType of
                <<"function">> ->
                    % Standard Chat API format
                    #{
                        <<"role">> => <<"tool">>,
                        <<"tool_call_id">> => ToolId,
                        <<"content">> => format_tool_result(Result)
                    };
                <<"function_call">> ->
                    % Responses API format  
                    #{
                        <<"type">> => <<"function_call_output">>,
                        <<"call_id">> => ToolId,
                        <<"output">> => format_tool_result(Result)
                    };
                _ ->
                    % Fallback format
                    #{
                        <<"role">> => <<"tool">>,
                        <<"tool_call_id">> => ToolId,
                        <<"content">> => format_tool_result(Result)
                    }
            end
        end, ToolCalls, Results),
        ?LOG_INFO("[TOOL_MSG] Successfully created ~p tool messages", [length(Messages)]),
        Messages
    catch
        Error:Reason:Stack ->
            ?LOG_ERROR("=== DETAILED ERROR REPORT ==="),
            ?LOG_ERROR("Error: ~p", [Error]),
            ?LOG_ERROR("Reason: ~p", [Reason]),
            ?LOG_ERROR("Stack trace:"),
            lists:foreach(fun(Frame) ->
                ?LOG_ERROR("  ~p", [Frame])
            end, Stack),
            ?LOG_ERROR("ToolCalls (~p items):", [length(ToolCalls)]),
            lists:foreach(fun({Index, ToolCall}) ->
                ?LOG_ERROR("  [~p] ~p", [Index, ToolCall])
            end, lists:zip(lists:seq(1, length(ToolCalls)), ToolCalls)),
            ?LOG_ERROR("Results (~p items):", [length(Results)]),
            lists:foreach(fun({Index, Result}) ->
                ?LOG_ERROR("  [~p] Type: ~p, Value: ~p", [Index, element_type(Result), Result])
            end, lists:zip(lists:seq(1, length(Results)), Results)),
            ?LOG_ERROR("=== END ERROR REPORT ==="),
            % Return safe fallback
            [#{
                <<"role">> => <<"tool">>,
                <<"tool_call_id">> => <<"error">>,
                <<"content">> => iolist_to_binary(io_lib:format("Error: ~p - ~p", [Error, Reason]))
            }]
    end.

format_tool_result(Result) when is_binary(Result) -> 
    Result;
format_tool_result({ok, #{<<"content">> := Content} = _Map}) when is_list(Content) ->
    % Handle Jina AI tool response format
    case Content of
        [#{<<"text">> := Text} | _] -> Text;
        _ -> jsx:encode(Content)
    end;
format_tool_result({ok, Result}) ->
    % Handle generic {ok, Result} tuples
    format_tool_result(Result);
format_tool_result({error, Reason}) -> 
    % Handle error tuples safely
    ErrorBinary = iolist_to_binary(io_lib:format("Error: ~p", [Reason])),
    ErrorBinary;
format_tool_result(Result) when is_map(Result) -> 
    % Handle maps with JSX - with detailed debugging
    io:format("DEBUG: Formatting map result: ~p~n", [Result]),
    try jsx:encode(Result) of
        Encoded -> 
            io:format("DEBUG: JSX encoding successful~n", []),
            Encoded
    catch
        Error:Reason ->
            io:format("DEBUG: JSX encoding failed - Error: ~p, Reason: ~p~n", [Error, Reason]),
            io:format("DEBUG: Map keys: ~p~n", [maps:keys(Result)]),
            io:format("DEBUG: Map values: ~p~n", [maps:values(Result)]),
            % Try to sanitize the map
            try
                SanitizedResult = sanitize_for_jsx(Result),
                io:format("DEBUG: Trying with sanitized map: ~p~n", [SanitizedResult]),
                jsx:encode(SanitizedResult)
            catch
                Error2:Reason2 ->
                    io:format("DEBUG: Even sanitized encoding failed - Error: ~p, Reason: ~p~n", [Error2, Reason2]),
                    iolist_to_binary(io_lib:format("~p", [Result]))
            end
    end;
format_tool_result(Result) when is_list(Result) -> 
    % Handle lists - check if it's a string first
    io:format("DEBUG: Formatting list result, length: ~p~n", [length(Result)]),
    case io_lib:printable_list(Result) of
        true -> 
            io:format("DEBUG: List is printable, converting to binary~n", []),
            list_to_binary(Result);
        false -> 
            io:format("DEBUG: List is not printable, trying JSX encode~n", []),
            try jsx:encode(Result) of
                Encoded -> 
                    io:format("DEBUG: JSX encoding of list successful~n", []),
                    Encoded
            catch
                Error:Reason ->
                    io:format("DEBUG: JSX encoding of list failed - Error: ~p, Reason: ~p~n", [Error, Reason]),
                    io:format("DEBUG: List elements: ~p~n", [lists:sublist(Result, 5)]), % Show first 5 elements
                    iolist_to_binary(io_lib:format("~p", [Result]))
            end
    end;
format_tool_result(Result) -> 
    % Handle any other type safely
    iolist_to_binary(io_lib:format("~p", [Result])).

%% Helper function to get element type for debugging
element_type(Value) when is_atom(Value) -> atom;
element_type(Value) when is_binary(Value) -> binary;
element_type(Value) when is_bitstring(Value) -> bitstring;
element_type(Value) when is_boolean(Value) -> boolean;
element_type(Value) when is_float(Value) -> float;
element_type(Value) when is_function(Value) -> function;
element_type(Value) when is_integer(Value) -> integer;
element_type(Value) when is_list(Value) -> list;
element_type(Value) when is_map(Value) -> map;
element_type(Value) when is_number(Value) -> number;
element_type(Value) when is_pid(Value) -> pid;
element_type(Value) when is_port(Value) -> port;
element_type(Value) when is_reference(Value) -> reference;
element_type(Value) when is_tuple(Value) -> tuple;
element_type(_) -> unknown.

%% Helper function to sanitize data for JSX encoding
sanitize_for_jsx(Value) when is_map(Value) ->
    maps:fold(fun(K, V, Acc) ->
        SafeKey = case K of
            K when is_atom(K) -> atom_to_binary(K, utf8);
            K when is_binary(K) -> K;
            _ -> iolist_to_binary(io_lib:format("~p", [K]))
        end,
        SafeValue = sanitize_for_jsx(V),
        maps:put(SafeKey, SafeValue, Acc)
    end, #{}, Value);
sanitize_for_jsx(Value) when is_list(Value) ->
    [sanitize_for_jsx(Item) || Item <- Value];
sanitize_for_jsx(Value) when is_pid(Value) ->
    list_to_binary(pid_to_list(Value));
sanitize_for_jsx(Value) when is_reference(Value) ->
    list_to_binary(ref_to_list(Value));
sanitize_for_jsx(Value) when is_function(Value) ->
    <<"#Function">>;
sanitize_for_jsx(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
sanitize_for_jsx(Value) ->
    Value.

%% Helper function to format tool call details for logging
format_tool_call_for_logging(ToolCall) when is_map(ToolCall) ->
    Type = maps:get(<<"type">>, ToolCall, <<"unknown">>),
    case Type of
        <<"function">> ->
            FuncInfo = maps:get(<<"function">>, ToolCall, #{}),
            Name = maps:get(<<"name">>, FuncInfo, <<"unknown">>),
            Id = maps:get(<<"id">>, ToolCall, <<"no-id">>),
            Args = maps:get(<<"arguments">>, FuncInfo, <<"{}">>),
            ArgsPreview = case byte_size(Args) > 50 of
                true -> <<(binary:part(Args, 0, 50))/binary, "...">>;
                false -> Args
            end,
            io_lib:format("🔧 ~s (id: ~s) args: ~s", [Name, Id, ArgsPreview]);
        <<"function_call">> ->
            Name = maps:get(<<"name">>, ToolCall, <<"unknown">>),
            CallId = maps:get(<<"call_id">>, ToolCall, <<"no-id">>),
            Args = maps:get(<<"arguments">>, ToolCall, <<"{}">>),
            ArgsPreview = case byte_size(Args) > 50 of
                true -> <<(binary:part(Args, 0, 50))/binary, "...">>;
                false -> Args
            end,
            io_lib:format("🔧 ~s (call_id: ~s) args: ~s", [Name, CallId, ArgsPreview]);
        _ ->
            io_lib:format("Unknown tool call type: ~p", [Type])
    end;
format_tool_call_for_logging(ToolCall) ->
    io_lib:format("Invalid tool call: ~p", [ToolCall]).

%% Helper function to format tool result details for logging  
format_tool_result_details(Id, Result) ->
    case Result of
        {ok, Data} when is_binary(Data) ->
            io_lib:format("✅ ~s: SUCCESS (~p bytes)", [Id, byte_size(Data)]);
        {ok, Data} when is_map(Data) ->
            io_lib:format("✅ ~s: SUCCESS (map with ~p keys)", [Id, maps:size(Data)]);
        {ok, Data} when is_list(Data) ->
            io_lib:format("✅ ~s: SUCCESS (list with ~p items)", [Id, length(Data)]);
        {ok, _} ->
            io_lib:format("✅ ~s: SUCCESS", [Id]);
        {error, Reason} ->
            io_lib:format("❌ ~s: ERROR - ~p", [Id, Reason]);
        _ ->
            io_lib:format("❓ ~s: UNKNOWN RESULT", [Id])
    end.

%% Helper function to ensure a value is binary
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) when is_list(Value) -> 
    case io_lib:printable_list(Value) of
        true -> list_to_binary(Value);
        false -> iolist_to_binary(io_lib:format("~p", [Value]))
    end;
ensure_binary(Value) -> iolist_to_binary(io_lib:format("~p", [Value])).

update_history(NewMessages, OldHistory) ->
    Combined = OldHistory ++ NewMessages,
    MaxHistory = 50,
    case length(Combined) > MaxHistory of
        true -> lists:nthtail(length(Combined) - MaxHistory, Combined);
        false -> Combined
    end.

update_metrics(State, successful_request) ->
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{
        total_requests => maps:get(total_requests, Metrics, 0) + 1,
        successful_requests => maps:get(successful_requests, Metrics, 0) + 1
    },
    State#state{metrics = NewMetrics};

update_metrics(State, failed_request) ->
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{
        total_requests => maps:get(total_requests, Metrics, 0) + 1,
        failed_requests => maps:get(failed_requests, Metrics, 0) + 1
    },
    State#state{metrics = NewMetrics}.

update_metrics_map(Metrics, Response) ->
    Usage = maps:get(<<"usage">>, Response, #{}),
    TotalTokens = maps:get(<<"total_tokens">>, Usage, 0),
    
    Metrics#{
        total_requests => maps:get(total_requests, Metrics, 0) + 1,
        successful_requests => maps:get(successful_requests, Metrics, 0) + 1,
        total_tokens => maps:get(total_tokens, Metrics, 0) + TotalTokens
    }.

%% Handle streaming response from OpenAI Chat API
handle_streaming_response(StreamPid, Messages, State) ->
    % Use the new streaming function handler
    case streaming_function_handler:handle_chat_api_stream(StreamPid, Messages, State) of
        {ok, Response, UpdatedState} ->
            % Handle the response with tool results if any
            case maps:get(tool_results, Response, []) of
                [] ->
                    % No tool calls executed
                    FinalState = update_metrics(UpdatedState, successful_request),
                    {ok, Response, FinalState};
                ToolResults ->
                    % Tool calls were executed, need to get final response
                    ToolCalls = maps:get(tool_calls, Response, []),
                    handle_tool_results_streaming(ToolCalls, ToolResults, Messages, StreamPid, UpdatedState)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Handle tool results and continue streaming
handle_tool_results_streaming(ToolCalls, ToolResults, Messages, StreamPid, State) ->
    % Create tool messages
    ToolMessages = create_tool_messages(ToolCalls, ToolResults),
    UpdatedMessages = Messages ++ ToolMessages,
    
    % Make another streaming request with tool results
    case openai_chat:create_streaming_completion(State#state.model, UpdatedMessages, #{
        tools => agent_tools:get_enhanced_tools(State#state.tools),
        tool_choice => <<"auto">>
    }) of
        ok ->
            % Continue with streaming response handling
            handle_streaming_response(StreamPid, UpdatedMessages, State);
        {error, Reason} ->
            StreamPid ! {stream_error, Reason},
            {error, Reason}
    end.

%% Parse streaming events from OpenAI
parse_streaming_event(Event) ->
    try
        case jsx:decode(Event, [return_maps]) of
            #{<<"choices">> := [Choice | _]} ->
                case maps:get(<<"delta">>, Choice, #{}) of
                    #{<<"content">> := Content} when Content =/= null ->
                        {token, Content};
                    _ ->
                        continue
                end;
            _ ->
                continue
        end
    catch
        _:_ ->
            continue
    end.

%% Helper function to create an assistant message from a response
create_assistant_message_from_response(Response, ToolCalls) ->
    % For the Responses API, we need to create a properly formatted assistant message
    % that contains the function calls
    #{
        role => <<"assistant">>,
        content => <<>>,  % Empty content when there are function calls
        tool_calls => ToolCalls
    }.

%% Helper function to create assistant output with function calls for Responses API
create_assistant_output_with_function_calls(ToolCalls) ->
    % Convert tool calls to proper output format for Responses API
    lists:map(fun(ToolCall) ->
        % The ToolCall should already be in the right format from the response
        ToolCall
    end, ToolCalls).

%% Split text into chunks for fake streaming
split_into_chunks(Text, ChunkSize) when is_list(Text) ->
    split_into_chunks_helper(Text, ChunkSize, []).

split_into_chunks_helper([], _ChunkSize, Acc) ->
    lists:reverse(Acc);
split_into_chunks_helper(Text, ChunkSize, Acc) when length(Text) =< ChunkSize ->
    lists:reverse([Text | Acc]);
split_into_chunks_helper(Text, ChunkSize, Acc) ->
    {Chunk, Rest} = lists:split(ChunkSize, Text),
    split_into_chunks_helper(Rest, ChunkSize, [Chunk | Acc]).

%% Cost tracking helper
track_cost(State, Response) ->
    Usage = maps:get(<<"usage">>, Response, #{}),
    cost_tracker:track_usage(State#state.id, State#state.model, Usage, #{
        agent_name => State#state.name,
        api_type => State#state.api_preference
    }).

%% Handle streaming events from Responses API
handle_streaming_response_events(StreamPid, Input, State) ->
    % Use the new streaming function handler for Responses API
    case streaming_function_handler:handle_responses_api_stream(StreamPid, Input, State) of
        {ok, Response, UpdatedState} ->
            % Handle the response with tool results if any
            case maps:get(tool_results, Response, []) of
                [] ->
                    % No tool calls executed
                    FinalState = update_metrics(UpdatedState, successful_request),
                    {ok, Response, FinalState};
                ToolResults ->
                    % Tool calls were executed, need to get final response
                    ToolCalls = maps:get(tool_calls, Response, []),
                    handle_tool_results_streaming_responses(ToolCalls, ToolResults, Input, StreamPid, UpdatedState)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Handle tool results for Responses API streaming
handle_tool_results_streaming_responses(ToolCalls, ToolResults, Input, StreamPid, State) ->
    % Create tool messages in Responses API format
    ToolMessages = create_tool_messages(ToolCalls, ToolResults),
    
    % Build updated input with tool results
    UpdatedInput = Input ++ ToolMessages,
    
    % Continue with another streaming request
    Options = #{
        tools => agent_tools:get_enhanced_tools(State#state.tools),
        tool_choice => <<"auto">>,
        instructions => State#state.system_prompt,
        parallel_tool_calls => true,
        temperature => 0.7,
        stream => true
    },
    
    case openai_responses:create_streaming_response(UpdatedInput, State#state.model, Options) of
        ok ->
            % Continue with streaming response handling
            handle_streaming_response_events(StreamPid, UpdatedInput, State);
        {error, Reason} ->
            StreamPid ! {stream_error, Reason},
            {error, Reason}
    end.

%% Parse streaming events from Responses API
parse_responses_streaming_event(Event) ->
    try
        EventType = maps:get(<<"type">>, Event, undefined),
        case EventType of
            <<"response.output_text.delta">> ->
                Delta = maps:get(<<"delta">>, Event, <<"">>),
                {output_text_delta, Delta};
            <<"response.output_item.added">> ->
                % New function call started
                Item = maps:get(<<"item">>, Event, #{}),
                case maps:get(<<"type">>, Item, undefined) of
                    <<"function_call">> ->
                        {function_call_started, Item};
                    _ -> continue
                end;
            <<"response.function_call_arguments.delta">> ->
                % Function call arguments being streamed
                Delta = maps:get(<<"delta">>, Event, <<"">>),
                ItemId = maps:get(<<"item_id">>, Event, <<"">>),
                {function_call_arguments_delta, ItemId, Delta};
            <<"response.function_call_arguments.done">> ->
                % Function call arguments complete
                Item = maps:get(<<"item">>, Event, #{}),
                {function_call_complete, Item};
            <<"response.output_item.done">> ->
                % Output item complete
                Item = maps:get(<<"item">>, Event, #{}),
                case maps:get(<<"type">>, Item, undefined) of
                    <<"function_call">> ->
                        {function_call_done, Item};
                    _ -> continue
                end;
            <<"response.done">> ->
                continue;
            <<"response.failed">> ->
                ErrorInfo = maps:get(<<"response">>, Event, #{}),
                Error = maps:get(<<"error">>, ErrorInfo, <<"Unknown error">>),
                {error, Error};
            _ ->
                continue
        end
    catch
        _:_ ->
            continue
    end.

