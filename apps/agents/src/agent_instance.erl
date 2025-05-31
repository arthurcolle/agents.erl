%% agent_instance.erl
%% Individual agent process with gen_server behavior
-module(agent_instance).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    execute/2,
    get_state/1,
    update_config/2
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
    last_activity :: erlang:timestamp()
}).

%% API Functions

start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

execute(Pid, Action) ->
    gen_server:call(Pid, {execute, Action}, 60000).

get_state(Pid) ->
    gen_server:call(Pid, get_state).

update_config(Pid, Config) ->
    gen_server:call(Pid, {update_config, Config}).

%% gen_server callbacks

init(Config) ->
    process_flag(trap_exit, true),
    
    State = #state{
        id = maps:get(id, Config, generate_id()),
        name = maps:get(name, Config, <<"Unnamed Agent">>),
        type = maps:get(type, Config, ai),
        model = maps:get(model, Config, <<"gpt-4">>),
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
        last_activity = erlang:timestamp()
    },
    
    {ok, State}.

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
        conversation_length => length(State#state.conversation_history)
    },
    {reply, StateMap, State};

handle_call({update_config, Config}, _From, State) ->
    NewState = State#state{
        name = maps:get(name, Config, State#state.name),
        model = maps:get(model, Config, State#state.model),
        tools = maps:get(tools, Config, State#state.tools),
        system_prompt = maps:get(system_prompt, Config, State#state.system_prompt)
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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

process_chat(Message, #state{type = ai} = State) ->
    Messages = build_messages(Message, State),
    Tools = agent_tools:get_tools(State#state.tools),
    
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

build_messages(UserMessage, State) ->
    SystemMessage = #{role => <<"system">>, content => State#state.system_prompt},
    UserMsg = #{role => <<"user">>, content => UserMessage},
    
    History = lists:reverse(State#state.conversation_history),
    [SystemMessage | History] ++ [UserMsg].

handle_chat_response(Response, Messages, State) ->
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
    ToolResults = execute_tool_calls(ToolCalls),
    
    ToolMessages = create_tool_messages(ToolCalls, ToolResults),
    UpdatedMessages = Messages ++ ToolMessages,
    
    case openai_chat:create_chat_completion(State#state.model, UpdatedMessages, #{}) of
        {ok, FinalResponse} ->
            Content = extract_message_content(FinalResponse),
            NewHistory = update_history(UpdatedMessages, State#state.conversation_history),
            
            UpdatedState = State#state{
                conversation_history = NewHistory,
                metrics = update_metrics_map(State#state.metrics, FinalResponse)
            },
            
            {ok, #{message => Content, response => FinalResponse}, UpdatedState};
        {error, Reason} ->
            {error, Reason}
    end.

execute_tool_calls(ToolCalls) ->
    lists:map(fun(ToolCall) ->
        ToolId = maps:get(<<"id">>, ToolCall),
        ToolName = binary_to_atom(maps:get(<<"function">>, maps:get(<<"function">>, ToolCall)), utf8),
        ArgsJson = maps:get(<<"arguments">>, maps:get(<<"function">>, ToolCall)),
        Args = jsx:decode(ArgsJson, [return_maps]),
        
        Result = agent_tools:execute_tool(ToolName, Args),
        {ToolId, Result}
    end, ToolCalls).

create_tool_messages(ToolCalls, Results) ->
    lists:map(fun({ToolId, Result}) ->
        #{
            role => <<"tool">>,
            tool_call_id => ToolId,
            content => format_tool_result(Result)
        }
    end, Results).

format_tool_result(Result) when is_binary(Result) -> Result;
format_tool_result(Result) -> jsx:encode(Result).

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