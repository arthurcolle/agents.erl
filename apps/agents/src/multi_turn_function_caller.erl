%%%-------------------------------------------------------------------
%%% @doc Multi-turn Function Calling System
%%% Advanced function calling with conversation memory and context
%%% @end
%%%-------------------------------------------------------------------
-module(multi_turn_function_caller).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    handle_multi_turn_conversation/3,
    execute_function_chain/3,
    get_conversation_context/1,
    clear_conversation_context/1,
    get_function_call_history/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
    agent_id,
    conversation_contexts = #{},
    function_call_history = [],
    max_context_length = 50,
    max_function_chain_length = 10
}).

-record(conversation_context, {
    conversation_id,
    messages = [],
    function_calls = [],
    variables = #{},
    last_activity,
    turn_count = 0
}).

-record(function_call_result, {
    function_name,
    parameters,
    result,
    success,
    execution_time,
    timestamp,
    turn_index
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(AgentId) ->
    gen_server:start_link({local, process_name(AgentId)}, ?MODULE, [AgentId], []).

%% @doc Handle a multi-turn conversation with function calling
-spec handle_multi_turn_conversation(binary(), binary(), map()) -> {ok, map()} | {error, term()}.
handle_multi_turn_conversation(AgentId, ConversationId, Message) ->
    ProcessName = process_name(AgentId),
    gen_server:call(ProcessName, {handle_conversation, ConversationId, Message}).

%% @doc Execute a chain of functions in sequence
-spec execute_function_chain(binary(), binary(), [map()]) -> {ok, [map()]} | {error, term()}.
execute_function_chain(AgentId, ConversationId, FunctionChain) ->
    ProcessName = process_name(AgentId),
    gen_server:call(ProcessName, {execute_chain, ConversationId, FunctionChain}).

%% @doc Get conversation context for debugging
-spec get_conversation_context(binary()) -> {ok, map()} | {error, not_found}.
get_conversation_context(AgentId) ->
    ProcessName = process_name(AgentId),
    gen_server:call(ProcessName, get_conversation_context).

%% @doc Clear conversation context
-spec clear_conversation_context(binary()) -> ok.
clear_conversation_context(AgentId) ->
    ProcessName = process_name(AgentId),
    gen_server:cast(ProcessName, clear_context).

%% @doc Get function call history
-spec get_function_call_history(binary()) -> {ok, [map()]}.
get_function_call_history(AgentId) ->
    ProcessName = process_name(AgentId),
    gen_server:call(ProcessName, get_function_history).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([AgentId]) ->
    {ok, #state{agent_id = AgentId}}.

handle_call({handle_conversation, ConversationId, Message}, _From, State) ->
    try
        {UpdatedState, Response} = process_conversation_turn(ConversationId, Message, State),
        {reply, {ok, Response}, UpdatedState}
    catch
        _:Error ->
            {reply, {error, Error}, State}
    end;

handle_call({execute_chain, ConversationId, FunctionChain}, _From, State) ->
    try
        {UpdatedState, Results} = execute_function_chain_internal(ConversationId, FunctionChain, State),
        {reply, {ok, Results}, UpdatedState}
    catch
        _:Error ->
            {reply, {error, Error}, State}
    end;

handle_call(get_conversation_context, _From, State) ->
    FormattedContexts = maps:map(fun(_Id, Context) ->
        format_conversation_context(Context)
    end, State#state.conversation_contexts),
    {reply, {ok, FormattedContexts}, State};

handle_call(get_function_history, _From, State) ->
    FormattedHistory = [format_function_call_result(Result) || Result <- State#state.function_call_history],
    {reply, {ok, FormattedHistory}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(clear_context, State) ->
    {noreply, State#state{conversation_contexts = #{}}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

process_conversation_turn(ConversationId, Message, State) ->
    %% Get or create conversation context
    Context = get_or_create_context(ConversationId, State#state.conversation_contexts),
    
    %% Update context with new message
    UpdatedContext = add_message_to_context(Message, Context),
    
    %% Analyze message for function calling needs
    FunctionCallNeeds = analyze_function_call_needs(Message, UpdatedContext),
    
    %% Execute any required function calls
    {FinalContext, FunctionResults} = case FunctionCallNeeds of
        {needs_functions, Functions} ->
            execute_required_functions(Functions, UpdatedContext, State);
        {no_functions_needed, _} ->
            {UpdatedContext, []}
    end,
    
    %% Generate response using AI model
    Response = generate_ai_response(Message, FinalContext, FunctionResults, State),
    
    %% Update conversation context with response
    ResponseContext = add_response_to_context(Response, FinalContext),
    
    %% Update state
    UpdatedContexts = maps:put(ConversationId, ResponseContext, State#state.conversation_contexts),
    UpdatedHistory = FunctionResults ++ lists:sublist(State#state.function_call_history, 
                                                    State#state.max_context_length - length(FunctionResults)),
    
    NewState = State#state{
        conversation_contexts = UpdatedContexts,
        function_call_history = UpdatedHistory
    },
    
    %% Record metrics
    record_conversation_metrics(State#state.agent_id, ConversationId, ResponseContext, FunctionResults),
    
    {NewState, Response}.

get_or_create_context(ConversationId, Contexts) ->
    case maps:get(ConversationId, Contexts, undefined) of
        undefined ->
            #conversation_context{
                conversation_id = ConversationId,
                last_activity = erlang:system_time(second)
            };
        ExistingContext ->
            ExistingContext
    end.

add_message_to_context(Message, Context) ->
    NewMessages = [Message | lists:sublist(Context#conversation_context.messages, 49)],
    Context#conversation_context{
        messages = NewMessages,
        last_activity = erlang:system_time(second),
        turn_count = Context#conversation_context.turn_count + 1
    }.

analyze_function_call_needs(Message, Context) ->
    %% Advanced analysis to determine if function calls are needed
    MessageContent = maps:get(content, Message, <<>>),
    MessageType = maps:get(type, Message, <<"text">>),
    
    %% Check for explicit function call requests
    ExplicitFunctionCall = extract_explicit_function_calls(MessageContent),
    
    %% Check for implicit function call needs based on context
    ImplicitFunctionCall = analyze_implicit_function_needs(MessageContent, Context),
    
    %% Check for multi-turn function call continuation
    ContinuationFunctionCall = check_function_call_continuation(Context),
    
    Functions = lists:flatten([ExplicitFunctionCall, ImplicitFunctionCall, ContinuationFunctionCall]),
    
    case Functions of
        [] -> {no_functions_needed, MessageContent};
        _ -> {needs_functions, Functions}
    end.

extract_explicit_function_calls(MessageContent) ->
    %% Look for patterns like "call function X with Y" or "use tool Z"
    Patterns = [
        {<<"call\\s+(\\w+)\\s+with\\s+(.+)">>, fun(Matches) ->
            [FunctionName, ParamsStr] = Matches,
            #{
                function => FunctionName,
                parameters => parse_parameters_from_text(ParamsStr),
                priority => high,
                type => explicit
            }
        end},
        {<<"use\\s+(\\w+)\\s+tool">>, fun(Matches) ->
            [ToolName] = Matches,
            #{
                function => <<ToolName/binary, "_tool">>,
                parameters => #{},
                priority => high,
                type => explicit
            }
        end}
    ],
    
    lists:flatten([extract_pattern_matches(MessageContent, Pattern, Handler) || {Pattern, Handler} <- Patterns]).

analyze_implicit_function_needs(MessageContent, Context) ->
    %% Analyze content for implicit function calling needs
    Keywords = [
        {[<<"analyze">>, <<"data">>, <<"dataset">>], <<"analyze_dataset">>},
        {[<<"create">>, <<"visualization">>, <<"chart">>], <<"create_visualization">>},
        {[<<"search">>, <<"find">>, <<"lookup">>], <<"knowledge_search">>},
        {[<<"calculate">>, <<"compute">>, <<"math">>], <<"calculate">>},
        {[<<"translate">>, <<"language">>], <<"translation_service">>},
        {[<<"code">>, <<"review">>, <<"programming">>], <<"code_review">>},
        {[<<"security">>, <<"scan">>, <<"vulnerability">>], <<"security_scan">>},
        {[<<"deploy">>, <<"infrastructure">>], <<"deploy_application">>}
    ],
    
    PreviousMessages = Context#conversation_context.messages,
    ContextualClues = extract_contextual_clues(PreviousMessages),
    
    ImplicitFunctions = lists:filtermap(fun({KeywordList, FunctionName}) ->
        case has_keywords(MessageContent, KeywordList) of
            true ->
                Parameters = infer_parameters_from_context(MessageContent, ContextualClues),
                {true, #{
                    function => FunctionName,
                    parameters => Parameters,
                    priority => medium,
                    type => implicit
                }};
            false ->
                false
        end
    end, Keywords),
    
    ImplicitFunctions.

check_function_call_continuation(Context) ->
    %% Check if previous function calls require follow-up
    RecentFunctionCalls = lists:sublist(Context#conversation_context.function_calls, 3),
    
    lists:filtermap(fun(FunctionCall) ->
        case requires_followup(FunctionCall) of
            {true, FollowupFunction} ->
                {true, #{
                    function => FollowupFunction,
                    parameters => extract_followup_parameters(FunctionCall),
                    priority => high,
                    type => continuation,
                    parent_call => FunctionCall
                }};
            false ->
                false
        end
    end, RecentFunctionCalls).

execute_required_functions(Functions, Context, State) ->
    %% Sort functions by priority
    SortedFunctions = lists:sort(fun(A, B) ->
        PriorityOrder = #{high => 3, medium => 2, low => 1},
        maps:get(priority, A, medium) >= maps:get(priority, B, medium)
    end, Functions),
    
    %% Execute functions in sequence, respecting chain length limit
    FunctionsToExecute = lists:sublist(SortedFunctions, State#state.max_function_chain_length),
    
    {UpdatedContext, Results} = lists:foldl(fun(FunctionSpec, {AccContext, AccResults}) ->
        Result = execute_single_function(FunctionSpec, AccContext, State),
        
        %% Update context with function result
        NewContext = add_function_result_to_context(Result, AccContext),
        
        {NewContext, [Result | AccResults]}
    end, {Context, []}, FunctionsToExecute),
    
    {UpdatedContext, lists:reverse(Results)}.

execute_single_function(FunctionSpec, Context, State) ->
    FunctionName = maps:get(function, FunctionSpec),
    Parameters = maps:get(parameters, FunctionSpec, #{}),
    
    StartTime = erlang:system_time(microsecond),
    
    try
        %% Execute the function using the agent's tools
        ToolResult = agent_tools:execute_tool(FunctionName, Parameters),
        
        EndTime = erlang:system_time(microsecond),
        ExecutionTime = EndTime - StartTime,
        
        #function_call_result{
            function_name = FunctionName,
            parameters = Parameters,
            result = ToolResult,
            success = true,
            execution_time = ExecutionTime,
            timestamp = erlang:system_time(second),
            turn_index = Context#conversation_context.turn_count
        }
    catch
        _:Error ->
            ErrorEndTime = erlang:system_time(microsecond),
            ErrorExecTime = ErrorEndTime - StartTime,
            
            #function_call_result{
                function_name = FunctionName,
                parameters = Parameters,
                result = #{error => format_error(Error)},
                success = false,
                execution_time = ErrorExecTime,
                timestamp = erlang:system_time(second),
                turn_index = Context#conversation_context.turn_count
            }
    end.

generate_ai_response(Message, Context, FunctionResults, State) ->
    %% Prepare context for AI model
    SystemPrompt = build_system_prompt_with_context(Context, FunctionResults),
    
    %% Format conversation history
    Messages = format_messages_for_ai(Context#conversation_context.messages, FunctionResults),
    
    %% Call AI model
    case openai_chat:chat_completion(#{
        model => <<"gpt-4o">>,
        messages => Messages,
        system => SystemPrompt,
        temperature => 0.7,
        max_tokens => 2000,
        tools => get_available_tools(State#state.agent_id)
    }) of
        {ok, Response} ->
            #{
                content => maps:get(content, Response, <<>>),
                type => <<"assistant">>,
                timestamp => erlang:system_time(second),
                function_calls_used => length(FunctionResults),
                turn_count => Context#conversation_context.turn_count
            };
        {error, Error} ->
            #{
                content => <<"I apologize, but I encountered an error processing your request.">>,
                type => <<"assistant">>,
                error => Error,
                timestamp => erlang:system_time(second)
            }
    end.

%% Helper functions for text analysis and parameter extraction

extract_pattern_matches(Text, Pattern, Handler) ->
    case re:run(Text, Pattern, [global, {capture, all_but_first, binary}]) of
        {match, Matches} ->
            [Handler(Match) || Match <- Matches];
        nomatch ->
            []
    end.

parse_parameters_from_text(ParamsStr) ->
    %% Simple parameter parsing - can be enhanced
    case jsx:is_json(ParamsStr) of
        true ->
            try jsx:decode(ParamsStr, [return_maps]) catch _:_ -> #{text => ParamsStr} end;
        false ->
            #{text => ParamsStr}
    end.

has_keywords(Text, Keywords) ->
    LowerText = string:lowercase(Text),
    lists:any(fun(Keyword) ->
        binary:match(LowerText, string:lowercase(Keyword)) =/= nomatch
    end, Keywords).

infer_parameters_from_context(MessageContent, ContextualClues) ->
    %% Basic parameter inference - can be made more sophisticated
    #{
        query => MessageContent,
        context => ContextualClues,
        timestamp => erlang:system_time(second)
    }.

extract_contextual_clues(Messages) ->
    %% Extract relevant information from previous messages
    RecentMessages = lists:sublist(Messages, 5),
    
    #{
        recent_topics => [extract_topic(Msg) || Msg <- RecentMessages],
        mentioned_entities => lists:flatten([extract_entities(Msg) || Msg <- RecentMessages]),
        user_preferences => extract_preferences(RecentMessages)
    }.

requires_followup(FunctionCall) ->
    %% Determine if a function call requires follow-up
    case FunctionCall#function_call_result.function_name of
        <<"analyze_dataset">> ->
            case maps:get(visualization_suggested, FunctionCall#function_call_result.result, false) of
                true -> {true, <<"create_visualization">>};
                false -> false
            end;
        <<"security_scan">> ->
            case maps:get(vulnerabilities_found, FunctionCall#function_call_result.result, false) of
                true -> {true, <<"threat_analysis">>};
                false -> false
            end;
        _ ->
            false
    end.

extract_followup_parameters(FunctionCall) ->
    %% Extract parameters for follow-up function calls
    case FunctionCall#function_call_result.function_name of
        <<"analyze_dataset">> ->
            #{
                data_source => maps:get(data_source, FunctionCall#function_call_result.parameters, <<>>),
                analysis_results => FunctionCall#function_call_result.result
            };
        <<"security_scan">> ->
            #{
                scan_results => FunctionCall#function_call_result.result,
                target => maps:get(target, FunctionCall#function_call_result.parameters, <<>>)
            };
        _ ->
            #{}
    end.

add_function_result_to_context(Result, Context) ->
    NewFunctionCalls = [Result | lists:sublist(Context#conversation_context.function_calls, 19)],
    
    %% Update variables based on function result
    UpdatedVariables = update_context_variables(Result, Context#conversation_context.variables),
    
    Context#conversation_context{
        function_calls = NewFunctionCalls,
        variables = UpdatedVariables
    }.

update_context_variables(FunctionResult, CurrentVariables) ->
    %% Update context variables based on function results
    case FunctionResult#function_call_result.function_name of
        <<"analyze_dataset">> ->
            maps:merge(CurrentVariables, #{
                last_dataset => maps:get(data_source, FunctionResult#function_call_result.parameters, <<>>),
                analysis_timestamp => FunctionResult#function_call_result.timestamp
            });
        <<"create_visualization">> ->
            maps:merge(CurrentVariables, #{
                last_chart_type => maps:get(chart_type, FunctionResult#function_call_result.parameters, <<>>),
                visualization_timestamp => FunctionResult#function_call_result.timestamp
            });
        _ ->
            CurrentVariables
    end.

add_response_to_context(Response, Context) ->
    NewMessages = [Response | lists:sublist(Context#conversation_context.messages, 49)],
    Context#conversation_context{
        messages = NewMessages,
        last_activity = erlang:system_time(second)
    }.

build_system_prompt_with_context(Context, FunctionResults) ->
    BasePrompt = <<"You are an AI assistant capable of multi-turn conversations and function calling. ">>,
    
    ContextPrompt = case Context#conversation_context.turn_count of
        0 -> <<BasePrompt/binary, "This is the start of a new conversation.">>;
        Count -> <<BasePrompt/binary, "This is turn ", (integer_to_binary(Count))/binary, " of an ongoing conversation.">>
    end,
    
    FunctionPrompt = case FunctionResults of
        [] -> ContextPrompt;
        _ ->
            FunctionSummary = summarize_function_results(FunctionResults),
            <<ContextPrompt/binary, " Recent function calls: ", FunctionSummary/binary>>
    end,
    
    FunctionPrompt.

summarize_function_results(FunctionResults) ->
    Summaries = lists:map(fun(Result) ->
        case Result#function_call_result.success of
            true -> <<(Result#function_call_result.function_name)/binary, " (success)">>;
            false -> <<(Result#function_call_result.function_name)/binary, " (failed)">>
        end
    end, FunctionResults),
    
    iolist_to_binary(lists:join(<<", ">>, Summaries)).

format_messages_for_ai(Messages, FunctionResults) ->
    %% Format conversation messages for AI model
    FormattedMessages = lists:map(fun(Message) ->
        #{
            role => maps:get(type, Message, <<"user">>),
            content => maps:get(content, Message, <<>>)
        }
    end, lists:reverse(lists:sublist(Messages, 10))),
    
    %% Add function results as system messages if any
    case FunctionResults of
        [] -> FormattedMessages;
        _ ->
            FunctionMessage = #{
                role => <<"system">>,
                content => format_function_results_for_ai(FunctionResults)
            },
            FormattedMessages ++ [FunctionMessage]
    end.

format_function_results_for_ai(FunctionResults) ->
    ResultStrings = lists:map(fun(Result) ->
        ResultData = jsx:encode(Result#function_call_result.result),
        <<(Result#function_call_result.function_name)/binary, " returned: ", ResultData/binary>>
    end, FunctionResults),
    
    iolist_to_binary(lists:join(<<"\n">>, ResultStrings)).

get_available_tools(AgentId) ->
    %% Get tools available for this agent
    case agent_tools:get_agent_tools(AgentId) of
        {ok, Tools} -> Tools;
        _ -> []
    end.

%% Utility functions

format_conversation_context(Context) ->
    #{
        conversation_id => Context#conversation_context.conversation_id,
        turn_count => Context#conversation_context.turn_count,
        message_count => length(Context#conversation_context.messages),
        function_call_count => length(Context#conversation_context.function_calls),
        variables => Context#conversation_context.variables,
        last_activity => Context#conversation_context.last_activity
    }.

format_function_call_result(Result) ->
    #{
        function_name => Result#function_call_result.function_name,
        parameters => Result#function_call_result.parameters,
        success => Result#function_call_result.success,
        execution_time_ms => Result#function_call_result.execution_time / 1000,
        timestamp => Result#function_call_result.timestamp,
        turn_index => Result#function_call_result.turn_index
    }.

extract_topic(Message) ->
    %% Simple topic extraction - can be enhanced with NLP
    Content = maps:get(content, Message, <<>>),
    case byte_size(Content) of
        Size when Size > 20 -> binary:part(Content, 0, 20);
        _ -> Content
    end.

extract_entities(Message) ->
    %% Simple entity extraction - can be enhanced with NLP
    Content = maps:get(content, Message, <<>>),
    Words = binary:split(Content, <<" ">>, [global]),
    [Word || Word <- Words, byte_size(Word) > 3].

extract_preferences(Messages) ->
    %% Extract user preferences from message history
    #{
        communication_style => <<"conversational">>,
        detail_level => <<"medium">>,
        preferred_format => <<"text">>
    }.

record_conversation_metrics(AgentId, ConversationId, Context, FunctionResults) ->
    %% Record metrics for the conversation
    Metrics = #{
        agent_id => AgentId,
        conversation_id => ConversationId,
        turn_count => Context#conversation_context.turn_count,
        function_calls_count => length(FunctionResults),
        successful_function_calls => length([R || R <- FunctionResults, R#function_call_result.success]),
        timestamp => erlang:system_time(second)
    },
    
    %% Send to metrics engine
    case whereis(realtime_metrics_engine) of
        undefined -> ok;
        _ -> realtime_metrics_engine:record_multi_turn_conversation(AgentId, 
                                                                   Context#conversation_context.turn_count, 
                                                                   Metrics)
    end.

execute_function_chain_internal(ConversationId, FunctionChain, State) ->
    Context = get_or_create_context(ConversationId, State#state.conversation_contexts),
    
    Results = lists:map(fun(FunctionSpec) ->
        execute_single_function(FunctionSpec, Context, State)
    end, FunctionChain),
    
    %% Update context with all results
    UpdatedContext = lists:foldl(fun(Result, AccContext) ->
        add_function_result_to_context(Result, AccContext)
    end, Context, Results),
    
    %% Update state
    UpdatedContexts = maps:put(ConversationId, UpdatedContext, State#state.conversation_contexts),
    NewState = State#state{conversation_contexts = UpdatedContexts},
    
    {NewState, [format_function_call_result(R) || R <- Results]}.

format_error(Error) ->
    iolist_to_binary(io_lib:format("~p", [Error])).

process_name(AgentId) ->
    binary_to_atom(<<"multi_turn_", AgentId/binary>>, utf8).