%% smart_reasoning_agent.erl
%% Enhanced agent implementing OTC-PO principles for optimal tool usage
%% Addresses: "It doesn't seem to get search results, and sequentially process them until its found answers.
%%           It isn't smart and it doesn't do multi-turn function calling"
-module(smart_reasoning_agent).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    create_smart_agent/1,
    chat/2,
    multi_turn_reasoning/2,
    smart_search_and_reason/2,
    get_agent_metrics/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    agent_id :: binary(),
    model :: binary(),
    system_prompt :: binary(),
    conversation_history = [] :: list(),
    tool_usage_stats = #{} :: map(),
    reasoning_cache = #{} :: map(),
    productivity_score = 1.0 :: float(),
    intelligence_level = beginner :: atom(),  % beginner | intermediate | advanced
    max_reasoning_depth = 5 :: integer()
}).

-record(reasoning_step, {
    step_number :: integer(),
    reasoning_type :: atom(),  % internal | tool_assisted | hybrid
    input :: binary(),
    output :: binary(),
    confidence :: float(),
    tool_calls_used :: integer(),
    timestamp :: integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link(Config) ->
    AgentId = maps:get(id, Config),
    gen_server:start_link(?MODULE, Config, []).

create_smart_agent(Config) ->
    DefaultConfig = #{
        model => <<"gpt-4-turbo">>,
        system_prompt => get_smart_system_prompt(),
        intelligence_level => intermediate,
        max_reasoning_depth => 5
    },
    
    FinalConfig = maps:merge(DefaultConfig, Config),
    case start_link(FinalConfig) of
        {ok, Pid} -> {ok, Pid};
        Error -> Error
    end.

%% Enhanced chat with intelligent reasoning
chat(AgentPid, Message) ->
    gen_server:call(AgentPid, {chat, Message}, 60000).

%% Multi-turn reasoning for complex questions
multi_turn_reasoning(AgentPid, ComplexQuestion) ->
    gen_server:call(AgentPid, {multi_turn_reasoning, ComplexQuestion}, 120000).

%% Smart search with sequential processing
smart_search_and_reason(AgentPid, SearchQuery) ->
    gen_server:call(AgentPid, {smart_search_and_reason, SearchQuery}, 90000).

%% Get agent performance metrics
get_agent_metrics(AgentPid) ->
    gen_server:call(AgentPid, get_metrics).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Config) ->
    AgentId = maps:get(id, Config),
    colored_logger:success("Starting Smart Reasoning Agent: ~s", [AgentId]),
    
    % Initialize optimizer and search processor
    intelligent_tool_optimizer:start_link(),
    intelligent_search_processor:start_link(),
    
    State = #state{
        agent_id = AgentId,
        model = maps:get(model, Config),
        system_prompt = maps:get(system_prompt, Config),
        intelligence_level = maps:get(intelligence_level, Config, intermediate),
        max_reasoning_depth = maps:get(max_reasoning_depth, Config, 5)
    },
    
    {ok, State}.

handle_call({chat, Message}, _From, State) ->
    Result = execute_intelligent_chat(Message, State),
    NewState = update_conversation_history(Message, Result, State),
    {reply, Result, NewState};

handle_call({multi_turn_reasoning, ComplexQuestion}, _From, State) ->
    Result = execute_multi_turn_reasoning(ComplexQuestion, State),
    NewState = update_reasoning_stats(Result, State),
    {reply, Result, NewState};

handle_call({smart_search_and_reason, SearchQuery}, _From, State) ->
    Result = execute_smart_search_reasoning(SearchQuery, State),
    NewState = update_tool_usage_stats(Result, State),
    {reply, Result, NewState};

handle_call(get_metrics, _From, State) ->
    Metrics = compile_agent_metrics(State),
    {reply, Metrics, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Intelligent Chat Implementation
%%====================================================================

execute_intelligent_chat(Message, State) ->
    colored_logger:info("Smart agent processing: ~s", [Message]),
    
    % Step 1: Analyze message complexity and requirements
    MessageAnalysis = analyze_message_complexity(Message, State),
    
    % Step 2: Decide optimal reasoning strategy
    Strategy = decide_reasoning_strategy(MessageAnalysis, State),
    
    % Step 3: Execute strategy with optimal tool usage
    case Strategy of
        internal_reasoning ->
            execute_internal_reasoning(Message, State);
        tool_assisted_reasoning ->
            execute_tool_assisted_reasoning(Message, State);
        hybrid_reasoning ->
            execute_hybrid_reasoning(Message, State);
        multi_turn_required ->
            execute_multi_turn_reasoning(Message, State)
    end.

%% Internal reasoning (0 tool calls - optimal efficiency)
execute_internal_reasoning(Message, State) ->
    colored_logger:success("Using internal reasoning - 0 tool calls", []),
    
    % Use internal knowledge and reasoning capabilities
    ReasoningSteps = [
        #reasoning_step{
            step_number = 1,
            reasoning_type = internal,
            input = Message,
            output = <<"Analyzing question using internal knowledge...">>,
            confidence = 0.8,
            tool_calls_used = 0,
            timestamp = erlang:system_time(millisecond)
        }
    ],
    
    % Generate response using internal reasoning
    InternalResponse = generate_internal_response(Message, State),
    
    #{
        response => InternalResponse,
        strategy => internal_reasoning,
        tool_calls => 0,
        reasoning_steps => ReasoningSteps,
        confidence => 0.8,
        productivity_score => 1.0  % Perfect productivity
    }.

%% Tool-assisted reasoning with optimization
execute_tool_assisted_reasoning(Message, State) ->
    colored_logger:info("Using optimized tool-assisted reasoning", []),
    
    % Get optimal tool sequence from optimizer
    AvailableTools = get_available_tools(),
    OptimalSequence = intelligent_tool_optimizer:optimize_tool_sequence(
        Message, AvailableTools, State#state.conversation_history
    ),
    
    % Execute optimized tool sequence
    case maps:get(strategy, OptimalSequence) of
        internal_reasoning ->
            % Optimizer determined internal reasoning is better
            execute_internal_reasoning(Message, State);
        minimal_tool_sequence ->
            % Execute minimal tool sequence
            execute_optimized_tool_sequence(Message, OptimalSequence, State)
    end.

execute_optimized_tool_sequence(Message, OptimalSequence, State) ->
    ToolSequence = maps:get(sequence, OptimalSequence, []),
    colored_logger:info("Executing optimized sequence: ~p tools", [length(ToolSequence)]),
    
    % Execute tools sequentially with context building
    {Results, ReasoningSteps} = execute_tool_sequence_with_reasoning(
        ToolSequence, Message, State, [], []
    ),
    
    % Synthesize final response
    FinalResponse = synthesize_response_from_tools(Results, Message, State),
    
    % Calculate productivity
    ToolCalls = length(ToolSequence),
    Success = is_response_successful(FinalResponse),
    Productivity = case {Success, ToolCalls} of
        {true, 0} -> 1.0;
        {true, N} -> 1.0 / N;
        {false, _} -> 0.0
    end,
    
    % Track productivity
    intelligent_tool_optimizer:track_tool_productivity(
        <<"tool_sequence">>, #{tools => ToolSequence}, FinalResponse, Success
    ),
    
    #{
        response => FinalResponse,
        strategy => tool_assisted_reasoning,
        tool_calls => ToolCalls,
        reasoning_steps => ReasoningSteps,
        confidence => maps:get(estimated_productivity, OptimalSequence, 0.7),
        productivity_score => Productivity
    }.

%% Hybrid reasoning (combination of internal and external)
execute_hybrid_reasoning(Message, State) ->
    colored_logger:info("Using hybrid reasoning strategy", []),
    
    % Step 1: Use internal reasoning first
    InternalResult = execute_internal_reasoning(Message, State),
    InternalConfidence = maps:get(confidence, InternalResult),
    
    case InternalConfidence > 0.7 of
        true ->
            % Internal reasoning sufficient
            colored_logger:success("Internal reasoning sufficient (confidence: ~.3f)", [InternalConfidence]),
            InternalResult#{strategy => hybrid_reasoning};
        false ->
            % Supplement with targeted tool use
            colored_logger:info("Supplementing with targeted tool use", []),
            ToolResult = execute_targeted_tool_supplement(Message, InternalResult, State),
            combine_internal_and_tool_results(InternalResult, ToolResult, State)
    end.

execute_targeted_tool_supplement(Message, InternalResult, State) ->
    % Identify specific information gaps
    InternalResponse = maps:get(response, InternalResult),
    InformationGaps = identify_information_gaps(InternalResponse, Message),
    
    % Use minimal tools to fill gaps
    case InformationGaps of
        [] ->
            InternalResult;
        Gaps ->
            % Execute minimal search for gaps
            SearchQuery = generate_gap_filling_query(Gaps, Message),
            SearchResult = intelligent_search_processor:smart_search(SearchQuery, <<"web_search">>),
            
            #{
                tool_results => SearchResult,
                tool_calls => 1,
                gaps_addressed => Gaps
            }
    end.

%%====================================================================
%% Multi-turn Reasoning Implementation
%%====================================================================

execute_multi_turn_reasoning(ComplexQuestion, State) ->
    colored_logger:info("Starting multi-turn reasoning for complex question", []),
    
    % Plan multi-turn sequence
    TurnPlan = intelligent_search_processor:multi_turn_search(
        ComplexQuestion, State#state.conversation_history
    ),
    
    % Execute turns with progressive reasoning
    TurnResults = execute_reasoning_turns(TurnPlan, State, []),
    
    % Synthesize final comprehensive answer
    FinalAnswer = synthesize_multi_turn_answer(ComplexQuestion, TurnResults, State),
    
    #{
        response => FinalAnswer,
        strategy => multi_turn_reasoning,
        turns => maps:get(turns, TurnPlan),
        tool_calls => count_total_tool_calls(TurnResults),
        reasoning_depth => length(TurnResults),
        confidence => calculate_multi_turn_confidence(TurnResults)
    }.

execute_reasoning_turns(TurnPlan, State, CompletedTurns) ->
    TurnResults = maps:get(turn_results, TurnPlan, []),
    
    lists:foldl(fun(Turn, Acc) ->
        colored_logger:info("Executing reasoning turn: ~s", [maps:get(question, Turn)]),
        
        % Execute turn with accumulated context
        TurnResult = execute_single_reasoning_turn(Turn, Acc, State),
        
        % Track turn completion
        intelligent_tool_optimizer:track_tool_productivity(
            <<"reasoning_turn">>, Turn, TurnResult, is_turn_successful(TurnResult)
        ),
        
        [TurnResult | Acc]
    end, CompletedTurns, TurnResults).

execute_single_reasoning_turn(Turn, PreviousTurns, State) ->
    Question = maps:get(question, Turn),
    
    % Build context from previous turns
    TurnContext = build_turn_context(PreviousTurns),
    
    % Decide if tools needed for this turn
    case intelligent_tool_optimizer:should_use_tool(Question, <<"search">>) of
        {true, Reason} ->
            colored_logger:info("Turn requires tool use: ~s", [Reason]),
            execute_tool_turn(Question, TurnContext, State);
        {false, Reason} ->
            colored_logger:info("Turn uses internal reasoning: ~s", [Reason]),
            execute_internal_turn(Question, TurnContext, State)
    end.

execute_tool_turn(Question, Context, State) ->
    % Use smart search processor
    SearchResult = intelligent_search_processor:sequential_search_until_found(Question, 3),
    
    #{
        question => Question,
        answer => maps:get(answer, SearchResult, <<"No answer found">>),
        method => tool_assisted,
        tool_calls => maps:get(iterations, SearchResult, 1),
        confidence => maps:get(confidence, SearchResult, 0.5),
        context => Context
    }.

execute_internal_turn(Question, Context, State) ->
    % Use internal reasoning with context
    Answer = generate_contextual_internal_response(Question, Context, State),
    
    #{
        question => Question,
        answer => Answer,
        method => internal,
        tool_calls => 0,
        confidence => 0.8,
        context => Context
    }.

%%====================================================================
%% Smart Search and Reasoning
%%====================================================================

execute_smart_search_reasoning(SearchQuery, State) ->
    colored_logger:info("Starting smart search and reasoning", []),
    
    % Use intelligent search processor for sequential search
    SearchResult = intelligent_search_processor:sequential_search_until_found(SearchQuery, 5),
    
    case maps:get(status, SearchResult) of
        complete ->
            % Search found complete answer
            colored_logger:success("Search completed successfully", []),
            #{
                response => maps:get(answer, SearchResult),
                strategy => smart_search,
                tool_calls => maps:get(iterations, SearchResult),
                confidence => maps:get(confidence, SearchResult),
                search_iterations => maps:get(iterations, SearchResult)
            };
        partial ->
            % Search found partial answer - use reasoning to enhance
            colored_logger:info("Enhancing partial search results with reasoning", []),
            PartialAnswer = maps:get(answer, SearchResult),
            EnhancedAnswer = enhance_partial_answer_with_reasoning(PartialAnswer, SearchQuery, State),
            
            #{
                response => EnhancedAnswer,
                strategy => hybrid_search_reasoning,
                tool_calls => maps:get(iterations, SearchResult),
                confidence => maps:get(confidence, SearchResult, 0.6),
                enhancement => <<"reasoning_applied">>
            };
        failed ->
            % Search failed - fall back to internal reasoning
            colored_logger:warning("Search failed - using internal reasoning fallback", []),
            InternalResult = execute_internal_reasoning(SearchQuery, State),
            InternalResult#{
                strategy => fallback_reasoning,
                note => <<"search_failed_internal_fallback">>
            }
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

analyze_message_complexity(Message, State) ->
    MessageBinary = ensure_binary(Message),
    
    % Analyze various complexity factors
    Length = byte_size(MessageBinary),
    QuestionWords = count_question_words(MessageBinary),
    TechnicalTerms = count_technical_terms(MessageBinary),
    ContextRequirement = assess_context_requirement(MessageBinary, State),
    
    ComplexityScore = calculate_complexity_score(Length, QuestionWords, TechnicalTerms, ContextRequirement),
    
    #{
        length => Length,
        question_words => QuestionWords,
        technical_terms => TechnicalTerms,
        context_requirement => ContextRequirement,
        complexity_score => ComplexityScore
    }.

decide_reasoning_strategy(MessageAnalysis, State) ->
    ComplexityScore = maps:get(complexity_score, MessageAnalysis),
    IntelligenceLevel = State#state.intelligence_level,
    
    case {ComplexityScore, IntelligenceLevel} of
        {Score, _} when Score < 0.3 ->
            internal_reasoning;
        {Score, advanced} when Score < 0.7 ->
            internal_reasoning;  % Advanced agents can handle more internally
        {Score, intermediate} when Score < 0.5 ->
            internal_reasoning;
        {Score, _} when Score < 0.8 ->
            tool_assisted_reasoning;
        {Score, _} when Score >= 0.8 ->
            multi_turn_required;
        _ ->
            hybrid_reasoning
    end.

generate_internal_response(Message, State) ->
    % Enhanced internal response generation
    MessageBinary = ensure_binary(Message),
    
    % Check reasoning cache first
    case maps:get(MessageBinary, State#state.reasoning_cache, undefined) of
        undefined ->
            % Generate new response
            Response = create_internal_response(MessageBinary, State),
            Response;
        CachedResponse ->
            colored_logger:info("Using cached internal response", []),
            CachedResponse
    end.

create_internal_response(Message, State) ->
    % Simple internal reasoning (can be enhanced with actual LLM integration)
    QuestionType = classify_question_type(Message),
    
    case QuestionType of
        factual ->
            <<"Based on my knowledge: ">>;
        computational ->
            <<"Let me calculate this: ">>;
        comparison ->
            <<"Comparing these items: ">>;
        _ ->
            <<"After considering your question: ">>
    end.

execute_tool_sequence_with_reasoning([], _Message, _State, Results, Steps) ->
    {lists:reverse(Results), lists:reverse(Steps)};
execute_tool_sequence_with_reasoning([{Tool, Args} | Rest], Message, State, Results, Steps) ->
    colored_logger:info("Executing tool: ~s", [Tool]),
    
    % Execute tool
    ToolResult = execute_tool_with_context(Tool, Args, Results),
    
    % Create reasoning step
    Step = #reasoning_step{
        step_number = length(Steps) + 1,
        reasoning_type = tool_assisted,
        input = iolist_to_binary([Tool, <<": ">>, format_args(Args)]),
        output = format_tool_result(ToolResult),
        confidence = 0.7,
        tool_calls_used = 1,
        timestamp = erlang:system_time(millisecond)
    },
    
    execute_tool_sequence_with_reasoning(Rest, Message, State, [ToolResult | Results], [Step | Steps]).

synthesize_response_from_tools(Results, Message, State) ->
    % Combine tool results into coherent response
    case Results of
        [] ->
            <<"No information found from tools">>;
        [SingleResult] ->
            format_single_tool_result(SingleResult, Message);
        MultipleResults ->
            format_multiple_tool_results(MultipleResults, Message)
    end.

identify_information_gaps(InternalResponse, OriginalMessage) ->
    % Simple gap identification (can be enhanced)
    ResponseBinary = ensure_binary(InternalResponse),
    
    Gaps = [],
    
    % Check for common gap indicators
    case binary:match(ResponseBinary, [<<"uncertain">>, <<"unclear">>, <<"unknown">>]) of
        nomatch -> Gaps;
        _ -> [<<"uncertainty_detected">> | Gaps]
    end.

generate_gap_filling_query(Gaps, OriginalMessage) ->
    % Generate targeted query to fill information gaps
    case Gaps of
        [] -> OriginalMessage;
        [<<"uncertainty_detected">> | _] ->
            iolist_to_binary([OriginalMessage, <<" facts details verification">>]);
        _ ->
            OriginalMessage
    end.

combine_internal_and_tool_results(InternalResult, ToolResult, State) ->
    InternalResponse = maps:get(response, InternalResult),
    ToolData = maps:get(tool_results, ToolResult, #{}),
    
    CombinedResponse = case maps:get(answer, ToolData, undefined) of
        undefined -> InternalResponse;
        ToolAnswer -> iolist_to_binary([InternalResponse, <<" Additionally: ">>, ToolAnswer])
    end,
    
    #{
        response => CombinedResponse,
        strategy => hybrid_reasoning,
        tool_calls => maps:get(tool_calls, ToolResult, 0),
        confidence => (maps:get(confidence, InternalResult) + 0.7) / 2,  % Average confidences
        method => <<"internal_plus_tool_supplement">>
    }.

synthesize_multi_turn_answer(Question, TurnResults, State) ->
    % Synthesize comprehensive answer from multiple turns
    Answers = [maps:get(answer, Turn) || Turn <- TurnResults],
    ValidAnswers = [A || A <- Answers, byte_size(A) > 0],
    
    case ValidAnswers of
        [] ->
            <<"Unable to find comprehensive answer after multi-turn reasoning">>;
        [SingleAnswer] ->
            SingleAnswer;
        MultipleAnswers ->
            synthesize_multiple_answers(Question, MultipleAnswers)
    end.

synthesize_multiple_answers(Question, Answers) ->
    iolist_to_binary([
        <<"Comprehensive analysis: ">>,
        string:join([binary_to_list(A) || A <- lists:sublist(Answers, 3)], " Furthermore, ")
    ]).

build_turn_context(PreviousTurns) ->
    % Build context from previous turn results
    PrevAnswers = [maps:get(answer, Turn) || Turn <- PreviousTurns],
    iolist_to_binary(string:join([binary_to_list(A) || A <- PrevAnswers], " ")).

generate_contextual_internal_response(Question, Context, State) ->
    % Generate response using question and accumulated context
    QuestionBinary = ensure_binary(Question),
    ContextBinary = ensure_binary(Context),
    
    case byte_size(ContextBinary) of
        0 ->
            create_internal_response(QuestionBinary, State);
        _ ->
            iolist_to_binary([
                <<"Based on the context and my reasoning: ">>,
                create_internal_response(QuestionBinary, State)
            ])
    end.

enhance_partial_answer_with_reasoning(PartialAnswer, Query, State) ->
    % Use internal reasoning to enhance partial search results
    EnhancementPrompt = iolist_to_binary([
        <<"Enhancing partial information: ">>, PartialAnswer,
        <<" with additional reasoning about: ">>, Query
    ]),
    
    Enhancement = create_internal_response(EnhancementPrompt, State),
    iolist_to_binary([PartialAnswer, <<" ">>, Enhancement]).

get_available_tools() ->
    % Get list of available tools from agent_tools registry
    case catch agent_tools:list_tools() of
        Tools when is_list(Tools) -> Tools;
        _ -> [<<"search">>, <<"calculator">>, <<"web_search">>]  % Default tools
    end.

execute_tool_with_context(Tool, Args, PreviousResults) ->
    % Execute tool with context from previous results
    case catch agent_tools:execute_tool(Tool, Args) of
        {ok, Result} -> Result;
        _ -> #{error => <<"Tool execution failed">>, tool => Tool}
    end.

format_args(Args) when is_map(Args) ->
    iolist_to_binary(io_lib:format("~p", [Args]));
format_args(Args) ->
    ensure_binary(Args).

format_tool_result(Result) when is_map(Result) ->
    case maps:get(content, Result, undefined) of
        undefined -> iolist_to_binary(io_lib:format("~p", [Result]));
        Content -> ensure_binary(Content)
    end;
format_tool_result(Result) ->
    ensure_binary(Result).

format_single_tool_result(Result, _Message) ->
    format_tool_result(Result).

format_multiple_tool_results(Results, _Message) ->
    FormattedResults = [format_tool_result(R) || R <- Results],
    iolist_to_binary(string:join([binary_to_list(F) || F <- FormattedResults], " ")).

is_response_successful(Response) ->
    ResponseBinary = ensure_binary(Response),
    byte_size(ResponseBinary) > 10 andalso 
    binary:match(ResponseBinary, [<<"error">>, <<"failed">>, <<"unable">>]) == nomatch.

is_turn_successful(Turn) ->
    Answer = maps:get(answer, Turn, <<>>),
    byte_size(Answer) > 5.

count_total_tool_calls(TurnResults) ->
    lists:sum([maps:get(tool_calls, Turn, 0) || Turn <- TurnResults]).

calculate_multi_turn_confidence(TurnResults) ->
    Confidences = [maps:get(confidence, Turn, 0.5) || Turn <- TurnResults],
    case Confidences of
        [] -> 0.5;
        _ -> lists:sum(Confidences) / length(Confidences)
    end.

update_conversation_history(Message, Result, State) ->
    NewEntry = {erlang:system_time(millisecond), Message, Result},
    NewHistory = [NewEntry | lists:sublist(State#state.conversation_history, 19)],
    State#state{conversation_history = NewHistory}.

update_reasoning_stats(Result, State) ->
    ToolCalls = maps:get(tool_calls, Result, 0),
    NewProductivity = case maps:get(confidence, Result, 0.0) of
        Confidence when Confidence > 0.7 andalso ToolCalls == 0 -> 1.0;
        Confidence when Confidence > 0.7 -> 1.0 / max(1, ToolCalls);
        _ -> 0.5 / max(1, ToolCalls)
    end,
    
    State#state{productivity_score = NewProductivity}.

update_tool_usage_stats(Result, State) ->
    ToolCalls = maps:get(tool_calls, Result, 0),
    Strategy = maps:get(strategy, Result, unknown),
    
    CurrentStats = State#state.tool_usage_stats,
    NewStats = CurrentStats#{
        total_calls => maps:get(total_calls, CurrentStats, 0) + ToolCalls,
        last_strategy => Strategy,
        last_productivity => maps:get(productivity_score, Result, 0.5)
    },
    
    State#state{tool_usage_stats = NewStats}.

compile_agent_metrics(State) ->
    OptimizationMetrics = intelligent_tool_optimizer:get_tool_productivity(),
    
    #{
        agent_id => State#state.agent_id,
        intelligence_level => State#state.intelligence_level,
        productivity_score => State#state.productivity_score,
        tool_usage_stats => State#state.tool_usage_stats,
        conversation_count => length(State#state.conversation_history),
        optimization_metrics => OptimizationMetrics,
        reasoning_cache_size => maps:size(State#state.reasoning_cache)
    }.

%% Helper functions for complexity analysis
count_question_words(Message) ->
    QuestionWords = [<<"what">>, <<"why">>, <<"how">>, <<"when">>, <<"where">>, <<"who">>, <<"which">>],
    MessageLower = string:lowercase(binary_to_list(Message)),
    lists:sum([
        case string:find(MessageLower, binary_to_list(QW)) of
            nomatch -> 0;
            _ -> 1
        end || QW <- QuestionWords
    ]).

count_technical_terms(Message) ->
    TechnicalWords = [<<"algorithm">>, <<"implementation">>, <<"architecture">>, <<"optimization">>, <<"system">>],
    MessageLower = string:lowercase(binary_to_list(Message)),
    lists:sum([
        case string:find(MessageLower, binary_to_list(TW)) of
            nomatch -> 0;
            _ -> 1
        end || TW <- TechnicalWords
    ]).

assess_context_requirement(Message, State) ->
    % Check if message references previous conversation
    MessageLower = string:lowercase(binary_to_list(Message)),
    ContextIndicators = ["this", "that", "previous", "earlier", "above", "mentioned"],
    
    ContextScore = lists:sum([
        case string:find(MessageLower, Indicator) of
            nomatch -> 0;
            _ -> 1
        end || Indicator <- ContextIndicators
    ]),
    
    ContextScore / length(ContextIndicators).

calculate_complexity_score(Length, QuestionWords, TechnicalTerms, ContextRequirement) ->
    % Normalize and combine factors
    LengthScore = min(1.0, Length / 200),  % Normalize to 200 chars
    QuestionScore = min(1.0, QuestionWords / 3),  % Normalize to 3 question words
    TechnicalScore = min(1.0, TechnicalTerms / 2),  % Normalize to 2 technical terms
    
    % Weighted average
    (LengthScore * 0.2 + QuestionScore * 0.3 + TechnicalScore * 0.3 + ContextRequirement * 0.2).

classify_question_type(Question) ->
    QuestionLower = string:lowercase(binary_to_list(Question)),
    
    case {string:find(QuestionLower, "what"), 
          string:find(QuestionLower, "calculate"),
          string:find(QuestionLower, "compare"),
          string:find(QuestionLower, "why")} of
        {Match, _, _, _} when Match =/= nomatch -> factual;
        {_, Match, _, _} when Match =/= nomatch -> computational;
        {_, _, Match, _} when Match =/= nomatch -> comparison;
        {_, _, _, Match} when Match =/= nomatch -> causal;
        _ -> general
    end.

get_smart_system_prompt() ->
    <<"You are an intelligent reasoning agent that optimizes tool usage following OTC-PO principles. "
      "You prefer internal reasoning when possible (0 tool calls = optimal), use minimal tool calls when external information is needed, "
      "and process information sequentially until finding comprehensive answers. "
      "You demonstrate smart multi-turn function calling and build context progressively across reasoning steps.">>.

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) -> list_to_binary(io_lib:format("~p", [Value])).