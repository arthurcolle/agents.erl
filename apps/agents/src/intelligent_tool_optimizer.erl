%% intelligent_tool_optimizer.erl
%% Implementation of OTC-PO (Optimal Tool Call-controlled Policy Optimization)
%% Based on "Acting Less is Reasoning More!" research
-module(intelligent_tool_optimizer).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    optimize_tool_sequence/3,
    track_tool_productivity/4,
    should_use_tool/2,
    get_tool_productivity/0,
    reset_metrics/0,
    plan_multi_turn_sequence/2,
    evaluate_reasoning_capability/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    tool_calls = 0 :: integer(),           % Total tool calls made
    successful_results = 0 :: integer(),   % Successful tool calls  
    reasoning_cache = #{} :: map(),        % Cache of internal reasoning results
    tool_productivity_history = [] :: list(), % History of productivity metrics
    cognitive_load_threshold = 0.7 :: float(), % When to prefer internal reasoning
    max_sequential_calls = 3 :: integer(),     % Max tool calls per reasoning chain
    context_window = [] :: list()              % Recent context for sequential reasoning
}).

%% Tool productivity metric: TP = Successful Results / Total Tool Calls
-define(CALCULATE_PRODUCTIVITY(Successful, Total), 
    case Total of
        0 -> 1.0;  % Perfect productivity when no tools used
        _ -> Successful / Total
    end).

%% Reward function for tool efficiency (from OTC-PO paper)
-define(TOOL_EFFICIENCY_REWARD(ToolCalls, OptimalCalls),
    case OptimalCalls of
        0 when ToolCalls == 0 -> 1.0;
        0 -> math:cos(ToolCalls * math:pi() / (2 * ToolCalls + 1));
        _ -> math:sin((2 * OptimalCalls * ToolCalls / (ToolCalls + OptimalCalls)) * math:pi() / (2 * OptimalCalls))
    end).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Main optimization function - decides optimal tool sequence
optimize_tool_sequence(Question, AvailableTools, Context) ->
    gen_server:call(?MODULE, {optimize_sequence, Question, AvailableTools, Context}).

%% Track productivity metrics for learning
track_tool_productivity(ToolName, Arguments, Result, Success) ->
    gen_server:cast(?MODULE, {track_productivity, ToolName, Arguments, Result, Success}).

%% Intelligent decision: should we use a tool or rely on internal reasoning?
should_use_tool(Question, PotentialTool) ->
    gen_server:call(?MODULE, {should_use_tool, Question, PotentialTool}).

%% Get current tool productivity metrics
get_tool_productivity() ->
    gen_server:call(?MODULE, get_productivity).

%% Reset metrics for new session
reset_metrics() ->
    gen_server:cast(?MODULE, reset_metrics).

%% Plan multi-turn function calling sequence (addresses your multi-turn issue)
plan_multi_turn_sequence(Question, AvailableTools) ->
    gen_server:call(?MODULE, {plan_sequence, Question, AvailableTools}).

%% Evaluate if we have internal reasoning capability for this question
evaluate_reasoning_capability(Question, Domain) ->
    gen_server:call(?MODULE, {evaluate_reasoning, Question, Domain}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    colored_logger:success("Starting Intelligent Tool Optimizer with OTC-PO", []),
    {ok, #state{}}.

handle_call({optimize_sequence, Question, AvailableTools, Context}, _From, State) ->
    OptimizedSequence = plan_optimal_sequence(Question, AvailableTools, Context, State),
    {reply, OptimizedSequence, State};

handle_call({should_use_tool, Question, PotentialTool}, _From, State) ->
    Decision = make_tool_decision(Question, PotentialTool, State),
    {reply, Decision, State};

handle_call(get_productivity, _From, State) ->
    Productivity = ?CALCULATE_PRODUCTIVITY(State#state.successful_results, State#state.tool_calls),
    Metrics = #{
        tool_productivity => Productivity,
        total_calls => State#state.tool_calls,
        successful_calls => State#state.successful_results,
        efficiency_score => calculate_efficiency_score(State)
    },
    {reply, Metrics, State};

handle_call({plan_sequence, Question, AvailableTools}, _From, State) ->
    Sequence = plan_multi_turn_reasoning(Question, AvailableTools, State),
    {reply, Sequence, State};

handle_call({evaluate_reasoning, Question, Domain}, _From, State) ->
    CanReason = can_reason_internally(Question, Domain, State),
    {reply, CanReason, State}.

handle_cast({track_productivity, ToolName, Arguments, Result, Success}, State) ->
    NewState = update_productivity_metrics(ToolName, Arguments, Result, Success, State),
    {noreply, NewState};

handle_cast(reset_metrics, State) ->
    colored_logger:info("Resetting tool productivity metrics", []),
    {noreply, State#state{
        tool_calls = 0,
        successful_results = 0,
        tool_productivity_history = [],
        context_window = []
    }}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Implementing OTC-PO concepts
%%====================================================================

%% Plan optimal tool sequence following OTC-PO principles
plan_optimal_sequence(Question, AvailableTools, Context, State) ->
    colored_logger:info("Planning optimal tool sequence for: ~s", [binary_to_list(Question)]),
    
    % Step 1: Evaluate if we can answer with internal reasoning (0 tool calls = optimal)
    case can_reason_internally(Question, unknown, State) of
        {true, Confidence} when Confidence > State#state.cognitive_load_threshold ->
            colored_logger:success("Using internal reasoning - 0 tool calls needed", []),
            #{
                strategy => internal_reasoning,
                tool_calls => 0,
                confidence => Confidence,
                reasoning => <<"Question can be answered using internal knowledge">>
            };
        _ ->
            % Step 2: Plan minimal tool sequence
            plan_minimal_tool_sequence(Question, AvailableTools, Context, State)
    end.

%% Plan minimal tool sequence (core OTC-PO implementation)
plan_minimal_tool_sequence(Question, AvailableTools, Context, State) ->
    % Analyze question complexity and information needs
    InfoNeeds = analyze_information_needs(Question),
    
    % Select most efficient tools for each need
    OptimalTools = select_optimal_tools(InfoNeeds, AvailableTools),
    
    % Plan sequential execution to minimize redundancy
    Sequence = plan_sequential_execution(OptimalTools, Context),
    
    colored_logger:info("Planned minimal sequence: ~p tools needed", [length(Sequence)]),
    
    #{
        strategy => minimal_tool_sequence,
        tool_calls => length(Sequence),
        sequence => Sequence,
        estimated_productivity => estimate_productivity(Sequence, State)
    }.

%% Intelligent decision making (addresses "not smart" issue)
make_tool_decision(Question, PotentialTool, State) ->
    % Calculate current productivity
    CurrentProductivity = ?CALCULATE_PRODUCTIVITY(State#state.successful_results, State#state.tool_calls),
    
    % Estimate tool necessity
    Necessity = estimate_tool_necessity(Question, PotentialTool),
    
    % Apply OTC-PO decision logic
    Decision = case {CurrentProductivity, Necessity} of
        {P, N} when P > 0.8 andalso N < 0.5 ->
            % High productivity, low necessity - prefer internal reasoning
            {false, <<"High productivity achieved - using internal reasoning">>};
        {P, N} when P < 0.3 andalso N > 0.7 ->
            % Low productivity, high necessity - use tool but optimize
            {true, <<"Low productivity - optimizing tool use">>};
        {_, N} when N > 0.8 ->
            % High necessity - tool required
            {true, <<"Tool necessary for accurate answer">>};
        _ ->
            % Default to internal reasoning (OTC-PO bias)
            {false, <<"Defaulting to internal reasoning for efficiency">>}
    end,
    
    colored_logger:info("Tool decision for ~s: ~p (productivity: ~.3f, necessity: ~.3f)", 
        [PotentialTool, element(1, Decision), CurrentProductivity, Necessity]),
    Decision.

%% Multi-turn reasoning planner (addresses your multi-turn function calling issue)
plan_multi_turn_reasoning(Question, AvailableTools, State) ->
    colored_logger:info("Planning multi-turn reasoning sequence", []),
    
    % Break down complex question into sub-questions
    SubQuestions = decompose_question(Question),
    
    % Plan tool sequence for each sub-question
    TurnPlans = lists:map(fun(SubQ) ->
        plan_turn(SubQ, AvailableTools, State)
    end, SubQuestions),
    
    % Optimize sequence to minimize total tool calls
    OptimizedSequence = optimize_turn_sequence(TurnPlans),
    
    colored_logger:success("Multi-turn sequence planned: ~p turns, ~p total tools", 
        [length(OptimizedSequence), count_total_tools(OptimizedSequence)]),
    
    #{
        turns => OptimizedSequence,
        total_tool_calls => count_total_tools(OptimizedSequence),
        estimated_success_rate => estimate_sequence_success(OptimizedSequence, State)
    }.

%% Update productivity metrics following OTC-PO reward structure
update_productivity_metrics(ToolName, Arguments, Result, Success, State) ->
    NewToolCalls = State#state.tool_calls + 1,
    NewSuccessful = case Success of
        true -> State#state.successful_results + 1;
        false -> State#state.successful_results
    end,
    
    % Calculate new productivity
    NewProductivity = ?CALCULATE_PRODUCTIVITY(NewSuccessful, NewToolCalls),
    
    % Apply OTC-PO reward calculation
    OptimalCalls = estimate_optimal_calls_for_context(State#state.context_window),
    EfficiencyReward = ?TOOL_EFFICIENCY_REWARD(NewToolCalls, OptimalCalls),
    
    % Update history
    NewHistory = [{erlang:system_time(millisecond), NewProductivity, EfficiencyReward} | 
                  lists:sublist(State#state.tool_productivity_history, 99)],
    
    colored_logger:info("Tool productivity updated: ~.3f (efficiency reward: ~.3f)", 
        [NewProductivity, EfficiencyReward]),
    
    State#state{
        tool_calls = NewToolCalls,
        successful_results = NewSuccessful,
        tool_productivity_history = NewHistory
    }.

%% Internal reasoning capability evaluation
can_reason_internally(Question, Domain, State) ->
    % Analyze question for internal reasoning capability
    QuestionBinary = ensure_binary(Question),
    
    % Simple heuristics (can be enhanced with ML models)
    InternalReasoningScore = case {Domain, analyze_question_type(QuestionBinary)} of
        {math, arithmetic} -> 0.9;
        {general, factual} -> check_knowledge_cache(QuestionBinary, State);
        {logic, reasoning} -> 0.8;
        {current_events, _} -> 0.1;  % Need external search
        {technical, specific} -> 0.3;
        _ -> 0.5
    end,
    
    {InternalReasoningScore > 0.6, InternalReasoningScore}.

%% Question analysis and decomposition
decompose_question(Question) ->
    % Simple decomposition (can be enhanced)
    QuestionBinary = ensure_binary(Question),
    case binary:match(QuestionBinary, [<<"and">>, <<"also">>, <<"additionally">>]) of
        nomatch -> [QuestionBinary];
        _ -> 
            % Split complex question into parts
            Parts = binary:split(QuestionBinary, [<<" and ">>, <<" also ">>, <<" additionally ">>], [global]),
            [string:trim(Part) || Part <- Parts, byte_size(Part) > 0]
    end.

%% Helper functions
analyze_information_needs(Question) ->
    QuestionBinary = ensure_binary(Question),
    
    % Identify what type of information is needed
    case analyze_question_type(QuestionBinary) of
        factual -> [current_facts, verification];
        computational -> [calculation, verification];
        research -> [search, synthesis, verification];
        comparison -> [multiple_sources, analysis];
        _ -> [search]
    end.

analyze_question_type(Question) ->
    QuestionLower = string:lowercase(binary_to_list(Question)),
    
    case {string:find(QuestionLower, "calculate"), 
          string:find(QuestionLower, "when"),
          string:find(QuestionLower, "compare"),
          string:find(QuestionLower, "research")} of
        {nomatch, nomatch, nomatch, nomatch} -> factual;
        {Match, _, _, _} when Match =/= nomatch -> computational;
        {_, Match, _, _} when Match =/= nomatch -> temporal;
        {_, _, Match, _} when Match =/= nomatch -> comparison;
        {_, _, _, Match} when Match =/= nomatch -> research;
        _ -> factual
    end.

select_optimal_tools(InfoNeeds, AvailableTools) ->
    % Map information needs to most efficient tools
    ToolMap = #{
        current_facts => [<<"search">>, <<"web_search">>],
        calculation => [<<"calculator">>, <<"code_interpreter">>],
        verification => [<<"search">>, <<"fact_check">>],
        synthesis => [<<"summarizer">>, <<"analyzer">>]
    },
    
    % Select best available tool for each need
    lists:foldl(fun(Need, Acc) ->
        case maps:get(Need, ToolMap, []) of
            [] -> Acc;
            PotentialTools ->
                BestTool = select_best_available_tool(PotentialTools, AvailableTools),
                case BestTool of
                    undefined -> Acc;
                    Tool -> [Tool | Acc]
                end
        end
    end, [], InfoNeeds).

select_best_available_tool(PotentialTools, AvailableTools) ->
    AvailableList = [ensure_binary(Tool) || Tool <- AvailableTools],
    case [Tool || Tool <- PotentialTools, lists:member(Tool, AvailableList)] of
        [] -> undefined;
        [Best | _] -> Best
    end.

plan_sequential_execution(Tools, Context) ->
    % Plan tools in logical sequence to build context
    UniqueTools = lists:usort(Tools),
    
    % Order tools by dependency and efficiency
    Ordered = lists:sort(fun(A, B) ->
        tool_priority(A) =< tool_priority(B)
    end, UniqueTools),
    
    % Create execution plan
    [{Tool, generate_tool_args(Tool, Context)} || Tool <- Ordered].

tool_priority(<<"search">>) -> 1;
tool_priority(<<"web_search">>) -> 1;
tool_priority(<<"calculator">>) -> 2;
tool_priority(<<"code_interpreter">>) -> 2;
tool_priority(<<"summarizer">>) -> 3;
tool_priority(<<"analyzer">>) -> 4;
tool_priority(_) -> 5.

generate_tool_args(Tool, Context) ->
    % Generate optimized arguments based on context
    case Tool of
        <<"search">> -> #{query => extract_search_terms(Context)};
        <<"calculator">> -> #{expression => extract_calculation(Context)};
        _ -> #{}
    end.

plan_turn(SubQuestion, AvailableTools, State) ->
    % Plan single turn with minimal tools
    case can_reason_internally(SubQuestion, unknown, State) of
        {true, Confidence} when Confidence > 0.7 ->
            #{type => internal, question => SubQuestion, tools => []};
        _ ->
            InfoNeeds = analyze_information_needs(SubQuestion),
            Tools = select_optimal_tools(InfoNeeds, AvailableTools),
            #{type => external, question => SubQuestion, tools => Tools}
    end.

optimize_turn_sequence(TurnPlans) ->
    % Merge adjacent external turns to reduce tool switching overhead
    merge_adjacent_external_turns(TurnPlans).

merge_adjacent_external_turns([]) -> [];
merge_adjacent_external_turns([Turn]) -> [Turn];
merge_adjacent_external_turns([Turn1, Turn2 | Rest]) ->
    case {maps:get(type, Turn1), maps:get(type, Turn2)} of
        {external, external} ->
            % Merge external turns
            MergedTools = lists:usort(maps:get(tools, Turn1) ++ maps:get(tools, Turn2)),
            MergedTurn = #{
                type => external,
                question => iolist_to_binary([maps:get(question, Turn1), <<" AND ">>, maps:get(question, Turn2)]),
                tools => MergedTools
            },
            merge_adjacent_external_turns([MergedTurn | Rest]);
        _ ->
            [Turn1 | merge_adjacent_external_turns([Turn2 | Rest])]
    end.

count_total_tools(TurnPlans) ->
    lists:sum([length(maps:get(tools, Turn, [])) || Turn <- TurnPlans]).

estimate_productivity(Sequence, State) ->
    CurrentProductivity = ?CALCULATE_PRODUCTIVITY(State#state.successful_results, State#state.tool_calls),
    % Estimate based on sequence length and current performance
    EstimatedCalls = length(Sequence),
    case EstimatedCalls of
        0 -> 1.0;  % Perfect productivity for internal reasoning
        _ -> max(0.1, CurrentProductivity * (1.0 / EstimatedCalls))
    end.

estimate_sequence_success(TurnPlans, State) ->
    % Estimate success based on turn complexity and tool reliability
    BaseSuccess = ?CALCULATE_PRODUCTIVITY(State#state.successful_results, State#state.tool_calls),
    TurnComplexity = length(TurnPlans),
    max(0.1, BaseSuccess * (1.0 / math:sqrt(TurnComplexity))).

estimate_tool_necessity(Question, Tool) ->
    % Heuristic for tool necessity
    QuestionBinary = ensure_binary(Question),
    ToolBinary = ensure_binary(Tool),
    
    case {analyze_question_type(QuestionBinary), ToolBinary} of
        {computational, <<"calculator">>} -> 0.9;
        {research, <<"search">>} -> 0.8;
        {factual, <<"search">>} -> 0.6;
        {temporal, <<"search">>} -> 0.9;
        _ -> 0.5
    end.

calculate_efficiency_score(State) ->
    Productivity = ?CALCULATE_PRODUCTIVITY(State#state.successful_results, State#state.tool_calls),
    AvgCalls = case length(State#state.tool_productivity_history) of
        0 -> 1.0;
        N -> State#state.tool_calls / N
    end,
    Productivity * (1.0 / AvgCalls).

estimate_optimal_calls_for_context(Context) ->
    % Estimate optimal calls based on context complexity
    case length(Context) of
        0 -> 0;
        N when N =< 2 -> 1;
        N when N =< 5 -> 2;
        _ -> 3
    end.

check_knowledge_cache(Question, State) ->
    % Check if we have cached knowledge for this question
    case maps:get(Question, State#state.reasoning_cache, undefined) of
        undefined -> 0.3;  % No cached knowledge
        _ -> 0.8  % Cached knowledge available
    end.

extract_search_terms(Context) ->
    case Context of
        [] -> <<"general search">>;
        [Latest | _] -> Latest
    end.

extract_calculation(_Context) ->
    <<"basic calculation">>.

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) -> list_to_binary(io_lib:format("~p", [Value])).