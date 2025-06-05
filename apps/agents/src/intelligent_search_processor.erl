%% intelligent_search_processor.erl
%% Smart search processor that gets results and processes them sequentially until finding answers
%% Addresses the user's concern: "doesn't get search results, and sequentially process them until its found answers"
-module(intelligent_search_processor).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    smart_search/2,
    sequential_search_until_found/2,
    multi_turn_search/2,
    analyze_search_results/2,
    build_comprehensive_answer/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    search_history = [] :: list(),
    knowledge_graph = #{} :: map(),
    answer_confidence_threshold = 0.8 :: float(),
    max_search_iterations = 5 :: integer(),
    context_window = [] :: list()
}).

-record(search_result, {
    query :: binary(),
    results :: list(),
    relevance_score :: float(),
    timestamp :: integer(),
    source :: binary()
}).

-record(knowledge_piece, {
    content :: binary(),
    confidence :: float(),
    source :: binary(),
    context :: binary()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Main smart search function that processes results sequentially
smart_search(Question, SearchEngine) ->
    gen_server:call(?MODULE, {smart_search, Question, SearchEngine}, 30000).

%% Sequential search until comprehensive answer is found
sequential_search_until_found(Question, MaxIterations) ->
    gen_server:call(?MODULE, {sequential_search, Question, MaxIterations}, 60000).

%% Multi-turn search with context building
multi_turn_search(Question, Context) ->
    gen_server:call(?MODULE, {multi_turn_search, Question, Context}, 45000).

%% Analyze search results for relevance and completeness
analyze_search_results(Results, Question) ->
    gen_server:call(?MODULE, {analyze_results, Results, Question}).

%% Build comprehensive answer from multiple search iterations
build_comprehensive_answer(Question, SearchHistory) ->
    gen_server:call(?MODULE, {build_answer, Question, SearchHistory}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    colored_logger:success("Starting Intelligent Search Processor", []),
    {ok, #state{}}.

handle_call({smart_search, Question, SearchEngine}, _From, State) ->
    Result = execute_smart_search(Question, SearchEngine, State),
    {reply, Result, State};

handle_call({sequential_search, Question, MaxIterations}, _From, State) ->
    Result = execute_sequential_search(Question, MaxIterations, State),
    NewState = update_search_history(Result, State),
    {reply, Result, NewState};

handle_call({multi_turn_search, Question, Context}, _From, State) ->
    Result = execute_multi_turn_search(Question, Context, State),
    NewState = update_context_window(Question, Context, State),
    {reply, Result, NewState};

handle_call({analyze_results, Results, Question}, _From, State) ->
    Analysis = score_results_by_relevance(Results, Question),
    {reply, Analysis, State};

handle_call({build_answer, Question, SearchHistory}, _From, State) ->
    Answer = synthesize_comprehensive_answer(Question, SearchHistory),
    {reply, Answer, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Smart Search Implementation
%%====================================================================

%% Execute intelligent search with result processing
execute_smart_search(Question, SearchEngine, State) ->
    colored_logger:info("Starting smart search for: ~s", [Question]),
    
    % Step 1: Generate optimal search query
    OptimalQuery = generate_optimal_query(Question, State),
    colored_logger:info("Generated optimal query: ~s", [OptimalQuery]),
    
    % Step 2: Execute search
    SearchResults = perform_search(OptimalQuery, SearchEngine),
    
    % Step 3: Process and analyze results
    ProcessedResults = process_search_results(SearchResults, Question),
    
    % Step 4: Extract knowledge pieces
    KnowledgePieces = extract_knowledge_from_results(ProcessedResults, Question),
    
    % Step 5: Evaluate if we have sufficient information
    case evaluate_answer_completeness(KnowledgePieces, Question) of
        {complete, Answer, Confidence} ->
            colored_logger:success("Complete answer found with confidence: ~.3f", [Confidence]),
            #{
                status => complete,
                answer => Answer,
                confidence => Confidence,
                search_iterations => 1,
                knowledge_sources => length(KnowledgePieces)
            };
        {incomplete, PartialAnswer, MissingInfo} ->
            colored_logger:warning("Incomplete answer - missing: ~p", [MissingInfo]),
            #{
                status => incomplete,
                partial_answer => PartialAnswer,
                missing_info => MissingInfo,
                requires_additional_search => true,
                suggested_queries => generate_followup_queries(MissingInfo, Question)
            }
    end.

%% Sequential search until comprehensive answer is found
execute_sequential_search(Question, MaxIterations, State) ->
    colored_logger:info("Starting sequential search (max ~p iterations)", [MaxIterations]),
    
    InitialContext = #{
        question => Question,
        knowledge_graph => #{},
        search_count => 0,
        confidence_threshold => State#state.answer_confidence_threshold
    },
    
    sequential_search_loop(InitialContext, MaxIterations, []).

sequential_search_loop(Context, MaxIterations, SearchHistory) ->
    SearchCount = maps:get(search_count, Context),
    
    case SearchCount >= MaxIterations of
        true ->
            colored_logger:warning("Reached maximum search iterations (~p)", [MaxIterations]),
            finalize_sequential_search(Context, SearchHistory);
        false ->
            % Execute next search iteration
            CurrentQuery = generate_next_query(Context, SearchHistory),
            colored_logger:info("Search iteration ~p: ~s", [SearchCount + 1, CurrentQuery]),
            
            % Perform search
            Results = perform_search(CurrentQuery, <<"web_search">>),
            ProcessedResults = process_search_results(Results, maps:get(question, Context)),
            
            % Update knowledge graph
            UpdatedKnowledge = update_knowledge_graph(
                ProcessedResults, 
                maps:get(knowledge_graph, Context)
            ),
            
            % Evaluate completeness
            case evaluate_knowledge_completeness(UpdatedKnowledge, maps:get(question, Context)) of
                {complete, Answer, Confidence} ->
                    colored_logger:success("Sequential search complete after ~p iterations", [SearchCount + 1]),
                    #{
                        status => complete,
                        answer => Answer,
                        confidence => Confidence,
                        iterations => SearchCount + 1,
                        search_history => SearchHistory ++ [CurrentQuery],
                        knowledge_graph => UpdatedKnowledge
                    };
                {incomplete, _PartialAnswer, MissingInfo} ->
                    % Continue searching
                    NewContext = Context#{
                        knowledge_graph => UpdatedKnowledge,
                        search_count => SearchCount + 1,
                        missing_info => MissingInfo
                    },
                    sequential_search_loop(NewContext, MaxIterations, SearchHistory ++ [CurrentQuery])
            end
    end.

%% Multi-turn search with context building
execute_multi_turn_search(Question, Context, State) ->
    colored_logger:info("Starting multi-turn search with context", []),
    
    % Decompose question into sub-questions
    SubQuestions = decompose_complex_question(Question, Context),
    colored_logger:info("Decomposed into ~p sub-questions", [length(SubQuestions)]),
    
    % Process each sub-question sequentially, building context
    TurnResults = process_turns_sequentially(SubQuestions, #{}, []),
    
    % Synthesize final answer from all turns
    FinalAnswer = synthesize_multi_turn_answer(Question, TurnResults),
    
    #{
        status => complete,
        answer => FinalAnswer,
        turns => length(SubQuestions),
        turn_results => TurnResults,
        context_built => true
    }.

%%====================================================================
%% Core Processing Functions
%%====================================================================

%% Generate optimal search query using intelligent analysis
generate_optimal_query(Question, State) ->
    QuestionBinary = ensure_binary(Question),
    
    % Analyze question type and extract key terms
    KeyTerms = extract_key_terms(QuestionBinary),
    QuestionType = classify_question_type(QuestionBinary),
    
    % Consider search history to avoid redundancy
    RecentQueries = [Q || {Q, _} <- lists:sublist(State#state.search_history, 3)],
    
    % Generate query based on type and context
    OptimalQuery = case QuestionType of
        factual ->
            build_factual_query(KeyTerms, RecentQueries);
        comparative ->
            build_comparative_query(KeyTerms, RecentQueries);
        temporal ->
            build_temporal_query(KeyTerms, RecentQueries);
        causal ->
            build_causal_query(KeyTerms, RecentQueries);
        _ ->
            build_general_query(KeyTerms, RecentQueries)
    end,
    
    ensure_binary(OptimalQuery).

%% Process search results intelligently
process_search_results(RawResults, Question) ->
    colored_logger:info("Processing ~p search results", [length(RawResults)]),
    
    % Score results by relevance
    ScoredResults = score_results_by_relevance(RawResults, Question),
    
    % Filter low-quality results
    QualityResults = filter_quality_results(ScoredResults),
    
    % Extract structured information
    StructuredResults = extract_structured_info(QualityResults),
    
    colored_logger:info("Processed to ~p high-quality results", [length(StructuredResults)]),
    StructuredResults.

%% Extract knowledge pieces from processed results
extract_knowledge_from_results(Results, Question) ->
    lists:foldl(fun(Result, Acc) ->
        Knowledge = extract_knowledge_piece(Result, Question),
        case Knowledge of
            undefined -> Acc;
            K -> [K | Acc]
        end
    end, [], Results).

extract_knowledge_piece(Result, Question) ->
    % Extract relevant knowledge based on question context
    Content = maps:get(content, Result, <<>>),
    Relevance = maps:get(relevance_score, Result, 0.0),
    
    case Relevance > 0.3 of
        true ->
            #knowledge_piece{
                content = Content,
                confidence = Relevance,
                source = maps:get(source, Result, <<"unknown">>),
                context = Question
            };
        false ->
            undefined
    end.

%% Evaluate if we have complete answer
evaluate_answer_completeness(KnowledgePieces, Question) ->
    % Combine knowledge pieces
    CombinedKnowledge = combine_knowledge_pieces(KnowledgePieces),
    
    % Calculate overall confidence
    OverallConfidence = calculate_overall_confidence(KnowledgePieces),
    
    % Check for completeness
    case {has_sufficient_information(CombinedKnowledge, Question), OverallConfidence > 0.8} of
        {true, true} ->
            Answer = synthesize_answer_from_knowledge(CombinedKnowledge, Question),
            {complete, Answer, OverallConfidence};
        {false, _} ->
            MissingInfo = identify_missing_information(CombinedKnowledge, Question),
            PartialAnswer = synthesize_answer_from_knowledge(CombinedKnowledge, Question),
            {incomplete, PartialAnswer, MissingInfo};
        {true, false} ->
            MissingInfo = [<<"requires_higher_confidence_sources">>],
            PartialAnswer = synthesize_answer_from_knowledge(CombinedKnowledge, Question),
            {incomplete, PartialAnswer, MissingInfo}
    end.

%% Generate follow-up queries for missing information
generate_followup_queries(MissingInfo, OriginalQuestion) ->
    lists:map(fun(MissingPiece) ->
        generate_specific_query(MissingPiece, OriginalQuestion)
    end, MissingInfo).

generate_specific_query(MissingPiece, OriginalQuestion) ->
    case MissingPiece of
        <<"timeline">> ->
            iolist_to_binary([OriginalQuestion, <<" timeline chronology when">>]);
        <<"cause">> ->
            iolist_to_binary([OriginalQuestion, <<" why reason cause">>]);
        <<"location">> ->
            iolist_to_binary([OriginalQuestion, <<" where location place">>]);
        <<"process">> ->
            iolist_to_binary([OriginalQuestion, <<" how process method">>]);
        _ ->
            iolist_to_binary([OriginalQuestion, <<" ">>, MissingPiece])
    end.

%%====================================================================
%% Multi-turn Processing
%%====================================================================

decompose_complex_question(Question, Context) ->
    QuestionBinary = ensure_binary(Question),
    
    % Simple decomposition strategy (can be enhanced with NLP)
    case binary:match(QuestionBinary, [<<"and">>, <<"also">>, <<"furthermore">>, <<"additionally">>]) of
        nomatch ->
            % Single question - break down by information needs
            break_down_by_info_needs(QuestionBinary);
        _ ->
            % Multiple parts question
            binary:split(QuestionBinary, [<<" and ">>, <<" also ">>, <<" furthermore ">>, <<" additionally ">>], [global])
    end.

break_down_by_info_needs(Question) ->
    % Identify what information is needed
    case classify_question_type(Question) of
        comparative ->
            [
                iolist_to_binary([Question, <<" definition first item">>]),
                iolist_to_binary([Question, <<" definition second item">>]),
                iolist_to_binary([Question, <<" comparison analysis">>])
            ];
        causal ->
            [
                iolist_to_binary([Question, <<" background context">>]),
                iolist_to_binary([Question, <<" direct causes">>]),
                iolist_to_binary([Question, <<" effects outcomes">>])
            ];
        temporal ->
            [
                iolist_to_binary([Question, <<" timeline chronology">>]),
                iolist_to_binary([Question, <<" current status">>])
            ];
        _ ->
            [Question]  % Single turn sufficient
    end.

process_turns_sequentially([], _BuildingContext, TurnResults) ->
    lists:reverse(TurnResults);
process_turns_sequentially([SubQuestion | Rest], BuildingContext, TurnResults) ->
    colored_logger:info("Processing turn: ~s", [SubQuestion]),
    
    % Search for this sub-question
    SearchResult = execute_smart_search(SubQuestion, <<"web_search">>, #state{}),
    
    % Extract key information
    TurnInfo = extract_turn_information(SearchResult, SubQuestion),
    
    % Update building context
    NewContext = update_building_context(TurnInfo, BuildingContext),
    
    % Continue with next turn
    process_turns_sequentially(Rest, NewContext, [TurnInfo | TurnResults]).

extract_turn_information(SearchResult, SubQuestion) ->
    #{
        question => SubQuestion,
        answer => maps:get(answer, SearchResult, <<"No answer found">>),
        confidence => maps:get(confidence, SearchResult, 0.0),
        key_facts => extract_key_facts(SearchResult)
    }.

extract_key_facts(SearchResult) ->
    % Extract key facts from search result
    case maps:get(status, SearchResult) of
        complete ->
            Answer = maps:get(answer, SearchResult, <<>>),
            extract_facts_from_text(Answer);
        _ ->
            []
    end.

extract_facts_from_text(Text) ->
    % Simple fact extraction (can be enhanced)
    Sentences = binary:split(Text, [<<".">>, <<"!">>, <<"?">>], [global]),
    [string:trim(S) || S <- Sentences, byte_size(S) > 10].

synthesize_multi_turn_answer(Question, TurnResults) ->
    % Combine information from all turns
    AllAnswers = [maps:get(answer, Turn) || Turn <- TurnResults],
    AllFacts = lists:flatten([maps:get(key_facts, Turn, []) || Turn <- TurnResults]),
    
    % Build comprehensive answer
    synthesize_comprehensive_answer_from_parts(Question, AllAnswers, AllFacts).

synthesize_comprehensive_answer_from_parts(Question, Answers, Facts) ->
    % Simple synthesis (can be enhanced with NLP)
    ValidAnswers = [A || A <- Answers, byte_size(A) > 0],
    ValidFacts = [F || F <- Facts, byte_size(F) > 5],
    
    case {ValidAnswers, ValidFacts} of
        {[], []} ->
            <<"No sufficient information found to answer the question.">>;
        {[SingleAnswer], []} ->
            SingleAnswer;
        {Answers, Facts} ->
            iolist_to_binary([
                <<"Based on comprehensive search: ">>,
                string:join([binary_to_list(A) || A <- Answers], " Furthermore, "),
                case Facts of
                    [] -> <<>>;
                    _ -> iolist_to_binary([<<" Key facts: ">>, 
                                         string:join([binary_to_list(F) || F <- lists:sublist(Facts, 3)], "; ")])
                end
            ])
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

perform_search(Query, SearchEngine) ->
    % Simulate web search (integrate with actual search engines)
    colored_logger:info("Searching: ~s", [Query]),
    
    % Mock search results for demonstration
    MockResults = [
        #{content => <<"Relevant information about the query">>, source => <<"example.com">>, relevance => 0.8},
        #{content => <<"Additional context for the search">>, source => <<"source.org">>, relevance => 0.6},
        #{content => <<"Supporting evidence and details">>, source => <<"reference.net">>, relevance => 0.7}
    ],
    
    % Add timestamp and query info
    [Result#{query => Query, timestamp => erlang:system_time(millisecond)} || Result <- MockResults].

score_results_by_relevance(Results, Question) ->
    QuestionTerms = extract_key_terms(Question),
    
    lists:map(fun(Result) ->
        Content = maps:get(content, Result, <<>>),
        Score = calculate_relevance_score(Content, QuestionTerms),
        Result#{relevance_score => Score}
    end, Results).

calculate_relevance_score(Content, KeyTerms) ->
    ContentLower = string:lowercase(binary_to_list(Content)),
    MatchCount = lists:sum([
        case string:find(ContentLower, string:lowercase(binary_to_list(Term))) of
            nomatch -> 0;
            _ -> 1
        end || Term <- KeyTerms
    ]),
    min(1.0, MatchCount / max(1, length(KeyTerms))).

filter_quality_results(Results) ->
    lists:filter(fun(Result) ->
        maps:get(relevance_score, Result, 0.0) >= 0.3
    end, Results).

extract_structured_info(Results) ->
    lists:map(fun(Result) ->
        Result#{
            content_type => detect_content_type(Result),
            key_phrases => extract_key_phrases(Result)
        }
    end, Results).

detect_content_type(Result) ->
    Content = maps:get(content, Result, <<>>),
    case binary:match(Content, [<<"http">>, <<"www">>, <<".com">>]) of
        nomatch -> text;
        _ -> web_content
    end.

extract_key_phrases(Result) ->
    Content = maps:get(content, Result, <<>>),
    Words = binary:split(Content, [<<" ">>, <<",">>, <<".">>], [global]),
    ImportantWords = [W || W <- Words, byte_size(W) > 3],
    lists:sublist(ImportantWords, 5).

extract_key_terms(Question) ->
    QuestionBinary = ensure_binary(Question),
    Words = binary:split(QuestionBinary, [<<" ">>, <<"?">>, <<"!">>, <<",">>], [global]),
    
    % Filter out common words
    StopWords = [<<"the">>, <<"is">>, <<"are">>, <<"and">>, <<"or">>, <<"but">>, <<"in">>, <<"on">>, <<"at">>, <<"to">>, <<"for">>, <<"of">>, <<"with">>, <<"by">>],
    ImportantWords = [W || W <- Words, byte_size(W) > 2, not lists:member(string:lowercase(W), StopWords)],
    
    lists:sublist(ImportantWords, 5).

classify_question_type(Question) ->
    QuestionLower = string:lowercase(binary_to_list(Question)),
    
    case {string:find(QuestionLower, "compare"), 
          string:find(QuestionLower, "why"),
          string:find(QuestionLower, "when"),
          string:find(QuestionLower, "what")} of
        {Match, _, _, _} when Match =/= nomatch -> comparative;
        {_, Match, _, _} when Match =/= nomatch -> causal;
        {_, _, Match, _} when Match =/= nomatch -> temporal;
        {_, _, _, Match} when Match =/= nomatch -> factual;
        _ -> general
    end.

build_factual_query(KeyTerms, _RecentQueries) ->
    string:join([binary_to_list(T) || T <- KeyTerms], " ").

build_comparative_query(KeyTerms, _RecentQueries) ->
    string:join([binary_to_list(T) || T <- KeyTerms] ++ ["comparison", "difference"], " ").

build_temporal_query(KeyTerms, _RecentQueries) ->
    string:join([binary_to_list(T) || T <- KeyTerms] ++ ["timeline", "when", "date"], " ").

build_causal_query(KeyTerms, _RecentQueries) ->
    string:join([binary_to_list(T) || T <- KeyTerms] ++ ["why", "cause", "reason"], " ").

build_general_query(KeyTerms, _RecentQueries) ->
    string:join([binary_to_list(T) || T <- KeyTerms], " ").

combine_knowledge_pieces(KnowledgePieces) ->
    Contents = [K#knowledge_piece.content || K <- KnowledgePieces],
    iolist_to_binary(string:join([binary_to_list(C) || C <- Contents], " ")).

calculate_overall_confidence(KnowledgePieces) ->
    case KnowledgePieces of
        [] -> 0.0;
        Pieces ->
            Confidences = [K#knowledge_piece.confidence || K <- Pieces],
            lists:sum(Confidences) / length(Confidences)
    end.

has_sufficient_information(CombinedKnowledge, _Question) ->
    % Simple heuristic - can be enhanced
    byte_size(CombinedKnowledge) > 50.

identify_missing_information(CombinedKnowledge, Question) ->
    % Identify what's missing based on question type and current knowledge
    QuestionType = classify_question_type(Question),
    
    MissingPieces = case QuestionType of
        temporal -> check_temporal_info(CombinedKnowledge);
        causal -> check_causal_info(CombinedKnowledge);
        comparative -> check_comparative_info(CombinedKnowledge);
        _ -> check_general_info(CombinedKnowledge)
    end,
    
    [M || M <- MissingPieces, M =/= undefined].

check_temporal_info(Knowledge) ->
    HasDates = binary:match(Knowledge, [<<"20">>, <<"19">>, <<"date">>, <<"year">>]) =/= nomatch,
    case HasDates of
        true -> [];
        false -> [<<"timeline">>]
    end.

check_causal_info(Knowledge) ->
    HasCausal = binary:match(Knowledge, [<<"because">>, <<"due to">>, <<"caused by">>, <<"reason">>]) =/= nomatch,
    case HasCausal of
        true -> [];
        false -> [<<"cause">>]
    end.

check_comparative_info(Knowledge) ->
    HasComparison = binary:match(Knowledge, [<<"than">>, <<"versus">>, <<"compared">>, <<"difference">>]) =/= nomatch,
    case HasComparison of
        true -> [];
        false -> [<<"comparison">>]
    end.

check_general_info(Knowledge) ->
    case byte_size(Knowledge) < 100 of
        true -> [<<"more_details">>];
        false -> []
    end.

synthesize_answer_from_knowledge(Knowledge, _Question) ->
    % Simple synthesis - can be enhanced with NLP
    case byte_size(Knowledge) of
        0 -> <<"No information found">>;
        _ -> iolist_to_binary([<<"Based on available information: ">>, Knowledge])
    end.

generate_next_query(Context, SearchHistory) ->
    Question = maps:get(question, Context),
    MissingInfo = maps:get(missing_info, Context, []),
    
    case MissingInfo of
        [] -> Question;
        [FirstMissing | _] ->
            generate_specific_query(FirstMissing, Question)
    end.

update_knowledge_graph(ProcessedResults, CurrentGraph) ->
    lists:foldl(fun(Result, Graph) ->
        Key = maps:get(source, Result, <<"unknown">>),
        Content = maps:get(content, Result, <<>>),
        Graph#{Key => Content}
    end, CurrentGraph, ProcessedResults).

evaluate_knowledge_completeness(KnowledgeGraph, Question) ->
    % Convert knowledge graph to knowledge pieces
    KnowledgePieces = [
        #knowledge_piece{content = Content, confidence = 0.7, source = Source, context = Question}
        || {Source, Content} <- maps:to_list(KnowledgeGraph)
    ],
    
    evaluate_answer_completeness(KnowledgePieces, Question).

finalize_sequential_search(Context, SearchHistory) ->
    KnowledgeGraph = maps:get(knowledge_graph, Context),
    Question = maps:get(question, Context),
    
    case maps:size(KnowledgeGraph) of
        0 ->
            #{
                status => failed,
                answer => <<"No relevant information found after exhaustive search">>,
                iterations => length(SearchHistory),
                search_history => SearchHistory
            };
        _ ->
            % Synthesize best possible answer from available knowledge
            AllContent = [Content || {_Source, Content} <- maps:to_list(KnowledgeGraph)],
            CombinedContent = iolist_to_binary(string:join([binary_to_list(C) || C <- AllContent], " ")),
            
            #{
                status => partial,
                answer => synthesize_answer_from_knowledge(CombinedContent, Question),
                confidence => 0.6,  % Lower confidence for incomplete search
                iterations => length(SearchHistory),
                search_history => SearchHistory,
                note => <<"Answer synthesized from partial information">>
            }
    end.

update_search_history(Result, State) ->
    NewHistory = [{maps:get(question, Result, <<"unknown">>), Result} | 
                  lists:sublist(State#state.search_history, 9)],
    State#state{search_history = NewHistory}.

update_context_window(Question, Context, State) ->
    NewWindow = [Question | lists:sublist(State#state.context_window, 4)],
    State#state{context_window = NewWindow}.

update_building_context(TurnInfo, BuildingContext) ->
    Question = maps:get(question, TurnInfo),
    Answer = maps:get(answer, TurnInfo),
    BuildingContext#{Question => Answer}.

synthesize_comprehensive_answer(Question, SearchHistory) ->
    % Extract all answers from search history
    Answers = [maps:get(answer, Result, <<>>) || {_Q, Result} <- SearchHistory],
    ValidAnswers = [A || A <- Answers, byte_size(A) > 0],
    
    case ValidAnswers of
        [] -> <<"No comprehensive answer could be synthesized">>;
        [SingleAnswer] -> SingleAnswer;
        MultipleAnswers ->
            iolist_to_binary([
                <<"Comprehensive answer: ">>,
                string:join([binary_to_list(A) || A <- lists:sublist(MultipleAnswers, 3)], " Additionally, ")
            ])
    end.

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) -> list_to_binary(io_lib:format("~p", [Value])).