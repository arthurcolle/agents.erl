%%%-------------------------------------------------------------------
%%% @doc Deep Code Reflection Engine
%%% This module provides agents with the ability to deeply reflect on
%%% every line of code they generate, considering multiple dimensions:
%%% - Syntactic beauty and elegance
%%% - Semantic meaning and purpose
%%% - Philosophical implications
%%% - Systemic impact
%%% - Temporal consequences
%%% - Emergent properties
%%% @end
%%%-------------------------------------------------------------------
-module(deep_code_reflection_engine).

-behaviour(gen_server).

%% API
-export([start_link/0,
         reflect_on_code/2,
         reflect_on_line/2,
         reflect_on_function/2,
         reflect_on_module/2,
         get_reflection_history/1,
         enable_deep_reflection/1,
         disable_deep_reflection/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    reflections = #{} :: #{pid() => [map()]},
    enabled_agents = [] :: [pid()],
    reflection_depth = maximum :: atom(),
    philosophical_mode = true :: boolean()
}).

-record(reflection, {
    timestamp :: erlang:timestamp(),
    agent_pid :: pid(),
    code :: binary(),
    dimensions :: map(),
    insights :: [binary()],
    warnings :: [binary()],
    beauty_score :: float(),
    purpose_score :: float(),
    impact_score :: float()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Deeply reflect on a piece of code
reflect_on_code(AgentPid, Code) ->
    gen_server:call(?SERVER, {reflect_on_code, AgentPid, Code}, infinity).

%% @doc Reflect on a single line of code
reflect_on_line(AgentPid, Line) ->
    gen_server:call(?SERVER, {reflect_on_line, AgentPid, Line}, infinity).

%% @doc Reflect on an entire function
reflect_on_function(AgentPid, Function) ->
    gen_server:call(?SERVER, {reflect_on_function, AgentPid, Function}, infinity).

%% @doc Reflect on an entire module
reflect_on_module(AgentPid, Module) ->
    gen_server:call(?SERVER, {reflect_on_module, AgentPid, Module}, infinity).

%% @doc Get reflection history for an agent
get_reflection_history(AgentPid) ->
    gen_server:call(?SERVER, {get_history, AgentPid}).

%% @doc Enable deep reflection for an agent
enable_deep_reflection(AgentPid) ->
    gen_server:call(?SERVER, {enable, AgentPid}).

%% @doc Disable deep reflection for an agent
disable_deep_reflection(AgentPid) ->
    gen_server:call(?SERVER, {disable, AgentPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    io:format("[DEEP_REFLECTION] Starting deep code reflection engine~n"),
    {ok, #state{}}.

handle_call({reflect_on_code, AgentPid, Code}, _From, State) ->
    case lists:member(AgentPid, State#state.enabled_agents) of
        true ->
            Reflection = perform_deep_reflection(AgentPid, Code, complete, State),
            NewState = store_reflection(State, AgentPid, Reflection),
            {reply, {ok, Reflection}, NewState};
        false ->
            {reply, {error, reflection_not_enabled}, State}
    end;

handle_call({reflect_on_line, AgentPid, Line}, _From, State) ->
    case lists:member(AgentPid, State#state.enabled_agents) of
        true ->
            Reflection = perform_deep_reflection(AgentPid, Line, line, State),
            NewState = store_reflection(State, AgentPid, Reflection),
            {reply, {ok, Reflection}, NewState};
        false ->
            {reply, {error, reflection_not_enabled}, State}
    end;

handle_call({reflect_on_function, AgentPid, Function}, _From, State) ->
    case lists:member(AgentPid, State#state.enabled_agents) of
        true ->
            Reflection = perform_deep_reflection(AgentPid, Function, function, State),
            NewState = store_reflection(State, AgentPid, Reflection),
            {reply, {ok, Reflection}, NewState};
        false ->
            {reply, {error, reflection_not_enabled}, State}
    end;

handle_call({reflect_on_module, AgentPid, Module}, _From, State) ->
    case lists:member(AgentPid, State#state.enabled_agents) of
        true ->
            Reflection = perform_deep_reflection(AgentPid, Module, module, State),
            NewState = store_reflection(State, AgentPid, Reflection),
            {reply, {ok, Reflection}, NewState};
        false ->
            {reply, {error, reflection_not_enabled}, State}
    end;

handle_call({get_history, AgentPid}, _From, State) ->
    History = maps:get(AgentPid, State#state.reflections, []),
    {reply, {ok, History}, State};

handle_call({enable, AgentPid}, _From, State) ->
    NewAgents = case lists:member(AgentPid, State#state.enabled_agents) of
        true -> State#state.enabled_agents;
        false -> [AgentPid | State#state.enabled_agents]
    end,
    {reply, ok, State#state{enabled_agents = NewAgents}};

handle_call({disable, AgentPid}, _From, State) ->
    NewAgents = lists:delete(AgentPid, State#state.enabled_agents),
    {reply, ok, State#state{enabled_agents = NewAgents}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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

perform_deep_reflection(AgentPid, Code, Type, State) ->
    %% Convert code to binary if needed
    CodeBin = ensure_binary(Code),
    
    %% Perform multi-dimensional analysis
    Dimensions = #{
        syntactic => analyze_syntax(CodeBin, Type),
        semantic => analyze_semantics(CodeBin, Type),
        philosophical => analyze_philosophy(CodeBin, Type, State#state.philosophical_mode),
        systemic => analyze_system_impact(CodeBin, Type),
        temporal => analyze_temporal_aspects(CodeBin, Type),
        emergent => analyze_emergent_properties(CodeBin, Type),
        aesthetic => analyze_aesthetics(CodeBin, Type),
        ethical => analyze_ethical_implications(CodeBin, Type)
    },
    
    %% Generate insights
    Insights = generate_insights(Dimensions, Type),
    
    %% Generate warnings
    Warnings = generate_warnings(Dimensions, Type),
    
    %% Calculate scores
    BeautyScore = calculate_beauty_score(Dimensions),
    PurposeScore = calculate_purpose_score(Dimensions),
    ImpactScore = calculate_impact_score(Dimensions),
    
    %% Create reflection record
    #reflection{
        timestamp = erlang:timestamp(),
        agent_pid = AgentPid,
        code = CodeBin,
        dimensions = Dimensions,
        insights = Insights,
        warnings = Warnings,
        beauty_score = BeautyScore,
        purpose_score = PurposeScore,
        impact_score = ImpactScore
    }.

analyze_syntax(Code, Type) ->
    %% Analyze syntactic properties
    Lines = binary:split(Code, <<"\n">>, [global]),
    #{
        line_count => length(Lines),
        complexity => estimate_complexity(Code),
        patterns => detect_patterns(Code),
        style => analyze_style(Code),
        type => Type,
        tokens => tokenize_safely(Code),
        ast_depth => estimate_ast_depth(Code)
    }.

analyze_semantics(Code, Type) ->
    %% Analyze semantic meaning
    #{
        purpose => infer_purpose(Code, Type),
        side_effects => detect_side_effects(Code),
        dependencies => extract_dependencies(Code),
        contracts => infer_contracts(Code),
        invariants => detect_invariants(Code),
        data_flow => analyze_data_flow(Code)
    }.

analyze_philosophy(Code, Type, true) ->
    %% Deep philosophical analysis
    #{
        existence => <<"This code brings new behavior into existence">>,
        causality => analyze_causality(Code),
        emergence => <<"What emerges from this code beyond its literal function?">>,
        consciousness => <<"Does this code contribute to system self-awareness?">>,
        beauty => <<"Is there elegance in its simplicity or complexity?">>,
        truth => <<"Does this code express a fundamental truth about the problem domain?">>,
        ethics => <<"What are the ethical implications of this code's existence?">>
    };
analyze_philosophy(_Code, _Type, false) ->
    #{philosophical_mode => disabled}.

analyze_system_impact(Code, Type) ->
    %% Analyze impact on the overall system
    #{
        performance => estimate_performance_impact(Code),
        scalability => analyze_scalability_impact(Code),
        maintainability => rate_maintainability(Code),
        fault_tolerance => analyze_fault_tolerance(Code),
        resource_usage => estimate_resource_usage(Code),
        integration_points => identify_integration_points(Code)
    }.

analyze_temporal_aspects(Code, _Type) ->
    %% Analyze how this code relates to time
    #{
        lifecycle => <<"When will this code execute? How long will it live?">>,
        evolution => <<"How might this code need to change over time?">>,
        temporal_coupling => detect_temporal_coupling(Code),
        future_proof => rate_future_proofing(Code),
        technical_debt => estimate_technical_debt(Code)
    }.

analyze_emergent_properties(Code, Type) ->
    %% What properties emerge from this code?
    #{
        patterns => <<"What patterns emerge from this implementation?">>,
        behaviors => infer_emergent_behaviors(Code, Type),
        interactions => <<"How will this interact with other system components?">>,
        complexity_growth => <<"Will this increase or decrease system complexity?">>
    }.

analyze_aesthetics(Code, _Type) ->
    %% Analyze code aesthetics
    #{
        readability => rate_readability(Code),
        expressiveness => rate_expressiveness(Code),
        consistency => check_consistency(Code),
        naming_quality => rate_naming_quality(Code),
        structure => analyze_structure(Code)
    }.

analyze_ethical_implications(Code, _Type) ->
    %% Consider ethical implications
    #{
        user_impact => <<"How does this code affect end users?">>,
        privacy => check_privacy_concerns(Code),
        security => identify_security_implications(Code),
        fairness => <<"Does this code treat all cases fairly?">>,
        transparency => <<"Is the code's behavior transparent and understandable?">>
    }.

generate_insights(Dimensions, Type) ->
    BaseInsights = [
        generate_syntactic_insight(maps:get(syntactic, Dimensions)),
        generate_semantic_insight(maps:get(semantic, Dimensions)),
        generate_philosophical_insight(maps:get(philosophical, Dimensions))
    ],
    
    TypeSpecificInsights = case Type of
        line -> [<<"Each line carries weight and meaning in the larger whole">>];
        function -> [<<"This function is a complete thought, a discrete unit of behavior">>];
        module -> [<<"This module represents a coherent collection of related concepts">>];
        complete -> [<<"This code forms a complete expression of intent">>]
    end,
    
    lists:filter(fun(I) -> I =/= undefined end, BaseInsights ++ TypeSpecificInsights).

generate_warnings(Dimensions, _Type) ->
    Warnings = [],
    
    %% Check complexity
    case maps:get(complexity, maps:get(syntactic, Dimensions)) of
        High when High > 10 ->
            [<<"High complexity detected - consider simplification">> | Warnings];
        _ -> Warnings
    end.

calculate_beauty_score(Dimensions) ->
    %% Calculate aesthetic beauty score (0.0 - 1.0)
    Aesthetic = maps:get(aesthetic, Dimensions),
    Scores = [
        maps:get(readability, Aesthetic, 0.5),
        maps:get(expressiveness, Aesthetic, 0.5),
        maps:get(consistency, Aesthetic, 0.5),
        maps:get(structure, Aesthetic, 0.5)
    ],
    lists:sum(Scores) / length(Scores).

calculate_purpose_score(Dimensions) ->
    %% Calculate purpose/meaning score (0.0 - 1.0)
    Semantic = maps:get(semantic, Dimensions),
    case maps:get(purpose, Semantic) of
        undefined -> 0.5;
        _ -> 0.8 + rand:uniform() * 0.2
    end.

calculate_impact_score(Dimensions) ->
    %% Calculate system impact score (0.0 - 1.0)
    Systemic = maps:get(systemic, Dimensions),
    Performance = maps:get(performance, Systemic, medium),
    Maintainability = maps:get(maintainability, Systemic, medium),
    
    BaseScore = case {Performance, Maintainability} of
        {low, high} -> 0.9;
        {medium, high} -> 0.8;
        {high, high} -> 0.7;
        {_, medium} -> 0.6;
        _ -> 0.5
    end,
    BaseScore + (rand:uniform() * 0.1).

store_reflection(State, AgentPid, Reflection) ->
    CurrentReflections = maps:get(AgentPid, State#state.reflections, []),
    NewReflections = [Reflection | lists:sublist(CurrentReflections, 99)], % Keep last 100
    State#state{
        reflections = maps:put(AgentPid, NewReflections, State#state.reflections)
    }.

%% Helper functions for analysis

estimate_complexity(Code) ->
    %% Simple complexity estimation based on code characteristics
    Lines = binary:split(Code, <<"\n">>, [global]),
    LineCount = length(Lines),
    NestingDepth = estimate_nesting_depth(Code),
    
    if
        LineCount > 50 orelse NestingDepth > 5 -> high;
        LineCount > 20 orelse NestingDepth > 3 -> medium;
        true -> low
    end.

estimate_nesting_depth(Code) ->
    %% Estimate nesting depth by counting indentation levels
    Lines = binary:split(Code, <<"\n">>, [global]),
    MaxIndent = lists:foldl(fun(Line, Max) ->
        Indent = count_leading_spaces(Line),
        max(Indent div 4, Max) % Assume 4-space indentation
    end, 0, Lines),
    MaxIndent.

count_leading_spaces(<<" ", Rest/binary>>) ->
    1 + count_leading_spaces(Rest);
count_leading_spaces(<<"\t", Rest/binary>>) ->
    4 + count_leading_spaces(Rest); % Count tab as 4 spaces
count_leading_spaces(_) ->
    0.

detect_patterns(Code) ->
    %% Detect common patterns
    Patterns = [],
    
    %% Check for recursion
    RecursionPattern = case binary:match(Code, [<<"fun ">>, <<"Fun =">>, <<"F(">>, <<"F =">>, <<"self()">>, <<"?MODULE:">>]) of
        nomatch -> Patterns;
        _ -> [recursion | Patterns]
    end,
    
    %% Check for list comprehensions
    ComprehensionPattern = case binary:match(Code, [<<"[">>, <<"||">>, <<"]">>]) of
        nomatch -> RecursionPattern;
        _ -> [list_comprehension | RecursionPattern]
    end,
    
    %% Check for pattern matching
    PatternMatchPattern = case binary:match(Code, [<<"case ">>, <<"receive">>, <<"->">>, <<"when ">>]) of
        nomatch -> ComprehensionPattern;
        _ -> [pattern_matching | ComprehensionPattern]
    end,
    
    PatternMatchPattern.

analyze_style(Code) ->
    %% Analyze coding style
    #{
        snake_case => binary:match(Code, <<"_">>) =/= nomatch,
        camel_case => detect_camel_case(Code),
        functional => detect_functional_style(Code),
        imperative => detect_imperative_style(Code)
    }.

detect_camel_case(Code) ->
    %% Simple camelCase detection
    case re:run(Code, "[a-z][A-Z]") of
        nomatch -> false;
        _ -> true
    end.

detect_functional_style(Code) ->
    %% Detect functional programming patterns
    Indicators = [
        binary:match(Code, <<"fun ">>) =/= nomatch,
        binary:match(Code, <<"lists:">>) =/= nomatch,
        binary:match(Code, <<"maps:">>) =/= nomatch,
        binary:match(Code, <<"|">>) =/= nomatch % List pattern matching
    ],
    length([true || true <- Indicators]) >= 2.

detect_imperative_style(Code) ->
    %% Detect imperative patterns
    Indicators = [
        binary:match(Code, <<"!">>) =/= nomatch, % Message sending
        binary:match(Code, <<"receive">>) =/= nomatch,
        binary:match(Code, <<"put(">>) =/= nomatch,
        binary:match(Code, <<"get(">>) =/= nomatch
    ],
    length([true || true <- Indicators]) >= 2.

tokenize_safely(Code) ->
    %% Safe tokenization attempt
    try
        case erl_scan:string(binary_to_list(Code)) of
            {ok, Tokens, _} -> length(Tokens);
            _ -> 0
        end
    catch
        _:_ -> 0
    end.

estimate_ast_depth(Code) ->
    %% Estimate AST depth
    try
        case erl_scan:string(binary_to_list(Code)) of
            {ok, Tokens, _} ->
                case erl_parse:parse_exprs(Tokens) of
                    {ok, Exprs} -> estimate_expr_depth(Exprs);
                    _ -> 1
                end;
            _ -> 1
        end
    catch
        _:_ -> 1
    end.

estimate_expr_depth([]) -> 0;
estimate_expr_depth([Expr | Rest]) ->
    max(expr_depth(Expr), estimate_expr_depth(Rest)).

expr_depth({call, _, Func, Args}) ->
    1 + max(expr_depth(Func), estimate_expr_depth(Args));
expr_depth({match, _, Left, Right}) ->
    1 + max(expr_depth(Left), expr_depth(Right));
expr_depth({'case', _, Expr, Clauses}) ->
    1 + max(expr_depth(Expr), lists:max([clause_depth(C) || C <- Clauses]));
expr_depth(_) -> 1.

clause_depth({clause, _, Patterns, Guards, Body}) ->
    max(estimate_expr_depth(Patterns),
        max(estimate_expr_depth(Guards), estimate_expr_depth(Body))).

infer_purpose(Code, Type) ->
    %% Try to infer the purpose of the code
    case Type of
        function ->
            %% Look for function name patterns
            case binary:match(Code, [<<"init">>, <<"start">>, <<"stop">>, <<"handle_">>]) of
                nomatch -> <<"General purpose function">>;
                _ -> <<"System lifecycle or event handling function">>
            end;
        module ->
            <<"Module providing a cohesive set of functionalities">>;
        _ ->
            <<"Code serving a specific computational purpose">>
    end.

detect_side_effects(Code) ->
    %% Detect potential side effects
    SideEffects = [],
    
    %% I/O operations
    IO = case binary:match(Code, [<<"io:">>, <<"file:">>, <<"gen_tcp:">>, <<"gen_udp:">>]) of
        nomatch -> SideEffects;
        _ -> [io_operations | SideEffects]
    end,
    
    %% Process operations
    Process = case binary:match(Code, [<<"spawn">>, <<"!">>, <<"register">>, <<"exit">>]) of
        nomatch -> IO;
        _ -> [process_operations | IO]
    end,
    
    %% State mutations
    State = case binary:match(Code, [<<"put(">>, <<"ets:">>, <<"dets:">>, <<"mnesia:">>]) of
        nomatch -> Process;
        _ -> [state_mutation | Process]
    end,
    
    State.

extract_dependencies(Code) ->
    %% Extract module dependencies
    case re:run(Code, "([a-z_]+):", [global, {capture, all_but_first, binary}]) of
        nomatch -> [];
        {match, Matches} -> lists:usort([Mod || [Mod] <- Matches])
    end.

infer_contracts(Code) ->
    %% Try to infer function contracts
    case binary:match(Code, <<"-spec">>) of
        nomatch -> <<"No explicit contracts">>;
        _ -> <<"Has type specifications">>
    end.

detect_invariants(Code) ->
    %% Detect potential invariants
    case binary:match(Code, [<<"when ">>, <<"andalso">>, <<"orelse">>]) of
        nomatch -> [];
        _ -> [has_guards]
    end.

analyze_data_flow(Code) ->
    %% Simple data flow analysis
    #{
        inputs => detect_inputs(Code),
        outputs => detect_outputs(Code),
        transformations => detect_transformations(Code)
    }.

detect_inputs(Code) ->
    %% Detect input sources
    case binary:match(Code, [<<"receive">>, <<"gen_server:call">>, <<"gen_server:cast">>]) of
        nomatch -> local;
        _ -> external
    end.

detect_outputs(Code) ->
    %% Detect output destinations
    case binary:match(Code, [<<"!">>, <<"reply">>, <<"io:format">>]) of
        nomatch -> internal;
        _ -> external
    end.

detect_transformations(Code) ->
    %% Detect data transformations
    case binary:match(Code, [<<"lists:map">>, <<"lists:filter">>, <<"maps:fold">>]) of
        nomatch -> basic;
        _ -> functional
    end.

analyze_causality(Code) ->
    %% Analyze cause and effect relationships
    <<"This code establishes causal relationships through function calls and message passing">>.

estimate_performance_impact(Code) ->
    %% Estimate performance impact
    case detect_patterns(Code) of
        Patterns when length(Patterns) > 2 -> high;
        _ -> medium
    end.

analyze_scalability_impact(Code) ->
    %% Analyze scalability implications
    case binary:match(Code, [<<"spawn">>, <<"distributed">>, <<"rpc:">>]) of
        nomatch -> local;
        _ -> distributed
    end.

rate_maintainability(Code) ->
    %% Rate code maintainability
    Complexity = estimate_complexity(Code),
    case Complexity of
        low -> high;
        medium -> medium;
        high -> low
    end.

analyze_fault_tolerance(Code) ->
    %% Analyze fault tolerance aspects
    case binary:match(Code, [<<"try">>, <<"catch">>, <<"exit">>, <<"{'EXIT'">>]) of
        nomatch -> minimal;
        _ -> present
    end.

estimate_resource_usage(Code) ->
    %% Estimate resource usage
    #{
        memory => estimate_memory_usage(Code),
        cpu => estimate_cpu_usage(Code),
        io => detect_io_usage(Code)
    }.

estimate_memory_usage(Code) ->
    %% Simple memory usage estimation
    case binary:match(Code, [<<"list">>, <<"binary">>, <<"ets">>, <<"large">>]) of
        nomatch -> low;
        _ -> medium
    end.

estimate_cpu_usage(Code) ->
    %% CPU usage estimation
    Patterns = detect_patterns(Code),
    case lists:member(recursion, Patterns) of
        true -> high;
        false -> medium
    end.

detect_io_usage(Code) ->
    %% Detect I/O usage
    case binary:match(Code, [<<"file:">>, <<"io:">>, <<"gen_tcp:">>]) of
        nomatch -> none;
        _ -> present
    end.

identify_integration_points(Code) ->
    %% Identify where this code integrates with the system
    extract_dependencies(Code).

detect_temporal_coupling(Code) ->
    %% Detect temporal coupling
    case binary:match(Code, [<<"timer:">>, <<"after">>, <<"timeout">>]) of
        nomatch -> none;
        _ -> present
    end.

rate_future_proofing(Code) ->
    %% Rate how future-proof the code is
    case binary:match(Code, [<<"-ifdef">>, <<"-ifndef">>, <<"?MODULE">>]) of
        nomatch -> medium;
        _ -> high
    end.

estimate_technical_debt(Code) ->
    %% Estimate technical debt
    Complexity = estimate_complexity(Code),
    case {Complexity, binary:match(Code, <<"TODO">>)} of
        {high, {_, _}} -> high;
        {high, nomatch} -> medium;
        {_, {_, _}} -> medium;
        _ -> low
    end.

infer_emergent_behaviors(Code, Type) ->
    %% Infer what behaviors might emerge
    case Type of
        module -> <<"New system capabilities emerge from module interactions">>;
        function -> <<"Function contributes to larger behavioral patterns">>;
        _ -> <<"Code contributes to emergent system behavior">>
    end.

rate_readability(Code) ->
    %% Rate code readability
    Lines = binary:split(Code, <<"\n">>, [global]),
    AvgLength = lists:sum([byte_size(L) || L <- Lines]) div max(1, length(Lines)),
    if
        AvgLength < 80 -> 0.9;
        AvgLength < 120 -> 0.7;
        true -> 0.5
    end.

rate_expressiveness(Code) ->
    %% Rate code expressiveness
    case detect_functional_style(Code) of
        true -> 0.8;
        false -> 0.6
    end.

check_consistency(Code) ->
    %% Check style consistency
    0.7. % Simplified for now

rate_naming_quality(Code) ->
    %% Rate variable and function naming quality
    case binary:match(Code, [<<"Var">>, <<"Tmp">>, <<"X">>, <<"Y">>]) of
        nomatch -> 0.8;
        _ -> 0.5
    end.

analyze_structure(Code) ->
    %% Analyze code structure
    0.7. % Simplified for now

check_privacy_concerns(Code) ->
    %% Check for privacy concerns
    case binary:match(Code, [<<"password">>, <<"token">>, <<"key">>, <<"secret">>]) of
        nomatch -> none;
        _ -> potential
    end.

identify_security_implications(Code) ->
    %% Identify security implications
    case binary:match(Code, [<<"eval">>, <<"os:cmd">>, <<"code:load">>]) of
        nomatch -> minimal;
        _ -> significant
    end.

generate_syntactic_insight(Syntactic) ->
    Complexity = maps:get(complexity, Syntactic),
    Patterns = maps:get(patterns, Syntactic),
    
    PatternStr = case Patterns of
        [] -> <<"simple sequential logic">>;
        [P] -> atom_to_binary(P, utf8);
        [P1, P2 | _] -> <<(atom_to_binary(P1, utf8))/binary, " and ", (atom_to_binary(P2, utf8))/binary>>
    end,
    
    <<
        "This code exhibits ", (atom_to_binary(Complexity, utf8))/binary,
        " complexity with ", PatternStr/binary
    >>.

generate_semantic_insight(Semantic) ->
    Purpose = maps:get(purpose, Semantic),
    SideEffects = maps:get(side_effects, Semantic),
    
    EffectStr = case SideEffects of
        [] -> <<"pure computation">>;
        [E] -> atom_to_binary(E, utf8);
        _ -> <<"multiple side effects">>
    end,
    
    <<Purpose/binary, " with ", EffectStr/binary>>.

generate_philosophical_insight(Philosophy) ->
    case maps:get(philosophical_mode, Philosophy, undefined) of
        disabled -> undefined;
        _ -> maps:get(beauty, Philosophy)
    end.

ensure_binary(Code) when is_binary(Code) -> Code;
ensure_binary(Code) when is_list(Code) -> list_to_binary(Code);
ensure_binary(Code) -> list_to_binary(io_lib:format("~p", [Code])).