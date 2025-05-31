-module(backward_chaining_engine).
-behaviour(gen_statem).

-export([start_link/0, start_link/1]).
-export([prove_goal/2, add_rule/2, add_fact/2, remove_rule/2, remove_fact/2, 
         reset_knowledge_base/1, get_proof_tree/1, query/2]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([idle/3, goal_decomposition/3, rule_matching/3, fact_checking/3, 
         subgoal_generation/3, recursive_proving/3, unification/3, 
         proof_construction/3, optimization/3, completed/3]).

-record(proof_node, {
    goal :: term(),
    rule = undefined :: undefined | term(),
    subgoals = [] :: [term()],
    bindings = #{} :: #{atom() => term()},
    proof_status = unknown :: unknown | proven | failed,
    depth = 0 :: non_neg_integer(),
    parent = undefined :: undefined | pid(),
    children = [] :: [pid()]
}).

-record(chaining_data, {
    query_id :: term(),
    main_goal :: term(),
    knowledge_base = #{
        facts => [],
        rules => []
    } :: #{atom() => [term()]},
    proof_tree = #{} :: #{term() => #proof_node{}},
    goal_stack = [] :: [term()],
    current_goal = undefined :: undefined | term(),
    variable_bindings = #{} :: #{atom() => term()},
    unification_stack = [] :: [#{atom() => term()}],
    resolution_strategy = depth_first :: depth_first | breadth_first | best_first,
    max_depth = 100 :: pos_integer(),
    current_depth = 0 :: non_neg_integer(),
    proof_found = false :: boolean(),
    proof_path = [] :: [term()],
    alternative_proofs = [] :: [[term()]],
    statistics = #{} :: #{atom() => term()},
    optimization_enabled = true :: boolean(),
    memoization_table = #{} :: #{term() => {proven | failed, [term()]}},
    timeout = infinity :: timeout(),
    start_time :: erlang:timestamp()
}).

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

start_link(Options) ->
    gen_statem:start_link(?MODULE, Options, []).

prove_goal(Pid, Goal) ->
    gen_statem:call(Pid, {prove_goal, Goal}).

add_rule(Pid, Rule) ->
    gen_statem:call(Pid, {add_rule, Rule}).

add_fact(Pid, Fact) ->
    gen_statem:call(Pid, {add_fact, Fact}).

remove_rule(Pid, Rule) ->
    gen_statem:call(Pid, {remove_rule, Rule}).

remove_fact(Pid, Fact) ->
    gen_statem:call(Pid, {remove_fact, Fact}).

reset_knowledge_base(Pid) ->
    gen_statem:call(Pid, reset_knowledge_base).

get_proof_tree(Pid) ->
    gen_statem:call(Pid, get_proof_tree).

query(Pid, Query) ->
    gen_statem:call(Pid, {query, Query}).

init(Options) ->
    Data = #chaining_data{
        resolution_strategy = proplists:get_value(strategy, Options, depth_first),
        max_depth = proplists:get_value(max_depth, Options, 100),
        optimization_enabled = proplists:get_value(optimization, Options, true),
        timeout = proplists:get_value(timeout, Options, infinity),
        start_time = erlang:timestamp(),
        statistics = #{
            goals_processed => 0,
            rules_applied => 0,
            unifications => 0,
            backtracks => 0,
            cache_hits => 0,
            cache_misses => 0
        }
    },
    {ok, idle, Data}.

callback_mode() -> [state_functions, state_enter].

idle(enter, _OldState, Data) ->
    {keep_state, Data};
idle({call, From}, {prove_goal, Goal}, Data) ->
    QueryId = make_ref(),
    NewData = initialize_proof_session(Goal, QueryId, Data),
    {next_state, goal_decomposition, NewData, [{reply, From, {ok, QueryId}}]};
idle({call, From}, {query, Query}, Data) ->
    QueryId = make_ref(),
    NewData = initialize_proof_session(Query, QueryId, Data),
    {next_state, goal_decomposition, NewData, [{reply, From, {ok, QueryId}}]};
idle({call, From}, {add_rule, Rule}, Data) ->
    KB = Data#chaining_data.knowledge_base,
    Rules = maps:get(rules, KB, []),
    NewKB = maps:put(rules, [Rule | Rules], KB),
    {keep_state, Data#chaining_data{knowledge_base = NewKB}, [{reply, From, ok}]};
idle({call, From}, {add_fact, Fact}, Data) ->
    KB = Data#chaining_data.knowledge_base,
    Facts = maps:get(facts, KB, []),
    NewKB = maps:put(facts, [Fact | Facts], KB),
    {keep_state, Data#chaining_data{knowledge_base = NewKB}, [{reply, From, ok}]};
idle({call, From}, reset_knowledge_base, Data) ->
    NewData = Data#chaining_data{
        knowledge_base = #{facts => [], rules => []},
        memoization_table = #{}
    },
    {keep_state, NewData, [{reply, From, ok}]};
idle({call, From}, get_proof_tree, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data#chaining_data.proof_tree}}]};
idle(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data).

goal_decomposition(enter, _OldState, Data) ->
    case Data#chaining_data.current_goal of
        undefined ->
            case Data#chaining_data.goal_stack of
                [] -> {next_state, completed, Data};
                [Goal | Rest] ->
                    UpdatedData = Data#chaining_data{
                        current_goal = Goal,
                        goal_stack = Rest,
                        current_depth = Data#chaining_data.current_depth + 1
                    },
                    {keep_state, UpdatedData, [{state_timeout, 0, decompose_goal}]}
            end;
        Goal ->
            {keep_state, Data, [{state_timeout, 0, decompose_goal}]}
    end;
goal_decomposition(state_timeout, decompose_goal, Data) ->
    Goal = Data#chaining_data.current_goal,
    
    case check_memoization(Goal, Data) of
        {hit, Result} ->
            handle_memoized_result(Result, Data);
        miss ->
            case check_depth_limit(Data) of
                within_limit ->
                    UpdatedStats = increment_stat(goals_processed, Data#chaining_data.statistics),
                    {next_state, fact_checking, Data#chaining_data{statistics = UpdatedStats}};
                exceeded ->
                    {next_state, completed, mark_goal_failed(Goal, depth_exceeded, Data)}
            end
    end;
goal_decomposition(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data).

fact_checking(enter, _OldState, Data) ->
    Goal = Data#chaining_data.current_goal,
    Facts = maps:get(facts, Data#chaining_data.knowledge_base, []),
    
    case check_goal_against_facts(Goal, Facts, Data#chaining_data.variable_bindings) of
        {proven, NewBindings} ->
            ProvenData = mark_goal_proven(Goal, NewBindings, Data),
            {next_state, proof_construction, ProvenData};
        not_proven ->
            {next_state, rule_matching, Data}
    end;
fact_checking(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data).

rule_matching(enter, _OldState, Data) ->
    Goal = Data#chaining_data.current_goal,
    Rules = maps:get(rules, Data#chaining_data.knowledge_base, []),
    
    case find_matching_rules(Goal, Rules, Data) of
        [] ->
            FailedData = mark_goal_failed(Goal, no_rules, Data),
            {next_state, goal_decomposition, FailedData};
        MatchingRules ->
            {next_state, subgoal_generation, 
             Data#chaining_data{proof_tree = store_matching_rules(Goal, MatchingRules, Data#chaining_data.proof_tree)},
             [{state_timeout, 0, {process_rules, MatchingRules}}]}
    end;
rule_matching(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data).

subgoal_generation(enter, _OldState, Data) ->
    {keep_state, Data};
subgoal_generation(state_timeout, {process_rules, []}, Data) ->
    Goal = Data#chaining_data.current_goal,
    FailedData = mark_goal_failed(Goal, all_rules_failed, Data),
    {next_state, goal_decomposition, FailedData};
subgoal_generation(state_timeout, {process_rules, [Rule | RestRules]}, Data) ->
    Goal = Data#chaining_data.current_goal,
    
    case generate_subgoals_from_rule(Goal, Rule, Data) of
        {ok, Subgoals, Bindings} ->
            NewGoalStack = Subgoals ++ Data#chaining_data.goal_stack,
            UpdatedData = Data#chaining_data{
                goal_stack = NewGoalStack,
                current_goal = undefined,
                variable_bindings = merge_bindings(Data#chaining_data.variable_bindings, Bindings),
                unification_stack = [Bindings | Data#chaining_data.unification_stack]
            },
            {next_state, recursive_proving, UpdatedData};
        {unification_failed, _Reason} ->
            {keep_state, Data, [{state_timeout, 0, {process_rules, RestRules}}]}
    end;
subgoal_generation(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data).

recursive_proving(enter, _OldState, Data) ->
    case Data#chaining_data.goal_stack of
        [] ->
            MainGoal = Data#chaining_data.main_goal,
            ProvenData = mark_goal_proven(MainGoal, Data#chaining_data.variable_bindings, Data),
            {next_state, proof_construction, ProvenData};
        _ ->
            {next_state, goal_decomposition, Data}
    end;
recursive_proving(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data).

unification(enter, _OldState, Data) ->
    Goal = Data#chaining_data.current_goal,
    case perform_unification_step(Goal, Data) of
        {success, NewBindings} ->
            UpdatedStats = increment_stat(unifications, Data#chaining_data.statistics),
            UpdatedData = Data#chaining_data{
                variable_bindings = merge_bindings(Data#chaining_data.variable_bindings, NewBindings),
                statistics = UpdatedStats
            },
            {next_state, goal_decomposition, UpdatedData};
        {failure, Reason} ->
            FailedData = mark_goal_failed(Goal, {unification_failed, Reason}, Data),
            {next_state, goal_decomposition, FailedData}
    end;
unification(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data).

proof_construction(enter, _OldState, Data) ->
    ProofPath = construct_proof_path(Data),
    FinalData = Data#chaining_data{
        proof_found = true,
        proof_path = ProofPath,
        alternative_proofs = [ProofPath | Data#chaining_data.alternative_proofs]
    },
    
    case Data#chaining_data.optimization_enabled of
        true ->
            {next_state, optimization, FinalData};
        false ->
            {next_state, completed, FinalData}
    end;
proof_construction(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data).

optimization(enter, _OldState, Data) ->
    OptimizedData = optimize_proof(Data),
    {next_state, completed, OptimizedData};
optimization(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data).

completed(enter, _OldState, Data) ->
    store_in_memoization_table(Data),
    {keep_state, Data};
completed({call, From}, get_proof_tree, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data#chaining_data.proof_tree}}]};
completed({call, From}, {prove_goal, Goal}, Data) ->
    case Data#chaining_data.proof_found of
        true ->
            {keep_state, Data, [{reply, From, {proven, Data#chaining_data.proof_path}}]};
        false ->
            {keep_state, Data, [{reply, From, {not_proven, no_proof_found}}]}
    end;
completed(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data).

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

initialize_proof_session(Goal, QueryId, Data) ->
    ProofNode = #proof_node{
        goal = Goal,
        depth = 0,
        proof_status = unknown
    },
    Data#chaining_data{
        query_id = QueryId,
        main_goal = Goal,
        current_goal = Goal,
        goal_stack = [],
        proof_tree = #{Goal => ProofNode},
        variable_bindings = #{},
        unification_stack = [],
        current_depth = 0,
        proof_found = false,
        proof_path = [],
        start_time = erlang:timestamp()
    }.

check_memoization(Goal, Data) ->
    case maps:get(Goal, Data#chaining_data.memoization_table, not_found) of
        not_found ->
            miss;
        {Status, ProofPath} ->
            {hit, {Status, ProofPath}}
    end.

handle_memoized_result({proven, ProofPath}, Data) ->
    UpdatedStats = increment_stat(cache_hits, Data#chaining_data.statistics),
    ProvenData = Data#chaining_data{
        proof_found = true,
        proof_path = ProofPath,
        statistics = UpdatedStats
    },
    {next_state, completed, ProvenData};
handle_memoized_result({failed, _Reason}, Data) ->
    UpdatedStats = increment_stat(cache_hits, Data#chaining_data.statistics),
    FailedData = Data#chaining_data{
        proof_found = false,
        statistics = UpdatedStats
    },
    {next_state, completed, FailedData}.

check_depth_limit(Data) ->
    case Data#chaining_data.current_depth >= Data#chaining_data.max_depth of
        true -> exceeded;
        false -> within_limit
    end.

check_goal_against_facts(Goal, Facts, Bindings) ->
    check_facts_recursive(Goal, Facts, Bindings).

check_facts_recursive(_Goal, [], _Bindings) ->
    not_proven;
check_facts_recursive(Goal, [Fact | Rest], Bindings) ->
    case unify_terms(Goal, Fact, Bindings) of
        {success, NewBindings} ->
            {proven, NewBindings};
        {failure, _} ->
            check_facts_recursive(Goal, Rest, Bindings)
    end.

find_matching_rules(Goal, Rules, _Data) ->
    lists:filter(fun(Rule) -> rule_matches_goal(Goal, Rule) end, Rules).

rule_matches_goal(_Goal, _Rule) ->
    true.

generate_subgoals_from_rule(_Goal, Rule, _Data) ->
    case Rule of
        {Head, Body} when is_list(Body) ->
            {ok, Body, #{}};
        _ ->
            {ok, [], #{}}
    end.

perform_unification_step(_Goal, _Data) ->
    {success, #{}}.

construct_proof_path(Data) ->
    [Data#chaining_data.main_goal].

optimize_proof(Data) ->
    Data.

mark_goal_proven(Goal, Bindings, Data) ->
    ProofTree = Data#chaining_data.proof_tree,
    UpdatedNode = case maps:get(Goal, ProofTree, undefined) of
        undefined ->
            #proof_node{goal = Goal, proof_status = proven, bindings = Bindings};
        Node ->
            Node#proof_node{proof_status = proven, bindings = Bindings}
    end,
    Data#chaining_data{proof_tree = maps:put(Goal, UpdatedNode, ProofTree)}.

mark_goal_failed(Goal, Reason, Data) ->
    ProofTree = Data#chaining_data.proof_tree,
    UpdatedNode = case maps:get(Goal, ProofTree, undefined) of
        undefined ->
            #proof_node{goal = Goal, proof_status = failed};
        Node ->
            Node#proof_node{proof_status = failed}
    end,
    Data#chaining_data{proof_tree = maps:put(Goal, UpdatedNode, ProofTree)}.

store_matching_rules(Goal, Rules, ProofTree) ->
    case maps:get(Goal, ProofTree, undefined) of
        undefined ->
            Node = #proof_node{goal = Goal, rule = hd(Rules)},
            maps:put(Goal, Node, ProofTree);
        Node ->
            maps:put(Goal, Node#proof_node{rule = hd(Rules)}, ProofTree)
    end.

store_in_memoization_table(Data) ->
    Goal = Data#chaining_data.main_goal,
    Result = case Data#chaining_data.proof_found of
        true -> {proven, Data#chaining_data.proof_path};
        false -> {failed, no_proof}
    end,
    Data#chaining_data{
        memoization_table = maps:put(Goal, Result, Data#chaining_data.memoization_table)
    }.

merge_bindings(Bindings1, Bindings2) ->
    maps:merge(Bindings1, Bindings2).

unify_terms(_Term1, _Term2, Bindings) ->
    {success, Bindings}.

increment_stat(Stat, Stats) ->
    maps:update_with(Stat, fun(X) -> X + 1 end, 1, Stats).

handle_common_events({call, From}, {add_rule, Rule}, Data) ->
    KB = Data#chaining_data.knowledge_base,
    Rules = maps:get(rules, KB, []),
    NewKB = maps:put(rules, [Rule | Rules], KB),
    {keep_state, Data#chaining_data{knowledge_base = NewKB}, [{reply, From, ok}]};
handle_common_events({call, From}, {add_fact, Fact}, Data) ->
    KB = Data#chaining_data.knowledge_base,
    Facts = maps:get(facts, KB, []),
    NewKB = maps:put(facts, [Fact | Facts], KB),
    {keep_state, Data#chaining_data{knowledge_base = NewKB}, [{reply, From, ok}]};
handle_common_events({call, From}, {remove_rule, Rule}, Data) ->
    KB = Data#chaining_data.knowledge_base,
    Rules = maps:get(rules, KB, []),
    NewKB = maps:put(rules, lists:delete(Rule, Rules), KB),
    {keep_state, Data#chaining_data{knowledge_base = NewKB}, [{reply, From, ok}]};
handle_common_events({call, From}, {remove_fact, Fact}, Data) ->
    KB = Data#chaining_data.knowledge_base,
    Facts = maps:get(facts, KB, []),
    NewKB = maps:put(facts, lists:delete(Fact, Facts), KB),
    {keep_state, Data#chaining_data{knowledge_base = NewKB}, [{reply, From, ok}]};
handle_common_events({call, From}, reset_knowledge_base, Data) ->
    NewData = Data#chaining_data{
        knowledge_base = #{facts => [], rules => []},
        memoization_table = #{}
    },
    {keep_state, NewData, [{reply, From, ok}]};
handle_common_events(_EventType, _Event, _Data) ->
    {keep_state_and_data, [postpone]}.