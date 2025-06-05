-module(distributed_error_coordinator).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    report_error/4,
    get_cluster_health/0,
    coordinate_fix/3,
    sync_patterns/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SYNC_INTERVAL, 30000).  % 30 seconds
-define(HEALTH_CHECK_INTERVAL, 10000).  % 10 seconds

-record(state, {
    node_errors = #{} :: map(),  % Node -> ErrorStats
    global_patterns = [] :: list(),
    fix_coordination = #{} :: map(),
    cluster_health = healthy :: atom(),
    consensus_engine :: pid()
}).

-record(node_error_stats, {
    node :: node(),
    error_count = 0 :: integer(),
    error_rate = 0.0 :: float(),
    last_errors = [] :: list(),
    health_score = 100.0 :: float(),
    available_fixes = [] :: list()
}).

-record(distributed_fix, {
    fix_id :: binary(),
    error_pattern :: term(),
    source_node :: node(),
    target_nodes = [] :: list(),
    success_rate = 0.0 :: float(),
    consensus_required = false :: boolean(),
    applied_at = [] :: list()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

report_error(Node, Module, Function, Error) ->
    gen_server:cast(?SERVER, {report_error, Node, Module, Function, Error}).

get_cluster_health() ->
    gen_server:call(?SERVER, get_cluster_health).

coordinate_fix(ErrorPattern, FixStrategy, TargetNodes) ->
    gen_server:call(?SERVER, {coordinate_fix, ErrorPattern, FixStrategy, TargetNodes}).

sync_patterns() ->
    gen_server:cast(?SERVER, sync_patterns).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    error_logger:info_msg("Distributed Error Coordinator starting up...~n"),
    
    %% Start consensus engine
    self() ! start_consensus_engine,
    
    %% Join cluster error coordination
    net_kernel:monitor_nodes(true),
    
    %% Schedule periodic tasks
    erlang:send_after(?SYNC_INTERVAL, self(), sync_patterns),
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), check_cluster_health),
    
    {ok, #state{}}.

handle_call(get_cluster_health, _From, State) ->
    Health = calculate_cluster_health(State#state.node_errors),
    {reply, Health, State};

handle_call({coordinate_fix, ErrorPattern, FixStrategy, TargetNodes}, _From, State) ->
    Fix = create_distributed_fix(ErrorPattern, FixStrategy, TargetNodes),
    
    %% Check if consensus is needed
    case needs_consensus(Fix, State) of
        true ->
            Result = coordinate_with_consensus(Fix, State),
            {reply, Result, State};
        false ->
            Result = apply_distributed_fix(Fix, State),
            {reply, Result, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({report_error, Node, Module, Function, Error}, State) ->
    %% Update node error statistics
    NewState = update_node_errors(Node, Module, Function, Error, State),
    
    %% Check if this error pattern is spreading
    case is_error_spreading(Error, NewState) of
        true ->
            coordinate_preventive_measures(Error, NewState);
        false ->
            ok
    end,
    
    {noreply, NewState};

handle_cast(sync_patterns, State) ->
    %% Sync error patterns across cluster
    NewState = synchronize_patterns(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start_consensus_engine, State) ->
    ConsensusPid = spawn_link(fun() -> consensus_engine_loop() end),
    {noreply, State#state{consensus_engine = ConsensusPid}};

handle_info(sync_patterns, State) ->
    %% Perform pattern synchronization
    NewState = synchronize_patterns(State),
    
    %% Schedule next sync
    erlang:send_after(?SYNC_INTERVAL, self(), sync_patterns),
    
    {noreply, NewState};

handle_info(check_cluster_health, State) ->
    %% Check health of all nodes
    NewState = perform_health_check(State),
    
    %% Take action if cluster health is degrading
    case NewState#state.cluster_health of
        critical ->
            initiate_emergency_measures(NewState);
        degraded ->
            initiate_preventive_measures(NewState);
        _ ->
            ok
    end,
    
    %% Schedule next check
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), check_cluster_health),
    
    {noreply, NewState};

handle_info({nodedown, Node}, State) ->
    error_logger:warning_msg("🔴 Node ~p went down~n", [Node]),
    
    %% Remove node from tracking
    NewErrors = maps:remove(Node, State#state.node_errors),
    
    %% Redistribute any pending fixes
    redistribute_fixes(Node, State),
    
    {noreply, State#state{node_errors = NewErrors}};

handle_info({nodeup, Node}, State) ->
    error_logger:info_msg("🟢 Node ~p came up~n", [Node]),
    
    %% Initialize node tracking
    NewErrors = maps:put(Node, #node_error_stats{node = Node}, 
                        State#state.node_errors),
    
    %% Share current patterns with new node
    share_patterns_with_node(Node, State#state.global_patterns),
    
    {noreply, State#state{node_errors = NewErrors}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions - Error Tracking
%%%===================================================================

update_node_errors(Node, Module, Function, Error, State) ->
    NodeStats = case maps:find(Node, State#state.node_errors) of
        {ok, Stats} -> Stats;
        error -> #node_error_stats{node = Node}
    end,
    
    %% Update error statistics
    UpdatedStats = NodeStats#node_error_stats{
        error_count = NodeStats#node_error_stats.error_count + 1,
        last_errors = [{erlang:timestamp(), Module, Function, Error} | 
                      lists:sublist(NodeStats#node_error_stats.last_errors, 99)],
        error_rate = calculate_error_rate(NodeStats),
        health_score = calculate_node_health(NodeStats)
    },
    
    %% Check if node has a fix for this error
    case auto_error_fixer:apply_learned_fix(Module, Function) of
        {ok, FixType} ->
            %% Share successful fix with other nodes
            broadcast_fix(Node, Module, Function, FixType, Error);
        _ ->
            ok
    end,
    
    State#state{
        node_errors = maps:put(Node, UpdatedStats, State#state.node_errors)
    }.

calculate_error_rate(#node_error_stats{last_errors = Errors}) ->
    Now = erlang:timestamp(),
    RecentErrors = [E || {Time, _, _, _} = E <- Errors,
                        timer:now_diff(Now, Time) < 60000000],  % Last minute
    length(RecentErrors) / 60.0.  % Errors per second

calculate_node_health(#node_error_stats{error_rate = Rate, error_count = Count}) ->
    %% Simple health score based on error rate and total errors
    BaseScore = 100.0,
    RatePenalty = min(50.0, Rate * 10),
    CountPenalty = min(30.0, Count / 100),
    max(0.0, BaseScore - RatePenalty - CountPenalty).

is_error_spreading(Error, State) ->
    %% Check if similar error is occurring on multiple nodes
    ErrorType = element(1, Error),
    NodesWithError = lists:filter(fun({_Node, Stats}) ->
        lists:any(fun({_, _, _, E}) ->
            element(1, E) =:= ErrorType
        end, Stats#node_error_stats.last_errors)
    end, maps:to_list(State#state.node_errors)),
    
    length(NodesWithError) >= 3.  % Threshold for spreading

coordinate_preventive_measures(Error, State) ->
    error_logger:warning_msg("⚠️ Error spreading detected: ~p~n", [Error]),
    
    %% Notify all nodes to take preventive action
    Nodes = [node() | nodes()],
    lists:foreach(fun(Node) ->
        gen_server:cast({predictive_error_prevention, Node}, 
                       {apply_preventive_measures, [Error]})
    end, Nodes).

%%%===================================================================
%%% Internal functions - Pattern Synchronization
%%%===================================================================

synchronize_patterns(State) ->
    %% Collect patterns from all nodes
    AllPatterns = collect_patterns_from_nodes(),
    
    %% Merge with local patterns
    LocalPatterns = get_local_patterns(),
    MergedPatterns = merge_patterns(LocalPatterns, AllPatterns),
    
    %% Distribute merged patterns
    distribute_patterns(MergedPatterns),
    
    State#state{global_patterns = MergedPatterns}.

collect_patterns_from_nodes() ->
    Nodes = nodes(),
    
    %% Request patterns from each node
    Responses = lists:map(fun(Node) ->
        try
            gen_server:call({auto_error_fixer, Node}, get_error_patterns, 5000)
        catch
            _:_ -> []
        end
    end, Nodes),
    
    lists:flatten(Responses).

get_local_patterns() ->
    try
        gen_server:call(auto_error_fixer, get_error_patterns)
    catch
        _:_ -> []
    end.

merge_patterns(Local, Remote) ->
    %% Combine patterns, preferring those with higher success rates
    AllPatterns = Local ++ Remote,
    
    %% Group by pattern signature
    Grouped = lists:foldl(fun(Pattern, Acc) ->
        Key = pattern_signature(Pattern),
        dict:append(Key, Pattern, Acc)
    end, dict:new(), AllPatterns),
    
    %% Select best pattern from each group
    lists:map(fun({_Key, Patterns}) ->
        % Find pattern with highest success rate
        lists:foldl(fun(P, Best) ->
            PRate = maps:get(success_rate, P, 0.0),
            BestRate = maps:get(success_rate, Best, 0.0),
            if PRate > BestRate -> P; true -> Best end
        end, hd(Patterns), tl(Patterns))
    end, dict:to_list(Grouped)).

pattern_signature(Pattern) ->
    %% Create unique signature for pattern
    {maps:get(module, Pattern),
     maps:get(function, Pattern),
     maps:get(error_type, Pattern)}.

distribute_patterns(Patterns) ->
    %% Share patterns with all nodes
    Nodes = nodes(),
    lists:foreach(fun(Node) ->
        lists:foreach(fun(Pattern) ->
            gen_server:cast({auto_error_fixer, Node}, 
                          {inject_pattern, Pattern, 
                           maps:get(success_rate, Pattern, 0.5)})
        end, Patterns)
    end, Nodes).

%%%===================================================================
%%% Internal functions - Fix Coordination
%%%===================================================================

create_distributed_fix(ErrorPattern, FixStrategy, TargetNodes) ->
    #distributed_fix{
        fix_id = generate_fix_id(),
        error_pattern = ErrorPattern,
        source_node = node(),
        target_nodes = TargetNodes,
        consensus_required = length(TargetNodes) > 3
    }.

needs_consensus(Fix, State) ->
    %% Require consensus for critical fixes affecting multiple nodes
    Fix#distributed_fix.consensus_required orelse
    is_critical_fix(Fix#distributed_fix.error_pattern).

coordinate_with_consensus(Fix, State) ->
    case State#state.consensus_engine of
        undefined -> {error, no_consensus_engine};
        Pid ->
            Pid ! {propose_fix, Fix, self()},
            receive
                {consensus_reached, approved} ->
                    apply_distributed_fix(Fix, State);
                {consensus_reached, rejected} ->
                    {error, consensus_rejected}
            after 10000 ->
                {error, consensus_timeout}
            end
    end.

apply_distributed_fix(Fix, _State) ->
    error_logger:info_msg("🔧 Applying distributed fix ~p to nodes ~p~n",
                         [Fix#distributed_fix.fix_id, Fix#distributed_fix.target_nodes]),
    
    %% Apply fix to each target node
    Results = lists:map(fun(Node) ->
        apply_fix_to_node(Node, Fix)
    end, Fix#distributed_fix.target_nodes),
    
    %% Calculate success rate
    SuccessCount = length([R || R = {ok, _} <- Results]),
    SuccessRate = SuccessCount / length(Results),
    
    %% Broadcast results
    broadcast_fix_results(Fix, Results, SuccessRate),
    
    {ok, #{
        fix_id => Fix#distributed_fix.fix_id,
        success_rate => SuccessRate,
        results => Results
    }}.

apply_fix_to_node(Node, Fix) when Node =:= node() ->
    %% Apply fix locally
    apply_local_fix(Fix);
apply_fix_to_node(Node, Fix) ->
    %% Apply fix remotely
    try
        gen_server:call({auto_error_fixer, Node}, 
                       {apply_fix, Fix#distributed_fix.error_pattern}, 5000)
    catch
        Type:Reason ->
            {error, {Node, Type, Reason}}
    end.

apply_local_fix(Fix) ->
    %% Extract module and function from error pattern
    case Fix#distributed_fix.error_pattern of
        #{module := Module, function := Function} ->
            auto_error_fixer:apply_learned_fix(Module, Function);
        _ ->
            {error, invalid_pattern}
    end.

broadcast_fix_results(Fix, Results, SuccessRate) ->
    Msg = {fix_applied, Fix#distributed_fix.fix_id, SuccessRate, Results},
    lists:foreach(fun(Node) ->
        gen_server:cast({?MODULE, Node}, Msg)
    end, nodes()).

%%%===================================================================
%%% Internal functions - Health Monitoring
%%%===================================================================

calculate_cluster_health(NodeErrors) ->
    NodeHealthScores = [Stats#node_error_stats.health_score || 
                       {_Node, Stats} <- maps:to_list(NodeErrors)],
    
    case NodeHealthScores of
        [] -> healthy;
        Scores ->
            AvgHealth = lists:sum(Scores) / length(Scores),
            if
                AvgHealth >= 80.0 -> healthy;
                AvgHealth >= 60.0 -> degraded;
                true -> critical
            end
    end.

perform_health_check(State) ->
    %% Update health scores for all nodes
    UpdatedErrors = maps:map(fun(_Node, Stats) ->
        Stats#node_error_stats{
            health_score = calculate_node_health(Stats)
        }
    end, State#state.node_errors),
    
    %% Calculate overall cluster health
    ClusterHealth = calculate_cluster_health(UpdatedErrors),
    
    State#state{
        node_errors = UpdatedErrors,
        cluster_health = ClusterHealth
    }.

initiate_emergency_measures(State) ->
    error_logger:error_msg("🚨 CLUSTER HEALTH CRITICAL - Initiating emergency measures~n"),
    
    %% Find healthiest nodes
    HealthyNodes = find_healthy_nodes(State#state.node_errors),
    
    %% Redirect traffic to healthy nodes
    lists:foreach(fun(Node) ->
        gen_server:cast({load_balancer, Node}, {redirect_to, HealthyNodes})
    end, nodes()),
    
    %% Trigger aggressive error recovery
    trigger_cluster_recovery(State).

initiate_preventive_measures(State) ->
    error_logger:warning_msg("⚠️ Cluster health degraded - Initiating preventive measures~n"),
    
    %% Identify problematic nodes
    ProblematicNodes = find_problematic_nodes(State#state.node_errors),
    
    %% Apply preventive fixes
    lists:foreach(fun(Node) ->
        apply_preventive_fixes(Node, State)
    end, ProblematicNodes).

find_healthy_nodes(NodeErrors) ->
    HealthyList = [{Node, Stats} || {Node, Stats} <- maps:to_list(NodeErrors),
                                    Stats#node_error_stats.health_score >= 80.0],
    [Node || {Node, _} <- lists:sort(fun({_, S1}, {_, S2}) ->
        S1#node_error_stats.health_score > S2#node_error_stats.health_score
    end, HealthyList)].

find_problematic_nodes(NodeErrors) ->
    [Node || {Node, Stats} <- maps:to_list(NodeErrors),
             Stats#node_error_stats.health_score < 60.0].

trigger_cluster_recovery(State) ->
    %% Collect all recent errors
    AllErrors = lists:flatten([Stats#node_error_stats.last_errors || 
                              {_, Stats} <- maps:to_list(State#state.node_errors)]),
    
    %% Group by error type
    GroupedErrors = group_errors_by_type(AllErrors),
    
    %% Apply fixes for most common errors
    lists:foreach(fun({ErrorType, Errors}) when length(Errors) > 5 ->
        coordinate_cluster_wide_fix(ErrorType, State)
    end, GroupedErrors).

%%%===================================================================
%%% Internal functions - Consensus Engine
%%%===================================================================

consensus_engine_loop() ->
    consensus_engine_loop(#{}).

consensus_engine_loop(Proposals) ->
    receive
        {propose_fix, Fix, From} ->
            %% Simple majority consensus
            ProposalId = Fix#distributed_fix.fix_id,
            Nodes = [node() | nodes()],
            
            %% Request votes
            lists:foreach(fun(Node) ->
                gen_server:cast({?MODULE, Node}, {vote_request, ProposalId, Fix})
            end, Nodes),
            
            %% Collect votes
            erlang:send_after(5000, self(), {close_voting, ProposalId, From}),
            
            consensus_engine_loop(maps:put(ProposalId, {Fix, From, []}, Proposals));
            
        {vote, ProposalId, Vote, Node} ->
            case maps:find(ProposalId, Proposals) of
                {ok, {Fix, From, Votes}} ->
                    NewVotes = [{Node, Vote} | Votes],
                    consensus_engine_loop(maps:put(ProposalId, {Fix, From, NewVotes}, 
                                                  Proposals));
                error ->
                    consensus_engine_loop(Proposals)
            end;
            
        {close_voting, ProposalId, From} ->
            case maps:find(ProposalId, Proposals) of
                {ok, {_Fix, From, Votes}} ->
                    %% Count votes
                    YesVotes = length([V || V = {_, yes} <- Votes]),
                    NoVotes = length([V || V = {_, no} <- Votes]),
                    
                    Result = if YesVotes > NoVotes -> approved;
                               true -> rejected
                            end,
                    
                    From ! {consensus_reached, Result},
                    consensus_engine_loop(maps:remove(ProposalId, Proposals));
                error ->
                    consensus_engine_loop(Proposals)
            end;
            
        stop ->
            ok
    end.

%%%===================================================================
%%% Helper functions
%%%===================================================================

generate_fix_id() ->
    crypto:strong_rand_bytes(16).

is_critical_fix(ErrorPattern) ->
    %% Determine if fix is critical based on error type
    case maps:get(error_type, ErrorPattern, unknown) of
        out_of_memory -> true;
        system_limit -> true;
        node_disconnected -> true;
        _ -> false
    end.

share_patterns_with_node(Node, Patterns) ->
    lists:foreach(fun(Pattern) ->
        gen_server:cast({auto_error_fixer, Node}, 
                       {inject_pattern, Pattern, 
                        maps:get(success_rate, Pattern, 0.5)})
    end, Patterns).

redistribute_fixes(DownNode, State) ->
    %% Find fixes that were targeting the down node
    lists:foreach(fun({FixId, Fix}) ->
        case lists:member(DownNode, Fix#distributed_fix.target_nodes) of
            true ->
                %% Remove down node and reapply to remaining nodes
                RemainingNodes = Fix#distributed_fix.target_nodes -- [DownNode],
                case RemainingNodes of
                    [] -> ok;
                    _ ->
                        NewFix = Fix#distributed_fix{target_nodes = RemainingNodes},
                        apply_distributed_fix(NewFix, State)
                end;
            false ->
                ok
        end
    end, maps:to_list(State#state.fix_coordination)).

broadcast_fix(SourceNode, Module, Function, FixType, Error) ->
    %% Share successful fix with all nodes
    Fix = #{
        source_node => SourceNode,
        module => Module,
        function => Function,
        fix_type => FixType,
        error => Error,
        timestamp => erlang:timestamp()
    },
    
    lists:foreach(fun(Node) ->
        gen_server:cast({?MODULE, Node}, {successful_fix, Fix})
    end, nodes()).

group_errors_by_type(Errors) ->
    Grouped = lists:foldl(fun({_Time, _Module, _Function, Error}, Acc) ->
        Type = element(1, Error),
        dict:append(Type, Error, Acc)
    end, dict:new(), Errors),
    
    dict:to_list(Grouped).

coordinate_cluster_wide_fix(ErrorType, State) ->
    %% Create a fix for this error type
    Fix = #distributed_fix{
        fix_id = generate_fix_id(),
        error_pattern = #{error_type => ErrorType},
        source_node = node(),
        target_nodes = [node() | nodes()],
        consensus_required = true
    },
    
    coordinate_with_consensus(Fix, State).

apply_preventive_fixes(Node, _State) ->
    %% Trigger preventive measures on specific node
    gen_server:cast({predictive_error_prevention, Node}, 
                   {apply_preventive_measures, []}).