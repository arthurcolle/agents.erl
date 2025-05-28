%% hierarchical_task_executor.erl
%% Hierarchical task execution with recursive decomposition
%% Advanced gen_statem for complex task hierarchies and dependencies
-module(hierarchical_task_executor).
-behaviour(gen_statem).

-export([
    start_link/1,
    submit_task/2,
    decompose_task/2,
    execute_subtasks/2,
    aggregate_results/1,
    escalate_task/2,
    delegate_task/3,
    monitor_task_hierarchy/1,
    rebalance_load/1,
    optimize_execution_order/1
]).

-export([init/1, callback_mode/0, terminate/3]).
-export([
    idle/3,
    analyzing/3,
    decomposing/3,
    planning/3,
    executing/3,
    aggregating/3,
    optimizing/3,
    escalating/3,
    completed/3
]).

-record(task_data, {
    task_id,
    root_task,
    task_hierarchy = #{},
    execution_tree = #{},
    dependency_graph = #{},
    resource_allocation = #{},
    execution_timeline = [],
    performance_metrics = #{},
    optimization_history = [],
    delegation_map = #{},
    escalation_rules = #{},
    completion_criteria = #{},
    current_focus = [],
    cognitive_load = 0.0
}).

-record(task_node, {
    node_id,
    parent_id,
    children_ids = [],
    task_spec,
    complexity_level = medium,
    resource_requirements = #{},
    dependencies = [],
    estimated_duration = 5000,
    actual_duration = undefined,
    execution_status = pending, % pending, executing, completed, failed, delegated
    result = undefined,
    delegation_target = undefined,
    optimization_metadata = #{},
    learning_data = #{}
}).

-record(execution_plan, {
    plan_id,
    task_ordering = [],
    parallelization_groups = [],
    resource_schedule = #{},
    dependency_resolution = #{},
    optimization_strategy = balanced,
    risk_mitigation = #{},
    adaptive_parameters = #{},
    performance_targets = #{}
}).

%% Public API

start_link(TaskConfig) ->
    gen_statem:start_link(?MODULE, TaskConfig, []).

submit_task(ExecutorPid, TaskSpec) ->
    gen_statem:call(ExecutorPid, {submit_task, TaskSpec}).

decompose_task(ExecutorPid, TaskId) ->
    gen_statem:call(ExecutorPid, {decompose, TaskId}).

execute_subtasks(ExecutorPid, SubtaskList) ->
    gen_statem:call(ExecutorPid, {execute_subtasks, SubtaskList}).

aggregate_results(ExecutorPid) ->
    gen_statem:call(ExecutorPid, aggregate_results).

escalate_task(ExecutorPid, TaskId) ->
    gen_statem:call(ExecutorPid, {escalate, TaskId}).

delegate_task(ExecutorPid, TaskId, DelegationTarget) ->
    gen_statem:call(ExecutorPid, {delegate, TaskId, DelegationTarget}).

monitor_task_hierarchy(ExecutorPid) ->
    gen_statem:call(ExecutorPid, monitor_hierarchy).

rebalance_load(ExecutorPid) ->
    gen_statem:call(ExecutorPid, rebalance_load).

optimize_execution_order(ExecutorPid) ->
    gen_statem:call(ExecutorPid, optimize_order).

%% Gen_statem callbacks

init(TaskConfig) ->
    TaskId = generate_task_id(),
    
    Data = #task_data{
        task_id = TaskId,
        escalation_rules = maps:get(escalation_rules, TaskConfig, #{}),
        completion_criteria = maps:get(completion_criteria, TaskConfig, #{}),
        performance_metrics = initialize_performance_metrics(),
        cognitive_load = 0.0
    },
    
    {ok, idle, Data}.

callback_mode() ->
    state_functions.

%% State: idle - Waiting for task submission
idle({call, From}, {submit_task, TaskSpec}, Data) ->
    % New root task submitted
    RootTaskId = generate_task_id(),
    RootTask = create_task_node(RootTaskId, undefined, TaskSpec),
    
    NewData = Data#task_data{
        root_task = RootTask,
        task_hierarchy = #{RootTaskId => RootTask},
        current_focus = [RootTaskId]
    },
    
    {next_state, analyzing, NewData, [{reply, From, {ok, RootTaskId}}]};

idle({call, From}, _Request, _Data) ->
    {keep_state_and_data, [{reply, From, {error, no_active_task}}]};

idle(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data, idle).

%% State: analyzing - Analyzing task complexity and requirements
analyzing(enter, _OldState, Data) ->
    % Analyze root task complexity
    RootTask = Data#task_data.root_task,
    spawn(fun() -> 
        Analysis = analyze_task_complexity(RootTask),
        gen_statem:cast(self(), {analysis_complete, Analysis})
    end),
    {keep_state_and_data, [{state_timeout, 5000, analysis_timeout}]};

analyzing(cast, {analysis_complete, Analysis}, Data) ->
    % Task analysis complete, decide next action
    case maps:get(decomposition_needed, Analysis, false) of
        true ->
            % Task needs decomposition
            AnalyzedData = store_task_analysis(Analysis, Data),
            {next_state, decomposing, AnalyzedData};
        false ->
            % Task can be executed directly
            DirectData = prepare_direct_execution(Analysis, Data),
            {next_state, executing, DirectData}
    end;

analyzing(state_timeout, analysis_timeout, Data) ->
    % Analysis timeout, assume simple direct execution
    {next_state, executing, Data};

analyzing(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data, analyzing).

%% State: decomposing - Breaking down complex tasks into subtasks
decomposing(enter, _OldState, Data) ->
    % Start task decomposition process
    RootTask = Data#task_data.root_task,
    spawn(fun() ->
        DecompositionResult = perform_task_decomposition(RootTask, Data),
        gen_statem:cast(self(), {decomposition_complete, DecompositionResult})
    end),
    {keep_state_and_data, [{state_timeout, 10000, decomposition_timeout}]};

decomposing(cast, {decomposition_complete, DecompositionResult}, Data) ->
    case DecompositionResult of
        {success, SubtaskHierarchy, DependencyGraph} ->
            % Decomposition successful, update hierarchy
            NewData = Data#task_data{
                task_hierarchy = merge_task_hierarchy(SubtaskHierarchy, Data#task_data.task_hierarchy),
                dependency_graph = DependencyGraph
            },
            {next_state, planning, NewData};
        {partial, PartialHierarchy, UnresolvedTasks} ->
            % Partial decomposition, handle unresolved tasks
            PartialData = handle_partial_decomposition(PartialHierarchy, UnresolvedTasks, Data),
            {next_state, planning, PartialData};
        {failure, Reason} ->
            % Decomposition failed, escalate or simplify
            case should_escalate_decomposition_failure(Reason, Data) of
                true ->
                    {next_state, escalating, add_escalation_reason(Reason, Data)};
                false ->
                    SimplifiedData = create_simplified_execution_plan(Data),
                    {next_state, planning, SimplifiedData}
            end
    end;

decomposing({call, From}, {decompose, TaskId}, Data) ->
    % Manual decomposition request
    case maps:get(TaskId, Data#task_data.task_hierarchy, undefined) of
        undefined ->
            {keep_state_and_data, [{reply, From, {error, task_not_found}}]};
        TaskNode ->
            spawn(fun() ->
                DecompResult = decompose_specific_task(TaskNode, Data),
                gen_statem:cast(self(), {manual_decomposition_complete, TaskId, DecompResult})
            end),
            {keep_state_and_data, [{reply, From, {ok, decomposition_started}}]}
    end;

decomposing(cast, {manual_decomposition_complete, TaskId, Result}, Data) ->
    % Manual decomposition completed
    UpdatedData = update_task_decomposition(TaskId, Result, Data),
    {keep_state, UpdatedData};

decomposing(state_timeout, decomposition_timeout, Data) ->
    % Decomposition timeout, create simplified plan
    SimplifiedData = create_emergency_execution_plan(Data),
    {next_state, planning, SimplifiedData};

decomposing(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data, decomposing).

%% State: planning - Creating execution plan for task hierarchy
planning(enter, _OldState, Data) ->
    % Create comprehensive execution plan
    spawn(fun() ->
        ExecutionPlan = create_comprehensive_execution_plan(Data),
        gen_statem:cast(self(), {planning_complete, ExecutionPlan})
    end),
    {keep_state_and_data, [{state_timeout, 8000, planning_timeout}]};

planning(cast, {planning_complete, ExecutionPlan}, Data) ->
    % Execution plan ready, validate and proceed
    case validate_execution_plan(ExecutionPlan, Data) of
        {valid, ValidatedPlan} ->
            PlannedData = Data#task_data{execution_tree = ValidatedPlan},
            {next_state, executing, PlannedData};
        {invalid, ValidationErrors} ->
            % Plan validation failed, replan
            ReplanData = handle_planning_errors(ValidationErrors, Data),
            {repeat_state, ReplanData};
        {needs_optimization, OptimizationNeeds} ->
            % Plan needs optimization before execution
            OptData = prepare_for_optimization(OptimizationNeeds, ExecutionPlan, Data),
            {next_state, optimizing, OptData}
    end;

planning({call, From}, optimize_order, Data) ->
    % Manual optimization request during planning
    spawn(fun() ->
        OptimizedOrder = optimize_task_execution_order(Data),
        gen_statem:cast(self(), {optimization_result, OptimizedOrder})
    end),
    {keep_state_and_data, [{reply, From, {ok, optimization_started}}]};

planning(cast, {optimization_result, OptimizedOrder}, Data) ->
    % Apply optimization results
    OptimizedData = apply_execution_optimization(OptimizedOrder, Data),
    {keep_state, OptimizedData};

planning(state_timeout, planning_timeout, Data) ->
    % Planning timeout, use current plan
    DefaultPlan = create_default_execution_plan(Data),
    PlannedData = Data#task_data{execution_tree = DefaultPlan},
    {next_state, executing, PlannedData};

planning(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data, planning).

%% State: executing - Active execution of task hierarchy
executing(enter, _OldState, Data) ->
    % Start execution of planned tasks
    start_hierarchical_execution(Data),
    {keep_state_and_data, []};

executing(cast, {task_completed, TaskId, Result}, Data) ->
    % Individual task completed
    UpdatedData = record_task_completion(TaskId, Result, Data),
    
    case check_hierarchy_completion_status(UpdatedData) of
        {completed, FinalResults} ->
            % All tasks completed
            CompletedData = UpdatedData#task_data{
                performance_metrics = calculate_final_metrics(FinalResults, UpdatedData)
            },
            {next_state, completed, CompletedData};
        {partial, CompletionStatus} ->
            % Some tasks completed, continue execution
            ContinuedData = continue_hierarchical_execution(CompletionStatus, UpdatedData),
            {keep_state, ContinuedData};
        {blocked, BlockingIssues} ->
            % Execution blocked, need intervention
            case resolve_execution_blocks(BlockingIssues, UpdatedData) of
                {resolved, ResolvedData} ->
                    {keep_state, ResolvedData};
                {needs_optimization, OptData} ->
                    {next_state, optimizing, OptData};
                {needs_escalation, EscData} ->
                    {next_state, escalating, EscData}
            end
    end;

executing(cast, {task_failed, TaskId, Reason}, Data) ->
    % Individual task failed
    FailureData = record_task_failure(TaskId, Reason, Data),
    
    case analyze_failure_impact(TaskId, Reason, FailureData) of
        {recoverable, RecoveryStrategy} ->
            % Failure is recoverable, execute recovery
            RecoveredData = execute_failure_recovery(RecoveryStrategy, FailureData),
            {keep_state, RecoveredData};
        {needs_replan, ReplanningNeeds} ->
            % Need to replan execution
            ReplanData = initiate_replanning(ReplanningNeeds, FailureData),
            {next_state, planning, ReplanData};
        {critical, CriticalFailure} ->
            % Critical failure, escalate
            EscalationData = prepare_critical_escalation(CriticalFailure, FailureData),
            {next_state, escalating, EscalationData}
    end;

executing({call, From}, {execute_subtasks, SubtaskList}, Data) ->
    % Execute specific subtasks
    case validate_subtask_list(SubtaskList, Data) of
        {valid, ValidatedSubtasks} ->
            execute_specific_subtasks(ValidatedSubtasks, Data),
            {keep_state_and_data, [{reply, From, {ok, subtasks_started}}]};
        {invalid, ValidationErrors} ->
            {keep_state_and_data, [{reply, From, {error, ValidationErrors}}]}
    end;

executing({call, From}, {delegate, TaskId, Target}, Data) ->
    % Delegate specific task
    case delegate_task_to_target(TaskId, Target, Data) of
        {ok, DelegatedData} ->
            {keep_state, DelegatedData, [{reply, From, {ok, task_delegated}}]};
        {error, DelegationError} ->
            {keep_state_and_data, [{reply, From, {error, DelegationError}}]}
    end;

executing({call, From}, rebalance_load, Data) ->
    % Rebalance execution load
    RebalancedData = perform_load_rebalancing(Data),
    {keep_state, RebalancedData, [{reply, From, {ok, load_rebalanced}}]};

executing(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data, executing).

%% State: aggregating - Aggregating results from completed subtasks
aggregating(enter, _OldState, Data) ->
    % Aggregate results from completed tasks
    spawn(fun() ->
        AggregationResult = aggregate_hierarchical_results(Data),
        gen_statem:cast(self(), {aggregation_complete, AggregationResult})
    end),
    {keep_state_and_data, [{state_timeout, 5000, aggregation_timeout}]};

aggregating(cast, {aggregation_complete, AggregationResult}, Data) ->
    case AggregationResult of
        {success, AggregatedResults} ->
            % Aggregation successful
            CompletedData = Data#task_data{
                performance_metrics = finalize_performance_metrics(AggregatedResults, Data)
            },
            {next_state, completed, CompletedData};
        {partial, PartialResults, MissingData} ->
            % Partial aggregation, handle missing data
            PartialData = handle_partial_aggregation(PartialResults, MissingData, Data),
            {repeat_state, PartialData};
        {failure, AggregationFailure} ->
            % Aggregation failed, escalate
            FailureData = add_aggregation_failure(AggregationFailure, Data),
            {next_state, escalating, FailureData}
    end;

aggregating({call, From}, aggregate_results, Data) ->
    % Manual aggregation request
    CurrentResults = extract_current_results(Data),
    {keep_state_and_data, [{reply, From, {ok, CurrentResults}}]};

aggregating(state_timeout, aggregation_timeout, Data) ->
    % Aggregation timeout, use partial results
    PartialResults = create_partial_aggregation(Data),
    PartialData = Data#task_data{
        performance_metrics = finalize_performance_metrics(PartialResults, Data)
    },
    {next_state, completed, PartialData};

aggregating(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data, aggregating).

%% State: optimizing - Optimizing execution strategy and resource allocation
optimizing(enter, _OldState, Data) ->
    % Optimize execution strategy
    spawn(fun() ->
        OptimizationResult = perform_execution_optimization(Data),
        gen_statem:cast(self(), {optimization_complete, OptimizationResult})
    end),
    {keep_state_and_data, [{state_timeout, 6000, optimization_timeout}]};

optimizing(cast, {optimization_complete, OptimizationResult}, Data) ->
    case OptimizationResult of
        {improved, OptimizedStrategy} ->
            % Optimization improved strategy
            OptimizedData = apply_optimized_strategy(OptimizedStrategy, Data),
            {next_state, executing, OptimizedData};
        {no_improvement, _CurrentStrategy} ->
            % No improvement found, continue with current strategy
            {next_state, executing, Data};
        {optimization_error, Error} ->
            % Optimization failed, continue without optimization
            ErrorData = record_optimization_error(Error, Data),
            {next_state, executing, ErrorData}
    end;

optimizing(state_timeout, optimization_timeout, Data) ->
    % Optimization timeout, proceed without optimization
    {next_state, executing, Data};

optimizing(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data, optimizing).

%% State: escalating - Escalating issues to higher-level management
escalating(enter, _OldState, Data) ->
    % Escalate issues according to escalation rules
    EscalationRules = Data#task_data.escalation_rules,
    EscalationTarget = determine_escalation_target(EscalationRules, Data),
    escalate_to_target(EscalationTarget, Data),
    {keep_state_and_data, [{state_timeout, 15000, escalation_timeout}]};

escalating(cast, {escalation_response, Response}, Data) ->
    case Response of
        {intervention, InterventionPlan} ->
            % Higher level intervention provided
            InterventionData = apply_escalation_intervention(InterventionPlan, Data),
            {next_state, executing, InterventionData};
        {guidance, GuidanceInstructions} ->
            % Guidance provided, modify execution
            GuidedData = apply_escalation_guidance(GuidanceInstructions, Data),
            {next_state, planning, GuidedData};
        {abort, AbortReason} ->
            % Task aborted by escalation target
            {stop, {aborted, AbortReason}, Data}
    end;

escalating({call, From}, {escalate, TaskId}, Data) ->
    % Manual escalation request
    case escalate_specific_task(TaskId, Data) of
        {ok, EscalatedData} ->
            {keep_state, EscalatedData, [{reply, From, {ok, task_escalated}}]};
        {error, EscalationError} ->
            {keep_state_and_data, [{reply, From, {error, EscalationError}}]}
    end;

escalating(state_timeout, escalation_timeout, Data) ->
    % Escalation timeout, attempt recovery
    RecoveryData = attempt_autonomous_recovery(Data),
    {next_state, executing, RecoveryData};

escalating(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data, escalating).

%% State: completed - Task hierarchy execution completed
completed(enter, _OldState, Data) ->
    % Finalize completion and cleanup
    finalize_task_completion(Data),
    {keep_state_and_data, []};

completed({call, From}, monitor_hierarchy, Data) ->
    % Provide completion monitoring data
    CompletionData = generate_completion_report(Data),
    {keep_state_and_data, [{reply, From, {ok, CompletionData}}]};

completed({call, From}, _Request, _Data) ->
    {keep_state_and_data, [{reply, From, {error, task_completed}}]};

completed(EventType, Event, Data) ->
    handle_common_events(EventType, Event, Data, completed).

%% Common event handling
handle_common_events({call, From}, monitor_hierarchy, Data, _CurrentState) ->
    HierarchyStatus = generate_hierarchy_status(Data),
    {keep_state_and_data, [{reply, From, {ok, HierarchyStatus}}]};

handle_common_events(info, {resource_update, ResourceData}, Data, _CurrentState) ->
    UpdatedData = update_resource_allocation(ResourceData, Data),
    {keep_state, UpdatedData};

handle_common_events(_EventType, _Event, _Data, _CurrentState) ->
    keep_state_and_data.

terminate(_Reason, _StateName, Data) ->
    cleanup_task_resources(Data),
    ok.

%% Internal Functions

generate_task_id() ->
    list_to_binary([
        "task_",
        atom_to_list(node()),
        "_",
        integer_to_list(erlang:system_time(microsecond))
    ]).

create_task_node(NodeId, ParentId, TaskSpec) ->
    #task_node{
        node_id = NodeId,
        parent_id = ParentId,
        task_spec = TaskSpec,
        complexity_level = analyze_task_complexity_level(TaskSpec),
        resource_requirements = extract_resource_requirements(TaskSpec),
        estimated_duration = estimate_task_duration(TaskSpec)
    }.

%% Placeholder implementations for complex hierarchical operations
analyze_task_complexity(_Task) -> #{decomposition_needed => true, complexity => high}.
store_task_analysis(_Analysis, Data) -> Data.
prepare_direct_execution(_Analysis, Data) -> Data.
perform_task_decomposition(_Task, _Data) -> {success, #{}, #{}}.
merge_task_hierarchy(_New, Existing) -> Existing.
handle_partial_decomposition(_Partial, _Unresolved, Data) -> Data.
should_escalate_decomposition_failure(_Reason, _Data) -> false.
add_escalation_reason(_Reason, Data) -> Data.
create_simplified_execution_plan(Data) -> Data.
decompose_specific_task(_TaskNode, _Data) -> decomposition_result.
update_task_decomposition(_TaskId, _Result, Data) -> Data.
create_emergency_execution_plan(Data) -> Data.
create_comprehensive_execution_plan(_Data) -> execution_plan.
validate_execution_plan(_Plan, _Data) -> {valid, validated_plan}.
handle_planning_errors(_Errors, Data) -> Data.
prepare_for_optimization(_Needs, _Plan, Data) -> Data.
optimize_task_execution_order(_Data) -> optimized_order.
apply_execution_optimization(_Order, Data) -> Data.
create_default_execution_plan(_Data) -> default_plan.
start_hierarchical_execution(_Data) -> ok.
record_task_completion(_TaskId, _Result, Data) -> Data.
check_hierarchy_completion_status(_Data) -> {partial, completion_status}.
continue_hierarchical_execution(_Status, Data) -> Data.
resolve_execution_blocks(_Issues, Data) -> {resolved, Data}.
record_task_failure(_TaskId, _Reason, Data) -> Data.
analyze_failure_impact(_TaskId, _Reason, _Data) -> {recoverable, recovery_strategy}.
execute_failure_recovery(_Strategy, Data) -> Data.
initiate_replanning(_Needs, Data) -> Data.
prepare_critical_escalation(_Failure, Data) -> Data.
validate_subtask_list(_List, _Data) -> {valid, validated_list}.
execute_specific_subtasks(_Subtasks, _Data) -> ok.
delegate_task_to_target(_TaskId, _Target, Data) -> {ok, Data}.
perform_load_rebalancing(Data) -> Data.
aggregate_hierarchical_results(_Data) -> {success, aggregated_results}.
finalize_performance_metrics(_Results, _Data) -> #{}.
handle_partial_aggregation(_Partial, _Missing, Data) -> Data.
add_aggregation_failure(_Failure, Data) -> Data.
extract_current_results(_Data) -> current_results.
create_partial_aggregation(_Data) -> partial_results.
perform_execution_optimization(_Data) -> {improved, optimized_strategy}.
apply_optimized_strategy(_Strategy, Data) -> Data.
record_optimization_error(_Error, Data) -> Data.
determine_escalation_target(_Rules, _Data) -> escalation_target.
escalate_to_target(_Target, _Data) -> ok.
apply_escalation_intervention(_Plan, Data) -> Data.
apply_escalation_guidance(_Instructions, Data) -> Data.
escalate_specific_task(_TaskId, Data) -> {ok, Data}.
attempt_autonomous_recovery(Data) -> Data.
finalize_task_completion(_Data) -> ok.
generate_completion_report(_Data) -> completion_report.
generate_hierarchy_status(_Data) -> hierarchy_status.
update_resource_allocation(_ResourceData, Data) -> Data.
cleanup_task_resources(_Data) -> ok.
analyze_task_complexity_level(_TaskSpec) -> medium.
extract_resource_requirements(_TaskSpec) -> #{}.
estimate_task_duration(_TaskSpec) -> 5000.
initialize_performance_metrics() -> #{}.
calculate_final_metrics(_Results, _Data) -> #{}.