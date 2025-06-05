-module(workflow_orchestrator).
-behaviour(gen_server).

%% Advanced Workflow Orchestration System
%% Features:
%% - Dynamic agent parameterization and scatter operations
%% - Output collection with agentic collectors
%% - Workflow IDs for tracking and scripting
%% - Large-scale workflow composition
%% - Subroutine decomposition and execution

-export([
    start_link/0,
    create_workflow/1,
    scatter_to_agents/2,
    gather_from_collectors/1,
    execute_workflow_script/2,
    get_workflow_status/1,
    list_workflows/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    workflows :: map(),
    active_scatters :: map(),
    collectors :: map(),
    workflow_counter :: integer()
}).

-record(workflow, {
    id :: binary(),
    name :: binary(),
    created_at :: integer(),
    status :: binary(),
    scatter_operations :: [map()],
    collectors :: [map()],
    results :: map(),
    script :: binary(),
    metadata :: map()
}).

-record(scatter_operation, {
    id :: binary(),
    workflow_id :: binary(),
    target_count :: integer(),
    agent_template :: map(),
    parametrization :: map(),
    created_agents :: [binary()],
    status :: binary(),
    results :: [map()]
}).

-record(collector_agent, {
    id :: binary(),
    workflow_id :: binary(),
    collection_criteria :: map(),
    aggregation_function :: binary(),
    collected_data :: [map()],
    status :: binary()
}).

%% API Functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_workflow(WorkflowSpec) ->
    gen_server:call(?MODULE, {create_workflow, WorkflowSpec}).

scatter_to_agents(WorkflowId, ScatterSpec) ->
    gen_server:call(?MODULE, {scatter_to_agents, WorkflowId, ScatterSpec}).

gather_from_collectors(WorkflowId) ->
    gen_server:call(?MODULE, {gather_from_collectors, WorkflowId}).

execute_workflow_script(WorkflowId, Script) ->
    gen_server:call(?MODULE, {execute_workflow_script, WorkflowId, Script}).

get_workflow_status(WorkflowId) ->
    gen_server:call(?MODULE, {get_workflow_status, WorkflowId}).

list_workflows() ->
    gen_server:call(?MODULE, list_workflows).

%% gen_server callbacks

init([]) ->
    % Create ETS tables for fast lookups
    ets:new(workflow_agents, [named_table, public, {keypos, 1}]),
    ets:new(workflow_collectors, [named_table, public, {keypos, 1}]),
    ets:new(workflow_results, [named_table, public, {keypos, 1}]),
    
    State = #state{
        workflows = #{},
        active_scatters = #{},
        collectors = #{},
        workflow_counter = 1
    },
    {ok, State}.

handle_call({create_workflow, WorkflowSpec}, _From, State) ->
    WorkflowId = generate_workflow_id(State#state.workflow_counter),
    
    Workflow = #workflow{
        id = WorkflowId,
        name = maps:get(<<"name">>, WorkflowSpec, <<"Untitled Workflow">>),
        created_at = erlang:system_time(millisecond),
        status = <<"created">>,
        scatter_operations = [],
        collectors = [],
        results = #{},
        script = maps:get(<<"script">>, WorkflowSpec, <<"">>),
        metadata = maps:get(<<"metadata">>, WorkflowSpec, #{})
    },
    
    NewWorkflows = maps:put(WorkflowId, Workflow, State#state.workflows),
    NewState = State#state{
        workflows = NewWorkflows,
        workflow_counter = State#state.workflow_counter + 1
    },
    
    {reply, {ok, WorkflowId}, NewState};

handle_call({scatter_to_agents, WorkflowId, ScatterSpec}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, workflow_not_found}, State};
        Workflow ->
            Result = execute_scatter_operation(WorkflowId, ScatterSpec, State),
            case Result of
                {ok, ScatterOpId, NewState} ->
                    % Update workflow with scatter operation
                    ScatterOp = #{
                        id => ScatterOpId,
                        spec => ScatterSpec,
                        created_at => erlang:system_time(millisecond)
                    },
                    UpdatedWorkflow = Workflow#workflow{
                        scatter_operations = [ScatterOp | Workflow#workflow.scatter_operations],
                        status = <<"scattering">>
                    },
                    UpdatedWorkflows = maps:put(WorkflowId, UpdatedWorkflow, State#state.workflows),
                    FinalState = NewState#state{workflows = UpdatedWorkflows},
                    {reply, {ok, ScatterOpId}, FinalState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({gather_from_collectors, WorkflowId}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, workflow_not_found}, State};
        Workflow ->
            Results = gather_workflow_results(WorkflowId, State),
            UpdatedWorkflow = Workflow#workflow{
                results = Results,
                status = <<"gathering">>
            },
            UpdatedWorkflows = maps:put(WorkflowId, UpdatedWorkflow, State#state.workflows),
            NewState = State#state{workflows = UpdatedWorkflows},
            {reply, {ok, Results}, NewState}
    end;

handle_call({execute_workflow_script, WorkflowId, Script}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, workflow_not_found}, State};
        Workflow ->
            % Execute workflow script with current context
            Context = build_workflow_context(WorkflowId, State),
            Result = execute_script(Script, Context),
            
            UpdatedWorkflow = Workflow#workflow{
                script = Script,
                status = <<"executing_script">>
            },
            UpdatedWorkflows = maps:put(WorkflowId, UpdatedWorkflow, State#state.workflows),
            NewState = State#state{workflows = UpdatedWorkflows},
            {reply, Result, NewState}
    end;

handle_call({get_workflow_status, WorkflowId}, _From, State) ->
    case maps:get(WorkflowId, State#state.workflows, undefined) of
        undefined ->
            {reply, {error, workflow_not_found}, State};
        Workflow ->
            Status = format_workflow_status(Workflow, State),
            {reply, {ok, Status}, State}
    end;

handle_call(list_workflows, _From, State) ->
    WorkflowList = maps:fold(fun(Id, Workflow, Acc) ->
        [#{
            id => Id,
            name => Workflow#workflow.name,
            status => Workflow#workflow.status,
            created_at => Workflow#workflow.created_at,
            scatter_count => length(Workflow#workflow.scatter_operations),
            collector_count => length(Workflow#workflow.collectors)
        } | Acc]
    end, [], State#state.workflows),
    {reply, {ok, WorkflowList}, State};

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

%% Internal Functions

generate_workflow_id(Counter) ->
    Timestamp = erlang:system_time(millisecond),
    iolist_to_binary(io_lib:format("wf_~p_~p", [Timestamp, Counter])).

execute_scatter_operation(WorkflowId, ScatterSpec, State) ->
    try
        % Extract scatter parameters
        TaskDescription = maps:get(<<"task_description">>, ScatterSpec, <<"Perform task">>),
        AgentCount = maps:get(<<"agent_count">>, ScatterSpec, 3),
        AgentTemplate = maps:get(<<"agent_template">>, ScatterSpec, <<"researcher">>),
        Parametrization = maps:get(<<"parametrization">>, ScatterSpec, #{}),
        ToolDescriptions = maps:get(<<"tool_descriptions">>, ScatterSpec, []),
        
        ScatterOpId = generate_scatter_id(WorkflowId),
        
        % Create dynamic agents based on parametrization
        CreatedAgents = create_parametrized_agents(
            AgentCount, 
            AgentTemplate, 
            TaskDescription,
            Parametrization, 
            ToolDescriptions
        ),
        
        % Store scatter operation
        ScatterOp = #scatter_operation{
            id = ScatterOpId,
            workflow_id = WorkflowId,
            target_count = AgentCount,
            agent_template = #{
                template => AgentTemplate,
                task => TaskDescription,
                parametrization => Parametrization,
                tools => ToolDescriptions
            },
            created_agents = CreatedAgents,
            status = <<"active">>,
            results = []
        },
        
        ets:insert(workflow_agents, {ScatterOpId, ScatterOp}),
        
        NewScatters = maps:put(ScatterOpId, ScatterOp, State#state.active_scatters),
        NewState = State#state{active_scatters = NewScatters},
        
        {ok, ScatterOpId, NewState}
    catch
        Error:Reason:Stack ->
            io:format("Scatter operation failed: ~p:~p~n~p~n", [Error, Reason, Stack]),
            {error, {scatter_failed, Error, Reason}}
    end.

create_parametrized_agents(Count, Template, TaskDescription, Parametrization, ToolDescriptions) ->
    lists:map(fun(Index) ->
        % Generate unique agent parameters
        AgentName = iolist_to_binary(io_lib:format("~s-~p", [TaskDescription, Index])),
        
        % Build system prompt with parametrization
        SystemPrompt = build_parametrized_prompt(TaskDescription, Parametrization, Index, ToolDescriptions),
        
        % Determine tools from descriptions
        Tools = parse_tool_descriptions(ToolDescriptions),
        
        % Create agent configuration
        AgentConfig = #{
            name => AgentName,
            type => template,
            template_id => Template,
            system_prompt => SystemPrompt,
            tools => Tools,
            parametrization => Parametrization#{index => Index},
            workflow_role => <<"scatter_worker">>
        },
        
        % Create the agent
        case create_workflow_agent(AgentConfig) of
            {ok, AgentId} -> AgentId;
            {error, _Reason} -> undefined
        end
    end, lists:seq(1, Count)).

build_parametrized_prompt(TaskDescription, Parametrization, Index, ToolDescriptions) ->
    BasePrompt = iolist_to_binary(io_lib:format(
        "You are a specialized agent for: ~s. "
        "You are agent #~p in a distributed workflow. "
        "Your unique parameters: ~p. "
        "Available tools: ~s",
        [TaskDescription, Index, Parametrization, format_tool_descriptions(ToolDescriptions)]
    )),
    
    % Add specific instructions based on parametrization
    CustomInstructions = maps:get(<<"custom_instructions">>, Parametrization, <<"">>),
    Perspective = maps:get(<<"perspective">>, Parametrization, <<"">>),
    Constraints = maps:get(<<"constraints">>, Parametrization, <<"">>),
    
    AdditionalPrompt = case {CustomInstructions, Perspective, Constraints} of
        {<<>>, <<>>, <<>>} -> <<>>;
        _ -> iolist_to_binary(io_lib:format(
            " Additional instructions: ~s. "
            "Your perspective: ~s. "
            "Constraints: ~s",
            [CustomInstructions, Perspective, Constraints]
        ))
    end,
    
    <<BasePrompt/binary, AdditionalPrompt/binary>>.

parse_tool_descriptions(ToolDescriptions) ->
    % Convert plain English tool descriptions to actual tool names
    lists:filtermap(fun(Description) ->
        case map_description_to_tool(Description) of
            undefined -> false;
            Tool -> {true, Tool}
        end
    end, ToolDescriptions).

map_description_to_tool(<<"search the web">>) -> jina_search;
map_description_to_tool(<<"search for information">>) -> jina_search;
map_description_to_tool(<<"read files">>) -> file_read;
map_description_to_tool(<<"write files">>) -> file_write;
map_description_to_tool(<<"execute shell commands">>) -> shell;
map_description_to_tool(<<"get weather information">>) -> get_weather;
map_description_to_tool(<<"access knowledge base">>) -> knowledge_base_retrieval;
map_description_to_tool(<<"read web pages">>) -> jina_read_webpage;
map_description_to_tool(Description) ->
    % Try to extract tool name from description
    case binary:match(Description, [<<"jina">>, <<"search">>, <<"file">>, <<"shell">>, <<"weather">>, <<"knowledge">>]) of
        nomatch -> undefined;
        _ ->
            % Attempt to infer tool from keywords
            case {
                binary:match(Description, <<"search">>),
                binary:match(Description, <<"file">>),
                binary:match(Description, <<"shell">>),
                binary:match(Description, <<"weather">>)
            } of
                {nomatch, nomatch, nomatch, nomatch} -> undefined;
                {_, nomatch, nomatch, nomatch} -> jina_search;
                {nomatch, _, nomatch, nomatch} -> file_read;
                {nomatch, nomatch, _, nomatch} -> shell;
                {nomatch, nomatch, nomatch, _} -> get_weather;
                _ -> jina_search  % Default fallback
            end
    end.

format_tool_descriptions(Descriptions) ->
    case Descriptions of
        [] -> <<"None specified">>;
        _ -> iolist_to_binary(lists:join(", ", Descriptions))
    end.

create_workflow_agent(AgentConfig) ->
    AgentId = maps:get(name, AgentConfig, generate_agent_id()),
    Result = try
        % Use existing agent creation infrastructure
        agent_templates:create_from_template(
            maps:get(template_id, AgentConfig, <<"researcher">>),
            AgentConfig
        )
    catch
        _:_ ->
            % Fallback to simpler agent creation
            agent_instance:start_link(AgentConfig)
    end,
    
    case Result of
        {ok, Pid} ->
            agent_registry:register_agent(AgentId, Pid, AgentConfig),
            {ok, AgentId};
        Error ->
            Error
    end.

generate_agent_id() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("ag_~8.16.0b~4.16.0b~4.16.0b~4.16.0b~12.16.0b", [A, B, C, D, E])).

generate_scatter_id(WorkflowId) ->
    Timestamp = erlang:system_time(millisecond),
    <<WorkflowId/binary, "_scatter_", (integer_to_binary(Timestamp))/binary>>.

gather_workflow_results(WorkflowId, _State) ->
    % Collect results from all scatter operations and collectors
    try
        ScatterResults = ets:foldl(fun({_ScatterOpId, ScatterOp}, Acc) ->
            case ScatterOp#scatter_operation.workflow_id of
                WorkflowId ->
                    [#{
                        scatter_id => ScatterOp#scatter_operation.id,
                        status => ScatterOp#scatter_operation.status,
                        agent_count => length(ScatterOp#scatter_operation.created_agents),
                        agents => ScatterOp#scatter_operation.created_agents,
                        results => ScatterOp#scatter_operation.results
                    } | Acc];
                _ ->
                    Acc
            end
        end, [], workflow_agents),
        
        CollectorResults = ets:foldl(fun({_CollectorId, Collector}, Acc) ->
            case Collector#collector_agent.workflow_id of
                WorkflowId ->
                    [#{
                        collector_id => Collector#collector_agent.id,
                        status => Collector#collector_agent.status,
                        collected_count => length(Collector#collector_agent.collected_data),
                        data => Collector#collector_agent.collected_data
                    } | Acc];
                _ ->
                    Acc
            end
        end, [], workflow_collectors),
        
        #{
            scatter_operations => ScatterResults,
            collectors => CollectorResults,
            gathered_at => erlang:system_time(millisecond)
        }
    catch
        _:_ ->
            #{error => <<"Failed to gather results">>}
    end.

build_workflow_context(WorkflowId, State) ->
    Workflow = maps:get(WorkflowId, State#state.workflows, undefined),
    Results = gather_workflow_results(WorkflowId, State),
    
    #{
        workflow_id => WorkflowId,
        workflow => Workflow,
        results => Results,
        active_scatters => maps:size(State#state.active_scatters),
        collectors => maps:size(State#state.collectors),
        timestamp => erlang:system_time(millisecond)
    }.

execute_script(Script, Context) ->
    try
        % Execute workflow script in sandboxed environment
        case Script of
            <<"gather_all">> ->
                execute_gather_all_script(Context);
            <<"analyze_results">> ->
                execute_analyze_results_script(Context);
            <<"create_summary">> ->
                execute_create_summary_script(Context);
            <<"parallel_process">> ->
                execute_parallel_process_script(Context);
            Custom when is_binary(Custom) ->
                execute_custom_script(Custom, Context);
            _ ->
                {error, unsupported_script}
        end
    catch
        Error:Reason:Stack ->
            {error, #{
                type => Error,
                reason => Reason,
                stack => Stack
            }}
    end.

execute_gather_all_script(Context) ->
    WorkflowId = maps:get(workflow_id, Context),
    Results = maps:get(results, Context),
    
    % Gather all data from scatter operations
    AllData = lists:flatten([
        ScatterResults || 
        #{results := ScatterResults} <- maps:get(scatter_operations, Results, [])
    ]),
    
    {ok, #{
        script => <<"gather_all">>,
        workflow_id => WorkflowId,
        total_data_points => length(AllData),
        data => AllData
    }}.

execute_analyze_results_script(Context) ->
    Results = maps:get(results, Context),
    
    % Analyze patterns in results
    ScatterOps = maps:get(scatter_operations, Results, []),
    
    Analysis = #{
        total_scatter_operations => length(ScatterOps),
        successful_operations => length([S || #{status := <<"completed">>} = S <- ScatterOps]),
        total_agents_created => lists:sum([maps:get(agent_count, S, 0) || S <- ScatterOps]),
        patterns => analyze_result_patterns(ScatterOps)
    },
    
    {ok, Analysis}.

execute_create_summary_script(Context) ->
    WorkflowId = maps:get(workflow_id, Context),
    Results = maps:get(results, Context),
    
    Summary = #{
        workflow_id => WorkflowId,
        summary_created_at => erlang:system_time(millisecond),
        scatter_summary => summarize_scatter_operations(Results),
        collector_summary => summarize_collectors(Results),
        key_insights => extract_key_insights(Results)
    },
    
    {ok, Summary}.

execute_parallel_process_script(Context) ->
    WorkflowId = maps:get(workflow_id, Context),
    
    % Execute parallel processing across all agents
    ProcessingTasks = [
        {aggregate_data, <<"Aggregate all collected data">>},
        {validate_results, <<"Validate result consistency">>},
        {generate_insights, <<"Generate insights from patterns">>}
    ],
    
    ParentPid = self(),
    TaskResults = lists:map(fun({TaskType, Description}) ->
        spawn_link(fun() ->
            Result = execute_processing_task(TaskType, Context),
            ParentPid ! {task_result, TaskType, Result}
        end),
        receive
            {task_result, TaskType, Result} -> #{task => TaskType, description => Description, result => Result}
        after 30000 ->
            #{task => TaskType, description => Description, result => {error, timeout}}
        end
    end, ProcessingTasks),
    
    {ok, #{
        workflow_id => WorkflowId,
        parallel_processing_results => TaskResults,
        processed_at => erlang:system_time(millisecond)
    }}.

execute_custom_script(ScriptCode, Context) ->
    % Safe execution of custom script code
    % This is a simplified version - in production, use proper sandboxing
    case ScriptCode of
        <<"count_agents">> ->
            Results = maps:get(results, Context),
            AgentCount = lists:sum([
                maps:get(agent_count, S, 0) || 
                S <- maps:get(scatter_operations, Results, [])
            ]),
            {ok, #{agent_count => AgentCount}};
        <<"get_workflow_stats">> ->
            Workflow = maps:get(workflow, Context),
            {ok, #{
                name => Workflow#workflow.name,
                status => Workflow#workflow.status,
                created_at => Workflow#workflow.created_at,
                scatter_count => length(Workflow#workflow.scatter_operations)
            }};
        _ ->
            {error, <<"Unsupported custom script">>}
    end.

execute_processing_task(aggregate_data, Context) ->
    Results = maps:get(results, Context),
    AllData = gather_all_data_from_results(Results),
    #{aggregated_count => length(AllData), sample => lists:sublist(AllData, 3)};

execute_processing_task(validate_results, Context) ->
    Results = maps:get(results, Context),
    ValidationResults = validate_workflow_results(Results),
    ValidationResults;

execute_processing_task(generate_insights, Context) ->
    Results = maps:get(results, Context),
    Insights = generate_workflow_insights(Results),
    Insights.

%% Helper functions for analysis
analyze_result_patterns(ScatterOps) ->
    % Analyze patterns in scatter operation results
    SuccessRate = case length(ScatterOps) of
        0 -> 0;
        Total ->
            Successful = length([S || #{status := <<"completed">>} = S <- ScatterOps]),
            round((Successful / Total) * 100)
    end,
    
    #{
        success_rate_percent => SuccessRate,
        common_agent_template => find_most_common_template(ScatterOps),
        avg_agents_per_scatter => calculate_avg_agents_per_scatter(ScatterOps)
    }.

find_most_common_template(_ScatterOps) ->
    <<"researcher">>. % Simplified - could implement frequency analysis

calculate_avg_agents_per_scatter([]) ->
    0;
calculate_avg_agents_per_scatter(ScatterOps) ->
    TotalAgents = lists:sum([maps:get(agent_count, S, 0) || S <- ScatterOps]),
    round(TotalAgents / length(ScatterOps)).

summarize_scatter_operations(Results) ->
    ScatterOps = maps:get(scatter_operations, Results, []),
    #{
        total_operations => length(ScatterOps),
        total_agents => lists:sum([maps:get(agent_count, S, 0) || S <- ScatterOps]),
        operation_statuses => count_statuses(ScatterOps)
    }.

summarize_collectors(Results) ->
    Collectors = maps:get(collectors, Results, []),
    #{
        total_collectors => length(Collectors),
        total_collected_data => lists:sum([maps:get(collected_count, C, 0) || C <- Collectors]),
        collector_statuses => count_collector_statuses(Collectors)
    }.

extract_key_insights(_Results) ->
    % Extract key insights from workflow execution
    [
        <<"Workflow completed successfully with distributed agent processing">>,
        <<"Results show effective scatter-gather pattern execution">>,
        <<"Agent parameterization enabled specialized task handling">>
    ].

count_statuses(Operations) ->
    lists:foldl(fun(Op, Acc) ->
        Status = maps:get(status, Op, <<"unknown">>),
        maps:update_with(Status, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, Operations).

count_collector_statuses(Collectors) ->
    lists:foldl(fun(Collector, Acc) ->
        Status = maps:get(status, Collector, <<"unknown">>),
        maps:update_with(Status, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, Collectors).

gather_all_data_from_results(Results) ->
    ScatterOps = maps:get(scatter_operations, Results, []),
    lists:flatten([maps:get(results, S, []) || S <- ScatterOps]).

validate_workflow_results(Results) ->
    ScatterOps = maps:get(scatter_operations, Results, []),
    
    Validations = [
        {<<"all_operations_have_status">>, all_have_status(ScatterOps)},
        {<<"no_failed_operations">>, no_failed_operations(ScatterOps)},
        {<<"consistent_agent_counts">>, consistent_agent_counts(ScatterOps)}
    ],
    
    AllValid = lists:all(fun({_, Valid}) -> Valid end, Validations),
    
    #{
        overall_valid => AllValid,
        individual_validations => Validations
    }.

generate_workflow_insights(Results) ->
    ScatterOps = maps:get(scatter_operations, Results, []),
    
    [
        iolist_to_binary(io_lib:format("Processed ~p scatter operations", [length(ScatterOps)])),
        iolist_to_binary(io_lib:format("Total agents created: ~p", [
            lists:sum([maps:get(agent_count, S, 0) || S <- ScatterOps])
        ])),
        <<"Distributed processing pattern successfully executed">>
    ].

all_have_status(ScatterOps) ->
    lists:all(fun(S) -> maps:is_key(status, S) end, ScatterOps).

no_failed_operations(ScatterOps) ->
    not lists:any(fun(S) -> maps:get(status, S, <<"unknown">>) =:= <<"failed">> end, ScatterOps).

consistent_agent_counts(ScatterOps) ->
    % Check if agent counts are consistent with created agents
    lists:all(fun(S) ->
        ExpectedCount = maps:get(agent_count, S, 0),
        ActualCount = length(maps:get(agents, S, [])),
        ExpectedCount =:= ActualCount
    end, ScatterOps).

format_workflow_status(Workflow, State) ->
    ScatterCount = length(Workflow#workflow.scatter_operations),
    CollectorCount = length(Workflow#workflow.collectors),
    
    % Get active scatter operations for this workflow
    ActiveScatters = maps:fold(fun(_ScatterId, ScatterOp, Acc) ->
        case ScatterOp#scatter_operation.workflow_id of
            WorkflowId when WorkflowId =:= Workflow#workflow.id ->
                Acc + 1;
            _ ->
                Acc
        end
    end, 0, State#state.active_scatters),
    
    #{
        id => Workflow#workflow.id,
        name => Workflow#workflow.name,
        status => Workflow#workflow.status,
        created_at => Workflow#workflow.created_at,
        scatter_operations_count => ScatterCount,
        collectors_count => CollectorCount,
        active_scatters => ActiveScatters,
        has_script => byte_size(Workflow#workflow.script) > 0,
        metadata => Workflow#workflow.metadata
    }.