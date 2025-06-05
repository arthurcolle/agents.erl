-module(workflow_api_handler).

-export([init/2]).

%% Workflow API Handler
%% Provides REST endpoints for the advanced workflow orchestration system
%% Supporting scatter-gather patterns, dynamic agent creation, and scripting

init(Req0 = #{method := <<"POST">>}, State) ->
    Path = cowboy_req:path(Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    case catch jsx:decode(Body, [return_maps]) of
        {'EXIT', _} ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => <<"Invalid JSON format in request body.">>,
                error_type => <<"json_decode_error">>
            }), Req1),
            {ok, Req, State};
        DecodedBody ->
            handle_workflow_operation(Path, DecodedBody, Req1, State)
    end;

init(Req0 = #{method := <<"GET">>}, State) ->
    Path = cowboy_req:path(Req0),
    handle_workflow_query(Path, Req0, State);

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => <<"Method not allowed. This endpoint supports GET and POST requests.">>,
        error_type => <<"method_not_allowed">>
    }), Req0),
    {ok, Req, State}.

%% Handle POST operations
handle_workflow_operation(<<"/api/workflows/create">>, Body, Req, State) ->
    handle_create_workflow(Body, Req, State);

handle_workflow_operation(<<"/api/workflows/scatter">>, Body, Req, State) ->
    handle_scatter_operation(Body, Req, State);

handle_workflow_operation(<<"/api/workflows/gather">>, Body, Req, State) ->
    handle_gather_operation(Body, Req, State);

handle_workflow_operation(<<"/api/workflows/script">>, Body, Req, State) ->
    handle_script_execution(Body, Req, State);

handle_workflow_operation(<<"/api/workflows/compose">>, Body, Req, State) ->
    handle_workflow_composition(Body, Req, State);

handle_workflow_operation(<<"/api/workflows/subroutine">>, Body, Req, State) ->
    handle_subroutine_execution(Body, Req, State);

handle_workflow_operation(Path, _Body, Req, State) ->
    Req1 = cowboy_req:reply(404, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => iolist_to_binary([<<"Unknown workflow endpoint: ">>, Path]),
        error_type => <<"endpoint_not_found">>,
        available_endpoints => [
            <<"/api/workflows/create">>,
            <<"/api/workflows/scatter">>,
            <<"/api/workflows/gather">>,
            <<"/api/workflows/script">>,
            <<"/api/workflows/compose">>,
            <<"/api/workflows/subroutine">>
        ]
    }), Req),
    {ok, Req1, State}.

%% Handle GET queries
handle_workflow_query(<<"/api/workflows">>, Req, State) ->
    handle_list_workflows(Req, State);

handle_workflow_query(<<"/api/workflows/", WorkflowId/binary>>, Req, State) ->
    handle_get_workflow_status(WorkflowId, Req, State);

handle_workflow_query(Path, Req, State) ->
    Req1 = cowboy_req:reply(404, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => iolist_to_binary([<<"Unknown workflow query endpoint: ">>, Path]),
        error_type => <<"endpoint_not_found">>
    }), Req),
    {ok, Req1, State}.

%% Implementation functions

handle_create_workflow(Body, Req, State) ->
    WorkflowSpec = maps:merge(#{
        <<"name">> => <<"Unnamed Workflow">>,
        <<"description">> => <<"Auto-generated workflow">>,
        <<"metadata">> => #{}
    }, Body),
    
    case workflow_orchestrator:create_workflow(WorkflowSpec) of
        {ok, WorkflowId} ->
            Response = jsx:encode(#{
                success => true,
                operation => <<"create_workflow">>,
                workflow_id => WorkflowId,
                name => maps:get(<<"name">>, WorkflowSpec),
                created_at => erlang:system_time(millisecond)
            }),
            Req1 = cowboy_req:reply(201, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req);
        {error, Reason} ->
            Req1 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => format_error(Reason),
                error_type => <<"workflow_creation_failed">>
            }), Req)
    end,
    {ok, Req1, State}.

handle_scatter_operation(Body, Req, State) ->
    WorkflowId = maps:get(<<"workflow_id">>, Body, undefined),
    
    if WorkflowId =:= undefined ->
        Req1 = cowboy_req:reply(400, #{
            <<"content-type">> => <<"application/json">>
        }, jsx:encode(#{
            success => false,
            error => <<"Missing required 'workflow_id' field.">>,
            error_type => <<"missing_required_field">>
        }), Req);
    true ->
        ScatterSpec = maps:merge(#{
            <<"agent_count">> => 3,
            <<"agent_template">> => <<"researcher">>,
            <<"task_description">> => <<"Perform analysis">>,
            <<"parametrization">> => #{},
            <<"tool_descriptions">> => []
        }, Body),
        
        case workflow_orchestrator:scatter_to_agents(WorkflowId, ScatterSpec) of
            {ok, ScatterOpId} ->
                Response = jsx:encode(#{
                    success => true,
                    operation => <<"scatter">>,
                    workflow_id => WorkflowId,
                    scatter_operation_id => ScatterOpId,
                    agent_count => maps:get(<<"agent_count">>, ScatterSpec),
                    task_description => maps:get(<<"task_description">>, ScatterSpec),
                    created_at => erlang:system_time(millisecond)
                }),
                Req1 = cowboy_req:reply(200, #{
                    <<"content-type">> => <<"application/json">>
                }, Response, Req);
            {error, Reason} ->
                Req1 = cowboy_req:reply(400, #{
                    <<"content-type">> => <<"application/json">>
                }, jsx:encode(#{
                    success => false,
                    error => format_error(Reason),
                    error_type => <<"scatter_operation_failed">>
                }), Req)
        end
    end,
    {ok, Req1, State}.

handle_gather_operation(Body, Req, State) ->
    WorkflowId = maps:get(<<"workflow_id">>, Body, undefined),
    
    if WorkflowId =:= undefined ->
        Req1 = cowboy_req:reply(400, #{
            <<"content-type">> => <<"application/json">>
        }, jsx:encode(#{
            success => false,
            error => <<"Missing required 'workflow_id' field.">>,
            error_type => <<"missing_required_field">>
        }), Req);
    true ->
        case workflow_orchestrator:gather_from_collectors(WorkflowId) of
            {ok, Results} ->
                Response = jsx:encode(#{
                    success => true,
                    operation => <<"gather">>,
                    workflow_id => WorkflowId,
                    results => Results,
                    gathered_at => erlang:system_time(millisecond)
                }),
                Req1 = cowboy_req:reply(200, #{
                    <<"content-type">> => <<"application/json">>
                }, Response, Req);
            {error, Reason} ->
                Req1 = cowboy_req:reply(400, #{
                    <<"content-type">> => <<"application/json">>
                }, jsx:encode(#{
                    success => false,
                    error => format_error(Reason),
                    error_type => <<"gather_operation_failed">>
                }), Req)
        end
    end,
    {ok, Req1, State}.

handle_script_execution(Body, Req, State) ->
    WorkflowId = maps:get(<<"workflow_id">>, Body, undefined),
    Script = maps:get(<<"script">>, Body, undefined),
    
    case {WorkflowId, Script} of
        {undefined, _} ->
            Req1 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => <<"Missing required 'workflow_id' field.">>,
                error_type => <<"missing_required_field">>
            }), Req);
        {_, undefined} ->
            Req1 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => <<"Missing required 'script' field.">>,
                error_type => <<"missing_required_field">>
            }), Req);
        {_, _} ->
            case workflow_orchestrator:execute_workflow_script(WorkflowId, Script) of
                {ok, ScriptResult} ->
                    Response = jsx:encode(#{
                        success => true,
                        operation => <<"execute_script">>,
                        workflow_id => WorkflowId,
                        script => Script,
                        result => ScriptResult,
                        executed_at => erlang:system_time(millisecond)
                    }),
                    Req1 = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, Response, Req);
                {error, Reason} ->
                    Req1 = cowboy_req:reply(400, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{
                        success => false,
                        error => format_error(Reason),
                        error_type => <<"script_execution_failed">>
                    }), Req)
            end
    end,
    {ok, Req1, State}.

handle_workflow_composition(Body, Req, State) ->
    % Advanced workflow composition with multiple stages
    WorkflowStages = maps:get(<<"stages">>, Body, []),
    Dependencies = maps:get(<<"dependencies">>, Body, #{}),
    CompositionType = maps:get(<<"composition_type">>, Body, <<"sequential">>),
    
    % Create master workflow
    MasterWorkflowSpec = #{
        <<"name">> => maps:get(<<"name">>, Body, <<"Composed Workflow">>),
        <<"metadata">> => #{
            <<"composition_type">> => CompositionType,
            <<"stage_count">> => length(WorkflowStages),
            <<"dependencies">> => Dependencies
        }
    },
    
    case workflow_orchestrator:create_workflow(MasterWorkflowSpec) of
        {ok, MasterWorkflowId} ->
            % Execute composition based on type
            CompositionResult = case CompositionType of
                <<"sequential">> ->
                    execute_sequential_composition(MasterWorkflowId, WorkflowStages);
                <<"parallel">> ->
                    execute_parallel_composition(MasterWorkflowId, WorkflowStages);
                <<"dag">> ->
                    execute_dag_composition(MasterWorkflowId, WorkflowStages, Dependencies);
                _ ->
                    {error, <<"Unsupported composition type">>}
            end,
            
            case CompositionResult of
                {ok, CompositionDetails} ->
                    Response = jsx:encode(#{
                        success => true,
                        operation => <<"workflow_composition">>,
                        master_workflow_id => MasterWorkflowId,
                        composition_type => CompositionType,
                        stages => length(WorkflowStages),
                        details => CompositionDetails,
                        created_at => erlang:system_time(millisecond)
                    }),
                    Req1 = cowboy_req:reply(201, #{
                        <<"content-type">> => <<"application/json">>
                    }, Response, Req);
                {error, Reason} ->
                    Req1 = cowboy_req:reply(400, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{
                        success => false,
                        error => format_error(Reason),
                        error_type => <<"composition_failed">>
                    }), Req)
            end;
        {error, Reason} ->
            Req1 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => format_error(Reason),
                error_type => <<"master_workflow_creation_failed">>
            }), Req)
    end,
    {ok, Req1, State}.

handle_subroutine_execution(Body, Req, State) ->
    % Execute predefined agentic subroutines
    SubroutineName = maps:get(<<"subroutine">>, Body, undefined),
    Parameters = maps:get(<<"parameters">>, Body, #{}),
    
    case SubroutineName of
        undefined ->
            Req1 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => <<"Missing required 'subroutine' field.">>,
                error_type => <<"missing_required_field">>,
                available_subroutines => [
                    <<"research_and_analyze">>,
                    <<"multi_perspective_analysis">>,
                    <<"data_processing_pipeline">>,
                    <<"consensus_building">>,
                    <<"quality_assurance_review">>
                ]
            }), Req);
        _ ->
            case execute_subroutine(SubroutineName, Parameters) of
                {ok, SubroutineResult} ->
                    Response = jsx:encode(#{
                        success => true,
                        operation => <<"execute_subroutine">>,
                        subroutine => SubroutineName,
                        parameters => Parameters,
                        result => SubroutineResult,
                        executed_at => erlang:system_time(millisecond)
                    }),
                    Req1 = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, Response, Req);
                {error, Reason} ->
                    Req1 = cowboy_req:reply(400, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{
                        success => false,
                        error => format_error(Reason),
                        error_type => <<"subroutine_execution_failed">>
                    }), Req)
            end
    end,
    {ok, Req1, State}.

handle_list_workflows(Req, State) ->
    case workflow_orchestrator:list_workflows() of
        {ok, Workflows} ->
            Response = jsx:encode(#{
                success => true,
                workflows => Workflows,
                total_count => length(Workflows),
                retrieved_at => erlang:system_time(millisecond)
            }),
            Req1 = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req);
        {error, Reason} ->
            Req1 = cowboy_req:reply(500, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => format_error(Reason),
                error_type => <<"list_workflows_failed">>
            }), Req)
    end,
    {ok, Req1, State}.

handle_get_workflow_status(WorkflowId, Req, State) ->
    case workflow_orchestrator:get_workflow_status(WorkflowId) of
        {ok, Status} ->
            Response = jsx:encode(#{
                success => true,
                workflow_id => WorkflowId,
                status => Status,
                retrieved_at => erlang:system_time(millisecond)
            }),
            Req1 = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req);
        {error, workflow_not_found} ->
            Req1 = cowboy_req:reply(404, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => <<"Workflow not found.">>,
                error_type => <<"workflow_not_found">>,
                workflow_id => WorkflowId
            }), Req);
        {error, Reason} ->
            Req1 = cowboy_req:reply(500, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => format_error(Reason),
                error_type => <<"get_workflow_status_failed">>
            }), Req)
    end,
    {ok, Req1, State}.

%% Composition execution functions

execute_sequential_composition(MasterWorkflowId, WorkflowStages) ->
    % Execute stages one after another
    Results = lists:foldl(fun(Stage, Acc) ->
        StageResult = execute_workflow_stage(MasterWorkflowId, Stage, Acc),
        [StageResult | Acc]
    end, [], WorkflowStages),
    
    {ok, #{
        type => <<"sequential">>,
        stages_executed => length(WorkflowStages),
        results => lists:reverse(Results)
    }}.

execute_parallel_composition(MasterWorkflowId, WorkflowStages) ->
    % Execute all stages in parallel
    ParentPid = self(),
    Tasks = lists:map(fun(Stage) ->
        spawn_link(fun() ->
            Result = execute_workflow_stage(MasterWorkflowId, Stage, []),
            ParentPid ! {stage_result, Stage, Result}
        end)
    end, WorkflowStages),
    
    Results = collect_stage_results(Tasks, WorkflowStages, []),
    
    {ok, #{
        type => <<"parallel">>,
        stages_executed => length(WorkflowStages),
        results => Results
    }}.

execute_dag_composition(MasterWorkflowId, WorkflowStages, Dependencies) ->
    % Execute stages based on dependency DAG
    ExecutionOrder = topological_sort(WorkflowStages, Dependencies),
    
    Results = lists:foldl(fun(Stage, Acc) ->
        % Wait for dependencies to complete
        DependencyResults = wait_for_dependencies(Stage, Dependencies, Acc),
        StageResult = execute_workflow_stage(MasterWorkflowId, Stage, DependencyResults),
        [StageResult | Acc]
    end, [], ExecutionOrder),
    
    {ok, #{
        type => <<"dag">>,
        execution_order => ExecutionOrder,
        stages_executed => length(WorkflowStages),
        results => lists:reverse(Results)
    }}.

execute_workflow_stage(MasterWorkflowId, Stage, PreviousResults) ->
    StageType = maps:get(<<"type">>, Stage, <<"scatter">>),
    StageName = maps:get(<<"name">>, Stage, <<"Unnamed Stage">>),
    
    case StageType of
        <<"scatter">> ->
            ScatterSpec = maps:get(<<"scatter_spec">>, Stage, #{}),
            case workflow_orchestrator:scatter_to_agents(MasterWorkflowId, ScatterSpec) of
                {ok, ScatterOpId} ->
                    #{stage => StageName, type => <<"scatter">>, scatter_op_id => ScatterOpId, status => <<"completed">>};
                {error, Reason} ->
                    #{stage => StageName, type => <<"scatter">>, status => <<"failed">>, error => Reason}
            end;
        <<"gather">> ->
            case workflow_orchestrator:gather_from_collectors(MasterWorkflowId) of
                {ok, Results} ->
                    #{stage => StageName, type => <<"gather">>, results => Results, status => <<"completed">>};
                {error, Reason} ->
                    #{stage => StageName, type => <<"gather">>, status => <<"failed">>, error => Reason}
            end;
        <<"script">> ->
            Script = maps:get(<<"script">>, Stage, <<"analyze_results">>),
            case workflow_orchestrator:execute_workflow_script(MasterWorkflowId, Script) of
                {ok, ScriptResult} ->
                    #{stage => StageName, type => <<"script">>, script => Script, result => ScriptResult, status => <<"completed">>};
                {error, Reason} ->
                    #{stage => StageName, type => <<"script">>, status => <<"failed">>, error => Reason}
            end;
        _ ->
            #{stage => StageName, type => StageType, status => <<"unsupported">>, error => <<"Unknown stage type">>}
    end.

collect_stage_results([], [], Results) ->
    lists:reverse(Results);
collect_stage_results([_Task | RestTasks], [Stage | RestStages], Results) ->
    receive
        {stage_result, Stage, Result} ->
            collect_stage_results(RestTasks, RestStages, [Result | Results])
    after 60000 ->
        TimeoutResult = #{stage => maps:get(<<"name">>, Stage, <<"Unknown">>), status => <<"timeout">>},
        collect_stage_results(RestTasks, RestStages, [TimeoutResult | Results])
    end.

topological_sort(Stages, _Dependencies) ->
    % Simplified topological sort - in production, implement proper DAG sorting
    Stages.

wait_for_dependencies(_Stage, _Dependencies, PreviousResults) ->
    % Simplified dependency resolution - filter previous results
    PreviousResults.

%% Subroutine execution functions

execute_subroutine(<<"research_and_analyze">>, Parameters) ->
    Topic = maps:get(<<"topic">>, Parameters, <<"general research">>),
    AgentCount = maps:get(<<"agent_count">>, Parameters, 3),
    
    % Create workflow for research and analysis
    WorkflowSpec = #{
        <<"name">> => iolist_to_binary([<<"Research and Analysis: ">>, Topic]),
        <<"metadata">> => #{<<"subroutine">> => <<"research_and_analyze">>}
    },
    
    case workflow_orchestrator:create_workflow(WorkflowSpec) of
        {ok, WorkflowId} ->
            % Scatter research tasks
            ScatterSpec = #{
                <<"agent_count">> => AgentCount,
                <<"agent_template">> => <<"researcher">>,
                <<"task_description">> => iolist_to_binary([<<"Research topic: ">>, Topic]),
                <<"parametrization">> => #{
                    <<"perspective">> => <<"different viewpoints">>,
                    <<"depth">> => <<"comprehensive">>
                },
                <<"tool_descriptions">> => [<<"search the web">>, <<"access knowledge base">>]
            },
            
            case workflow_orchestrator:scatter_to_agents(WorkflowId, ScatterSpec) of
                {ok, ScatterOpId} ->
                    % Execute analysis script
                    timer:sleep(5000), % Allow time for agents to work
                    case workflow_orchestrator:execute_workflow_script(WorkflowId, <<"analyze_results">>) of
                        {ok, AnalysisResult} ->
                            {ok, #{
                                workflow_id => WorkflowId,
                                scatter_operation_id => ScatterOpId,
                                topic => Topic,
                                agent_count => AgentCount,
                                analysis => AnalysisResult
                            }};
                        {error, Reason} ->
                            {error, {analysis_failed, Reason}}
                    end;
                {error, Reason} ->
                    {error, {scatter_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {workflow_creation_failed, Reason}}
    end;

execute_subroutine(<<"multi_perspective_analysis">>, Parameters) ->
    Topic = maps:get(<<"topic">>, Parameters, <<"analysis topic">>),
    Perspectives = maps:get(<<"perspectives">>, Parameters, [<<"analytical">>, <<"creative">>, <<"critical">>]),
    
    % Create agents with different perspectives
    WorkflowSpec = #{
        <<"name">> => iolist_to_binary([<<"Multi-Perspective Analysis: ">>, Topic]),
        <<"metadata">> => #{<<"subroutine">> => <<"multi_perspective_analysis">>}
    },
    
    case workflow_orchestrator:create_workflow(WorkflowSpec) of
        {ok, WorkflowId} ->
            Results = lists:map(fun(Perspective) ->
                ScatterSpec = #{
                    <<"agent_count">> => 1,
                    <<"agent_template">> => <<"analyst">>,
                    <<"task_description">> => iolist_to_binary([<<"Analyze from ">>, Perspective, <<" perspective: ">>, Topic]),
                    <<"parametrization">> => #{<<"perspective">> => Perspective},
                    <<"tool_descriptions">> => [<<"search the web">>, <<"analyze data">>]
                },
                workflow_orchestrator:scatter_to_agents(WorkflowId, ScatterSpec)
            end, Perspectives),
            
            SuccessfulResults = [R || {ok, _} = R <- Results],
            
            {ok, #{
                workflow_id => WorkflowId,
                topic => Topic,
                perspectives => Perspectives,
                successful_perspectives => length(SuccessfulResults),
                total_perspectives => length(Perspectives)
            }};
        {error, Reason} ->
            {error, {workflow_creation_failed, Reason}}
    end;

execute_subroutine(SubroutineName, _Parameters) ->
    {error, iolist_to_binary([<<"Unsupported subroutine: ">>, SubroutineName])}.

%% Helper functions

format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error({Type, Details}) when is_atom(Type) ->
    iolist_to_binary([atom_to_binary(Type, utf8), <<": ">>, format_error(Details)]);
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).