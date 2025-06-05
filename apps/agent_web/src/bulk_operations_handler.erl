-module(bulk_operations_handler).

-export([init/2]).

%% Bulk operations handler for mass agent management and operations
%% Supports:
%% - Bulk messaging to all agents
%% - System prompt modifications (prefix/suffix)
%% - Callback/lambda operations on agents and data
%% - Batch agent management

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
            handle_bulk_operation(Path, DecodedBody, Req1, State)
    end;

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => <<"Method not allowed. This endpoint only supports POST requests.">>,
        error_type => <<"method_not_allowed">>
    }), Req0),
    {ok, Req, State}.

%% Handle different bulk operations based on path
handle_bulk_operation(<<"/api/bulk/broadcast">>, Body, Req, State) ->
    handle_broadcast(Body, Req, State);

handle_bulk_operation(<<"/api/bulk/system-prompt">>, Body, Req, State) ->
    handle_system_prompt_modification(Body, Req, State);

handle_bulk_operation(<<"/api/bulk/callback">>, Body, Req, State) ->
    handle_callback_operation(Body, Req, State);

handle_bulk_operation(<<"/api/bulk/agent-management">>, Body, Req, State) ->
    handle_batch_agent_management(Body, Req, State);

handle_bulk_operation(<<"/api/bulk/query">>, Body, Req, State) ->
    handle_bulk_query(Body, Req, State);

handle_bulk_operation(<<"/api/bulk/transform">>, Body, Req, State) ->
    handle_data_transformation(Body, Req, State);

handle_bulk_operation(Path, _Body, Req, State) ->
    Req1 = cowboy_req:reply(404, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => iolist_to_binary([<<"Unknown bulk operation endpoint: ">>, Path]),
        error_type => <<"endpoint_not_found">>,
        available_endpoints => [
            <<"/api/bulk/broadcast">>,
            <<"/api/bulk/system-prompt">>,
            <<"/api/bulk/callback">>,
            <<"/api/bulk/agent-management">>,
            <<"/api/bulk/query">>,
            <<"/api/bulk/transform">>
        ]
    }), Req),
    {ok, Req1, State}.

%% Broadcast message to all or filtered agents
handle_broadcast(#{<<"message">> := Message} = Body, Req, State) ->
    Filters = maps:get(<<"filters">>, Body, #{}),
    Options = maps:get(<<"options">>, Body, #{}),
    
    % Get all agents and apply filters
    AllAgents = agent_registry:list_agents(),
    FilteredAgents = apply_agent_filters(AllAgents, Filters),
    
    % Determine execution mode
    ExecutionMode = maps:get(<<"execution_mode">>, Options, <<"parallel">>),
    Timeout = maps:get(<<"timeout">>, Options, 30000),
    
    case ExecutionMode of
        <<"parallel">> ->
            Results = execute_broadcast_parallel(FilteredAgents, Message, Timeout);
        <<"sequential">> ->
            Results = execute_broadcast_sequential(FilteredAgents, Message, Timeout);
        <<"fire_and_forget">> ->
            spawn(fun() -> execute_broadcast_parallel(FilteredAgents, Message, Timeout) end),
            Results = #{mode => fire_and_forget, agent_count => length(FilteredAgents)}
    end,
    
    Response = jsx:encode(#{
        success => true,
        operation => <<"broadcast">>,
        message => Message,
        agents_targeted => length(FilteredAgents),
        execution_mode => ExecutionMode,
        results => Results
    }),
    
    Req1 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req),
    {ok, Req1, State};

handle_broadcast(_Body, Req, State) ->
    Req1 = cowboy_req:reply(400, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => <<"Missing required 'message' field for broadcast operation.">>,
        error_type => <<"missing_required_field">>,
        required_fields => [<<"message">>],
        optional_fields => [<<"filters">>, <<"options">>]
    }), Req),
    {ok, Req1, State}.

%% System prompt modification (prefix/suffix operations)
handle_system_prompt_modification(#{<<"operation">> := Operation} = Body, Req, State) ->
    Filters = maps:get(<<"filters">>, Body, #{}),
    Text = maps:get(<<"text">>, Body, <<"">>),
    
    % Get filtered agents
    AllAgents = agent_registry:list_agents(),
    FilteredAgents = apply_agent_filters(AllAgents, Filters),
    
    % Apply system prompt modification
    Results = case Operation of
        <<"add_prefix">> ->
            modify_system_prompts(FilteredAgents, prefix, Text);
        <<"add_suffix">> ->
            modify_system_prompts(FilteredAgents, suffix, Text);
        <<"replace">> ->
            modify_system_prompts(FilteredAgents, replace, Text);
        <<"reset">> ->
            reset_system_prompts(FilteredAgents);
        _ ->
            #{error => <<"Invalid operation. Use: add_prefix, add_suffix, replace, or reset">>}
    end,
    
    Response = jsx:encode(#{
        success => true,
        operation => <<"system_prompt_modification">>,
        modification_type => Operation,
        text => Text,
        agents_modified => length(FilteredAgents),
        results => Results
    }),
    
    Req1 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req),
    {ok, Req1, State};

handle_system_prompt_modification(_Body, Req, State) ->
    Req1 = cowboy_req:reply(400, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => <<"Missing required 'operation' field.">>,
        error_type => <<"missing_required_field">>,
        valid_operations => [<<"add_prefix">>, <<"add_suffix">>, <<"replace">>, <<"reset">>]
    }), Req),
    {ok, Req1, State}.

%% Callback/lambda operations on agents and data
handle_callback_operation(#{<<"callback_type">> := CallbackType} = Body, Req, State) ->
    Filters = maps:get(<<"filters">>, Body, #{}),
    Parameters = maps:get(<<"parameters">>, Body, #{}),
    Code = maps:get(<<"code">>, Body, <<"">>),
    
    % Get filtered agents or data
    AllAgents = agent_registry:list_agents(),
    FilteredAgents = apply_agent_filters(AllAgents, Filters),
    
    % Execute callback operation
    Results = case CallbackType of
        <<"map">> ->
            execute_map_operation(FilteredAgents, Code, Parameters);
        <<"filter">> ->
            execute_filter_operation(FilteredAgents, Code, Parameters);
        <<"reduce">> ->
            execute_reduce_operation(FilteredAgents, Code, Parameters);
        <<"foreach">> ->
            execute_foreach_operation(FilteredAgents, Code, Parameters);
        <<"custom">> ->
            execute_custom_callback(FilteredAgents, Code, Parameters);
        _ ->
            #{error => <<"Invalid callback type">>}
    end,
    
    Response = jsx:encode(#{
        success => true,
        operation => <<"callback">>,
        callback_type => CallbackType,
        agents_processed => length(FilteredAgents),
        results => Results
    }),
    
    Req1 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req),
    {ok, Req1, State};

handle_callback_operation(_Body, Req, State) ->
    Req1 = cowboy_req:reply(400, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => <<"Missing required 'callback_type' field.">>,
        error_type => <<"missing_required_field">>,
        valid_callback_types => [<<"map">>, <<"filter">>, <<"reduce">>, <<"foreach">>, <<"custom">>]
    }), Req),
    {ok, Req1, State}.

%% Batch agent management operations
handle_batch_agent_management(#{<<"operation">> := Operation} = Body, Req, State) ->
    Filters = maps:get(<<"filters">>, Body, #{}),
    Parameters = maps:get(<<"parameters">>, Body, #{}),
    
    % Get filtered agents
    AllAgents = agent_registry:list_agents(),
    FilteredAgents = apply_agent_filters(AllAgents, Filters),
    
    % Execute management operation
    Results = case Operation of
        <<"stop">> ->
            stop_agents(FilteredAgents);
        <<"restart">> ->
            restart_agents(FilteredAgents);
        <<"update_config">> ->
            update_agent_configs(FilteredAgents, Parameters);
        <<"get_status">> ->
            get_agent_statuses(FilteredAgents);
        <<"get_metrics">> ->
            get_agent_metrics(FilteredAgents);
        <<"clear_history">> ->
            clear_agent_histories(FilteredAgents);
        _ ->
            #{error => <<"Invalid management operation">>}
    end,
    
    Response = jsx:encode(#{
        success => true,
        operation => <<"agent_management">>,
        management_operation => Operation,
        agents_affected => length(FilteredAgents),
        results => Results
    }),
    
    Req1 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req),
    {ok, Req1, State};

handle_batch_agent_management(_Body, Req, State) ->
    Req1 = cowboy_req:reply(400, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => <<"Missing required 'operation' field.">>,
        error_type => <<"missing_required_field">>,
        valid_operations => [<<"stop">>, <<"restart">>, <<"update_config">>, 
                           <<"get_status">>, <<"get_metrics">>, <<"clear_history">>]
    }), Req),
    {ok, Req1, State}.

%% Bulk query operations for getting agent data
handle_bulk_query(#{<<"query_type">> := QueryType} = Body, Req, State) ->
    Filters = maps:get(<<"filters">>, Body, #{}),
    Parameters = maps:get(<<"parameters">>, Body, #{}),
    
    AllAgents = agent_registry:list_agents(),
    FilteredAgents = apply_agent_filters(AllAgents, Filters),
    
    Results = case QueryType of
        <<"agent_info">> ->
            get_bulk_agent_info(FilteredAgents, Parameters);
        <<"conversation_history">> ->
            get_bulk_conversation_history(FilteredAgents, Parameters);
        <<"performance_metrics">> ->
            get_bulk_performance_metrics(FilteredAgents, Parameters);
        <<"capabilities">> ->
            get_bulk_capabilities(FilteredAgents);
        <<"health_check">> ->
            get_bulk_health_check(FilteredAgents);
        _ ->
            #{error => <<"Invalid query type">>}
    end,
    
    Response = jsx:encode(#{
        success => true,
        operation => <<"bulk_query">>,
        query_type => QueryType,
        agents_queried => length(FilteredAgents),
        results => Results
    }),
    
    Req1 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req),
    {ok, Req1, State};

handle_bulk_query(_Body, Req, State) ->
    Req1 = cowboy_req:reply(400, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => <<"Missing required 'query_type' field.">>,
        error_type => <<"missing_required_field">>,
        valid_query_types => [<<"agent_info">>, <<"conversation_history">>, 
                             <<"performance_metrics">>, <<"capabilities">>, <<"health_check">>]
    }), Req),
    {ok, Req1, State}.

%% Data transformation operations
handle_data_transformation(#{<<"transform_type">> := TransformType} = Body, Req, State) ->
    Data = maps:get(<<"data">>, Body, []),
    Parameters = maps:get(<<"parameters">>, Body, #{}),
    
    Results = case TransformType of
        <<"aggregate">> ->
            aggregate_data(Data, Parameters);
        <<"normalize">> ->
            normalize_data(Data, Parameters);
        <<"analyze">> ->
            analyze_data(Data, Parameters);
        <<"format">> ->
            format_data(Data, Parameters);
        _ ->
            #{error => <<"Invalid transform type">>}
    end,
    
    Response = jsx:encode(#{
        success => true,
        operation => <<"data_transformation">>,
        transform_type => TransformType,
        input_size => length(Data),
        results => Results
    }),
    
    Req1 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req),
    {ok, Req1, State};

handle_data_transformation(_Body, Req, State) ->
    Req1 = cowboy_req:reply(400, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => <<"Missing required 'transform_type' field.">>,
        error_type => <<"missing_required_field">>,
        valid_transform_types => [<<"aggregate">>, <<"normalize">>, <<"analyze">>, <<"format">>]
    }), Req),
    {ok, Req1, State}.

%% Internal helper functions

%% Apply filters to agent list
apply_agent_filters(Agents, Filters) ->
    lists:filter(fun({_Id, _Pid, Meta}) ->
        check_agent_filters(Meta, Filters)
    end, Agents).

check_agent_filters(_Meta, Filters) when map_size(Filters) =:= 0 ->
    true;
check_agent_filters(Meta, Filters) ->
    maps:fold(fun(FilterKey, FilterValue, Acc) ->
        case FilterKey of
            <<"type">> ->
                Acc andalso (maps:get(type, Meta, undefined) =:= FilterValue);
            <<"model">> ->
                Acc andalso (maps:get(model, Meta, undefined) =:= FilterValue);
            <<"template_id">> ->
                Acc andalso (maps:get(template_id, Meta, undefined) =:= FilterValue);
            <<"status">> ->
                Acc andalso (maps:get(status, Meta, undefined) =:= FilterValue);
            <<"name_contains">> ->
                Name = maps:get(name, Meta, <<"">>),
                Acc andalso (binary:match(Name, FilterValue) =/= nomatch);
            _ ->
                Acc
        end
    end, true, Filters).

%% Execute broadcast operations
execute_broadcast_parallel(Agents, Message, Timeout) ->
    ParentPid = self(),
    Tasks = lists:map(fun({Id, Pid, _Meta}) ->
        spawn_link(fun() ->
            Result = safe_agent_chat(Pid, Message, Timeout),
            ParentPid ! {broadcast_result, Id, Result}
        end)
    end, Agents),
    
    collect_broadcast_results(Tasks, Agents, []).

execute_broadcast_sequential(Agents, Message, Timeout) ->
    lists:map(fun({Id, Pid, _Meta}) ->
        Result = safe_agent_chat(Pid, Message, Timeout),
        #{agent_id => Id, result => Result}
    end, Agents).

collect_broadcast_results([], _Agents, Results) ->
    lists:reverse(Results);
collect_broadcast_results([_Task | Remaining], [{Id, _, _} | RestAgents], Results) ->
    receive
        {broadcast_result, Id, Result} ->
            NewResult = #{agent_id => Id, result => Result},
            collect_broadcast_results(Remaining, RestAgents, [NewResult | Results])
    after 35000 ->
        TimeoutResult = #{agent_id => Id, result => {error, timeout}},
        collect_broadcast_results(Remaining, RestAgents, [TimeoutResult | Results])
    end.

safe_agent_chat(Pid, Message, _Timeout) ->
    try
        case catch agent_instance:execute(Pid, #{action => <<"chat">>, message => Message}) of
            {ok, Response} -> {ok, Response};
            {error, Reason} -> {error, Reason};
            {'EXIT', Reason} -> {error, {agent_crashed, Reason}};
            Other -> {error, {unexpected_response, Other}}
        end
    catch
        Error:Reason6:_Stack ->
            {error, {Error, Reason6}}
    end.

%% System prompt modification functions
modify_system_prompts(Agents, Operation, Text) ->
    lists:map(fun({Id, Pid, _Meta}) ->
        Result = modify_agent_system_prompt(Pid, Operation, Text),
        #{agent_id => Id, result => Result}
    end, Agents).

modify_agent_system_prompt(Pid, Operation, Text) ->
    try
        % Get current state
        case agent_instance:get_state(Pid) of
            StateMap when is_map(StateMap) ->
                CurrentPrompt = maps:get(system_prompt, StateMap, <<"">>),
                NewPrompt = case Operation of
                    prefix -> <<Text/binary, " ", CurrentPrompt/binary>>;
                    suffix -> <<CurrentPrompt/binary, " ", Text/binary>>;
                    replace -> Text
                end,
                
                % Update the agent's system prompt
                case agent_instance:update_config(Pid, #{system_prompt => NewPrompt}) of
                    ok -> {ok, #{old_prompt => CurrentPrompt, new_prompt => NewPrompt}};
                    ErrorResult -> ErrorResult
                end;
            ErrorResult ->
                ErrorResult
        end
    catch
        Error3:Reason3 ->
            {error, {Error3, Reason3}}
    end.

reset_system_prompts(Agents) ->
    lists:map(fun({Id, Pid, Meta}) ->
        TemplateId = maps:get(template_id, Meta, undefined),
        Result = case TemplateId of
            undefined ->
                {error, no_template_to_reset_to};
            _ ->
                case agent_templates:get_template(TemplateId) of
                    {ok, Template} ->
                        OriginalPrompt = maps:get(system_prompt, Template, <<"">>),
                        modify_agent_system_prompt(Pid, replace, OriginalPrompt);
                    Error ->
                        Error
                end
        end,
        #{agent_id => Id, result => Result}
    end, Agents).

%% Callback operation functions
execute_map_operation(Agents, Code, Parameters) ->
    lists:map(fun({Id, Pid, Meta}) ->
        Result = execute_agent_callback(Id, Pid, Meta, Code, Parameters),
        #{agent_id => Id, result => Result}
    end, Agents).

execute_filter_operation(Agents, Code, Parameters) ->
    FilteredAgents = lists:filter(fun({Id, Pid, Meta}) ->
        case execute_agent_callback(Id, Pid, Meta, Code, Parameters) of
            {ok, true} -> true;
            {ok, <<"true">>} -> true;
            _ -> false
        end
    end, Agents),
    #{filtered_agents => [{Id, Meta} || {Id, _Pid, Meta} <- FilteredAgents]}.

execute_reduce_operation(Agents, Code, Parameters) ->
    InitialValue = maps:get(<<"initial_value">>, Parameters, 0),
    lists:foldl(fun({Id, Pid, Meta}, Acc) ->
        case execute_agent_callback(Id, Pid, Meta, Code, Parameters#{<<"accumulator">> => Acc}) of
            {ok, NewAcc} -> NewAcc;
            _ -> Acc
        end
    end, InitialValue, Agents).

execute_foreach_operation(Agents, Code, Parameters) ->
    lists:foreach(fun({Id, Pid, Meta}) ->
        execute_agent_callback(Id, Pid, Meta, Code, Parameters)
    end, Agents),
    #{operation => completed, agents_processed => length(Agents)}.

execute_custom_callback(Agents, Code, _Parameters) ->
    % Execute custom Erlang code safely
    try
        case Code of
            <<"get_memory_usage">> ->
                lists:map(fun({Id, Pid, _Meta}) ->
                    Memory = case process_info(Pid, memory) of
                        {memory, Bytes} -> Bytes;
                        _ -> 0
                    end,
                    #{agent_id => Id, memory_bytes => Memory}
                end, Agents);
            <<"get_message_queue_length">> ->
                lists:map(fun({Id, Pid, _Meta}) ->
                    QueueLen = case process_info(Pid, message_queue_len) of
                        {message_queue_len, Len} -> Len;
                        _ -> 0
                    end,
                    #{agent_id => Id, queue_length => QueueLen}
                end, Agents);
            _ ->
                #{error => <<"Unsupported custom callback code">>, available_codes => [
                    <<"get_memory_usage">>, <<"get_message_queue_length">>
                ]}
        end
    catch
        Error2:Reason2 ->
            #{error => #{type => Error2, reason => Reason2}}
    end.

execute_agent_callback(Id, Pid, Meta, Code, Parameters) ->
    % Simple callback execution - can be extended for more complex operations
    try
        case Code of
            <<"return_agent_id">> ->
                {ok, Id};
            <<"return_agent_type">> ->
                {ok, maps:get(type, Meta, undefined)};
            <<"check_memory_usage">> ->
                Threshold = maps:get(<<"threshold">>, Parameters, 1000000),
                case process_info(Pid, memory) of
                    {memory, Bytes} -> {ok, Bytes > Threshold};
                    _ -> {ok, false}
                end;
            _ ->
                {error, unsupported_callback}
        end
    catch
        Error4:Reason4 ->
            {error, {Error4, Reason4}}
    end.

%% Agent management functions
stop_agents(Agents) ->
    lists:map(fun({Id, Pid, _Meta}) ->
        Result = try
            case catch agent_instance:terminate(normal, #{}) of
                ok -> {ok, stopped};
                _ -> 
                    exit(Pid, normal),
                    {ok, force_stopped}
            end
        catch
            _:_ -> {ok, already_stopped}
        end,
        #{agent_id => Id, result => Result}
    end, Agents).

restart_agents(Agents) ->
    lists:map(fun({Id, _Pid, Meta}) ->
        % Stop and restart agent
        agent_registry:unregister_agent(Id),
        case agent_templates:create_from_template(
            maps:get(template_id, Meta, <<"simple">>), 
            #{id => Id}
        ) of
            {ok, NewPid} ->
                agent_registry:register_agent(Id, NewPid, Meta),
                {ok, restarted};
            Error ->
                Error
        end
    end, Agents).

update_agent_configs(Agents, Parameters) ->
    lists:map(fun({Id, Pid, _Meta}) ->
        Result = agent_instance:update_config(Pid, Parameters),
        #{agent_id => Id, result => Result}
    end, Agents).

get_agent_statuses(Agents) ->
    lists:map(fun({Id, Pid, Meta}) ->
        Status = case is_process_alive(Pid) of
            true -> <<"running">>;
            false -> <<"stopped">>
        end,
        #{
            agent_id => Id,
            status => Status,
            meta => Meta
        }
    end, Agents).

get_agent_metrics(Agents) ->
    lists:map(fun({Id, Pid, _Meta}) ->
        Metrics = try
            case agent_instance:get_state(Pid) of
                StateMap when is_map(StateMap) ->
                    maps:get(metrics, StateMap, #{});
                _ ->
                    #{}
            end
        catch
            _:_ -> #{error => <<"Failed to get metrics">>}
        end,
        #{agent_id => Id, metrics => Metrics}
    end, Agents).

clear_agent_histories(Agents) ->
    lists:map(fun({Id, Pid, _Meta}) ->
        Result = try
            agent_instance:update_config(Pid, #{conversation_history => []}),
            {ok, cleared}
        catch
            Error5:Reason5 -> {error, {Error5, Reason5}}
        end,
        #{agent_id => Id, result => Result}
    end, Agents).

%% Query functions
get_bulk_agent_info(Agents, _Parameters) ->
    lists:map(fun({Id, Pid, Meta}) ->
        State = try
            agent_instance:get_state(Pid)
        catch
            _:_ -> #{error => <<"Cannot get state">>}
        end,
        #{
            agent_id => Id,
            meta => Meta,
            state => State
        }
    end, Agents).

get_bulk_conversation_history(Agents, Parameters) ->
    Limit = maps:get(<<"limit">>, Parameters, 10),
    lists:map(fun({Id, Pid, _Meta}) ->
        History = try
            case agent_instance:get_state(Pid) of
                StateMap when is_map(StateMap) ->
                    AllHistory = maps:get(conversation_history, StateMap, []),
                    lists:sublist(AllHistory, Limit);
                _ ->
                    []
            end
        catch
            _:_ -> []
        end,
        #{agent_id => Id, conversation_history => History}
    end, Agents).

get_bulk_performance_metrics(Agents, _Parameters) ->
    lists:map(fun({Id, Pid, _Meta}) ->
        Memory = case process_info(Pid, memory) of
            {memory, Bytes} -> Bytes;
            _ -> 0
        end,
        QueueLen = case process_info(Pid, message_queue_len) of
            {message_queue_len, Len} -> Len;
            _ -> 0
        end,
        #{
            agent_id => Id,
            memory_bytes => Memory,
            message_queue_length => QueueLen,
            is_alive => is_process_alive(Pid)
        }
    end, Agents).

get_bulk_capabilities(Agents) ->
    lists:map(fun({Id, _Pid, Meta}) ->
        Capabilities = maps:get(capabilities, Meta, []),
        Tools = maps:get(tools, Meta, []),
        #{
            agent_id => Id,
            capabilities => Capabilities,
            tools => Tools,
            type => maps:get(type, Meta, undefined),
            template_id => maps:get(template_id, Meta, undefined)
        }
    end, Agents).

get_bulk_health_check(Agents) ->
    lists:map(fun({Id, Pid, Meta}) ->
        Health = #{
            is_alive => is_process_alive(Pid),
            memory_ok => case process_info(Pid, memory) of
                {memory, Bytes} -> Bytes < 100000000; % 100MB threshold
                _ -> false
            end,
            queue_ok => case process_info(Pid, message_queue_len) of
                {message_queue_len, Len} -> Len < 100;
                _ -> false
            end,
            last_activity => maps:get(last_activity, Meta, 0)
        },
        OverallHealth = maps:get(is_alive, Health) andalso 
                       maps:get(memory_ok, Health) andalso 
                       maps:get(queue_ok, Health),
        #{
            agent_id => Id,
            health_status => case OverallHealth of
                true -> <<"healthy">>;
                false -> <<"unhealthy">>
            end,
            health_details => Health
        }
    end, Agents).

%% Data transformation functions
aggregate_data(Data, Parameters) ->
    AggType = maps:get(<<"aggregation_type">>, Parameters, <<"count">>),
    case AggType of
        <<"count">> ->
            #{count => length(Data)};
        <<"sum">> ->
            Field = maps:get(<<"field">>, Parameters, <<"value">>),
            Sum = lists:sum([maps:get(Field, Item, 0) || Item <- Data]),
            #{sum => Sum};
        <<"average">> ->
            Field = maps:get(<<"field">>, Parameters, <<"value">>),
            Values = [maps:get(Field, Item, 0) || Item <- Data],
            Avg = case length(Values) of
                0 -> 0;
                N -> lists:sum(Values) / N
            end,
            #{average => Avg};
        _ ->
            #{error => <<"Unsupported aggregation type">>}
    end.

normalize_data(Data, _Parameters) ->
    % Simple normalization example
    lists:map(fun(Item) when is_map(Item) ->
        maps:map(fun(_K, V) when is_number(V) ->
            V / 100.0;  % Example normalization
        (_, V) -> V
        end, Item);
    (Item) -> Item
    end, Data).

analyze_data(Data, Parameters) ->
    AnalysisType = maps:get(<<"analysis_type">>, Parameters, <<"summary">>),
    case AnalysisType of
        <<"summary">> ->
            #{
                total_items => length(Data),
                item_types => get_data_types(Data),
                sample => lists:sublist(Data, 3)
            };
        _ ->
            #{error => <<"Unsupported analysis type">>}
    end.

format_data(Data, Parameters) ->
    Format = maps:get(<<"format">>, Parameters, <<"json">>),
    case Format of
        <<"json">> ->
            jsx:encode(Data);
        <<"csv">> ->
            format_as_csv(Data);
        _ ->
            #{error => <<"Unsupported format">>}
    end.

get_data_types(Data) ->
    TypeCounts = lists:foldl(fun(Item, Acc) ->
        Type = case Item of
            Item when is_map(Item) -> map;
            Item when is_list(Item) -> list;
            Item when is_binary(Item) -> binary;
            Item when is_number(Item) -> number;
            _ -> other
        end,
        maps:update_with(Type, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, Data),
    TypeCounts.

format_as_csv([]) ->
    <<"">>;
format_as_csv([FirstItem | _] = Data) when is_map(FirstItem) ->
    Keys = maps:keys(FirstItem),
    Header = lists:join(",", [atom_to_binary(K, utf8) || K <- Keys]),
    Rows = lists:map(fun(Item) ->
        Values = [format_csv_value(maps:get(K, Item, "")) || K <- Keys],
        lists:join(",", Values)
    end, Data),
    iolist_to_binary([Header, "\n", lists:join("\n", Rows)]);
format_as_csv(Data) ->
    Rows = lists:map(fun(Item) ->
        format_csv_value(Item)
    end, Data),
    iolist_to_binary(lists:join("\n", Rows)).

format_csv_value(Value) when is_binary(Value) -> Value;
format_csv_value(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
format_csv_value(Value) when is_number(Value) -> integer_to_binary(Value);
format_csv_value(Value) -> iolist_to_binary(io_lib:format("~p", [Value])).