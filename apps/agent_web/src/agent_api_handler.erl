-module(agent_api_handler).

-export([init/2]).

init(Req0 = #{method := <<"GET">>}, State) ->
    StartTime = erlang:monotonic_time(microsecond),
    Method = <<"GET">>,
    Path = cowboy_req:path(Req0),
    Headers = cowboy_req:headers(Req0),
    ClientIP = get_client_ip(Req0),
    UserAgent = maps:get(<<"user-agent">>, Headers, <<"unknown">>),
    
    Result = case cowboy_req:binding(id, Req0) of
        undefined ->
            case Path of
                <<"/api/agents/health">> ->
                    get_agents_health(Req0, State);
                _ ->
                    list_agents(Req0, State)
            end;
        AgentId ->
            get_agent(AgentId, Req0, State)
    end,
    
    % Log the HTTP request
    case Result of
        {ok, Req, State} ->
            Duration = erlang:monotonic_time(microsecond) - StartTime,
            % Status will be determined by the response that was sent
            Status = case Path of
                <<"/api/agents/health">> -> 200;
                _ -> 200  % List agents returns 200
            end,
            system_awareness_logger:log_http_request(Method, Path, Headers, Status, Duration),
            system_awareness_logger:log_network_event(<<"http_request">>, 
                #{client_ip => ClientIP, user_agent => UserAgent, path => Path, method => Method}, 
                <<"incoming">>);
        _ -> ok
    end,
    Result;

init(Req0 = #{method := <<"POST">>}, State) ->
    log_and_handle_request(Req0, State, fun create_agent/2);

init(Req0 = #{method := <<"DELETE">>}, State) ->
    AgentId = cowboy_req:binding(id, Req0),
    log_and_handle_request(Req0, State, fun(Req, St) -> delete_agent(AgentId, Req, St) end);

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => <<"Method not allowed. This endpoint supports GET, POST, and DELETE methods.">>,
        error_type => <<"method_not_allowed">>,
        suggestion => <<"Use GET to list/view agents, POST to create agents, or DELETE to remove agents.">>
    }), Req0),
    {ok, Req, State}.

get_agents_health(Req0, State) ->
    Agents = agent_registry:list_agents(),
    HealthData = lists:map(fun({Id, Pid, Meta}) ->
        #{
            id => ensure_binary(Id),
            status => get_agent_status(Pid),
            memory => get_agent_memory(Pid),
            message_queue_len => get_message_queue_len(Pid),
            last_activity => maps:get(last_activity, Meta, erlang:system_time(millisecond)),
            health_score => calculate_health_score(Pid, Meta)
        }
    end, Agents),
    
    Response = jsx:encode(#{agents_health => HealthData}),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req0),
    {ok, Req, State}.

calculate_health_score(Pid, _Meta) ->
    % Simple health calculation based on memory and queue length
    Memory = get_agent_memory(Pid),
    QueueLen = get_message_queue_len(Pid),
    
    % Calculate score (0-100)
    MemoryScore = max(0, 100 - (Memory div 10)), % Penalize high memory
    QueueScore = max(0, 100 - (QueueLen * 10)),  % Penalize long queues
    
    round((MemoryScore + QueueScore) / 2).

list_agents(Req0, State) ->
    % Parse pagination parameters
    PaginationParams = pagination_utils:parse_pagination_params(Req0),
    #{page := _Page, page_size := _PageSize, offset := Offset} = PaginationParams,
    
    Agents = agent_registry:list_agents(),
    AgentList = lists:map(fun({Id, Pid, Meta}) ->
        % Ensure ID is always a binary
        BinaryId = case Id of
            B when is_binary(B) -> B;
            L when is_list(L) -> list_to_binary(L);
            A when is_atom(A) -> atom_to_binary(A, utf8);
            _ -> list_to_binary(io_lib:format("~p", [Id]))
        end,
        
        % Get additional template info if it's a template-based agent
        {DisplayName, Model} = case maps:get(template_id, Meta, undefined) of
            undefined -> 
                {maps:get(name, Meta, maps:get(type, Meta, <<"Unknown">>)), 
                 maps:get(model, Meta, <<"Unknown">>)};
            TemplateId ->
                % Check if there's a model override in the meta first
                ModelFromMeta = maps:get(model, Meta, undefined),
                case ModelFromMeta of
                    undefined ->
                        % Fall back to template model
                        case catch agent_templates:get_template(TemplateId) of
                            {ok, Template} ->
                                {maps:get(name, Template, TemplateId), 
                                 maps:get(model, Template, <<"Unknown">>)};
                            _ ->
                                {TemplateId, <<"Unknown">>}
                        end;
                    OverrideModel ->
                        % Use the override model from agent meta
                        case catch agent_templates:get_template(TemplateId) of
                            {ok, Template} ->
                                {maps:get(name, Template, TemplateId), OverrideModel};
                            _ ->
                                {TemplateId, OverrideModel}
                        end
                end
        end,
        
        % Ensure all fields are properly formatted
        #{
            id => BinaryId,
            pid => list_to_binary(pid_to_list(Pid)),
            type => ensure_binary(maps:get(type, Meta, <<"simple">>)),
            name => ensure_binary(DisplayName),
            label => ensure_binary(maps:get(label, Meta, DisplayName)),
            model => ensure_binary(Model),
            template_id => maps:get(template_id, Meta, undefined),
            tools => ensure_binary_list(maps:get(tools, Meta, [])),
            capabilities => ensure_binary_list(maps:get(capabilities, Meta, [])),
            status => get_agent_status(Pid),
            memory => get_agent_memory(Pid),
            message_queue_len => get_message_queue_len(Pid),
            created_at => maps:get(created_at, Meta, erlang:system_time(millisecond)),
            conversation_length => maps:get(conversation_length, Meta, 0)
        }
    end, Agents),
    
    % Apply pagination
    {PageAgents, Metadata} = pagination_utils:paginate_list(
        AgentList, 
        Offset, 
        maps:get(page_size, PaginationParams)
    ),
    
    % Format response with pagination metadata
    PaginatedResponse = pagination_utils:format_pagination_response(
        PageAgents, 
        Metadata, 
        <<"agents">>, 
        #{}
    ),
    
    Response = jsx:encode(PaginatedResponse),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req0),
    {ok, Req, State}.

get_agent(AgentId, Req0, State) ->
    % Ensure AgentId is in the correct format
    NormalizedId = normalize_agent_id(AgentId),
    case agent_registry:find_agent(NormalizedId) of
        {ok, Pid} ->
            case catch agent:get_info(Pid) of
                Info when is_map(Info) ->
                    Response = jsx:encode(Info),
                    Req = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, Response, Req0);
                _ ->
                    % If get_info fails, create basic info
                    BasicInfo = #{
                        id => NormalizedId,
                        pid => list_to_binary(pid_to_list(Pid)),
                        status => <<"active">>
                    },
                    Response = jsx:encode(BasicInfo),
                    Req = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, Response, Req0)
            end;
        {error, agent_not_found} ->
            Req = cowboy_req:reply(404, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => <<"The requested agent could not be found.">>,
                error_type => <<"agent_not_found">>,
                suggestion => <<"Please check the agent ID or use GET /api/agents to see available agents.">>
            }), Req0)
    end,
    {ok, Req, State}.

create_agent(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case catch jsx:decode(Body, [return_maps]) of
        {'EXIT', _} ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => <<"Invalid JSON format in request body.">>,
                error_type => <<"json_decode_error">>,
                suggestion => <<"Please ensure your request body contains valid JSON.">>
            }), Req1),
            {ok, Req, State};
        DecodedBody ->
            case DecodedBody of
                #{<<"type">> := Type, <<"name">> := Name} = RequestData ->
                    AgentType = binary_to_atom(Type, utf8),
                    AgentName = Name,
                    Tools = maps:get(<<"tools">>, RequestData, []),
                    
                    case create_agent_by_type(AgentType, AgentName, Tools) of
                        {ok, AgentId, Pid} ->
                            Response = jsx:encode(#{
                                success => true,
                                agent => #{
                                    id => AgentId,
                                    pid => list_to_binary(pid_to_list(Pid)),
                                    type => Type,
                                    name => Name
                                }
                            }),
                            Req = cowboy_req:reply(201, #{
                                <<"content-type">> => <<"application/json">>
                            }, Response, Req1);
                        {error, Reason} ->
                            Req = cowboy_req:reply(400, #{
                                <<"content-type">> => <<"application/json">>
                            }, jsx:encode(#{
                                success => false,
                                error => iolist_to_binary(["Failed to create agent: ", Reason]),
                                error_type => <<"agent_creation_failed">>,
                                suggestion => <<"Please check the agent type and configuration parameters.">>
                            }), Req1)
                    end;
                _ ->
                    Req = cowboy_req:reply(400, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{
                        success => false,
                        error => <<"Invalid request format. Missing required 'type' and 'name' fields.">>,
                        error_type => <<"invalid_request">>,
                        suggestion => <<"Expected format: {\"type\": \"agent_type\", \"name\": \"agent_name\", \"tools\": [...]}">>
                    }), Req1)
            end
    end,
    {ok, Req, State}.

delete_agent(AgentId, Req0, State) ->
    NormalizedId = normalize_agent_id(AgentId),
    case agent_registry:find_agent(NormalizedId) of
        {ok, Pid} ->
            % Stop the agent gracefully
            case catch agent:stop_agent(Pid) of
                _ -> ok  % Ignore any errors from stopping
            end,
            agent_registry:unregister_agent(NormalizedId),
            Req = cowboy_req:reply(204, #{}, Req0);
        {error, agent_not_found} ->
            Req = cowboy_req:reply(404, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => <<"The requested agent could not be found.">>,
                error_type => <<"agent_not_found">>,
                suggestion => <<"Please check the agent ID or use GET /api/agents to see available agents.">>
            }), Req0)
    end,
    {ok, Req, State}.

create_agent_by_type(simple, Name, Tools) ->
    AgentId = generate_uuid(),
    Config = #{
        id => AgentId,
        name => ensure_binary(Name),
        type => simple,
        model => <<"simple">>,
        tools => Tools,
        system_prompt => <<"You are a simple agent that can help with basic tasks.">>
    },
    case catch agent_instance:start_link(Config) of
        {ok, Pid} ->
            Meta = #{
                type => simple,
                name => ensure_binary(Name),
                label => ensure_binary(Name),
                tools => ensure_binary_list(Tools),
                capabilities => [<<"basic_conversation">>],
                created_at => erlang:system_time(millisecond),
                conversation_length => 0,
                model => <<"simple">>,
                status => <<"active">>
            },
            agent_registry:register_agent(AgentId, Pid, Meta),
            {ok, AgentId, Pid};
        Error ->
            {error, iolist_to_binary(io_lib:format("Failed to start simple agent: ~p", [Error]))}
    end;

create_agent_by_type(ai, Name, Tools) ->
    AgentId = generate_uuid(),
    ToolList = lists:map(fun(T) -> 
        case T of
            B when is_binary(B) -> binary_to_atom(B, utf8);
            A when is_atom(A) -> A;
            L when is_list(L) -> list_to_atom(L);
            _ -> T
        end
    end, Tools),
    
    % Define capabilities based on tools
    Capabilities = [<<"conversation">>, <<"multi_turn_chat">>] ++ 
                  [<<"function_calling">> || length(ToolList) > 0] ++
                  [atom_to_binary(Tool, utf8) || Tool <- ToolList],
    
    Config = #{
        id => AgentId,
        name => ensure_binary(Name),
        type => ai,
        model => <<"gpt-4">>,
        tools => ToolList,
        system_prompt => <<"You are a helpful AI assistant with advanced capabilities. You can engage in multi-turn conversations and use various tools to help users.">>
    },
    
    case catch agent_instance:start_link(Config) of
        {ok, Pid} ->
            Meta = #{
                type => ai,
                name => ensure_binary(Name),
                label => ensure_binary(Name),
                tools => ensure_binary_list(Tools),
                capabilities => Capabilities,
                created_at => erlang:system_time(millisecond),
                conversation_length => 0,
                model => <<"gpt-4">>,
                status => <<"active">>,
                system_prompt => <<"You are a helpful AI assistant with advanced capabilities.">>
            },
            agent_registry:register_agent(AgentId, Pid, Meta),
            {ok, AgentId, Pid};
        Error ->
            {error, iolist_to_binary(io_lib:format("Failed to start AI agent: ~p", [Error]))}
    end;

create_agent_by_type(template, Name, TemplateId) when is_binary(TemplateId) ->
    % Create agent from template
    AgentId = generate_uuid(),
    case agent_templates:create_from_template(TemplateId, #{agent_id => AgentId}) of
        {ok, Pid} ->
            agent_registry:register_agent(AgentId, Pid, #{
                type => template, 
                name => Name,
                template_id => TemplateId
            }),
            {ok, AgentId, Pid};
        {error, Reason} ->
            {error, Reason}
    end;

create_agent_by_type(researcher, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"researcher">>);

create_agent_by_type(coder, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"coder">>);

create_agent_by_type(analyst, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"analyst">>);

create_agent_by_type(orchestrator, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"orchestrator">>);

create_agent_by_type(monitor, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"monitor">>);

create_agent_by_type(translator, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"translator">>);

create_agent_by_type(teacher, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"teacher">>);

create_agent_by_type(debugger, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"debugger">>);

create_agent_by_type(advanced_researcher, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"advanced_researcher">>);

create_agent_by_type(efficient_coder, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"efficient_coder">>);

create_agent_by_type(lightweight_helper, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"lightweight_helper">>);

create_agent_by_type(quantum_analyst, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"quantum_analyst">>);

create_agent_by_type(micro_orchestrator, Name, _Tools) ->
    create_agent_by_type(template, Name, <<"micro_orchestrator">>);

create_agent_by_type(_, _, _) ->
    {error, <<"Unknown agent type">>}.

%% Helper functions
generate_uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    ID = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", 
                       [A, B, C, D, E]),
    list_to_binary(ID).

normalize_agent_id(Id) when is_binary(Id) -> Id;
normalize_agent_id(Id) when is_list(Id) -> list_to_binary(Id);
normalize_agent_id(Id) when is_atom(Id) -> atom_to_binary(Id, utf8);
normalize_agent_id(Id) -> list_to_binary(io_lib:format("~p", [Id])).

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) -> list_to_binary(io_lib:format("~p", [Value])).

ensure_binary_list(List) when is_list(List) ->
    lists:map(fun ensure_binary/1, List);
ensure_binary_list(_) -> [].

%% Helper functions for agent metrics
get_agent_status(Pid) ->
    case process_info(Pid, status) of
        {status, running} -> <<"active">>;
        {status, waiting} -> <<"idle">>;
        {status, suspended} -> <<"suspended">>;
        _ -> <<"unknown">>
    end.

get_agent_memory(Pid) ->
    case process_info(Pid, memory) of
        {memory, Bytes} -> 
            % Convert to MB
            round(Bytes / 1048576);
        _ -> 0
    end.

get_message_queue_len(Pid) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, Len} -> Len;
        _ -> 0
    end.

%% HTTP logging helper functions
log_and_handle_request(Req0, State, HandlerFun) ->
    StartTime = erlang:monotonic_time(microsecond),
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    Headers = cowboy_req:headers(Req0),
    ClientIP = get_client_ip(Req0),
    UserAgent = maps:get(<<"user-agent">>, Headers, <<"unknown">>),
    
    Result = HandlerFun(Req0, State),
    
    % Log the HTTP request
    case Result of
        {ok, Req, NewState} ->
            Duration = erlang:monotonic_time(microsecond) - StartTime,
            Status = get_response_status(Req),
            system_awareness_logger:log_http_request(Method, Path, Headers, Status, Duration),
            system_awareness_logger:log_network_event(<<"http_request">>, 
                #{client_ip => ClientIP, user_agent => UserAgent, path => Path, method => Method}, 
                <<"incoming">>),
            {ok, Req, NewState};
        _ -> Result
    end.

get_client_ip(Req) ->
    case cowboy_req:header(<<"x-forwarded-for">>, Req) of
        undefined ->
            case cowboy_req:peer(Req) of
                {IP, _Port} -> 
                    case IP of
                        {A, B, C, D} -> 
                            iolist_to_binary(io_lib:format("~w.~w.~w.~w", [A, B, C, D]));
                        {A, B, C, D, E, F, G, H} ->
                            iolist_to_binary(io_lib:format("~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b", 
                                            [A, B, C, D, E, F, G, H]));
                        _ -> <<"unknown">>
                    end;
                _ -> <<"unknown">>
            end;
        ForwardedFor ->
            % Use the first IP in the X-Forwarded-For header
            case binary:split(ForwardedFor, <<",">>) of
                [FirstIP | _] -> string:trim(FirstIP);
                _ -> ForwardedFor
            end
    end.

get_response_status(_Req) ->
    % Since we can't reliably get the response status from the request object
    % in this version of cowboy, we'll return a default status
    200.