-module(agent_api_handler).

-export([init/2]).

init(Req0 = #{method := <<"GET">>}, State) ->
    case cowboy_req:binding(id, Req0) of
        undefined ->
            list_agents(Req0, State);
        AgentId ->
            get_agent(AgentId, Req0, State)
    end;

init(Req0 = #{method := <<"POST">>}, State) ->
    create_agent(Req0, State);

init(Req0 = #{method := <<"DELETE">>}, State) ->
    AgentId = cowboy_req:binding(id, Req0),
    delete_agent(AgentId, Req0, State);

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{}, Req0),
    {ok, Req, State}.

list_agents(Req0, State) ->
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
                {maps:get(name, Meta, maps:get(type, Meta, <<"Unknown">>)), <<"Unknown">>};
            TemplateId ->
                case catch agent_templates:get_template(TemplateId) of
                    {ok, Template} ->
                        {maps:get(name, Template, TemplateId), maps:get(model, Template, <<"Unknown">>)};
                    _ ->
                        {TemplateId, <<"Unknown">>}
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
            status => <<"active">>,
            created_at => maps:get(created_at, Meta, erlang:system_time(millisecond)),
            conversation_length => maps:get(conversation_length, Meta, 0)
        }
    end, Agents),
    
    Response = jsx:encode(#{agents => AgentList}),
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
            }, jsx:encode(#{error => <<"Agent not found">>}), Req0)
    end,
    {ok, Req, State}.

create_agent(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"type">> := Type, <<"name">> := Name} = RequestData ->
            AgentType = binary_to_atom(Type, utf8),
            AgentName = Name,
            Tools = maps:get(<<"tools">>, RequestData, []),
            
            case create_agent_by_type(AgentType, AgentName, Tools) of
                {ok, AgentId, Pid} ->
                    Response = jsx:encode(#{
                        id => AgentId,
                        pid => list_to_binary(pid_to_list(Pid)),
                        type => Type,
                        name => Name
                    }),
                    Req = cowboy_req:reply(201, #{
                        <<"content-type">> => <<"application/json">>
                    }, Response, Req1);
                {error, Reason} ->
                    Req = cowboy_req:reply(400, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{error => Reason}), Req1)
            end;
        _ ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Invalid request body">>}), Req1)
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
            }, jsx:encode(#{error => <<"Agent not found">>}), Req0)
    end,
    {ok, Req, State}.

create_agent_by_type(simple, Name, Tools) ->
    AgentId = list_to_binary(uuid:to_string(uuid:uuid4())),
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
    AgentId = list_to_binary(uuid:to_string(uuid:uuid4())),
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
    AgentId = list_to_binary(uuid:to_string(uuid:uuid4())),
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
normalize_agent_id(Id) when is_binary(Id) -> Id;
normalize_agent_id(Id) when is_list(Id) -> list_to_binary(Id);
normalize_agent_id(Id) when is_atom(Id) -> atom_to_binary(Id, utf8);
normalize_agent_id(Id) -> list_to_binary(io_lib:format("~p", [Id])).

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
ensure_binary(Value) -> list_to_binary(io_lib:format("~p", [Value])).

ensure_string(Value) when is_list(Value) -> Value;
ensure_string(Value) when is_binary(Value) -> binary_to_list(Value);
ensure_string(Value) when is_atom(Value) -> atom_to_list(Value);
ensure_string(Value) -> io_lib:format("~p", [Value]).

ensure_binary_list(List) when is_list(List) ->
    lists:map(fun ensure_binary/1, List);
ensure_binary_list(_) -> [].