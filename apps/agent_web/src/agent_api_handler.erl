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
        #{id => Id, 
          pid => list_to_binary(pid_to_list(Pid)),
          type => maps:get(type, Meta, undefined),
          capabilities => maps:get(capabilities, Meta, [])}
    end, Agents),
    
    Response = jsx:encode(#{agents => AgentList}),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req0),
    {ok, Req, State}.

get_agent(AgentId, Req0, State) ->
    case agent_registry:find_agent(binary_to_list(AgentId)) of
        {ok, Pid} ->
            Info = agent:get_info(Pid),
            Response = jsx:encode(Info),
            Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req0);
        {error, not_found} ->
            Req = cowboy_req:reply(404, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Agent not found">>}), Req0)
    end,
    {ok, Req, State}.

create_agent(Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    case jsx:decode(Body, [return_maps]) of
        #{<<"type">> := Type, <<"name">> := Name} ->
            AgentType = binary_to_atom(Type, utf8),
            AgentName = binary_to_list(Name),
            Tools = maps:get(<<"tools">>, jsx:decode(Body, [return_maps]), []),
            
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
    case agent_registry:find_agent(binary_to_list(AgentId)) of
        {ok, Pid} ->
            agent:stop_agent(Pid),
            agent_registry:unregister_agent(binary_to_list(AgentId)),
            Req = cowboy_req:reply(204, #{}, Req0);
        {error, not_found} ->
            Req = cowboy_req:reply(404, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Agent not found">>}), Req0)
    end,
    {ok, Req, State}.

create_agent_by_type(simple, Name, _Tools) ->
    {ok, Pid} = simple_agent:start_link(Name),
    AgentId = uuid:to_string(uuid:uuid4()),
    agent_registry:register_agent(AgentId, Pid, #{type => simple, name => Name}),
    {ok, AgentId, Pid};

create_agent_by_type(ai, Name, Tools) ->
    ToolList = lists:map(fun(T) -> binary_to_atom(T, utf8) end, Tools),
    {ok, Pid} = agent:start_link(#{
        name => Name,
        model => "gpt-4",
        tools => ToolList,
        system_prompt => "You are a helpful AI assistant."
    }),
    AgentId = uuid:to_string(uuid:uuid4()),
    agent_registry:register_agent(AgentId, Pid, #{type => ai, name => Name, tools => ToolList}),
    {ok, AgentId, Pid};

create_agent_by_type(template, Name, TemplateId) when is_binary(TemplateId) ->
    % Create agent from template
    AgentId = uuid:to_string(uuid:uuid4()),
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

create_agent_by_type(_, _, _) ->
    {error, <<"Unknown agent type">>}.