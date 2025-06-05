-module(agent_communication_handler).

-export([init/2]).

init(Req0 = #{method := <<"POST">>}, State) ->
    Path = cowboy_req:path(Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    Response = case Path of
        <<"/api/agents/communicate">> ->
            handle_send_message(Body);
        <<"/api/agents/broadcast">> ->
            handle_broadcast(Body);
        <<"/api/agents/collaboration/create">> ->
            handle_create_collaboration(Body);
        <<"/api/agents/collaboration/join">> ->
            handle_join_collaboration(Body);
        <<"/api/agents/collaboration/leave">> ->
            handle_leave_collaboration(Body);
        <<"/api/agents/budget/reset">> ->
            handle_reset_budget(Body);
        _ ->
            {error, <<"Unknown endpoint">>}
    end,
    
    {StatusCode, JsonResponse} = case Response of
        {ok, Data} -> {200, jsx:encode(#{success => true, data => Data})};
        {error, Reason} -> {400, jsx:encode(#{success => false, error => Reason})}
    end,
    
    Req = cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"application/json">>
    }, JsonResponse, Req1),
    
    {ok, Req, State};

init(Req0 = #{method := <<"GET">>}, State) ->
    Path = cowboy_req:path(Req0),
    
    Response = case Path of
        <<"/api/agents/messages/", AgentId/binary>> ->
            agent_communication_api:get_agent_messages(AgentId);
        <<"/api/agents/collaborations">> ->
            agent_communication_api:get_collaborations();
        <<"/api/agents/budget/", AgentId/binary>> ->
            {ok, agent_communication_api:get_budget_info(AgentId)};
        _ ->
            {error, <<"Unknown endpoint">>}
    end,
    
    {StatusCode, JsonResponse} = case Response of
        {ok, Data} -> {200, jsx:encode(#{success => true, data => Data})};
        {error, Reason} -> {400, jsx:encode(#{success => false, error => Reason})}
    end,
    
    Req = cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"application/json">>
    }, JsonResponse, Req0),
    
    {ok, Req, State};

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, State}.

%% Internal functions
handle_send_message(Body) ->
    try
        #{<<"from">> := From, <<"to">> := To, <<"message">> := Message} = jsx:decode(Body, [return_maps]),
        
        case To of
            <<"broadcast">> ->
                agent_communication_api:broadcast_message(From, Message);
            _ ->
                agent_communication_api:send_message(From, To, Message)
        end
    catch
        _:_ -> {error, <<"Invalid request body">>}
    end.

handle_broadcast(Body) ->
    try
        #{<<"from">> := From, <<"message">> := Message} = jsx:decode(Body, [return_maps]),
        agent_communication_api:broadcast_message(From, Message)
    catch
        _:_ -> {error, <<"Invalid request body">>}
    end.

handle_create_collaboration(Body) ->
    try
        #{<<"agents">> := AgentIds, <<"topic">> := Topic} = jsx:decode(Body, [return_maps]),
        agent_communication_api:create_collaboration(AgentIds, Topic)
    catch
        _:_ -> {error, <<"Invalid request body">>}
    end.

handle_join_collaboration(Body) ->
    try
        #{<<"agentId">> := AgentId, <<"collaborationId">> := CollabId} = jsx:decode(Body, [return_maps]),
        agent_communication_api:join_collaboration(AgentId, CollabId)
    catch
        _:_ -> {error, <<"Invalid request body">>}
    end.

handle_leave_collaboration(Body) ->
    try
        #{<<"agentId">> := AgentId, <<"collaborationId">> := CollabId} = jsx:decode(Body, [return_maps]),
        agent_communication_api:leave_collaboration(AgentId, CollabId)
    catch
        _:_ -> {error, <<"Invalid request body">>}
    end.

handle_reset_budget(Body) ->
    try
        #{<<"agentId">> := AgentId} = jsx:decode(Body, [return_maps]),
        agent_communication_api:reset_budget(AgentId),
        {ok, #{message => <<"Budget reset successfully">>, agentId => AgentId}}
    catch
        _:_ -> {error, <<"Invalid request body">>}
    end.