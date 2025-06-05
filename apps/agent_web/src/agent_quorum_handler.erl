-module(agent_quorum_handler).

-export([init/2]).

init(Req0 = #{method := <<"POST">>}, State) ->
    Path = cowboy_req:path(Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    Response = case Path of
        <<"/api/agents/quorum/propose">> ->
            handle_propose_decision(Body);
        <<"/api/agents/quorum/", Rest/binary>> ->
            case binary:split(Rest, <<"/">>) of
                [DecisionId, <<"vote">>] ->
                    handle_cast_vote(DecisionId, Body);
                _ ->
                    {error, <<"Unknown endpoint">>}
            end;
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
        <<"/api/agents/quorum/decisions">> ->
            agent_quorum:list_decisions();
        <<"/api/agents/quorum/decisions/", DecisionId/binary>> ->
            agent_quorum:get_decision(DecisionId);
        <<"/api/agents/quorum/history/", AgentId/binary>> ->
            agent_quorum:get_agent_voting_history(AgentId);
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
handle_propose_decision(Body) ->
    try
        #{
            <<"topic">> := Topic,
            <<"proposer">> := Proposer,
            <<"participants">> := Participants,
            <<"threshold">> := Threshold,
            <<"deadline">> := Deadline
        } = jsx:decode(Body, [return_maps]),
        
        % Ensure threshold is a float between 0 and 1
        ThresholdFloat = case Threshold of
            T when is_float(T) -> T;
            T when is_integer(T) -> T / 100.0  % Convert percentage to decimal
        end,
        
        agent_quorum:propose_decision(Topic, Proposer, Participants, ThresholdFloat, Deadline)
    catch
        _:_ -> {error, <<"Invalid request body">>}
    end.

handle_cast_vote(DecisionId, Body) ->
    try
        #{<<"agentId">> := AgentId, <<"vote">> := Vote} = jsx:decode(Body, [return_maps]),
        
        VoteAtom = case Vote of
            <<"yes">> -> yes;
            <<"no">> -> no;
            <<"abstain">> -> abstain;
            _ -> throw(invalid_vote)
        end,
        
        agent_quorum:cast_vote(DecisionId, AgentId, VoteAtom)
    catch
        _:_ -> {error, <<"Invalid request body">>}
    end.