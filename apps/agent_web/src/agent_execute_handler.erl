-module(agent_execute_handler).

-export([init/2]).

init(Req0 = #{method := <<"POST">>}, State) ->
    AgentId = cowboy_req:binding(id, Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    case jsx:decode(Body, [return_maps]) of
        #{<<"action">> := Action, <<"params">> := Params} ->
            NormalizedId = normalize_agent_id(AgentId),
            case agent_registry:find_agent(NormalizedId) of
                {ok, Pid} ->
                    % Log the action request
                    ConversationId = <<"http_", AgentId/binary>>,
                    conversation_stats_logger:log_message(ConversationId, NormalizedId, <<"user">>, 
                        #{action => Action, params => Params}),
                    
                    Result = execute_action(Pid, Action, Params),
                    
                    % Log the response
                    conversation_stats_logger:log_message(ConversationId, NormalizedId, <<"agent">>, Result),
                    
                    Response = jsx:encode(#{result => Result}),
                    Req = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, Response, Req1);
                {error, agent_not_found} ->
                    Req = cowboy_req:reply(404, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{error => <<"Agent not found">>}), Req1)
            end;
        _ ->
            Req = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"Invalid request body">>}), Req1)
    end,
    {ok, Req, State};

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{}, <<>>, Req0),
    {ok, Req, State}.

execute_action(Pid, <<"chat">>, #{<<"message">> := Message}) ->
    % Ensure message is in correct format
    MessageStr = case Message of
        B when is_binary(B) -> binary_to_list(B);
        L when is_list(L) -> L;
        _ -> io_lib:format("~p", [Message])
    end,
    case catch agent:chat(Pid, MessageStr) of
        Response when is_list(Response) -> list_to_binary(Response);
        Response when is_binary(Response) -> Response;
        Response -> list_to_binary(io_lib:format("~p", [Response]))
    end;

execute_action(Pid, <<"stream_chat">>, #{<<"message">> := Message}) ->
    % For streaming chat, use the same logic but indicate streaming
    MessageStr = case Message of
        B when is_binary(B) -> binary_to_list(B);
        L when is_list(L) -> L;
        _ -> io_lib:format("~p", [Message])
    end,
    case catch agent:stream_chat(Pid, MessageStr) of
        ok -> #{status => <<"streaming">>};
        Response -> list_to_binary(io_lib:format("~p", [Response]))
    end;

execute_action(Pid, <<"process">>, #{<<"task">> := Task}) ->
    case catch agent:process_task(Pid, Task) of
        Result when is_map(Result) -> Result;
        Result -> #{result => Result}
    end;

execute_action(_Pid, Action, _Params) ->
    #{error => <<"Unknown action: ", Action/binary>>}.

%% Helper functions
normalize_agent_id(Id) when is_binary(Id) -> Id;
normalize_agent_id(Id) when is_list(Id) -> list_to_binary(Id);
normalize_agent_id(Id) when is_atom(Id) -> atom_to_binary(Id, utf8);
normalize_agent_id(Id) -> list_to_binary(io_lib:format("~p", [Id])).