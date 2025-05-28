-module(agent_execute_handler).

-export([init/2]).

init(Req0 = #{method := <<"POST">>}, State) ->
    AgentId = cowboy_req:binding(id, Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    case jsx:decode(Body, [return_maps]) of
        #{<<"action">> := Action, <<"params">> := Params} ->
            case agent_registry:find_agent(binary_to_list(AgentId)) of
                {ok, Pid} ->
                    Result = execute_action(Pid, Action, Params),
                    Response = jsx:encode(#{result => Result}),
                    Req = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, Response, Req1);
                {error, not_found} ->
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
    Req = cowboy_req:reply(405, #{}, Req0),
    {ok, Req, State}.

execute_action(Pid, <<"chat">>, #{<<"message">> := Message}) ->
    Response = agent:chat(Pid, binary_to_list(Message)),
    list_to_binary(Response);

execute_action(Pid, <<"process">>, #{<<"task">> := Task}) ->
    Result = agent:process_task(Pid, Task),
    Result;

execute_action(_Pid, _Action, _Params) ->
    #{error => <<"Unknown action">>}.