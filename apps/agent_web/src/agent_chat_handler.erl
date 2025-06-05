-module(agent_chat_handler).

-export([init/2]).

%% Helper function to format errors in a human-readable way
format_error(timeout) ->
    <<"The agent took too long to respond. This might happen when the agent is processing a complex request or the AI service is slow.">>;
format_error(noproc) ->
    <<"The agent is no longer running. It may have crashed or been stopped.">>;
format_error({badrpc, nodedown}) ->
    <<"The agent service is temporarily unavailable. Please try again in a moment.">>;
format_error({error, connection_failed}) ->
    <<"Unable to connect to the AI service. Please check your internet connection and try again.">>;
format_error({error, api_key_invalid}) ->
    <<"The AI service API key is invalid or expired. Please check your configuration.">>;
format_error({error, rate_limited}) ->
    <<"Too many requests have been made. Please wait a moment before trying again.">>;
format_error({error, insufficient_quota}) ->
    <<"The AI service quota has been exceeded. Please check your billing or upgrade your plan.">>;
format_error({badarg, _}) ->
    <<"Invalid input provided to the agent. Please check your message format.">>;
format_error({function_clause, _}) ->
    <<"The agent encountered an unexpected input format. Please try rephrasing your message.">>;
format_error({error, {json_decode, _}}) ->
    <<"Invalid JSON format in the request. Please check your message format.">>;
format_error(Error) when is_atom(Error) ->
    io_lib:format("An unexpected error occurred: ~s", [atom_to_list(Error)]);
format_error({Error, Reason}) when is_atom(Error) ->
    io_lib:format("~s: ~s", [format_atom_error(Error), format_reason(Reason)]);
format_error(Error) ->
    io_lib:format("An unexpected error occurred: ~p", [Error]).

format_atom_error(throw) -> "The agent encountered an error while processing";
format_atom_error(error) -> "A system error occurred";
format_atom_error(exit) -> "The agent process exited unexpectedly";
format_atom_error(undef) -> "A required function is not available";
format_atom_error(badmatch) -> "Data format mismatch occurred";
format_atom_error(Other) -> atom_to_list(Other).

format_reason(Reason) when is_binary(Reason) -> Reason;
format_reason(Reason) when is_list(Reason) -> list_to_binary(Reason);
format_reason(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
format_reason(Reason) -> io_lib:format("~p", [Reason]).

init(Req0 = #{method := <<"POST">>}, State) ->
    AgentId = cowboy_req:binding(id, Req0),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    case catch jsx:decode(Body, [return_maps]) of
        {'EXIT', _} ->
            ReqJsonError = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{
                success => false,
                error => <<"Invalid JSON format in request body.">>,
                error_type => <<"json_decode_error">>,
                suggestion => <<"Please ensure your request body contains valid JSON.">>
            }), Req1),
            {ok, ReqJsonError, State};
        DecodedBody ->
            case DecodedBody of
                #{<<"message">> := Message} ->
            case agent_registry:find_agent(AgentId) of
                {ok, Pid} ->
                    try
                        % Send message to agent and get response
                        Response = agent:chat(Pid, Message),
                        
                        % Format response
                        JsonResponse = jsx:encode(#{
                            success => true,
                            response => Response,
                            agent_id => AgentId
                        }),
                        
                        ReqSuccess = cowboy_req:reply(200, #{
                            <<"content-type">> => <<"application/json">>
                        }, JsonResponse, Req1),
                        {ok, ReqSuccess, State}
                    catch
                        Error:Reason ->
                            HumanError = format_error({Error, Reason}),
                            ErrorResponse = jsx:encode(#{
                                success => false,
                                error => iolist_to_binary(HumanError),
                                error_type => <<"agent_error">>,
                                suggestion => <<"Please try again or contact support if the problem persists.">>
                            }),
                            ReqError = cowboy_req:reply(500, #{
                                <<"content-type">> => <<"application/json">>
                            }, ErrorResponse, Req1),
                            {ok, ReqError, State}
                    end;
                _ ->
                    ReqNotFound = cowboy_req:reply(404, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{
                        success => false,
                        error => <<"The requested agent could not be found. It may have been stopped or never existed.">>,
                        error_type => <<"agent_not_found">>,
                        suggestion => <<"Please check the agent ID and ensure the agent is running.">>
                    }), Req1),
                    {ok, ReqNotFound, State}
            end;
                _ ->
                    ReqBadRequest = cowboy_req:reply(400, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{
                        success => false,
                        error => <<"The request format is invalid. Please ensure you're sending a JSON object with a 'message' field.">>,
                        error_type => <<"invalid_request">>,
                        suggestion => <<"Expected format: {\"message\": \"your message here\"}">>
                    }), Req1),
                    {ok, ReqBadRequest, State}
            end
    end;

init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        success => false,
        error => <<"Only POST requests are allowed for this endpoint.">>,
        error_type => <<"method_not_allowed">>,
        suggestion => <<"Please use a POST request with a JSON body containing your message.">>
    }), Req0),
    {ok, Req, State}.