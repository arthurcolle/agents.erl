%%%-------------------------------------------------------------------
%%% @doc Cowboy handler for mock API endpoints
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_mock_handler).

-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    Method = cowboy_req:method(Req0),
    
    %% Extract API path (remove /api/mock prefix)
    ApiPath = binary:replace(Path, <<"/api/mock">>, <<>>),
    
    %% Create mock operation
    Operation = #{
        <<"operationId">> => generate_operation_id(Method, ApiPath),
        <<"responses">> => #{
            <<"200">> => #{
                <<"description">> => <<"Success">>,
                <<"content">> => #{
                    <<"application/json">> => #{
                        <<"schema">> => #{
                            <<"type">> => <<"object">>
                        }
                    }
                }
            }
        }
    },
    
    %% Generate mock response
    {Status, Headers, Body} = openapi_mock_engine:generate_response(
        openapi_mock_engine, 
        Operation, 
        #{path => ApiPath, method => Method}
    ),
    
    Req = cowboy_req:reply(Status, Headers, jsx:encode(Body), Req0),
    
    {ok, Req, State}.

generate_operation_id(Method, Path) ->
    CleanPath = binary:replace(Path, <<"/">>, <<"_">>, [global]),
    <<Method/binary, CleanPath/binary>>.