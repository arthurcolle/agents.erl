%%%-------------------------------------------------------------------
%%% @doc Cowboy handler for GraphQL endpoint
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_graphql_handler).

-export([init/2]).

init(Req0, State) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    
    %% Parse GraphQL request
    Request = jsx:decode(Body, [return_maps]),
    
    Query = maps:get(<<"query">>, Request),
    Variables = maps:get(<<"variables">>, Request, #{}),
    OperationName = maps:get(<<"operationName">>, Request, undefined),
    
    %% Determine API name from header or default
    ApiName = cowboy_req:header(<<"x-api-name">>, Req, <<"default">>),
    
    %% Execute GraphQL
    Result = case determine_operation_type(Query) of
        query -> openapi_graphql_bridge:execute_query(ApiName, Query, Variables);
        mutation -> openapi_graphql_bridge:execute_mutation(ApiName, Query, Variables);
        subscription -> {error, <<"Subscriptions not supported via HTTP">>}
    end,
    
    Response = case Result of
        {ok, Data} -> Data;
        {error, Error} -> #{<<"errors">> => [#{<<"message">> => Error}]}
    end,
    
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Response),
        Req),
    
    {ok, Req2, State}.

determine_operation_type(Query) ->
    %% Simple detection - in production would use proper parser
    case binary:match(Query, <<"mutation">>) of
        {_, _} -> mutation;
        nomatch ->
            case binary:match(Query, <<"subscription">>) of
                {_, _} -> subscription;
                nomatch -> query
            end
    end.