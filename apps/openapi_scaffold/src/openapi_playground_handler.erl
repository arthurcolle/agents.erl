%%%-------------------------------------------------------------------
%%% @doc Cowboy handler for OpenAPI Playground
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_playground_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    
    Result = case {Method, Path} of
        {<<"GET">>, <<"/api/playground/specs">>} ->
            handle_get_specs(Req0);
        {<<"POST">>, <<"/api/playground/execute/", SpecName/binary>>} ->
            handle_execute_request(SpecName, Req0);
        {<<"GET">>, <<"/api/playground/spec/", SpecName/binary>>} ->
            handle_get_spec(SpecName, Req0);
        _ ->
            cowboy_req:reply(404, #{}, <<"Not found">>, Req0)
    end,
    
    {ok, Result, State}.

handle_get_specs(Req) ->
    %% Get all registered specs
    Specs = #{
        <<"petstore">> => #{
            <<"name">> => <<"Pet Store API">>,
            <<"version">> => <<"1.0.0">>
        },
        <<"example">> => #{
            <<"name">> => <<"Example API">>,
            <<"version">> => <<"2.0.0">>
        }
    },
    
    cowboy_req:reply(200, 
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Specs),
        Req).

handle_execute_request(SpecName, Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    Request = jsx:decode(Body, [return_maps]),
    
    %% Execute the request
    Result = openapi_playground:execute_request(SpecName, Request),
    
    Response = case Result of
        {ok, Data} -> Data;
        {error, Error} -> #{<<"error">> => Error}
    end,
    
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Response),
        Req).

handle_get_spec(SpecName, Req) ->
    case openapi_playground:get_playground_data(SpecName) of
        {ok, Data} ->
            cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(Data),
                Req);
        {error, not_found} ->
            cowboy_req:reply(404, #{}, <<"Spec not found">>, Req)
    end.