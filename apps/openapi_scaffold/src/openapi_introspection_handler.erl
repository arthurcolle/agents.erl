%%%-------------------------------------------------------------------
%%% @doc OpenAPI Introspection Cowboy Handler
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_introspection_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    Path = cowboy_req:path(Req),
    Method = cowboy_req:method(Req),
    
    Result = case {Method, Path} of
        {<<"GET">>, <<"/openapi/specs">>} ->
            handle_list_specs();
            
        {<<"GET">>, <<"/openapi/specs/", SpecId/binary>>} ->
            handle_get_spec_info(SpecId);
            
        {<<"GET">>, <<"/openapi/routes/", SpecId/binary>>} ->
            handle_get_routes(SpecId);
            
        {<<"GET">>, <<"/openapi/schemas/", SpecId/binary>>} ->
            handle_get_schemas(SpecId);
            
        _ ->
            {404, #{}, #{<<"error">> => <<"Not found">>}}
    end,
    
    {StatusCode, Headers, Body} = Result,
    {ok, cowboy_req:reply(StatusCode, 
        maps:merge(#{<<"content-type">> => <<"application/json">>}, Headers),
        jsx:encode(Body), Req), State}.

handle_list_specs() ->
    case openapi_introspection:get_loaded_specs() of
        {ok, Specs} ->
            {200, #{}, #{<<"specs">> => Specs}};
        {error, Reason} ->
            {500, #{}, #{<<"error">> => format_error(Reason)}}
    end.

handle_get_spec_info(SpecId) ->
    case openapi_introspection:get_spec_info(SpecId) of
        {ok, Info} ->
            {200, #{}, Info};
        {error, not_found} ->
            {404, #{}, #{<<"error">> => <<"Spec not found">>}};
        {error, Reason} ->
            {500, #{}, #{<<"error">> => format_error(Reason)}}
    end.

handle_get_routes(SpecId) ->
    case openapi_introspection:get_routes(SpecId) of
        {ok, Routes} ->
            {200, #{}, #{<<"routes">> => Routes}};
        {error, not_found} ->
            {404, #{}, #{<<"error">> => <<"Spec not found">>}};
        {error, Reason} ->
            {500, #{}, #{<<"error">> => format_error(Reason)}}
    end.

handle_get_schemas(SpecId) ->
    case openapi_introspection:get_schemas(SpecId) of
        {ok, Schemas} ->
            {200, #{}, #{<<"schemas">> => Schemas}};
        {error, not_found} ->
            {404, #{}, #{<<"error">> => <<"Spec not found">>}};
        {error, Reason} ->
            {500, #{}, #{<<"error">> => format_error(Reason)}}
    end.

format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).