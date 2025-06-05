%%%-------------------------------------------------------------------
%%% @doc OpenAPI Dynamic Request Handler
%%% Handles all dynamic routes based on loaded OpenAPI specs
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_dynamic_handler).
-behaviour(cowboy_handler).

-include("../include/openapi_scaffold.hrl").

-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    Method = cowboy_req:method(Req0),
    
    %% Try to match the route
    case openapi_router:match_route(Path, Method) of
        {ok, Route, PathParams} ->
            %% Add path parameters to request
            Req1 = lists:foldl(fun({Name, Value}, ReqAcc) ->
                cowboy_req:set_binding(Name, Value, ReqAcc)
            end, Req0, PathParams),
            
            %% Get the operation details
            Operation = maps:get(Method, Route#route.methods),
            
            %% Validate the request
            case openapi_validator:validate_request(Req1, Operation) of
                {ok, ValidatedReq} ->
                    handle_request(ValidatedReq, Route, Operation, State);
                {error, ValidationErrors} ->
                    ErrorResponse = #{
                        <<"error">> => <<"Validation failed">>,
                        <<"errors">> => ValidationErrors
                    },
                    {ok, cowboy_req:reply(400, 
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(ErrorResponse), Req1), State}
            end;
            
        {error, not_found} ->
            %% No matching route
            ErrorResponse = #{
                <<"error">> => <<"Route not found">>,
                <<"path">> => Path,
                <<"method">> => Method
            },
            {ok, cowboy_req:reply(404, 
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(ErrorResponse), Req0), State}
    end.

handle_request(Req, Route, Operation, State) ->
    SpecId = Route#route.spec_id,
    
    %% Check if we should proxy the request
    ProxyEnabled = application:get_env(openapi_scaffold, proxy_enabled, true),
    
    Response = case ProxyEnabled of
        true ->
            %% Try to proxy the request
            case openapi_proxy:proxy_request(SpecId, Req, Route, Operation) of
                {ok, ProxyStatusCode, Headers, Body} ->
                    {ProxyStatusCode, Headers, Body};
                {error, no_proxy_config} ->
                    %% No proxy config, generate mock response
                    generate_mock_response(Operation);
                {error, Reason} ->
                    %% Proxy failed
                    ErrorBody = jsx:encode(#{
                        <<"error">> => <<"Proxy failed">>,
                        <<"reason">> => format_error(Reason)
                    }),
                    {502, #{<<"content-type">> => <<"application/json">>}, ErrorBody}
            end;
        false ->
            %% Proxy disabled, generate mock response
            generate_mock_response(Operation)
    end,
    
    {StatusCode, RespHeaders, RespBody} = Response,
    
    %% Validate response if enabled
    case openapi_validator:validate_response(StatusCode, RespHeaders, RespBody) of
        ok ->
            ok;
        {error, _ValidationErrors} ->
            %% Log validation errors but still return the response
            error_logger:warning_msg("Response validation failed: ~p~n", [_ValidationErrors])
    end,
    
    {ok, cowboy_req:reply(StatusCode, RespHeaders, RespBody, Req), State}.

generate_mock_response(Operation) ->
    %% Extract response schemas
    Responses = maps:get(<<"responses">>, Operation, #{}),
    
    %% Try to find a successful response (2xx)
    SuccessResponse = find_success_response(Responses),
    
    case SuccessResponse of
        {StatusCode, ResponseDef} ->
            %% Generate mock data based on schema
            {Headers, Body} = generate_response_content(ResponseDef),
            {StatusCode, Headers, Body};
        not_found ->
            %% No response defined, return generic success
            {200, #{<<"content-type">> => <<"application/json">>}, 
             jsx:encode(#{<<"message">> => <<"Success">>})}
    end.

find_success_response(Responses) ->
    %% Look for 200, 201, or 2XX responses
    case maps:find(<<"200">>, Responses) of
        {ok, Resp} -> {200, Resp};
        error ->
            case maps:find(<<"201">>, Responses) of
                {ok, Resp} -> {201, Resp};
                error ->
                    case maps:find(<<"2XX">>, Responses) of
                        {ok, Resp} -> {200, Resp};
                        error -> not_found
                    end
            end
    end.

generate_response_content(ResponseDef) ->
    Content = maps:get(<<"content">>, ResponseDef, #{}),
    
    %% Try to find JSON content
    case maps:find(<<"application/json">>, Content) of
        {ok, MediaType} ->
            Schema = maps:get(<<"schema">>, MediaType, #{}),
            MockData = generate_mock_data(Schema),
            Headers = #{<<"content-type">> => <<"application/json">>},
            Body = jsx:encode(MockData),
            {Headers, Body};
        error ->
            %% Try other content types
            case maps:to_list(Content) of
                [{ContentType, MediaType} | _] ->
                    Schema = maps:get(<<"schema">>, MediaType, #{}),
                    MockData = generate_mock_data(Schema),
                    Headers = #{<<"content-type">> => ContentType},
                    Body = format_response_body(ContentType, MockData),
                    {Headers, Body};
                [] ->
                    %% No content defined
                    {#{}, <<>>}
            end
    end.

generate_mock_data(Schema) ->
    Type = maps:get(<<"type">>, Schema, <<"object">>),
    generate_mock_for_type(Type, Schema).

generate_mock_for_type(<<"string">>, Schema) ->
    case maps:find(<<"enum">>, Schema) of
        {ok, [First | _]} -> First;
        error ->
            case maps:find(<<"format">>, Schema) of
                {ok, <<"date">>} -> <<"2024-01-01">>;
                {ok, <<"date-time">>} -> <<"2024-01-01T00:00:00Z">>;
                {ok, <<"email">>} -> <<"example@example.com">>;
                {ok, <<"uuid">>} -> <<"550e8400-e29b-41d4-a716-446655440000">>;
                _ -> <<"example string">>
            end
    end;

generate_mock_for_type(<<"integer">>, Schema) ->
    case maps:find(<<"minimum">>, Schema) of
        {ok, Min} -> Min;
        error -> 42
    end;

generate_mock_for_type(<<"number">>, Schema) ->
    case maps:find(<<"minimum">>, Schema) of
        {ok, Min} -> float(Min);
        error -> 3.14
    end;

generate_mock_for_type(<<"boolean">>, _Schema) ->
    true;

generate_mock_for_type(<<"array">>, Schema) ->
    ItemSchema = maps:get(<<"items">>, Schema, #{}),
    MinItems = maps:get(<<"minItems">>, Schema, 1),
    MaxItems = maps:get(<<"maxItems">>, Schema, 3),
    NumItems = min(MaxItems, max(MinItems, 2)),
    [generate_mock_data(ItemSchema) || _ <- lists:seq(1, NumItems)];

generate_mock_for_type(<<"object">>, Schema) ->
    Properties = maps:get(<<"properties">>, Schema, #{}),
    Required = maps:get(<<"required">>, Schema, maps:keys(Properties)),
    
    maps:fold(fun(PropName, PropSchema, Acc) ->
        case lists:member(PropName, Required) of
            true ->
                maps:put(PropName, generate_mock_data(PropSchema), Acc);
            false ->
                %% Include optional fields 50% of the time
                case rand:uniform(2) of
                    1 -> maps:put(PropName, generate_mock_data(PropSchema), Acc);
                    2 -> Acc
                end
        end
    end, #{}, Properties);

generate_mock_for_type(_, _) ->
    null.

format_response_body(<<"application/json">>, Data) ->
    jsx:encode(Data);
format_response_body(<<"application/xml">>, Data) ->
    %% Simple XML generation
    iolist_to_binary(data_to_xml(Data));
format_response_body(<<"text/plain">>, Data) when is_binary(Data) ->
    Data;
format_response_body(<<"text/plain">>, Data) ->
    iolist_to_binary(io_lib:format("~p", [Data]));
format_response_body(_, Data) when is_binary(Data) ->
    Data;
format_response_body(_, Data) ->
    jsx:encode(Data).

data_to_xml(Data) when is_map(Data) ->
    maps:fold(fun(K, V, Acc) ->
        Tag = binary_to_list(K),
        [Acc, "<", Tag, ">", data_to_xml(V), "</", Tag, ">"]
    end, [], Data);
data_to_xml(Data) when is_list(Data) ->
    [["<item>", data_to_xml(Item), "</item>"] || Item <- Data];
data_to_xml(Data) when is_binary(Data) ->
    Data;
data_to_xml(Data) ->
    io_lib:format("~p", [Data]).

format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).