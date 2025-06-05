%%%-------------------------------------------------------------------
%%% @doc OpenAPI Request/Response Validator
%%% Validates requests and responses against OpenAPI schemas
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_validator).
-behaviour(gen_server).

-export([start_link/0, validate_request/2, validate_response/3, set_validation_enabled/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    validation_enabled = true :: boolean(),
    schema_cache = #{} :: #{binary() => term()}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec validate_request(cowboy_req:req(), map()) -> {ok, cowboy_req:req()} | {error, [map()]}.
validate_request(Req, Operation) ->
    gen_server:call(?MODULE, {validate_request, Req, Operation}).

-spec validate_response(integer(), map(), map()) -> ok | {error, [map()]}.
validate_response(StatusCode, Headers, Body) ->
    gen_server:call(?MODULE, {validate_response, StatusCode, Headers, Body}).

-spec set_validation_enabled(boolean()) -> ok.
set_validation_enabled(Enabled) ->
    gen_server:cast(?MODULE, {set_validation_enabled, Enabled}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Enabled = application:get_env(openapi_scaffold, validation_enabled, true),
    {ok, #state{validation_enabled = Enabled}}.

handle_call({validate_request, Req, Operation}, _From, 
            #state{validation_enabled = false} = State) ->
    {reply, {ok, Req}, State};

handle_call({validate_request, Req, Operation}, _From, State) ->
    Result = do_validate_request(Req, Operation, State),
    {reply, Result, State};

handle_call({validate_response, StatusCode, Headers, Body}, _From,
            #state{validation_enabled = false} = State) ->
    {reply, ok, State};

handle_call({validate_response, StatusCode, Headers, Body}, _From, State) ->
    Result = do_validate_response(StatusCode, Headers, Body, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({set_validation_enabled, Enabled}, State) ->
    {noreply, State#state{validation_enabled = Enabled}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

do_validate_request(Req0, Operation, State) ->
    Errors = [],
    
    %% Validate parameters
    {Req1, ParamErrors} = validate_parameters(Req0, Operation),
    Errors1 = Errors ++ ParamErrors,
    
    %% Validate request body
    {Req2, BodyErrors} = validate_request_body(Req1, Operation),
    Errors2 = Errors1 ++ BodyErrors,
    
    %% Validate headers
    {Req3, HeaderErrors} = validate_headers(Req2, Operation),
    Errors3 = Errors2 ++ HeaderErrors,
    
    case Errors3 of
        [] -> {ok, Req3};
        _ -> {error, Errors3}
    end.

validate_parameters(Req, Operation) ->
    Parameters = maps:get(<<"parameters">>, Operation, []),
    
    {FinalReq, Errors} = lists:foldl(fun(Param, {ReqAcc, ErrorsAcc}) ->
        In = maps:get(<<"in">>, Param),
        Name = maps:get(<<"name">>, Param),
        Required = maps:get(<<"required">>, Param, false),
        Schema = maps:get(<<"schema">>, Param, #{}),
        
        case validate_parameter(In, Name, Required, Schema, ReqAcc) of
            {ok, NewReq} ->
                {NewReq, ErrorsAcc};
            {error, Error} ->
                {ReqAcc, [Error | ErrorsAcc]}
        end
    end, {Req, []}, Parameters),
    
    {FinalReq, lists:reverse(Errors)}.

validate_parameter(<<"path">>, Name, Required, Schema, Req) ->
    %% Path parameters are extracted by the router
    Bindings = cowboy_req:bindings(Req),
    case maps:find(Name, Bindings) of
        {ok, Value} ->
            case validate_value(Value, Schema) of
                ok -> {ok, Req};
                {error, Reason} ->
                    {error, #{
                        <<"in">> => <<"path">>,
                        <<"name">> => Name,
                        <<"reason">> => Reason
                    }}
            end;
        error when Required ->
            {error, #{
                <<"in">> => <<"path">>,
                <<"name">> => Name,
                <<"reason">> => <<"Required parameter missing">>
            }};
        error ->
            {ok, Req}
    end;

validate_parameter(<<"query">>, Name, Required, Schema, Req) ->
    QsVals = cowboy_req:parse_qs(Req),
    case lists:keyfind(Name, 1, QsVals) of
        {Name, Value} ->
            case validate_value(Value, Schema) of
                ok -> {ok, Req};
                {error, Reason} ->
                    {error, #{
                        <<"in">> => <<"query">>,
                        <<"name">> => Name,
                        <<"reason">> => Reason
                    }}
            end;
        false when Required ->
            {error, #{
                <<"in">> => <<"query">>,
                <<"name">> => Name,
                <<"reason">> => <<"Required parameter missing">>
            }};
        false ->
            {ok, Req}
    end;

validate_parameter(<<"header">>, Name, Required, Schema, Req) ->
    Headers = cowboy_req:headers(Req),
    HeaderName = string:lowercase(Name),
    case maps:find(HeaderName, Headers) of
        {ok, Value} ->
            case validate_value(Value, Schema) of
                ok -> {ok, Req};
                {error, Reason} ->
                    {error, #{
                        <<"in">> => <<"header">>,
                        <<"name">> => Name,
                        <<"reason">> => Reason
                    }}
            end;
        error when Required ->
            {error, #{
                <<"in">> => <<"header">>,
                <<"name">> => Name,
                <<"reason">> => <<"Required parameter missing">>
            }};
        error ->
            {ok, Req}
    end;

validate_parameter(<<"cookie">>, Name, Required, Schema, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(Name, 1, Cookies) of
        {Name, Value} ->
            case validate_value(Value, Schema) of
                ok -> {ok, Req};
                {error, Reason} ->
                    {error, #{
                        <<"in">> => <<"cookie">>,
                        <<"name">> => Name,
                        <<"reason">> => Reason
                    }}
            end;
        false when Required ->
            {error, #{
                <<"in">> => <<"cookie">>,
                <<"name">> => Name,
                <<"reason">> => <<"Required parameter missing">>
            }};
        false ->
            {ok, Req}
    end.

validate_request_body(Req0, Operation) ->
    case maps:find(<<"requestBody">>, Operation) of
        {ok, RequestBody} ->
            Required = maps:get(<<"required">>, RequestBody, false),
            Content = maps:get(<<"content">>, RequestBody, #{}),
            
            %% Read the body
            {ok, Body, Req1} = read_body(Req0),
            
            case Body of
                <<>> when Required ->
                    {Req1, [#{<<"reason">> => <<"Request body is required">>}]};
                <<>> ->
                    {Req1, []};
                _ ->
                    %% Get content type
                    ContentType = cowboy_req:header(<<"content-type">>, Req1, <<"application/json">>),
                    MediaType = parse_media_type(ContentType),
                    
                    case maps:find(MediaType, Content) of
                        {ok, MediaTypeObj} ->
                            Schema = maps:get(<<"schema">>, MediaTypeObj, #{}),
                            case validate_json_body(Body, Schema) of
                                ok ->
                                    {Req1, []};
                                {error, Errors} ->
                                    {Req1, Errors}
                            end;
                        error ->
                            {Req1, [#{<<"reason">> => <<"Unsupported content type">>}]}
                    end
            end;
        error ->
            {Req0, []}
    end.

validate_headers(Req, Operation) ->
    %% Headers are validated as parameters
    {Req, []}.

validate_json_body(Body, Schema) ->
    try
        Json = jsx:decode(Body, [return_maps]),
        validate_json_schema(Json, Schema)
    catch
        error:_ ->
            {error, [#{<<"reason">> => <<"Invalid JSON">>}]}
    end.

validate_value(Value, Schema) ->
    Type = maps:get(<<"type">>, Schema, <<"string">>),
    validate_type(Value, Type, Schema).

validate_type(Value, <<"string">>, Schema) when is_binary(Value) ->
    validate_string_constraints(Value, Schema);
validate_type(Value, <<"string">>, _Schema) ->
    {error, <<"Expected string">>};

validate_type(Value, <<"integer">>, Schema) ->
    try
        IntVal = binary_to_integer(Value),
        validate_number_constraints(IntVal, Schema)
    catch
        error:_ ->
            {error, <<"Expected integer">>}
    end;

validate_type(Value, <<"number">>, Schema) ->
    try
        NumVal = case binary:match(Value, <<".">>) of
            nomatch -> binary_to_integer(Value);
            _ -> binary_to_float(Value)
        end,
        validate_number_constraints(NumVal, Schema)
    catch
        error:_ ->
            {error, <<"Expected number">>}
    end;

validate_type(<<"true">>, <<"boolean">>, _Schema) -> ok;
validate_type(<<"false">>, <<"boolean">>, _Schema) -> ok;
validate_type(true, <<"boolean">>, _Schema) -> ok;
validate_type(false, <<"boolean">>, _Schema) -> ok;
validate_type(_Value, <<"boolean">>, _Schema) ->
    {error, <<"Expected boolean">>};

validate_type(Value, <<"array">>, Schema) when is_list(Value) ->
    ItemSchema = maps:get(<<"items">>, Schema, #{}),
    validate_array_items(Value, ItemSchema, Schema);
validate_type(_Value, <<"array">>, _Schema) ->
    {error, <<"Expected array">>};

validate_type(Value, <<"object">>, Schema) when is_map(Value) ->
    validate_object_properties(Value, Schema);
validate_type(_Value, <<"object">>, _Schema) ->
    {error, <<"Expected object">>}.

validate_string_constraints(Value, Schema) ->
    Checks = [
        {<<"minLength">>, fun(Min) -> byte_size(Value) >= Min end},
        {<<"maxLength">>, fun(Max) -> byte_size(Value) =< Max end},
        {<<"pattern">>, fun(Pattern) -> 
            case re:run(Value, Pattern) of
                {match, _} -> true;
                nomatch -> false
            end
        end},
        {<<"enum">>, fun(Enum) -> lists:member(Value, Enum) end}
    ],
    
    check_constraints(Value, Schema, Checks).

validate_number_constraints(Value, Schema) ->
    Checks = [
        {<<"minimum">>, fun(Min) -> Value >= Min end},
        {<<"maximum">>, fun(Max) -> Value =< Max end},
        {<<"exclusiveMinimum">>, fun(Min) -> Value > Min end},
        {<<"exclusiveMaximum">>, fun(Max) -> Value < Max end},
        {<<"multipleOf">>, fun(Mult) -> 
            (Value rem Mult) =:= 0
        end}
    ],
    
    check_constraints(Value, Schema, Checks).

validate_array_items([], _ItemSchema, Schema) ->
    case maps:get(<<"minItems">>, Schema, 0) of
        0 -> ok;
        _ -> {error, <<"Array has too few items">>}
    end;
validate_array_items(Items, ItemSchema, Schema) ->
    %% Check array constraints
    MinItems = maps:get(<<"minItems">>, Schema, 0),
    MaxItems = maps:get(<<"maxItems">>, Schema, undefined),
    
    Length = length(Items),
    if
        Length < MinItems ->
            {error, <<"Array has too few items">>};
        MaxItems =/= undefined, Length > MaxItems ->
            {error, <<"Array has too many items">>};
        true ->
            %% Validate each item
            case validate_all_items(Items, ItemSchema) of
                ok -> ok;
                {error, _} = Error -> Error
            end
    end.

validate_all_items([], _Schema) ->
    ok;
validate_all_items([Item | Rest], Schema) ->
    case validate_json_schema(Item, Schema) of
        ok -> validate_all_items(Rest, Schema);
        {error, _} = Error -> Error
    end.

validate_object_properties(Object, Schema) ->
    Properties = maps:get(<<"properties">>, Schema, #{}),
    Required = maps:get(<<"required">>, Schema, []),
    AdditionalProperties = maps:get(<<"additionalProperties">>, Schema, true),
    
    %% Check required properties
    MissingRequired = [Prop || Prop <- Required, not maps:is_key(Prop, Object)],
    case MissingRequired of
        [] ->
            %% Validate each property
            validate_object_props(maps:to_list(Object), Properties, AdditionalProperties);
        _ ->
            {error, [#{<<"reason">> => <<"Missing required properties">>,
                      <<"properties">> => MissingRequired}]}
    end.

validate_object_props([], _Properties, _AdditionalProperties) ->
    ok;
validate_object_props([{Key, Value} | Rest], Properties, AdditionalProperties) ->
    case maps:find(Key, Properties) of
        {ok, PropSchema} ->
            case validate_json_schema(Value, PropSchema) of
                ok ->
                    validate_object_props(Rest, Properties, AdditionalProperties);
                {error, Reason} ->
                    {error, [#{<<"property">> => Key, <<"reason">> => Reason}]}
            end;
        error when AdditionalProperties =:= false ->
            {error, [#{<<"reason">> => <<"Additional properties not allowed">>,
                      <<"property">> => Key}]};
        error when is_map(AdditionalProperties) ->
            %% Validate against additionalProperties schema
            case validate_json_schema(Value, AdditionalProperties) of
                ok ->
                    validate_object_props(Rest, Properties, AdditionalProperties);
                {error, Reason} ->
                    {error, [#{<<"property">> => Key, <<"reason">> => Reason}]}
            end;
        error ->
            %% Additional properties allowed
            validate_object_props(Rest, Properties, AdditionalProperties)
    end.

validate_json_schema(Value, Schema) ->
    case maps:find(<<"type">>, Schema) of
        {ok, Type} ->
            validate_type(Value, Type, Schema);
        error ->
            %% No type specified, check for other validation keywords
            ok
    end.

check_constraints(_Value, _Schema, []) ->
    ok;
check_constraints(Value, Schema, [{Key, CheckFun} | Rest]) ->
    case maps:find(Key, Schema) of
        {ok, Constraint} ->
            case CheckFun(Constraint) of
                true ->
                    check_constraints(Value, Schema, Rest);
                false ->
                    {error, iolist_to_binary([<<"Constraint failed: ">>, Key])}
            end;
        error ->
            check_constraints(Value, Schema, Rest)
    end.

do_validate_response(StatusCode, Headers, Body, State) ->
    %% TODO: Implement response validation
    ok.

read_body(Req0) ->
    read_body(Req0, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.

parse_media_type(ContentType) ->
    case binary:split(ContentType, <<";">>) of
        [MediaType, _] -> MediaType;
        [MediaType] -> MediaType
    end.