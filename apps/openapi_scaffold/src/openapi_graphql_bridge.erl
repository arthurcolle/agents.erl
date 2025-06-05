%%%-------------------------------------------------------------------
%%% @doc GraphQL Gateway for REST APIs
%%% Automatically creates a GraphQL interface for any OpenAPI spec
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_graphql_bridge).
-behaviour(gen_server).

-export([
    start_link/0,
    register_api/2,
    execute_query/3,
    execute_mutation/3,
    execute_subscription/3,
    get_schema/1,
    introspect/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    apis :: map(),
    schemas :: map(),
    resolvers :: map(),
    subscriptions :: map()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register OpenAPI spec as GraphQL API
register_api(Name, OpenAPISpec) ->
    gen_server:call(?MODULE, {register_api, Name, OpenAPISpec}).

%% @doc Execute GraphQL query
execute_query(ApiName, Query, Variables) ->
    gen_server:call(?MODULE, {execute_query, ApiName, Query, Variables}, 30000).

%% @doc Execute GraphQL mutation
execute_mutation(ApiName, Mutation, Variables) ->
    gen_server:call(?MODULE, {execute_mutation, ApiName, Mutation, Variables}, 30000).

%% @doc Execute GraphQL subscription
execute_subscription(ApiName, Subscription, Variables) ->
    gen_server:call(?MODULE, {execute_subscription, ApiName, Subscription, Variables}).

%% @doc Get GraphQL schema for API
get_schema(ApiName) ->
    gen_server:call(?MODULE, {get_schema, ApiName}).

%% @doc Introspect GraphQL schema
introspect(ApiName) ->
    gen_server:call(?MODULE, {introspect, ApiName}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    State = #state{
        apis = #{},
        schemas = #{},
        resolvers = #{},
        subscriptions = #{}
    },
    {ok, State}.

handle_call({register_api, Name, OpenAPISpec}, _From, State) ->
    %% Convert OpenAPI to GraphQL schema
    {Schema, Resolvers} = convert_to_graphql(OpenAPISpec),
    
    %% Store API info
    NewApis = maps:put(Name, OpenAPISpec, State#state.apis),
    NewSchemas = maps:put(Name, Schema, State#state.schemas),
    NewResolvers = maps:put(Name, Resolvers, State#state.resolvers),
    
    NewState = State#state{
        apis = NewApis,
        schemas = NewSchemas,
        resolvers = NewResolvers
    },
    
    {reply, ok, NewState};

handle_call({execute_query, ApiName, Query, Variables}, _From, State) ->
    Result = execute_graphql_query(ApiName, Query, Variables, State),
    {reply, Result, State};

handle_call({execute_mutation, ApiName, Mutation, Variables}, _From, State) ->
    Result = execute_graphql_mutation(ApiName, Mutation, Variables, State),
    {reply, Result, State};

handle_call({execute_subscription, ApiName, Subscription, Variables}, From, State) ->
    {SubscriptionId, NewState} = setup_subscription(ApiName, Subscription, Variables, From, State),
    {reply, {ok, SubscriptionId}, NewState};

handle_call({get_schema, ApiName}, _From, State) ->
    Schema = maps:get(ApiName, State#state.schemas, undefined),
    {reply, {ok, Schema}, State};

handle_call({introspect, ApiName}, _From, State) ->
    Result = perform_introspection(ApiName, State),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({subscription_update, SubscriptionId, Data}, State) ->
    %% Forward subscription update to client
    case find_subscription(SubscriptionId, State) of
        {ok, {From, _}} ->
            gen_server:reply(From, {data, Data});
        error ->
            ok
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - OpenAPI to GraphQL Conversion
%%====================================================================

convert_to_graphql(OpenAPISpec) ->
    %% Extract components
    Paths = maps:get(<<"paths">>, OpenAPISpec, #{}),
    Components = maps:get(<<"components">>, OpenAPISpec, #{}),
    Schemas = maps:get(<<"schemas">>, Components, #{}),
    
    %% Convert schemas to GraphQL types
    GraphQLTypes = convert_schemas_to_types(Schemas),
    
    %% Generate Query type from GET endpoints
    {QueryFields, QueryResolvers} = generate_query_type(Paths),
    
    %% Generate Mutation type from POST/PUT/DELETE endpoints  
    {MutationFields, MutationResolvers} = generate_mutation_type(Paths),
    
    %% Generate Subscription type for real-time endpoints
    {SubscriptionFields, SubscriptionResolvers} = generate_subscription_type(Paths),
    
    %% Build complete schema
    Schema = #{
        types => GraphQLTypes,
        query => #{
            name => <<"Query">>,
            fields => QueryFields
        },
        mutation => #{
            name => <<"Mutation">>,
            fields => MutationFields
        },
        subscription => #{
            name => <<"Subscription">>,
            fields => SubscriptionFields
        }
    },
    
    %% Combine resolvers
    Resolvers = #{
        query => QueryResolvers,
        mutation => MutationResolvers,
        subscription => SubscriptionResolvers
    },
    
    {Schema, Resolvers}.

convert_schemas_to_types(Schemas) ->
    maps:map(fun(Name, Schema) ->
        convert_schema_to_type(Name, Schema)
    end, Schemas).

convert_schema_to_type(Name, Schema) ->
    Properties = maps:get(<<"properties">>, Schema, #{}),
    Required = maps:get(<<"required">>, Schema, []),
    
    Fields = maps:map(fun(FieldName, FieldSchema) ->
        IsRequired = lists:member(FieldName, Required),
        #{
            type => convert_json_type_to_graphql(FieldSchema),
            required => IsRequired,
            description => maps:get(<<"description">>, FieldSchema, <<>>)
        }
    end, Properties),
    
    #{
        name => Name,
        kind => <<"OBJECT">>,
        fields => Fields,
        description => maps:get(<<"description">>, Schema, <<>>)
    }.

convert_json_type_to_graphql(#{<<"type">> := <<"string">>}) ->
    <<"String">>;
convert_json_type_to_graphql(#{<<"type">> := <<"integer">>}) ->
    <<"Int">>;
convert_json_type_to_graphql(#{<<"type">> := <<"number">>}) ->
    <<"Float">>;
convert_json_type_to_graphql(#{<<"type">> := <<"boolean">>}) ->
    <<"Boolean">>;
convert_json_type_to_graphql(#{<<"type">> := <<"array">>, <<"items">> := Items}) ->
    ItemType = convert_json_type_to_graphql(Items),
    <<"[", ItemType/binary, "]">>;
convert_json_type_to_graphql(#{<<"$ref">> := Ref}) ->
    %% Extract type name from reference
    extract_type_name(Ref);
convert_json_type_to_graphql(_) ->
    <<"String">>.

extract_type_name(Ref) ->
    Parts = binary:split(Ref, <<"/">>, [global]),
    lists:last(Parts).

%%====================================================================
%% Internal functions - Query Generation
%%====================================================================

generate_query_type(Paths) ->
    %% Generate GraphQL queries from GET endpoints
    {Fields, Resolvers} = maps:fold(fun(Path, PathItem, {FieldsAcc, ResolversAcc}) ->
        case maps:get(<<"get">>, PathItem, undefined) of
            undefined -> {FieldsAcc, ResolversAcc};
            Operation ->
                {Field, Resolver} = generate_query_field(Path, Operation),
                FieldName = maps:get(name, Field),
                {
                    maps:put(FieldName, Field, FieldsAcc),
                    maps:put(FieldName, Resolver, ResolversAcc)
                }
        end
    end, {#{}, #{}}, Paths),
    
    {Fields, Resolvers}.

generate_query_field(Path, Operation) ->
    %% Generate GraphQL field from GET operation
    FieldName = generate_field_name(Path, <<"get">>),
    
    %% Extract parameters
    Parameters = maps:get(<<"parameters">>, Operation, []),
    Args = convert_parameters_to_args(Parameters),
    
    %% Determine return type
    ReturnType = determine_return_type(Operation),
    
    Field = #{
        name => FieldName,
        type => ReturnType,
        args => Args,
        description => maps:get(<<"summary">>, Operation, <<>>)
    },
    
    %% Create resolver
    Resolver = create_query_resolver(Path, Operation),
    
    {Field, Resolver}.

generate_field_name(Path, Method) ->
    %% Convert path to GraphQL field name
    Parts = binary:split(Path, <<"/">>, [global]),
    CleanParts = [clean_path_part(P) || P <- Parts, P =/= <<>>],
    
    %% Prefix with method for uniqueness
    Prefix = case Method of
        <<"get">> -> <<"">>;
        <<"post">> -> <<"create">>;
        <<"put">> -> <<"update">>;
        <<"delete">> -> <<"delete">>;
        _ -> Method
    end,
    
    BaseName = iolist_to_binary([capitalize(P) || P <- CleanParts]),
    case Prefix of
        <<>> -> lcfirst(BaseName);
        _ -> lcfirst(<<Prefix/binary, BaseName/binary>>)
    end.

clean_path_part(Part) ->
    %% Remove path parameters
    case binary:match(Part, <<"{">>) of
        nomatch -> Part;
        _ -> 
            %% Extract parameter name
            [_, ParamPart] = binary:split(Part, <<"{">>),
            [ParamName, _] = binary:split(ParamPart, <<"}">>),
            <<"By", (capitalize(ParamName))/binary>>
    end.

capitalize(<<First:8, Rest/binary>>) ->
    <<(string:to_upper(First)):8, Rest/binary>>;
capitalize(<<>>) ->
    <<>>.

lcfirst(<<First:8, Rest/binary>>) ->
    <<(string:to_lower(First)):8, Rest/binary>>;
lcfirst(<<>>) ->
    <<>>.

convert_parameters_to_args(Parameters) ->
    %% Convert OpenAPI parameters to GraphQL arguments
    maps:from_list(lists:map(fun(Param) ->
        Name = maps:get(<<"name">>, Param),
        Schema = maps:get(<<"schema">>, Param, #{}),
        Required = maps:get(<<"required">>, Param, false),
        
        ArgDef = #{
            type => convert_json_type_to_graphql(Schema),
            required => Required,
            description => maps:get(<<"description">>, Param, <<>>)
        },
        
        {Name, ArgDef}
    end, Parameters)).

determine_return_type(Operation) ->
    %% Determine GraphQL return type from operation responses
    Responses = maps:get(<<"responses">>, Operation, #{}),
    
    %% Look for successful response
    SuccessResponse = find_success_response(Responses),
    
    case SuccessResponse of
        undefined -> <<"String">>;
        Response ->
            %% Extract type from response schema
            extract_response_type(Response)
    end.

find_success_response(Responses) ->
    %% Find 200/201/204 response
    SuccessCodes = [<<"200">>, <<"201">>, <<"204">>],
    lists:foldl(fun(Code, Acc) ->
        case Acc of
            undefined -> maps:get(Code, Responses, undefined);
            _ -> Acc
        end
    end, undefined, SuccessCodes).

extract_response_type(Response) ->
    Content = maps:get(<<"content">>, Response, #{}),
    
    case maps:get(<<"application/json">>, Content, undefined) of
        undefined -> <<"String">>;
        MediaType ->
            case maps:get(<<"schema">>, MediaType, undefined) of
                undefined -> <<"String">>;
                Schema -> convert_json_type_to_graphql(Schema)
            end
    end.

create_query_resolver(Path, Operation) ->
    %% Create resolver function for query
    fun(Parent, Args, Context) ->
        execute_rest_query(Path, Operation, Args, Context)
    end.

%%====================================================================
%% Internal functions - Mutation Generation
%%====================================================================

generate_mutation_type(Paths) ->
    Methods = [<<"post">>, <<"put">>, <<"patch">>, <<"delete">>],
    
    {Fields, Resolvers} = maps:fold(fun(Path, PathItem, Acc) ->
        lists:foldl(fun(Method, {FieldsAcc, ResolversAcc}) ->
            case maps:get(Method, PathItem, undefined) of
                undefined -> {FieldsAcc, ResolversAcc};
                Operation ->
                    {Field, Resolver} = generate_mutation_field(Path, Method, Operation),
                    FieldName = maps:get(name, Field),
                    {
                        maps:put(FieldName, Field, FieldsAcc),
                        maps:put(FieldName, Resolver, ResolversAcc)
                    }
            end
        end, Acc, Methods)
    end, {#{}, #{}}, Paths),
    
    {Fields, Resolvers}.

generate_mutation_field(Path, Method, Operation) ->
    %% Generate GraphQL mutation field
    FieldName = generate_field_name(Path, Method),
    
    %% Extract parameters and request body
    Parameters = maps:get(<<"parameters">>, Operation, []),
    RequestBody = maps:get(<<"requestBody">>, Operation, #{}),
    
    %% Convert to GraphQL arguments
    Args = generate_mutation_args(Parameters, RequestBody),
    
    %% Determine return type
    ReturnType = determine_return_type(Operation),
    
    Field = #{
        name => FieldName,
        type => ReturnType,
        args => Args,
        description => maps:get(<<"summary">>, Operation, <<>>)
    },
    
    %% Create resolver
    Resolver = create_mutation_resolver(Path, Method, Operation),
    
    {Field, Resolver}.

generate_mutation_args(Parameters, RequestBody) ->
    %% Combine parameters and request body into arguments
    ParamArgs = convert_parameters_to_args(Parameters),
    
    %% Add input argument for request body
    BodyArgs = case RequestBody of
        #{<<"content">> := Content} ->
            case maps:get(<<"application/json">>, Content, undefined) of
                undefined -> #{};
                MediaType ->
                    case maps:get(<<"schema">>, MediaType, undefined) of
                        undefined -> #{};
                        Schema ->
                            InputType = convert_json_type_to_graphql(Schema),
                            #{
                                <<"input">> => #{
                                    type => InputType,
                                    required => maps:get(<<"required">>, RequestBody, false),
                                    description => <<"Input data">>
                                }
                            }
                    end
            end;
        _ -> #{}
    end,
    
    maps:merge(ParamArgs, BodyArgs).

create_mutation_resolver(Path, Method, Operation) ->
    %% Create resolver function for mutation
    fun(Parent, Args, Context) ->
        execute_rest_mutation(Path, Method, Operation, Args, Context)
    end.

%%====================================================================
%% Internal functions - Subscription Generation
%%====================================================================

generate_subscription_type(Paths) ->
    %% Generate subscriptions for endpoints that support WebSocket or SSE
    {Fields, Resolvers} = maps:fold(fun(Path, PathItem, {FieldsAcc, ResolversAcc}) ->
        %% Check for x-subscription extension
        case maps:get(<<"x-subscription">>, PathItem, undefined) of
            undefined -> {FieldsAcc, ResolversAcc};
            SubscriptionConfig ->
                {Field, Resolver} = generate_subscription_field(Path, SubscriptionConfig),
                FieldName = maps:get(name, Field),
                {
                    maps:put(FieldName, Field, FieldsAcc),
                    maps:put(FieldName, Resolver, ResolversAcc)
                }
        end
    end, {#{}, #{}}, Paths),
    
    {Fields, Resolvers}.

generate_subscription_field(Path, Config) ->
    %% Generate GraphQL subscription field
    FieldName = generate_subscription_name(Path),
    
    %% Extract parameters
    Parameters = maps:get(<<"parameters">>, Config, []),
    Args = convert_parameters_to_args(Parameters),
    
    %% Determine return type
    ReturnType = maps:get(<<"type">>, Config, <<"String">>),
    
    Field = #{
        name => FieldName,
        type => ReturnType,
        args => Args,
        description => maps:get(<<"description">>, Config, <<>>)
    },
    
    %% Create resolver
    Resolver = create_subscription_resolver(Path, Config),
    
    {Field, Resolver}.

generate_subscription_name(Path) ->
    Parts = binary:split(Path, <<"/">>, [global]),
    CleanParts = [clean_path_part(P) || P <- Parts, P =/= <<>>],
    BaseName = iolist_to_binary([capitalize(P) || P <- CleanParts]),
    lcfirst(<<BaseName/binary, "Updates">>).

create_subscription_resolver(Path, Config) ->
    %% Create resolver function for subscription
    fun(Parent, Args, Context) ->
        setup_rest_subscription(Path, Config, Args, Context)
    end.

%%====================================================================
%% Internal functions - Query Execution
%%====================================================================

execute_graphql_query(ApiName, Query, Variables, State) ->
    case maps:get(ApiName, State#state.schemas, undefined) of
        undefined ->
            {error, api_not_found};
        Schema ->
            %% Parse GraphQL query
            case parse_graphql(Query) of
                {ok, ParsedQuery} ->
                    %% Get resolvers
                    Resolvers = maps:get(ApiName, State#state.resolvers),
                    
                    %% Execute query
                    execute_parsed_query(ParsedQuery, Variables, Schema, Resolvers);
                {error, ParseError} ->
                    {error, ParseError}
            end
    end.

execute_graphql_mutation(ApiName, Mutation, Variables, State) ->
    case maps:get(ApiName, State#state.schemas, undefined) of
        undefined ->
            {error, api_not_found};
        Schema ->
            %% Parse GraphQL mutation
            case parse_graphql(Mutation) of
                {ok, ParsedMutation} ->
                    %% Get resolvers
                    Resolvers = maps:get(ApiName, State#state.resolvers),
                    
                    %% Execute mutation
                    execute_parsed_mutation(ParsedMutation, Variables, Schema, Resolvers);
                {error, ParseError} ->
                    {error, ParseError}
            end
    end.

parse_graphql(Query) ->
    %% Simple GraphQL parser
    %% In production, would use a proper GraphQL parser
    try
        %% Extract operation type and selections
        {ok, #{
            operation => <<"query">>,
            selections => parse_selections(Query)
        }}
    catch
        _:_ -> {error, <<"Invalid GraphQL syntax">>}
    end.

parse_selections(Query) ->
    %% Extract field selections from query
    %% Simplified implementation
    [].

execute_parsed_query(ParsedQuery, Variables, Schema, Resolvers) ->
    %% Execute parsed GraphQL query
    Context = #{
        schema => Schema,
        variables => Variables
    },
    
    %% Get query resolvers
    QueryResolvers = maps:get(query, Resolvers),
    
    %% Execute selections
    Selections = maps:get(selections, ParsedQuery),
    Results = execute_selections(Selections, QueryResolvers, Context),
    
    {ok, #{
        <<"data">> => Results
    }}.

execute_parsed_mutation(ParsedMutation, Variables, Schema, Resolvers) ->
    %% Execute parsed GraphQL mutation
    Context = #{
        schema => Schema,
        variables => Variables
    },
    
    %% Get mutation resolvers
    MutationResolvers = maps:get(mutation, Resolvers),
    
    %% Execute selections
    Selections = maps:get(selections, ParsedMutation),
    Results = execute_selections(Selections, MutationResolvers, Context),
    
    {ok, #{
        <<"data">> => Results
    }}.

execute_selections(Selections, Resolvers, Context) ->
    %% Execute field selections
    maps:from_list(lists:map(fun(Selection) ->
        FieldName = maps:get(name, Selection),
        Args = resolve_arguments(maps:get(args, Selection, #{}), Context),
        
        %% Get resolver
        case maps:get(FieldName, Resolvers, undefined) of
            undefined ->
                {FieldName, null};
            Resolver ->
                %% Execute resolver
                Result = Resolver(undefined, Args, Context),
                {FieldName, Result}
        end
    end, Selections)).

resolve_arguments(Args, #{variables := Variables}) ->
    %% Resolve variable references in arguments
    maps:map(fun(_Name, Value) ->
        case Value of
            <<"$", VarName/binary>> ->
                maps:get(VarName, Variables, null);
            _ ->
                Value
        end
    end, Args).

%%====================================================================
%% Internal functions - REST Execution
%%====================================================================

execute_rest_query(Path, Operation, Args, Context) ->
    %% Execute REST GET request
    %% Build URL with parameters
    Url = build_rest_url(Path, Args),
    Headers = extract_headers(Args),
    
    %% Make HTTP request
    case hackney:get(Url, Headers, <<>>, []) of
        {ok, 200, _RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            jsx:decode(Body, [return_maps]);
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {error, #{
                <<"statusCode">> => StatusCode,
                <<"body">> => Body
            }};
        {error, Reason} ->
            {error, Reason}
    end.

execute_rest_mutation(Path, Method, Operation, Args, Context) ->
    %% Execute REST POST/PUT/DELETE request
    Url = build_rest_url(Path, Args),
    Headers = extract_headers(Args),
    Body = maps:get(<<"input">>, Args, #{}),
    
    %% Make HTTP request
    MethodAtom = binary_to_atom(Method, utf8),
    JsonBody = jsx:encode(Body),
    
    case hackney:request(MethodAtom, Url, Headers, JsonBody, []) of
        {ok, StatusCode, _RespHeaders, ClientRef} when StatusCode >= 200, StatusCode < 300 ->
            {ok, RespBody} = hackney:body(ClientRef),
            case RespBody of
                <<>> -> #{<<"success">> => true};
                _ -> jsx:decode(RespBody, [return_maps])
            end;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {error, #{
                <<"statusCode">> => StatusCode,
                <<"body">> => RespBody
            }};
        {error, Reason} ->
            {error, Reason}
    end.

build_rest_url(PathTemplate, Args) ->
    %% Build URL from path template and arguments
    BaseUrl = maps:get(<<"baseUrl">>, Args, <<"http://localhost:8080">>),
    
    %% Replace path parameters
    Path = replace_path_params(PathTemplate, Args),
    
    %% Add query parameters
    QueryParams = extract_query_params(Args),
    QueryString = build_query_string(QueryParams),
    
    case QueryString of
        <<>> -> <<BaseUrl/binary, Path/binary>>;
        _ -> <<BaseUrl/binary, Path/binary, "?", QueryString/binary>>
    end.

replace_path_params(Path, Args) ->
    %% Replace {param} with actual values
    Parts = binary:split(Path, <<"/">>, [global]),
    ReplacedParts = lists:map(fun(Part) ->
        case binary:match(Part, <<"{">>) of
            nomatch -> Part;
            _ ->
                [_, ParamPart] = binary:split(Part, <<"{">>),
                [ParamName, _] = binary:split(ParamPart, <<"}">>),
                maps:get(ParamName, Args, Part)
        end
    end, Parts),
    
    iolist_to_binary(lists:join(<<"/">>, ReplacedParts)).

extract_headers(Args) ->
    %% Extract header parameters
    %% In production, would filter based on parameter definitions
    [].

extract_query_params(Args) ->
    %% Extract query parameters
    %% In production, would filter based on parameter definitions
    maps:filter(fun(K, _V) ->
        %% Exclude special keys
        not lists:member(K, [<<"baseUrl">>, <<"input">>])
    end, Args).

build_query_string(Params) ->
    Parts = maps:fold(fun(K, V, Acc) ->
        [uri_string:quote(K), "=", uri_string:quote(to_string(V)) | Acc]
    end, [], Params),
    
    case Parts of
        [] -> <<>>;
        _ -> iolist_to_binary(lists:join("&", lists:reverse(Parts)))
    end.

to_string(V) when is_binary(V) -> V;
to_string(V) when is_integer(V) -> integer_to_binary(V);
to_string(V) when is_float(V) -> float_to_binary(V);
to_string(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_string(V) -> iolist_to_binary(io_lib:format("~p", [V])).

%%====================================================================
%% Internal functions - Subscriptions
%%====================================================================

setup_subscription(ApiName, Subscription, Variables, From, State) ->
    %% Generate subscription ID
    SubId = generate_subscription_id(),
    
    %% Parse subscription
    case parse_graphql(Subscription) of
        {ok, ParsedSub} ->
            %% Store subscription info
            SubInfo = #{
                api => ApiName,
                subscription => ParsedSub,
                variables => Variables,
                from => From
            },
            
            NewSubs = maps:put(SubId, SubInfo, State#state.subscriptions),
            
            %% Start subscription handler
            start_subscription_handler(SubId, SubInfo),
            
            {SubId, State#state{subscriptions = NewSubs}};
        {error, _} ->
            {error, State}
    end.

setup_rest_subscription(Path, Config, Args, Context) ->
    %% Set up REST-based subscription (WebSocket or SSE)
    Type = maps:get(<<"type">>, Config, <<"websocket">>),
    
    case Type of
        <<"websocket">> ->
            setup_websocket_subscription(Path, Args, Context);
        <<"sse">> ->
            setup_sse_subscription(Path, Args, Context);
        _ ->
            {error, unsupported_subscription_type}
    end.

setup_websocket_subscription(Path, Args, Context) ->
    %% Connect to WebSocket endpoint
    Url = build_websocket_url(Path, Args),
    
    case gun:open(Url, 443, #{protocols => [http2]}) of
        {ok, ConnPid} ->
            %% Upgrade to WebSocket
            StreamRef = gun:ws_upgrade(ConnPid, Path),
            
            %% Return subscription stream
            {ok, #{
                type => websocket,
                connection => ConnPid,
                stream => StreamRef
            }};
        {error, Reason} ->
            {error, Reason}
    end.

setup_sse_subscription(Path, Args, Context) ->
    %% Connect to SSE endpoint
    Url = build_rest_url(Path, Args),
    
    %% Start SSE client
    {ok, SsePid} = sse_client:start_link(Url),
    
    {ok, #{
        type => sse,
        client => SsePid
    }}.

build_websocket_url(Path, Args) ->
    %% Build WebSocket URL
    BaseUrl = maps:get(<<"wsUrl">>, Args, <<"ws://localhost:8080">>),
    <<BaseUrl/binary, Path/binary>>.

start_subscription_handler(SubId, SubInfo) ->
    %% Start process to handle subscription updates
    spawn_link(fun() -> subscription_handler_loop(SubId, SubInfo) end).

subscription_handler_loop(SubId, SubInfo) ->
    receive
        {ws, {text, Data}} ->
            %% Handle WebSocket message
            Parsed = jsx:decode(Data, [return_maps]),
            self() ! {subscription_update, SubId, Parsed},
            subscription_handler_loop(SubId, SubInfo);
        
        {sse, Event} ->
            %% Handle SSE event
            self() ! {subscription_update, SubId, Event},
            subscription_handler_loop(SubId, SubInfo);
        
        stop ->
            ok
    end.

find_subscription(SubId, State) ->
    case maps:get(SubId, State#state.subscriptions, undefined) of
        undefined -> error;
        SubInfo -> {ok, {maps:get(from, SubInfo), SubInfo}}
    end.

generate_subscription_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

%%====================================================================
%% Internal functions - Introspection
%%====================================================================

perform_introspection(ApiName, State) ->
    case maps:get(ApiName, State#state.schemas, undefined) of
        undefined ->
            {error, api_not_found};
        Schema ->
            %% Build introspection result
            Introspection = #{
                <<"__schema">> => #{
                    <<"types">> => introspect_types(Schema),
                    <<"queryType">> => #{<<"name">> => <<"Query">>},
                    <<"mutationType">> => #{<<"name">> => <<"Mutation">>},
                    <<"subscriptionType">> => #{<<"name">> => <<"Subscription">>},
                    <<"directives">> => []
                }
            },
            {ok, Introspection}
    end.

introspect_types(Schema) ->
    %% Build type introspection
    Types = maps:get(types, Schema, #{}),
    
    %% Add root types
    AllTypes = Types#{
        <<"Query">> => maps:get(query, Schema),
        <<"Mutation">> => maps:get(mutation, Schema),
        <<"Subscription">> => maps:get(subscription, Schema)
    },
    
    %% Convert to introspection format
    maps:fold(fun(Name, Type, Acc) ->
        [introspect_single_type(Name, Type) | Acc]
    end, [], AllTypes).

introspect_single_type(Name, Type) ->
    #{
        <<"name">> => Name,
        <<"kind">> => maps:get(kind, Type, <<"OBJECT">>),
        <<"description">> => maps:get(description, Type, null),
        <<"fields">> => introspect_fields(maps:get(fields, Type, #{})),
        <<"interfaces">> => [],
        <<"possibleTypes">> => null,
        <<"enumValues">> => null,
        <<"inputFields">> => null
    }.

introspect_fields(Fields) ->
    maps:fold(fun(Name, Field, Acc) ->
        [#{
            <<"name">> => Name,
            <<"description">> => maps:get(description, Field, null),
            <<"args">> => introspect_args(maps:get(args, Field, #{})),
            <<"type">> => introspect_type_ref(maps:get(type, Field)),
            <<"isDeprecated">> => false,
            <<"deprecationReason">> => null
        } | Acc]
    end, [], Fields).

introspect_args(Args) ->
    maps:fold(fun(Name, Arg, Acc) ->
        [#{
            <<"name">> => Name,
            <<"description">> => maps:get(description, Arg, null),
            <<"type">> => introspect_type_ref(maps:get(type, Arg)),
            <<"defaultValue">> => null
        } | Acc]
    end, [], Args).

introspect_type_ref(TypeName) when is_binary(TypeName) ->
    %% Parse type reference (handle lists and non-null)
    case TypeName of
        <<"[", Rest/binary>> ->
            %% List type
            [InnerType, _] = binary:split(Rest, <<"]">>),
            #{
                <<"kind">> => <<"LIST">>,
                <<"ofType">> => introspect_type_ref(InnerType)
            };
        _ ->
            %% Named type
            #{
                <<"kind">> => <<"SCALAR">>,
                <<"name">> => TypeName
            }
    end.