%%%-------------------------------------------------------------------
%%% @doc Interactive API Playground
%%% Better than Swagger UI with live editing, WebSocket support,
%%% and advanced testing features.
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_playground).
-behaviour(gen_server).

-export([
    start_link/0,
    register_spec/2,
    get_playground_data/1,
    execute_request/2,
    start_websocket/2,
    generate_example/2,
    validate_request/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Cowboy handlers
-export([init/2, websocket_init/1, websocket_handle/2, 
         websocket_info/2, terminate/3]).

-record(state, {
    specs :: map(),
    websockets :: map(),
    request_history :: list(),
    mock_engine :: pid()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register OpenAPI spec for playground
register_spec(Name, Spec) ->
    gen_server:call(?MODULE, {register_spec, Name, Spec}).

%% @doc Get playground data for spec
get_playground_data(SpecName) ->
    gen_server:call(?MODULE, {get_playground_data, SpecName}).

%% @doc Execute API request from playground
execute_request(SpecName, Request) ->
    gen_server:call(?MODULE, {execute_request, SpecName, Request}, 30000).

%% @doc Start WebSocket connection for real-time updates
start_websocket(SpecName, Options) ->
    gen_server:call(?MODULE, {start_websocket, SpecName, Options}).

%% @doc Generate example request/response
generate_example(SpecName, Operation) ->
    gen_server:call(?MODULE, {generate_example, SpecName, Operation}).

%% @doc Validate request against schema
validate_request(SpecName, Request) ->
    gen_server:call(?MODULE, {validate_request, SpecName, Request}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Start mock engine
    {ok, MockEngine} = openapi_mock_engine:start_link(),
    
    State = #state{
        specs = #{},
        websockets = #{},
        request_history = [],
        mock_engine = MockEngine
    },
    
    {ok, State}.

handle_call({register_spec, Name, Spec}, _From, State) ->
    %% Process and enhance spec for playground
    EnhancedSpec = enhance_spec_for_playground(Spec),
    
    NewSpecs = maps:put(Name, EnhancedSpec, State#state.specs),
    {reply, ok, State#state{specs = NewSpecs}};

handle_call({get_playground_data, SpecName}, _From, State) ->
    case maps:get(SpecName, State#state.specs, undefined) of
        undefined ->
            {reply, {error, spec_not_found}, State};
        Spec ->
            PlaygroundData = generate_playground_data(Spec, State),
            {reply, {ok, PlaygroundData}, State}
    end;

handle_call({execute_request, SpecName, Request}, _From, State) ->
    Result = execute_playground_request(SpecName, Request, State),
    
    %% Store in history
    HistoryEntry = #{
        timestamp => erlang:system_time(second),
        spec => SpecName,
        request => Request,
        result => Result
    },
    NewHistory = [HistoryEntry | lists:sublist(State#state.request_history, 99)],
    
    {reply, Result, State#state{request_history = NewHistory}};

handle_call({start_websocket, SpecName, Options}, _From, State) ->
    %% Generate WebSocket ID
    WsId = generate_ws_id(),
    
    %% Store WebSocket info
    WsInfo = #{
        spec => SpecName,
        options => Options,
        started_at => erlang:system_time(second)
    },
    
    NewWebsockets = maps:put(WsId, WsInfo, State#state.websockets),
    
    {reply, {ok, WsId}, State#state{websockets = NewWebsockets}};

handle_call({generate_example, SpecName, Operation}, _From, State) ->
    case maps:get(SpecName, State#state.specs, undefined) of
        undefined ->
            {reply, {error, spec_not_found}, State};
        Spec ->
            Example = generate_operation_example(Spec, Operation, State#state.mock_engine),
            {reply, {ok, Example}, State}
    end;

handle_call({validate_request, SpecName, Request}, _From, State) ->
    case maps:get(SpecName, State#state.specs, undefined) of
        undefined ->
            {reply, {error, spec_not_found}, State};
        Spec ->
            ValidationResult = validate_against_spec(Request, Spec),
            {reply, ValidationResult, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Cowboy WebSocket callbacks
%%====================================================================

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    %% Set up WebSocket connection
    self() ! {init_connection},
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    %% Handle incoming WebSocket messages
    try
        Data = jsx:decode(Msg, [return_maps]),
        Response = handle_ws_message(Data, State),
        {reply, {text, jsx:encode(Response)}, State}
    catch
        _:_ ->
            Error = #{<<"error">> => <<"Invalid JSON">>},
            {reply, {text, jsx:encode(Error)}, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({init_connection}, State) ->
    %% Send initial connection data
    InitMsg = #{
        <<"type">> => <<"connected">>,
        <<"timestamp">> => erlang:system_time(second)
    },
    {reply, {text, jsx:encode(InitMsg)}, State};

websocket_info({api_update, Update}, State) ->
    %% Forward API updates to WebSocket
    {reply, {text, jsx:encode(Update)}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal functions - Spec Enhancement
%%====================================================================

enhance_spec_for_playground(Spec) ->
    %% Add playground-specific enhancements
    Enhanced = Spec#{
        <<"x-playground">> => #{
            <<"features">> => [
                <<"live-editing">>,
                <<"request-history">>,
                <<"websocket-support">>,
                <<"mock-responses">>,
                <<"example-generation">>,
                <<"validation">>,
                <<"performance-testing">>
            ],
            <<"authentication">> => extract_auth_methods(Spec),
            <<"servers">> => enhance_servers(maps:get(<<"servers">>, Spec, []))
        }
    },
    
    %% Enhance each operation
    Paths = maps:get(<<"paths">>, Enhanced, #{}),
    EnhancedPaths = maps:map(fun(Path, PathItem) ->
        enhance_path_item(Path, PathItem)
    end, Paths),
    
    Enhanced#{<<"paths">> => EnhancedPaths}.

extract_auth_methods(Spec) ->
    %% Extract authentication methods from spec
    Components = maps:get(<<"components">>, Spec, #{}),
    SecuritySchemes = maps:get(<<"securitySchemes">>, Components, #{}),
    
    maps:map(fun(_Name, Scheme) ->
        enhance_auth_scheme(Scheme)
    end, SecuritySchemes).

enhance_auth_scheme(#{<<"type">> := <<"oauth2">>} = Scheme) ->
    %% Add OAuth2 playground features
    Scheme#{
        <<"x-playground">> => #{
            <<"authorizationUrl">> => maps:get(<<"authorizationUrl">>, 
                maps:get(<<"flows">>, Scheme, #{}), <<>>),
            <<"tokenUrl">> => maps:get(<<"tokenUrl">>, 
                maps:get(<<"flows">>, Scheme, #{}), <<>>),
            <<"refreshUrl">> => maps:get(<<"refreshUrl">>, 
                maps:get(<<"flows">>, Scheme, #{}), <<>>)
        }
    };
enhance_auth_scheme(Scheme) ->
    Scheme.

enhance_servers(Servers) ->
    %% Add playground-specific server features
    lists:map(fun(Server) ->
        Server#{
            <<"x-playground">> => #{
                <<"healthCheck">> => true,
                <<"proxy">> => true,
                <<"mock">> => true
            }
        }
    end, Servers).

enhance_path_item(Path, PathItem) ->
    %% Enhance each operation in the path
    maps:map(fun(Method, Operation) when is_map(Operation) ->
        enhance_operation(Path, Method, Operation);
    (Key, Value) -> Value
    end, PathItem).

enhance_operation(Path, Method, Operation) ->
    %% Add playground-specific operation features
    Operation#{
        <<"x-playground">> => #{
            <<"examples">> => generate_auto_examples(Operation),
            <<"testCases">> => generate_test_cases(Path, Method, Operation),
            <<"performance">> => #{
                <<"loadTest">> => true,
                <<"metrics">> => true
            },
            <<"mock">> => #{
                <<"enabled">> => true,
                <<"delay">> => 0,
                <<"statusCode">> => 200
            }
        }
    }.

generate_auto_examples(Operation) ->
    %% Generate examples based on schema
    RequestBody = maps:get(<<"requestBody">>, Operation, #{}),
    Responses = maps:get(<<"responses">>, Operation, #{}),
    
    #{
        <<"request">> => generate_request_example(RequestBody),
        <<"responses">> => maps:map(fun(_StatusCode, Response) ->
            generate_response_example(Response)
        end, Responses)
    }.

generate_request_example(#{<<"content">> := Content}) ->
    maps:map(fun(_MediaType, MediaTypeObj) ->
        case maps:get(<<"schema">>, MediaTypeObj, undefined) of
            undefined -> #{};
            Schema -> generate_example_from_schema(Schema)
        end
    end, Content);
generate_request_example(_) ->
    #{}.

generate_response_example(#{<<"content">> := Content}) ->
    maps:map(fun(_MediaType, MediaTypeObj) ->
        case maps:get(<<"schema">>, MediaTypeObj, undefined) of
            undefined -> #{};
            Schema -> generate_example_from_schema(Schema)
        end
    end, Content);
generate_response_example(_) ->
    #{}.

generate_example_from_schema(#{<<"type">> := <<"object">>, <<"properties">> := Props}) ->
    maps:map(fun(_Name, PropSchema) ->
        generate_example_value(PropSchema)
    end, Props);
generate_example_from_schema(#{<<"type">> := <<"array">>, <<"items">> := Items}) ->
    [generate_example_from_schema(Items)];
generate_example_from_schema(Schema) ->
    generate_example_value(Schema).

generate_example_value(#{<<"type">> := <<"string">>, <<"format">> := Format}) ->
    generate_formatted_string(Format);
generate_example_value(#{<<"type">> := <<"string">>, <<"enum">> := [First|_]}) ->
    First;
generate_example_value(#{<<"type">> := <<"string">>}) ->
    <<"example string">>;
generate_example_value(#{<<"type">> := <<"integer">>}) ->
    42;
generate_example_value(#{<<"type">> := <<"number">>}) ->
    3.14;
generate_example_value(#{<<"type">> := <<"boolean">>}) ->
    true;
generate_example_value(#{<<"$ref">> := _}) ->
    <<"[Referenced Schema]">>;
generate_example_value(_) ->
    null.

generate_formatted_string(<<"email">>) -> <<"user@example.com">>;
generate_formatted_string(<<"date">>) -> <<"2024-01-15">>;
generate_formatted_string(<<"date-time">>) -> <<"2024-01-15T10:30:00Z">>;
generate_formatted_string(<<"uuid">>) -> <<"550e8400-e29b-41d4-a716-446655440000">>;
generate_formatted_string(<<"uri">>) -> <<"https://example.com">>;
generate_formatted_string(<<"hostname">>) -> <<"example.com">>;
generate_formatted_string(<<"ipv4">>) -> <<"192.168.1.1">>;
generate_formatted_string(<<"ipv6">>) -> <<"2001:0db8:85a3:0000:0000:8a2e:0370:7334">>;
generate_formatted_string(_) -> <<"example">>.

generate_test_cases(Path, Method, Operation) ->
    %% Generate automated test cases
    [
        #{
            <<"name">> => <<"Success case">>,
            <<"request">> => generate_success_request(Operation),
            <<"expectedStatus">> => 200,
            <<"assertions">> => generate_success_assertions(Operation)
        },
        #{
            <<"name">> => <<"Missing required fields">>,
            <<"request">> => #{},
            <<"expectedStatus">> => 400,
            <<"assertions">> => [
                #{<<"path">> => <<"error">>, <<"exists">> => true}
            ]
        },
        #{
            <<"name">> => <<"Invalid data type">>,
            <<"request">> => generate_invalid_request(Operation),
            <<"expectedStatus">> => 400,
            <<"assertions">> => [
                #{<<"path">> => <<"error">>, <<"contains">> => <<"validation">>}
            ]
        }
    ].

generate_success_request(Operation) ->
    %% Generate a valid request based on operation schema
    RequestBody = maps:get(<<"requestBody">>, Operation, #{}),
    generate_request_example(RequestBody).

generate_success_assertions(Operation) ->
    %% Generate assertions for successful response
    [
        #{<<"path">> => <<"status">>, <<"equals">> => <<"success">>},
        #{<<"responseTime">> => #{<<"lessThan">> => 1000}}
    ].

generate_invalid_request(Operation) ->
    %% Generate request with invalid data types
    #{
        <<"string_field">> => 123,  % Number instead of string
        <<"number_field">> => <<"not a number">>,
        <<"required_field">> => null
    }.

%%====================================================================
%% Internal functions - Playground Data
%%====================================================================

generate_playground_data(Spec, State) ->
    %% Generate comprehensive playground data
    #{
        <<"spec">> => Spec,
        <<"navigation">> => generate_navigation(Spec),
        <<"defaultValues">> => generate_default_values(Spec),
        <<"savedRequests">> => get_saved_requests(State),
        <<"environment">> => #{
            <<"variables">> => get_environment_variables(),
            <<"baseUrl">> => get_default_base_url(Spec)
        },
        <<"features">> => #{
            <<"codeGeneration">> => #{
                <<"languages">> => [<<"curl">>, <<"javascript">>, <<"python">>, 
                                   <<"go">>, <<"rust">>, <<"erlang">>]
            },
            <<"import">> => #{
                <<"formats">> => [<<"postman">>, <<"insomnia">>, <<"har">>]
            },
            <<"export">> => #{
                <<"formats">> => [<<"postman">>, <<"insomnia">>, <<"openapi">>]
            }
        }
    }.

generate_navigation(Spec) ->
    %% Generate navigation tree for playground
    Paths = maps:get(<<"paths">>, Spec, #{}),
    Tags = maps:get(<<"tags">>, Spec, []),
    
    %% Group operations by tags
    TaggedOperations = group_operations_by_tag(Paths),
    
    %% Build navigation structure
    lists:map(fun(#{<<"name">> := TagName} = Tag) ->
        Operations = maps:get(TagName, TaggedOperations, []),
        Tag#{
            <<"operations">> => Operations,
            <<"expanded">> => true
        }
    end, Tags).

group_operations_by_tag(Paths) ->
    %% Group all operations by their tags
    maps:fold(fun(Path, PathItem, Acc) ->
        maps:fold(fun(Method, Operation, Acc2) when is_map(Operation) ->
            Tags = maps:get(<<"tags">>, Operation, [<<"untagged">>]),
            OpInfo = #{
                <<"path">> => Path,
                <<"method">> => Method,
                <<"operationId">> => maps:get(<<"operationId">>, Operation, 
                    generate_operation_id(Path, Method)),
                <<"summary">> => maps:get(<<"summary">>, Operation, <<>>)
            },
            lists:foldl(fun(Tag, Acc3) ->
                maps:update_with(Tag, fun(Ops) -> [OpInfo | Ops] end, [OpInfo], Acc3)
            end, Acc2, Tags);
        (_, _, Acc2) -> Acc2
        end, Acc, PathItem)
    end, #{}, Paths).

generate_default_values(Spec) ->
    %% Generate default values for common fields
    #{
        <<"headers">> => #{
            <<"Accept">> => <<"application/json">>,
            <<"Content-Type">> => <<"application/json">>
        },
        <<"authentication">> => extract_default_auth(Spec),
        <<"timeout">> => 30000
    }.

extract_default_auth(Spec) ->
    %% Extract default authentication values
    Security = maps:get(<<"security">>, Spec, []),
    case Security of
        [] -> #{};
        [First | _] -> 
            %% Use first security requirement as default
            maps:map(fun(_SchemeName, _Scopes) ->
                #{<<"value">> => <<"">>, <<"configured">> => false}
            end, First)
    end.

get_saved_requests(State) ->
    %% Get saved requests from history
    lists:map(fun(#{request := Req} = Entry) ->
        Entry#{
            <<"id">> => generate_request_id(),
            <<"name">> => generate_request_name(Req)
        }
    end, lists:sublist(State#state.request_history, 10)).

generate_request_name(#{<<"path">> := Path, <<"method">> := Method}) ->
    iolist_to_binary([Method, " ", Path]).

get_environment_variables() ->
    %% Get environment variables for playground
    #{
        <<"apiKey">> => <<"${API_KEY}">>,
        <<"baseUrl">> => <<"${BASE_URL}">>,
        <<"userId">> => <<"${USER_ID}">>
    }.

get_default_base_url(Spec) ->
    %% Get default server URL
    case maps:get(<<"servers">>, Spec, []) of
        [] -> <<"http://localhost:8080">>;
        [#{<<"url">> := Url} | _] -> Url
    end.

%%====================================================================
%% Internal functions - Request Execution
%%====================================================================

execute_playground_request(SpecName, Request, State) ->
    %% Execute request with playground features
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Check if using mock
    UseMock = maps:get(<<"useMock">>, Request, false),
    
    Result = case UseMock of
        true ->
            %% Use mock engine
            execute_mock_request(SpecName, Request, State);
        false ->
            %% Execute real request
            execute_real_request(SpecName, Request, State)
    end,
    
    %% Calculate execution time
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    
    %% Add metrics to result
    add_execution_metrics(Result, Duration).

execute_mock_request(SpecName, Request, State) ->
    %% Get operation from spec
    case maps:get(SpecName, State#state.specs, undefined) of
        undefined ->
            {error, spec_not_found};
        Spec ->
            %% Find operation
            Path = maps:get(<<"path">>, Request),
            Method = maps:get(<<"method">>, Request),
            
            case find_operation(Spec, Path, Method) of
                {ok, Operation} ->
                    %% Generate mock response
                    MockResponse = openapi_mock_engine:generate_response(
                        State#state.mock_engine, Operation, Request),
                    {ok, MockResponse};
                error ->
                    {error, operation_not_found}
            end
    end.

execute_real_request(_SpecName, Request, _State) ->
    %% Execute actual HTTP request
    Url = build_request_url(Request),
    Headers = maps:get(<<"headers">>, Request, #{}),
    Body = maps:get(<<"body">>, Request, <<>>),
    Method = binary_to_atom(string:lowercase(maps:get(<<"method">>, Request)), utf8),
    
    %% Convert headers to list format
    HeadersList = maps:to_list(Headers),
    
    %% Make HTTP request
    case hackney:request(Method, Url, HeadersList, Body, []) of
        {ok, StatusCode, RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            
            %% Parse response
            ParsedBody = try
                jsx:decode(RespBody, [return_maps])
            catch
                _:_ -> RespBody
            end,
            
            {ok, #{
                <<"status">> => StatusCode,
                <<"headers">> => maps:from_list(RespHeaders),
                <<"body">> => ParsedBody
            }};
        {error, Reason} ->
            {error, Reason}
    end.

build_request_url(Request) ->
    BaseUrl = maps:get(<<"baseUrl">>, Request, <<>>),
    Path = maps:get(<<"path">>, Request, <<>>),
    QueryParams = maps:get(<<"queryParams">>, Request, #{}),
    
    %% Build query string
    QueryString = build_query_string(QueryParams),
    
    %% Combine URL parts
    FullPath = case QueryString of
        <<>> -> Path;
        _ -> iolist_to_binary([Path, <<"?">>, QueryString])
    end,
    
    iolist_to_binary([BaseUrl, FullPath]).

build_query_string(Params) ->
    %% Build URL-encoded query string
    Parts = maps:fold(fun(K, V, Acc) ->
        Encoded = [uri_string:quote(K), <<"=">>, uri_string:quote(to_string(V))],
        [iolist_to_binary(Encoded) | Acc]
    end, [], Params),
    
    case Parts of
        [] -> <<>>;
        _ -> iolist_to_binary(lists:join(<<"&">>, lists:reverse(Parts)))
    end.

to_string(V) when is_binary(V) -> V;
to_string(V) when is_integer(V) -> integer_to_binary(V);
to_string(V) when is_float(V) -> float_to_binary(V);
to_string(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_string(V) -> iolist_to_binary(io_lib:format("~p", [V])).

add_execution_metrics({ok, Response}, Duration) ->
    {ok, Response#{
        <<"metrics">> => #{
            <<"duration">> => Duration,
            <<"timestamp">> => erlang:system_time(second)
        }
    }};
add_execution_metrics(Error, Duration) ->
    {error, #{
        <<"error">> => Error,
        <<"metrics">> => #{
            <<"duration">> => Duration,
            <<"timestamp">> => erlang:system_time(second)
        }
    }}.

%%====================================================================
%% Internal functions - WebSocket
%%====================================================================

handle_ws_message(#{<<"type">> := <<"subscribe">>, <<"operation">> := Operation}, State) ->
    %% Subscribe to operation updates
    #{
        <<"type">> => <<"subscribed">>,
        <<"operation">> => Operation,
        <<"id">> => generate_subscription_id()
    };

handle_ws_message(#{<<"type">> := <<"execute">>, <<"request">> := Request}, State) ->
    %% Execute request via WebSocket
    Result = execute_playground_request(maps:get(spec, State), Request, State),
    
    #{
        <<"type">> => <<"response">>,
        <<"result">> => Result
    };

handle_ws_message(#{<<"type">> := <<"generateCode">>, 
                   <<"language">> := Language,
                   <<"request">> := Request}, State) ->
    %% Generate code snippet
    Code = generate_code_snippet(Language, Request),
    
    #{
        <<"type">> => <<"codeGenerated">>,
        <<"language">> => Language,
        <<"code">> => Code
    };

handle_ws_message(_, _State) ->
    #{<<"error">> => <<"Unknown message type">>}.

generate_code_snippet(<<"curl">>, Request) ->
    generate_curl_snippet(Request);
generate_code_snippet(<<"javascript">>, Request) ->
    generate_javascript_snippet(Request);
generate_code_snippet(<<"python">>, Request) ->
    generate_python_snippet(Request);
generate_code_snippet(<<"erlang">>, Request) ->
    generate_erlang_snippet(Request);
generate_code_snippet(_, _) ->
    <<"// Code generation not supported for this language">>.

generate_curl_snippet(Request) ->
    Method = maps:get(<<"method">>, Request),
    Url = build_request_url(Request),
    Headers = maps:get(<<"headers">>, Request, #{}),
    Body = maps:get(<<"body">>, Request, <<>>),
    
    HeaderFlags = maps:fold(fun(K, V, Acc) ->
        [io_lib:format("-H '~s: ~s' ", [K, V]) | Acc]
    end, [], Headers),
    
    BodyFlag = case Body of
        <<>> -> "";
        _ -> io_lib:format("-d '~s' ", [jsx:encode(Body)])
    end,
    
    iolist_to_binary([
        "curl -X ", Method, " '", Url, "' \\\n",
        "  ", HeaderFlags, "\\\n",
        "  ", BodyFlag
    ]).

generate_javascript_snippet(Request) ->
    Method = maps:get(<<"method">>, Request),
    Url = build_request_url(Request),
    Headers = maps:get(<<"headers">>, Request, #{}),
    Body = maps:get(<<"body">>, Request, <<>>),
    
    iolist_to_binary([
        "const response = await fetch('", Url, "', {\n",
        "  method: '", Method, "',\n",
        "  headers: ", jsx:encode(Headers), ",\n",
        case Body of
            <<>> -> "";
            _ -> ["  body: JSON.stringify(", jsx:encode(Body), "),\n"]
        end,
        "});\n\n",
        "const data = await response.json();\n",
        "console.log(data);"
    ]).

generate_python_snippet(Request) ->
    Method = string:lowercase(maps:get(<<"method">>, Request)),
    Url = build_request_url(Request),
    Headers = maps:get(<<"headers">>, Request, #{}),
    Body = maps:get(<<"body">>, Request, <<>>),
    
    iolist_to_binary([
        "import requests\n\n",
        "response = requests.", Method, "(\n",
        "    '", Url, "',\n",
        "    headers=", format_python_dict(Headers), ",\n",
        case Body of
            <<>> -> "";
            _ -> ["    json=", format_python_dict(Body), ",\n"]
        end,
        ")\n\n",
        "print(response.json())"
    ]).

generate_erlang_snippet(Request) ->
    Method = binary_to_atom(string:lowercase(maps:get(<<"method">>, Request)), utf8),
    Url = build_request_url(Request),
    Headers = maps:get(<<"headers">>, Request, #{}),
    Body = maps:get(<<"body">>, Request, <<>>),
    
    iolist_to_binary([
        "Request = #{\n",
        "    method => ", atom_to_list(Method), ",\n",
        "    url => <<\"", Url, "\">>,\n",
        "    headers => ", io_lib:format("~p", [Headers]), ",\n",
        "    body => ", io_lib:format("~p", [jsx:encode(Body)]), "\n",
        "},\n\n",
        "{ok, StatusCode, RespHeaders, ClientRef} = hackney:request(\n",
        "    maps:get(method, Request),\n",
        "    maps:get(url, Request),\n",
        "    maps:to_list(maps:get(headers, Request)),\n",
        "    maps:get(body, Request),\n",
        "    []\n",
        "),\n",
        "{ok, Body} = hackney:body(ClientRef),\n",
        "jsx:decode(Body, [return_maps])."
    ]).

format_python_dict(Map) when is_map(Map) ->
    Items = maps:fold(fun(K, V, Acc) ->
        [io_lib:format("'~s': ~s", [K, format_python_value(V)]) | Acc]
    end, [], Map),
    iolist_to_binary(["{", lists:join(", ", lists:reverse(Items)), "}"]).

format_python_value(V) when is_binary(V) -> 
    io_lib:format("'~s'", [V]);
format_python_value(V) when is_integer(V) -> 
    integer_to_list(V);
format_python_value(V) when is_float(V) -> 
    float_to_list(V);
format_python_value(V) when is_boolean(V) -> 
    case V of true -> "True"; false -> "False" end;
format_python_value(V) when is_map(V) -> 
    format_python_dict(V);
format_python_value(V) when is_list(V) -> 
    jsx:encode(V).

%%====================================================================
%% Internal functions - Utilities
%%====================================================================

find_operation(Spec, Path, Method) ->
    Paths = maps:get(<<"paths">>, Spec, #{}),
    case maps:get(Path, Paths, undefined) of
        undefined -> error;
        PathItem ->
            case maps:get(Method, PathItem, undefined) of
                undefined -> error;
                Operation -> {ok, Operation}
            end
    end.

generate_operation_example(Spec, OperationId, MockEngine) ->
    %% Find operation by ID
    case find_operation_by_id(Spec, OperationId) of
        {ok, Operation} ->
            %% Generate example using mock engine
            Request = generate_example_request(Operation),
            Response = openapi_mock_engine:generate_response(MockEngine, Operation, Request),
            #{
                <<"request">> => Request,
                <<"response">> => Response
            };
        error ->
            #{<<"error">> => <<"Operation not found">>}
    end.

find_operation_by_id(Spec, OperationId) ->
    Paths = maps:get(<<"paths">>, Spec, #{}),
    find_operation_by_id_in_paths(Paths, OperationId).

find_operation_by_id_in_paths(Paths, OperationId) ->
    maps:fold(fun(_Path, PathItem, Acc) ->
        case Acc of
            {ok, _} -> Acc;
            error ->
                maps:fold(fun(_Method, Operation, Acc2) when is_map(Operation) ->
                    case maps:get(<<"operationId">>, Operation, undefined) of
                        OperationId -> {ok, Operation};
                        _ -> Acc2
                    end;
                (_, _, Acc2) -> Acc2
                end, error, PathItem)
        end
    end, error, Paths).

generate_example_request(Operation) ->
    %% Generate complete example request
    #{
        <<"headers">> => generate_example_headers(Operation),
        <<"queryParams">> => generate_example_query_params(Operation),
        <<"body">> => generate_example_body(Operation)
    }.

generate_example_headers(Operation) ->
    %% Generate example headers based on parameters
    Parameters = maps:get(<<"parameters">>, Operation, []),
    HeaderParams = [P || P <- Parameters, maps:get(<<"in">>, P) =:= <<"header">>],
    
    maps:from_list([{maps:get(<<"name">>, P), 
                     generate_param_example(P)} || P <- HeaderParams]).

generate_example_query_params(Operation) ->
    %% Generate example query parameters
    Parameters = maps:get(<<"parameters">>, Operation, []),
    QueryParams = [P || P <- Parameters, maps:get(<<"in">>, P) =:= <<"query">>],
    
    maps:from_list([{maps:get(<<"name">>, P), 
                     generate_param_example(P)} || P <- QueryParams]).

generate_example_body(Operation) ->
    %% Generate example request body
    case maps:get(<<"requestBody">>, Operation, undefined) of
        undefined -> #{};
        RequestBody -> generate_request_example(RequestBody)
    end.

generate_param_example(#{<<"schema">> := Schema} = Param) ->
    %% Use example if provided, otherwise generate
    case maps:get(<<"example">>, Param, undefined) of
        undefined -> generate_example_value(Schema);
        Example -> Example
    end.

validate_against_spec(Request, Spec) ->
    %% Validate request against OpenAPI spec
    Path = maps:get(<<"path">>, Request),
    Method = maps:get(<<"method">>, Request),
    
    case find_operation(Spec, Path, Method) of
        {ok, Operation} ->
            %% Validate parameters
            ParamResult = validate_parameters(Request, Operation),
            
            %% Validate request body
            BodyResult = validate_request_body(Request, Operation),
            
            %% Combine results
            case {ParamResult, BodyResult} of
                {ok, ok} -> {ok, valid};
                _ ->
                    Errors = lists:flatten([
                        case ParamResult of {error, E} -> E; _ -> [] end,
                        case BodyResult of {error, E} -> E; _ -> [] end
                    ]),
                    {error, Errors}
            end;
        error ->
            {error, [{<<"path">>, <<"Operation not found">>}]}
    end.

validate_parameters(Request, Operation) ->
    %% Validate request parameters
    Parameters = maps:get(<<"parameters">>, Operation, []),
    
    Errors = lists:filtermap(fun(Param) ->
        ParamName = maps:get(<<"name">>, Param),
        Required = maps:get(<<"required">>, Param, false),
        In = maps:get(<<"in">>, Param),
        
        Value = get_param_value(Request, In, ParamName),
        
        case {Required, Value} of
            {true, undefined} ->
                {true, {ParamName, <<"Required parameter missing">>}};
            {_, undefined} ->
                false;
            _ ->
                %% Validate against schema
                case maps:get(<<"schema">>, Param, undefined) of
                    undefined -> false;
                    Schema ->
                        case validate_value(Value, Schema) of
                            ok -> false;
                            {error, Reason} -> {true, {ParamName, Reason}}
                        end
                end
        end
    end, Parameters),
    
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

validate_request_body(Request, Operation) ->
    %% Validate request body against schema
    case maps:get(<<"requestBody">>, Operation, undefined) of
        undefined -> ok;
        RequestBodySpec ->
            Required = maps:get(<<"required">>, RequestBodySpec, false),
            Body = maps:get(<<"body">>, Request, undefined),
            
            case {Required, Body} of
                {true, undefined} ->
                    {error, [{<<"body">>, <<"Required request body missing">>}]};
                {_, undefined} ->
                    ok;
                _ ->
                    %% Validate against content schema
                    validate_body_content(Body, RequestBodySpec)
            end
    end.

get_param_value(Request, <<"query">>, Name) ->
    maps:get(Name, maps:get(<<"queryParams">>, Request, #{}), undefined);
get_param_value(Request, <<"header">>, Name) ->
    maps:get(Name, maps:get(<<"headers">>, Request, #{}), undefined);
get_param_value(Request, <<"path">>, Name) ->
    maps:get(Name, maps:get(<<"pathParams">>, Request, #{}), undefined);
get_param_value(_, _, _) ->
    undefined.

validate_value(Value, Schema) ->
    %% Simple schema validation
    Type = maps:get(<<"type">>, Schema, undefined),
    
    case Type of
        <<"string">> when is_binary(Value) -> ok;
        <<"integer">> when is_integer(Value) -> ok;
        <<"number">> when is_number(Value) -> ok;
        <<"boolean">> when is_boolean(Value) -> ok;
        <<"array">> when is_list(Value) -> ok;
        <<"object">> when is_map(Value) -> ok;
        _ -> {error, <<"Type mismatch">>}
    end.

validate_body_content(Body, RequestBodySpec) ->
    %% Get content type schema
    Content = maps:get(<<"content">>, RequestBodySpec, #{}),
    
    %% For now, validate against application/json schema
    case maps:get(<<"application/json">>, Content, undefined) of
        undefined -> ok;
        MediaTypeObj ->
            case maps:get(<<"schema">>, MediaTypeObj, undefined) of
                undefined -> ok;
                Schema -> validate_value(Body, Schema)
            end
    end.

generate_ws_id() ->
    %% Generate unique WebSocket ID
    base64:encode(crypto:strong_rand_bytes(16)).

generate_subscription_id() ->
    %% Generate unique subscription ID
    base64:encode(crypto:strong_rand_bytes(8)).

generate_request_id() ->
    %% Generate unique request ID
    base64:encode(crypto:strong_rand_bytes(8)).

generate_operation_id(Path, Method) ->
    %% Generate operation ID from path and method
    PathParts = binary:split(Path, <<"/">>, [global]),
    CleanParts = [P || P <- PathParts, P =/= <<>>],
    MethodStr = binary_to_list(Method),
    PathStr = string:join([binary_to_list(P) || P <- CleanParts], "_"),
    iolist_to_binary([MethodStr, "_", PathStr]).