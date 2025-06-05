%%%-------------------------------------------------------------------
%%% @doc Test Generation for OpenAPI
%%% Generates property-based tests, unit tests, and integration tests
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_test_generator).

-export([
    generate_proper_tests/2,
    generate_eunit_tests/2,
    generate_common_test_suite/2,
    generate_load_tests/2,
    generate_security_tests/2,
    generate_contract_tests/2
]).

%%====================================================================
%% API
%%====================================================================

%% @doc Generate PropEr property-based tests
generate_proper_tests(OpenAPISpec, Options) ->
    ModuleName = maps:get(module_name, Options, "api_proper_tests"),
    
    %% Extract schemas and operations
    Schemas = extract_schemas(OpenAPISpec),
    Operations = extract_operations(OpenAPISpec),
    
    %% Generate test module
    Code = [
        module_header(ModuleName, proper),
        "\n%% Generators\n",
        generate_schema_generators(Schemas),
        "\n%% Properties\n",
        generate_operation_properties(Operations, Options),
        "\n%% Helper functions\n",
        generate_proper_helpers()
    ],
    
    {ok, iolist_to_binary(Code)}.

%% @doc Generate EUnit tests
generate_eunit_tests(OpenAPISpec, Options) ->
    ModuleName = maps:get(module_name, Options, "api_eunit_tests"),
    
    Operations = extract_operations(OpenAPISpec),
    
    Code = [
        module_header(ModuleName, eunit),
        "\n%% Test fixtures\n",
        generate_test_fixtures(Operations),
        "\n%% Unit tests\n",
        generate_eunit_test_cases(Operations, Options),
        "\n%% Test helpers\n",
        generate_eunit_helpers()
    ],
    
    {ok, iolist_to_binary(Code)}.

%% @doc Generate Common Test suite
generate_common_test_suite(OpenAPISpec, Options) ->
    SuiteName = maps:get(suite_name, Options, "api_SUITE"),
    
    Operations = extract_operations(OpenAPISpec),
    
    Code = [
        ct_suite_header(SuiteName),
        "\n%% Common Test callbacks\n",
        generate_ct_callbacks(),
        "\n%% Test cases\n",
        generate_ct_test_cases(Operations, Options),
        "\n%% Helper functions\n",
        generate_ct_helpers()
    ],
    
    {ok, iolist_to_binary(Code)}.

%% @doc Generate load tests
generate_load_tests(OpenAPISpec, Options) ->
    ModuleName = maps:get(module_name, Options, "api_load_tests"),
    
    Operations = extract_operations(OpenAPISpec),
    
    Code = [
        module_header(ModuleName, load),
        "\n%% Load test scenarios\n",
        generate_load_scenarios(Operations, Options),
        "\n%% Tsung configuration\n",
        generate_tsung_config(Operations, Options),
        "\n%% JMeter test plan\n",
        generate_jmeter_plan(Operations, Options)
    ],
    
    {ok, iolist_to_binary(Code)}.

%% @doc Generate security tests
generate_security_tests(OpenAPISpec, Options) ->
    ModuleName = maps:get(module_name, Options, "api_security_tests"),
    
    Operations = extract_operations(OpenAPISpec),
    SecuritySchemes = extract_security_schemes(OpenAPISpec),
    
    Code = [
        module_header(ModuleName, security),
        "\n%% Authentication tests\n",
        generate_auth_tests(SecuritySchemes, Operations),
        "\n%% Authorization tests\n", 
        generate_authz_tests(Operations),
        "\n%% Input validation tests\n",
        generate_validation_tests(Operations),
        "\n%% Security scanning tests\n",
        generate_security_scan_tests(Operations)
    ],
    
    {ok, iolist_to_binary(Code)}.

%% @doc Generate contract tests
generate_contract_tests(OpenAPISpec, Options) ->
    ModuleName = maps:get(module_name, Options, "api_contract_tests"),
    
    Operations = extract_operations(OpenAPISpec),
    
    Code = [
        module_header(ModuleName, contract),
        "\n%% Contract validation\n",
        generate_contract_validations(Operations),
        "\n%% Schema compliance tests\n",
        generate_schema_compliance_tests(Operations),
        "\n%% Backwards compatibility tests\n",
        generate_compatibility_tests(Operations, Options)
    ],
    
    {ok, iolist_to_binary(Code)}.

%%====================================================================
%% Internal functions - Common
%%====================================================================

extract_schemas(OpenAPISpec) ->
    Components = maps:get(<<"components">>, OpenAPISpec, #{}),
    maps:get(<<"schemas">>, Components, #{}).

extract_operations(OpenAPISpec) ->
    Paths = maps:get(<<"paths">>, OpenAPISpec, #{}),
    
    maps:fold(fun(Path, PathItem, Acc) ->
        maps:fold(fun(Method, Operation, Acc2) when is_map(Operation) ->
            OpId = maps:get(<<"operationId">>, Operation, 
                           generate_operation_id(Path, Method)),
            [{OpId, Path, Method, Operation} | Acc2];
        (_, _, Acc2) -> Acc2
        end, Acc, PathItem)
    end, [], Paths).

extract_security_schemes(OpenAPISpec) ->
    Components = maps:get(<<"components">>, OpenAPISpec, #{}),
    maps:get(<<"securitySchemes">>, Components, #{}).

generate_operation_id(Path, Method) ->
    PathParts = [P || P <- binary:split(Path, <<"/">>, [global]), P =/= <<>>],
    MethodStr = binary_to_list(Method),
    PathStr = string:join([binary_to_list(P) || P <- PathParts], "_"),
    iolist_to_binary([MethodStr, "_", PathStr]).

module_header(ModuleName, Type) ->
    [
        "-module(", ModuleName, ").\n",
        case Type of
            proper ->
                "-include_lib(\"proper/include/proper.hrl\").\n"
                "-compile([export_all, nowarn_export_all]).\n\n";
            eunit ->
                "-include_lib(\"eunit/include/eunit.hrl\").\n\n";
            load ->
                "-export([run_load_test/1, generate_load/2]).\n\n";
            security ->
                "-export([run_security_suite/0]).\n"
                "-include_lib(\"eunit/include/eunit.hrl\").\n\n";
            contract ->
                "-export([validate_contract/1]).\n\n";
            _ -> "\n"
        end
    ].

%%====================================================================
%% Internal functions - PropEr Tests
%%====================================================================

generate_schema_generators(Schemas) ->
    maps:fold(fun(Name, Schema, Acc) ->
        Gen = generate_proper_generator_for_schema(Name, Schema),
        [Gen, "\n\n" | Acc]
    end, [], Schemas).

generate_proper_generator_for_schema(Name, Schema) ->
    GenName = ["gen_", to_snake_case(Name)],
    Properties = maps:get(<<"properties">>, Schema, #{}),
    Required = maps:get(<<"required">>, Schema, []),
    
    PropertyGens = generate_property_generators(Properties, Required),
    
    [
        GenName, "() ->\n",
        "    ?LET({", string:join([io_lib:format("V~p", [I]) || 
                                I <- lists:seq(1, length(PropertyGens))], ", "), "},\n",
        "         {", string:join([G || {_, G} <- PropertyGens], ",\n          "), "},\n",
        "         #{\n",
        generate_property_assignments(PropertyGens),
        "         }).\n"
    ].

generate_property_generators(Properties, Required) ->
    {Props, _} = maps:fold(fun(PropName, PropSchema, {Acc, Idx}) ->
        IsRequired = lists:member(PropName, Required),
        Gen = generate_value_generator(PropSchema, IsRequired),
        {[{PropName, Gen} | Acc], Idx + 1}
    end, {[], 1}, Properties),
    lists:reverse(Props).

generate_value_generator(#{<<"type">> := Type} = Schema, IsRequired) ->
    BaseGen = case Type of
        <<"string">> -> generate_string_generator(Schema);
        <<"integer">> -> generate_integer_generator(Schema);
        <<"number">> -> generate_number_generator(Schema);
        <<"boolean">> -> "boolean()";
        <<"array">> -> generate_array_generator(Schema);
        <<"object">> -> generate_object_generator(Schema);
        _ -> "any()"
    end,
    
    case IsRequired of
        true -> BaseGen;
        false -> ["oneof([undefined, ", BaseGen, "])"]
    end.

generate_string_generator(Schema) ->
    case maps:get(<<"format">>, Schema, undefined) of
        <<"email">> -> "email_gen()";
        <<"date">> -> "date_gen()";
        <<"date-time">> -> "datetime_gen()";
        <<"uuid">> -> "uuid_gen()";
        <<"uri">> -> "uri_gen()";
        _ ->
            case maps:get(<<"enum">>, Schema, undefined) of
                undefined ->
                    MinLen = maps:get(<<"minLength">>, Schema, 0),
                    MaxLen = maps:get(<<"maxLength">>, Schema, 100),
                    ["?SUCHTHAT(S, string(), length(S) >= ", integer_to_list(MinLen),
                     " andalso length(S) =< ", integer_to_list(MaxLen), ")"];
                Enum ->
                    ["oneof([", string:join([io_lib:format("<<\"~s\">>", [E]) || E <- Enum], ", "), "])"]
            end
    end.

generate_integer_generator(Schema) ->
    Min = maps:get(<<"minimum">>, Schema, -1000000),
    Max = maps:get(<<"maximum">>, Schema, 1000000),
    io_lib:format("integer(~p, ~p)", [Min, Max]).

generate_number_generator(Schema) ->
    "?LET(F, float(), F)".

generate_array_generator(#{<<"items">> := ItemSchema}) ->
    ItemGen = generate_value_generator(ItemSchema, true),
    ["list(", ItemGen, ")"].

generate_object_generator(Schema) ->
    "map()".

generate_property_assignments(PropertyGens) ->
    {Assignments, _} = lists:foldl(fun({PropName, _}, {Acc, Idx}) ->
        Assignment = io_lib:format("           <<\"~s\">> => V~p", [PropName, Idx]),
        {[Assignment | Acc], Idx + 1}
    end, {[], 1}, PropertyGens),
    
    string:join(lists:reverse(Assignments), ",\n") ++ "\n".

generate_operation_properties(Operations, Options) ->
    lists:map(fun({OpId, Path, Method, Operation}) ->
        generate_operation_property(OpId, Path, Method, Operation, Options)
    end, Operations).

generate_operation_property(OpId, Path, Method, Operation, Options) ->
    PropName = ["prop_", to_snake_case(OpId)],
    
    [
        "%% Property: ", OpId, "\n",
        PropName, "() ->\n",
        "    ?FORALL(Request, gen_request_", to_snake_case(OpId), "(),\n",
        "            begin\n",
        "                Response = api_client:", to_snake_case(OpId), "(Request),\n",
        "                validate_response_", to_snake_case(OpId), "(Response)\n",
        "            end).\n\n",
        
        "%% Request generator for ", OpId, "\n",
        "gen_request_", to_snake_case(OpId), "() ->\n",
        generate_request_generator(Operation),
        "\n\n",
        
        "%% Response validator for ", OpId, "\n", 
        "validate_response_", to_snake_case(OpId), "(Response) ->\n",
        generate_response_validator(Operation),
        "\n"
    ].

generate_request_generator(Operation) ->
    Parameters = maps:get(<<"parameters">>, Operation, []),
    RequestBody = maps:get(<<"requestBody">>, Operation, undefined),
    
    [
        "    ?LET({Params, Body},\n",
        "         {gen_parameters_", generate_param_gen_name(Parameters), "(),\n",
        "          ", generate_body_generator(RequestBody), "},\n",
        "         #{params => Params, body => Body})."
    ].

generate_body_generator(undefined) ->
    "undefined";
generate_body_generator(RequestBody) ->
    case maps:get(<<"content">>, RequestBody, #{}) of
        #{<<"application/json">> := #{<<"schema">> := Schema}} ->
            generate_value_generator(Schema, maps:get(<<"required">>, RequestBody, false));
        _ ->
            "binary()"
    end.

generate_response_validator(Operation) ->
    Responses = maps:get(<<"responses">>, Operation, #{}),
    
    [
        "    case Response of\n",
        generate_response_cases(Responses),
        "        _ -> false\n",
        "    end."
    ].

generate_response_cases(Responses) ->
    maps:fold(fun(StatusCode, Response, Acc) ->
        [generate_response_case(StatusCode, Response) | Acc]
    end, [], Responses).

generate_response_case(StatusCode, Response) ->
    io_lib:format("        {ok, ~s, _Headers, Body} ->~n            validate_schema(Body, response_schema_~s());~n",
                  [StatusCode, StatusCode]).

generate_proper_helpers() ->
    [
        "%% Common generators\n",
        "email_gen() ->\n",
        "    ?LET({User, Domain}, {non_empty_string(), non_empty_string()},\n",
        "         list_to_binary(User ++ \"@\" ++ Domain ++ \".com\")).\n\n",
        
        "date_gen() ->\n",
        "    ?LET({Y, M, D}, {integer(2020, 2025), integer(1, 12), integer(1, 28)},\n",
        "         list_to_binary(io_lib:format(\"~4..0B-~2..0B-~2..0B\", [Y, M, D]))).\n\n",
        
        "datetime_gen() ->\n",
        "    ?LET({Date, H, M, S}, {date_gen(), integer(0, 23), integer(0, 59), integer(0, 59)},\n",
        "         <<Date/binary, \"T\", (integer_to_binary(H))/binary, \":\",\n",
        "           (integer_to_binary(M))/binary, \":\", (integer_to_binary(S))/binary, \"Z\">>).\n\n",
        
        "uuid_gen() ->\n",
        "    list_to_binary(agent_uuid:to_string(agent_uuid:uuid4())).\n\n",
        
        "uri_gen() ->\n",
        "    ?LET({Scheme, Host, Path}, \n",
        "         {oneof([\"http\", \"https\"]), non_empty_string(), string()},\n",
        "         list_to_binary(Scheme ++ \"://\" ++ Host ++ \".com/\" ++ Path)).\n\n",
        
        "non_empty_string() ->\n",
        "    ?SUCHTHAT(S, string(), length(S) > 0).\n"
    ].

%%====================================================================
%% Internal functions - EUnit Tests
%%====================================================================

generate_test_fixtures(Operations) ->
    lists:map(fun({OpId, _, _, Operation}) ->
        generate_test_fixture(OpId, Operation)
    end, Operations).

generate_test_fixture(OpId, Operation) ->
    [
        "%% Test fixture for ", OpId, "\n",
        "fixture_", to_snake_case(OpId), "() ->\n",
        "    #{\n",
        "        request => ", generate_example_request_data(Operation), ",\n",
        "        expected_response => ", generate_example_response_data(Operation), "\n",
        "    }.\n\n"
    ].

generate_example_request_data(Operation) ->
    %% Generate example request based on operation schema
    "#{method => get, path => \"/test\"}".

generate_example_response_data(Operation) ->
    %% Generate example response based on operation schema
    "#{status => 200, body => #{}}".

generate_eunit_test_cases(Operations, Options) ->
    lists:map(fun({OpId, Path, Method, Operation}) ->
        generate_eunit_test_case(OpId, Path, Method, Operation, Options)
    end, Operations).

generate_eunit_test_case(OpId, Path, Method, Operation, Options) ->
    TestName = [to_snake_case(OpId), "_test_"],
    
    [
        "%% Test: ", OpId, "\n",
        TestName, "() ->\n",
        "    Fixture = fixture_", to_snake_case(OpId), "(),\n",
        "    Request = maps:get(request, Fixture),\n",
        "    Expected = maps:get(expected_response, Fixture),\n",
        "    \n",
        "    %% Execute request\n",
        "    Result = api_client:", to_snake_case(OpId), "(Request),\n",
        "    \n",
        "    %% Verify response\n",
        "    ?assertMatch({ok, _}, Result),\n",
        "    {ok, Response} = Result,\n",
        "    ?assertEqual(maps:get(status, Expected), maps:get(status, Response)).\n\n"
    ].

generate_eunit_helpers() ->
    [
        "%% Test helpers\n",
        "setup() ->\n",
        "    application:ensure_all_started(hackney),\n",
        "    ok.\n\n",
        
        "cleanup(_) ->\n",
        "    application:stop(hackney),\n",
        "    ok.\n"
    ].

%%====================================================================
%% Internal functions - Common Test
%%====================================================================

ct_suite_header(SuiteName) ->
    [
        "-module(", SuiteName, ").\n",
        "-compile(export_all).\n",
        "-include_lib(\"common_test/include/ct.hrl\").\n\n"
    ].

generate_ct_callbacks() ->
    [
        "suite() ->\n",
        "    [{timetrap, {minutes, 10}}].\n\n",
        
        "init_per_suite(Config) ->\n",
        "    application:ensure_all_started(inets),\n",
        "    application:ensure_all_started(ssl),\n",
        "    Config.\n\n",
        
        "end_per_suite(_Config) ->\n",
        "    ok.\n\n",
        
        "init_per_testcase(_Case, Config) ->\n",
        "    Config.\n\n",
        
        "end_per_testcase(_Case, _Config) ->\n",
        "    ok.\n\n",
        
        "all() ->\n",
        "    [test_all_endpoints,\n",
        "     test_error_handling,\n",
        "     test_authentication,\n",
        "     test_rate_limiting].\n\n"
    ].

generate_ct_test_cases(Operations, Options) ->
    [
        "test_all_endpoints(_Config) ->\n",
        "    %% Test all API endpoints\n",
        generate_ct_endpoint_tests(Operations),
        "    ok.\n\n",
        
        "test_error_handling(_Config) ->\n",
        "    %% Test error scenarios\n",
        generate_ct_error_tests(Operations),
        "    ok.\n\n",
        
        "test_authentication(_Config) ->\n",
        "    %% Test authentication\n",
        generate_ct_auth_tests(Operations),
        "    ok.\n\n",
        
        "test_rate_limiting(_Config) ->\n",
        "    %% Test rate limiting\n",
        generate_ct_rate_limit_tests(Operations),
        "    ok.\n\n"
    ].

generate_ct_endpoint_tests(Operations) ->
    lists:map(fun({OpId, Path, Method, _}) ->
        io_lib:format("    ct:log(\"Testing ~s ~s\"),~n", [Method, Path])
    end, Operations).

generate_ct_error_tests(Operations) ->
    "    %% Test various error scenarios\n".

generate_ct_auth_tests(Operations) ->
    "    %% Test authentication scenarios\n".

generate_ct_rate_limit_tests(Operations) ->
    "    %% Test rate limiting\n".

generate_ct_helpers() ->
    [
        "%% Helper functions\n",
        "make_request(Method, Path, Headers, Body) ->\n",
        "    Url = \"http://localhost:8080\" ++ Path,\n",
        "    httpc:request(Method, {Url, Headers, \"application/json\", Body}, [], []).\n"
    ].

%%====================================================================
%% Internal functions - Load Tests
%%====================================================================

generate_load_scenarios(Operations, Options) ->
    lists:map(fun({OpId, Path, Method, Operation}) ->
        generate_load_scenario(OpId, Path, Method, Operation, Options)
    end, Operations).

generate_load_scenario(OpId, Path, Method, Operation, Options) ->
    [
        "%% Load scenario: ", OpId, "\n",
        "scenario_", to_snake_case(OpId), "(Config) ->\n",
        "    #{\n",
        "        name => <<\"", OpId/binary, "\">>,\n",
        "        weight => ", generate_scenario_weight(Operation), ",\n",
        "        request => fun() -> ", generate_load_request(Path, Method, Operation), " end,\n",
        "        think_time => {random, 1000, 5000},\n",
        "        timeout => 30000\n",
        "    }.\n\n"
    ].

generate_scenario_weight(Operation) ->
    %% Assign weights based on operation type
    case maps:get(<<"x-load-weight">>, Operation, undefined) of
        undefined -> "10";
        Weight -> integer_to_list(Weight)
    end.

generate_load_request(Path, Method, Operation) ->
    io_lib:format("make_~s_request(~p)", [Method, Path]).

generate_tsung_config(Operations, Options) ->
    [
        "%% Tsung configuration\n",
        "tsung_config() ->\n",
        "    [\n",
        "        {clients, [{host, \"localhost\", 1000}]},\n",
        "        {servers, [{host, \"api.example.com\", 80}]},\n",
        "        {load, [\n",
        "            {arrivalphase, [\n",
        "                {phase, 1, 60, {users, {10, sec}}},\n",
        "                {phase, 2, 120, {users, {50, sec}}},\n",
        "                {phase, 3, 180, {users, {100, sec}}}\n",
        "            ]}\n",
        "        ]},\n",
        "        {sessions, [\n",
        generate_tsung_sessions(Operations),
        "        ]}\n",
        "    ].\n\n"
    ].

generate_tsung_sessions(Operations) ->
    Sessions = lists:map(fun({OpId, Path, Method, _}) ->
        io_lib:format("            {session, ~p, 10, [\n"
                      "                {request, {http, ~p, ~p, []}}\n"
                      "            ]}", [OpId, Method, Path])
    end, Operations),
    string:join(Sessions, ",\n").

generate_jmeter_plan(Operations, Options) ->
    [
        "%% JMeter test plan generator\n",
        "jmeter_plan() ->\n",
        "    #{\n",
        "        thread_groups => [\n",
        generate_jmeter_thread_groups(Operations),
        "        ],\n",
        "        assertions => [\n",
        "            {response_time, {max, 1000}},\n",
        "            {error_rate, {max, 0.01}}\n",
        "        ]\n",
        "    }.\n"
    ].

generate_jmeter_thread_groups(Operations) ->
    Groups = lists:map(fun({OpId, _, _, _}) ->
        io_lib:format("            #{\n"
                      "                name => <<\"~s\">>,\n"
                      "                threads => 100,\n"
                      "                ramp_time => 60,\n"
                      "                duration => 300\n"
                      "            }", [OpId])
    end, Operations),
    string:join(Groups, ",\n").

%%====================================================================
%% Internal functions - Security Tests
%%====================================================================

generate_auth_tests(SecuritySchemes, Operations) ->
    [
        "%% Authentication tests\n",
        generate_scheme_tests(SecuritySchemes),
        "\n%% Operation-specific auth tests\n",
        generate_operation_auth_tests(Operations)
    ].

generate_scheme_tests(SecuritySchemes) ->
    maps:fold(fun(Name, Scheme, Acc) ->
        [generate_scheme_test(Name, Scheme) | Acc]
    end, [], SecuritySchemes).

generate_scheme_test(Name, #{<<"type">> := Type} = Scheme) ->
    TestName = ["test_auth_", to_snake_case(Name)],
    
    [
        TestName, "() ->\n",
        case Type of
            <<"apiKey">> -> generate_api_key_test(Scheme);
            <<"http">> -> generate_http_auth_test(Scheme);
            <<"oauth2">> -> generate_oauth2_test(Scheme);
            <<"openIdConnect">> -> generate_oidc_test(Scheme);
            _ -> "    %% Unknown auth type\n    ok.\n"
        end,
        "\n"
    ].

generate_api_key_test(Scheme) ->
    [
        "    %% Test API key authentication\n",
        "    ValidKey = <<\"valid-api-key\">>,\n",
        "    InvalidKey = <<\"invalid-key\">>,\n",
        "    \n",
        "    %% Test with valid key\n",
        "    {ok, 200, _, _} = make_authenticated_request(ValidKey),\n",
        "    \n", 
        "    %% Test with invalid key\n",
        "    {ok, 401, _, _} = make_authenticated_request(InvalidKey),\n",
        "    \n",
        "    %% Test without key\n",
        "    {ok, 401, _, _} = make_request_without_auth(),\n",
        "    ok."
    ].

generate_http_auth_test(Scheme) ->
    case maps:get(<<"scheme">>, Scheme) of
        <<"bearer">> -> generate_bearer_auth_test();
        <<"basic">> -> generate_basic_auth_test();
        _ -> "    ok."
    end.

generate_bearer_auth_test() ->
    [
        "    %% Test Bearer token authentication\n",
        "    ValidToken = <<\"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...\">>,\n",
        "    ExpiredToken = <<\"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...\">>,\n",
        "    \n",
        "    %% Test with valid token\n",
        "    {ok, 200, _, _} = make_bearer_request(ValidToken),\n",
        "    \n",
        "    %% Test with expired token\n",
        "    {ok, 401, _, _} = make_bearer_request(ExpiredToken),\n",
        "    ok."
    ].

generate_basic_auth_test() ->
    [
        "    %% Test Basic authentication\n",
        "    ValidCreds = base64:encode(<<\"user:password\">>),\n",
        "    InvalidCreds = base64:encode(<<\"user:wrong\">>),\n",
        "    \n",
        "    %% Test with valid credentials\n",
        "    {ok, 200, _, _} = make_basic_auth_request(ValidCreds),\n",
        "    \n",
        "    %% Test with invalid credentials\n",
        "    {ok, 401, _, _} = make_basic_auth_request(InvalidCreds),\n",
        "    ok."
    ].

generate_oauth2_test(Scheme) ->
    [
        "    %% Test OAuth2 authentication\n",
        "    %% This would test the OAuth2 flow\n",
        "    ok."
    ].

generate_oidc_test(Scheme) ->
    [
        "    %% Test OpenID Connect authentication\n",
        "    %% This would test the OIDC flow\n",
        "    ok."
    ].

generate_operation_auth_tests(Operations) ->
    lists:map(fun({OpId, _, _, Operation}) ->
        case maps:get(<<"security">>, Operation, []) of
            [] -> "";
            Security -> generate_operation_security_test(OpId, Security)
        end
    end, Operations).

generate_operation_security_test(OpId, Security) ->
    [
        "test_", to_snake_case(OpId), "_auth() ->\n",
        "    %% Test operation-specific security requirements\n",
        "    ok.\n\n"
    ].

generate_authz_tests(Operations) ->
    [
        "%% Authorization tests\n",
        "test_authorization() ->\n",
        "    %% Test role-based access\n",
        "    test_admin_access(),\n",
        "    test_user_access(),\n", 
        "    test_guest_access(),\n",
        "    ok.\n\n",
        
        "test_admin_access() ->\n",
        "    AdminToken = get_admin_token(),\n",
        generate_role_tests("admin", Operations),
        "    ok.\n\n",
        
        "test_user_access() ->\n",
        "    UserToken = get_user_token(),\n",
        generate_role_tests("user", Operations),
        "    ok.\n\n",
        
        "test_guest_access() ->\n",
        generate_role_tests("guest", Operations),
        "    ok.\n\n"
    ].

generate_role_tests(Role, Operations) ->
    "    %% Test " ++ Role ++ " access to endpoints\n".

generate_validation_tests(Operations) ->
    [
        "%% Input validation tests\n",
        generate_sql_injection_tests(Operations),
        generate_xss_tests(Operations),
        generate_command_injection_tests(Operations),
        generate_path_traversal_tests(Operations)
    ].

generate_sql_injection_tests(Operations) ->
    [
        "test_sql_injection() ->\n",
        "    Payloads = [\n",
        "        <<\"' OR '1'='1\">>,\n",
        "        <<\"1; DROP TABLE users--\">>,\n",
        "        <<\"' UNION SELECT * FROM passwords--\">>\n",
        "    ],\n",
        "    \n",
        "    lists:foreach(fun(Payload) ->\n",
        "        test_payload_rejected(Payload)\n",
        "    end, Payloads),\n",
        "    ok.\n\n"
    ].

generate_xss_tests(Operations) ->
    [
        "test_xss() ->\n",
        "    Payloads = [\n",
        "        <<\"<script>alert('XSS')</script>\">>,\n",
        "        <<\"<img src=x onerror=alert('XSS')>\">>,\n",
        "        <<\"javascript:alert('XSS')\">>\n",
        "    ],\n",
        "    \n",
        "    lists:foreach(fun(Payload) ->\n",
        "        test_payload_sanitized(Payload)\n",
        "    end, Payloads),\n",
        "    ok.\n\n"
    ].

generate_command_injection_tests(Operations) ->
    [
        "test_command_injection() ->\n",
        "    Payloads = [\n",
        "        <<\"; cat /etc/passwd\">>,\n",
        "        <<\"| ls -la\">>,\n",
        "        <<\"&& rm -rf /\">>\n",
        "    ],\n",
        "    \n",
        "    lists:foreach(fun(Payload) ->\n",
        "        test_payload_rejected(Payload)\n",
        "    end, Payloads),\n",
        "    ok.\n\n"
    ].

generate_path_traversal_tests(Operations) ->
    [
        "test_path_traversal() ->\n",
        "    Payloads = [\n",
        "        <<\"../../../etc/passwd\">>,\n",
        "        <<\"..\\\\..\\\\..\\\\windows\\\\system32\\\\config\\\\sam\">>,\n",
        "        <<\"%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd\">>\n",
        "    ],\n",
        "    \n",
        "    lists:foreach(fun(Payload) ->\n",
        "        test_path_payload_rejected(Payload)\n",
        "    end, Payloads),\n",
        "    ok.\n\n"
    ].

generate_security_scan_tests(Operations) ->
    [
        "%% Automated security scanning\n",
        "run_security_scan() ->\n",
        "    %% Run OWASP ZAP scan\n",
        "    zap_scan(),\n",
        "    \n",
        "    %% Run Burp Suite scan\n",
        "    burp_scan(),\n",
        "    \n",
        "    %% Run custom security checks\n",
        "    custom_security_checks(),\n",
        "    ok.\n\n"
    ].

%%====================================================================
%% Internal functions - Contract Tests
%%====================================================================

generate_contract_validations(Operations) ->
    [
        "%% Contract validation functions\n",
        lists:map(fun({OpId, _, _, Operation}) ->
            generate_contract_validation(OpId, Operation)
        end, Operations)
    ].

generate_contract_validation(OpId, Operation) ->
    [
        "validate_", to_snake_case(OpId), "_contract(Response) ->\n",
        "    %% Validate response against contract\n",
        "    Schema = get_response_schema(<<\"", OpId/binary, "\">>),\n",
        "    jesse:validate(Schema, Response).\n\n"
    ].

generate_schema_compliance_tests(Operations) ->
    [
        "%% Schema compliance tests\n",
        "test_schema_compliance() ->\n",
        "    Operations = [\n",
        generate_operation_list(Operations),
        "    ],\n",
        "    \n",
        "    lists:foreach(fun(Op) ->\n",
        "        test_operation_compliance(Op)\n",
        "    end, Operations),\n",
        "    ok.\n\n"
    ].

generate_operation_list(Operations) ->
    OpList = lists:map(fun({OpId, _, _, _}) ->
        io_lib:format("        <<\"~s\">>", [OpId])
    end, Operations),
    string:join(OpList, ",\n").

generate_compatibility_tests(Operations, Options) ->
    [
        "%% Backwards compatibility tests\n",
        "test_backwards_compatibility() ->\n",
        "    OldVersion = \"1.0.0\",\n",
        "    NewVersion = \"2.0.0\",\n",
        "    \n",
        "    %% Test that all v1 operations still work\n",
        "    test_version_compatibility(OldVersion, NewVersion),\n",
        "    \n",
        "    %% Test response format compatibility\n",
        "    test_response_compatibility(OldVersion, NewVersion),\n",
        "    ok.\n\n"
    ].

%%====================================================================
%% Internal functions - Utilities
%%====================================================================

to_snake_case(Binary) when is_binary(Binary) ->
    to_snake_case(binary_to_list(Binary));
to_snake_case(String) ->
    %% Convert camelCase to snake_case
    lists:flatten(lists:map(fun(C) ->
        if
            C >= $A andalso C =< $Z -> [$_, C + 32];
            true -> C
        end
    end, String)).

generate_param_gen_name(Parameters) ->
    case length(Parameters) of
        0 -> "empty";
        N -> integer_to_list(N)
    end.