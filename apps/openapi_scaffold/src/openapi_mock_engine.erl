%%%-------------------------------------------------------------------
%%% @doc Realistic Mock Engine
%%% Generates realistic mock data based on OpenAPI schemas with
%%% support for relationships, state management, and realistic patterns.
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_mock_engine).
-behaviour(gen_server).

-export([
    start_link/0,
    generate_response/3,
    generate_data/2,
    set_scenario/2,
    get_state/1,
    reset_state/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    data_store :: ets:tid(),
    scenarios :: map(),
    generators :: map(),
    relationships :: map(),
    current_scenario :: atom()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Generate mock response for operation
generate_response(Engine, Operation, Request) ->
    gen_server:call(Engine, {generate_response, Operation, Request}).

%% @doc Generate mock data based on schema
generate_data(Schema, Options) ->
    gen_server:call(?MODULE, {generate_data, Schema, Options}).

%% @doc Set mock scenario (e.g., error, slow, success)
set_scenario(Engine, Scenario) ->
    gen_server:call(Engine, {set_scenario, Scenario}).

%% @doc Get current mock state
get_state(Engine) ->
    gen_server:call(Engine, get_state).

%% @doc Reset mock state
reset_state(Engine) ->
    gen_server:call(Engine, reset_state).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize data store
    DataStore = ets:new(mock_data, [set, private]),
    
    %% Load realistic data generators
    Generators = init_generators(),
    
    %% Define relationships between entities
    Relationships = init_relationships(),
    
    %% Define mock scenarios
    Scenarios = init_scenarios(),
    
    %% Seed initial data
    seed_initial_data(DataStore, Generators),
    
    State = #state{
        data_store = DataStore,
        scenarios = Scenarios,
        generators = Generators,
        relationships = Relationships,
        current_scenario = normal
    },
    
    {ok, State}.

handle_call({generate_response, Operation, Request}, _From, State) ->
    Response = generate_operation_response(Operation, Request, State),
    {reply, Response, State};

handle_call({generate_data, Schema, Options}, _From, State) ->
    Data = generate_schema_data(Schema, Options, State),
    {reply, {ok, Data}, State};

handle_call({set_scenario, Scenario}, _From, State) ->
    {reply, ok, State#state{current_scenario = Scenario}};

handle_call(get_state, _From, State) ->
    CurrentState = export_state(State),
    {reply, {ok, CurrentState}, State};

handle_call(reset_state, _From, State) ->
    %% Clear and reseed data
    ets:delete_all_objects(State#state.data_store),
    seed_initial_data(State#state.data_store, State#state.generators),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Initialization
%%====================================================================

init_generators() ->
    #{
        %% Person names
        first_names => [
            <<"James">>, <<"Mary">>, <<"John">>, <<"Patricia">>, <<"Robert">>,
            <<"Jennifer">>, <<"Michael">>, <<"Linda">>, <<"William">>, <<"Elizabeth">>
        ],
        last_names => [
            <<"Smith">>, <<"Johnson">>, <<"Williams">>, <<"Brown">>, <<"Jones">>,
            <<"Garcia">>, <<"Miller">>, <<"Davis">>, <<"Rodriguez">>, <<"Martinez">>
        ],
        
        %% Company names
        company_names => [
            <<"Acme Corp">>, <<"GlobalTech">>, <<"Innovate Inc">>, <<"FutureSoft">>,
            <<"DataDyne">>, <<"CyberCore">>, <<"TechVault">>, <<"CloudNine">>,
            <<"QuantumLeap">>, <<"NexGen Solutions">>
        ],
        
        %% Products
        product_names => [
            <<"Premium Widget">>, <<"Pro Gadget">>, <<"Ultra Device">>, 
            <<"Smart Tool">>, <<"Mega Component">>, <<"Super Module">>,
            <<"Power Unit">>, <<"Elite System">>, <<"Prime Kit">>, <<"Max Pack">>
        ],
        
        %% Addresses
        street_names => [
            <<"Main">>, <<"First">>, <<"Second">>, <<"Third">>, <<"Park">>,
            <<"Oak">>, <<"Pine">>, <<"Maple">>, <<"Cedar">>, <<"Elm">>
        ],
        street_types => [
            <<"Street">>, <<"Avenue">>, <<"Boulevard">>, <<"Drive">>, <<"Road">>,
            <<"Lane">>, <<"Way">>, <<"Court">>, <<"Place">>, <<"Circle">>
        ],
        cities => [
            <<"New York">>, <<"Los Angeles">>, <<"Chicago">>, <<"Houston">>,
            <<"Phoenix">>, <<"Philadelphia">>, <<"San Antonio">>, <<"San Diego">>,
            <<"Dallas">>, <<"San Jose">>
        ],
        states => [
            <<"NY">>, <<"CA">>, <<"TX">>, <<"FL">>, <<"IL">>, 
            <<"PA">>, <<"OH">>, <<"GA">>, <<"NC">>, <<"MI">>
        ],
        
        %% Lorem ipsum
        lorem_words => [
            <<"lorem">>, <<"ipsum">>, <<"dolor">>, <<"sit">>, <<"amet">>,
            <<"consectetur">>, <<"adipiscing">>, <<"elit">>, <<"sed">>, <<"do">>,
            <<"eiusmod">>, <<"tempor">>, <<"incididunt">>, <<"ut">>, <<"labore">>
        ]
    }.

init_relationships() ->
    #{
        user => #{
            orders => {has_many, order},
            profile => {has_one, profile},
            addresses => {has_many, address}
        },
        order => #{
            user => {belongs_to, user},
            items => {has_many, order_item},
            shipping_address => {has_one, address}
        },
        product => #{
            categories => {has_many, category},
            reviews => {has_many, review}
        }
    }.

init_scenarios() ->
    #{
        normal => #{
            latency => {0, 100},
            error_rate => 0.0,
            status_codes => [200, 201]
        },
        slow => #{
            latency => {1000, 5000},
            error_rate => 0.0,
            status_codes => [200, 201]
        },
        flaky => #{
            latency => {0, 1000},
            error_rate => 0.2,
            status_codes => [200, 201, 500, 503]
        },
        error => #{
            latency => {0, 100},
            error_rate => 1.0,
            status_codes => [400, 404, 500]
        },
        rate_limited => #{
            latency => {0, 100},
            error_rate => 0.5,
            status_codes => [200, 429]
        }
    }.

seed_initial_data(DataStore, Generators) ->
    %% Seed users
    Users = [generate_user(I, Generators) || I <- lists:seq(1, 100)],
    [ets:insert(DataStore, {{user, maps:get(id, U)}, U}) || U <- Users],
    
    %% Seed products
    Products = [generate_product(I, Generators) || I <- lists:seq(1, 50)],
    [ets:insert(DataStore, {{product, maps:get(id, P)}, P}) || P <- Products],
    
    %% Seed orders
    Orders = [generate_order(I, Users, Products, Generators) || I <- lists:seq(1, 200)],
    [ets:insert(DataStore, {{order, maps:get(id, O)}, O}) || O <- Orders].

%%====================================================================
%% Internal functions - Response Generation
%%====================================================================

generate_operation_response(Operation, Request, State) ->
    %% Apply scenario-based behavior
    Scenario = maps:get(State#state.current_scenario, State#state.scenarios),
    
    %% Simulate latency
    simulate_latency(Scenario),
    
    %% Determine response status
    Status = determine_status(Operation, Scenario),
    
    %% Generate response based on status
    case Status of
        200 -> generate_success_response(Operation, Request, State);
        201 -> generate_created_response(Operation, Request, State);
        204 -> {204, #{}, <<>>};
        400 -> generate_error_response(400, <<"Bad Request">>, <<"Invalid input">>);
        404 -> generate_error_response(404, <<"Not Found">>, <<"Resource not found">>);
        429 -> generate_rate_limit_response();
        500 -> generate_error_response(500, <<"Internal Server Error">>, <<"Something went wrong">>);
        503 -> generate_error_response(503, <<"Service Unavailable">>, <<"Service temporarily unavailable">>)
    end.

simulate_latency(#{latency := {Min, Max}}) ->
    Latency = Min + rand:uniform(Max - Min),
    timer:sleep(Latency).

determine_status(Operation, Scenario) ->
    ErrorRate = maps:get(error_rate, Scenario),
    StatusCodes = maps:get(status_codes, Scenario),
    
    case rand:uniform() < ErrorRate of
        true ->
            %% Return error status
            ErrorCodes = [S || S <- StatusCodes, S >= 400],
            lists:nth(rand:uniform(length(ErrorCodes)), ErrorCodes);
        false ->
            %% Return success status
            SuccessCodes = [S || S <- StatusCodes, S < 400],
            case SuccessCodes of
                [] -> 200;
                _ -> lists:nth(rand:uniform(length(SuccessCodes)), SuccessCodes)
            end
    end.

generate_success_response(Operation, Request, State) ->
    %% Extract response schema
    Responses = maps:get(<<"responses">>, Operation, #{}),
    Response200 = maps:get(<<"200">>, Responses, #{}),
    
    %% Generate response data
    ResponseData = case extract_response_schema(Response200) of
        undefined -> #{};
        Schema -> generate_schema_data(Schema, #{}, State)
    end,
    
    %% Add headers
    Headers = #{
        <<"content-type">> => <<"application/json">>,
        <<"x-request-id">> => generate_request_id(),
        <<"x-response-time">> => integer_to_binary(rand:uniform(100))
    },
    
    {200, Headers, ResponseData}.

generate_created_response(Operation, Request, State) ->
    %% Similar to success but with 201 and Location header
    {Status, Headers, Body} = generate_success_response(Operation, Request, State),
    
    %% Add Location header
    ResourceId = maps:get(<<"id">>, Body, generate_id()),
    Location = <<"/api/resources/", ResourceId/binary>>,
    
    {201, Headers#{<<"location">> => Location}, Body}.

generate_error_response(Status, Error, Message) ->
    Headers = #{
        <<"content-type">> => <<"application/json">>,
        <<"x-request-id">> => generate_request_id()
    },
    
    Body = #{
        <<"error">> => Error,
        <<"message">> => Message,
        <<"timestamp">> => iso8601_timestamp(),
        <<"status">> => Status
    },
    
    {Status, Headers, Body}.

generate_rate_limit_response() ->
    Headers = #{
        <<"content-type">> => <<"application/json">>,
        <<"x-rate-limit-limit">> => <<"100">>,
        <<"x-rate-limit-remaining">> => <<"0">>,
        <<"x-rate-limit-reset">> => integer_to_binary(erlang:system_time(second) + 3600)
    },
    
    Body = #{
        <<"error">> => <<"Too Many Requests">>,
        <<"message">> => <<"Rate limit exceeded. Please retry after one hour.">>,
        <<"retry_after">> => 3600
    },
    
    {429, Headers, Body}.

extract_response_schema(Response) ->
    Content = maps:get(<<"content">>, Response, #{}),
    case maps:get(<<"application/json">>, Content, undefined) of
        undefined -> undefined;
        MediaType -> maps:get(<<"schema">>, MediaType, undefined)
    end.

%%====================================================================
%% Internal functions - Data Generation
%%====================================================================

generate_schema_data(#{<<"type">> := <<"object">>} = Schema, Options, State) ->
    Properties = maps:get(<<"properties">>, Schema, #{}),
    Required = maps:get(<<"required">>, Schema, []),
    
    maps:fold(fun(PropName, PropSchema, Acc) ->
        Value = generate_property_value(PropName, PropSchema, Options, State),
        maps:put(PropName, Value, Acc)
    end, #{}, Properties);

generate_schema_data(#{<<"type">> := <<"array">>} = Schema, Options, State) ->
    Items = maps:get(<<"items">>, Schema, #{}),
    MinItems = maps:get(<<"minItems">>, Schema, 0),
    MaxItems = maps:get(<<"maxItems">>, Schema, 10),
    
    Count = MinItems + rand:uniform(max(1, MaxItems - MinItems)),
    [generate_schema_data(Items, Options, State) || _ <- lists:seq(1, Count)];

generate_schema_data(Schema, Options, State) ->
    generate_value(Schema, Options, State).

generate_property_value(PropName, Schema, Options, State) ->
    %% Check for special property names that need realistic data
    case PropName of
        <<"id">> -> generate_id();
        <<"email">> -> generate_email(State);
        <<"firstName">> -> generate_first_name(State);
        <<"lastName">> -> generate_last_name(State);
        <<"name">> -> generate_full_name(State);
        <<"company">> -> generate_company_name(State);
        <<"phone">> -> generate_phone_number();
        <<"address">> -> generate_address(State);
        <<"city">> -> generate_city(State);
        <<"state">> -> generate_state(State);
        <<"zipCode">> -> generate_zip_code();
        <<"country">> -> <<"USA">>;
        <<"price">> -> generate_price();
        <<"quantity">> -> rand:uniform(100);
        <<"status">> -> generate_status(Schema);
        <<"createdAt">> -> iso8601_timestamp();
        <<"updatedAt">> -> iso8601_timestamp();
        _ -> generate_value(Schema, Options, State)
    end.

generate_value(#{<<"type">> := <<"string">>} = Schema, _Options, State) ->
    case maps:get(<<"enum">>, Schema, undefined) of
        undefined -> generate_string_value(Schema, State);
        Enum -> lists:nth(rand:uniform(length(Enum)), Enum)
    end;

generate_value(#{<<"type">> := <<"integer">>} = Schema, _Options, _State) ->
    Min = maps:get(<<"minimum">>, Schema, 0),
    Max = maps:get(<<"maximum">>, Schema, 1000000),
    Min + rand:uniform(Max - Min);

generate_value(#{<<"type">> := <<"number">>} = Schema, _Options, _State) ->
    Min = maps:get(<<"minimum">>, Schema, 0.0),
    Max = maps:get(<<"maximum">>, Schema, 1000000.0),
    Min + (Max - Min) * rand:uniform();

generate_value(#{<<"type">> := <<"boolean">>}, _Options, _State) ->
    rand:uniform() > 0.5;

generate_value(#{<<"$ref">> := Ref}, Options, State) ->
    %% Handle schema references
    TypeName = extract_type_name(Ref),
    generate_referenced_data(TypeName, Options, State);

generate_value(_, _, _) ->
    null.

generate_string_value(Schema, State) ->
    Format = maps:get(<<"format">>, Schema, undefined),
    
    case Format of
        <<"email">> -> generate_email(State);
        <<"date">> -> generate_date();
        <<"date-time">> -> iso8601_timestamp();
        <<"uuid">> -> generate_uuid();
        <<"uri">> -> generate_uri();
        <<"hostname">> -> generate_hostname();
        _ ->
            MinLength = maps:get(<<"minLength">>, Schema, 5),
            MaxLength = maps:get(<<"maxLength">>, Schema, 50),
            generate_lorem(MinLength, MaxLength, State)
    end.

%%====================================================================
%% Internal functions - Realistic Data Generators
%%====================================================================

generate_id() ->
    base64:encode(crypto:strong_rand_bytes(9)).

generate_uuid() ->
    agent_uuid:to_string(agent_uuid:uuid4()).

generate_email(State) ->
    FirstName = generate_first_name(State),
    LastName = generate_last_name(State),
    Domains = [<<"gmail.com">>, <<"yahoo.com">>, <<"hotmail.com">>, 
               <<"example.com">>, <<"company.com">>],
    Domain = lists:nth(rand:uniform(length(Domains)), Domains),
    
    <<(string:lowercase(FirstName))/binary, ".", 
      (string:lowercase(LastName))/binary, "@", Domain/binary>>.

generate_first_name(State) ->
    Names = maps:get(first_names, State#state.generators),
    lists:nth(rand:uniform(length(Names)), Names).

generate_last_name(State) ->
    Names = maps:get(last_names, State#state.generators),
    lists:nth(rand:uniform(length(Names)), Names).

generate_full_name(State) ->
    First = generate_first_name(State),
    Last = generate_last_name(State),
    <<First/binary, " ", Last/binary>>.

generate_company_name(State) ->
    Companies = maps:get(company_names, State#state.generators),
    lists:nth(rand:uniform(length(Companies)), Companies).

generate_phone_number() ->
    AreaCode = 200 + rand:uniform(799),
    Exchange = 200 + rand:uniform(799),
    Number = rand:uniform(9999),
    iolist_to_binary(io_lib:format("(~3..0B) ~3..0B-~4..0B", 
                                   [AreaCode, Exchange, Number])).

generate_address(State) ->
    StreetNum = integer_to_binary(rand:uniform(9999)),
    Streets = maps:get(street_names, State#state.generators),
    Types = maps:get(street_types, State#state.generators),
    Street = lists:nth(rand:uniform(length(Streets)), Streets),
    Type = lists:nth(rand:uniform(length(Types)), Types),
    
    #{
        <<"street">> => <<StreetNum/binary, " ", Street/binary, " ", Type/binary>>,
        <<"city">> => generate_city(State),
        <<"state">> => generate_state(State),
        <<"zipCode">> => generate_zip_code(),
        <<"country">> => <<"USA">>
    }.

generate_city(State) ->
    Cities = maps:get(cities, State#state.generators),
    lists:nth(rand:uniform(length(Cities)), Cities).

generate_state(State) ->
    States = maps:get(states, State#state.generators),
    lists:nth(rand:uniform(length(States)), States).

generate_zip_code() ->
    iolist_to_binary(io_lib:format("~5..0B", [rand:uniform(99999)])).

generate_price() ->
    %% Generate realistic prices
    BasePrice = rand:uniform(10000) / 100,
    %% Round to .99 or .95 endings
    Cents = case rand:uniform(3) of
        1 -> 0.99;
        2 -> 0.95;
        3 -> 0.00
    end,
    float_to_binary(erlang:floor(BasePrice) + Cents, [{decimals, 2}]).

generate_status(Schema) ->
    %% Use enum if available, otherwise common statuses
    case maps:get(<<"enum">>, Schema, undefined) of
        undefined ->
            Statuses = [<<"pending">>, <<"processing">>, <<"completed">>, 
                       <<"failed">>, <<"cancelled">>],
            lists:nth(rand:uniform(length(Statuses)), Statuses);
        Enum ->
            lists:nth(rand:uniform(length(Enum)), Enum)
    end.

generate_date() ->
    %% Generate date within last year
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Past = Now - rand:uniform(365 * 24 * 60 * 60),
    {{Y, M, D}, _} = calendar:gregorian_seconds_to_datetime(Past),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

iso8601_timestamp() ->
    Now = calendar:universal_time(),
    {{Y, M, D}, {H, Mi, S}} = Now,
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Y, M, D, H, Mi, S])).

generate_uri() ->
    Schemes = [<<"http">>, <<"https">>],
    Scheme = lists:nth(rand:uniform(length(Schemes)), Schemes),
    Host = generate_hostname(),
    Path = generate_path(),
    <<Scheme/binary, "://", Host/binary, Path/binary>>.

generate_hostname() ->
    Subdomains = [<<"www">>, <<"api">>, <<"app">>, <<"mail">>, <<>>],
    Domains = [<<"example">>, <<"test">>, <<"demo">>, <<"sample">>],
    Tlds = [<<"com">>, <<"org">>, <<"net">>, <<"io">>],
    
    Subdomain = lists:nth(rand:uniform(length(Subdomains)), Subdomains),
    Domain = lists:nth(rand:uniform(length(Domains)), Domains),
    Tld = lists:nth(rand:uniform(length(Tlds)), Tlds),
    
    case Subdomain of
        <<>> -> <<Domain/binary, ".", Tld/binary>>;
        _ -> <<Subdomain/binary, ".", Domain/binary, ".", Tld/binary>>
    end.

generate_path() ->
    Segments = [<<"api">>, <<"v1">>, <<"users">>, <<"products">>, <<"orders">>],
    NumSegments = rand:uniform(3),
    SelectedSegments = [lists:nth(rand:uniform(length(Segments)), Segments) 
                       || _ <- lists:seq(1, NumSegments)],
    iolist_to_binary(["/", lists:join("/", SelectedSegments)]).

generate_lorem(MinLength, MaxLength, State) ->
    Words = maps:get(lorem_words, State#state.generators),
    TargetLength = MinLength + rand:uniform(max(1, MaxLength - MinLength)),
    
    generate_lorem_text(Words, TargetLength, []).

generate_lorem_text(_, Length, Acc) when Length =< 0 ->
    iolist_to_binary(lists:join(" ", lists:reverse(Acc)));
generate_lorem_text(Words, Length, Acc) ->
    Word = lists:nth(rand:uniform(length(Words)), Words),
    generate_lorem_text(Words, Length - byte_size(Word) - 1, [Word | Acc]).

%%====================================================================
%% Internal functions - Related Data
%%====================================================================

generate_user(Id, Generators) ->
    #{
        id => integer_to_binary(Id),
        email => generate_email(#state{generators = Generators}),
        firstName => generate_first_name(#state{generators = Generators}),
        lastName => generate_last_name(#state{generators = Generators}),
        phone => generate_phone_number(),
        createdAt => iso8601_timestamp(),
        status => <<"active">>
    }.

generate_product(Id, Generators) ->
    ProductNames = maps:get(product_names, Generators),
    #{
        id => integer_to_binary(Id),
        name => lists:nth(rand:uniform(length(ProductNames)), ProductNames),
        description => generate_lorem(50, 200, #state{generators = Generators}),
        price => generate_price(),
        stock => rand:uniform(1000),
        category => generate_category(),
        createdAt => iso8601_timestamp()
    }.

generate_order(Id, Users, Products, Generators) ->
    User = lists:nth(rand:uniform(length(Users)), Users),
    NumItems = 1 + rand:uniform(5),
    Items = [generate_order_item(Products) || _ <- lists:seq(1, NumItems)],
    
    #{
        id => integer_to_binary(Id),
        userId => maps:get(id, User),
        items => Items,
        total => calculate_order_total(Items),
        status => generate_order_status(),
        createdAt => iso8601_timestamp()
    }.

generate_order_item(Products) ->
    Product = lists:nth(rand:uniform(length(Products)), Products),
    Quantity = 1 + rand:uniform(10),
    
    #{
        productId => maps:get(id, Product),
        quantity => Quantity,
        price => maps:get(price, Product),
        subtotal => calculate_subtotal(maps:get(price, Product), Quantity)
    }.

generate_category() ->
    Categories = [<<"Electronics">>, <<"Clothing">>, <<"Books">>, 
                 <<"Home & Garden">>, <<"Sports">>, <<"Toys">>],
    lists:nth(rand:uniform(length(Categories)), Categories).

generate_order_status() ->
    Statuses = [<<"pending">>, <<"processing">>, <<"shipped">>, 
               <<"delivered">>, <<"cancelled">>],
    lists:nth(rand:uniform(length(Statuses)), Statuses).

calculate_order_total(Items) ->
    Total = lists:foldl(fun(Item, Acc) ->
        Subtotal = maps:get(subtotal, Item),
        Acc + binary_to_float(Subtotal)
    end, 0.0, Items),
    float_to_binary(Total, [{decimals, 2}]).

calculate_subtotal(Price, Quantity) ->
    PriceFloat = binary_to_float(Price),
    float_to_binary(PriceFloat * Quantity, [{decimals, 2}]).

generate_referenced_data(TypeName, Options, State) ->
    %% Look up existing data or generate new
    case lookup_or_generate(TypeName, State) of
        {ok, Data} -> Data;
        error -> #{}
    end.

lookup_or_generate(TypeName, State) ->
    %% Try to find existing data
    Pattern = {{TypeName, '_'}, '_'},
    case ets:match_object(State#state.data_store, Pattern, 1) of
        {[{_, Data}], _} -> {ok, Data};
        _ -> error
    end.

%%====================================================================
%% Internal functions - Utilities
%%====================================================================

export_state(State) ->
    %% Export current mock state
    AllData = ets:tab2list(State#state.data_store),
    
    #{
        scenario => State#state.current_scenario,
        data => maps:from_list(AllData),
        statistics => #{
            total_records => ets:info(State#state.data_store, size)
        }
    }.

generate_request_id() ->
    base64:encode(crypto:strong_rand_bytes(12)).

extract_type_name(Ref) ->
    Parts = binary:split(Ref, <<"/">>, [global]),
    list_to_atom(binary_to_list(lists:last(Parts))).

floor(X) ->
    T = erlang:trunc(X),
    case X < T of
        true -> T - 1;
        false -> T
    end.