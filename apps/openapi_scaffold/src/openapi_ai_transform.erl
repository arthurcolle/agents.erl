%%%-------------------------------------------------------------------
%%% @doc AI-Powered Request/Response Transformation
%%% Uses AI models to intelligently transform API requests and responses,
%%% handle data migration, and provide semantic understanding.
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_ai_transform).
-behaviour(gen_server).

-export([
    start_link/0,
    transform_request/3,
    transform_response/3,
    generate_migration/3,
    semantic_validation/2,
    auto_fix_request/2,
    explain_error/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    ai_client :: pid(),
    cache :: ets:tid(),
    transformations :: map(),
    metrics :: pid()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Transform request using AI understanding
transform_request(Operation, Request, Options) ->
    gen_server:call(?MODULE, {transform_request, Operation, Request, Options}).

%% @doc Transform response for better usability
transform_response(Operation, Response, Options) ->
    gen_server:call(?MODULE, {transform_response, Operation, Response, Options}).

%% @doc Generate migration between API versions
generate_migration(OldSpec, NewSpec, Options) ->
    gen_server:call(?MODULE, {generate_migration, OldSpec, NewSpec, Options}, 60000).

%% @doc Semantic validation using AI
semantic_validation(Data, Schema) ->
    gen_server:call(?MODULE, {semantic_validation, Data, Schema}).

%% @doc Auto-fix malformed requests
auto_fix_request(Request, Schema) ->
    gen_server:call(?MODULE, {auto_fix_request, Request, Schema}).

%% @doc Generate human-readable error explanations
explain_error(Error, Context) ->
    gen_server:call(?MODULE, {explain_error, Error, Context}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize ETS cache for transformation patterns
    Cache = ets:new(ai_transform_cache, [set, protected, {read_concurrency, true}]),
    
    %% Start AI client
    {ok, AIClient} = start_ai_client(),
    
    %% Initialize metrics
    Metrics = openapi_metrics:register_transformer(?MODULE),
    
    State = #state{
        ai_client = AIClient,
        cache = Cache,
        transformations = #{},
        metrics = Metrics
    },
    
    {ok, State}.

handle_call({transform_request, Operation, Request, Options}, _From, State) ->
    StartTime = erlang:monotonic_time(microsecond),
    
    %% Check cache first
    CacheKey = {transform_request, Operation, erlang:phash2(Request)},
    Result = case ets:lookup(State#state.cache, CacheKey) of
        [{_, CachedResult}] -> 
            openapi_metrics:record_cache_hit(State#state.metrics, transform_request),
            CachedResult;
        [] ->
            %% Use AI to transform
            TransformResult = ai_transform_request(State#state.ai_client, Operation, Request, Options),
            ets:insert(State#state.cache, {CacheKey, TransformResult}),
            openapi_metrics:record_cache_miss(State#state.metrics, transform_request),
            TransformResult
    end,
    
    Duration = erlang:monotonic_time(microsecond) - StartTime,
    openapi_metrics:record_transformation_time(State#state.metrics, transform_request, Duration),
    
    {reply, Result, State};

handle_call({transform_response, Operation, Response, Options}, _From, State) ->
    %% Apply AI-powered transformations
    Transformed = case maps:get(transform_type, Options, auto) of
        auto ->
            %% Let AI decide the best transformation
            auto_transform_response(State#state.ai_client, Operation, Response);
        flatten ->
            %% Flatten nested structures
            flatten_response(State#state.ai_client, Response);
        enrich ->
            %% Add additional context and metadata
            enrich_response(State#state.ai_client, Operation, Response);
        simplify ->
            %% Simplify for frontend consumption
            simplify_response(State#state.ai_client, Response)
    end,
    
    {reply, {ok, Transformed}, State};

handle_call({generate_migration, OldSpec, NewSpec, Options}, _From, State) ->
    %% Use AI to understand schema changes and generate migration code
    MigrationPlan = analyze_schema_changes(State#state.ai_client, OldSpec, NewSpec),
    
    %% Generate migration functions
    MigrationCode = case maps:get(target_language, Options, erlang) of
        erlang -> generate_erlang_migration(MigrationPlan);
        javascript -> generate_js_migration(MigrationPlan);
        sql -> generate_sql_migration(MigrationPlan);
        _ -> {error, unsupported_language}
    end,
    
    {reply, MigrationCode, State};

handle_call({semantic_validation, Data, Schema}, _From, State) ->
    %% Use AI to perform semantic validation beyond JSON schema
    ValidationResult = perform_semantic_validation(State#state.ai_client, Data, Schema),
    {reply, ValidationResult, State};

handle_call({auto_fix_request, Request, Schema}, _From, State) ->
    %% Use AI to fix common issues in requests
    FixedRequest = ai_fix_request(State#state.ai_client, Request, Schema),
    {reply, FixedRequest, State};

handle_call({explain_error, Error, Context}, _From, State) ->
    %% Generate human-readable error explanation
    Explanation = generate_error_explanation(State#state.ai_client, Error, Context),
    {reply, {ok, Explanation}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - AI Client
%%====================================================================

start_ai_client() ->
    %% Initialize OpenAI/Anthropic client for transformations
    Config = #{
        api_key => os:getenv("OPENAI_API_KEY"),
        model => "gpt-4",
        temperature => 0.3,
        max_tokens => 2000
    },
    openai_client:start_link(Config).

%%====================================================================
%% Internal functions - Request Transformation
%%====================================================================

ai_transform_request(AIClient, Operation, Request, Options) ->
    %% Build context for AI
    Context = #{
        operation => Operation,
        request => Request,
        options => Options,
        instruction => <<"Transform this API request to match the expected format. 
                         Fix any issues, add missing required fields with sensible defaults,
                         and ensure data types are correct.">>
    },
    
    %% Get AI transformation
    case ai_complete(AIClient, Context) of
        {ok, TransformedRequest} ->
            %% Validate the transformation
            case validate_transformation(TransformedRequest, Operation) of
                ok -> {ok, TransformedRequest};
                {error, Reason} -> 
                    %% Try to fix validation errors
                    fix_and_retry(AIClient, TransformedRequest, Reason, Operation)
            end;
        {error, Reason} ->
            {error, {ai_transformation_failed, Reason}}
    end.

fix_and_retry(AIClient, Request, ValidationError, Operation) ->
    Context = #{
        request => Request,
        error => ValidationError,
        operation => Operation,
        instruction => <<"The transformed request has validation errors. 
                         Please fix these specific issues and return a valid request.">>
    },
    
    case ai_complete(AIClient, Context) of
        {ok, FixedRequest} -> {ok, FixedRequest};
        {error, _} -> {error, {cannot_fix_request, ValidationError}}
    end.

%%====================================================================
%% Internal functions - Response Transformation
%%====================================================================

auto_transform_response(AIClient, Operation, Response) ->
    %% Let AI decide the best transformation based on the response structure
    Context = #{
        operation => Operation,
        response => Response,
        instruction => <<"Analyze this API response and apply the most appropriate transformation:
                         1. If deeply nested, flatten it for easier consumption
                         2. If sparse, enrich with helpful metadata
                         3. If complex, simplify while preserving essential data
                         4. If already well-structured, enhance with computed fields">>
    },
    
    {ok, Transformed} = ai_complete(AIClient, Context),
    Transformed.

flatten_response(AIClient, Response) ->
    Context = #{
        response => Response,
        instruction => <<"Flatten this nested response structure into a single level object.
                         Use dot notation for nested field names (e.g., 'user.address.city').
                         Preserve all data but make it easily accessible.">>
    },
    
    {ok, Flattened} = ai_complete(AIClient, Context),
    Flattened.

enrich_response(AIClient, Operation, Response) ->
    Context = #{
        operation => Operation,
        response => Response,
        instruction => <<"Enrich this response with additional helpful information:
                         1. Add human-readable descriptions for codes/enums
                         2. Include calculated fields (e.g., age from birthdate)
                         3. Add relevant links and references
                         4. Include helpful metadata about the data">>
    },
    
    {ok, Enriched} = ai_complete(AIClient, Context),
    Enriched.

simplify_response(AIClient, Response) ->
    Context = #{
        response => Response,
        instruction => <<"Simplify this response for frontend consumption:
                         1. Remove internal/technical fields
                         2. Rename fields to be more user-friendly
                         3. Group related data logically
                         4. Format dates/numbers for display">>
    },
    
    {ok, Simplified} = ai_complete(AIClient, Context),
    Simplified.

%%====================================================================
%% Internal functions - Migration Generation
%%====================================================================

analyze_schema_changes(AIClient, OldSpec, NewSpec) ->
    Context = #{
        old_spec => OldSpec,
        new_spec => NewSpec,
        instruction => <<"Analyze the differences between these two API specifications.
                         Identify:
                         1. Added/removed endpoints
                         2. Changed request/response schemas
                         3. Modified authentication methods
                         4. Breaking vs non-breaking changes
                         Return a structured migration plan.">>
    },
    
    {ok, MigrationPlan} = ai_complete(AIClient, Context),
    MigrationPlan.

generate_erlang_migration(MigrationPlan) ->
    %% Generate Erlang migration module
    Code = [
        "-module(api_migration_v1_to_v2).\n",
        "-export([migrate_request/2, migrate_response/2]).\n\n",
        generate_erlang_migration_functions(MigrationPlan)
    ],
    {ok, iolist_to_binary(Code)}.

generate_js_migration(MigrationPlan) ->
    %% Generate JavaScript migration code
    Code = [
        "// API Migration from v1 to v2\n",
        "export const migrateRequest = (endpoint, request) => {\n",
        generate_js_migration_logic(MigrationPlan, request),
        "};\n\n",
        "export const migrateResponse = (endpoint, response) => {\n",
        generate_js_migration_logic(MigrationPlan, response),
        "};\n"
    ],
    {ok, iolist_to_binary(Code)}.

generate_sql_migration(MigrationPlan) ->
    %% Generate SQL migration scripts
    Code = [
        "-- API Data Migration Script\n",
        "BEGIN TRANSACTION;\n\n",
        generate_sql_migration_statements(MigrationPlan),
        "\nCOMMIT;\n"
    ],
    {ok, iolist_to_binary(Code)}.

%%====================================================================
%% Internal functions - Semantic Validation
%%====================================================================

perform_semantic_validation(AIClient, Data, Schema) ->
    Context = #{
        data => Data,
        schema => Schema,
        instruction => <<"Perform semantic validation on this data:
                         1. Check if enum values make logical sense
                         2. Verify date ranges are reasonable
                         3. Ensure numeric values are within expected bounds
                         4. Check for logical inconsistencies between fields
                         5. Validate business rules that can't be expressed in JSON Schema
                         Return validation results with specific issues found.">>
    },
    
    case ai_complete(AIClient, Context) of
        {ok, ValidationResult} ->
            parse_validation_result(ValidationResult);
        {error, Reason} ->
            {error, {semantic_validation_failed, Reason}}
    end.

parse_validation_result(Result) ->
    %% Parse AI response into structured validation result
    case Result of
        #{<<"valid">> := true} ->
            {ok, valid};
        #{<<"valid">> := false, <<"errors">> := Errors} ->
            {error, {validation_errors, Errors}};
        _ ->
            {error, invalid_validation_response}
    end.

%%====================================================================
%% Internal functions - Request Fixing
%%====================================================================

ai_fix_request(AIClient, Request, Schema) ->
    Context = #{
        request => Request,
        schema => Schema,
        instruction => <<"Fix this malformed request to match the schema:
                         1. Add missing required fields with sensible defaults
                         2. Fix data type mismatches
                         3. Remove unknown fields
                         4. Ensure format compliance (dates, emails, etc.)
                         5. Fix common typos in field names
                         Return the corrected request.">>
    },
    
    case ai_complete(AIClient, Context) of
        {ok, FixedRequest} -> {ok, FixedRequest};
        {error, Reason} -> {error, {cannot_fix_request, Reason}}
    end.

%%====================================================================
%% Internal functions - Error Explanation
%%====================================================================

generate_error_explanation(AIClient, Error, Context) ->
    ExplainContext = #{
        error => Error,
        context => Context,
        instruction => <<"Generate a helpful, human-readable explanation for this error:
                         1. Explain what went wrong in simple terms
                         2. Suggest specific steps to fix the issue
                         3. Provide example of correct usage if applicable
                         4. Include relevant documentation references
                         Make it friendly and helpful, not technical.">>
    },
    
    case ai_complete(AIClient, ExplainContext) of
        {ok, Explanation} ->
            format_error_explanation(Explanation, Error);
        {error, _} ->
            %% Fallback to basic explanation
            basic_error_explanation(Error)
    end.

format_error_explanation(Explanation, OriginalError) ->
    #{
        error => OriginalError,
        explanation => Explanation,
        timestamp => erlang:system_time(second),
        helpful_links => generate_helpful_links(OriginalError)
    }.

basic_error_explanation(Error) ->
    #{
        error => Error,
        explanation => <<"An error occurred. Please check your request and try again.">>,
        timestamp => erlang:system_time(second)
    }.

%%====================================================================
%% Internal functions - AI Communication
%%====================================================================

ai_complete(AIClient, Context) ->
    %% Format context as prompt
    Prompt = format_ai_prompt(Context),
    
    %% Call AI API
    case openai_client:chat_completion(AIClient, Prompt) of
        {ok, Response} ->
            %% Parse AI response as JSON
            try
                {ok, jsx:decode(Response, [return_maps])}
            catch
                _:_ -> 
                    %% If not JSON, return as text
                    {ok, Response}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

format_ai_prompt(#{instruction := Instruction} = Context) ->
    %% Build a structured prompt for the AI
    ContextStr = maps:fold(fun(K, V, Acc) when K =/= instruction ->
        [io_lib:format("~p: ~p~n", [K, V]) | Acc]
    end, [], Context),
    
    [
        "You are an API transformation expert. ",
        binary_to_list(Instruction), "\n\n",
        "Context:\n",
        ContextStr,
        "\n\nRespond with valid JSON where possible."
    ].

%%====================================================================
%% Internal functions - Utilities
%%====================================================================

validate_transformation(TransformedRequest, Operation) ->
    %% Basic validation of transformed request
    %% In production, this would use the actual OpenAPI schema
    case maps:is_key(required_fields, Operation) of
        true ->
            RequiredFields = maps:get(required_fields, Operation),
            check_required_fields(TransformedRequest, RequiredFields);
        false ->
            ok
    end.

check_required_fields(Request, RequiredFields) ->
    Missing = [Field || Field <- RequiredFields, not maps:is_key(Field, Request)],
    case Missing of
        [] -> ok;
        _ -> {error, {missing_required_fields, Missing}}
    end.

generate_helpful_links(Error) ->
    %% Generate documentation links based on error type
    BaseUrl = "https://api-docs.example.com/",
    
    case Error of
        {missing_required_fields, _} ->
            [<<BaseUrl/binary, "guides/required-fields">>];
        {validation_errors, _} ->
            [<<BaseUrl/binary, "guides/validation">>];
        {unauthorized, _} ->
            [<<BaseUrl/binary, "guides/authentication">>];
        _ ->
            [<<BaseUrl/binary, "guides/troubleshooting">>]
    end.

generate_erlang_migration_functions(MigrationPlan) ->
    %% Generate actual migration functions based on plan
    %% This is simplified - real implementation would be more complex
    [
        "migrate_request(Endpoint, Request) ->\n",
        "    case Endpoint of\n",
        generate_endpoint_migrations(maps:get(endpoints, MigrationPlan, []), request),
        "        _ -> Request\n",
        "    end.\n\n",
        "migrate_response(Endpoint, Response) ->\n",
        "    case Endpoint of\n",
        generate_endpoint_migrations(maps:get(endpoints, MigrationPlan, []), response),
        "        _ -> Response\n",
        "    end.\n"
    ].

generate_endpoint_migrations(Endpoints, Type) ->
    lists:map(fun(#{path := Path, changes := Changes}) ->
        Migrations = [C || C <- Changes, maps:get(type, C) == Type],
        case Migrations of
            [] -> "";
            _ -> generate_migration_case(Path, Migrations)
        end
    end, Endpoints).

generate_migration_case(Path, Migrations) ->
    io_lib:format("        ~p ->~n            ~s;~n", 
                  [Path, generate_migration_body(Migrations)]).

generate_migration_body(Migrations) ->
    %% Generate actual transformation code
    "Request". % Simplified

generate_js_migration_logic(MigrationPlan, Type) ->
    "    // Migration logic here\n    return request;\n".

generate_sql_migration_statements(MigrationPlan) ->
    "-- Migration statements here\n".