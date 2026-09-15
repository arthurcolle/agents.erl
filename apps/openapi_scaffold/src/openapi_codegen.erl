%%%-------------------------------------------------------------------
%%% @doc OpenAPI Code Generator
%%% Generates Erlang modules and records from OpenAPI schemas
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_codegen).
-behaviour(gen_server).

-export([start_link/0, generate_from_spec/2, get_generated_modules/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(CALL_TIMEOUT, 5000).

-record(state, {
    generated_modules = #{} :: #{binary() => [binary()]}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec generate_from_spec(binary(), map()) -> {ok, [binary()]} | {error, term()}.
generate_from_spec(SpecId, Spec) ->
    gen_server:call(?MODULE, {generate, SpecId, Spec}, 60000).

-spec get_generated_modules(binary()) -> {ok, [binary()]} | {error, not_found}.
get_generated_modules(SpecId) ->
    gen_server:call(?MODULE, {get_modules, SpecId}, ?CALL_TIMEOUT).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Ensure generated directory exists
    case ensure_generated_dir() of
        ok ->
            {ok, #state{}};
        {error, Reason} ->
            {stop, {generated_dir_unavailable, Reason}}
    end.

handle_call({generate, SpecId, Spec}, _From, State) ->
    case do_generate(SpecId, Spec) of
        {ok, Modules} ->
            NewState = State#state{
                generated_modules = maps:put(SpecId, Modules, State#state.generated_modules)
            },
            {reply, {ok, Modules}, NewState};
        Error ->
            {reply, Error, State}
    end;

handle_call({get_modules, SpecId}, _From, #state{generated_modules = Modules} = State) ->
    case maps:find(SpecId, Modules) of
        {ok, ModuleList} ->
            {reply, {ok, ModuleList}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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

do_generate(SpecId, Spec) ->
    try
        %% Generate different types of modules
        SchemaModules = generate_schema_modules(SpecId, Spec),
        HandlerModules = generate_handler_modules(SpecId, Spec),
        ClientModule = generate_client_module(SpecId, Spec),
        
        AllModules = SchemaModules ++ HandlerModules ++ [ClientModule],
        
        %% Compile all generated modules
        lists:foreach(fun compile_module/1, AllModules),
        
        {ok, AllModules}
    catch
        error:Reason ->
            {error, {generation_failed, Reason}}
    end.

generate_schema_modules(SpecId, Spec) ->
    Components = maps:get(<<"components">>, Spec, #{}),
    Schemas = maps:get(<<"schemas">>, Components, #{}),
    
    maps:fold(fun(SchemaName, Schema, Acc) ->
        ModuleName = schema_to_module_name(SpecId, SchemaName),
        generate_schema_module(ModuleName, SchemaName, Schema),
        [ModuleName | Acc]
    end, [], Schemas).

generate_schema_module(ModuleName, SchemaName, Schema) ->
    %% Generate record definition
    RecordName = erlang_identifier(SchemaName),
    Fields = extract_fields(Schema),
    
    ModuleCode = [
        "-module(", module_name_text(ModuleName), ").\n",
        "-export([new/0, new/1, to_map/1, from_map/1, validate/1]).\n\n",
        generate_record_definition(RecordName, Fields),
        "\n",
        generate_new_functions(RecordName, Fields),
        "\n",
        generate_conversion_functions(RecordName, Fields),
        "\n",
        generate_validation_function(RecordName, Fields, Schema)
    ],
    
    write_module(ModuleName, ModuleCode).

generate_handler_modules(SpecId, Spec) ->
    Paths = maps:get(<<"paths">>, Spec, #{}),
    
    maps:fold(fun(Path, PathItem, Acc) ->
        Operations = extract_operations(PathItem),
        case Operations of
            [] -> Acc;
            _ ->
                HandlerModule = path_to_handler_name(SpecId, Path),
                generate_handler_module(HandlerModule, Path, Operations, Spec),
                [HandlerModule | Acc]
        end
    end, [], Paths).

generate_handler_module(ModuleName, Path, Operations, Spec) ->
    ModuleCode = [
        "-module(", module_name_text(ModuleName), ").\n",
        "-behaviour(cowboy_handler).\n\n",
        "-export([init/2]).\n\n",
        "init(Req, State) ->\n",
        "    Method = cowboy_req:method(Req),\n",
        "    handle_method(Method, Req, State).\n\n",
        generate_method_handlers(Operations, Spec)
    ],
    
    write_module(ModuleName, ModuleCode).

generate_client_module(SpecId, Spec) ->
    ModuleName = generated_module_name(<<(SpecId)/binary, "_client">>),
    Servers = maps:get(<<"servers">>, Spec, []),
    Paths = maps:get(<<"paths">>, Spec, #{}),
    
    ModuleCode = [
        "-module(", module_name_text(ModuleName), ").\n",
        "-export([new/0, new/1]).\n",
        generate_api_exports(Paths),
        "\n\n",
        "-record(client, {\n",
        "    base_url :: binary(),\n",
        "    headers :: [{binary(), binary()}],\n",
        "    timeout :: pos_integer()\n",
        "}).\n\n",
        generate_constructor_functions(Servers),
        "\n",
        generate_api_functions(Paths, Spec)
    ],
    
    write_module(ModuleName, ModuleCode),
    ModuleName.

extract_fields(Schema) ->
    Properties = maps:get(<<"properties">>, Schema, #{}),
    Required = maps:get(<<"required">>, Schema, []),
    
    maps:fold(fun(PropName, PropSchema, Acc) ->
        Type = maps:get(<<"type">>, PropSchema, <<"any">>),
        IsRequired = lists:member(PropName, Required),
        [{PropName, Type, IsRequired} | Acc]
    end, [], Properties).

extract_operations(PathItem) ->
    Methods = [<<"get">>, <<"post">>, <<"put">>, <<"delete">>, <<"patch">>, <<"head">>, <<"options">>],
    lists:filtermap(fun(Method) ->
        case maps:find(Method, PathItem) of
            {ok, Operation} ->
                {true, {Method, Operation}};
            error ->
                false
        end
    end, Methods).

generate_record_definition(RecordName, Fields) ->
    FieldDefs = lists:map(fun({FieldName, Type, _Required}) ->
        FieldIdentifier = erlang_identifier(FieldName),
        TypeSpec = type_to_spec(Type),
        io_lib:format("    ~s :: ~s", [FieldIdentifier, TypeSpec])
    end, Fields),
    
    [
        "-record(", RecordName, ", {\n",
        string:join(FieldDefs, ",\n"),
        "\n})."
    ].

generate_new_functions(RecordName, Fields) ->
    DefaultValues = lists:map(fun({FieldName, Type, Required}) ->
        FieldIdentifier = erlang_identifier(FieldName),
        Default = default_value(Type, Required),
        io_lib:format("        ~s = ~p", [FieldIdentifier, Default])
    end, Fields),
    
    [
        "new() ->\n",
        "    #", RecordName, "{\n",
        string:join(DefaultValues, ",\n"),
        "\n    }.\n\n",
        "new(Props) when is_list(Props) ->\n",
        "    new(maps:from_list(Props));\n",
        "new(Map) when is_map(Map) ->\n",
        "    from_map(Map)."
    ].

generate_conversion_functions(RecordName, Fields) ->
    RecordVar = ["#", RecordName],
    
    %% to_map function
    ToMapFields = lists:map(fun({FieldName, _, _}) ->
        FieldIdentifier = erlang_identifier(FieldName),
        io_lib:format("        ~p => Record#~s.~s", [FieldName, RecordName, FieldIdentifier])
    end, Fields),
    
    %% from_map function
    FromMapFields = lists:map(fun({FieldName, _, _}) ->
        FieldIdentifier = erlang_identifier(FieldName),
        io_lib:format("        ~s = maps:get(~p, Map, undefined)", [FieldIdentifier, FieldName])
    end, Fields),
    
    [
        "to_map(", RecordVar, "{} = Record) ->\n",
        "    #{\n",
        string:join(ToMapFields, ",\n"),
        "\n    }.\n\n",
        "from_map(Map) when is_map(Map) ->\n",
        "    #", RecordName, "{\n",
        string:join(FromMapFields, ",\n"),
        "\n    }."
    ].

generate_validation_function(RecordName, Fields, Schema) ->
    RecordVar = ["#", RecordName],
    
    ValidationChecks = lists:map(fun({FieldName, Type, Required}) ->
        FieldIdentifier = erlang_identifier(FieldName),
        generate_field_validation(RecordName, FieldIdentifier, Type, Required)
    end, Fields),
    
    [
        "validate(", RecordVar, "{} = Record) ->\n",
        "    Errors = lists:flatten([\n",
        string:join(ValidationChecks, ",\n"),
        "\n    ]),\n",
        "    case Errors of\n",
        "        [] -> ok;\n",
        "        _ -> {error, Errors}\n",
        "    end."
    ].

generate_field_validation(RecordName, FieldIdentifier, _Type, Required) ->
    FieldVar = ["Record#", RecordName, ".", FieldIdentifier],
    RequiredCheck = case Required of
        true ->
            io_lib:format("        case ~s of undefined -> {~s, required}; _ -> [] end",
                         [FieldVar, FieldIdentifier]);
        false ->
            "        []"
    end,
    RequiredCheck.

generate_method_handlers(Operations, Spec) ->
    Handlers = lists:map(fun({Method, Operation}) ->
        MethodUpper = string:uppercase(binary_to_list(Method)),
        generate_method_handler(MethodUpper, Operation, Spec)
    end, Operations),
    
    [
        string:join(Handlers, "\n\n"),
        "\n\n",
        "handle_method(_, Req, State) ->\n",
        "    {ok, cowboy_req:reply(405, #{}, <<\"Method Not Allowed\">>, Req), State}."
    ].

generate_method_handler(Method, Operation, Spec) ->
    OperationId = maps:get(<<"operationId">>, Operation, <<"unknown">>),
    
    [
        "handle_method(<<\"", Method, "\">>, Req0, State) ->\n",
        "    %% Operation: ", io_lib:format("~p", [OperationId]), "\n",
        "    case openapi_validator:validate_request(Req0, ", 
            io_lib:format("~p", [Operation]), ") of\n",
        "        {ok, ValidatedReq} ->\n",
        "            %% TODO: Implement actual logic or proxy\n",
        "            Response = #{<<\"message\">> => <<\"Not implemented\">>},\n",
        "            {ok, cowboy_req:reply(501, \n",
        "                #{<<\"content-type\">> => <<\"application/json\">>},\n",
        "                jsx:encode(Response), ValidatedReq), State};\n",
        "        {error, Errors} ->\n",
        "            {ok, cowboy_req:reply(400, \n",
        "                #{<<\"content-type\">> => <<\"application/json\">>},\n",
        "                jsx:encode(#{<<\"errors\">> => Errors}), Req0), State}\n",
        "    end;"
    ].

generate_api_exports(Paths) ->
    Exports = maps:fold(fun(Path, PathItem, Acc) ->
        Operations = extract_operations(PathItem),
        lists:foldl(fun({Method, Op}, AccIn) ->
            case maps:find(<<"operationId">>, Op) of
                {ok, OpId} ->
                    FuncName = operation_id_to_function(OpId),
                    Arity = case maps:get(<<"parameters">>, Op, []) of
                        [] -> 1;
                        Params -> length(Params) + 1
                    end,
                    [io_lib:format("-export([~s/~p]).\n", [FuncName, Arity]) | AccIn];
                error ->
                    AccIn
            end
        end, Acc, Operations)
    end, [], Paths),
    
    lists:reverse(Exports).

generate_constructor_functions(Servers) ->
    BaseUrl = case Servers of
        [#{<<"url">> := Url} | _] when is_binary(Url) -> Url;
        _ -> <<"http://localhost">>
    end,
    
    [
        "new() ->\n",
        "    new(", io_lib:format("~p", [BaseUrl]), ").\n\n",
        "new(BaseUrl) ->\n",
        "    #client{\n",
        "        base_url = BaseUrl,\n",
        "        headers = [{<<\"User-Agent\">>, <<\"OpenAPI-Scaffold/1.0\">>}],\n",
        "        timeout = 30000\n",
        "    }."
    ].

generate_api_functions(Paths, Spec) ->
    Functions = maps:fold(fun(Path, PathItem, Acc) ->
        Operations = extract_operations(PathItem),
        lists:foldl(fun({Method, Op}, AccIn) ->
            case maps:find(<<"operationId">>, Op) of
                {ok, OpId} ->
                    [generate_api_function(Path, Method, Op, Spec) | AccIn];
                error ->
                    AccIn
            end
        end, Acc, Operations)
    end, [], Paths),
    
    string:join(lists:reverse(Functions), "\n\n").

generate_api_function(Path, Method, Operation, Spec) ->
    OpId = maps:get(<<"operationId">>, Operation),
    FuncName = operation_id_to_function(OpId),
    Parameters = maps:get(<<"parameters">>, Operation, []),
    
    %% Generate function signature
    ParamNames = lists:map(fun(P) ->
        erlang_identifier(maps:get(<<"name">>, P, <<"param">>))
    end, Parameters),
    
    Args = ["Client" | ParamNames],
    
    [
        FuncName, "(", string:join(Args, ", "), ") ->\n",
        "    %% TODO: Implement API call\n",
        "    {error, not_implemented}."
    ].

%% Helper functions

schema_to_module_name(SpecId, SchemaName) ->
    generated_module_name(iolist_to_binary([
        SpecId,
        <<"_schema_">>,
        erlang_identifier(SchemaName)
    ])).

path_to_handler_name(SpecId, Path) ->
    %% Convert path to valid module name
    CleanPath = binary:replace(Path, [<<"/">>, <<"{">>, <<"}">>, <<"-">>], <<"_">>, [global]),
    ModuleName = <<SpecId/binary, "_handler", CleanPath/binary>>,
    generated_module_name(ModuleName).

operation_id_to_function(OpId) ->
    %% Convert operationId to a bounded, syntactically valid function name.
    erlang_identifier(OpId).

type_to_spec(<<"string">>) -> "binary()";
type_to_spec(<<"integer">>) -> "integer()";
type_to_spec(<<"number">>) -> "number()";
type_to_spec(<<"boolean">>) -> "boolean()";
type_to_spec(<<"array">>) -> "list()";
type_to_spec(<<"object">>) -> "map()";
type_to_spec(_) -> "any()".

default_value(<<"string">>, true) -> <<"">>;
default_value(<<"string">>, false) -> undefined;
default_value(<<"integer">>, true) -> 0;
default_value(<<"integer">>, false) -> undefined;
default_value(<<"number">>, true) -> 0.0;
default_value(<<"number">>, false) -> undefined;
default_value(<<"boolean">>, true) -> false;
default_value(<<"boolean">>, false) -> undefined;
default_value(<<"array">>, true) -> [];
default_value(<<"array">>, false) -> undefined;
default_value(<<"object">>, true) -> #{};
default_value(<<"object">>, false) -> undefined;
default_value(_, _) -> undefined.

write_module(ModuleName, Code) ->
    GenDir = generated_dir(),
    FileName = filename:join(GenDir, module_name_text(ModuleName) ++ ".erl"),
    case file:write_file(FileName, Code) of
        ok ->
            ok;
        {error, Reason} ->
            error({write_failed, FileName, Reason})
    end.

compile_module(ModuleName) ->
    GenDir = generated_dir(),
    FileName = filename:join(GenDir, module_name_text(ModuleName) ++ ".erl"),
    
    case compile:file(FileName, [binary, return_errors]) of
        {ok, CompiledModule, Binary} ->
            case code:load_binary(CompiledModule, FileName, Binary) of
                {module, _} ->
                    ok;
                {error, Reason} ->
                    error({load_failed, ModuleName, Reason})
            end;
        {error, Errors, _} ->
            error({compilation_failed, ModuleName, Errors})
    end.

generated_dir() ->
    case application:get_env(openapi_scaffold, generated_dir, "src/generated") of
        Dir when is_binary(Dir) -> binary_to_list(Dir);
        Dir when is_list(Dir) -> Dir;
        _ -> "src/generated"
    end.

ensure_generated_dir() ->
    GenDir = generated_dir(),
    filelib:ensure_dir(filename:join(GenDir, ".keep")).

generated_module_name(Value) ->
    %% Keep generated identifiers bounded and syntactically valid. The Erlang
    %% compiler allocates the module atom only after this validation, avoiding
    %% atom allocation for malformed or unbounded specification input.
    Name = erlang_identifier(Value),
    case Name of
        [First | _] when First >= $a, First =< $z -> list_to_binary(Name);
        _ -> list_to_binary("generated_" ++ Name)
    end.

module_name_text(Value) when is_binary(Value) -> binary_to_list(Value);
module_name_text(Value) when is_list(Value) -> Value;
module_name_text(Value) when is_atom(Value) -> atom_to_list(Value).

erlang_identifier(Value) when is_binary(Value) ->
    erlang_identifier(binary_to_list(Value));
erlang_identifier(Value) when is_list(Value) ->
    Lower = string:lowercase(Value),
    Sanitized = [case C of
        Ch when (Ch >= $a andalso Ch =< $z) orelse
                (Ch >= $A andalso Ch =< $Z) orelse
                (Ch >= $0 andalso Ch =< $9) orelse Ch =:= $_ -> Ch;
        _ -> $_
    end || C <- Lower],
    Bounded = lists:sublist(Sanitized, 120),
    case Bounded of
        [] -> "field";
        [First | _] when First >= $a, First =< $z ->
            case is_reserved_identifier(Bounded) of
                true -> "field_" ++ Bounded;
                false -> Bounded
            end;
        _ -> "field_" ++ Bounded
    end;
erlang_identifier(Value) when is_atom(Value) -> erlang_identifier(atom_to_list(Value));
erlang_identifier(_) -> "field".

is_reserved_identifier(Name) ->
    lists:member(Name, [
        "after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl",
        "bsr", "bxor", "case", "catch", "cond", "div", "end", "fun", "if",
        "let", "maybe", "not", "of", "or", "orelse", "receive", "rem", "try",
        "when", "xor"
    ]).
