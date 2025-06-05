-module(model_definition_manager).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([create_model/1, update_model/2, get_model/1, get_all_models/0, delete_model/1]).
-export([validate_field/1, generate_code/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    models = #{} :: map(),
    storage_path = "priv/data/models.json" :: string()
}).

-record(model_definition, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    fields = [] :: list(),
    relationships = [] :: list(),
    metadata = #{} :: map(),
    created_at :: integer(),
    updated_at :: integer()
}).

-record(field_definition, {
    name :: binary(),
    type :: binary(),
    required = false :: boolean(),
    description = <<>> :: binary(),
    index_type = <<"none">> :: binary(),
    embedding_model = <<"none">> :: binary(),
    storage_backend = <<"memory">> :: binary(),
    metadata = #{} :: map()
}).

%%% API Functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

create_model(ModelData) ->
    gen_server:call(?MODULE, {create_model, ModelData}).

update_model(ModelId, ModelData) ->
    gen_server:call(?MODULE, {update_model, ModelId, ModelData}).

get_model(ModelId) ->
    gen_server:call(?MODULE, {get_model, ModelId}).

get_all_models() ->
    gen_server:call(?MODULE, get_all_models).

delete_model(ModelId) ->
    gen_server:call(?MODULE, {delete_model, ModelId}).

validate_field(FieldData) ->
    gen_server:call(?MODULE, {validate_field, FieldData}).

generate_code(ModelId, Language) ->
    gen_server:call(?MODULE, {generate_code, ModelId, Language}).

%%% GenServer Callbacks

init([]) ->
    ?LOG_INFO("Starting Model Definition Manager"),
    StoragePath = "apps/agent_web/priv/data/models.json",
    Models = load_models(StoragePath),
    {ok, #state{models = Models, storage_path = StoragePath}}.

handle_call({create_model, ModelData}, _From, State) ->
    try
        ModelId = generate_model_id(),
        Now = erlang:system_time(second),
        
        Fields = parse_fields(maps:get(fields, ModelData, [])),
        Relationships = parse_relationships(maps:get(relationships, ModelData, [])),
        
        Model = #model_definition{
            id = ModelId,
            name = maps:get(name, ModelData),
            description = maps:get(description, ModelData, <<>>),
            fields = Fields,
            relationships = Relationships,
            metadata = maps:get(metadata, ModelData, #{}),
            created_at = Now,
            updated_at = Now
        },
        
        NewModels = maps:put(ModelId, Model, State#state.models),
        save_models(NewModels, State#state.storage_path),
        
        ?LOG_INFO("Created model: ~p", [ModelId]),
        {reply, {ok, ModelId}, State#state{models = NewModels}}
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to create model: ~p:~p", [Error, Reason]),
            {reply, {error, {creation_failed, Reason}}, State}
    end;

handle_call({update_model, ModelId, ModelData}, _From, State) ->
    case maps:find(ModelId, State#state.models) of
        {ok, ExistingModel} ->
            try
                Now = erlang:system_time(second),
                Fields = parse_fields(maps:get(fields, ModelData, [])),
                Relationships = parse_relationships(maps:get(relationships, ModelData, [])),
                
                UpdatedModel = ExistingModel#model_definition{
                    name = maps:get(name, ModelData, ExistingModel#model_definition.name),
                    description = maps:get(description, ModelData, ExistingModel#model_definition.description),
                    fields = Fields,
                    relationships = Relationships,
                    metadata = maps:get(metadata, ModelData, ExistingModel#model_definition.metadata),
                    updated_at = Now
                },
                
                NewModels = maps:put(ModelId, UpdatedModel, State#state.models),
                save_models(NewModels, State#state.storage_path),
                
                ?LOG_INFO("Updated model: ~p", [ModelId]),
                {reply, {ok, ModelId}, State#state{models = NewModels}}
            catch
                Error:Reason ->
                    ?LOG_ERROR("Failed to update model ~p: ~p:~p", [ModelId, Error, Reason]),
                    {reply, {error, {update_failed, Reason}}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_model, ModelId}, _From, State) ->
    case maps:find(ModelId, State#state.models) of
        {ok, Model} ->
            {reply, {ok, model_to_map(Model)}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_all_models, _From, State) ->
    Models = maps:fold(fun(_Id, Model, Acc) ->
        [model_to_map(Model) | Acc]
    end, [], State#state.models),
    {reply, {ok, Models}, State};

handle_call({delete_model, ModelId}, _From, State) ->
    case maps:find(ModelId, State#state.models) of
        {ok, _Model} ->
            NewModels = maps:remove(ModelId, State#state.models),
            save_models(NewModels, State#state.storage_path),
            ?LOG_INFO("Deleted model: ~p", [ModelId]),
            {reply, ok, State#state{models = NewModels}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({validate_field, FieldData}, _From, State) ->
    try
        Field = parse_field(FieldData),
        Validation = validate_field_definition(Field),
        {reply, {ok, Validation}, State}
    catch
        Error:Reason ->
            ?LOG_ERROR("Field validation failed: ~p:~p", [Error, Reason]),
            {reply, {error, {validation_failed, Reason}}, State}
    end;

handle_call({generate_code, ModelId, Language}, _From, State) ->
    case maps:find(ModelId, State#state.models) of
        {ok, Model} ->
            try
                Code = generate_model_code(Model, Language),
                {reply, {ok, Code}, State}
            catch
                Error:Reason ->
                    ?LOG_ERROR("Code generation failed for model ~p: ~p:~p", [ModelId, Error, Reason]),
                    {reply, {error, {generation_failed, Reason}}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal Functions

generate_model_id() ->
    Timestamp = integer_to_binary(erlang:system_time(microsecond)),
    Random = integer_to_binary(rand:uniform(999999)),
    <<"model_", Timestamp/binary, "_", Random/binary>>.

parse_fields(FieldsData) when is_list(FieldsData) ->
    [parse_field(FieldData) || FieldData <- FieldsData].

parse_field(FieldData) when is_map(FieldData) ->
    #field_definition{
        name = maps:get(name, FieldData),
        type = maps:get(type, FieldData),
        required = maps:get(required, FieldData, false),
        description = maps:get(description, FieldData, <<>>),
        index_type = maps:get(index_type, FieldData, <<"none">>),
        embedding_model = maps:get(embedding_model, FieldData, <<"none">>),
        storage_backend = maps:get(storage_backend, FieldData, <<"memory">>),
        metadata = maps:get(metadata, FieldData, #{})
    }.

parse_relationships(RelationshipsData) when is_list(RelationshipsData) ->
    RelationshipsData.

validate_field_definition(Field) ->
    Validations = [
        validate_field_name(Field#field_definition.name),
        validate_field_type(Field#field_definition.type),
        validate_index_type(Field#field_definition.index_type),
        validate_embedding_model(Field#field_definition.embedding_model),
        validate_storage_backend(Field#field_definition.storage_backend)
    ],
    
    Errors = [Error || {error, Error} <- Validations],
    case Errors of
        [] -> #{valid => true, errors => []};
        _ -> #{valid => false, errors => Errors}
    end.

validate_field_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    ok;
validate_field_name(_) ->
    {error, <<"Field name must be a non-empty string">>}.

validate_field_type(Type) when Type =:= <<"string">>; Type =:= <<"text">>;
                               Type =:= <<"integer">>; Type =:= <<"boolean">>;
                               Type =:= <<"datetime">>; Type =:= <<"geo">> ->
    ok;
validate_field_type(_) ->
    {error, <<"Invalid field type">>}.

validate_index_type(IndexType) when IndexType =:= <<"none">>; IndexType =:= <<"exact">>;
                                   IndexType =:= <<"fulltext">>; IndexType =:= <<"vector">>;
                                   IndexType =:= <<"numeric">>; IndexType =:= <<"geo">>;
                                   IndexType =:= <<"datetime">> ->
    ok;
validate_index_type(_) ->
    {error, <<"Invalid index type">>}.

validate_embedding_model(Model) when Model =:= <<"none">>; Model =:= <<"text-embedding-3-small">>;
                                    Model =:= <<"text-embedding-3-large">>; Model =:= <<"text-embedding-ada-002">>;
                                    Model =:= <<"sentence-transformers">>; Model =:= <<"ollama-embed">> ->
    ok;
validate_embedding_model(_) ->
    {error, <<"Invalid embedding model">>}.

validate_storage_backend(Backend) when Backend =:= <<"memory">>; Backend =:= <<"redis">>;
                                      Backend =:= <<"postgres">>; Backend =:= <<"qdrant">>;
                                      Backend =:= <<"weaviate">>; Backend =:= <<"pinecone">> ->
    ok;
validate_storage_backend(_) ->
    {error, <<"Invalid storage backend">>}.

model_to_map(#model_definition{} = Model) ->
    #{
        id => Model#model_definition.id,
        name => Model#model_definition.name,
        description => Model#model_definition.description,
        fields => [field_to_map(Field) || Field <- Model#model_definition.fields],
        relationships => Model#model_definition.relationships,
        metadata => Model#model_definition.metadata,
        created_at => Model#model_definition.created_at,
        updated_at => Model#model_definition.updated_at
    }.

field_to_map(#field_definition{} = Field) ->
    #{
        name => Field#field_definition.name,
        type => Field#field_definition.type,
        required => Field#field_definition.required,
        description => Field#field_definition.description,
        index_type => Field#field_definition.index_type,
        embedding_model => Field#field_definition.embedding_model,
        storage_backend => Field#field_definition.storage_backend,
        metadata => Field#field_definition.metadata
    }.

load_models(StoragePath) ->
    case file:read_file(StoragePath) of
        {ok, Data} ->
            try
                DecodedData = jsx:decode(Data, [return_maps]),
                maps:fold(fun(Id, ModelData, Acc) ->
                    Model = map_to_model(ModelData),
                    maps:put(Id, Model, Acc)
                end, #{}, DecodedData)
            catch
                _:_ ->
                    ?LOG_WARNING("Failed to load models from ~p, starting with empty state", [StoragePath]),
                    #{}
            end;
        {error, enoent} ->
            ?LOG_INFO("Models file ~p does not exist, starting with empty state", [StoragePath]),
            #{};
        {error, Reason} ->
            ?LOG_ERROR("Failed to read models file ~p: ~p", [StoragePath, Reason]),
            #{}
    end.

save_models(Models, StoragePath) ->
    ModelsMap = maps:fold(fun(Id, Model, Acc) ->
        maps:put(Id, model_to_map(Model), Acc)
    end, #{}, Models),
    
    Data = jsx:encode(ModelsMap, [space, {indent, 2}]),
    
    ok = filelib:ensure_dir(StoragePath),
    case file:write_file(StoragePath, Data) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to save models to ~p: ~p", [StoragePath, Reason]),
            error
    end.

map_to_model(ModelData) when is_map(ModelData) ->
    Fields = [map_to_field(FieldData) || FieldData <- maps:get(fields, ModelData, [])],
    #model_definition{
        id = maps:get(id, ModelData),
        name = maps:get(name, ModelData),
        description = maps:get(description, ModelData, <<>>),
        fields = Fields,
        relationships = maps:get(relationships, ModelData, []),
        metadata = maps:get(metadata, ModelData, #{}),
        created_at = maps:get(created_at, ModelData, 0),
        updated_at = maps:get(updated_at, ModelData, 0)
    }.

map_to_field(FieldData) when is_map(FieldData) ->
    #field_definition{
        name = maps:get(name, FieldData),
        type = maps:get(type, FieldData),
        required = maps:get(required, FieldData, false),
        description = maps:get(description, FieldData, <<>>),
        index_type = maps:get(index_type, FieldData, <<"none">>),
        embedding_model = maps:get(embedding_model, FieldData, <<"none">>),
        storage_backend = maps:get(storage_backend, FieldData, <<"memory">>),
        metadata = maps:get(metadata, FieldData, #{})
    }.

generate_model_code(Model, <<"python">>) ->
    generate_python_code(Model);
generate_model_code(Model, <<"erlang">>) ->
    generate_erlang_code(Model);
generate_model_code(_Model, Language) ->
    throw({unsupported_language, Language}).

generate_python_code(Model) ->
    FieldsCode = string:join([generate_python_field(Field) || Field <- Model#model_definition.fields], ",\n    "),
    iolist_to_binary([
        "from pydantic import BaseModel, Field\n",
        "from typing import Optional\n",
        "from datetime import datetime\n\n",
        "class ", Model#model_definition.name, "(BaseModel):\n",
        "    \"\"\"", Model#model_definition.description, "\"\"\"\n",
        "    ", FieldsCode, "\n\n",
        "    class Config:\n",
        "        json_encoders = {\n",
        "            datetime: lambda v: v.isoformat()\n",
        "        }\n"
    ]).

generate_python_field(Field) ->
    TypeMapping = #{
        <<"string">> => "str",
        <<"text">> => "str",
        <<"integer">> => "int",
        <<"boolean">> => "bool",
        <<"datetime">> => "datetime",
        <<"geo">> => "tuple[float, float]"
    },
    
    PythonType = maps:get(Field#field_definition.type, TypeMapping, "str"),
    OptionalType = case Field#field_definition.required of
        true -> PythonType;
        false -> "Optional[" ++ PythonType ++ "]"
    end,
    
    DefaultValue = case Field#field_definition.required of
        true -> "";
        false -> " = None"
    end,
    
    FieldParams = case Field#field_definition.description of
        <<>> -> "";
        Desc -> ", description=\"" ++ binary_to_list(Desc) ++ "\""
    end,
    
    binary_to_list(Field#field_definition.name) ++ ": " ++ OptionalType ++ 
    " = Field(" ++ DefaultValue ++ FieldParams ++ ")".

generate_erlang_code(Model) ->
    RecordFields = string:join([generate_erlang_field(Field) || Field <- Model#model_definition.fields], ",\n    "),
    iolist_to_binary([
        "%% Generated model: ", Model#model_definition.name, "\n",
        "%% ", Model#model_definition.description, "\n\n",
        "-record(", Model#model_definition.name, ", {\n",
        "    ", RecordFields, "\n",
        "}).\n"
    ]).

generate_erlang_field(Field) ->
    DefaultValue = case Field#field_definition.required of
        true -> "";
        false -> " = undefined"
    end,
    
    Comment = case Field#field_definition.description of
        <<>> -> "";
        Desc -> " %% " ++ binary_to_list(Desc)
    end,
    
    binary_to_list(Field#field_definition.name) ++ DefaultValue ++ " :: " ++
    erlang_type_for_field(Field#field_definition.type) ++ Comment.

erlang_type_for_field(<<"string">>) -> "binary()";
erlang_type_for_field(<<"text">>) -> "binary()";
erlang_type_for_field(<<"integer">>) -> "integer()";
erlang_type_for_field(<<"boolean">>) -> "boolean()";
erlang_type_for_field(<<"datetime">>) -> "integer()";
erlang_type_for_field(<<"geo">>) -> "{float(), float()}";
erlang_type_for_field(_) -> "term()".