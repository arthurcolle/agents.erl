-module(embedding_integration).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([compute_embedding/2, compute_embeddings_batch/2, search_similar/3]).
-export([register_model/2, get_available_models/0, get_model_info/1]).
-export([create_vector_index/2, add_to_index/3, search_index/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    models = #{} :: map(),
    indices = #{} :: map(),
    cache = #{} :: map(),
    cache_ttl = 3600 :: integer(),  % 1 hour cache TTL
    storage_path = "priv/data/embeddings.json" :: string()
}).

-record(embedding_model, {
    name :: binary(),
    provider :: binary(),
    dimensions :: integer(),
    max_tokens :: integer(),
    cost_per_token :: float(),
    endpoint :: binary(),
    api_key :: binary() | undefined,
    metadata = #{} :: map()
}).

-record(vector_index, {
    name :: binary(),
    model :: binary(),
    vectors = #{} :: map(),
    metadata = #{} :: map(),
    created_at :: integer(),
    updated_at :: integer()
}).

%%% API Functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

compute_embedding(Text, ModelName) ->
    gen_server:call(?MODULE, {compute_embedding, Text, ModelName}, 30000).

compute_embeddings_batch(Texts, ModelName) ->
    gen_server:call(?MODULE, {compute_embeddings_batch, Texts, ModelName}, 60000).

search_similar(QueryVector, IndexName, TopK) ->
    gen_server:call(?MODULE, {search_similar, QueryVector, IndexName, TopK}).

register_model(ModelName, ModelConfig) ->
    gen_server:call(?MODULE, {register_model, ModelName, ModelConfig}).

get_available_models() ->
    gen_server:call(?MODULE, get_available_models).

get_model_info(ModelName) ->
    gen_server:call(?MODULE, {get_model_info, ModelName}).

create_vector_index(IndexName, ModelName) ->
    gen_server:call(?MODULE, {create_vector_index, IndexName, ModelName}).

add_to_index(IndexName, Id, Text) ->
    gen_server:call(?MODULE, {add_to_index, IndexName, Id, Text}).

search_index(IndexName, QueryText, TopK) ->
    gen_server:call(?MODULE, {search_index, IndexName, QueryText, TopK}).

%%% GenServer Callbacks

init([]) ->
    ?LOG_INFO("Starting Embedding Integration System"),
    StoragePath = "apps/agent_web/priv/data/embeddings.json",
    State = load_embeddings_state(StoragePath),
    DefaultModels = get_default_models(),
    UpdatedState = State#state{models = DefaultModels},
    save_embeddings_state(UpdatedState),
    {ok, UpdatedState}.

handle_call({compute_embedding, Text, ModelName}, _From, State) ->
    try
        case maps:find(ModelName, State#state.models) of
            {ok, Model} ->
                CacheKey = {ModelName, Text},
                case check_cache(CacheKey, State#state.cache) of
                    {hit, Vector} ->
                        ?LOG_INFO("Cache hit for embedding: ~p", [ModelName]),
                        {reply, {ok, Vector}, State};
                    miss ->
                        case compute_embedding_internal(Text, Model) of
                            {ok, Vector} ->
                                NewCache = update_cache(CacheKey, Vector, State#state.cache),
                                NewState = State#state{cache = NewCache},
                                save_embeddings_state(NewState),
                                ?LOG_INFO("Computed embedding for text (~p chars) using ~p", [byte_size(Text), ModelName]),
                                {reply, {ok, Vector}, NewState};
                            {error, Reason} ->
                                ?LOG_ERROR("Failed to compute embedding with ~p: ~p", [ModelName, Reason]),
                                {reply, {error, Reason}, State}
                        end
                end;
            error ->
                {reply, {error, {model_not_found, ModelName}}, State}
        end
    catch
        Error1:Reason1 ->
            ?LOG_ERROR("Exception in compute_embedding: ~p:~p", [Error1, Reason1]),
            {reply, {error, {computation_failed, Reason1}}, State}
    end;

handle_call({compute_embeddings_batch, Texts, ModelName}, _From, State) ->
    try
        case maps:find(ModelName, State#state.models) of
            {ok, Model} ->
                Results = lists:map(fun(Text) ->
                    CacheKey = {ModelName, Text},
                    case check_cache(CacheKey, State#state.cache) of
                        {hit, Vector} -> {ok, Vector};
                        miss -> compute_embedding_internal(Text, Model)
                    end
                end, Texts),
                
                % Update cache for new embeddings
                NewCache = lists:foldl(fun({Text, Result}, CacheAcc) ->
                    case Result of
                        {ok, Vector} ->
                            update_cache({ModelName, Text}, Vector, CacheAcc);
                        _ -> CacheAcc
                    end
                end, State#state.cache, lists:zip(Texts, Results)),
                
                NewState = State#state{cache = NewCache},
                save_embeddings_state(NewState),
                
                ?LOG_INFO("Computed ~p embeddings using ~p", [length(Texts), ModelName]),
                {reply, {ok, Results}, NewState};
            error ->
                {reply, {error, {model_not_found, ModelName}}, State}
        end
    catch
        Error2:Reason2 ->
            ?LOG_ERROR("Exception in compute_embeddings_batch: ~p:~p", [Error2, Reason2]),
            {reply, {error, {computation_failed, Reason2}}, State}
    end;

handle_call({search_similar, QueryVector, IndexName, TopK}, _From, State) ->
    try
        case maps:find(IndexName, State#state.indices) of
            {ok, Index} ->
                Similarities = compute_similarities(QueryVector, Index#vector_index.vectors),
                TopResults = lists:sublist(lists:sort(fun({_, Sim1}, {_, Sim2}) ->
                    Sim1 >= Sim2
                end, Similarities), TopK),
                {reply, {ok, TopResults}, State};
            error ->
                {reply, {error, {index_not_found, IndexName}}, State}
        end
    catch
        Error3:Reason3 ->
            ?LOG_ERROR("Exception in search_similar: ~p:~p", [Error3, Reason3]),
            {reply, {error, {search_failed, Reason3}}, State}
    end;

handle_call({register_model, ModelName, ModelConfig}, _From, State) ->
    try
        Model = #embedding_model{
            name = ModelName,
            provider = maps:get(provider, ModelConfig),
            dimensions = maps:get(dimensions, ModelConfig),
            max_tokens = maps:get(max_tokens, ModelConfig, 8192),
            cost_per_token = maps:get(cost_per_token, ModelConfig, 0.0),
            endpoint = maps:get(endpoint, ModelConfig, <<>>),
            api_key = maps:get(api_key, ModelConfig, undefined),
            metadata = maps:get(metadata, ModelConfig, #{})
        },
        
        NewModels = maps:put(ModelName, Model, State#state.models),
        NewState = State#state{models = NewModels},
        save_embeddings_state(NewState),
        
        ?LOG_INFO("Registered embedding model: ~p", [ModelName]),
        {reply, ok, NewState}
    catch
        Error6:Reason6 ->
            ?LOG_ERROR("Failed to register model ~p: ~p:~p", [ModelName, Error6, Reason6]),
            {reply, {error, {registration_failed, Reason6}}, State}
    end;

handle_call(get_available_models, _From, State) ->
    Models = maps:fold(fun(Name, Model, Acc) ->
        [#{
            name => Name,
            provider => Model#embedding_model.provider,
            dimensions => Model#embedding_model.dimensions,
            max_tokens => Model#embedding_model.max_tokens,
            cost_per_token => Model#embedding_model.cost_per_token,
            metadata => Model#embedding_model.metadata
        } | Acc]
    end, [], State#state.models),
    {reply, {ok, Models}, State};

handle_call({get_model_info, ModelName}, _From, State) ->
    case maps:find(ModelName, State#state.models) of
        {ok, Model} ->
            ModelInfo = #{
                name => Model#embedding_model.name,
                provider => Model#embedding_model.provider,
                dimensions => Model#embedding_model.dimensions,
                max_tokens => Model#embedding_model.max_tokens,
                cost_per_token => Model#embedding_model.cost_per_token,
                endpoint => Model#embedding_model.endpoint,
                metadata => Model#embedding_model.metadata
            },
            {reply, {ok, ModelInfo}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({create_vector_index, IndexName, ModelName}, _From, State) ->
    try
        case maps:find(ModelName, State#state.models) of
            {ok, _Model} ->
                Now = erlang:system_time(second),
                Index = #vector_index{
                    name = IndexName,
                    model = ModelName,
                    vectors = #{},
                    metadata = #{},
                    created_at = Now,
                    updated_at = Now
                },
                
                NewIndices = maps:put(IndexName, Index, State#state.indices),
                NewState = State#state{indices = NewIndices},
                save_embeddings_state(NewState),
                
                ?LOG_INFO("Created vector index: ~p using model ~p", [IndexName, ModelName]),
                {reply, ok, NewState};
            error ->
                {reply, {error, {model_not_found, ModelName}}, State}
        end
    catch
        Error7:Reason7 ->
            ?LOG_ERROR("Failed to create index ~p: ~p:~p", [IndexName, Error7, Reason7]),
            {reply, {error, {creation_failed, Reason7}}, State}
    end;

handle_call({add_to_index, IndexName, Id, Text}, _From, State) ->
    try
        case maps:find(IndexName, State#state.indices) of
            {ok, Index} ->
                ModelName = Index#vector_index.model,
                case compute_embedding(Text, ModelName) of
                    {ok, Vector} ->
                        NewVectors = maps:put(Id, Vector, Index#vector_index.vectors),
                        UpdatedIndex = Index#vector_index{
                            vectors = NewVectors,
                            updated_at = erlang:system_time(second)
                        },
                        NewIndices = maps:put(IndexName, UpdatedIndex, State#state.indices),
                        NewState = State#state{indices = NewIndices},
                        save_embeddings_state(NewState),
                        
                        ?LOG_INFO("Added vector to index ~p: ~p", [IndexName, Id]),
                        {reply, ok, NewState};
                    {error, Reason} ->
                        {reply, {error, {embedding_failed, Reason}}, State}
                end;
            error ->
                {reply, {error, {index_not_found, IndexName}}, State}
        end
    catch
        Error4:Reason4 ->
            ?LOG_ERROR("Failed to add to index ~p: ~p:~p", [IndexName, Error4, Reason4]),
            {reply, {error, {addition_failed, Reason4}}, State}
    end;

handle_call({search_index, IndexName, QueryText, TopK}, _From, State) ->
    try
        case maps:find(IndexName, State#state.indices) of
            {ok, Index} ->
                ModelName = Index#vector_index.model,
                case compute_embedding(QueryText, ModelName) of
                    {ok, QueryVector} ->
                        Similarities = compute_similarities(QueryVector, Index#vector_index.vectors),
                        TopResults = lists:sublist(lists:sort(fun({_, Sim1}, {_, Sim2}) ->
                            Sim1 >= Sim2
                        end, Similarities), TopK),
                        {reply, {ok, TopResults}, State};
                    {error, Reason} ->
                        {reply, {error, {embedding_failed, Reason}}, State}
                end;
            error ->
                {reply, {error, {index_not_found, IndexName}}, State}
        end
    catch
        Error5:Reason5 ->
            ?LOG_ERROR("Failed to search index ~p: ~p:~p", [IndexName, Error5, Reason5]),
            {reply, {error, {search_failed, Reason5}}, State}
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

get_default_models() ->
    #{
        <<"text-embedding-3-small">> => #embedding_model{
            name = <<"text-embedding-3-small">>,
            provider = <<"openai">>,
            dimensions = 1536,
            max_tokens = 8192,
            cost_per_token = 0.00002,
            endpoint = <<"https://api.openai.com/v1/embeddings">>,
            api_key = get_openai_api_key(),
            metadata = #{<<"description">> => <<"OpenAI text-embedding-3-small model">>}
        },
        <<"text-embedding-3-large">> => #embedding_model{
            name = <<"text-embedding-3-large">>,
            provider = <<"openai">>,
            dimensions = 3072,
            max_tokens = 8192,
            cost_per_token = 0.00013,
            endpoint = <<"https://api.openai.com/v1/embeddings">>,
            api_key = get_openai_api_key(),
            metadata = #{<<"description">> => <<"OpenAI text-embedding-3-large model">>}
        },
        <<"text-embedding-ada-002">> => #embedding_model{
            name = <<"text-embedding-ada-002">>,
            provider = <<"openai">>,
            dimensions = 1536,
            max_tokens = 8192,
            cost_per_token = 0.0001,
            endpoint = <<"https://api.openai.com/v1/embeddings">>,
            api_key = get_openai_api_key(),
            metadata = #{<<"description">> => <<"OpenAI text-embedding-ada-002 model">>}
        },
        <<"sentence-transformers">> => #embedding_model{
            name = <<"sentence-transformers">>,
            provider = <<"local">>,
            dimensions = 768,
            max_tokens = 512,
            cost_per_token = 0.0,
            endpoint = <<"http://localhost:8001/embeddings">>,
            api_key = undefined,
            metadata = #{<<"description">> => <<"Local sentence-transformers model">>}
        },
        <<"ollama-embed">> => #embedding_model{
            name = <<"ollama-embed">>,
            provider = <<"ollama">>,
            dimensions = 4096,
            max_tokens = 2048,
            cost_per_token = 0.0,
            endpoint = <<"http://localhost:11434/api/embeddings">>,
            api_key = undefined,
            metadata = #{<<"description">> => <<"Ollama local embedding model">>}
        }
    }.

get_openai_api_key() ->
    case os:getenv("OPENAI_API_KEY") of
        false -> undefined;
        Key -> list_to_binary(Key)
    end.

compute_embedding_internal(Text, #embedding_model{provider = <<"openai">>} = Model) ->
    compute_openai_embedding(Text, Model);
compute_embedding_internal(Text, #embedding_model{provider = <<"local">>} = Model) ->
    compute_local_embedding(Text, Model);
compute_embedding_internal(Text, #embedding_model{provider = <<"ollama">>} = Model) ->
    compute_ollama_embedding(Text, Model);
compute_embedding_internal(_Text, Model) ->
    {error, {unsupported_provider, Model#embedding_model.provider}}.

compute_openai_embedding(Text, Model) ->
    case Model#embedding_model.api_key of
        undefined ->
            {error, missing_api_key};
        ApiKey ->
            Headers = [
                {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
                {"Content-Type", "application/json"}
            ],
            
            Body = jsx:encode(#{
                <<"input">> => Text,
                <<"model">> => Model#embedding_model.name,
                <<"encoding_format">> => <<"float">>
            }),
            
            case httpc:request(post, {binary_to_list(Model#embedding_model.endpoint), Headers, "application/json", Body}, [], []) of
                {ok, {{_, 200, _}, _, ResponseBody}} ->
                    try
                        Response = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
                        Data = maps:get(<<"data">>, Response),
                        [FirstEmbedding | _] = Data,
                        Vector = maps:get(<<"embedding">>, FirstEmbedding),
                        {ok, Vector}
                    catch
                        _:Error ->
                            {error, {parsing_failed, Error}}
                    end;
                {ok, {{_, StatusCode, _}, _, ErrorBody}} ->
                    {error, {api_error, StatusCode, ErrorBody}};
                {error, Reason} ->
                    {error, {request_failed, Reason}}
            end
    end.

compute_local_embedding(Text, Model) ->
    % Mock implementation for local embedding models
    % In a real implementation, this would call a local embedding service
    Dimensions = Model#embedding_model.dimensions,
    Vector = [rand:uniform() - 0.5 || _ <- lists:seq(1, Dimensions)],
    % Normalize the vector
    Magnitude = math:sqrt(lists:sum([X * X || X <- Vector])),
    NormalizedVector = [X / Magnitude || X <- Vector],
    {ok, NormalizedVector}.

compute_ollama_embedding(Text, Model) ->
    % Mock implementation for Ollama embedding models
    % In a real implementation, this would call the Ollama API
    Dimensions = Model#embedding_model.dimensions,
    Vector = [rand:uniform() - 0.5 || _ <- lists:seq(1, Dimensions)],
    % Normalize the vector
    Magnitude = math:sqrt(lists:sum([X * X || X <- Vector])),
    NormalizedVector = [X / Magnitude || X <- Vector],
    {ok, NormalizedVector}.

check_cache(Key, Cache) ->
    case maps:find(Key, Cache) of
        {ok, {Vector, Timestamp}} ->
            Now = erlang:system_time(second),
            if
                Now - Timestamp < 3600 -> % 1 hour TTL
                    {hit, Vector};
                true ->
                    miss
            end;
        error ->
            miss
    end.

update_cache(Key, Vector, Cache) ->
    Timestamp = erlang:system_time(second),
    maps:put(Key, {Vector, Timestamp}, Cache).

compute_similarities(QueryVector, VectorMap) ->
    maps:fold(fun(Id, Vector, Acc) ->
        Similarity = cosine_similarity(QueryVector, Vector),
        [{Id, Similarity} | Acc]
    end, [], VectorMap).

cosine_similarity(Vector1, Vector2) when length(Vector1) =:= length(Vector2) ->
    DotProduct = lists:sum([X * Y || {X, Y} <- lists:zip(Vector1, Vector2)]),
    Magnitude1 = math:sqrt(lists:sum([X * X || X <- Vector1])),
    Magnitude2 = math:sqrt(lists:sum([X * X || X <- Vector2])),
    
    case {Magnitude1, Magnitude2} of
        {+0.0, _} -> 0.0;
        {_, +0.0} -> 0.0;
        _ -> DotProduct / (Magnitude1 * Magnitude2)
    end;
cosine_similarity(_, _) ->
    0.0.

load_embeddings_state(StoragePath) ->
    case file:read_file(StoragePath) of
        {ok, Data} ->
            try
                DecodedData = jsx:decode(Data, [return_maps]),
                #state{
                    models = load_models(maps:get(<<"models">>, DecodedData, #{})),
                    indices = load_indices(maps:get(<<"indices">>, DecodedData, #{})),
                    cache = #{}, % Don't persist cache
                    storage_path = StoragePath
                }
            catch
                _:_ ->
                    ?LOG_WARNING("Failed to load embeddings state from ~p, starting with empty state", [StoragePath]),
                    #state{storage_path = StoragePath}
            end;
        {error, enoent} ->
            ?LOG_INFO("Embeddings state file ~p does not exist, starting with empty state", [StoragePath]),
            #state{storage_path = StoragePath};
        {error, Reason} ->
            ?LOG_ERROR("Failed to read embeddings state file ~p: ~p", [StoragePath, Reason]),
            #state{storage_path = StoragePath}
    end.

load_models(ModelsMap) ->
    maps:fold(fun(Name, ModelData, Acc) ->
        Model = map_to_embedding_model(ModelData),
        maps:put(Name, Model, Acc)
    end, #{}, ModelsMap).

load_indices(IndicesMap) ->
    maps:fold(fun(Name, IndexData, Acc) ->
        Index = map_to_vector_index(IndexData),
        maps:put(Name, Index, Acc)
    end, #{}, IndicesMap).

save_embeddings_state(State) ->
    EmbeddingsData = #{
        <<"models">> => maps:fold(fun(Name, Model, Acc) ->
            maps:put(Name, embedding_model_to_map(Model), Acc)
        end, #{}, State#state.models),
        <<"indices">> => maps:fold(fun(Name, Index, Acc) ->
            maps:put(Name, vector_index_to_map(Index), Acc)
        end, #{}, State#state.indices)
    },
    
    Data = jsx:encode(EmbeddingsData, [space, {indent, 2}]),
    
    ok = filelib:ensure_dir(State#state.storage_path),
    case file:write_file(State#state.storage_path, Data) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to save embeddings state to ~p: ~p", [State#state.storage_path, Reason]),
            error
    end.

embedding_model_to_map(#embedding_model{} = Model) ->
    #{
        name => Model#embedding_model.name,
        provider => Model#embedding_model.provider,
        dimensions => Model#embedding_model.dimensions,
        max_tokens => Model#embedding_model.max_tokens,
        cost_per_token => Model#embedding_model.cost_per_token,
        endpoint => Model#embedding_model.endpoint,
        api_key => Model#embedding_model.api_key,
        metadata => Model#embedding_model.metadata
    }.

map_to_embedding_model(ModelData) ->
    #embedding_model{
        name = maps:get(name, ModelData),
        provider = maps:get(provider, ModelData),
        dimensions = maps:get(dimensions, ModelData),
        max_tokens = maps:get(max_tokens, ModelData, 8192),
        cost_per_token = maps:get(cost_per_token, ModelData, 0.0),
        endpoint = maps:get(endpoint, ModelData, <<>>),
        api_key = maps:get(api_key, ModelData, undefined),
        metadata = maps:get(metadata, ModelData, #{})
    }.

vector_index_to_map(#vector_index{} = Index) ->
    #{
        name => Index#vector_index.name,
        model => Index#vector_index.model,
        vectors => Index#vector_index.vectors,
        metadata => Index#vector_index.metadata,
        created_at => Index#vector_index.created_at,
        updated_at => Index#vector_index.updated_at
    }.

map_to_vector_index(IndexData) ->
    #vector_index{
        name = maps:get(name, IndexData),
        model = maps:get(model, IndexData),
        vectors = maps:get(vectors, IndexData, #{}),
        metadata = maps:get(metadata, IndexData, #{}),
        created_at = maps:get(created_at, IndexData, 0),
        updated_at = maps:get(updated_at, IndexData, 0)
    }.