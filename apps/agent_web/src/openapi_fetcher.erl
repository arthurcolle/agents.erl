%%%-------------------------------------------------------------------
%%% @doc OpenAPI Schema Fetcher V2 - Fixed version
%%% Fetches and parses OpenAPI specifications from various sources
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_fetcher).
-behaviour(gen_server).

-export([start_link/0]).
-export([
    fetch_schema/1,
    parse_schema/1,
    get_cached_schema/1,
    clear_cache/0,
    add_schema_source/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    schemas = #{} :: map(),
    sources = #{} :: map(),
    cache_dir :: string()
}).

-define(DEFAULT_SOURCES, #{
    openai => "https://raw.githubusercontent.com/openai/openai-openapi/master/openapi.yaml",
    anthropic => "https://raw.githubusercontent.com/anthropics/anthropic-sdk-python/main/openapi.yaml",
    github => "https://raw.githubusercontent.com/github/rest-api-description/main/descriptions/api.github.com/api.github.com.yaml"
}).

%% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

fetch_schema(Source) ->
    gen_server:call(?MODULE, {fetch_schema, Source}, 30000).

parse_schema(SchemaData) ->
    gen_server:call(?MODULE, {parse_schema, SchemaData}).

get_cached_schema(Source) ->
    gen_server:call(?MODULE, {get_cached_schema, Source}).

clear_cache() ->
    gen_server:call(?MODULE, clear_cache).

add_schema_source(Name, Url) ->
    gen_server:call(?MODULE, {add_source, Name, Url}).

%% gen_server callbacks

init([]) ->
    % Ensure inets application is started for HTTP client
    ok = application:ensure_started(inets),
    ok = application:ensure_started(ssl),
    
    CacheDir = filename:join([code:priv_dir(agent_web), "cache", "schemas"]),
    filelib:ensure_dir(filename:join(CacheDir, "dummy")),
    
    State = #state{
        schemas = #{},
        sources = ?DEFAULT_SOURCES,
        cache_dir = CacheDir
    },
    
    {ok, State}.

handle_call({fetch_schema, Source}, _From, State) ->
    case fetch_and_cache_schema(Source, State) of
        {ok, Schema} ->
            NewSchemas = maps:put(Source, Schema, State#state.schemas),
            {reply, {ok, Schema}, State#state{schemas = NewSchemas}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({parse_schema, SchemaData}, _From, State) ->
    case parse_openapi_spec(SchemaData) of
        {ok, ParsedSchema} ->
            {reply, {ok, ParsedSchema}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_cached_schema, Source}, _From, State) ->
    Reply = case maps:get(Source, State#state.schemas, undefined) of
        undefined ->
            % Try to load from cache file
            case load_cached_schema(Source, State#state.cache_dir) of
                {ok, Schema} -> {ok, Schema};
                {error, _} -> {error, not_found}
            end;
        Schema ->
            {ok, Schema}
    end,
    {reply, Reply, State};

handle_call(clear_cache, _From, State) ->
    % Clear memory cache
    NewState = State#state{schemas = #{}},
    
    % Clear disk cache
    CacheFiles = filelib:wildcard(filename:join(State#state.cache_dir, "*.yaml")),
    lists:foreach(fun file:delete/1, CacheFiles),
    
    {reply, ok, NewState};

handle_call({add_source, Name, Url}, _From, State) ->
    NewSources = maps:put(Name, Url, State#state.sources),
    {reply, ok, State#state{sources = NewSources}};

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

%% Internal functions

fetch_and_cache_schema(Source, State) ->
    case maps:get(Source, State#state.sources, undefined) of
        undefined ->
            {error, unknown_source};
        Url ->
            case fetch_schema_from_url(Url) of
                {ok, SchemaData} ->
                    % Cache to disk
                    CacheFile = filename:join(State#state.cache_dir, atom_to_list(Source) ++ ".yaml"),
                    file:write_file(CacheFile, SchemaData),
                    
                    % Parse the schema
                    parse_openapi_spec(SchemaData);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

fetch_schema_from_url(Url) ->
    % Set SSL options for HTTPS
    SslOpts = [
        {ssl, [
            {verify, verify_none},
            {versions, ['tlsv1.2', 'tlsv1.3']},
            {ciphers, ssl:cipher_suites(all, 'tlsv1.3')}
        ]}
    ],
    
    HttpOpts = [
        {timeout, 30000},
        {connect_timeout, 10000}
    ],
    
    case httpc:request(get, {Url, []}, HttpOpts ++ SslOpts, [{body_format, binary}]) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_, StatusCode, Reason}, _, _}} ->
            {error, {http_error, StatusCode, Reason}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

load_cached_schema(Source, CacheDir) ->
    CacheFile = filename:join(CacheDir, atom_to_list(Source) ++ ".yaml"),
    case file:read_file(CacheFile) of
        {ok, Data} ->
            parse_openapi_spec(Data);
        {error, Reason} ->
            {error, Reason}
    end.

parse_openapi_spec(YamlData) when is_binary(YamlData) ->
    try
        % Parse YAML - yamerl returns a list of documents
        case yamerl_constr:string(binary_to_list(YamlData)) of
            [Doc] ->
                % Convert to maps
                Schema = yaml_doc_to_map(Doc),
                
                % Extract and structure the schema
                ParsedSchema = #{
                    openapi => maps:get(<<"openapi">>, Schema, <<"3.0.0">>),
                    info => maps:get(<<"info">>, Schema, #{}),
                    servers => maps:get(<<"servers">>, Schema, []),
                    paths => parse_paths(maps:get(<<"paths">>, Schema, #{})),
                    components => maps:get(<<"components">>, Schema, #{}),
                    tags => maps:get(<<"tags">>, Schema, [])
                },
                
                {ok, ParsedSchema};
            [] ->
                {error, empty_yaml};
            _ ->
                {error, multiple_yaml_documents}
        end
    catch
        Error:Reason:Stacktrace ->
            error_logger:error_msg("YAML parsing error: ~p:~p~n~p~n", [Error, Reason, Stacktrace]),
            {error, {parse_error, Error, Reason}}
    end.

parse_paths(Paths) when is_map(Paths) ->
    maps:fold(fun(Path, PathItem, Acc) ->
        ParsedOps = parse_path_operations(Path, PathItem),
        maps:put(Path, ParsedOps, Acc)
    end, #{}, Paths);
parse_paths(_) -> #{}.

parse_path_operations(Path, PathItem) when is_map(PathItem) ->
    Operations = [<<"get">>, <<"post">>, <<"put">>, <<"delete">>, <<"patch">>, <<"options">>, <<"head">>],
    
    lists:foldl(fun(Method, Acc) ->
        case maps:get(Method, PathItem, undefined) of
            undefined -> Acc;
            OpData when is_map(OpData) ->
                ParsedOp = #{
                    method => Method,
                    path => Path,
                    operation_id => maps:get(<<"operationId">>, OpData, <<>>),
                    summary => maps:get(<<"summary">>, OpData, <<>>),
                    description => maps:get(<<"description">>, OpData, <<>>),
                    tags => maps:get(<<"tags">>, OpData, []),
                    parameters => parse_parameters(maps:get(<<"parameters">>, OpData, [])),
                    request_body => maps:get(<<"requestBody">>, OpData, undefined),
                    responses => maps:get(<<"responses">>, OpData, #{}),
                    security => maps:get(<<"security">>, OpData, [])
                },
                [{binary_to_atom(Method, utf8), ParsedOp} | Acc];
            _ -> Acc
        end
    end, [], Operations);
parse_path_operations(_, _) -> [].

parse_parameters(Params) when is_list(Params) ->
    lists:map(fun(Param) when is_map(Param) ->
        #{
            name => maps:get(<<"name">>, Param, <<>>),
            in => maps:get(<<"in">>, Param, <<>>),
            required => maps:get(<<"required">>, Param, false),
            schema => maps:get(<<"schema">>, Param, #{}),
            description => maps:get(<<"description">>, Param, <<>>)
        };
    (_) -> #{}
    end, Params);
parse_parameters(_) -> [].

% Convert yamerl parsed data to Erlang maps
yaml_doc_to_map(Doc) when is_list(Doc) ->
    % Check if it's a proplist (list of tuples)
    case is_yaml_proplist(Doc) of
        true ->
            maps:from_list([{ensure_binary(K), yaml_doc_to_map(V)} || {K, V} <- Doc]);
        false ->
            [yaml_doc_to_map(Item) || Item <- Doc]
    end;
yaml_doc_to_map(Value) ->
    % Handle scalar values
    ensure_binary(Value).

is_yaml_proplist([]) -> true;
is_yaml_proplist([{_, _} | Rest]) -> is_yaml_proplist(Rest);
is_yaml_proplist(_) -> false.

ensure_binary(X) when is_binary(X) -> X;
ensure_binary(X) when is_list(X) -> 
    try list_to_binary(X) 
    catch _:_ -> list_to_binary(io_lib:format("~p", [X]))
    end;
ensure_binary(X) when is_atom(X) -> atom_to_binary(X, utf8);
ensure_binary(X) when is_integer(X) -> integer_to_binary(X);
ensure_binary(X) when is_float(X) -> float_to_binary(X);
ensure_binary(X) -> list_to_binary(io_lib:format("~p", [X])).