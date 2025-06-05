%%%-------------------------------------------------------------------
%%% @doc OpenAPI Introspection Service
%%% Provides API exploration and documentation endpoints
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_introspection).
-behaviour(gen_server).

-include("../include/openapi_scaffold.hrl").

-export([start_link/0, get_loaded_specs/0, get_spec_info/1, get_routes/1, get_schemas/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    introspection_enabled = true :: boolean()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_loaded_specs() -> {ok, [map()]} | {error, term()}.
get_loaded_specs() ->
    gen_server:call(?MODULE, get_loaded_specs).

-spec get_spec_info(binary()) -> {ok, map()} | {error, not_found}.
get_spec_info(SpecId) ->
    gen_server:call(?MODULE, {get_spec_info, SpecId}).

-spec get_routes(binary()) -> {ok, [map()]} | {error, not_found}.
get_routes(SpecId) ->
    gen_server:call(?MODULE, {get_routes, SpecId}).

-spec get_schemas(binary()) -> {ok, map()} | {error, not_found}.
get_schemas(SpecId) ->
    gen_server:call(?MODULE, {get_schemas, SpecId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Enabled = application:get_env(openapi_scaffold, introspection_enabled, true),
    {ok, #state{introspection_enabled = Enabled}}.

handle_call(get_loaded_specs, _From, #state{introspection_enabled = true} = State) ->
    %% Get all loaded specs from the parser
    case gen_server:call(openapi_parser, get_all_specs) of
        {ok, Specs} ->
            SpecList = maps:fold(fun(SpecId, Spec, Acc) ->
                Info = extract_spec_summary(SpecId, Spec),
                [Info | Acc]
            end, [], Specs),
            {reply, {ok, SpecList}, State};
        _ ->
            %% Fallback: list files in specs directory
            case application:get_env(openapi_scaffold, specs_dir) of
                {ok, SpecsDir} ->
                    case file:list_dir(SpecsDir) of
                        {ok, Files} ->
                            Specs = lists:filtermap(fun(File) ->
                                case filename:extension(File) of
                                    Ext when Ext =:= ".yaml"; Ext =:= ".yml"; Ext =:= ".json" ->
                                        SpecId = filename:rootname(File),
                                        {true, #{
                                            <<"id">> => list_to_binary(SpecId),
                                            <<"filename">> => list_to_binary(File),
                                            <<"status">> => <<"available">>
                                        }};
                                    _ ->
                                        false
                                end
                            end, Files),
                            {reply, {ok, Specs}, State};
                        {error, _} ->
                            {reply, {ok, []}, State}
                    end;
                _ ->
                    {reply, {ok, []}, State}
            end
    end;

handle_call({get_spec_info, SpecId}, _From, #state{introspection_enabled = true} = State) ->
    case openapi_parser:get_spec(SpecId) of
        {ok, Spec} ->
            Info = build_spec_info(SpecId, Spec),
            {reply, {ok, Info}, State};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({get_routes, SpecId}, _From, #state{introspection_enabled = true} = State) ->
    case openapi_router:get_routes(SpecId) of
        {ok, Routes} ->
            RouteInfo = [route_to_map(R) || R <- Routes],
            {reply, {ok, RouteInfo}, State};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({get_schemas, SpecId}, _From, #state{introspection_enabled = true} = State) ->
    case openapi_parser:get_spec(SpecId) of
        {ok, Spec} ->
            Components = maps:get(<<"components">>, Spec, #{}),
            Schemas = maps:get(<<"schemas">>, Components, #{}),
            {reply, {ok, Schemas}, State};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call(_Request, _From, #state{introspection_enabled = false} = State) ->
    {reply, {error, introspection_disabled}, State};

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

extract_spec_summary(SpecId, Spec) ->
    Info = maps:get(<<"info">>, Spec, #{}),
    Paths = maps:get(<<"paths">>, Spec, #{}),
    
    #{
        <<"id">> => SpecId,
        <<"title">> => maps:get(<<"title">>, Info, <<"Untitled">>),
        <<"version">> => maps:get(<<"version">>, Info, <<"1.0.0">>),
        <<"description">> => maps:get(<<"description">>, Info, <<"">>),
        <<"path_count">> => maps:size(Paths),
        <<"openapi_version">> => maps:get(<<"openapi">>, Spec, <<"3.0.0">>)
    }.

build_spec_info(SpecId, Spec) ->
    Info = maps:get(<<"info">>, Spec, #{}),
    Paths = maps:get(<<"paths">>, Spec, #{}),
    Components = maps:get(<<"components">>, Spec, #{}),
    Servers = maps:get(<<"servers">>, Spec, []),
    
    %% Count operations
    OperationCount = maps:fold(fun(_Path, PathItem, Acc) ->
        Methods = [<<"get">>, <<"post">>, <<"put">>, <<"delete">>, 
                   <<"patch">>, <<"head">>, <<"options">>],
        MethodCount = length([M || M <- Methods, maps:is_key(M, PathItem)]),
        Acc + MethodCount
    end, 0, Paths),
    
    %% Get schema count
    Schemas = maps:get(<<"schemas">>, Components, #{}),
    SchemaCount = maps:size(Schemas),
    
    %% Get generated modules
    {ok, Modules} = openapi_codegen:get_generated_modules(SpecId),
    
    #{
        <<"id">> => SpecId,
        <<"info">> => Info,
        <<"servers">> => Servers,
        <<"statistics">> => #{
            <<"paths">> => maps:size(Paths),
            <<"operations">> => OperationCount,
            <<"schemas">> => SchemaCount,
            <<"generated_modules">> => length(Modules)
        },
        <<"features">> => #{
            <<"validation_enabled">> => application:get_env(openapi_scaffold, validation_enabled, true),
            <<"proxy_enabled">> => application:get_env(openapi_scaffold, proxy_enabled, true),
            <<"hot_reload_enabled">> => application:get_env(openapi_scaffold, hot_reload_enabled, true)
        },
        <<"generated_modules">> => [atom_to_binary(M, utf8) || M <- Modules]
    }.

route_to_map(#route{path = Path, path_pattern = Pattern, methods = Methods, spec_id = SpecId}) ->
    #{
        <<"path">> => Path,
        <<"pattern">> => pattern_to_string(Pattern),
        <<"methods">> => maps:keys(Methods),
        <<"spec_id">> => SpecId,
        <<"parameters">> => extract_path_params(Pattern)
    }.

pattern_to_string(Pattern) ->
    Parts = lists:map(fun
        ({param, Name}) -> <<"{", Name/binary, "}">>;
        (Part) -> Part
    end, Pattern),
    iolist_to_binary([<<"/">>, lists:join(<<"/">>, Parts)]).

extract_path_params(Pattern) ->
    lists:filtermap(fun
        ({param, Name}) -> {true, Name};
        (_) -> false
    end, Pattern).