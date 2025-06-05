%%%-------------------------------------------------------------------
%%% @doc OpenAPI Specification Parser
%%% Parses OpenAPI 3.0+ specifications from YAML or JSON
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_parser).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

-export([start_link/0, parse_file/1, parse_content/2, get_spec/1, get_all_specs/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    specs = #{} :: #{binary() => map()},
    file_monitors = #{} :: #{reference() => binary()}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec parse_file(file:filename()) -> {ok, map()} | {error, term()}.
parse_file(FilePath) ->
    gen_server:call(?MODULE, {parse_file, FilePath}, 30000).

-spec parse_content(binary(), yaml | json) -> {ok, map()} | {error, term()}.
parse_content(Content, Format) ->
    gen_server:call(?MODULE, {parse_content, Content, Format}, 30000).

-spec get_spec(binary()) -> {ok, map()} | {error, not_found}.
get_spec(SpecId) ->
    gen_server:call(?MODULE, {get_spec, SpecId}).

-spec get_all_specs() -> {ok, map()} | {error, term()}.
get_all_specs() ->
    gen_server:call(?MODULE, get_all_specs).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call({parse_file, FilePath}, _From, State) ->
    case do_parse_file(FilePath) of
        {ok, Spec} = Result ->
            SpecId = filename:rootname(filename:basename(FilePath)),
            NewState = store_spec(SpecId, Spec, FilePath, State),
            {reply, Result, NewState};
        Error ->
            {reply, Error, State}
    end;

handle_call({parse_content, Content, Format}, _From, State) ->
    Result = do_parse_content(Content, Format),
    {reply, Result, State};

handle_call({get_spec, SpecId}, _From, #state{specs = Specs} = State) ->
    case maps:find(SpecId, Specs) of
        {ok, Spec} ->
            {reply, {ok, Spec}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_all_specs, _From, #state{specs = Specs} = State) ->
    {reply, {ok, Specs}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({check_file, FilePath, SpecId, LastModified}, State) ->
    %% Check if file has been modified
    case file:read_file_info(FilePath) of
        {ok, FileInfo} ->
            case FileInfo#file_info.mtime of
                LastModified ->
                    %% File hasn't changed, schedule next check
                    erlang:send_after(5000, self(), {check_file, FilePath, SpecId, LastModified}),
                    {noreply, State};
                NewModified ->
                    %% File changed, reload it
                    error_logger:info_msg("OpenAPI spec file changed: ~s~n", [FilePath]),
                    case do_parse_file(FilePath) of
                        {ok, Spec} ->
                            NewState = store_spec(SpecId, Spec, FilePath, State),
                            %% Notify router of spec update
                            openapi_router:reload_spec(SpecId, Spec),
                            {noreply, NewState};
                        {error, Reason} ->
                            error_logger:error_msg("Failed to reload spec ~s: ~p~n", [FilePath, Reason]),
                            %% Continue monitoring even if reload failed
                            erlang:send_after(5000, self(), {check_file, FilePath, SpecId, NewModified}),
                            {noreply, State}
                    end
            end;
        {error, _} ->
            %% File no longer exists, stop monitoring
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{file_monitors = Monitors}) ->
    %% Cancel all timers
    maps:foreach(fun(Ref, _) ->
        erlang:cancel_timer(Ref)
    end, Monitors),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

do_parse_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            Format = case filename:extension(FilePath) of
                Ext when Ext =:= ".yaml"; Ext =:= ".yml" -> yaml;
                ".json" -> json;
                _ -> auto
            end,
            do_parse_content(Content, Format);
        {error, _} = Error ->
            Error
    end.

do_parse_content(Content, yaml) ->
    try
        case yamerl_constr:string(binary_to_list(Content)) of
            [Doc] ->
                Spec = yaml_to_map(Doc),
                validate_spec(Spec);
            _ ->
                {error, multiple_documents}
        end
    catch
        error:Reason ->
            {error, {yaml_parse_error, Reason}}
    end;

do_parse_content(Content, json) ->
    try
        Spec = jsx:decode(Content, [return_maps, {labels, atom}]),
        validate_spec(Spec)
    catch
        error:Reason ->
            {error, {json_parse_error, Reason}}
    end;

do_parse_content(Content, auto) ->
    %% Try JSON first, then YAML
    case do_parse_content(Content, json) of
        {ok, _} = Result ->
            Result;
        {error, _} ->
            do_parse_content(Content, yaml)
    end.

yaml_to_map(YamlDoc) when is_list(YamlDoc) ->
    case lists:all(fun({_, _}) -> true; (_) -> false end, YamlDoc) of
        true ->
            maps:from_list([{list_to_binary(K), yaml_to_map(V)} || {K, V} <- YamlDoc]);
        false ->
            [yaml_to_map(Item) || Item <- YamlDoc]
    end;
yaml_to_map(Value) when is_list(Value) ->
    try list_to_binary(Value)
    catch _:_ -> Value
    end;
yaml_to_map(Value) ->
    Value.

validate_spec(Spec) when is_map(Spec) ->
    %% Basic OpenAPI 3.0 validation
    case maps:get(<<"openapi">>, Spec, undefined) of
        Version when is_binary(Version), 
                     byte_size(Version) >= 3,
                     binary_part(Version, 0, 2) =:= <<"3.">> ->
            %% Check required fields
            Required = [<<"info">>, <<"paths">>],
            case lists:all(fun(Field) -> maps:is_key(Field, Spec) end, Required) of
                true ->
                    {ok, normalize_spec(Spec)};
                false ->
                    {error, missing_required_fields}
            end;
        _ ->
            {error, unsupported_openapi_version}
    end;
validate_spec(_) ->
    {error, invalid_spec_format}.

normalize_spec(Spec) ->
    %% Normalize the spec structure for easier processing
    Spec1 = ensure_defaults(Spec),
    Spec2 = resolve_references(Spec1),
    Spec2.

ensure_defaults(Spec) ->
    Defaults = #{
        <<"servers">> => [#{<<"url">> => <<"/">>}],
        <<"components">> => #{},
        <<"security">> => [],
        <<"tags">> => []
    },
    maps:merge(Defaults, Spec).

resolve_references(Spec) ->
    %% TODO: Implement $ref resolution
    %% For now, return spec as-is
    Spec.

store_spec(SpecId, Spec, FilePath, #state{specs = Specs, file_monitors = Monitors} = State) ->
    SpecIdBin = iolist_to_binary(SpecId),
    
    %% Set up file monitoring if hot reload is enabled
    NewMonitors = case application:get_env(openapi_scaffold, hot_reload_enabled, true) of
        true ->
            %% Remove old monitor if exists
            OldMonitors = case maps:to_list(Monitors) of
                [] -> Monitors;
                MonitorList ->
                    lists:foldl(fun({Ref, Id}, AccMonitors) ->
                        case Id =:= SpecIdBin of
                            true ->
                                erlang:cancel_timer(Ref),
                                maps:remove(Ref, AccMonitors);
                            false ->
                                AccMonitors
                        end
                    end, Monitors, MonitorList)
            end,
            %% Add new monitor using a timer
            Ref = start_file_monitor(FilePath, SpecIdBin),
            maps:put(Ref, SpecIdBin, OldMonitors);
        false ->
            Monitors
    end,
    
    State#state{
        specs = maps:put(SpecIdBin, Spec, Specs),
        file_monitors = NewMonitors
    }.

start_file_monitor(FilePath, SpecId) ->
    %% Simple file monitoring using timer
    %% Check file modification time every 5 seconds
    {ok, FileInfo} = file:read_file_info(FilePath),
    LastModified = FileInfo#file_info.mtime,
    erlang:send_after(5000, self(), {check_file, FilePath, SpecId, LastModified}).