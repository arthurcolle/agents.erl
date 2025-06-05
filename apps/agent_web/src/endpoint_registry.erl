%%%-------------------------------------------------------------------
%%% @doc Endpoint Registry V2 - Fixed version
%%% Stores and manages discovered API endpoints with their metadata
%%% @end
%%%-------------------------------------------------------------------
-module(endpoint_registry).
-behaviour(gen_server).

%% SQLite3 dependency removed - using ETS instead
-export([start_link/0]).
-export([
    register_endpoint/1,
    get_endpoint/2,
    get_all_endpoints/0,
    update_endpoint/1,
    remove_endpoint/2,
    get_endpoints_by_tag/1,
    log_request/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    endpoints = #{} :: map(),
    logs = [] :: list(),
    db :: pid() | undefined,
    db_path :: string()
}).

-record(endpoint, {
    method :: binary(),
    path :: binary(),
    operation_id :: binary(),
    summary :: binary(),
    tag :: binary(),
    parameters :: list(),
    request_schema :: map(),
    response_schema :: map(),
    last_updated :: erlang:timestamp()
}).

-record(request_log, {
    timestamp :: erlang:timestamp(),
    endpoint :: binary(),
    method :: binary(),
    request_payload :: map(),
    response_payload :: map(),
    cost_estimate :: float(),
    prompt_tokens :: integer(),
    completion_tokens :: integer()
}).

%% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_endpoint(Endpoint) ->
    gen_server:call(?MODULE, {register_endpoint, Endpoint}).

get_endpoint(Method, Path) ->
    gen_server:call(?MODULE, {get_endpoint, Method, Path}).

get_all_endpoints() ->
    gen_server:call(?MODULE, get_all_endpoints).

update_endpoint(Endpoint) ->
    gen_server:call(?MODULE, {update_endpoint, Endpoint}).

remove_endpoint(Method, Path) ->
    gen_server:call(?MODULE, {remove_endpoint, Method, Path}).

get_endpoints_by_tag(Tag) ->
    gen_server:call(?MODULE, {get_endpoints_by_tag, Tag}).

log_request(LogEntry) ->
    gen_server:cast(?MODULE, {log_request, LogEntry}).

%% gen_server callbacks

init([]) ->
    % Initialize ETS tables for fast lookups with error handling
    EndpointsTable = case ets:info(endpoints_table) of
        undefined -> ets:new(endpoints_table, [named_table, set, public]);
        _ -> endpoints_table
    end,
    
    LogsTable = case ets:info(request_logs) of
        undefined -> ets:new(request_logs, [named_table, ordered_set, public]);
        _ -> request_logs
    end,
    
    % Initialize SQLite database
    DbPath = filename:join([code:priv_dir(agent_web), "data", "endpoint_registry.db"]),
    filelib:ensure_dir(DbPath),
    
    State = case init_database(DbPath) of
        {ok, Db} ->
            % Load existing endpoints from database
            Endpoints = load_endpoints_from_db(Db),
            #state{
                endpoints = Endpoints,
                db = Db,
                db_path = DbPath
            };
        {error, Reason} ->
            error_logger:error_msg("Failed to initialize database: ~p~n", [Reason]),
            #state{
                endpoints = #{},
                db = undefined,
                db_path = DbPath
            }
    end,
    
    {ok, State}.

handle_call({register_endpoint, EndpointData}, _From, State) ->
    Endpoint = create_endpoint_record(EndpointData),
    Key = {Endpoint#endpoint.method, Endpoint#endpoint.path},
    
    % Store in ETS
    ets:insert(endpoints_table, {Key, Endpoint}),
    
    % Store in state
    NewEndpoints = maps:put(Key, Endpoint, State#state.endpoints),
    
    % Persist to database if available
    case State#state.db of
        undefined -> ok;
        Db -> persist_endpoint(Db, Endpoint)
    end,
    
    {reply, ok, State#state{endpoints = NewEndpoints}};

handle_call({get_endpoint, Method, Path}, _From, State) ->
    Key = {Method, Path},
    Reply = case maps:get(Key, State#state.endpoints, undefined) of
        undefined -> {error, not_found};
        Endpoint -> {ok, Endpoint}
    end,
    {reply, Reply, State};

handle_call(get_all_endpoints, _From, State) ->
    Endpoints = maps:values(State#state.endpoints),
    {reply, Endpoints, State};

handle_call({update_endpoint, EndpointData}, _From, State) ->
    Endpoint = create_endpoint_record(EndpointData),
    Key = {Endpoint#endpoint.method, Endpoint#endpoint.path},
    
    % Update in ETS
    ets:insert(endpoints_table, {Key, Endpoint}),
    
    % Update in state
    NewEndpoints = maps:put(Key, Endpoint, State#state.endpoints),
    
    % Update in database if available
    case State#state.db of
        undefined -> ok;
        Db -> update_endpoint_in_db(Db, Endpoint)
    end,
    
    {reply, ok, State#state{endpoints = NewEndpoints}};

handle_call({remove_endpoint, Method, Path}, _From, State) ->
    Key = {Method, Path},
    
    % Remove from ETS
    ets:delete(endpoints_table, Key),
    
    % Remove from state
    NewEndpoints = maps:remove(Key, State#state.endpoints),
    
    % Remove from database if available
    case State#state.db of
        undefined -> ok;
        Db -> remove_endpoint_from_db(Db, Method, Path)
    end,
    
    {reply, ok, State#state{endpoints = NewEndpoints}};

handle_call({get_endpoints_by_tag, Tag}, _From, State) ->
    Endpoints = [E || E <- maps:values(State#state.endpoints),
                      E#endpoint.tag =:= Tag],
    {reply, Endpoints, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({log_request, LogEntry}, State) ->
    % Store in ETS with timestamp as key for ordering
    ets:insert(request_logs, {LogEntry#request_log.timestamp, LogEntry}),
    
    % Persist to database if available
    case State#state.db of
        undefined -> ok;
        Db -> persist_request_log(Db, LogEntry)
    end,
    
    % Keep only last 1000 logs in memory
    case ets:info(request_logs, size) > 1000 of
        true ->
            FirstKey = ets:first(request_logs),
            ets:delete(request_logs, FirstKey);
        false ->
            ok
    end,
    
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Close database connection if open
    case State#state.db of
        undefined -> ok;
        Db -> sqlite3:close(Db)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

create_endpoint_record(Data) ->
    #endpoint{
        method = maps:get(method, Data),
        path = maps:get(path, Data),
        operation_id = maps:get(operation_id, Data, <<>>),
        summary = maps:get(summary, Data, <<>>),
        tag = maps:get(tag, Data, <<>>),
        parameters = maps:get(parameters, Data, []),
        request_schema = maps:get(request_schema, Data, #{}),
        response_schema = maps:get(response_schema, Data, #{}),
        last_updated = erlang:timestamp()
    }.

init_database(DbPath) ->
    case sqlite3:open(scaffold_db, [{file, DbPath}]) of
        {ok, Db} ->
            % Create endpoints table
            CreateEndpointsSQL = "CREATE TABLE IF NOT EXISTS endpoints (
                method TEXT,
                path TEXT,
                operation_id TEXT,
                summary TEXT,
                tag TEXT,
                parameters TEXT,
                request_schema TEXT,
                response_schema TEXT,
                last_updated INTEGER,
                PRIMARY KEY (method, path)
            )",
            
            case sqlite3:sql_exec(Db, CreateEndpointsSQL) of
                [{sql_error, _}] -> 
                    error_logger:error_msg("Failed to create endpoints table~n"),
                    sqlite3:close(Db),
                    {error, table_creation_failed};
                _ ->
                    % Create request logs table
                    CreateLogsSQL = "CREATE TABLE IF NOT EXISTS request_logs (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        timestamp INTEGER,
                        endpoint TEXT,
                        method TEXT,
                        request_payload TEXT,
                        response_payload TEXT,
                        cost_estimate REAL,
                        prompt_tokens INTEGER,
                        completion_tokens INTEGER
                    )",
                    
                    case sqlite3:sql_exec(Db, CreateLogsSQL) of
                        [{sql_error, _}] ->
                            error_logger:error_msg("Failed to create request_logs table~n"),
                            sqlite3:close(Db),
                            {error, table_creation_failed};
                        _ ->
                            {ok, Db}
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end.

load_endpoints_from_db(Db) ->
    case sqlite3:sql_exec(Db, "SELECT * FROM endpoints") of
        {error, _} -> #{};
        [{columns, _Columns}, {rows, Rows}] ->
            lists:foldl(fun(Row, Acc) ->
                case row_to_endpoint(Row) of
                    {ok, Endpoint} ->
                        Key = {Endpoint#endpoint.method, Endpoint#endpoint.path},
                        maps:put(Key, Endpoint, Acc);
                    {error, _} ->
                        Acc
                end
            end, #{}, Rows);
        Rows when is_list(Rows) ->
            % Filter out non-data rows
            DataRows = [R || R <- Rows, is_tuple(R), tuple_size(R) >= 9],
            lists:foldl(fun(Row, Acc) ->
                case row_to_endpoint(Row) of
                    {ok, Endpoint} ->
                        Key = {Endpoint#endpoint.method, Endpoint#endpoint.path},
                        maps:put(Key, Endpoint, Acc);
                    {error, _} ->
                        Acc
                end
            end, #{}, DataRows);
        _ -> #{}
    end.

row_to_endpoint({columns, _Columns}) -> {error, columns_row};
row_to_endpoint({Method, Path, OpId, Summary, Tag, ParamsJson, ReqSchemaJson, RespSchemaJson, LastUpdated}) ->
    try
        Endpoint = #endpoint{
            method = ensure_binary(Method),
            path = ensure_binary(Path),
            operation_id = ensure_binary(OpId),
            summary = ensure_binary(Summary),
            tag = ensure_binary(Tag),
            parameters = decode_json(ParamsJson),
            request_schema = decode_json(ReqSchemaJson),
            response_schema = decode_json(RespSchemaJson),
            last_updated = int_to_timestamp(LastUpdated)
        },
        {ok, Endpoint}
    catch
        _:_ -> {error, invalid_row}
    end.

ensure_binary(X) when is_binary(X) -> X;
ensure_binary(X) when is_list(X) -> list_to_binary(X);
ensure_binary(X) when is_atom(X) -> atom_to_binary(X, utf8);
ensure_binary(_) -> <<>>.

decode_json(JsonStr) when is_binary(JsonStr) ->
    try
        jsx:decode(JsonStr, [return_maps])
    catch
        _:_ -> #{}
    end;
decode_json(JsonStr) when is_list(JsonStr) ->
    decode_json(list_to_binary(JsonStr));
decode_json(_) -> #{}.

persist_endpoint(Db, Endpoint) ->
    SQL = "INSERT OR REPLACE INTO endpoints VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
    Params = [
        Endpoint#endpoint.method,
        Endpoint#endpoint.path,
        Endpoint#endpoint.operation_id,
        Endpoint#endpoint.summary,
        Endpoint#endpoint.tag,
        jsx:encode(Endpoint#endpoint.parameters),
        jsx:encode(Endpoint#endpoint.request_schema),
        jsx:encode(Endpoint#endpoint.response_schema),
        timestamp_to_int(Endpoint#endpoint.last_updated)
    ],
    
    case sqlite3:sql_exec(Db, SQL, Params) of
        {error, Reason} ->
            error_logger:error_msg("Failed to persist endpoint: ~p~n", [Reason]);
        _ ->
            ok
    end.

update_endpoint_in_db(Db, Endpoint) ->
    persist_endpoint(Db, Endpoint).

remove_endpoint_from_db(Db, Method, Path) ->
    SQL = "DELETE FROM endpoints WHERE method = ? AND path = ?",
    case sqlite3:sql_exec(Db, SQL, [Method, Path]) of
        {error, Reason} ->
            error_logger:error_msg("Failed to remove endpoint: ~p~n", [Reason]);
        _ ->
            ok
    end.

persist_request_log(Db, LogEntry) ->
    SQL = "INSERT INTO request_logs VALUES (NULL, ?, ?, ?, ?, ?, ?, ?, ?)",
    Params = [
        timestamp_to_int(LogEntry#request_log.timestamp),
        LogEntry#request_log.endpoint,
        LogEntry#request_log.method,
        jsx:encode(LogEntry#request_log.request_payload),
        jsx:encode(LogEntry#request_log.response_payload),
        LogEntry#request_log.cost_estimate,
        LogEntry#request_log.prompt_tokens,
        LogEntry#request_log.completion_tokens
    ],
    
    case sqlite3:sql_exec(Db, SQL, Params) of
        {error, Reason} ->
            error_logger:error_msg("Failed to persist request log: ~p~n", [Reason]);
        _ ->
            ok
    end.

timestamp_to_int({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

int_to_timestamp(Int) when is_integer(Int) ->
    MicroSecs = Int rem 1000000,
    TotalSecs = Int div 1000000,
    Secs = TotalSecs rem 1000000,
    MegaSecs = TotalSecs div 1000000,
    {MegaSecs, Secs, MicroSecs};
int_to_timestamp(_) ->
    erlang:timestamp().