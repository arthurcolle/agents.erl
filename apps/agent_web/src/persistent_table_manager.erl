%%%-------------------------------------------------------------------
%%% @doc
%%% Persistent Table Manager
%%% Manages ETS/DETS tables that persist across application restarts
%%% Ensures tables are created only once and data is preserved
%%% @end
%%%-------------------------------------------------------------------
-module(persistent_table_manager).
-behaviour(gen_server).

%% API
-export([start_link/0,
         ensure_table/2,
         ensure_table/3,
         ensure_ets_table/3,
         ensure_dets_table/3,
         ensure_mnesia_table/3,
         get_data_dir/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_DATA_DIR, "data/tables").

-record(state, {
    data_dir :: string(),
    managed_tables :: #{atom() => table_info()}
}).

-type table_info() :: #{
    type := ets | dets | mnesia,
    options := list(),
    persistence := boolean(),
    file_path => string()
}.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Ensure a table exists with default options
ensure_table(Name, Type) ->
    ensure_table(Name, Type, []).

%% @doc Ensure a table exists with specified options
ensure_table(Name, Type, Options) when Type =:= ets ->
    ensure_ets_table(Name, Options, true);
ensure_table(Name, Type, Options) when Type =:= dets ->
    ensure_dets_table(Name, Options, true);
ensure_table(Name, Type, Options) when Type =:= mnesia ->
    ensure_mnesia_table(Name, Options, true).

%% @doc Ensure an ETS table exists (with optional DETS backing)
ensure_ets_table(Name, Options, Persistent) ->
    gen_server:call(?SERVER, {ensure_ets_table, Name, Options, Persistent}).

%% @doc Ensure a DETS table exists
ensure_dets_table(Name, Options, _Persistent) ->
    gen_server:call(?SERVER, {ensure_dets_table, Name, Options}).

%% @doc Ensure a Mnesia table exists
ensure_mnesia_table(Name, Options, _Persistent) ->
    gen_server:call(?SERVER, {ensure_mnesia_table, Name, Options}).

%% @doc Get the data directory for persistent storage
get_data_dir() ->
    gen_server:call(?SERVER, get_data_dir).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    
    % Determine data directory
    DataDir = case application:get_env(agent_web, table_data_dir) of
        {ok, Dir} -> Dir;
        undefined -> 
            PrivDir = case code:priv_dir(agent_web) of
                {error, _} -> "priv";
                Dir -> Dir
            end,
            filename:join(PrivDir, ?DEFAULT_DATA_DIR)
    end,
    
    % Ensure data directory exists
    ok = filelib:ensure_dir(filename:join(DataDir, "dummy")),
    
    State = #state{
        data_dir = DataDir,
        managed_tables = #{}
    },
    
    {ok, State}.

handle_call({ensure_ets_table, Name, Options, Persistent}, _From, State) ->
    case ets:info(Name) of
        undefined ->
            % Table doesn't exist, create it
            FinalOptions = ensure_named_table(Name, Options),
            ets:new(Name, FinalOptions),
            
            % If persistent, load from DETS
            NewState = if
                Persistent ->
                    DetsFile = get_dets_file(State#state.data_dir, Name),
                    load_ets_from_dets(Name, DetsFile),
                    
                    % Schedule periodic sync
                    schedule_sync(Name, 30000), % Sync every 30 seconds
                    
                    % Update managed tables
                    TableInfo = #{
                        type => ets,
                        options => FinalOptions,
                        persistence => true,
                        file_path => DetsFile
                    },
                    State#state{
                        managed_tables = maps:put(Name, TableInfo, State#state.managed_tables)
                    };
                true ->
                    State
            end,
            
            {reply, {ok, created}, NewState};
        _ ->
            % Table already exists
            {reply, {ok, exists}, State}
    end;

handle_call({ensure_dets_table, Name, Options}, _From, State) ->
    DetsFile = get_dets_file(State#state.data_dir, Name),
    
    case dets:info(Name) of
        undefined ->
            % Open DETS table
            FinalOptions = [{file, DetsFile} | Options],
            case dets:open_file(Name, FinalOptions) of
                {ok, Name} ->
                    TableInfo = #{
                        type => dets,
                        options => FinalOptions,
                        persistence => true,
                        file_path => DetsFile
                    },
                    NewState = State#state{
                        managed_tables = maps:put(Name, TableInfo, State#state.managed_tables)
                    },
                    {reply, {ok, created}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        _ ->
            % Table already exists
            {reply, {ok, exists}, State}
    end;

handle_call({ensure_mnesia_table, Name, Options}, _From, State) ->
    % Ensure Mnesia is started
    case mnesia:system_info(is_running) of
        no ->
            mnesia:start();
        _ ->
            ok
    end,
    
    % Check if table exists
    case lists:member(Name, mnesia:system_info(tables)) of
        false ->
            % Create table
            TableType = proplists:get_value(type, Options, disc_copies),
            Attributes = proplists:get_value(attributes, Options, [key, value]),
            
            case mnesia:create_table(Name, [
                {TableType, [node()]},
                {attributes, Attributes}
                | proplists:delete(type, proplists:delete(attributes, Options))
            ]) of
                {atomic, ok} ->
                    TableInfo = #{
                        type => mnesia,
                        options => Options,
                        persistence => true
                    },
                    NewState = State#state{
                        managed_tables = maps:put(Name, TableInfo, State#state.managed_tables)
                    },
                    {reply, {ok, created}, NewState};
                {aborted, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        true ->
            % Table already exists
            {reply, {ok, exists}, State}
    end;

handle_call(get_data_dir, _From, State) ->
    {reply, State#state.data_dir, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({sync_ets_to_dets, TableName}, State) ->
    case maps:find(TableName, State#state.managed_tables) of
        {ok, #{type := ets, file_path := DetsFile}} ->
            sync_ets_to_dets(TableName, DetsFile),
            % Schedule next sync
            schedule_sync(TableName, 30000);
        _ ->
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Sync all persistent ETS tables before shutdown
    maps:foreach(fun(TableName, TableInfo) ->
        case TableInfo of
            #{type := ets, persistence := true, file_path := DetsFile} ->
                sync_ets_to_dets(TableName, DetsFile);
            _ ->
                ok
        end
    end, State#state.managed_tables),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ensure_named_table(Name, Options) ->
    case lists:member(named_table, Options) of
        true -> Options;
        false -> [named_table | Options]
    end.

get_dets_file(DataDir, TableName) ->
    filename:join(DataDir, atom_to_list(TableName) ++ ".dets").

load_ets_from_dets(EtsTable, DetsFile) ->
    case dets:open_file(DetsFile, [{type, set}]) of
        {ok, DetsTable} ->
            % Copy all data from DETS to ETS
            dets:traverse(DetsTable, fun(Object) ->
                ets:insert(EtsTable, Object),
                continue
            end),
            dets:close(DetsTable),
            ok;
        {error, {file_error, _, enoent}} ->
            % File doesn't exist yet, that's ok
            ok;
        {error, Reason} ->
            error_logger:warning_msg("Failed to load DETS file ~p: ~p~n", [DetsFile, Reason]),
            ok
    end.

sync_ets_to_dets(EtsTable, DetsFile) ->
    case dets:open_file(DetsFile, [{type, set}]) of
        {ok, DetsTable} ->
            % Clear DETS and copy all ETS data
            dets:delete_all_objects(DetsTable),
            ets:foldl(fun(Object, Acc) ->
                dets:insert(DetsTable, Object),
                Acc
            end, ok, EtsTable),
            dets:close(DetsTable),
            ok;
        {error, Reason} ->
            error_logger:error_msg("Failed to sync to DETS file ~p: ~p~n", [DetsFile, Reason]),
            error
    end.

schedule_sync(TableName, Interval) ->
    erlang:send_after(Interval, self(), {sync_ets_to_dets, TableName}).