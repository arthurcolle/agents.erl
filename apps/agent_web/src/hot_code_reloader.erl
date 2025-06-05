%% hot_code_reloader.erl
%% Hot code reloading system for agents.erl
-module(hot_code_reloader).
-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

%% API exports
-export([
    start_link/0,
    reload_module/1,
    reload_all_modules/0,
    reload_application/1,
    compile_and_reload/1,
    get_module_info/1,
    get_loaded_modules/0,
    purge_old_code/1,
    watch_files/1,
    stop_watch/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    watched_files = [] :: [string()],
    file_timestamps = #{} :: map(),
    watch_timer = undefined :: timer:tref() | undefined
}).

%% API Functions

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Reload a specific module
reload_module(Module) when is_atom(Module) ->
    gen_server:call(?SERVER, {reload_module, Module}).

%% Reload all changed modules in the system
reload_all_modules() ->
    gen_server:call(?SERVER, reload_all_modules).

%% Reload an entire application
reload_application(App) when is_atom(App) ->
    gen_server:call(?SERVER, {reload_application, App}).

%% Compile and reload a module from source
compile_and_reload(SourceFile) when is_list(SourceFile) ->
    gen_server:call(?SERVER, {compile_and_reload, SourceFile}).

%% Get information about a loaded module
get_module_info(Module) when is_atom(Module) ->
    gen_server:call(?SERVER, {get_module_info, Module}).

%% Get all loaded modules
get_loaded_modules() ->
    gen_server:call(?SERVER, get_loaded_modules).

%% Purge old code for a module
purge_old_code(Module) when is_atom(Module) ->
    gen_server:call(?SERVER, {purge_old_code, Module}).

%% Start watching files for changes
watch_files(Files) when is_list(Files) ->
    gen_server:call(?SERVER, {watch_files, Files}).

%% Stop file watching
stop_watch() ->
    gen_server:call(?SERVER, stop_watch).

%% gen_server callbacks

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({reload_module, Module}, _From, State) ->
    Result = do_reload_module(Module),
    {reply, Result, State};

handle_call(reload_all_modules, _From, State) ->
    Result = do_reload_all_modules(),
    {reply, Result, State};

handle_call({reload_application, App}, _From, State) ->
    Result = do_reload_application(App),
    {reply, Result, State};

handle_call({compile_and_reload, SourceFile}, _From, State) ->
    Result = do_compile_and_reload(SourceFile),
    {reply, Result, State};

handle_call({get_module_info, Module}, _From, State) ->
    Result = do_get_module_info(Module),
    {reply, Result, State};

handle_call(get_loaded_modules, _From, State) ->
    Result = do_get_loaded_modules(),
    {reply, Result, State};

handle_call({purge_old_code, Module}, _From, State) ->
    Result = do_purge_old_code(Module),
    {reply, Result, State};

handle_call({watch_files, Files}, _From, State) ->
    NewState = start_file_watching(Files, State),
    {reply, ok, NewState};

handle_call(stop_watch, _From, State) ->
    NewState = stop_file_watching(State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _, check_files}, State) ->
    NewState = check_file_changes(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    stop_file_watching(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

do_reload_module(Module) ->
    try
        case code:soft_purge(Module) of
            true ->
                case code:load_file(Module) of
                    {module, Module} ->
                        case code:soft_purge(Module) of
                            true -> 
                                {ok, #{
                                    module => Module,
                                    status => reloaded,
                                    timestamp => erlang:timestamp()
                                }};
                            false -> 
                                {error, {purge_failed, Module}}
                        end;
                    {error, Reason} ->
                        {error, {load_failed, Module, Reason}}
                end;
            false ->
                {error, {purge_failed, Module}}
        end
    catch
        _Error:_Reason:_Stacktrace ->
            {error, {reload_failed, Module}}
    end.

do_reload_all_modules() ->
    LoadedModules = [M || {M, _} <- code:all_loaded()],
    AppModules = lists:filter(fun is_application_module/1, LoadedModules),
    
    Results = lists:map(fun(Module) ->
        {Module, do_reload_module(Module)}
    end, AppModules),
    
    Successful = [M || {M, {ok, _}} <- Results],
    Failed = [{M, R} || {M, {error, R}} <- Results],
    
    #{
        total => length(AppModules),
        successful => length(Successful),
        failed => length(Failed),
        successful_modules => Successful,
        failed_modules => Failed,
        timestamp => erlang:timestamp()
    }.

do_reload_application(App) ->
    case application:get_key(App, modules) of
        {ok, Modules} ->
            Results = lists:map(fun(Module) ->
                {Module, do_reload_module(Module)}
            end, Modules),
            
            Successful = [M || {M, {ok, _}} <- Results],
            Failed = [{M, R} || {M, {error, R}} <- Results],
            
            % Restart application supervisors if needed
            restart_application_supervisors(App),
            
            {ok, #{
                application => App,
                total => length(Modules),
                successful => length(Successful),
                failed => length(Failed),
                successful_modules => Successful,
                failed_modules => Failed,
                timestamp => erlang:timestamp()
            }};
        undefined ->
            {error, {application_not_found, App}}
    end.

do_compile_and_reload(SourceFile) ->
    try
        % Determine output directory
        OutDir = determine_output_dir(SourceFile),
        
        % Compile the file
        case compile:file(SourceFile, [
            {outdir, OutDir},
            debug_info,
            return_errors,
            return_warnings
        ]) of
            {ok, Module} ->
                % Reload the compiled module
                case do_reload_module(Module) of
                    {ok, Info} ->
                        {ok, Info#{
                            source_file => SourceFile,
                            output_dir => OutDir,
                            compiled => true
                        }};
                    Error ->
                        Error
                end;
            {ok, Module, Warnings} ->
                case do_reload_module(Module) of
                    {ok, Info} ->
                        {ok, Info#{
                            source_file => SourceFile,
                            output_dir => OutDir,
                            compiled => true,
                            warnings => Warnings
                        }};
                    Error ->
                        Error
                end;
            {error, Errors, Warnings} ->
                {error, {compile_failed, Errors, Warnings}};
            error ->
                {error, {compile_failed, SourceFile}}
        end
    catch
        _Error:_Reason:_Stacktrace ->
            {error, {compile_and_reload_failed, SourceFile}}
    end.

do_get_module_info(Module) ->
    try
        case code:is_loaded(Module) of
            {file, BeamFile} ->
                {ok, #{
                    module => Module,
                    beam_file => BeamFile,
                    attributes => Module:module_info(attributes),
                    exports => Module:module_info(exports),
                    functions => Module:module_info(functions),
                    md5 => Module:module_info(md5),
                    compile => Module:module_info(compile)
                }};
            false ->
                {error, {module_not_loaded, Module}}
        end
    catch
        Error:Reason ->
            {error, {module_info_failed, Module, Error, Reason}}
    end.

do_get_loaded_modules() ->
    AllLoaded = code:all_loaded(),
    AppModules = [{M, F} || {M, F} <- AllLoaded, is_application_module(M)],
    
    #{
        total_loaded => length(AllLoaded),
        application_modules => length(AppModules),
        modules => AppModules,
        timestamp => erlang:timestamp()
    }.

do_purge_old_code(Module) ->
    try
        case code:soft_purge(Module) of
            true ->
                {ok, #{module => Module, purged => true}};
            false ->
                % Force purge if soft purge failed
                case code:purge(Module) of
                    true ->
                        {ok, #{module => Module, purged => true, forced => true}};
                    false ->
                        {error, {purge_failed, Module}}
                end
        end
    catch
        Error:Reason ->
            {error, {purge_failed, Module, Error, Reason}}
    end.

%% File watching functions

start_file_watching(Files, State) ->
    % Stop existing timer
    NewState = stop_file_watching(State),
    
    % Initialize file timestamps
    Timestamps = maps:from_list([
        {File, get_file_timestamp(File)} || File <- Files
    ]),
    
    % Start new timer
    {ok, Timer} = timer:send_interval(1000, {timeout, undefined, check_files}),
    
    NewState#state{
        watched_files = Files,
        file_timestamps = Timestamps,
        watch_timer = Timer
    }.

stop_file_watching(#state{watch_timer = undefined} = State) ->
    State;
stop_file_watching(#state{watch_timer = Timer} = State) ->
    timer:cancel(Timer),
    State#state{
        watched_files = [],
        file_timestamps = #{},
        watch_timer = undefined
    }.

check_file_changes(#state{watched_files = Files, file_timestamps = Timestamps} = State) ->
    ChangedFiles = lists:foldl(fun(File, Acc) ->
        CurrentTime = get_file_timestamp(File),
        OldTime = maps:get(File, Timestamps, 0),
        
        case CurrentTime > OldTime of
            true ->
                io:format("File changed: ~s~n", [File]),
                % Auto-compile and reload
                spawn(fun() -> 
                    case do_compile_and_reload(File) of
                        {ok, Info} ->
                            io:format("Auto-reloaded: ~p~n", [maps:get(module, Info)]);
                        {error, Reason} ->
                            io:format("Auto-reload failed: ~p~n", [Reason])
                    end
                end),
                [{File, CurrentTime} | Acc];
            false ->
                Acc
        end
    end, [], Files),
    
    % Update timestamps
    NewTimestamps = lists:foldl(fun({File, Time}, Acc) ->
        maps:put(File, Time, Acc)
    end, Timestamps, ChangedFiles),
    
    State#state{file_timestamps = NewTimestamps}.

%% Helper functions

is_application_module(Module) ->
    ModuleStr = atom_to_list(Module),
    lists:any(fun(Prefix) ->
        lists:prefix(Prefix, ModuleStr)
    end, ["agent", "openai", "mcp"]).

determine_output_dir(SourceFile) ->
    case string:str(SourceFile, "/apps/") of
        0 ->
            "_build/default/lib/agents/ebin";
        Pos ->
            % Extract app name from path
            After = lists:nthtail(Pos + 4, SourceFile),
            case string:chr(After, $/) of
                0 -> "_build/default/lib/agents/ebin";
                AppEnd ->
                    AppName = lists:sublist(After, AppEnd - 1),
                    "_build/default/lib/" ++ AppName ++ "/ebin"
            end
    end.

get_file_timestamp(File) ->
    case file:read_file_info(File) of
        {ok, #file_info{mtime = MTime}} ->
            calendar:datetime_to_gregorian_seconds(MTime);
        {error, _} ->
            0
    end.

restart_application_supervisors(App) ->
    % Get application supervisor
    case application:get_key(App, mod) of
        {ok, {Mod, _}} ->
            % Find supervisor processes
            case whereis(Mod) of
                undefined -> ok;
                Pid when is_pid(Pid) ->
                    % Send code_change signal to supervisor
                    sys:suspend(Pid),
                    sys:change_code(Pid, Mod, undefined, undefined),
                    sys:resume(Pid)
            end;
        undefined ->
            ok
    end.