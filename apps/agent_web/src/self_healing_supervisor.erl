-module(self_healing_supervisor).
-behaviour(gen_server).

%% API
-export([start_link/0,
         add_monitor/3,
         remove_monitor/1,
         get_health_status/0,
         auto_fix/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    monitors = #{} :: #{atom() => monitor_spec()},
    auto_fixes = #{} :: #{atom() => fix_function()},
    health_checks = #{} :: #{atom() => check_function()},
    error_patterns = [] :: [error_pattern()],
    recovery_attempts = #{} :: #{atom() => integer()}
}).

-type monitor_spec() :: #{
    module := atom(),
    health_check := fun(() -> ok | {error, term()}),
    auto_fix := fun((term()) -> ok | {error, term()}),
    max_retries := integer()
}.

-type fix_function() :: fun((term()) -> ok | {error, term()}).
-type check_function() :: fun(() -> ok | {error, term()}).
-type error_pattern() :: #{
    pattern := term(),
    fix := fix_function()
}.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_monitor(Name, Module, Options) ->
    gen_server:call(?MODULE, {add_monitor, Name, Module, Options}).

remove_monitor(Name) ->
    gen_server:call(?MODULE, {remove_monitor, Name}).

get_health_status() ->
    gen_server:call(?MODULE, get_health_status).

auto_fix(Error) ->
    gen_server:call(?MODULE, {auto_fix, Error}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Schedule periodic health checks
    timer:send_interval(5000, check_health),
    
    %% Initialize with common error patterns and fixes
    ErrorPatterns = [
        #{pattern => {badarith, '_'}, 
          fix => fun fix_arithmetic_error/1},
        #{pattern => {noproc, '_'},
          fix => fun fix_missing_process/1},
        #{pattern => {connection_failed, '_'},
          fix => fun fix_connection_error/1}
    ],
    
    {ok, #state{error_patterns = ErrorPatterns}}.

handle_call({add_monitor, Name, Module, Options}, _From, State) ->
    MonitorSpec = #{
        module => Module,
        health_check => maps:get(health_check, Options, fun default_health_check/1),
        auto_fix => maps:get(auto_fix, Options, fun default_auto_fix/2),
        max_retries => maps:get(max_retries, Options, 3)
    },
    NewMonitors = maps:put(Name, MonitorSpec, State#state.monitors),
    {reply, ok, State#state{monitors = NewMonitors}};

handle_call({remove_monitor, Name}, _From, State) ->
    NewMonitors = maps:remove(Name, State#state.monitors),
    {reply, ok, State#state{monitors = NewMonitors}};

handle_call(get_health_status, _From, State) ->
    HealthStatus = check_all_monitors(State),
    {reply, HealthStatus, State};

handle_call({auto_fix, Error}, _From, State) ->
    Result = apply_auto_fix(Error, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_health, State) ->
    %% Perform health checks and auto-fix if needed
    spawn(fun() -> perform_health_checks(State) end),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

check_all_monitors(#state{monitors = Monitors}) ->
    maps:fold(fun(Name, Spec, Acc) ->
        case check_monitor_health(Name, Spec) of
            ok -> [{Name, healthy} | Acc];
            {error, Reason} -> [{Name, {unhealthy, Reason}} | Acc]
        end
    end, [], Monitors).

check_monitor_health(Name, #{module := Module, health_check := HealthCheck}) ->
    try
        case whereis(Name) of
            undefined -> {error, not_running};
            Pid when is_pid(Pid) ->
                case HealthCheck(Module) of
                    ok -> ok;
                    Error -> Error
                end
        end
    catch
        _:Reason -> {error, Reason}
    end.

perform_health_checks(State) ->
    maps:foreach(fun(Name, Spec) ->
        case check_monitor_health(Name, Spec) of
            ok -> ok;
            {error, Reason} ->
                error_logger:warning_msg("[SELF_HEALING] ~p is unhealthy: ~p~n", [Name, Reason]),
                attempt_auto_fix(Name, Reason, Spec, State)
        end
    end, State#state.monitors).

attempt_auto_fix(Name, Error, #{auto_fix := AutoFix, max_retries := MaxRetries}, State) ->
    Attempts = maps:get(Name, State#state.recovery_attempts, 0),
    if
        Attempts < MaxRetries ->
            case AutoFix(Name, Error) of
                ok ->
                    error_logger:info_msg("[SELF_HEALING] Successfully fixed ~p~n", [Name]),
                    ok;
                {error, FixError} ->
                    error_logger:error_msg("[SELF_HEALING] Failed to fix ~p: ~p~n", [Name, FixError]),
                    {error, FixError}
            end;
        true ->
            error_logger:error_msg("[SELF_HEALING] Max retries exceeded for ~p~n", [Name]),
            {error, max_retries_exceeded}
    end.

apply_auto_fix(Error, #state{error_patterns = Patterns}) ->
    case find_matching_pattern(Error, Patterns) of
        {ok, Fix} -> Fix(Error);
        not_found -> {error, no_fix_available}
    end.

find_matching_pattern(_Error, []) ->
    not_found;
find_matching_pattern(Error, [#{pattern := Pattern, fix := Fix} | Rest]) ->
    case match_error_pattern(Error, Pattern) of
        true -> {ok, Fix};
        false -> find_matching_pattern(Error, Rest)
    end.

match_error_pattern({Type, _}, {Type, '_'}) -> true;
match_error_pattern(_, _) -> false.

%% Default implementations
default_health_check(Module) ->
    case whereis(Module) of
        undefined -> {error, not_running};
        Pid when is_pid(Pid) -> ok
    end.

default_auto_fix(Name, _Error) ->
    %% Try to restart the process
    case whereis(Name) of
        undefined ->
            %% Process doesn't exist, try to start it
            {error, cannot_restart_unknown_process};
        Pid ->
            %% Process exists but unhealthy, restart it
            exit(Pid, kill),
            timer:sleep(100),
            %% Supervisor should restart it
            ok
    end.

%% Specific fix functions
fix_arithmetic_error({badarith, StackTrace}) ->
    %% Extract module and line from stacktrace
    case extract_error_location(StackTrace) of
        {ok, Module, Line} ->
            error_logger:info_msg("[SELF_HEALING] Fixing arithmetic error in ~p:~p~n", [Module, Line]),
            %% For now, just restart the affected process
            %% In a real implementation, we could patch the code
            {error, manual_fix_required};
        _ ->
            {error, cannot_locate_error}
    end.

fix_missing_process({noproc, {gen_server, call, [ProcessName | _]}}) ->
    error_logger:info_msg("[SELF_HEALING] Attempting to start missing process: ~p~n", [ProcessName]),
    %% Try to start the missing process
    case find_process_spec(ProcessName) of
        {ok, StartSpec} ->
            start_missing_process(StartSpec);
        not_found ->
            {error, unknown_process_spec}
    end.

fix_connection_error({connection_failed, Details}) ->
    error_logger:info_msg("[SELF_HEALING] Fixing connection error: ~p~n", [Details]),
    %% Reset connection pools, clear caches, etc.
    mcp_registry:cleanup_failed_connections(),
    ok.

extract_error_location([{Module, _Function, _Arity, Props} | _]) ->
    case proplists:get_value(line, Props) of
        undefined -> {error, no_line_info};
        Line -> {ok, Module, Line}
    end;
extract_error_location(_) ->
    {error, invalid_stacktrace}.

find_process_spec(mcp_orchestration_engine) ->
    {ok, {mcp_orchestration_engine, start_link, []}};
find_process_spec(_) ->
    not_found.

start_missing_process({Module, Function, Args}) ->
    try
        case apply(Module, Function, Args) of
            {ok, _Pid} -> ok;
            Error -> {error, Error}
        end
    catch
        _:Reason -> {error, Reason}
    end.