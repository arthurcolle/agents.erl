-module(auto_healing_startup).

-export([ensure_all_systems_running/0,
         start_with_healing/0,
         monitor_and_fix/0]).

%% Automatically start all required systems with error handling
ensure_all_systems_running() ->
    error_logger:info_msg("[AUTO_HEALING] Starting system integrity check...~n"),
    
    %% Start self-healing supervisor first
    ensure_started(self_healing_supervisor),
    case whereis(self_healing_supervisor) of
        undefined ->
            error_logger:info_msg("[AUTO_HEALING] Starting self-healing supervisor~n"),
            case self_healing_supervisor:start_link() of
                {ok, SupPid} ->
                    error_logger:info_msg("[AUTO_HEALING] Self-healing supervisor started with PID ~p~n", [SupPid]);
                {error, {already_started, SupPid2}} ->
                    error_logger:info_msg("[AUTO_HEALING] Self-healing supervisor already running with PID ~p~n", [SupPid2]);
                SupError ->
                    error_logger:error_msg("[AUTO_HEALING] Failed to start self-healing supervisor: ~p~n", [SupError])
            end;
        SupPid ->
            error_logger:info_msg("[AUTO_HEALING] Self-healing supervisor already running with PID ~p~n", [SupPid])
    end,
    
    %% List of critical processes that must be running
    CriticalProcesses = [
        {mcp_registry, fun() -> mcp_registry:start_link() end},
        {mcp_monitor, fun() -> mcp_monitor:start_link() end},
        {mcp_orchestration_engine, fun() -> mcp_orchestration_engine:start_link() end},
        {mcp_manager, fun() -> mcp_manager:start_link() end},
        {enhanced_logger, fun() -> enhanced_logger:start_link() end}
    ],
    
    %% Start each process with error handling
    lists:foreach(fun({Name, StartFun}) ->
        case whereis(Name) of
            undefined ->
                error_logger:info_msg("[AUTO_HEALING] Starting missing process: ~p~n", [Name]),
                case StartFun() of
                    {ok, NewPid} ->
                        error_logger:info_msg("[AUTO_HEALING] Successfully started ~p with PID ~p~n", [Name, NewPid]);
                    {error, {already_started, ExistingPid}} ->
                        error_logger:info_msg("[AUTO_HEALING] ~p already running with PID ~p~n", [Name, ExistingPid]);
                    StartError ->
                        error_logger:error_msg("[AUTO_HEALING] Failed to start ~p: ~p~n", [Name, StartError])
                end;
            ExistingPid ->
                error_logger:info_msg("[AUTO_HEALING] ~p already running with PID ~p~n", [Name, ExistingPid])
        end
    end, CriticalProcesses),
    
    %% Register monitors for each critical process (only if self_healing_supervisor is available)
    case whereis(self_healing_supervisor) of
        undefined ->
            error_logger:warning_msg("[AUTO_HEALING] Self-healing supervisor not available, skipping monitor registration~n");
        _ ->
            lists:foreach(fun({Name, StartFun}) ->
                try
                    self_healing_supervisor:add_monitor(Name, Name, #{
                        health_check => fun(_) -> 
                            case whereis(Name) of
                                undefined -> {error, not_running};
                                Pid when is_pid(Pid) -> 
                                    case is_process_alive(Pid) of
                                        true -> ok;
                                        false -> {error, dead_process}
                                    end
                            end
                        end,
                        auto_fix => fun(_, _) -> 
                            error_logger:info_msg("[AUTO_HEALING] Attempting to restart ~p~n", [Name]),
                            StartFun()
                        end,
                        max_retries => 5
                    })
                catch
                    Error:Reason ->
                        error_logger:warning_msg("[AUTO_HEALING] Failed to add monitor for ~p: ~p:~p~n", [Name, Error, Reason])
                end
            end, CriticalProcesses)
    end,
    
    %% Start continuous monitoring
    spawn(fun() -> monitor_and_fix() end),
    
    ok.

%% Start the application with self-healing enabled
start_with_healing() ->
    %% Ensure error_resilience module is loaded
    code:ensure_loaded(error_resilience),
    code:ensure_loaded(self_healing_supervisor),
    
    %% Start the main application
    case application:start(agent_web) of
        ok ->
            error_logger:info_msg("[AUTO_HEALING] Application started successfully~n"),
            ensure_all_systems_running();
        {error, {already_started, agent_web}} ->
            error_logger:info_msg("[AUTO_HEALING] Application already started~n"),
            ensure_all_systems_running();
        Error ->
            error_logger:error_msg("[AUTO_HEALING] Failed to start application: ~p~n", [Error]),
            Error
    end.

%% Continuous monitoring and auto-fixing
monitor_and_fix() ->
    timer:sleep(10000), % Check every 10 seconds
    
    %% Check MCP connections
    fix_mcp_connections(),
    
    %% Check memory usage
    check_and_fix_memory(),
    
    %% Check for crashed gen_servers
    check_and_restart_crashed_servers(),
    
    %% Recurse
    monitor_and_fix().

fix_mcp_connections() ->
    case error_resilience:safe_call(mcp_registry, list_servers, 5000, []) of
        [] -> ok;
        Servers ->
            lists:foreach(fun(Server) ->
                case maps:get(status, Server, <<"error">>) of
                    <<"error">> ->
                        Id = maps:get(id, Server),
                        error_logger:info_msg("[AUTO_HEALING] Attempting to reconnect MCP server ~p~n", [Id]),
                        error_resilience:safe_cast(mcp_manager, {reconnect_server, Id});
                    _ -> ok
                end
            end, Servers)
    end.

check_and_fix_memory() ->
    try
        Memory = erlang:memory(),
        Total = proplists:get_value(total, Memory, 0),
        %% If memory usage is too high, trigger garbage collection
        if
            Total > 500000000 -> % 500MB
                error_logger:warning_msg("[AUTO_HEALING] High memory usage detected (~p MB), triggering GC~n", [Total div 1000000]),
                [erlang:garbage_collect(Pid) || Pid <- erlang:processes()],
                ok;
            true -> ok
        end
    catch
        _:_ -> ok
    end.

check_and_restart_crashed_servers() ->
    %% Get list of registered names
    RegisteredNames = registered(),
    
    %% Check each registered process
    lists:foreach(fun(Name) ->
        case whereis(Name) of
            undefined ->
                %% Process is registered but not running
                error_logger:warning_msg("[AUTO_HEALING] Registered process ~p not running~n", [Name]),
                %% Try to restart if it's a known process
                maybe_restart_process(Name);
            Pid ->
                %% Check if process is alive
                case is_process_alive(Pid) of
                    false ->
                        error_logger:warning_msg("[AUTO_HEALING] Process ~p (~p) is dead~n", [Name, Pid]),
                        maybe_restart_process(Name);
                    true -> ok
                end
        end
    end, RegisteredNames).

maybe_restart_process(mcp_monitor) ->
    mcp_monitor:start_link();
maybe_restart_process(mcp_orchestration_engine) ->
    mcp_orchestration_engine:start_link();
maybe_restart_process(mcp_registry) ->
    mcp_registry:start_link();
maybe_restart_process(mcp_manager) ->
    mcp_manager:start_link();
maybe_restart_process(_) ->
    %% Unknown process, skip
    ok.

ensure_started(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} -> ok;
        {error, Reason} ->
            error_logger:error_msg("[AUTO_HEALING] Failed to load module ~p: ~p~n", [Module, Reason])
    end.