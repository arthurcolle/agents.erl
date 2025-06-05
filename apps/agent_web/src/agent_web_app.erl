-module(agent_web_app).
-behaviour(application).

-export([start/2, stop/1, health_monitor/0, install_better_logging/0]).

%% Colorful logging macros
-define(LOG_STARTUP(Msg), colored_logger:production(stable, "[APP] " ++ Msg)).
-define(LOG_STARTUP(Msg, Args), colored_logger:production(stable, io_lib:format("[APP] " ++ Msg, Args))).
-define(LOG_WARNING(Msg), colored_logger:alarm(medium, "[WARNING] " ++ Msg)).
-define(LOG_WARNING(Msg, Args), colored_logger:alarm(medium, io_lib:format("[WARNING] " ++ Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, "[ERROR] " ++ Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format("[ERROR] " ++ Msg, Args))).
-define(LOG_HEALTH(Msg), colored_logger:system(cpu, "[HEALTH] " ++ Msg)).
-define(LOG_HEALTH(Msg, Args), colored_logger:system(cpu, io_lib:format("[HEALTH] " ++ Msg, Args))).
-define(LOG_SUCCESS(Msg), colored_logger:complete(success, "[APP] " ++ Msg)).
-define(LOG_SUCCESS(Msg, Args), colored_logger:complete(success, io_lib:format("[APP] " ++ Msg, Args))).

start(_StartType, _StartArgs) ->
    %% Install better logging FIRST, before any other output
    install_better_logging(),
    ?LOG_STARTUP("Starting agent_web application"),
    %% Ensure all required applications are started in correct order
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    ok = application:ensure_started(inets),
    ok = application:ensure_started(ranch),
    ok = application:ensure_started(cowlib),
    ok = application:ensure_started(cowboy),
    ok = application:ensure_started(gun),
    ok = application:ensure_started(yamerl),
    ok = application:ensure_started(gproc),
    ok = application:ensure_started(mnesia),
    ok = application:ensure_started(jsx),
    ok = application:ensure_started(agents),
    ok = application:ensure_started(openai),
    ?LOG_STARTUP("All required applications started"),
    case agent_web_sup:start_link() of
        {ok, Pid} ->
            ?LOG_STARTUP("Supervisor started successfully with PID ~p", [Pid]),
            %% Initialize default agents after supervisor starts
            spawn(fun() ->
                ?LOG_STARTUP("Waiting 1 second before initializing default agents"),
                timer:sleep(1000), %% Give the system time to fully initialize
                
                %% Verify all critical processes are running
                ?LOG_STARTUP("Verifying critical processes..."),
                CriticalProcesses = [mcp_registry, mcp_connection_manager, mcp_manager],
                lists:foreach(fun(Process) ->
                    case whereis(Process) of
                        undefined ->
                            ?LOG_WARNING("Process ~p is not running", [Process]);
                        Pid when is_pid(Pid) ->
                            ?LOG_STARTUP("Process ~p is running with PID ~p", [Process, Pid]);
                        Other ->
                            ?LOG_STARTUP("Process ~p has unexpected value: ~p", [Process, Other])
                    end
                end, CriticalProcesses),
                
                %% Check if Cowboy is listening
                Port = application:get_env(agent_web, port, 8080),
                case gen_tcp:connect("localhost", Port, [], 1000) of
                    {ok, Socket} ->
                        gen_tcp:close(Socket),
                        ?LOG_STARTUP("Cowboy is listening on port ~p", [Port]);
                    {error, ConnError} ->
                        ?LOG_WARNING("Cowboy not responding on port ~p: ~p", [Port, ConnError])
                end,
                
                %% Initialize default agents with error boundary
                safe_initialize("default agents", fun() ->
                    ?LOG_STARTUP("Starting default agent initialization"),
                    agent_initializer:init_default_agents(),
                    ?LOG_STARTUP("Default agent initialization completed")
                end),
                
                %% Initialize conversation storage with error boundary
                safe_initialize("conversation storage", fun() ->
                    ?LOG_STARTUP("Initializing conversation storage"),
                    conversation_handler:init_storage(),
                    ?LOG_STARTUP("Conversation storage initialized")
                end),
                
                %% Initialize timeline storage with error boundary
                safe_initialize("timeline storage", fun() ->
                    ?LOG_STARTUP("Initializing timeline storage"),
                    timeline_handler:init_storage(),
                    ?LOG_STARTUP("Timeline storage initialized")
                end),
                
                %% Initialize MCP server configuration with error boundary
                safe_initialize("MCP server configuration", fun() ->
                    ?LOG_STARTUP("Initializing MCP server configuration"),
                    mcp_server_config:init(),
                    ?LOG_STARTUP("MCP server configuration initialized"),
                    
                    %% Check and fix environment variables for MCP servers
                    ?LOG_STARTUP("Checking MCP server environment requirements"),
                    mcp_env_checker:check_and_fix_servers(),
                    ?LOG_STARTUP("MCP environment check completed")
                end),
                
                %% Initialize OAuth system with error boundary
                safe_initialize("OAuth system", fun() ->
                    ?LOG_STARTUP("Initializing OAuth system"),
                    oauth_handler:init_oauth_system(),
                    ?LOG_STARTUP("OAuth system initialized")
                end),
                
                %% Initialize training data generation system with error boundary
                safe_initialize("training data system", fun() ->
                    ?LOG_STARTUP("Initializing training data generation system"),
                    training_data_generator:schedule_generation(),
                    ?LOG_STARTUP("Training data generation system initialized")
                end),
                
                %% Start health monitoring
                ?LOG_STARTUP("Starting health monitor"),
                spawn(fun health_monitor/0),
                
                %% Log that conversation logging is active
                ?LOG_STARTUP("Conversation stats logging is ACTIVE - all messages will be tracked"),
                ?LOG_STARTUP("Stats table will display every 30 seconds"),
                ?LOG_STARTUP("Use /api/stats endpoints to control logging")
            end),
            {ok, Pid};
        {error, Reason} ->
            ?LOG_ERROR("Failed to start supervisor: ~p", [Reason]),
            {error, Reason}
    end.

stop(_State) ->
    ?LOG_STARTUP("Stopping agent_web application"),
    ok.

%% Safe initialization wrapper to prevent startup crashes
safe_initialize(ComponentName, InitFun) ->
    try
        InitFun(),
        ?LOG_STARTUP("Successfully initialized ~s", [ComponentName])
    catch
        Class:Error:Stack ->
            ?LOG_ERROR("Failed to initialize ~s: ~p:~p", [ComponentName, Class, Error]),
            ?LOG_ERROR("Stack trace: ~p", [Stack]),
            ?LOG_WARNING("Continuing startup despite ~s failure", [ComponentName])
    end.

health_monitor() ->
    timer:sleep(30000), % Wait 30 seconds between checks
    ?LOG_HEALTH("System health check at ~p", [calendar:local_time()]),
    
    % Get comprehensive system status
    try
        SystemStatus = system_health_handler:get_system_status(),
        OverallHealth = maps:get(overall_health, SystemStatus),
        
        case OverallHealth of
            <<"healthy">> ->
                ?LOG_HEALTH("System is healthy");
            <<"degraded">> ->
                ?LOG_HEALTH("WARNING: System is in degraded state"),
                % Broadcast health alert to websockets
                catch agent_ws_handler:broadcast(#{
                    type => <<"system_health">>,
                    status => <<"degraded">>,
                    data => SystemStatus
                })
        end,
        
        % Check for critical issues and broadcast them
        Processes = maps:get(processes, SystemStatus),
        DownProcesses = [P || P <- Processes, maps:get(status, P) =:= <<"down">>],
        if 
            length(DownProcesses) > 0 ->
                ?LOG_HEALTH("CRITICAL: Some processes are down: ~p", [DownProcesses]),
                catch agent_ws_handler:broadcast(#{
                    type => <<"system_error">>,
                    severity => <<"critical">>,
                    message => <<"Critical processes are down">>,
                    details => DownProcesses
                });
            true -> ok
        end,
        
        % Log memory usage
        Memory = maps:get(memory, SystemStatus),
        TotalMB = maps:get(total_mb, Memory),
        ProcessesMB = maps:get(processes_mb, Memory),
        ?LOG_HEALTH("Memory: ~p MB total, ~p MB processes", [TotalMB, ProcessesMB]),
        
        % Check web server
        WebServer = maps:get(web_server, SystemStatus),
        case maps:get(status, WebServer) of
            <<"running">> ->
                ?LOG_HEALTH("OK: Web server responding");
            _ ->
                ?LOG_HEALTH("ERROR: Web server not responding"),
                catch agent_ws_handler:broadcast(#{
                    type => <<"system_error">>,
                    severity => <<"high">>,
                    message => <<"Web server not responding">>,
                    details => WebServer
                })
        end
        
    catch
        Class:Error ->
            ?LOG_HEALTH("ERROR: Health check failed: ~p:~p", [Class, Error]),
            catch agent_ws_handler:broadcast(#{
                type => <<"system_error">>,
                severity => <<"high">>,
                message => <<"Health check failed">>,
                details => #{class => Class, error => Error}
            })
    end,
    
    health_monitor().

%% Install better logging configuration
install_better_logging() ->
    %% Suppress error_logger completely 
    error_logger:tty(false),
    
    %% Remove all existing handlers
    try logger:remove_handler(default) catch _:_ -> ok end,
    try logger:remove_handler(colored_logger) catch _:_ -> ok end,
    
    %% Install clean colored handler - wrap in try-catch to handle errors gracefully
    try
        logger:add_handler(colored_logger, colored_logger_handler, #{
            level => all,
            config => #{disable_colors => false}
        })
    catch
        _Class:_Error ->
            %% Fallback to basic logging if colored handler fails
            io:format("Warning: Could not install colored logger handler, using basic logging~n")
    end,
    
    %% Set logger level 
    logger:set_primary_config(level, info),
    
    ok.