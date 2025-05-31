-module(agent_web_app).
-behaviour(application).

-export([start/2, stop/1, health_monitor/0]).

start(_StartType, _StartArgs) ->
    io:format("[APP] Starting agent_web application~n"),
    %% Ensure SSL, Gun and inets applications are started
    ok = application:ensure_started(ssl),
    ok = application:ensure_started(inets),
    ok = application:ensure_started(gun),
    io:format("[APP] SSL and Gun applications started~n"),
    case agent_web_sup:start_link() of
        {ok, Pid} ->
            io:format("[APP] Supervisor started successfully with PID ~p~n", [Pid]),
            %% Initialize default agents after supervisor starts
            spawn(fun() ->
                io:format("[APP] Waiting 1 second before initializing default agents~n"),
                timer:sleep(1000), %% Give the system time to fully initialize
                
                %% Verify all critical processes are running
                io:format("[APP] Verifying critical processes...~n"),
                CriticalProcesses = [mcp_registry, mcp_connection_manager, mcp_manager],
                lists:foreach(fun(Process) ->
                    case whereis(Process) of
                        undefined ->
                            io:format("[WARNING] Process ~p is not running~n", [Process]);
                        Pid when is_pid(Pid) ->
                            io:format("[APP] Process ~p is running with PID ~p~n", [Process, Pid]);
                        Other ->
                            io:format("[APP] Process ~p has unexpected value: ~p~n", [Process, Other])
                    end
                end, CriticalProcesses),
                
                %% Check if Cowboy is listening
                Port = application:get_env(agent_web, port, 8080),
                case gen_tcp:connect("localhost", Port, [], 1000) of
                    {ok, Socket} ->
                        gen_tcp:close(Socket),
                        io:format("[APP] Cowboy is listening on port ~p~n", [Port]);
                    {error, ConnError} ->
                        io:format("[WARNING] Cowboy not responding on port ~p: ~p~n", [Port, ConnError])
                end,
                
                %% Initialize default agents with error boundary
                safe_initialize("default agents", fun() ->
                    io:format("[APP] Starting default agent initialization~n"),
                    agent_initializer:init_default_agents(),
                    io:format("[APP] Default agent initialization completed~n")
                end),
                
                %% Initialize conversation storage with error boundary
                safe_initialize("conversation storage", fun() ->
                    io:format("[APP] Initializing conversation storage~n"),
                    conversation_handler:init_storage(),
                    io:format("[APP] Conversation storage initialized~n")
                end),
                
                %% Initialize timeline storage with error boundary
                safe_initialize("timeline storage", fun() ->
                    io:format("[APP] Initializing timeline storage~n"),
                    timeline_handler:init_storage(),
                    io:format("[APP] Timeline storage initialized~n")
                end),
                
                %% Initialize MCP server configuration with error boundary
                safe_initialize("MCP server configuration", fun() ->
                    io:format("[APP] Initializing MCP server configuration~n"),
                    mcp_server_config:init(),
                    io:format("[APP] MCP server configuration initialized~n")
                end),
                
                %% Initialize OAuth system with error boundary
                safe_initialize("OAuth system", fun() ->
                    io:format("[APP] Initializing OAuth system~n"),
                    oauth_handler:init_oauth_system(),
                    io:format("[APP] OAuth system initialized~n")
                end),
                
                %% Start health monitoring
                io:format("[APP] Starting health monitor~n"),
                spawn(fun health_monitor/0)
            end),
            {ok, Pid};
        {error, Reason} ->
            io:format("[ERROR] Failed to start supervisor: ~p~n", [Reason]),
            {error, Reason}
    end.

stop(_State) ->
    io:format("[APP] Stopping agent_web application~n"),
    ok.

%% Safe initialization wrapper to prevent startup crashes
safe_initialize(ComponentName, InitFun) ->
    try
        InitFun(),
        io:format("[APP] Successfully initialized ~s~n", [ComponentName])
    catch
        Class:Error:Stack ->
            io:format("[ERROR] Failed to initialize ~s: ~p:~p~n", [ComponentName, Class, Error]),
            io:format("[ERROR] Stack trace: ~p~n", [Stack]),
            io:format("[APP] Continuing startup despite ~s failure~n", [ComponentName])
    end.

health_monitor() ->
    timer:sleep(30000), % Wait 30 seconds between checks
    io:format("[HEALTH] System health check at ~p~n", [calendar:local_time()]),
    
    % Get comprehensive system status
    try
        SystemStatus = system_health_handler:get_system_status(),
        OverallHealth = maps:get(overall_health, SystemStatus),
        
        case OverallHealth of
            <<"healthy">> ->
                io:format("[HEALTH] System is healthy~n");
            <<"degraded">> ->
                io:format("[HEALTH] WARNING: System is in degraded state~n"),
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
                io:format("[HEALTH] CRITICAL: Some processes are down: ~p~n", [DownProcesses]),
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
        io:format("[HEALTH] Memory: ~p MB total, ~p MB processes~n", [TotalMB, ProcessesMB]),
        
        % Check web server
        WebServer = maps:get(web_server, SystemStatus),
        case maps:get(status, WebServer) of
            <<"running">> ->
                io:format("[HEALTH] OK: Web server responding~n");
            _ ->
                io:format("[HEALTH] ERROR: Web server not responding~n"),
                catch agent_ws_handler:broadcast(#{
                    type => <<"system_error">>,
                    severity => <<"high">>,
                    message => <<"Web server not responding">>,
                    details => WebServer
                })
        end
        
    catch
        Class:Error ->
            io:format("[HEALTH] ERROR: Health check failed: ~p:~p~n", [Class, Error]),
            catch agent_ws_handler:broadcast(#{
                type => <<"system_error">>,
                severity => <<"high">>,
                message => <<"Health check failed">>,
                details => #{class => Class, error => Error}
            })
    end,
    
    health_monitor().