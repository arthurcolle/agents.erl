-module(agent_web_app).
-behaviour(application).

-export([start/2, stop/1, health_monitor/0]).

start(_StartType, _StartArgs) ->
    io:format("[APP] Starting agent_web application~n"),
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
                
                io:format("[APP] Starting default agent initialization~n"),
                agent_initializer:init_default_agents(),
                io:format("[APP] Default agent initialization completed~n"),
                
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

health_monitor() ->
    timer:sleep(30000), % Wait 30 seconds between checks
    io:format("[HEALTH] System health check at ~p~n", [calendar:local_time()]),
    
    % Check critical processes
    CriticalProcesses = [mcp_registry, mcp_connection_manager, mcp_manager],
    lists:foreach(fun(Process) ->
        case whereis(Process) of
            undefined ->
                io:format("[HEALTH] CRITICAL: Process ~p is down!~n", [Process]);
            _Pid ->
                io:format("[HEALTH] OK: Process ~p is running~n", [Process])
        end
    end, CriticalProcesses),
    
    % Check memory usage
    MemoryInfo = erlang:memory(),
    TotalMem = proplists:get_value(total, MemoryInfo),
    ProcessMem = proplists:get_value(processes, MemoryInfo),
    io:format("[HEALTH] Memory: ~p MB total, ~p MB processes~n", 
              [TotalMem div 1024 div 1024, ProcessMem div 1024 div 1024]),
    
    % Check port connectivity
    Port = application:get_env(agent_web, port, 8080),
    case gen_tcp:connect("localhost", Port, [], 1000) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            io:format("[HEALTH] OK: Web server responding on port ~p~n", [Port]);
        {error, ConnError} ->
            io:format("[HEALTH] ERROR: Web server not responding on port ~p: ~p~n", [Port, ConnError])
    end,
    
    health_monitor().