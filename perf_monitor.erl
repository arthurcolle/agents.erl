-module(perf_monitor).
-export([start/0, stop/0, report/0, jit_stats/0, memory_report/0]).

%% Performance monitoring for BeamAsm JIT system

start() ->
    io:format("=== BeamAsm JIT Performance Monitor ===~n"),
    io:format("Erlang Version: ~s~n", [erlang:system_info(system_version)]),
    io:format("JIT Flavor: ~p~n", [erlang:system_info(emu_flavor)]),
    io:format("Architecture: ~s~n", [erlang:system_info(system_architecture)]),
    io:format("Schedulers: ~p~n", [erlang:system_info(schedulers)]),
    
    %% Start monitoring process
    Pid = spawn(fun monitor_loop/0),
    register(perf_monitor, Pid),
    {ok, Pid}.

stop() ->
    case whereis(perf_monitor) of
        undefined -> not_started;
        Pid -> 
            Pid ! stop,
            ok
    end.

report() ->
    io:format("~n=== Performance Report ===~n"),
    jit_stats(),
    memory_report(),
    scheduler_report(),
    process_report().

jit_stats() ->
    io:format("~n--- JIT Statistics ---~n"),
    io:format("Emulator Flavor: ~p~n", [erlang:system_info(emu_flavor)]),
    io:format("Machine: ~s~n", [erlang:system_info(machine)]),
    
    %% Check if we're running with optimizations
    case erlang:system_info(emu_flavor) of
        jit ->
            io:format("✓ BeamAsm JIT is ENABLED~n"),
            io:format("✓ Native code generation active~n");
        emu ->
            io:format("⚠ Running interpreter mode (no JIT)~n")
    end.

memory_report() ->
    io:format("~n--- Memory Statistics ---~n"),
    Memory = erlang:memory(),
    lists:foreach(fun({Type, Bytes}) ->
        MB = Bytes / (1024 * 1024),
        io:format("~-15s: ~.2f MB~n", [Type, MB])
    end, Memory).

scheduler_report() ->
    io:format("~n--- Scheduler Statistics ---~n"),
    io:format("Schedulers Online: ~p~n", [erlang:system_info(schedulers_online)]),
    io:format("Logical Processors: ~p~n", [erlang:system_info(logical_processors)]),
    io:format("Async Threads: ~p~n", [erlang:system_info(thread_pool_size)]).

process_report() ->
    io:format("~n--- Process Statistics ---~n"),
    io:format("Process Count: ~p~n", [erlang:system_info(process_count)]),
    io:format("Process Limit: ~p~n", [erlang:system_info(process_limit)]),
    io:format("Port Count: ~p~n", [erlang:system_info(port_count)]).

%% Internal monitoring loop
monitor_loop() ->
    receive
        stop -> 
            io:format("Performance monitor stopped~n"),
            ok;
        {report_interval, Interval} ->
            timer:sleep(Interval),
            report(),
            monitor_loop();
        _ ->
            monitor_loop()
    after 30000 ->  % Report every 30 seconds by default
        report(),
        monitor_loop()
    end.

%% Agent-specific performance monitoring
agent_performance() ->
    io:format("~n--- Agent Performance ---~n"),
    
    %% Check agent processes
    AgentProcs = [P || P <- processes(), 
                       element(1, process_info(P, initial_call)) =:= {agent_instance, init, 1}],
    io:format("Active Agent Processes: ~p~n", [length(AgentProcs)]),
    
    %% Memory usage per agent
    lists:foreach(fun(Pid) ->
        case process_info(Pid, [memory, message_queue_len, heap_size]) of
            [{memory, Mem}, {message_queue_len, MsgQ}, {heap_size, Heap}] ->
                io:format("Agent ~p: Mem=~.2fKB, MsgQ=~p, Heap=~p~n", 
                         [Pid, Mem/1024, MsgQ, Heap]);
            _ -> ok
        end
    end, lists:sublist(AgentProcs, 10)).  % Limit to first 10 for readability