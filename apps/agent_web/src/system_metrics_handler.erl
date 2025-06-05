-module(system_metrics_handler).
-behaviour(cowboy_handler).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    erlang:send_after(1000, self(), update_metrics),
    {ok, State}.

websocket_handle({text, _}, State) ->
    {ok, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(update_metrics, State) ->
    Metrics = get_system_metrics(),
    Json = jsx:encode(Metrics),
    erlang:send_after(1000, self(), update_metrics),
    {reply, {text, Json}, State};
websocket_info(_Info, State) ->
    {ok, State}.

get_system_metrics() ->
    {TotalMem, AllocMem, _} = memsup:get_memory_data(),
    CpuUtil = cpu_sup:avg1() / 256,
    ProcessCount = erlang:system_info(process_count),
    ProcessLimit = erlang:system_info(process_limit),
    ReductionCount = element(1, erlang:statistics(reductions)),
    {_WallClock, _} = erlang:statistics(wall_clock),
    {RunTime, _} = erlang:statistics(runtime),
    MemoryInfo = erlang:memory(),
    
    AgentCount = try
        length(supervisor:which_children(agent_supervisor))
    catch
        _:_ -> 0
    end,
    
    #{
        timestamp => erlang:system_time(millisecond),
        memory => #{
            total => TotalMem * 1024 * 1024,
            allocated => AllocMem * 1024 * 1024,
            erlang_total => maps:get(total, MemoryInfo),
            erlang_processes => maps:get(processes, MemoryInfo),
            erlang_system => maps:get(system, MemoryInfo),
            erlang_atom => maps:get(atom, MemoryInfo),
            erlang_binary => maps:get(binary, MemoryInfo),
            erlang_code => maps:get(code, MemoryInfo),
            erlang_ets => maps:get(ets, MemoryInfo)
        },
        cpu => #{
            utilization => CpuUtil,
            runtime => RunTime
        },
        processes => #{
            count => ProcessCount,
            limit => ProcessLimit,
            utilization => (ProcessCount / ProcessLimit) * 100
        },
        reductions => #{
            total => ReductionCount
        },
        agents => #{
            count => AgentCount
        },
        uptime => erlang:system_time(millisecond) - element(1, erlang:statistics(wall_clock))
    }.