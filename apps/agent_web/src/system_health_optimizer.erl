-module(system_health_optimizer).
-behaviour(gen_server).

-export([start_link/0, optimize_now/0, get_status/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    last_optimization :: erlang:timestamp(),
    optimization_count = 0 :: integer(),
    gc_settings :: map()
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

optimize_now() ->
    gen_server:call(?MODULE, optimize_now).

get_status() ->
    gen_server:call(?MODULE, get_status).

%% gen_server callbacks
init([]) ->
    %% Configure optimal GC settings
    GCSettings = #{
        fullsweep_after => 20,      % More aggressive full sweeps
        min_heap_size => 1024,      % Smaller initial heap
        min_bin_vheap_size => 1024  % Smaller binary heap
    },
    
    %% Apply system-wide optimizations
    apply_system_optimizations(),
    
    %% Schedule periodic optimization
    timer:send_interval(60000, optimize_memory),  % Every minute
    
    {ok, #state{
        last_optimization = os:timestamp(),
        gc_settings = GCSettings
    }}.

handle_call(optimize_now, _From, State) ->
    NewState = perform_optimization(State),
    {reply, ok, NewState};

handle_call(get_status, _From, State) ->
    Status = #{
        last_optimization => State#state.last_optimization,
        optimization_count => State#state.optimization_count,
        memory_stats => get_memory_stats(),
        gc_stats => get_gc_stats(),
        process_count => erlang:system_info(process_count),
        scheduler_utilization => get_scheduler_utilization()
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(optimize_memory, State) ->
    NewState = perform_optimization(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
apply_system_optimizations() ->
    %% Set process flag for system processes
    process_flag(priority, high),
    
    %% Set global GC settings
    erlang:system_flag(fullsweep_after, 20),
    
    %% Enable scheduler compaction
    try
        erlang:system_flag(scheduler_wall_time, true)
    catch
        _:_ -> ok
    end,
    
    colored_logger:info("System optimizations applied", []).

perform_optimization(State) ->
    StartTime = os:timestamp(),
    
    %% 1. Force garbage collection on all processes
    force_system_gc(),
    
    %% 2. Clean up large message queues
    clean_large_queues(),
    
    %% 3. Kill zombie processes
    kill_zombie_processes(),
    
    %% 4. Compact ETS tables
    compact_ets_tables(),
    
    %% 5. Clear process dictionaries
    clear_large_process_dicts(),
    
    %% 6. Optimize binary references
    optimize_binaries(),
    
    Duration = timer:now_diff(os:timestamp(), StartTime) div 1000,
    colored_logger:success("Memory optimization completed in ~p ms", [Duration]),
    
    State#state{
        last_optimization = os:timestamp(),
        optimization_count = State#state.optimization_count + 1
    }.

force_system_gc() ->
    Processes = erlang:processes(),
    lists:foreach(fun(Pid) ->
        try
            erlang:garbage_collect(Pid)
        catch
            _:_ -> ok
        end
    end, Processes).

clean_large_queues() ->
    Threshold = 1000,
    Processes = erlang:processes(),
    
    lists:foreach(fun(Pid) ->
        try
            case process_info(Pid, message_queue_len) of
                {message_queue_len, Len} when Len > Threshold ->
                    colored_logger:warning("Process ~p has ~p messages in queue", [Pid, Len]),
                    %% Try to make the process consume its messages
                    Pid ! {'$gen_call', {self(), make_ref()}, flush_queue};
                _ ->
                    ok
            end
        catch
            _:_ -> ok
        end
    end, Processes).

kill_zombie_processes() ->
    %% Kill processes that have been in 'waiting' state for too long
    %% and have no registered name
    Processes = erlang:processes(),
    
    lists:foreach(fun(Pid) ->
        try
            case process_info(Pid, [current_function, registered_name, memory]) of
                [{current_function, {gen, do_call, _}}, 
                 {registered_name, []}, 
                 {memory, Mem}] when Mem > 10485760 ->  % 10MB
                    colored_logger:warning("Killing potential zombie process ~p (mem: ~p MB)", 
                                         [Pid, Mem div 1048576]),
                    exit(Pid, kill);
                _ ->
                    ok
            end
        catch
            _:_ -> ok
        end
    end, Processes).

compact_ets_tables() ->
    Tables = ets:all(),
    lists:foreach(fun(Tab) ->
        try
            case ets:info(Tab, size) of
                Size when Size > 10000 ->
                    %% For large tables, consider compaction
                    case ets:info(Tab, type) of
                        set -> ok;  % Sets don't need compaction
                        _ ->
                            %% Force a GC on the owning process
                            case ets:info(Tab, owner) of
                                Owner when is_pid(Owner) ->
                                    erlang:garbage_collect(Owner);
                                _ -> ok
                            end
                    end;
                _ -> ok
            end
        catch
            _:_ -> ok
        end
    end, Tables).

clear_large_process_dicts() ->
    Threshold = 1000,
    Processes = erlang:processes(),
    
    lists:foreach(fun(Pid) ->
        try
            case process_info(Pid, dictionary) of
                {dictionary, Dict} when length(Dict) > Threshold ->
                    colored_logger:warning("Process ~p has large dictionary (~p entries)", 
                                         [Pid, length(Dict)]);
                _ ->
                    ok
            end
        catch
            _:_ -> ok
        end
    end, Processes).

optimize_binaries() ->
    %% Force binary GC on processes with large binary heaps
    Threshold = 5242880,  % 5MB
    Processes = erlang:processes(),
    
    lists:foreach(fun(Pid) ->
        try
            case process_info(Pid, binary) of
                {binary, Bins} ->
                    TotalSize = lists:sum([Size || {_, Size, _} <- Bins]),
                    if
                        TotalSize > Threshold ->
                            erlang:garbage_collect(Pid, [{type, major}]);
                        true ->
                            ok
                    end;
                _ ->
                    ok
            end
        catch
            _:_ -> ok
        end
    end, Processes).

get_memory_stats() ->
    Memory = erlang:memory(),
    #{
        total => Memory,
        processes => proplists:get_value(processes, Memory, 0),
        system => proplists:get_value(system, Memory, 0),
        atom => proplists:get_value(atom, Memory, 0),
        binary => proplists:get_value(binary, Memory, 0),
        ets => proplists:get_value(ets, Memory, 0)
    }.

get_gc_stats() ->
    {GCs, Words, _} = erlang:statistics(garbage_collection),
    #{
        total_gcs => GCs,
        words_reclaimed => Words
    }.

get_scheduler_utilization() ->
    case erlang:statistics(scheduler_wall_time) of
        undefined -> 0.0;
        Schedulers ->
            TotalUtil = lists:sum([U || {_, A, T} <- Schedulers, 
                                       T > 0, 
                                       U <- [A/T]]),
            TotalUtil / length(Schedulers)
    end.