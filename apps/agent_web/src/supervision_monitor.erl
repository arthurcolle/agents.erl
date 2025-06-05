-module(supervision_monitor).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MONITOR_INTERVAL, 5000). % Monitor every 5 seconds

%% Safe logging function that falls back to io:format if colored_logger is not available
safe_log(Message) ->
    try
        colored_logger:success(Message, [])
    catch
        error:undef ->
            io:format("~s~n", [Message]);
        _:_ ->
            io:format("~s~n", [Message])
    end.

safe_log(Level, Message) ->
    try
        case Level of
            warning -> colored_logger:warning(Message, []);
            error -> colored_logger:error(Message, []);
            info -> colored_logger:info(Message, []);
            success -> colored_logger:success(Message, []);
            _ -> colored_logger:info(Message, [])
        end
    catch
        error:undef ->
            io:format("[~p] ~s~n", [Level, Message]);
        _:_ ->
            io:format("[~p] ~s~n", [Level, Message])
    end.

safe_log(Level, Format, Args) ->
    try
        case Level of
            warning -> colored_logger:warning(Format, Args);
            error -> colored_logger:error(Format, Args);
            info -> colored_logger:info(Format, Args);
            success -> colored_logger:success(Format, Args);
            fire -> colored_logger:fire(bright, Format, Args);
            data -> colored_logger:data(processed, Format, Args);
            _ -> colored_logger:info(Format, Args)
        end
    catch
        error:undef ->
            io:format("[~p] " ++ Format ++ "~n", [Level | Args]);
        _:_ ->
            io:format("[~p] " ++ Format ++ "~n", [Level | Args])
    end.

-record(state, {
    supervised_pids = #{},
    last_check = 0,
    error_count = 0
}).

start_link() ->
    safe_log("[SUP_MON] Starting Supervision Monitor"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    safe_log("[SUP_MON] Supervision Monitor initialized"),
    
    %% Start monitoring timer
    erlang:send_after(?MONITOR_INTERVAL, self(), monitor_supervisors),
    
    %% Monitor key supervisors
    Supervisors = [
        agent_web_sup,
        agent_supervisor,
        openai_sup,
        agents_sup
    ],
    
    %% Start monitoring each supervisor
    MonitoredPids = lists:foldl(fun(SupName, Acc) ->
        case whereis(SupName) of
            undefined ->
                safe_log(warning, "[SUP_MON] Supervisor ~p not found", [SupName]),
                Acc;
            Pid ->
                MonitorRef = monitor(process, Pid),
                safe_log(info, "[SUP_MON] Monitoring supervisor ~p (~p)", [SupName, Pid]),
                Acc#{Pid => #{name => SupName, monitor_ref => MonitorRef, status => running}}
        end
    end, #{}, Supervisors),
    
    {ok, #state{
        supervised_pids = MonitoredPids,
        last_check = erlang:system_time(millisecond)
    }}.

handle_call(get_status, _From, State) ->
    Status = #{
        monitored_supervisors => maps:size(State#state.supervised_pids),
        last_check => State#state.last_check,
        error_count => State#state.error_count,
        supervisors => maps:fold(fun(Pid, Info, Acc) ->
            [#{pid => Pid, info => Info} | Acc]
        end, [], State#state.supervised_pids)
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(monitor_supervisors, State) ->
    safe_log(data, "[SUP_MON] Running supervisor health check", []),
    
    %% Check health of all monitored supervisors
    NewState = check_supervisor_health(State),
    
    %% Schedule next check
    erlang:send_after(?MONITOR_INTERVAL, self(), monitor_supervisors),
    
    {noreply, NewState#state{last_check = erlang:system_time(millisecond)}};

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    case maps:get(Pid, State#state.supervised_pids, undefined) of
        undefined ->
            safe_log(warning, "[SUP_MON] Unknown process died: ~p", [Pid]),
            {noreply, State};
        #{name := SupName} ->
            safe_log(fire, "[SUP_MON] SUPERVISOR DOWN: ~p (~p) - Reason: ~p", 
                              [SupName, Pid, Reason]),
            
            %% Remove from monitoring
            NewSupervised = maps:remove(Pid, State#state.supervised_pids),
            
            %% Try to restart supervision
            spawn(fun() -> attempt_supervisor_restart(SupName, Reason) end),
            
            %% Update error count
            NewErrorCount = State#state.error_count + 1,
            
            {noreply, State#state{
                supervised_pids = NewSupervised,
                error_count = NewErrorCount
            }}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    safe_log(warning, "[SUP_MON] Supervision Monitor terminating"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Check health of all supervised processes
check_supervisor_health(State) ->
    maps:fold(fun(Pid, Info, AccState) ->
        case process_info(Pid) of
            undefined ->
                safe_log(fire, "[SUP_MON] Supervisor ~p appears dead", 
                                  [maps:get(name, Info)]),
                AccState;
            ProcInfo ->
                %% Check memory usage
                Memory = proplists:get_value(memory, ProcInfo, 0),
                MessageQueueLen = proplists:get_value(message_queue_len, ProcInfo, 0),
                
                %% Check if supervisor is healthy
                HealthStatus = assess_supervisor_health(Memory, MessageQueueLen),
                
                case HealthStatus of
                    healthy ->
                        safe_log(data, "[SUP_MON] ~p healthy (mem: ~p, queue: ~p)", 
                                          [maps:get(name, Info), Memory, MessageQueueLen]);
                    warning ->
                        safe_log(warning, "[SUP_MON] ~p showing warning signs (mem: ~p, queue: ~p)", 
                                             [maps:get(name, Info), Memory, MessageQueueLen]);
                    critical ->
                        safe_log(fire, "[SUP_MON] ~p in critical state (mem: ~p, queue: ~p)", 
                                          [maps:get(name, Info), Memory, MessageQueueLen])
                end,
                
                AccState
        end
    end, State, State#state.supervised_pids).

%% Assess supervisor health based on metrics
assess_supervisor_health(Memory, MessageQueueLen) ->
    if
        Memory > 50000000 orelse MessageQueueLen > 1000 -> critical;  % 50MB or 1000 messages
        Memory > 20000000 orelse MessageQueueLen > 100 -> warning;   % 20MB or 100 messages
        true -> healthy
    end.

%% Attempt to restart a failed supervisor
attempt_supervisor_restart(SupName, Reason) ->
    safe_log(warning, "[SUP_MON] Attempting to restart supervisor ~p", [SupName]),
    
    timer:sleep(1000), % Wait a bit before restart
    
    %% Try to start the supervisor again
    case SupName of
        agent_web_sup ->
            case agent_web_sup:start_link() of
                {ok, Pid} ->
                    safe_log(success, "[SUP_MON] Successfully restarted ~p (~p)", [SupName, Pid]);
                {error, Error} ->
                    safe_log(fire, "[SUP_MON] Failed to restart ~p: ~p", [SupName, Error])
            end;
        agent_supervisor ->
            case agent_supervisor:start_link() of
                {ok, Pid} ->
                    safe_log(success, "[SUP_MON] Successfully restarted ~p (~p)", [SupName, Pid]);
                {error, Error} ->
                    safe_log(fire, "[SUP_MON] Failed to restart ~p: ~p", [SupName, Error])
            end;
        _ ->
            safe_log(warning, "[SUP_MON] Don't know how to restart ~p", [SupName])
    end.