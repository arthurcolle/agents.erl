%%%-------------------------------------------------------------------
%%% @doc Supervised, idempotent post-start initialization and health
%%% monitoring for agent_web.
%%%
%%% This replaces the unlinked startup and monitoring processes that
%%% previously escaped the application supervision tree.
%%% @end
%%%-------------------------------------------------------------------
-module(agent_web_bootstrap).
-behaviour(gen_server).

-export([start_link/0, status/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(DEFAULT_HEALTH_INTERVAL, 30000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

status() ->
    gen_server:call(?MODULE, status, 5000).

init([]) ->
    Delay = non_neg_integer_env(bootstrap_delay_ms, 1000),
    Timer = erlang:send_after(Delay, self(), bootstrap),
    {ok, #{status => pending, failures => [], bootstrap_timer => Timer,
           health_timer => undefined}}.

handle_call(status, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(bootstrap, #{status := pending} = State) ->
    NewState = case application:get_env(agent_web, bootstrap_enabled, true) of
        true ->
            Failures = run_initializers(),
            HealthTimer = schedule_health_check(),
            State#{
                status => bootstrap_status(Failures),
                failures => Failures,
                bootstrap_timer => undefined,
                health_timer => HealthTimer,
                completed_at => erlang:system_time(millisecond)
            };
        false ->
            State#{status => disabled, bootstrap_timer => undefined}
    end,
    {noreply, NewState};
handle_info(bootstrap, State) ->
    %% A timer or an operator message may deliver bootstrap more than once;
    %% initialization is intentionally one-shot per process incarnation.
    {noreply, State};
handle_info(health_check, State) ->
    run_health_check(),
    {noreply, State#{health_timer => schedule_health_check()}};
handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    cancel_timer(maps:get(bootstrap_timer, State, undefined)),
    cancel_timer(maps:get(health_timer, State, undefined)),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

run_initializers() ->
    Initializers = [
        {default_agents, fun agent_initializer:init_default_agents/0},
        {conversation_storage, fun conversation_handler:init_storage/0},
        {timeline_storage, fun timeline_handler:init_storage/0},
        {mcp_server_config, fun initialize_mcp/0},
        {oauth, fun oauth_handler:init_oauth_system/0},
        {training_data, fun training_data_generator:schedule_generation/0}
    ],
    lists:reverse(lists:foldl(fun run_initializer/2, [], Initializers)).

run_initializer({Name, InitFun}, Failures) ->
    try
        case InitFun() of
            {error, InitReason} ->
                logger:error("agent_web initializer ~p returned an error: ~p", [Name, InitReason]),
                [#{component => Name, class => error, reason => InitReason} | Failures];
            _Result ->
                logger:info("agent_web initializer completed: ~p", [Name]),
                Failures
        end
    catch
        Class:CaughtReason:Stacktrace ->
            logger:error(
                "agent_web initializer ~p failed: ~p:~p~n~p",
                [Name, Class, CaughtReason, Stacktrace]
            ),
            [#{component => Name, class => Class, reason => CaughtReason} | Failures]
    end.

initialize_mcp() ->
    _ = mcp_server_config:init(),
    mcp_env_checker:check_and_fix_servers().

bootstrap_status([]) -> ready;
bootstrap_status(_Failures) -> degraded.

schedule_health_check() ->
    case application:get_env(agent_web, health_monitor_enabled, true) of
        true ->
            Interval = non_neg_integer_env(
                health_monitor_interval_ms,
                ?DEFAULT_HEALTH_INTERVAL
            ),
            erlang:send_after(Interval, self(), health_check);
        false ->
            undefined
    end.

non_neg_integer_env(Key, Default) ->
    case application:get_env(agent_web, Key, Default) of
        Value when is_integer(Value), Value >= 0 -> Value;
        _ -> Default
    end.

cancel_timer(Timer) when is_reference(Timer) ->
    _ = erlang:cancel_timer(Timer),
    ok;
cancel_timer(_) -> ok.

run_health_check() ->
    try
        SystemStatus = system_health_handler:get_system_status(),
        maybe_broadcast_health(SystemStatus)
    catch
        Class:Reason ->
            logger:error(
                "agent_web health check failed: ~p:~p",
                [Class, Reason]
            ),
            safe_broadcast(#{
                type => <<"system_error">>,
                severity => <<"high">>,
                message => <<"Health check failed">>,
                details => #{class => Class, error => Reason}
            })
    end.

maybe_broadcast_health(SystemStatus) ->
    case maps:get(overall_health, SystemStatus, <<"unknown">>) of
        <<"degraded">> ->
            safe_broadcast(#{
                type => <<"system_health">>,
                status => <<"degraded">>,
                data => SystemStatus
            });
        _ ->
            ok
    end,
    Processes = maps:get(processes, SystemStatus, []),
    Down = [
        Process
        || Process <- Processes,
           maps:get(status, Process, <<"unknown">>) =:= <<"down">>
    ],
    case Down of
        [] ->
            ok;
        _ ->
            safe_broadcast(#{
                type => <<"system_error">>,
                severity => <<"critical">>,
                message => <<"Critical processes are down">>,
                details => Down
            })
    end.

safe_broadcast(Event) ->
    try agent_ws_handler:broadcast(Event)
    catch
        _:_ -> ok
    end.
