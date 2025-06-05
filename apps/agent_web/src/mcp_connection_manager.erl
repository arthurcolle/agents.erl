-module(mcp_connection_manager).

%% Colorful logging macros
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format(Msg, Args))).
-define(LOG_WARNING(Msg), colored_logger:alarm(medium, Msg)).
-define(LOG_WARNING(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).
-define(LOG_SUCCESS(Msg), colored_logger:complete(success, Msg)).
-define(LOG_SUCCESS(Msg, Args), colored_logger:complete(success, io_lib:format(Msg, Args))).
-define(LOG_DEBUG(Msg), colored_logger:development(debugging, Msg)).
-define(LOG_DEBUG(Msg, Args), colored_logger:development(debugging, io_lib:format(Msg, Args))).

-behaviour(gen_server).

%% API
-export([start_link/0, connect_server/1, disconnect_server/1, 
         get_connection/1, list_connections/0, test_connection/1,
         get_connection_stats/0, force_reconnect/1, set_retry_config/2,
         get_health_status/1, cleanup_stale_connections/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
    connections = #{}, % ServerId -> {Pid, Status, Config, ConnectionInfo}
    retry_state = #{}, % ServerId -> {Attempts, LastAttempt, BackoffTime}
    health_checks = #{}, % ServerId -> {LastCheck, HealthStatus}
    connection_pool = #{} % ConnectionKey -> [AvailablePids]
}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    ?LOG_INFO("[MCP_CONN] Starting advanced MCP connection manager"),
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} ->
            ?LOG_INFO("[MCP_CONN] Advanced connection manager started successfully with PID ~p", [Pid]),
            {ok, Pid};
        {error, Reason} ->
            ?LOG_INFO("[ERROR] Failed to start MCP connection manager: ~p", [Reason]),
            {error, Reason}
    end.

connect_server(ServerId) ->
    gen_server:call(?MODULE, {connect_server, ServerId}).

disconnect_server(ServerId) ->
    gen_server:call(?MODULE, {disconnect_server, ServerId}).

get_connection(ServerId) ->
    gen_server:call(?MODULE, {get_connection, ServerId}).

list_connections() ->
    gen_server:call(?MODULE, list_connections).

test_connection(ServerId) ->
    gen_server:call(?MODULE, {test_connection, ServerId}).

get_connection_stats() ->
    gen_server:call(?MODULE, get_connection_stats).

force_reconnect(ServerId) ->
    gen_server:call(?MODULE, {force_reconnect, ServerId}).

set_retry_config(ServerId, Config) ->
    gen_server:call(?MODULE, {set_retry_config, ServerId, Config}).

get_health_status(ServerId) ->
    gen_server:call(?MODULE, {get_health_status, ServerId}).

cleanup_stale_connections() ->
    gen_server:call(?MODULE, cleanup_stale_connections).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    % Start health check timer
    erlang:send_after(30000, self(), health_check_tick),
    % Start cleanup timer
    erlang:send_after(300000, self(), cleanup_tick), % 5 minutes
    {ok, #state{}}.

handle_call({connect_server, ServerId}, _From, State) ->
    case should_attempt_connection(ServerId, State) of
        {ok, proceed} ->
            case mcp_registry:get_server(ServerId) of
                {ok, ServerConfig} ->
                    case maps:find(ServerId, State#state.connections) of
                        {ok, {_Pid, connected, _Config, _Info}} ->
                            {reply, {error, already_connected}, State};
                        {ok, {Pid, _Status, _Config, _Info}} ->
                            % Clean up existing connection
                            catch mcp_client_v2:stop(Pid),
                            start_new_connection_with_retry(ServerId, ServerConfig, State);
                        error ->
                            start_new_connection_with_retry(ServerId, ServerConfig, State)
                    end;
                {error, not_found} ->
                    {reply, {error, server_not_found}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({disconnect_server, ServerId}, _From, State) ->
    case maps:find(ServerId, State#state.connections) of
        {ok, {Pid, _Status, _Config, _Info}} ->
            catch mcp_client_v2:stop(Pid),
            NewConnections = maps:remove(ServerId, State#state.connections),
            NewRetryState = maps:remove(ServerId, State#state.retry_state),
            NewHealthChecks = maps:remove(ServerId, State#state.health_checks),
            gen_server:cast(mcp_registry, {update_server_status, ServerId, disconnected}),
            {reply, ok, State#state{
                connections = NewConnections,
                retry_state = NewRetryState,
                health_checks = NewHealthChecks
            }};
        error ->
            {reply, {error, not_connected}, State}
    end;

handle_call({get_connection, ServerId}, _From, State) ->
    case maps:find(ServerId, State#state.connections) of
        {ok, {Pid, Status, _Config, Info}} ->
            % Update last access time
            NewInfo = Info#{last_access => erlang:system_time(second)},
            NewConnections = maps:put(ServerId, {Pid, Status, _Config, NewInfo}, State#state.connections),
            {reply, {ok, {Pid, Status}}, State#state{connections = NewConnections}};
        error ->
            {reply, {error, not_connected}, State}
    end;

handle_call(list_connections, _From, State) ->
    Connections = maps:fold(fun(ServerId, {Pid, Status, Config, Info}, Acc) ->
        RetryInfo = maps:get(ServerId, State#state.retry_state, #{}),
        HealthInfo = maps:get(ServerId, State#state.health_checks, #{}),
        [#{
            server_id => ServerId,
            pid => Pid,
            status => Status,
            config => Config,
            connection_info => Info,
            retry_info => RetryInfo,
            health_info => HealthInfo
        } | Acc]
    end, [], State#state.connections),
    {reply, Connections, State};

handle_call({test_connection, ServerId}, _From, State) ->
    case maps:find(ServerId, State#state.connections) of
        {ok, {Pid, connected, _Config, Info}} ->
            StartTime = erlang:system_time(millisecond),
            case mcp_client_v2:ping(Pid) of
                {ok, _Result} ->
                    Latency = erlang:system_time(millisecond) - StartTime,
                    % Update health info
                    HealthInfo = #{
                        last_ping => erlang:system_time(second),
                        latency => Latency,
                        status => healthy
                    },
                    NewHealthChecks = maps:put(ServerId, HealthInfo, State#state.health_checks),
                    {reply, {ok, #{status => <<"reachable">>, latency => Latency}}, 
                     State#state{health_checks = NewHealthChecks}};
                {error, Reason} ->
                    % Update health info for failure
                    HealthInfo = #{
                        last_ping => erlang:system_time(second),
                        status => unhealthy,
                        error => Reason
                    },
                    NewHealthChecks = maps:put(ServerId, HealthInfo, State#state.health_checks),
                    {reply, {error, Reason}, State#state{health_checks = NewHealthChecks}}
            end;
        {ok, {_Pid, Status, _Config, _Info}} ->
            {reply, {error, {not_connected, Status}}, State};
        error ->
            {reply, {error, not_connected}, State}
    end;

handle_call(get_connection_stats, _From, State) ->
    Stats = #{
        total_connections => maps:size(State#state.connections),
        connected_count => count_by_status(connected, State#state.connections),
        disconnected_count => count_by_status(disconnected, State#state.connections),
        error_count => count_by_status(error, State#state.connections),
        retry_backlog => maps:size(State#state.retry_state),
        health_monitored => maps:size(State#state.health_checks)
    },
    {reply, Stats, State};

handle_call({force_reconnect, ServerId}, _From, State) ->
    % Reset retry state and force reconnection
    NewRetryState = maps:remove(ServerId, State#state.retry_state),
    case maps:find(ServerId, State#state.connections) of
        {ok, {Pid, _Status, Config, _Info}} ->
            catch mcp_client_v2:stop(Pid),
            start_new_connection_with_retry(ServerId, Config, 
                State#state{retry_state = NewRetryState});
        error ->
            {reply, {error, not_connected}, State}
    end;

handle_call({set_retry_config, ServerId, RetryConfig}, _From, State) ->
    CurrentRetry = maps:get(ServerId, State#state.retry_state, #{}),
    NewRetryConfig = maps:merge(CurrentRetry, RetryConfig),
    NewRetryState = maps:put(ServerId, NewRetryConfig, State#state.retry_state),
    {reply, ok, State#state{retry_state = NewRetryState}};

handle_call({get_health_status, ServerId}, _From, State) ->
    HealthInfo = maps:get(ServerId, State#state.health_checks, #{}),
    {reply, HealthInfo, State};

handle_call(cleanup_stale_connections, _From, State) ->
    {CleanedConnections, CleanedCount} = cleanup_stale_connections_internal(State#state.connections),
    {reply, #{cleaned => CleanedCount}, State#state{connections = CleanedConnections}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    % Handle connection process death
    case find_connection_by_pid(Pid, State#state.connections) of
        {ok, ServerId} ->
            error_logger:warning_msg("MCP connection ~s died: ~p~n", [ServerId, Reason]),
            NewConnections = maps:remove(ServerId, State#state.connections),
            gen_server:cast(mcp_registry, {update_server_status, ServerId, disconnected}),
            % Log disconnection event
            catch mcp_advanced_logger:log_connection_event(ServerId, disconnected, #{
                reason => Reason,
                unexpected => true
            }),
            % Schedule retry if enabled
            schedule_retry(ServerId, State),
            {noreply, State#state{connections = NewConnections}};
        error ->
            {noreply, State}
    end;

handle_info(health_check_tick, State) ->
    % Perform health checks on all connected servers
    NewState = perform_health_checks(State),
    % Schedule next health check
    erlang:send_after(30000, self(), health_check_tick),
    {noreply, NewState};

handle_info(cleanup_tick, State) ->
    % Clean up stale connections and retry state
    {CleanedConnections, _} = cleanup_stale_connections_internal(State#state.connections),
    CleanedRetryState = cleanup_stale_retry_state(State#state.retry_state),
    % Schedule next cleanup
    erlang:send_after(300000, self(), cleanup_tick),
    {noreply, State#state{
        connections = CleanedConnections,
        retry_state = CleanedRetryState
    }};

handle_info({retry_connection, ServerId}, State) ->
    % Handle scheduled retry
    case should_retry_connection(ServerId, State) of
        true ->
            attempt_reconnection(ServerId, State);
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Clean up all connections
    maps:fold(fun(_ServerId, {Pid, _Status, _Config, _Info}, _Acc) ->
        catch mcp_client_v2:stop(Pid)
    end, ok, State#state.connections),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

start_new_connection(ServerId, ServerConfig, State) ->
    StartTime = erlang:system_time(millisecond),
    
    case mcp_client_v2:start_link(ServerId, ServerConfig) of
        {ok, Pid} ->
            link(Pid),
            % Attempt to connect
            case mcp_client_v2:connect(Pid) of
                {ok, _ConnInfo} ->
                    ConnTime = erlang:system_time(millisecond) - StartTime,
                    ConnInfo = #{
                        connected_at => erlang:system_time(second),
                        last_access => erlang:system_time(second),
                        connection_attempts => 1,
                        connection_time => ConnTime
                    },
                    NewConnections = maps:put(ServerId, {Pid, connected, ServerConfig, ConnInfo}, 
                                            State#state.connections),
                    gen_server:cast(mcp_registry, {update_server_status, ServerId, connected}),
                    % Log successful connection
                    catch mcp_advanced_logger:log_connection_event(ServerId, connected, #{
                        url => maps:get(url, ServerConfig, undefined),
                        connection_time => ConnTime
                    }),
                    catch mcp_advanced_logger:log_performance_metric(connection_establishment, ConnTime, #{
                        server_id => ServerId,
                        url => maps:get(url, ServerConfig, undefined)
                    }),
                    {reply, {ok, Pid}, State#state{connections = NewConnections}};
                {error, Reason} ->
                    ConnTime = erlang:system_time(millisecond) - StartTime,
                    catch mcp_client_v2:stop(Pid),
                    gen_server:cast(mcp_registry, {update_server_status, ServerId, error}),
                    % Log failed connection
                    catch mcp_advanced_logger:log_connection_event(ServerId, connection_failed, #{
                        reason => Reason,
                        url => maps:get(url, ServerConfig, undefined),
                        connection_time => ConnTime
                    }),
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            ConnTime = erlang:system_time(millisecond) - StartTime,
            % Log startup failure
            catch mcp_advanced_logger:log_connection_event(ServerId, startup_failed, #{
                reason => Reason,
                url => maps:get(url, ServerConfig, undefined),
                connection_time => ConnTime
            }),
            {reply, {error, Reason}, State}
    end.

start_new_connection_with_retry(ServerId, ServerConfig, State) ->
    case start_new_connection(ServerId, ServerConfig, State) of
        {reply, {ok, Pid}, NewState} ->
            % Success - clear any retry state
            CleanRetryState = maps:remove(ServerId, NewState#state.retry_state),
            {reply, {ok, Pid}, NewState#state{retry_state = CleanRetryState}};
        {reply, {error, Reason}, NewState} ->
            % Failed - update retry state
            RetryInfo = calculate_retry_backoff(ServerId, NewState#state.retry_state),
            UpdatedRetryState = maps:put(ServerId, RetryInfo, NewState#state.retry_state),
            % Schedule retry
            BackoffTime = maps:get(backoff_time, RetryInfo, 5000),
            erlang:send_after(BackoffTime, self(), {retry_connection, ServerId}),
            {reply, {error, Reason}, NewState#state{retry_state = UpdatedRetryState}}
    end.

should_attempt_connection(ServerId, State) ->
    case maps:find(ServerId, State#state.retry_state) of
        {ok, #{max_attempts := MaxAttempts, attempts := Attempts}} when Attempts >= MaxAttempts ->
            {error, max_retries_exceeded};
        {ok, #{last_attempt := LastAttempt, backoff_time := BackoffTime}} ->
            Now = erlang:system_time(second),
            if Now - LastAttempt < BackoffTime div 1000 ->
                {error, backoff_active};
            true ->
                {ok, proceed}
            end;
        error ->
            {ok, proceed}
    end.

calculate_retry_backoff(ServerId, RetryState) ->
    case maps:find(ServerId, RetryState) of
        {ok, CurrentRetry} ->
            Attempts = maps:get(attempts, CurrentRetry, 0) + 1,
            BaseBackoff = maps:get(base_backoff, CurrentRetry, 5000),
            MaxBackoff = maps:get(max_backoff, CurrentRetry, 300000), % 5 minutes
            BackoffTime = min(BaseBackoff * round(math:pow(2, Attempts)), MaxBackoff),
            CurrentRetry#{
                attempts => Attempts,
                last_attempt => erlang:system_time(second),
                backoff_time => BackoffTime
            };
        error ->
            #{
                attempts => 1,
                last_attempt => erlang:system_time(second),
                base_backoff => 5000,
                max_backoff => 300000,
                max_attempts => 10,
                backoff_time => 5000
            }
    end.

schedule_retry(ServerId, State) ->
    case maps:find(ServerId, State#state.retry_state) of
        {ok, RetryInfo} ->
            MaxAttempts = maps:get(max_attempts, RetryInfo, 10),
            Attempts = maps:get(attempts, RetryInfo, 0),
            if Attempts < MaxAttempts ->
                BackoffTime = calculate_retry_backoff(ServerId, State#state.retry_state),
                NewBackoffTime = maps:get(backoff_time, BackoffTime, 5000),
                erlang:send_after(NewBackoffTime, self(), {retry_connection, ServerId});
            true ->
                ok
            end;
        error ->
            % First retry - use default settings
            erlang:send_after(5000, self(), {retry_connection, ServerId})
    end.

should_retry_connection(ServerId, State) ->
    case maps:find(ServerId, State#state.retry_state) of
        {ok, #{max_attempts := MaxAttempts, attempts := Attempts}} ->
            Attempts < MaxAttempts;
        error ->
            true
    end.

attempt_reconnection(ServerId, State) ->
    case mcp_registry:get_server(ServerId) of
        {ok, ServerConfig} ->
            case start_new_connection_with_retry(ServerId, ServerConfig, State) of
                {reply, {ok, _Pid}, NewState} ->
                    error_logger:info_msg("Successfully reconnected to MCP server ~s~n", [ServerId]),
                    {noreply, NewState};
                {reply, {error, _Reason}, NewState} ->
                    {noreply, NewState}
            end;
        {error, _} ->
            {noreply, State}
    end.

perform_health_checks(State) ->
    maps:fold(fun(ServerId, {Pid, connected, _Config, _Info}, AccState) ->
        case mcp_client_v2:ping(Pid) of
            {ok, _} ->
                HealthInfo = #{
                    last_check => erlang:system_time(second),
                    status => healthy
                },
                NewHealthChecks = maps:put(ServerId, HealthInfo, AccState#state.health_checks),
                AccState#state{health_checks = NewHealthChecks};
            {error, Reason} ->
                HealthInfo = #{
                    last_check => erlang:system_time(second),
                    status => unhealthy,
                    error => Reason
                },
                NewHealthChecks = maps:put(ServerId, HealthInfo, AccState#state.health_checks),
                AccState#state{health_checks = NewHealthChecks}
        end;
    (_, _, AccState) ->
        AccState
    end, State, State#state.connections).

cleanup_stale_connections_internal(Connections) ->
    Now = erlang:system_time(second),
    StaleThreshold = 3600, % 1 hour
    
    {NewConnections, CleanedCount} = maps:fold(fun(ServerId, {Pid, Status, Config, Info}, {Acc, Count}) ->
        LastAccess = maps:get(last_access, Info, Now),
        if Now - LastAccess > StaleThreshold andalso Status =/= connected ->
            catch mcp_client_v2:stop(Pid),
            {Acc, Count + 1};
        true ->
            {maps:put(ServerId, {Pid, Status, Config, Info}, Acc), Count}
        end
    end, {#{}, 0}, Connections),
    
    {NewConnections, CleanedCount}.

cleanup_stale_retry_state(RetryState) ->
    Now = erlang:system_time(second),
    StaleThreshold = 7200, % 2 hours
    
    maps:filter(fun(_ServerId, RetryInfo) ->
        LastAttempt = maps:get(last_attempt, RetryInfo, Now),
        Now - LastAttempt < StaleThreshold
    end, RetryState).

count_by_status(TargetStatus, Connections) ->
    maps:fold(fun(_ServerId, {_Pid, Status, _Config, _Info}, Count) ->
        case Status of
            TargetStatus -> Count + 1;
            _ -> Count
        end
    end, 0, Connections).

find_connection_by_pid(Pid, Connections) ->
    maps:fold(fun(ServerId, {ConnPid, _Status, _Config, _Info}, Acc) ->
        case ConnPid of
            Pid -> {ok, ServerId};
            _ -> Acc
        end
    end, error, Connections).