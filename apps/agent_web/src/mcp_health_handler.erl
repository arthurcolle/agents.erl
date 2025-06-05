%% @doc MCP Health Check Handler
%%
%% Provides health check endpoints for MCP Streamable HTTP transport
-module(mcp_health_handler).

-include_lib("kernel/include/logger.hrl").

%% Cowboy callbacks
-export([
    init/2
]).

%% Health check functions
-export([
    check_transport_health/0,
    check_session_manager_health/0,
    check_message_buffer_health/0
]).

%%% ============================================================================
%%% Cowboy Handler Implementation
%%% ============================================================================

%% @doc Initialize health check handler
init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    
    case Method of
        <<"GET">> ->
            handle_health_check(Req0, Opts);
        _ ->
            Req1 = cowboy_req:reply(405, #{
                <<"allow">> => <<"GET">>,
                <<"content-type">> => <<"text/plain">>
            }, <<"Method Not Allowed">>, Req0),
            {ok, Req1, Opts}
    end.

%% @doc Handle health check request
-spec handle_health_check(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
handle_health_check(Req0, Opts) ->
    StartTime = erlang:system_time(millisecond),
    
    % Perform comprehensive health checks
    HealthChecks = [
        {transport, check_transport_health()},
        {session_manager, check_session_manager_health()},
        {message_buffer, check_message_buffer_health()},
        {system, check_system_health()}
    ],
    
    EndTime = erlang:system_time(millisecond),
    CheckDuration = EndTime - StartTime,
    
    % Determine overall health status
    {OverallStatus, StatusCode} = determine_overall_status(HealthChecks),
    
    % Build response
    Response = #{
        <<"status">> => OverallStatus,
        <<"timestamp">> => iso8601_timestamp(),
        <<"version">> => <<"2025-03-26">>,
        <<"check_duration_ms">> => CheckDuration,
        <<"checks">> => format_health_checks(HealthChecks),
        <<"system_info">> => get_system_info()
    },
    
    ResponseBody = jsx:encode(Response),
    
    Req1 = cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"application/json">>,
        <<"cache-control">> => <<"no-cache">>
    }, ResponseBody, Req0),
    
    {ok, Req1, Opts}.

%%% ============================================================================
%%% Health Check Functions
%%% ============================================================================

%% @doc Check transport health
-spec check_transport_health() -> {ok, map()} | {error, map()}.
check_transport_health() ->
    try
        case whereis(mcp_transport_streamable_http) of
            undefined ->
                {error, #{
                    reason => <<"Transport process not running">>,
                    details => <<"MCP transport server is not started">>
                }};
            Pid when is_pid(Pid) ->
                case is_process_alive(Pid) of
                    true ->
                        {ok, #{
                            status => <<"healthy">>,
                            pid => list_to_binary(pid_to_list(Pid)),
                            uptime_ms => get_process_uptime(Pid)
                        }};
                    false ->
                        {error, #{
                            reason => <<"Transport process dead">>,
                            pid => list_to_binary(pid_to_list(Pid))
                        }}
                end
        end
    catch
        error:Reason ->
            {error, #{
                reason => <<"Health check failed">>,
                error => format_error(Reason)
            }}
    end.

%% @doc Check session manager health
-spec check_session_manager_health() -> {ok, map()} | {error, map()}.
check_session_manager_health() ->
    try
        case whereis(mcp_http_session_manager) of
            undefined ->
                {error, #{
                    reason => <<"Session manager not running">>,
                    details => <<"MCP session manager is not started">>
                }};
            Pid when is_pid(Pid) ->
                case is_process_alive(Pid) of
                    true ->
                        Sessions = mcp_http_session_manager:list_sessions(),
                        {ok, #{
                            status => <<"healthy">>,
                            pid => list_to_binary(pid_to_list(Pid)),
                            active_sessions => length(Sessions),
                            uptime_ms => get_process_uptime(Pid)
                        }};
                    false ->
                        {error, #{
                            reason => <<"Session manager process dead">>,
                            pid => list_to_binary(pid_to_list(Pid))
                        }}
                end
        end
    catch
        error:Reason ->
            {error, #{
                reason => <<"Session manager health check failed">>,
                error => format_error(Reason)
            }}
    end.

%% @doc Check message buffer health
-spec check_message_buffer_health() -> {ok, map()} | {error, map()}.
check_message_buffer_health() ->
    try
        case whereis(mcp_http_message_buffer) of
            undefined ->
                {error, #{
                    reason => <<"Message buffer not running">>,
                    details => <<"MCP message buffer is not started">>
                }};
            Pid when is_pid(Pid) ->
                case is_process_alive(Pid) of
                    true ->
                        {ok, #{
                            status => <<"healthy">>,
                            pid => list_to_binary(pid_to_list(Pid)),
                            uptime_ms => get_process_uptime(Pid)
                        }};
                    false ->
                        {error, #{
                            reason => <<"Message buffer process dead">>,
                            pid => list_to_binary(pid_to_list(Pid))
                        }}
                end
        end
    catch
        error:Reason ->
            {error, #{
                reason => <<"Message buffer health check failed">>,
                error => format_error(Reason)
            }}
    end.

%% @doc Check system health
-spec check_system_health() -> {ok, map()} | {error, map()}.
check_system_health() ->
    try
        MemoryInfo = erlang:memory(),
        ProcessCount = erlang:system_info(process_count),
        ProcessLimit = erlang:system_info(process_limit),
        
        % Check memory usage
        TotalMemory = proplists:get_value(total, MemoryInfo, 0),
        ProcessMemory = proplists:get_value(processes, MemoryInfo, 0),
        
        % Check process count
        ProcessUsage = ProcessCount / ProcessLimit,
        
        Warnings = [],
        
        % Add warnings if necessary
        WarningsWithMemory = case TotalMemory > 1024*1024*1024 of % 1GB
            true -> [<<"High memory usage">> | Warnings];
            false -> Warnings
        end,
        
        WarningsWithProcess = case ProcessUsage > 0.8 of
            true -> [<<"High process count">> | WarningsWithMemory];
            false -> WarningsWithMemory
        end,
        
        Status = case WarningsWithProcess of
            [] -> <<"healthy">>;
            _ -> <<"warning">>
        end,
        
        {ok, #{
            status => Status,
            memory_mb => TotalMemory div (1024*1024),
            process_memory_mb => ProcessMemory div (1024*1024),
            process_count => ProcessCount,
            process_limit => ProcessLimit,
            process_usage => ProcessUsage,
            warnings => WarningsWithProcess
        }}
    catch
        error:Reason ->
            {error, #{
                reason => <<"System health check failed">>,
                error => format_error(Reason)
            }}
    end.

%%% ============================================================================
%%% Utility Functions
%%% ============================================================================

%% @doc Determine overall health status
-spec determine_overall_status([{atom(), {ok, map()} | {error, map()}}]) -> 
    {binary(), integer()}.
determine_overall_status(HealthChecks) ->
    HasErrors = lists:any(fun({_Name, {error, _}}) -> true; 
                             ({_Name, {ok, _}}) -> false 
                          end, HealthChecks),
    
    HasWarnings = lists:any(fun
        ({_Name, {ok, #{status := <<"warning">>}}}) -> true;
        (_) -> false
    end, HealthChecks),
    
    case {HasErrors, HasWarnings} of
        {true, _} -> {<<"unhealthy">>, 503};
        {false, true} -> {<<"warning">>, 200};
        {false, false} -> {<<"healthy">>, 200}
    end.

%% @doc Format health checks for response
-spec format_health_checks([{atom(), {ok, map()} | {error, map()}}]) -> map().
format_health_checks(HealthChecks) ->
    maps:from_list([
        {atom_to_binary(Name), format_check_result(Result)}
        || {Name, Result} <- HealthChecks
    ]).

%% @doc Format individual check result
-spec format_check_result({ok, map()} | {error, map()}) -> map().
format_check_result({ok, Data}) ->
    Data#{result => <<"ok">>};
format_check_result({error, Data}) ->
    Data#{result => <<"error">>}.

%% @doc Get system information
-spec get_system_info() -> map().
get_system_info() ->
    #{
        node => atom_to_binary(node()),
        otp_release => list_to_binary(erlang:system_info(otp_release)),
        erts_version => list_to_binary(erlang:system_info(version)),
        system_architecture => list_to_binary(erlang:system_info(system_architecture)),
        cpu_topology => format_cpu_topology(erlang:system_info(cpu_topology)),
        schedulers => erlang:system_info(schedulers),
        schedulers_online => erlang:system_info(schedulers_online)
    }.

%% @doc Get process uptime in milliseconds
-spec get_process_uptime(pid()) -> non_neg_integer().
get_process_uptime(Pid) ->
    case process_info(Pid, [registered_name, current_function]) of
        undefined -> 0;
        _ ->
            Now = erlang:system_time(millisecond),
            {_Date, _Time} = erlang:system_time(),
            % This is a simplified calculation
            % In a real implementation, you'd track process start times
            Now rem 86400000 % Placeholder calculation
    end.

%% @doc Format CPU topology
-spec format_cpu_topology(term()) -> binary().
format_cpu_topology(undefined) ->
    <<"unknown">>;
format_cpu_topology(Topology) ->
    iolist_to_binary(io_lib:format("~p", [Topology])).

%% @doc Format error for response
-spec format_error(term()) -> binary().
format_error(Error) ->
    iolist_to_binary(io_lib:format("~p", [Error])).

%% @doc Generate ISO8601 timestamp
-spec iso8601_timestamp() -> binary().
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
                                   [Year, Month, Day, Hour, Minute, Second])).