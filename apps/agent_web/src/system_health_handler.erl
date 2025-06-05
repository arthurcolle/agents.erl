-module(system_health_handler).

-export([init/2, get_system_status/0]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    {ok, handle_request(Method, Req0), State}.

handle_request(<<"GET">>, Req) ->
    Path = cowboy_req:path(Req),
    case Path of
        <<"/api/system/metrics">> ->
            % Return simplified metrics for dashboard
            Metrics = get_simple_metrics(),
            ResponseBody = jsx:encode(Metrics),
            cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>,
                <<"access-control-allow-origin">> => <<"*">>,
                <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
                <<"access-control-allow-headers">> => <<"content-type">>
            }, ResponseBody, Req);
        _ ->
            % Return full system status
            SystemStatus = get_system_status(),
            ResponseBody = jsx:encode(SystemStatus),
            cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/json">>,
                <<"access-control-allow-origin">> => <<"*">>,
                <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
                <<"access-control-allow-headers">> => <<"content-type">>
            }, ResponseBody, Req)
    end;

handle_request(<<"OPTIONS">>, Req) ->
    cowboy_req:reply(200, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS">>,
        <<"access-control-allow-headers">> => <<"content-type">>
    }, Req);

handle_request(_, Req) ->
    cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req).

get_simple_metrics() ->
    % Get basic metrics for dashboard refresh
    MemoryInfo = erlang:memory(),
    TotalMem = proplists:get_value(total, MemoryInfo),
    ProcessMem = proplists:get_value(processes, MemoryInfo),
    
    % Calculate simple CPU usage approximation based on process count and reduction rate
    ProcessCount = erlang:system_info(process_count),
    MaxProcesses = erlang:system_info(process_limit),
    CpuUsage = min(90, round((ProcessCount / MaxProcesses) * 100)),
    
    % Calculate memory usage percentage
    MemoryUsage = round((ProcessMem / TotalMem) * 100),
    
    #{
        <<"cpuUsage">> => CpuUsage,
        <<"memoryUsage">> => MemoryUsage,
        <<"processCount">> => ProcessCount,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

get_system_status() ->
    % Check critical processes
    CriticalProcesses = [mcp_registry, mcp_connection_manager, mcp_manager, 
                        mcp_advanced_config, oauth_manager, mcp_monitor],
    ProcessStatuses = lists:map(fun(Process) ->
        case whereis(Process) of
            undefined ->
                #{process => atom_to_binary(Process), status => <<"down">>, pid => null};
            Pid when is_pid(Pid) ->
                #{process => atom_to_binary(Process), status => <<"running">>, pid => list_to_binary(pid_to_list(Pid))}
        end
    end, CriticalProcesses),
    
    % Get memory info
    MemoryInfo = erlang:memory(),
    TotalMem = proplists:get_value(total, MemoryInfo),
    ProcessMem = proplists:get_value(processes, MemoryInfo),
    
    % Check web server connectivity
    Port = application:get_env(agent_web, port, 8080),
    WebServerStatus = case gen_tcp:connect("localhost", Port, [], 1000) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            <<"running">>;
        {error, _} ->
            <<"error">>
    end,
    
    % Get MCP server status
    McpServerStatus = try
        case mcp_registry:list_servers() of
            {ok, Servers} ->
                lists:map(fun(Server) ->
                    #{
                        id => maps:get(id, Server, <<"unknown">>),
                        name => maps:get(name, Server, <<"unknown">>),
                        status => maps:get(status, Server, <<"unknown">>),
                        url => maps:get(url, Server, <<"unknown">>)
                    }
                end, Servers);
            {error, Reason} ->
                [#{error => <<"Failed to get MCP servers">>, reason => list_to_binary(io_lib:format("~p", [Reason]))}]
        end
    catch
        Class:Error ->
            [#{error => <<"Exception getting MCP servers">>, reason => list_to_binary(io_lib:format("~p:~p", [Class, Error]))}]
    end,
    
    % Get advanced monitoring metrics
    MonitoringMetrics = try
        mcp_monitor:get_system_health()
    catch
        _:_ -> #{status => <<"unavailable">>}
    end,
    
    % Get active alerts
    ActiveAlerts = try
        mcp_monitor:get_alerts()
    catch
        _:_ -> []
    end,
    
    % Overall system health
    ProcessesHealthy = lists:all(fun(#{status := Status}) -> Status =:= <<"running">> end, ProcessStatuses),
    WebServerHealthy = WebServerStatus =:= <<"running">>,
    MonitorHealthy = maps:get(status, MonitoringMetrics, unavailable) =/= critical,
    OverallHealth = case ProcessesHealthy andalso WebServerHealthy andalso MonitorHealthy of
        true when length(ActiveAlerts) =:= 0 -> <<"healthy">>;
        true -> <<"warning">>;
        false -> <<"degraded">>
    end,
    
    #{
        timestamp => list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second))),
        overall_health => OverallHealth,
        processes => ProcessStatuses,
        memory => #{
            total_mb => TotalMem div 1024 div 1024,
            processes_mb => ProcessMem div 1024 div 1024
        },
        web_server => #{
            port => Port,
            status => WebServerStatus
        },
        mcp_servers => McpServerStatus,
        monitoring => MonitoringMetrics,
        active_alerts => length(ActiveAlerts),
        alerts => ActiveAlerts
    }.