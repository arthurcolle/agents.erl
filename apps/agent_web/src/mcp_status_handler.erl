%% @doc MCP Status Handler
%%
%% Provides detailed status and diagnostics for MCP Streamable HTTP transport
-module(mcp_status_handler).

-include_lib("kernel/include/logger.hrl").

%% Cowboy callbacks
-export([
    init/2
]).

%% Status functions
-export([
    get_transport_status/0,
    get_session_statistics/0,
    get_buffer_statistics/0,
    get_performance_metrics/0
]).

%%% ============================================================================
%%% Cowboy Handler Implementation
%%% ============================================================================

%% @doc Initialize status handler
init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    
    case Method of
        <<"GET">> ->
            handle_status_request(Req0, Opts);
        _ ->
            Req1 = cowboy_req:reply(405, #{
                <<"allow">> => <<"GET">>,
                <<"content-type">> => <<"text/plain">>
            }, <<"Method Not Allowed">>, Req0),
            {ok, Req1, Opts}
    end.

%% @doc Handle status request
-spec handle_status_request(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
handle_status_request(Req0, Opts) ->
    StartTime = erlang:system_time(millisecond),
    
    % Get query parameters for filtering
    QueryParams = cowboy_req:parse_qs(Req0),
    Sections = get_requested_sections(QueryParams),
    
    % Collect status information
    StatusData = collect_status_data(Sections),
    
    EndTime = erlang:system_time(millisecond),
    CollectionTime = EndTime - StartTime,
    
    % Build response
    Response = #{
        <<"timestamp">> => iso8601_timestamp(),
        <<"collection_time_ms">> => CollectionTime,
        <<"sections">> => Sections,
        <<"data">> => StatusData
    },
    
    ResponseBody = jsx:encode(Response),
    
    Req1 = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>,
        <<"cache-control">> => <<"no-cache">>
    }, ResponseBody, Req0),
    
    {ok, Req1, Opts}.

%%% ============================================================================
%%% Status Collection Functions
%%% ============================================================================

%% @doc Get requested sections from query parameters
-spec get_requested_sections([{binary(), binary()}]) -> [binary()].
get_requested_sections(QueryParams) ->
    case proplists:get_value(<<"sections">>, QueryParams) of
        undefined ->
            % Default sections
            [<<"transport">>, <<"sessions">>, <<"buffers">>, <<"performance">>];
        SectionsParam ->
            binary:split(SectionsParam, <<",">>, [global])
    end.

%% @doc Collect status data for requested sections
-spec collect_status_data([binary()]) -> map().
collect_status_data(Sections) ->
    lists:foldl(fun(Section, Acc) ->
        case Section of
            <<"transport">> ->
                Acc#{transport => get_transport_status()};
            <<"sessions">> ->
                Acc#{sessions => get_session_statistics()};
            <<"buffers">> ->
                Acc#{buffers => get_buffer_statistics()};
            <<"performance">> ->
                Acc#{performance => get_performance_metrics()};
            <<"system">> ->
                Acc#{system => get_system_status()};
            <<"connections">> ->
                Acc#{connections => get_connection_statistics()};
            _ ->
                Acc
        end
    end, #{}, Sections).

%% @doc Get transport status
-spec get_transport_status() -> map().
get_transport_status() ->
    try
        TransportPid = whereis(mcp_transport_streamable_http),
        
        BaseStatus = #{
            running => TransportPid =/= undefined,
            pid => case TransportPid of
                undefined -> null;
                _ -> list_to_binary(pid_to_list(TransportPid))
            end,
            protocol_version => <<"2025-03-26">>
        },
        
        case TransportPid of
            undefined ->
                BaseStatus#{
                    status => <<"stopped">>,
                    error => <<"Transport process not running">>
                };
            _ ->
                ProcessInfo = get_process_info(TransportPid),
                BaseStatus#{
                    status => <<"running">>,
                    uptime_ms => maps:get(uptime_ms, ProcessInfo, 0),
                    memory_bytes => maps:get(memory_bytes, ProcessInfo, 0),
                    message_queue_len => maps:get(message_queue_len, ProcessInfo, 0),
                    capabilities => get_server_capabilities()
                }
        end
    catch
        error:Reason ->
            #{
                status => <<"error">>,
                error => format_error(Reason)
            }
    end.

%% @doc Get session statistics
-spec get_session_statistics() -> map().
get_session_statistics() ->
    try
        case whereis(mcp_http_session_manager) of
            undefined ->
                #{
                    status => <<"unavailable">>,
                    error => <<"Session manager not running">>
                };
            _Pid ->
                Sessions = mcp_http_session_manager:list_sessions(),
                
                {ActiveSessions, TotalStreams, StreamsBySession} = 
                    analyze_sessions(Sessions),
                
                #{
                    status => <<"active">>,
                    total_sessions => length(Sessions),
                    active_sessions => ActiveSessions,
                    total_streams => TotalStreams,
                    average_streams_per_session => case ActiveSessions of
                        0 -> 0;
                        _ -> TotalStreams / ActiveSessions
                    end,
                    sessions_by_stream_count => StreamsBySession,
                    oldest_session_age_ms => get_oldest_session_age(Sessions),
                    newest_session_age_ms => get_newest_session_age(Sessions)
                }
        end
    catch
        error:Reason ->
            #{
                status => <<"error">>,
                error => format_error(Reason)
            }
    end.

%% @doc Get buffer statistics  
-spec get_buffer_statistics() -> map().
get_buffer_statistics() ->
    try
        case whereis(mcp_http_message_buffer) of
            undefined ->
                #{
                    status => <<"unavailable">>,
                    error => <<"Message buffer not running">>
                };
            _Pid ->
                % This would require additional API methods in the buffer module
                % For now, return basic information
                #{
                    status => <<"active">>,
                    note => <<"Buffer statistics require additional API methods">>
                }
        end
    catch
        error:Reason ->
            #{
                status => <<"error">>,
                error => format_error(Reason)
            }
    end.

%% @doc Get performance metrics
-spec get_performance_metrics() -> map().
get_performance_metrics() ->
    try
        % System-wide metrics
        MemoryInfo = erlang:memory(),
        ProcessInfo = #{
            count => erlang:system_info(process_count),
            limit => erlang:system_info(process_limit),
            usage_percent => erlang:system_info(process_count) / 
                           erlang:system_info(process_limit) * 100
        },
        
        % Scheduler utilization
        SchedulerInfo = get_scheduler_utilization(),
        
        % Network statistics (if available)
        NetworkStats = get_network_statistics(),
        
        #{
            memory => #{
                total_bytes => proplists:get_value(total, MemoryInfo, 0),
                processes_bytes => proplists:get_value(processes, MemoryInfo, 0),
                system_bytes => proplists:get_value(system, MemoryInfo, 0),
                atom_bytes => proplists:get_value(atom, MemoryInfo, 0),
                binary_bytes => proplists:get_value(binary, MemoryInfo, 0),
                ets_bytes => proplists:get_value(ets, MemoryInfo, 0)
            },
            processes => ProcessInfo,
            schedulers => SchedulerInfo,
            network => NetworkStats,
            garbage_collection => get_gc_statistics()
        }
    catch
        error:Reason ->
            #{
                status => <<"error">>,
                error => format_error(Reason)
            }
    end.

%% @doc Get system status
-spec get_system_status() -> map().
get_system_status() ->
    #{
        node => atom_to_binary(node()),
        uptime_ms => get_system_uptime(),
        otp_release => list_to_binary(erlang:system_info(otp_release)),
        erts_version => list_to_binary(erlang:system_info(version)),
        system_architecture => list_to_binary(erlang:system_info(system_architecture)),
        word_size => erlang:system_info(wordsize),
        smp_support => erlang:system_info(smp_support),
        thread_pool_size => erlang:system_info(thread_pool_size),
        dirty_cpu_schedulers => erlang:system_info(dirty_cpu_schedulers),
        dirty_io_schedulers => erlang:system_info(dirty_io_schedulers)
    }.

%% @doc Get connection statistics
-spec get_connection_statistics() -> map().
get_connection_statistics() ->
    % This would integrate with Cowboy connection statistics
    % For now, return placeholder data
    #{
        note => <<"Connection statistics require Cowboy integration">>,
        active_connections => 0,
        total_connections => 0,
        connections_per_second => 0
    }.

%%% ============================================================================
%%% Analysis Functions
%%% ============================================================================

%% @doc Analyze sessions for statistics
-spec analyze_sessions(list()) -> {non_neg_integer(), non_neg_integer(), map()}.
analyze_sessions(Sessions) ->
    {ActiveCount, TotalStreams, StreamCounts} = lists:foldl(fun(Session, {Active, Total, Counts}) ->
        StreamCount = length(maps:get(streams, Session, [])),
        StreamCountBin = integer_to_binary(StreamCount),
        
        NewCounts = maps:update_with(StreamCountBin, fun(C) -> C + 1 end, 1, Counts),
        
        case StreamCount > 0 of
            true -> {Active + 1, Total + StreamCount, NewCounts};
            false -> {Active, Total, NewCounts}
        end
    end, {0, 0, #{}}, Sessions),
    
    {ActiveCount, TotalStreams, StreamCounts}.

%% @doc Get oldest session age
-spec get_oldest_session_age(list()) -> non_neg_integer().
get_oldest_session_age([]) -> 0;
get_oldest_session_age(Sessions) ->
    Now = erlang:system_time(millisecond),
    OldestCreated = lists:min([
        maps:get(created_at, Session, Now)
        || Session <- Sessions
    ]),
    Now - OldestCreated.

%% @doc Get newest session age
-spec get_newest_session_age(list()) -> non_neg_integer().
get_newest_session_age([]) -> 0;
get_newest_session_age(Sessions) ->
    Now = erlang:system_time(millisecond),
    NewestCreated = lists:max([
        maps:get(created_at, Session, 0)
        || Session <- Sessions
    ]),
    Now - NewestCreated.

%% @doc Get process information
-spec get_process_info(pid()) -> map().
get_process_info(Pid) ->
    case process_info(Pid, [memory, message_queue_len, total_heap_size]) of
        undefined ->
            #{};
        Info ->
            #{
                memory_bytes => proplists:get_value(memory, Info, 0),
                message_queue_len => proplists:get_value(message_queue_len, Info, 0),
                total_heap_size => proplists:get_value(total_heap_size, Info, 0),
                uptime_ms => 0 % Placeholder - would need process start time tracking
            }
    end.

%% @doc Get scheduler utilization
-spec get_scheduler_utilization() -> map().
get_scheduler_utilization() ->
    SchedulerCount = erlang:system_info(schedulers),
    OnlineCount = erlang:system_info(schedulers_online),
    
    #{
        total => SchedulerCount,
        online => OnlineCount,
        dirty_cpu => erlang:system_info(dirty_cpu_schedulers),
        dirty_io => erlang:system_info(dirty_io_schedulers),
        utilization_note => <<"Scheduler utilization tracking requires additional instrumentation">>
    }.

%% @doc Get network statistics
-spec get_network_statistics() -> map().
get_network_statistics() ->
    #{
        note => <<"Network statistics require OS-level integration">>,
        bytes_in => 0,
        bytes_out => 0,
        packets_in => 0,
        packets_out => 0
    }.

%% @doc Get garbage collection statistics
-spec get_gc_statistics() -> map().
get_gc_statistics() ->
    [{Number, Count, Mean, _Mean2}] = erlang:statistics(garbage_collection),
    
    #{
        collections => Count,
        words_reclaimed => Number,
        mean_reclaimed_per_collection => Mean,
        note => <<"Per-process GC statistics available via process_info/2">>
    }.

%% @doc Get server capabilities
-spec get_server_capabilities() -> map().
get_server_capabilities() ->
    #{
        logging => #{},
        tools => #{
            list_changed => true
        },
        resources => #{
            subscribe => true,
            list_changed => true
        },
        prompts => #{
            list_changed => true
        }
    }.

%% @doc Get system uptime (simplified)
-spec get_system_uptime() -> non_neg_integer().
get_system_uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    UpTime.

%%% ============================================================================
%%% Utility Functions
%%% ============================================================================

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