-module(agent_scheduling_handler).
-behaviour(cowboy_handler).

%% Cowboy handler callbacks
-export([init/2]).

%% Internal exports
-export([handle_request/3]).

%%%===================================================================
%%% Cowboy handler callbacks
%%%===================================================================

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    
    %% Parse path components
    PathParts = binary:split(Path, <<"/">>, [global, trim]),
    
    {ok, Body, Req2} = case Method of
        <<"GET">> -> {ok, <<>>, Req};
        _ -> cowboy_req:read_body(Req)
    end,
    
    %% Handle the request
    {Status, ResponseBody} = handle_request(Method, PathParts, Body),
    
    %% Send response
    Req3 = cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, ResponseBody, Req2),
    
    {ok, Req3, State}.

%%%===================================================================
%%% Request handling
%%%===================================================================

%% GET /api/scheduling/tasks - List all scheduled tasks
handle_request(<<"GET">>, [<<"api">>, <<"scheduling">>, <<"tasks">>], _Body) ->
    case agent_scheduler_engine:list_all_tasks() of
        {ok, Tasks} ->
            TasksJson = lists:map(fun task_to_json/1, Tasks),
            {200, jsx:encode(#{
                <<"status">> => <<"success">>,
                <<"tasks">> => TasksJson,
                <<"count">> => length(TasksJson)
            })};
        {error, Reason} ->
            {500, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => format_error(Reason)
            })}
    end;

%% GET /api/scheduling/agents/{agent_id}/tasks - Get tasks for specific agent
handle_request(<<"GET">>, [<<"api">>, <<"scheduling">>, <<"agents">>, AgentId, <<"tasks">>], _Body) ->
    case agent_scheduler_engine:get_agent_schedule(AgentId) of
        {ok, Tasks} ->
            TasksJson = lists:map(fun task_to_json/1, Tasks),
            {200, jsx:encode(#{
                <<"status">> => <<"success">>,
                <<"agent_id">> => AgentId,
                <<"tasks">> => TasksJson,
                <<"count">> => length(TasksJson)
            })};
        {error, Reason} ->
            {500, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => format_error(Reason)
            })}
    end;

%% POST /api/scheduling/tasks - Schedule a new task
handle_request(<<"POST">>, [<<"api">>, <<"scheduling">>, <<"tasks">>], Body) ->
    try
        Data = jsx:decode(Body, [return_maps]),
        
        %% Validate required fields
        AgentId = maps:get(<<"agent_id">>, Data),
        Task = maps:get(<<"task">>, Data),
        ScheduledTime = parse_scheduled_time(maps:get(<<"scheduled_time">>, Data)),
        Options = maps:get(<<"options">>, Data, #{}),
        
        case agent_scheduler_engine:schedule_task(AgentId, Task, ScheduledTime, Options) of
            {ok, TaskId} ->
                %% Store event in timeline
                timeline_event_store:store(#{
                    type => task_scheduled_api,
                    agent_id => AgentId,
                    task_id => TaskId,
                    task => Task,
                    scheduled_time => ScheduledTime,
                    source => api
                }),
                
                {201, jsx:encode(#{
                    <<"status">> => <<"success">>,
                    <<"task_id">> => TaskId,
                    <<"agent_id">> => AgentId,
                    <<"scheduled_time">> => format_timestamp(ScheduledTime)
                })};
            {error, Reason} ->
                {400, jsx:encode(#{
                    <<"status">> => <<"error">>,
                    <<"error">> => format_error(Reason)
                })}
        end
    catch
        _:_ ->
            {400, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => <<"invalid_request_body">>
            })}
    end;

%% DELETE /api/scheduling/tasks/{task_id} - Cancel a scheduled task
handle_request(<<"DELETE">>, [<<"api">>, <<"scheduling">>, <<"tasks">>, TaskId], _Body) ->
    case agent_scheduler_engine:cancel_task(TaskId) of
        ok ->
            %% Store cancellation event
            timeline_event_store:store(#{
                type => task_cancelled_api,
                task_id => TaskId,
                source => api
            }),
            
            {200, jsx:encode(#{
                <<"status">> => <<"success">>,
                <<"task_id">> => TaskId,
                <<"message">> => <<"Task cancelled">>
            })};
        {error, not_found} ->
            {404, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => <<"task_not_found">>
            })};
        {error, Reason} ->
            {500, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => format_error(Reason)
            })}
    end;

%% PUT /api/scheduling/tasks/{task_id} - Reschedule a task
handle_request(<<"PUT">>, [<<"api">>, <<"scheduling">>, <<"tasks">>, TaskId], Body) ->
    try
        Data = jsx:decode(Body, [return_maps]),
        NewTime = parse_scheduled_time(maps:get(<<"scheduled_time">>, Data)),
        Options = maps:get(<<"options">>, Data, #{}),
        
        case agent_scheduler_engine:reschedule_task(TaskId, NewTime, Options) of
            ok ->
                %% Store reschedule event
                timeline_event_store:store(#{
                    type => task_rescheduled_api,
                    task_id => TaskId,
                    new_time => NewTime,
                    source => api
                }),
                
                {200, jsx:encode(#{
                    <<"status">> => <<"success">>,
                    <<"task_id">> => TaskId,
                    <<"new_scheduled_time">> => format_timestamp(NewTime)
                })};
            {error, not_found} ->
                {404, jsx:encode(#{
                    <<"status">> => <<"error">>,
                    <<"error">> => <<"task_not_found">>
                })};
            {error, task_not_pending} ->
                {400, jsx:encode(#{
                    <<"status">> => <<"error">>,
                    <<"error">> => <<"task_not_pending">>
                })};
            {error, Reason} ->
                {500, jsx:encode(#{
                    <<"status">> => <<"error">>,
                    <<"error">> => format_error(Reason)
                })}
        end
    catch
        _:_ ->
            {400, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => <<"invalid_request_body">>
            })}
    end;

%% POST /api/scheduling/tasks/{task_id}/execute - Execute a task immediately
handle_request(<<"POST">>, [<<"api">>, <<"scheduling">>, <<"tasks">>, TaskId, <<"execute">>], _Body) ->
    case agent_scheduler_engine:execute_now(TaskId) of
        ok ->
            %% Store immediate execution event
            timeline_event_store:store(#{
                type => task_executed_immediately,
                task_id => TaskId,
                source => api
            }),
            
            {200, jsx:encode(#{
                <<"status">> => <<"success">>,
                <<"task_id">> => TaskId,
                <<"message">> => <<"Task execution started">>
            })};
        {error, not_found} ->
            {404, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => <<"task_not_found">>
            })};
        {error, task_not_pending} ->
            {400, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => <<"task_not_pending">>
            })};
        {error, Reason} ->
            {500, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => format_error(Reason)
            })}
    end;

%% GET /api/scheduling/tasks/{task_id} - Get task details
handle_request(<<"GET">>, [<<"api">>, <<"scheduling">>, <<"tasks">>, TaskId], _Body) ->
    case agent_scheduler_engine:get_task_status(TaskId) of
        {ok, Status} ->
            {200, jsx:encode(#{
                <<"status">> => <<"success">>,
                <<"task_id">> => TaskId,
                <<"task_status">> => Status
            })};
        {error, not_found} ->
            {404, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => <<"task_not_found">>
            })};
        {error, Reason} ->
            {500, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => format_error(Reason)
            })}
    end;

%% GET /api/scheduling/upcoming?window=3600 - Get upcoming tasks
handle_request(<<"GET">>, [<<"api">>, <<"scheduling">>, <<"upcoming">>], _Body) ->
    %% Default to 1 hour window
    TimeWindow = 3600,
    
    case agent_scheduler_engine:get_upcoming_tasks(TimeWindow) of
        {ok, Tasks} ->
            TasksJson = lists:map(fun task_to_json/1, Tasks),
            {200, jsx:encode(#{
                <<"status">> => <<"success">>,
                <<"tasks">> => TasksJson,
                <<"count">> => length(TasksJson),
                <<"time_window">> => TimeWindow
            })};
        {error, Reason} ->
            {500, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => format_error(Reason)
            })}
    end;

%% GET /api/scheduling/agents/{agent_id}/temporal - Get temporal context
handle_request(<<"GET">>, [<<"api">>, <<"scheduling">>, <<"agents">>, AgentId, <<"temporal">>], _Body) ->
    case agent_temporal_awareness:get_temporal_context(AgentId) of
        {ok, Context} ->
            {200, jsx:encode(#{
                <<"status">> => <<"success">>,
                <<"agent_id">> => AgentId,
                <<"temporal_context">> => context_to_json(Context)
            })};
        {error, not_enabled} ->
            {404, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => <<"temporal_awareness_not_enabled">>
            })};
        {error, Reason} ->
            {500, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => format_error(Reason)
            })}
    end;

%% POST /api/scheduling/agents/{agent_id}/temporal/enable - Enable temporal awareness
handle_request(<<"POST">>, [<<"api">>, <<"scheduling">>, <<"agents">>, AgentId, <<"temporal">>, <<"enable">>], _Body) ->
    case agent_temporal_awareness:enable_temporal_awareness(AgentId) of
        ok ->
            {200, jsx:encode(#{
                <<"status">> => <<"success">>,
                <<"agent_id">> => AgentId,
                <<"message">> => <<"Temporal awareness enabled">>
            })};
        {error, Reason} ->
            {500, jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"error">> => format_error(Reason)
            })}
    end;

%% Default handler for unmatched routes
handle_request(_Method, _Path, _Body) ->
    {404, jsx:encode(#{
        <<"status">> => <<"error">>,
        <<"error">> => <<"not_found">>
    })}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

task_to_json({scheduled_task, Id, AgentId, Task, ScheduledTime, Status, CreatedAt, Result, Retries, MaxRetries}) ->
    #{
        <<"id">> => Id,
        <<"agent_id">> => AgentId,
        <<"task">> => Task,
        <<"scheduled_time">> => format_timestamp(ScheduledTime),
        <<"status">> => Status,
        <<"created_at">> => format_timestamp(CreatedAt),
        <<"result">> => Result,
        <<"retries">> => Retries,
        <<"max_retries">> => MaxRetries
    }.

context_to_json(Context) ->
    %% Convert temporal context to JSON-safe format
    maps:map(
        fun(temporal_summary, Summary) ->
                Summary;
           (past_events, Events) ->
                lists:map(fun event_to_json/1, lists:sublist(Events, 10));
           (predicted_futures, Predictions) ->
                lists:map(fun event_to_json/1, Predictions);
           (K, V) ->
                V
        end,
        Context
    ).

event_to_json(Event) when is_map(Event) ->
    maps:map(
        fun(_K, V) when is_tuple(V) ->
                tuple_to_list(V);
           (_K, V) when is_atom(V) ->
                atom_to_binary(V, utf8);
           (_K, V) ->
                V
        end,
        Event
    ).

parse_scheduled_time(TimeStr) when is_binary(TimeStr) ->
    %% Parse ISO 8601 datetime or relative time
    case TimeStr of
        <<"+", RelativeTime/binary>> ->
            %% Parse relative time like "+1h", "+30m", "+1d"
            parse_relative_time(RelativeTime);
        _ ->
            %% Parse ISO 8601
            parse_iso8601(TimeStr)
    end;
parse_scheduled_time(Seconds) when is_integer(Seconds) ->
    %% Seconds from now
    add_seconds_to_timestamp(erlang:timestamp(), Seconds).

parse_relative_time(RelativeTime) ->
    %% Simple parser for relative times
    case binary:last(RelativeTime) of
        $s -> % seconds
            Seconds = binary_to_integer(binary:part(RelativeTime, 0, byte_size(RelativeTime) - 1)),
            add_seconds_to_timestamp(erlang:timestamp(), Seconds);
        $m -> % minutes
            Minutes = binary_to_integer(binary:part(RelativeTime, 0, byte_size(RelativeTime) - 1)),
            add_seconds_to_timestamp(erlang:timestamp(), Minutes * 60);
        $h -> % hours
            Hours = binary_to_integer(binary:part(RelativeTime, 0, byte_size(RelativeTime) - 1)),
            add_seconds_to_timestamp(erlang:timestamp(), Hours * 3600);
        $d -> % days
            Days = binary_to_integer(binary:part(RelativeTime, 0, byte_size(RelativeTime) - 1)),
            add_seconds_to_timestamp(erlang:timestamp(), Days * 86400);
        _ ->
            erlang:timestamp()
    end.

parse_iso8601(_DateTimeStr) ->
    %% Simplified - would need full ISO 8601 parser
    erlang:timestamp().

format_timestamp({Mega, Sec, Micro}) ->
    %% Convert to milliseconds
    Millis = (Mega * 1000000 + Sec) * 1000 + Micro div 1000,
    Millis.

format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(_Reason) ->
    <<"unknown_error">>.

add_seconds_to_timestamp({Mega, Sec, Micro}, Seconds) ->
    TotalMicro = (Mega * 1000000 + Sec) * 1000000 + Micro + (Seconds * 1000000),
    NewMega = TotalMicro div 1000000000000,
    NewSec = (TotalMicro rem 1000000000000) div 1000000,
    NewMicro = TotalMicro rem 1000000,
    {NewMega, NewSec, NewMicro}.