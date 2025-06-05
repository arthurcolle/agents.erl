-module(crash_report_api_handler).

-export([init/2,
         handle_get/2,
         handle_post/2,
         handle_delete/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Req = handle_request(Method, Req0, State),
    {ok, Req, State}.

handle_request(<<"GET">>, Req, State) ->
    handle_get(Req, State);
handle_request(<<"POST">>, Req, State) ->
    handle_post(Req, State);
handle_request(<<"DELETE">>, Req, State) ->
    handle_delete(Req, State);
handle_request(_, Req, State) ->
    respond_json(Req, 405, #{error => <<"Method not allowed">>}).

%%====================================================================
%% GET handlers
%%====================================================================

handle_get(Req0, State) ->
    Path = cowboy_req:path_info(Req0),
    handle_get_path(Path, Req0, State).

handle_get_path([], Req, _State) ->
    %% GET /api/crashes - List all crash reports
    QsVals = cowboy_req:parse_qs(Req),
    Filter = build_filter(QsVals),
    
    case crash_report_processor:get_crash_reports(Filter) of
        {ok, Reports} ->
            FormattedReports = [format_crash_report(R) || R <- Reports],
            respond_json(Req, 200, #{
                status => success,
                reports => FormattedReports,
                total => length(FormattedReports)
            });
        {error, Reason} ->
            respond_json(Req, 500, #{error => Reason})
    end;

handle_get_path([<<"stats">>], Req, _State) ->
    %% GET /api/crashes/stats - Get crash statistics
    case get_crash_statistics() of
        {ok, Stats} ->
            respond_json(Req, 200, #{
                status => success,
                statistics => Stats
            });
        {error, Reason} ->
            respond_json(Req, 500, #{error => Reason})
    end;

handle_get_path([CrashId], Req, _State) ->
    %% GET /api/crashes/:id - Get specific crash report
    case crash_report_processor:get_crash_reports(#{id => CrashId}) of
        {ok, [Report]} ->
            respond_json(Req, 200, #{
                status => success,
                report => format_crash_report(Report)
            });
        {ok, []} ->
            respond_json(Req, 404, #{error => <<"Crash report not found">>});
        {error, Reason} ->
            respond_json(Req, 500, #{error => Reason})
    end;

handle_get_path([CrashId, <<"analysis">>], Req, _State) ->
    %% GET /api/crashes/:id/analysis - Get crash analysis
    case crash_report_processor:get_crash_analysis(CrashId) of
        {ok, Analysis} ->
            respond_json(Req, 200, #{
                status => success,
                analysis => format_analysis(Analysis)
            });
        {error, not_found} ->
            respond_json(Req, 404, #{error => <<"Analysis not found">>});
        {error, Reason} ->
            respond_json(Req, 500, #{error => Reason})
    end;

handle_get_path([CrashId, <<"fixes">>], Req, _State) ->
    %% GET /api/crashes/:id/fixes - Get fix proposals
    case crash_report_processor:get_fix_proposals(CrashId) of
        {ok, Fixes} ->
            respond_json(Req, 200, #{
                status => success,
                fixes => [format_fix(F) || F <- Fixes]
            });
        {error, not_found} ->
            respond_json(Req, 404, #{error => <<"Fixes not found">>});
        {error, Reason} ->
            respond_json(Req, 500, #{error => Reason})
    end;

handle_get_path(_, Req, _State) ->
    respond_json(Req, 404, #{error => <<"Not found">>}).

%%====================================================================
%% POST handlers
%%====================================================================

handle_post(Req0, State) ->
    Path = cowboy_req:path_info(Req0),
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    
    case jsx:decode(Body, [return_maps]) of
        Data when is_map(Data) ->
            handle_post_path(Path, Data, Req, State);
        _ ->
            respond_json(Req, 400, #{error => <<"Invalid JSON">>})
    end.

handle_post_path([CrashId, <<"analyze">>], _Data, Req, _State) ->
    %% POST /api/crashes/:id/analyze - Trigger analysis
    case trigger_analysis(CrashId) of
        ok ->
            respond_json(Req, 202, #{
                status => accepted,
                message => <<"Analysis started">>
            });
        {error, Reason} ->
            respond_json(Req, 500, #{error => Reason})
    end;

handle_post_path([CrashId, <<"fix">>], Data, Req, _State) ->
    %% POST /api/crashes/:id/fix - Apply a fix
    FixId = maps:get(<<"fix_id">>, Data),
    case apply_fix(CrashId, FixId) of
        {ok, Result} ->
            respond_json(Req, 200, #{
                status => success,
                result => Result
            });
        {error, Reason} ->
            respond_json(Req, 500, #{error => Reason})
    end;

handle_post_path(_, _, Req, _State) ->
    respond_json(Req, 404, #{error => <<"Not found">>}).

%%====================================================================
%% DELETE handlers
%%====================================================================

handle_delete(Req0, State) ->
    Path = cowboy_req:path_info(Req0),
    handle_delete_path(Path, Req0, State).

handle_delete_path([CrashId], Req, _State) ->
    %% DELETE /api/crashes/:id - Delete crash report
    case delete_crash_report(CrashId) of
        ok ->
            respond_json(Req, 200, #{
                status => success,
                message => <<"Crash report deleted">>
            });
        {error, Reason} ->
            respond_json(Req, 500, #{error => Reason})
    end;

handle_delete_path(_, Req, _State) ->
    respond_json(Req, 404, #{error => <<"Not found">>}).

%%====================================================================
%% Internal functions
%%====================================================================

build_filter(QsVals) ->
    lists:foldl(fun({Key, Value}, Acc) ->
        case Key of
            <<"severity">> -> Acc#{severity => binary_to_atom(Value)};
            <<"status">> -> Acc#{status => binary_to_atom(Value)};
            <<"module">> -> Acc#{module => binary_to_atom(Value)};
            <<"since">> -> Acc#{since => binary_to_integer(Value)};
            _ -> Acc
        end
    end, #{}, QsVals).

format_crash_report({crash_report, Id, Timestamp, ProcessName, Pid, ErrorType, 
                    Reason, Module, Function, Line, Stacktrace, _State, 
                    Severity, Frequency, LastOccurrence, Status}) ->
    #{
        id => Id,
        timestamp => format_timestamp(Timestamp),
        process_name => ProcessName,
        pid => list_to_binary(pid_to_list(Pid)),
        error_type => format_error_type(ErrorType),
        reason => format_reason(Reason),
        location => #{
            module => Module,
            function => Function,
            line => Line
        },
        stacktrace => format_stacktrace(Stacktrace),
        severity => Severity,
        frequency => Frequency,
        last_occurrence => format_timestamp(LastOccurrence),
        status => Status
    }.

format_analysis({analysis_result, CrashId, RootCause, Impact, RelatedIssues, 
                Confidence, SuggestedActions}) ->
    #{
        crash_id => CrashId,
        root_cause => RootCause,
        impact => Impact,
        related_issues => RelatedIssues,
        confidence => Confidence,
        suggested_actions => SuggestedActions
    }.

format_fix({fix_proposal, Id, CrashId, Description, CodeChanges, 
           RiskLevel, EstimatedEffort, Automated}) ->
    #{
        id => Id,
        crash_id => CrashId,
        description => Description,
        code_changes => CodeChanges,
        risk_level => RiskLevel,
        estimated_effort => EstimatedEffort,
        automated => Automated
    }.

format_timestamp({MegaSec, Sec, MicroSec}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = 
        calendar:now_to_datetime({MegaSec, Sec, MicroSec}),
    list_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                      [Year, Month, Day, Hour, Min, Sec])
    ).

format_error_type(ErrorType) ->
    iolist_to_binary(io_lib:format("~p", [ErrorType])).

format_reason(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

format_stacktrace(Stacktrace) ->
    [format_stack_entry(Entry) || Entry <- Stacktrace].

format_stack_entry({Module, Function, Arity, Info}) when is_integer(Arity) ->
    #{
        module => Module,
        function => Function,
        arity => Arity,
        file => proplists:get_value(file, Info, <<"">>),
        line => proplists:get_value(line, Info, 0)
    };
format_stack_entry({Module, Function, Args, Info}) when is_list(Args) ->
    #{
        module => Module,
        function => Function,
        arity => length(Args),
        file => proplists:get_value(file, Info, <<"">>),
        line => proplists:get_value(line, Info, 0)
    }.

get_crash_statistics() ->
    try
        {ok, AllReports} = crash_report_processor:get_crash_reports(),
        
        %% Calculate statistics
        Stats = #{
            total_crashes => length(AllReports),
            by_severity => count_by_severity(AllReports),
            by_status => count_by_status(AllReports),
            by_module => top_modules(AllReports, 10),
            recent_crashes => recent_crashes(AllReports, 10),
            crash_rate => calculate_crash_rate(AllReports)
        },
        
        {ok, Stats}
    catch
        _:Reason ->
            {error, Reason}
    end.

count_by_severity(Reports) ->
    lists:foldl(fun({crash_report, _, _, _, _, _, _, _, _, _, _, _, Severity, _, _, _}, Acc) ->
        maps:update_with(Severity, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Reports).

count_by_status(Reports) ->
    lists:foldl(fun({crash_report, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Status}, Acc) ->
        maps:update_with(Status, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Reports).

top_modules(Reports, N) ->
    ModuleCounts = lists:foldl(fun({crash_report, _, _, _, _, _, _, Module, _, _, _, _, _, _, _, _}, Acc) ->
        maps:update_with(Module, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Reports),
    
    Sorted = lists:sort(fun({_, A}, {_, B}) -> A > B end, maps:to_list(ModuleCounts)),
    lists:sublist(Sorted, N).

recent_crashes(Reports, N) ->
    Sorted = lists:sort(fun({crash_report, _, T1, _, _, _, _, _, _, _, _, _, _, _, _, _},
                           {crash_report, _, T2, _, _, _, _, _, _, _, _, _, _, _, _, _}) ->
        T1 > T2
    end, Reports),
    
    lists:sublist([format_crash_report(R) || R <- Sorted], N).

calculate_crash_rate(Reports) ->
    Now = erlang:timestamp(),
    OneHourAgo = timestamp_subtract_seconds(Now, 3600),
    
    RecentCrashes = lists:filter(fun({crash_report, _, Timestamp, _, _, _, _, _, _, _, _, _, _, _, _, _}) ->
        Timestamp > OneHourAgo
    end, Reports),
    
    length(RecentCrashes).

timestamp_subtract_seconds({MegaSec, Sec, MicroSec}, Seconds) ->
    TotalMicroSec = (MegaSec * 1000000 + Sec) * 1000000 + MicroSec,
    NewTotalMicroSec = TotalMicroSec - (Seconds * 1000000),
    NewMegaSec = NewTotalMicroSec div 1000000000000,
    NewSec = (NewTotalMicroSec rem 1000000000000) div 1000000,
    NewMicroSec = NewTotalMicroSec rem 1000000,
    {NewMegaSec, NewSec, NewMicroSec}.

trigger_analysis(CrashId) ->
    %% Re-trigger analysis for a crash
    case crash_report_processor:get_crash_reports(#{id => CrashId}) of
        {ok, [Report]} ->
            crash_ai_analyzer:analyze(whereis(crash_ai_analyzer), Report),
            ok;
        _ ->
            {error, not_found}
    end.

apply_fix(_CrashId, _FixId) ->
    %% TODO: Implement automated fix application
    {ok, #{message => <<"Fix application not yet implemented">>}}.

delete_crash_report(_CrashId) ->
    %% TODO: Implement crash report deletion
    {error, not_implemented}.

respond_json(Req, StatusCode, Data) ->
    Body = jsx:encode(Data),
    cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req).