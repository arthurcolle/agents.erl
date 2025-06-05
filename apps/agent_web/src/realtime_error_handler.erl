-module(realtime_error_handler).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    monitor_pid :: pid()
}).

init(MonitorPid) ->
    {ok, #state{monitor_pid = MonitorPid}}.

handle_event({error, _GL, {Pid, Format, Data}}, State) ->
    Line = format_error_line(error, Pid, Format, Data),
    gen_server:cast(State#state.monitor_pid, {log_line, Line}),
    {ok, State};

handle_event({error_report, _GL, {Pid, Type, Report}}, State) ->
    Line = format_error_report(Pid, Type, Report),
    gen_server:cast(State#state.monitor_pid, {log_line, Line}),
    {ok, State};

handle_event({warning_msg, _GL, {Pid, Format, Data}}, State) ->
    Line = format_error_line(warning, Pid, Format, Data),
    gen_server:cast(State#state.monitor_pid, {log_line, Line}),
    {ok, State};

handle_event({info_report, _GL, {Pid, progress, _}}, State) ->
    %% Skip progress reports
    {ok, State};

handle_event(Event, State) ->
    %% Log other events for debugging
    error_logger:info_msg("Unhandled event in realtime_error_handler: ~p~n", [Event]),
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

format_error_line(Level, Pid, Format, Data) ->
    try
        Message = io_lib:format(Format, Data),
        Timestamp = format_timestamp(erlang:timestamp()),
        iolist_to_binary([
            Timestamp, <<" ">>,
            format_level(Level), <<" ">>,
            <<"PID:">>, pid_to_list(Pid), <<" ">>,
            Message
        ])
    catch
        _:_ ->
            iolist_to_binary(io_lib:format("~s ~s Format error: ~p",
                                           [format_timestamp(erlang:timestamp()),
                                            format_level(Level),
                                            Format]))
    end.

format_error_report(Pid, Type, Report) ->
    Timestamp = format_timestamp(erlang:timestamp()),
    iolist_to_binary([
        Timestamp, <<" ">>,
        format_report_type(Type), <<" ">>,
        <<"PID:">>, pid_to_list(Pid), <<" ">>,
        io_lib:format("~p", [Report])
    ]).

format_level(error) -> <<"[ERROR]">>;
format_level(warning) -> <<"[WARNING]">>;
format_level(info) -> <<"[INFO]">>;
format_level(_) -> <<"[UNKNOWN]">>.

format_report_type(crash_report) -> <<"[CRASH REPORT]">>;
format_report_type(supervisor_report) -> <<"[SUPERVISOR REPORT]">>;
format_report_type(Type) -> iolist_to_binary([<<"[">>, atom_to_binary(Type, utf8), <<"]">>]).

format_timestamp({MegaSec, Sec, MicroSec}) ->
    {{Year, Month, Day}, {Hour, Min, Seconds}} = 
        calendar:now_to_local_time({MegaSec, Sec, MicroSec}),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                  [Year, Month, Day, Hour, Min, Seconds]).