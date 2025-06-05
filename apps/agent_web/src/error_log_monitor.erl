-module(error_log_monitor).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    interpreter_pid :: pid()
}).

%%====================================================================
%% gen_event callbacks
%%====================================================================

init(InterpreterPid) ->
    {ok, #state{interpreter_pid = InterpreterPid}}.

handle_event({error, _GL, {Pid, Format, Data}}, State) ->
    %% Handle error messages
    LogLine = format_log_message(error, Pid, Format, Data),
    ai_error_interpreter:interpret_log_line(LogLine),
    {ok, State};

handle_event({error_report, _GL, {Pid, Type, Report}}, State) ->
    %% Handle error reports
    ErrorData = #{
        pid => Pid,
        error_type => Type,
        report => Report,
        timestamp => erlang:timestamp()
    },
    ai_error_interpreter:interpret_error(ErrorData),
    {ok, State};

handle_event({warning_msg, _GL, {Pid, Format, Data}}, State) ->
    %% Handle warning messages
    LogLine = format_log_message(warning, Pid, Format, Data),
    ai_error_interpreter:interpret_log_line(LogLine),
    {ok, State};

handle_event({warning_report, _GL, {Pid, Type, Report}}, State) ->
    %% Handle warning reports
    LogLine = format_report(warning, Pid, Type, Report),
    ai_error_interpreter:interpret_log_line(LogLine),
    {ok, State};

handle_event({info_msg, _GL, {Pid, Format, Data}}, State) ->
    %% Handle info messages - only process if they look like errors
    LogLine = format_log_message(info, Pid, Format, Data),
    case contains_error_keywords(LogLine) of
        true -> ai_error_interpreter:interpret_log_line(LogLine);
        false -> ok
    end,
    {ok, State};

handle_event({info_report, _GL, {Pid, Type, Report}}, State) ->
    %% Handle info reports that might contain errors
    case is_error_related_report(Type, Report) of
        true ->
            LogLine = format_report(info, Pid, Type, Report),
            ai_error_interpreter:interpret_log_line(LogLine);
        false ->
            ok
    end,
    {ok, State};

handle_event(_Event, State) ->
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

format_log_message(Level, Pid, Format, Data) ->
    try
        Message = io_lib:format(Format, Data),
        Timestamp = format_timestamp(erlang:timestamp()),
        PidStr = pid_to_list(Pid),
        iolist_to_binary([
            Timestamp, <<" [">>, atom_to_binary(Level, utf8), <<"] ">>,
            <<"PID:">>, PidStr, <<" ">>, Message
        ])
    catch
        _:_ ->
            %% If formatting fails, create a basic message
            iolist_to_binary(io_lib:format("~p [~p] Format error: ~p with ~p", 
                                           [erlang:timestamp(), Level, Format, Data]))
    end.

format_report(Level, Pid, Type, Report) ->
    Timestamp = format_timestamp(erlang:timestamp()),
    PidStr = pid_to_list(Pid),
    ReportStr = io_lib:format("~p", [Report]),
    iolist_to_binary([
        Timestamp, <<" [">>, atom_to_binary(Level, utf8), <<"] ">>,
        <<"PID:">>, PidStr, <<" Type:">>, atom_to_binary(Type, utf8), 
        <<" Report:">>, ReportStr
    ]).

format_timestamp({MegaSec, Sec, MicroSec}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = 
        calendar:now_to_local_time({MegaSec, Sec, MicroSec}),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                  [Year, Month, Day, Hour, Min, Sec]).

contains_error_keywords(LogLine) ->
    ErrorKeywords = [
        <<"error">>, <<"Error">>, <<"ERROR">>,
        <<"failed">>, <<"Failed">>, <<"FAILED">>,
        <<"crash">>, <<"Crash">>, <<"CRASH">>,
        <<"exception">>, <<"Exception">>, <<"EXCEPTION">>,
        <<"terminated">>, <<"Terminated">>, <<"TERMINATED">>,
        <<"badarg">>, <<"badmatch">>, <<"badarith">>,
        <<"function_clause">>, <<"case_clause">>,
        <<"timeout">>, <<"killed">>, <<"shutdown">>
    ],
    
    lists:any(fun(Keyword) ->
        binary:match(LogLine, Keyword) =/= nomatch
    end, ErrorKeywords).

is_error_related_report(supervisor_report, Report) ->
    %% Supervisor reports often indicate problems
    Context = proplists:get_value(errorContext, Report, undefined),
    Context =/= undefined andalso Context =/= start;
    
is_error_related_report(crash_report, _) ->
    true;
    
is_error_related_report(Type, _) ->
    %% Check if the type name suggests an error
    TypeBin = atom_to_binary(Type, utf8),
    contains_error_keywords(TypeBin).