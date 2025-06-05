-module(crash_report_handler).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    processor_pid :: pid()
}).

%%====================================================================
%% gen_event callbacks
%%====================================================================

init(ProcessorPid) ->
    {ok, #state{processor_pid = ProcessorPid}}.

handle_event({error_report, _GL, {Pid, crash_report, Report}}, State) ->
    %% Process OTP crash reports
    CrashData = parse_crash_report(Pid, Report),
    crash_report_processor:process_crash_report(CrashData),
    {ok, State};

handle_event({error_report, _GL, {Pid, supervisor_report, Report}}, State) ->
    %% Process supervisor reports
    CrashData = parse_supervisor_report(Pid, Report),
    crash_report_processor:process_crash_report(CrashData),
    {ok, State};

handle_event({error, _GL, {Pid, Format, Data}}, State) ->
    %% Process generic error messages
    case extract_crash_info(Format, Data) of
        {ok, CrashData} ->
            crash_report_processor:process_crash_report(CrashData);
        _ ->
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

parse_crash_report(Pid, Report) ->
    %% Extract crash information from OTP crash report
    #{
        pid => Pid,
        registered_name => proplists:get_value(registered_name, Report),
        error_type => proplists:get_value(error_info, Report, unknown),
        reason => extract_reason(Report),
        stacktrace => proplists:get_value(current_stacktrace, Report, []),
        initial_call => proplists:get_value(initial_call, Report),
        ancestors => proplists:get_value(ancestors, Report, []),
        messages => proplists:get_value(messages, Report, []),
        links => proplists:get_value(links, Report, []),
        dictionary => proplists:get_value(dictionary, Report, []),
        trap_exit => proplists:get_value(trap_exit, Report, false),
        status => proplists:get_value(status, Report),
        heap_size => proplists:get_value(heap_size, Report),
        stack_size => proplists:get_value(stack_size, Report),
        reductions => proplists:get_value(reductions, Report),
        state => extract_state(Report)
    }.

parse_supervisor_report(Pid, Report) ->
    %% Extract information from supervisor reports
    Context = proplists:get_value(errorContext, Report, undefined),
    Reason = proplists:get_value(reason, Report, undefined),
    Offender = proplists:get_value(offender, Report, []),
    
    %% Extract offender details
    OffenderPid = proplists:get_value(pid, Offender),
    OffenderId = proplists:get_value(id, Offender),
    {M, F, A} = proplists:get_value(mfargs, Offender, {undefined, undefined, undefined}),
    
    #{
        pid => OffenderPid,
        registered_name => OffenderId,
        error_type => {supervisor_error, Context},
        reason => Reason,
        stacktrace => extract_stacktrace_from_reason(Reason),
        supervisor => proplists:get_value(supervisor, Report),
        restart_type => proplists:get_value(restart_type, Offender),
        shutdown => proplists:get_value(shutdown, Offender),
        child_type => proplists:get_value(child_type, Offender),
        module => M,
        function => F,
        args => A
    }.

extract_reason(Report) ->
    case proplists:get_value(error_info, Report) of
        {Class, Reason, _Stacktrace} -> {Class, Reason};
        undefined -> proplists:get_value(reason, Report, unknown);
        Other -> Other
    end.

extract_state(Report) ->
    %% Try to extract gen_server/gen_statem state
    case proplists:get_value(dictionary, Report, []) of
        Dict when is_list(Dict) ->
            case proplists:get_value('$initial_call', Dict) of
                {gen_server, init_it, _} ->
                    %% This was a gen_server, try to get its state
                    proplists:get_value('$gen_server_state', Dict);
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

extract_stacktrace_from_reason({badarg, Stacktrace}) when is_list(Stacktrace) ->
    Stacktrace;
extract_stacktrace_from_reason({badmatch, _, Stacktrace}) when is_list(Stacktrace) ->
    Stacktrace;
extract_stacktrace_from_reason({case_clause, _, Stacktrace}) when is_list(Stacktrace) ->
    Stacktrace;
extract_stacktrace_from_reason({Reason, Stacktrace}) when is_list(Stacktrace) ->
    Stacktrace;
extract_stacktrace_from_reason(_) ->
    [].

extract_crash_info(Format, Data) ->
    %% Try to extract crash information from error messages
    %% This is for catching errors logged via error_logger:error_msg/2
    case is_crash_related(Format) of
        true ->
            {ok, #{
                pid => self(),
                error_type => error_msg,
                reason => {Format, Data},
                stacktrace => [],
                format => Format,
                data => Data
            }};
        false ->
            error
    end.

is_crash_related(Format) when is_list(Format) ->
    %% Check if the format string contains crash-related keywords
    Keywords = ["crash", "error", "failed", "terminated", "badarg", "badmatch", 
                "function_clause", "case_clause", "exception"],
    lists:any(fun(Keyword) -> 
        string:find(string:lowercase(Format), Keyword) =/= nomatch 
    end, Keywords);
is_crash_related(_) ->
    false.