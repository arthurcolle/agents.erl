-module(crash_report_ws_handler).
-behaviour(cowboy_websocket).

%% Cowboy callbacks
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(state, {
    client_id :: binary(),
    subscribed :: boolean()
}).

%%====================================================================
%% Cowboy callbacks
%%====================================================================

init(Req, _Opts) ->
    {cowboy_websocket, Req, #state{
        client_id = generate_client_id(),
        subscribed = false
    }}.

websocket_init(State = #state{client_id = ClientId}) ->
    error_logger:info_msg("WebSocket client connected: ~p~n", [ClientId]),
    
    %% Subscribe to crash reports
    crash_report_processor:subscribe(self()),
    
    %% Send initial connection message
    Msg = jsx:encode(#{
        type => <<"connected">>,
        client_id => ClientId,
        timestamp => erlang:system_time(second)
    }),
    
    {[{text, Msg}], State#state{subscribed = true}}.

websocket_handle({text, Msg}, State) ->
    try
        Data = jsx:decode(Msg, [return_maps]),
        handle_client_message(Data, State)
    catch
        _:_ ->
            ErrorMsg = jsx:encode(#{
                type => <<"error">>,
                message => <<"Invalid JSON">>
            }),
            {[{text, ErrorMsg}], State}
    end;

websocket_handle({binary, _}, State) ->
    {ok, State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({crash_report_event, Event}, State) ->
    %% Handle crash report events from the processor
    %% Process crash event with AI for dynamic analysis
    ai_error_processor:process_websocket_error(#{
        <<"crash_event">> => Event,
        <<"handler">> => <<"crash_report_ws_handler">>,
        <<"timestamp">> => erlang:system_time(second)
    }, <<"crash_report_system">>),
    
    Msg = format_crash_event(Event),
    {[{text, Msg}], State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, #state{subscribed = true}) ->
    %% Unsubscribe from crash reports
    crash_report_processor:unsubscribe(self()),
    ok;
terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

generate_client_id() ->
    list_to_binary(io_lib:format("ws_~p_~p", 
                                 [erlang:phash2(make_ref()), 
                                  erlang:system_time(microsecond)])).

handle_client_message(#{<<"type">> := <<"ping">>}, State) ->
    %% Respond to ping
    Pong = jsx:encode(#{
        type => <<"pong">>,
        timestamp => erlang:system_time(second)
    }),
    {[{text, Pong}], State};

handle_client_message(#{<<"type">> := <<"subscribe">>, 
                       <<"filter">> := Filter}, State) ->
    %% TODO: Implement filtered subscriptions
    Ack = jsx:encode(#{
        type => <<"subscribed">>,
        filter => Filter
    }),
    {[{text, Ack}], State};

handle_client_message(#{<<"type">> := <<"get_crash">>, 
                       <<"crash_id">> := CrashId}, State) ->
    %% Fetch specific crash details
    case crash_report_processor:get_crash_reports(#{id => CrashId}) of
        {ok, [Crash]} ->
            Msg = jsx:encode(#{
                type => <<"crash_details">>,
                crash => format_crash_for_ws(Crash)
            }),
            {[{text, Msg}], State};
        _ ->
            ErrorMsg = jsx:encode(#{
                type => <<"error">>,
                message => <<"Crash not found">>
            }),
            {[{text, ErrorMsg}], State}
    end;

handle_client_message(_, State) ->
    %% Unknown message type
    {ok, State}.

format_crash_event({new_crash, CrashReport}) ->
    jsx:encode(#{
        type => <<"new_crash">>,
        crash => format_crash_for_ws(CrashReport),
        timestamp => erlang:system_time(second)
    });

format_crash_event({crash_update, CrashId, CrashReport}) ->
    jsx:encode(#{
        type => <<"crash_update">>,
        crash_id => CrashId,
        updates => format_crash_for_ws(CrashReport),
        timestamp => erlang:system_time(second)
    });

format_crash_event({analysis_complete, CrashId, Analysis}) ->
    jsx:encode(#{
        type => <<"analysis_complete">>,
        crash_id => CrashId,
        analysis => format_analysis_for_ws(Analysis),
        timestamp => erlang:system_time(second)
    });

format_crash_event({fixes_ready, CrashId, Fixes}) ->
    jsx:encode(#{
        type => <<"fixes_ready">>,
        crash_id => CrashId,
        fixes => [format_fix_for_ws(F) || F <- Fixes],
        timestamp => erlang:system_time(second)
    }).

format_crash_for_ws({crash_report, Id, Timestamp, ProcessName, Pid, ErrorType, 
                    Reason, Module, Function, Line, Stacktrace, _State, 
                    Severity, Frequency, LastOccurrence, Status}) ->
    #{
        id => Id,
        timestamp => format_timestamp(Timestamp),
        process_name => ProcessName,
        pid => list_to_binary(pid_to_list(Pid)),
        error_type => iolist_to_binary(io_lib:format("~p", [ErrorType])),
        reason => iolist_to_binary(io_lib:format("~p", [Reason])),
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

format_analysis_for_ws({analysis_result, CrashId, RootCause, Impact, 
                       RelatedIssues, Confidence, SuggestedActions}) ->
    #{
        crash_id => CrashId,
        root_cause => RootCause,
        impact => Impact,
        related_issues => RelatedIssues,
        confidence => Confidence,
        suggested_actions => SuggestedActions
    }.

format_fix_for_ws({fix_proposal, Id, CrashId, Description, CodeChanges, 
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