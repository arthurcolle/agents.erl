-module(error_tracking_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    tracker_pid :: pid()
}).

init(TrackerPid) ->
    {ok, #state{tracker_pid = TrackerPid}}.

handle_event({error, _GL, {Pid, Format, Data}}, State) ->
    try
        Message = io_lib:format(Format, Data),
        State#state.tracker_pid ! {error_report, [
            {type, error},
            {pid, Pid},
            {error, Message},
            {module, unknown},
            {function, unknown}
        ]},
        {ok, State}
    catch
        _:_ -> {ok, State}
    end;

handle_event({error_report, _GL, {Pid, Type, Report}}, State) ->
    State#state.tracker_pid ! {error_report, [
        {type, Type},
        {pid, Pid},
        {error, Report},
        {module, unknown},
        {function, unknown}
    ]},
    {ok, State};

handle_event({warning_msg, _GL, {Pid, Format, Data}}, State) ->
    try
        Message = io_lib:format(Format, Data),
        State#state.tracker_pid ! {error_report, [
            {type, warning},
            {pid, Pid},
            {error, Message},
            {module, unknown},
            {function, unknown}
        ]},
        {ok, State}
    catch
        _:_ -> {ok, State}
    end;

handle_event({warning_report, _GL, {Pid, Type, Report}}, State) ->
    State#state.tracker_pid ! {error_report, [
        {type, warning},
        {pid, Pid},
        {error, Report},
        {module, unknown},
        {function, unknown}
    ]},
    {ok, State};

handle_event({info_msg, _GL, _Info}, State) ->
    %% Ignore info messages to reduce noise
    {ok, State};

handle_event({info_report, _GL, _Report}, State) ->
    %% Ignore info reports to reduce noise
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, {error, not_implemented}, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.