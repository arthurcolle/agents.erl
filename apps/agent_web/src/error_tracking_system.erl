-module(error_tracking_system).
-behaviour(gen_server).

-export([start_link/0, log_error/1, log_error/2, get_errors/0, get_errors/1, clear_errors/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_error_summary/0, get_error_trends/0, subscribe/1, unsubscribe/1]).

-record(error_entry, {
    id :: binary(),
    timestamp :: erlang:timestamp(),
    module :: atom(),
    function :: atom(),
    line :: integer(),
    error_type :: atom(),
    message :: binary(),
    stacktrace :: list(),
    context :: map(),
    severity :: critical | error | warning | info,
    count :: integer()
}).

-record(state, {
    errors = [] :: [#error_entry{}],
    error_counts = #{} :: map(),
    subscribers = [] :: [pid()],
    max_errors = 1000 :: integer()
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log_error(Error) ->
    log_error(Error, #{}).

log_error(Error, Context) ->
    gen_server:cast(?MODULE, {log_error, Error, Context}).

get_errors() ->
    gen_server:call(?MODULE, get_errors).

get_errors(Limit) ->
    gen_server:call(?MODULE, {get_errors, Limit}).

get_error_summary() ->
    gen_server:call(?MODULE, get_error_summary).

get_error_trends() ->
    gen_server:call(?MODULE, get_error_trends).

clear_errors() ->
    gen_server:call(?MODULE, clear_errors).

subscribe(Pid) ->
    gen_server:call(?MODULE, {subscribe, Pid}).

unsubscribe(Pid) ->
    gen_server:call(?MODULE, {unsubscribe, Pid}).

%% gen_server callbacks
init([]) ->
    process_flag(trap_exit, true),
    
    %% Install custom error logger handler
    error_logger:add_report_handler(error_tracking_handler, self()),
    
    %% Start periodic cleanup
    timer:send_interval(60000, cleanup_old_errors),
    
    {ok, #state{}}.

handle_call(get_errors, _From, State = #state{errors = Errors}) ->
    {reply, format_errors(Errors), State};

handle_call({get_errors, Limit}, _From, State = #state{errors = Errors}) ->
    Limited = lists:sublist(Errors, Limit),
    {reply, format_errors(Limited), State};

handle_call(get_error_summary, _From, State = #state{error_counts = Counts}) ->
    Summary = #{
        total_errors => length(State#state.errors),
        error_types => maps:fold(fun(K, V, Acc) ->
            [Type | _] = binary:split(K, <<":">>),
            maps:update_with(Type, fun(Old) -> Old + V end, V, Acc)
        end, #{}, Counts),
        severity_breakdown => calculate_severity_breakdown(State#state.errors),
        recent_errors => format_errors(lists:sublist(State#state.errors, 5))
    },
    {reply, Summary, State};

handle_call(get_error_trends, _From, State = #state{errors = Errors}) ->
    Trends = calculate_trends(Errors),
    {reply, Trends, State};

handle_call(clear_errors, _From, State) ->
    {reply, ok, State#state{errors = [], error_counts = #{}}};

handle_call({subscribe, Pid}, _From, State = #state{subscribers = Subs}) ->
    erlang:monitor(process, Pid),
    {reply, ok, State#state{subscribers = [Pid | Subs]}};

handle_call({unsubscribe, Pid}, _From, State = #state{subscribers = Subs}) ->
    {reply, ok, State#state{subscribers = lists:delete(Pid, Subs)}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({log_error, Error, Context}, State) ->
    Entry = create_error_entry(Error, Context),
    NewState = add_error(Entry, State),
    notify_subscribers(Entry, NewState#state.subscribers),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_old_errors, State = #state{errors = Errors, max_errors = Max}) ->
    NewErrors = case length(Errors) > Max of
        true -> lists:sublist(Errors, Max);
        false -> Errors
    end,
    {noreply, State#state{errors = NewErrors}};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{subscribers = Subs}) ->
    {noreply, State#state{subscribers = lists:delete(Pid, Subs)}};

handle_info({error_report, Report}, State) ->
    Entry = create_error_entry_from_report(Report),
    NewState = add_error(Entry, State),
    notify_subscribers(Entry, NewState#state.subscribers),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:delete_report_handler(error_tracking_handler),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
create_error_entry({error, {Module, Function, Args}}, Context) when is_atom(Module), is_atom(Function) ->
    create_error_entry({error, {Module, Function, length(Args)}, "Function call error"}, Context);

create_error_entry({error, {M, F, A}, Reason}, Context) ->
    #error_entry{
        id = generate_error_id(),
        timestamp = os:timestamp(),
        module = M,
        function = F,
        line = maps:get(line, Context, 0),
        error_type = error,
        message = format_reason(Reason),
        stacktrace = maps:get(stacktrace, Context, []),
        context = Context,
        severity = determine_severity(error, Context),
        count = 1
    };

create_error_entry({Type, Reason}, Context) ->
    {M, F, A} = get_caller_mfa(),
    #error_entry{
        id = generate_error_id(),
        timestamp = os:timestamp(),
        module = M,
        function = F,
        line = maps:get(line, Context, 0),
        error_type = Type,
        message = format_reason(Reason),
        stacktrace = maps:get(stacktrace, Context, []),
        context = Context,
        severity = determine_severity(Type, Context),
        count = 1
    };

create_error_entry(Error, Context) ->
    {M, F, A} = get_caller_mfa(),
    #error_entry{
        id = generate_error_id(),
        timestamp = os:timestamp(),
        module = M,
        function = F,
        line = maps:get(line, Context, 0),
        error_type = unknown,
        message = format_reason(Error),
        stacktrace = maps:get(stacktrace, Context, []),
        context = Context,
        severity = determine_severity(unknown, Context),
        count = 1
    }.

create_error_entry_from_report(Report) ->
    #error_entry{
        id = generate_error_id(),
        timestamp = os:timestamp(),
        module = proplists:get_value(module, Report, unknown),
        function = proplists:get_value(function, Report, unknown),
        line = proplists:get_value(line, Report, 0),
        error_type = proplists:get_value(type, Report, error),
        message = list_to_binary(io_lib:format("~p", [proplists:get_value(error, Report, unknown)])),
        stacktrace = proplists:get_value(stacktrace, Report, []),
        context = #{},
        severity = error,
        count = 1
    }.

add_error(Entry = #error_entry{module = M, function = F, error_type = Type}, 
          State = #state{errors = Errors, error_counts = Counts}) ->
    Key = iolist_to_binary([atom_to_binary(Type, utf8), ":", 
                           atom_to_binary(M, utf8), ":", 
                           atom_to_binary(F, utf8)]),
    NewCounts = maps:update_with(Key, fun(V) -> V + 1 end, 1, Counts),
    State#state{
        errors = [Entry | Errors],
        error_counts = NewCounts
    }.

format_errors(Errors) ->
    [format_error(E) || E <- Errors].

format_error(#error_entry{
    id = Id,
    timestamp = Timestamp,
    module = Module,
    function = Function,
    line = Line,
    error_type = Type,
    message = Message,
    stacktrace = Stacktrace,
    context = Context,
    severity = Severity,
    count = Count
}) ->
    #{
        id => Id,
        timestamp => format_timestamp(Timestamp),
        location => #{
            module => Module,
            function => Function,
            line => Line
        },
        error_type => Type,
        message => Message,
        stacktrace => format_stacktrace(Stacktrace),
        context => Context,
        severity => Severity,
        count => Count
    }.

format_timestamp({MegaSecs, Secs, MicroSecs}) ->
    {{Y, M, D}, {H, Min, S}} = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ",
                                  [Y, M, D, H, Min, S, MicroSecs])).

format_stacktrace(Stacktrace) when is_list(Stacktrace) ->
    [format_stack_entry(Entry) || Entry <- Stacktrace];
format_stacktrace(_) ->
    [].

format_stack_entry({M, F, A, Location}) when is_list(Location) ->
    #{
        module => M,
        function => F,
        arity => if is_list(A) -> length(A); true -> A end,
        file => proplists:get_value(file, Location, <<"unknown">>),
        line => proplists:get_value(line, Location, 0)
    };
format_stack_entry({M, F, A}) ->
    #{
        module => M,
        function => F,
        arity => if is_list(A) -> length(A); true -> A end,
        file => <<"unknown">>,
        line => 0
    };
format_stack_entry(_) ->
    #{}.

format_reason(Reason) when is_binary(Reason) ->
    Reason;
format_reason(Reason) when is_list(Reason) ->
    list_to_binary(Reason);
format_reason(Reason) ->
    list_to_binary(io_lib:format("~p", [Reason])).

determine_severity(error, _) -> error;
determine_severity(exit, Context) ->
    case maps:get(reason, Context, normal) of
        normal -> info;
        shutdown -> info;
        {shutdown, _} -> info;
        _ -> error
    end;
determine_severity(throw, _) -> warning;
determine_severity(_, _) -> error.

calculate_severity_breakdown(Errors) ->
    lists:foldl(fun(#error_entry{severity = Sev}, Acc) ->
        maps:update_with(Sev, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Errors).

calculate_trends(Errors) ->
    Now = os:timestamp(),
    OneHourAgo = subtract_time(Now, 3600),
    OneDayAgo = subtract_time(Now, 86400),
    
    #{
        last_hour => count_errors_since(Errors, OneHourAgo),
        last_day => count_errors_since(Errors, OneDayAgo),
        error_rate => calculate_error_rate(Errors, Now)
    }.

subtract_time({MegaSecs, Secs, MicroSecs}, SecondsDiff) ->
    TotalSecs = MegaSecs * 1000000 + Secs - SecondsDiff,
    NewMegaSecs = TotalSecs div 1000000,
    NewSecs = TotalSecs rem 1000000,
    {NewMegaSecs, NewSecs, MicroSecs}.

count_errors_since(Errors, Since) ->
    length([E || E <- Errors, E#error_entry.timestamp >= Since]).

calculate_error_rate([], _) -> 0.0;
calculate_error_rate(Errors, Now) ->
    OneMinuteAgo = subtract_time(Now, 60),
    RecentErrors = count_errors_since(Errors, OneMinuteAgo),
    RecentErrors / 60.0.  % Errors per second

get_caller_mfa() ->
    case erlang:process_info(self(), current_stacktrace) of
        {current_stacktrace, [_, _, {M, F, A, _} | _]} -> {M, F, A};
        {current_stacktrace, [_, _, {M, F, A} | _]} -> {M, F, A};
        _ -> {unknown, unknown, 0}
    end.

generate_error_id() ->
    %% Generate a unique ID using timestamp and random number
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    Random = rand:uniform(1000000),
    iolist_to_binary(io_lib:format("~p-~p-~p-~p", [MegaSecs, Secs, MicroSecs, Random])).

notify_subscribers(Entry, Subscribers) ->
    Msg = {error_logged, format_error(Entry)},
    [Pid ! Msg || Pid <- Subscribers].