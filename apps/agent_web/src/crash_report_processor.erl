-module(crash_report_processor).
-behaviour(gen_server).

%% API
-export([start_link/0,
         process_crash_report/1,
         get_crash_reports/0,
         get_crash_reports/1,
         get_crash_analysis/1,
         get_fix_proposals/1,
         subscribe/1,
         unsubscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    crash_reports = [] :: list(),
    analyses = #{} :: map(),
    fix_proposals = #{} :: map(),
    subscribers = [] :: list(pid()),
    ai_client :: pid() | undefined
}).

-record(crash_report, {
    id :: binary(),
    timestamp :: erlang:timestamp(),
    process_name :: atom() | undefined,
    pid :: pid(),
    error_type :: atom(),
    reason :: term(),
    module :: atom(),
    function :: atom(),
    line :: integer(),
    stacktrace :: list(),
    state :: term(),
    severity :: low | medium | high | critical,
    frequency :: integer(),
    last_occurrence :: erlang:timestamp(),
    status :: new | analyzing | analyzed | fixing | fixed
}).

-define(CRASH_TABLE, crash_reports).
-define(ANALYSIS_TABLE, crash_analyses).
-define(FIX_TABLE, fix_proposals).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process_crash_report(CrashData) ->
    gen_server:cast(?MODULE, {process_crash, CrashData}).

get_crash_reports() ->
    gen_server:call(?MODULE, get_all_reports).

get_crash_reports(Filter) ->
    gen_server:call(?MODULE, {get_reports, Filter}).

get_crash_analysis(CrashId) ->
    gen_server:call(?MODULE, {get_analysis, CrashId}).

get_fix_proposals(CrashId) ->
    gen_server:call(?MODULE, {get_fixes, CrashId}).

subscribe(Pid) ->
    gen_server:call(?MODULE, {subscribe, Pid}).

unsubscribe(Pid) ->
    gen_server:call(?MODULE, {unsubscribe, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Use persistent table manager for ETS tables
    persistent_table_manager:ensure_ets_table(?CRASH_TABLE, 
        [public, {keypos, #crash_report.id}], true),
    persistent_table_manager:ensure_ets_table(?ANALYSIS_TABLE, 
        [public], true),
    persistent_table_manager:ensure_ets_table(?FIX_TABLE, 
        [public], true),
    
    %% Set up error logger handler - comment out for now as it may be causing issues
    %% error_logger:add_report_handler(crash_report_handler, self()),
    
    %% Get the AI client from the registry (started by supervisor)
    AiPid = case whereis(crash_ai_analyzer) of
        undefined -> undefined;
        Pid -> Pid
    end,
    
    {ok, #state{ai_client = AiPid}}.

handle_call(get_all_reports, _From, State) ->
    Reports = ets:tab2list(?CRASH_TABLE),
    {reply, {ok, Reports}, State};

handle_call({get_reports, Filter}, _From, State) ->
    Reports = filter_reports(Filter),
    {reply, {ok, Reports}, State};

handle_call({get_analysis, CrashId}, _From, State) ->
    case ets:lookup(?ANALYSIS_TABLE, CrashId) of
        [{CrashId, Analysis}] -> {reply, {ok, Analysis}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call({get_fixes, CrashId}, _From, State) ->
    case ets:lookup(?FIX_TABLE, CrashId) of
        [{CrashId, Fixes}] -> {reply, {ok, Fixes}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call({subscribe, Pid}, _From, #state{subscribers = Subs} = State) ->
    monitor(process, Pid),
    {reply, ok, State#state{subscribers = [Pid | Subs]}};

handle_call({unsubscribe, Pid}, _From, #state{subscribers = Subs} = State) ->
    {reply, ok, State#state{subscribers = lists:delete(Pid, Subs)}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({process_crash, CrashData}, State) ->
    CrashReport = parse_crash_data(CrashData),
    
    %% Check if this is a duplicate
    case find_similar_crash(CrashReport) of
        {ok, ExistingId} ->
            update_crash_frequency(ExistingId),
            notify_subscribers({crash_update, ExistingId, CrashReport}, State);
        not_found ->
            ets:insert(?CRASH_TABLE, CrashReport),
            
            %% Trigger AI analysis
            crash_ai_analyzer:analyze(State#state.ai_client, CrashReport),
            
            notify_subscribers({new_crash, CrashReport}, State)
    end,
    
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({crash_analysis_complete, CrashId, Analysis}, State) ->
    ets:insert(?ANALYSIS_TABLE, {CrashId, Analysis}),
    
    %% Update crash status
    case ets:lookup(?CRASH_TABLE, CrashId) of
        [Crash] ->
            ets:insert(?CRASH_TABLE, Crash#crash_report{status = analyzed});
        _ -> ok
    end,
    
    %% Generate fix proposals
    crash_ai_analyzer:generate_fixes(State#state.ai_client, CrashId, Analysis),
    
    notify_subscribers({analysis_complete, CrashId, Analysis}, State),
    {noreply, State};

handle_info({fix_proposals_ready, CrashId, Fixes}, State) ->
    ets:insert(?FIX_TABLE, {CrashId, Fixes}),
    notify_subscribers({fixes_ready, CrashId, Fixes}, State),
    {noreply, State};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{subscribers = Subs} = State) ->
    {noreply, State#state{subscribers = lists:delete(Pid, Subs)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:delete_report_handler(crash_report_handler),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

parse_crash_data(CrashData) when is_map(CrashData) ->
    Id = generate_crash_id(),
    #crash_report{
        id = Id,
        timestamp = erlang:timestamp(),
        process_name = maps:get(registered_name, CrashData, undefined),
        pid = maps:get(pid, CrashData),
        error_type = maps:get(error_type, CrashData, unknown),
        reason = maps:get(reason, CrashData),
        module = extract_module(CrashData),
        function = extract_function(CrashData),
        line = extract_line(CrashData),
        stacktrace = maps:get(stacktrace, CrashData, []),
        state = maps:get(state, CrashData, undefined),
        severity = determine_severity(CrashData),
        frequency = 1,
        last_occurrence = erlang:timestamp(),
        status = new
    }.

extract_module(CrashData) ->
    case maps:get(stacktrace, CrashData, []) of
        [{Module, _, _, _} | _] -> Module;
        _ -> unknown
    end.

extract_function(CrashData) ->
    case maps:get(stacktrace, CrashData, []) of
        [{_, Function, _, _} | _] -> Function;
        _ -> unknown
    end.

extract_line(CrashData) ->
    case maps:get(stacktrace, CrashData, []) of
        [{_, _, _, Info} | _] -> 
            proplists:get_value(line, Info, 0);
        _ -> 0
    end.

determine_severity(CrashData) ->
    ProcessName = maps:get(registered_name, CrashData, undefined),
    ErrorType = maps:get(error_type, CrashData, unknown),
    
    %% Critical processes
    case ProcessName of
        mcp_registry -> critical;
        mcp_manager -> critical;
        agent_supervisor -> critical;
        _ ->
            case ErrorType of
                {badarg, _} -> high;
                {badmatch, _} -> medium;
                {case_clause, _} -> medium;
                {function_clause, _} -> medium;
                _ -> low
            end
    end.

find_similar_crash(#crash_report{module = Module, function = Function, line = Line}) ->
    case ets:match_object(?CRASH_TABLE, #crash_report{module = Module, 
                                                      function = Function, 
                                                      line = Line, 
                                                      _ = '_'}) of
        [#crash_report{id = Id} | _] -> {ok, Id};
        [] -> not_found
    end.

update_crash_frequency(CrashId) ->
    case ets:lookup(?CRASH_TABLE, CrashId) of
        [Crash] ->
            Updated = Crash#crash_report{
                frequency = Crash#crash_report.frequency + 1,
                last_occurrence = erlang:timestamp()
            },
            ets:insert(?CRASH_TABLE, Updated);
        _ -> ok
    end.

filter_reports(Filter) when is_map(Filter) ->
    AllReports = ets:tab2list(?CRASH_TABLE),
    lists:filter(fun(Report) -> matches_filter(Report, Filter) end, AllReports).

matches_filter(Report, Filter) ->
    lists:all(fun({Key, Value}) ->
        case Key of
            severity -> Report#crash_report.severity =:= Value;
            status -> Report#crash_report.status =:= Value;
            module -> Report#crash_report.module =:= Value;
            since -> Report#crash_report.timestamp >= Value;
            _ -> true
        end
    end, maps:to_list(Filter)).

generate_crash_id() ->
    list_to_binary(io_lib:format("crash_~p_~p", [erlang:phash2(make_ref()), 
                                                  erlang:system_time(microsecond)])).

notify_subscribers(Event, #state{subscribers = Subs}) ->
    [Pid ! {crash_report_event, Event} || Pid <- Subs].