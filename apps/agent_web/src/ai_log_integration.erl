-module(ai_log_integration).
-behaviour(gen_server).

%% API
-export([start_link/0,
         enable_ai_analysis/0,
         disable_ai_analysis/0,
         configure/1,
         get_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Logger handler callbacks
-export([log/2, adding_handler/1, removing_handler/1, changing_config/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    enabled = false :: boolean(),
    timeout_analyzer :: pid() | undefined,
    error_interpreter :: pid() | undefined,
    config = #{} :: map(),
    stats = #{} :: map()
}).

-define(HANDLER_ID, ai_log_handler).
-define(DEFAULT_CONFIG, #{
    analyze_timeouts => true,
    analyze_errors => true,
    analyze_warnings => true,
    periodic_interval => 300000,  % 5 minutes
    batch_errors => true,
    batch_size => 50,
    min_severity => warning
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enable_ai_analysis() ->
    gen_server:call(?MODULE, enable).

disable_ai_analysis() ->
    gen_server:call(?MODULE, disable).

configure(Config) ->
    gen_server:call(?MODULE, {configure, Config}).

get_status() ->
    gen_server:call(?MODULE, get_status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize with default config
    Config = ?DEFAULT_CONFIG,
    
    %% Start dependent services
    State = start_services(#state{config = Config}),
    
    %% Initialize stats
    Stats = #{
        logs_processed => 0,
        timeouts_detected => 0,
        errors_analyzed => 0,
        ai_calls => 0,
        start_time => erlang:system_time(second)
    },
    
    {ok, State#state{stats = Stats}}.

handle_call(enable, _From, State) ->
    case State#state.enabled of
        true ->
            {reply, {ok, already_enabled}, State};
        false ->
            %% Add logger handler
            ok = logger:add_handler(?HANDLER_ID, ?MODULE, #{
                level => maps:get(min_severity, State#state.config, warning),
                config => State#state.config
            }),
            
            %% Enable periodic analysis if configured
            case State#state.timeout_analyzer of
                undefined -> ok;
                Pid ->
                    Interval = maps:get(periodic_interval, State#state.config, 300000),
                    ai_timeout_analyzer:enable_periodic_analysis(Interval)
            end,
            
            ?LOG_INFO("AI log analysis enabled", #{module => ?MODULE}),
            {reply, ok, State#state{enabled = true}}
    end;

handle_call(disable, _From, State) ->
    case State#state.enabled of
        false ->
            {reply, {ok, already_disabled}, State};
        true ->
            %% Remove logger handler
            logger:remove_handler(?HANDLER_ID),
            
            %% Disable periodic analysis
            case State#state.timeout_analyzer of
                undefined -> ok;
                Pid -> ai_timeout_analyzer:disable_periodic_analysis()
            end,
            
            ?LOG_INFO("AI log analysis disabled", #{module => ?MODULE}),
            {reply, ok, State#state{enabled = false}}
    end;

handle_call({configure, NewConfig}, _From, State) ->
    %% Merge with existing config
    MergedConfig = maps:merge(State#state.config, NewConfig),
    
    %% Update logger handler if enabled
    case State#state.enabled of
        true ->
            logger:update_handler_config(?HANDLER_ID, config, MergedConfig);
        false ->
            ok
    end,
    
    %% Update periodic analysis interval if changed
    case maps:get(periodic_interval, NewConfig, undefined) of
        undefined -> ok;
        Interval when State#state.timeout_analyzer =/= undefined ->
            ai_timeout_analyzer:enable_periodic_analysis(Interval);
        _ -> ok
    end,
    
    {reply, ok, State#state{config = MergedConfig}};

handle_call(get_status, _From, State) ->
    Status = #{
        enabled => State#state.enabled,
        config => State#state.config,
        stats => State#state.stats,
        services => #{
            timeout_analyzer => State#state.timeout_analyzer =/= undefined,
            error_interpreter => State#state.error_interpreter =/= undefined
        }
    },
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({log_event, Event}, State) ->
    %% Process log event from logger handler
    NewStats = process_log_event(Event, State),
    {noreply, State#state{stats = NewStats}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Clean up logger handler if enabled
    case State#state.enabled of
        true -> logger:remove_handler(?HANDLER_ID);
        false -> ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Logger handler callbacks
%%====================================================================

log(#{level := Level, msg := Msg, meta := Meta} = LogEvent, _Config) ->
    %% Send to gen_server for processing
    gen_server:cast(?MODULE, {log_event, LogEvent}),
    ok.

adding_handler(Config) ->
    {ok, Config}.

removing_handler(_Config) ->
    ok.

changing_config(_SetOrUpdate, _OldConfig, NewConfig) ->
    {ok, NewConfig}.

%%====================================================================
%% Internal functions
%%====================================================================

start_services(State) ->
    %% Start or connect to timeout analyzer
    TimeoutAnalyzer = case whereis(ai_timeout_analyzer) of
        undefined ->
            case ai_timeout_analyzer:start_link() of
                {ok, Pid1} -> Pid1;
                _ -> undefined
            end;
        Pid1 -> Pid1
    end,
    
    %% Start or connect to error interpreter
    ErrorInterpreter = case whereis(ai_error_interpreter) of
        undefined ->
            case ai_error_interpreter:start_link() of
                {ok, Pid2} -> Pid2;
                _ -> undefined
            end;
        Pid2 -> Pid2
    end,
    
    State#state{
        timeout_analyzer = TimeoutAnalyzer,
        error_interpreter = ErrorInterpreter
    }.

process_log_event(#{level := Level, msg := Msg, meta := Meta} = Event, State) ->
    Config = State#state.config,
    Stats = State#state.stats,
    
    %% Update processed count
    NewStats = maps:update_with(logs_processed, fun(V) -> V + 1 end, 0, Stats),
    
    %% Check if we should analyze this log
    case should_analyze(Level, Config) of
        false -> 
            NewStats;
        true ->
            %% Detect and analyze different types of issues
            Stats1 = detect_and_analyze_timeout(Event, State, NewStats),
            Stats2 = detect_and_analyze_error(Event, State, Stats1),
            Stats2
    end.

should_analyze(Level, Config) ->
    MinLevel = maps:get(min_severity, Config, warning),
    logger:compare_levels(Level, MinLevel) >= 0.

detect_and_analyze_timeout(#{msg := Msg, meta := Meta} = Event, State, Stats) ->
    case State#state.config of
        #{analyze_timeouts := false} -> Stats;
        _ ->
            case is_timeout_event(Event) of
                {true, TimeoutInfo} ->
                    %% Send to timeout analyzer
                    case State#state.timeout_analyzer of
                        undefined -> ok;
                        Pid ->
                            ai_timeout_analyzer:analyze_timeout(TimeoutInfo, Meta),
                            maps:update_with(ai_calls, fun(V) -> V + 1 end, 0,
                                maps:update_with(timeouts_detected, fun(V) -> V + 1 end, 0, Stats))
                    end;
                false ->
                    Stats
            end
    end.

detect_and_analyze_error(#{level := Level, msg := Msg, meta := Meta} = Event, State, Stats) ->
    case State#state.config of
        #{analyze_errors := false} -> Stats;
        _ ->
            case is_significant_error(Level, Event) of
                true ->
                    %% Determine if we should use timeout analyzer or error interpreter
                    ErrorInfo = extract_error_info(Event),
                    
                    case should_batch_error(State#state.config) of
                        true ->
                            %% Send to timeout analyzer for batch processing
                            case State#state.timeout_analyzer of
                                undefined -> ok;
                                Pid ->
                                    ai_timeout_analyzer:analyze_significant_error(ErrorInfo, Meta)
                            end;
                        false ->
                            %% Send to error interpreter for immediate analysis
                            case State#state.error_interpreter of
                                undefined -> ok;
                                Pid ->
                                    ai_error_interpreter:interpret_error(ErrorInfo)
                            end
                    end,
                    
                    maps:update_with(errors_analyzed, fun(V) -> V + 1 end, 0, Stats);
                false ->
                    Stats
            end
    end.

is_timeout_event(#{msg := Msg} = Event) ->
    %% Check various timeout patterns
    MsgStr = format_msg_for_analysis(Msg),
    
    TimeoutPatterns = [
        {<<"gen_server .* timeout">>, gen_server_timeout},
        {<<"gen_statem .* timeout">>, gen_statem_timeout},
        {<<"call timeout">>, call_timeout},
        {<<"cast timeout">>, cast_timeout},
        {<<"TCP timeout">>, tcp_timeout},
        {<<"HTTP timeout">>, http_timeout},
        {<<"request timeout">>, request_timeout},
        {<<"deadline exceeded">>, deadline_exceeded},
        {<<"operation timed out">>, operation_timeout}
    ],
    
    case find_timeout_pattern(MsgStr, TimeoutPatterns) of
        {ok, Type} ->
            {true, #{
                type => Type,
                message => MsgStr,
                timestamp => erlang:system_time(microsecond),
                event => Event
            }};
        nomatch ->
            %% Check meta for timeout info
            case maps:get(timeout, maps:get(meta, Event, #{}), undefined) of
                undefined -> false;
                Timeout ->
                    {true, #{
                        type => meta_timeout,
                        timeout => Timeout,
                        message => MsgStr,
                        timestamp => erlang:system_time(microsecond),
                        event => Event
                    }}
            end
    end.

find_timeout_pattern(_Msg, []) ->
    nomatch;
find_timeout_pattern(Msg, [{Pattern, Type} | Rest]) ->
    case re:run(Msg, Pattern, [{capture, none}]) of
        match -> {ok, Type};
        nomatch -> find_timeout_pattern(Msg, Rest)
    end.

is_significant_error(Level, Event) when Level >= error ->
    true;
is_significant_error(warning, #{msg := Msg} = Event) ->
    %% Check if warning contains error indicators
    MsgStr = format_msg_for_analysis(Msg),
    ErrorIndicators = [<<"error">>, <<"failed">>, <<"crash">>, <<"exception">>, 
                       <<"terminated">>, <<"killed">>, <<"exit">>],
    lists:any(fun(Indicator) ->
        binary:match(MsgStr, Indicator) =/= nomatch
    end, ErrorIndicators);
is_significant_error(_, _) ->
    false.

extract_error_info(#{level := Level, msg := Msg, meta := Meta} = Event) ->
    #{
        level => Level,
        message => format_msg_for_analysis(Msg),
        meta => Meta,
        timestamp => erlang:system_time(microsecond),
        type => classify_error_type(Event)
    }.

classify_error_type(#{meta := #{error_type := Type}}) ->
    Type;
classify_error_type(#{msg := {report, #{error_type := Type}}}) ->
    Type;
classify_error_type(#{msg := {string, Str}}) ->
    %% Try to extract error type from string
    extract_error_type_from_string(Str);
classify_error_type(_) ->
    unknown.

extract_error_type_from_string(Str) when is_list(Str) ->
    extract_error_type_from_string(list_to_binary(Str));
extract_error_type_from_string(Str) when is_binary(Str) ->
    %% Common error type patterns
    Patterns = [
        {<<"badarg">>, badarg},
        {<<"badarith">>, badarith},
        {<<"badmatch">>, badmatch},
        {<<"function_clause">>, function_clause},
        {<<"case_clause">>, case_clause},
        {<<"if_clause">>, if_clause},
        {<<"undef">>, undef},
        {<<"noproc">>, noproc},
        {<<"killed">>, killed},
        {<<"normal">>, normal},
        {<<"shutdown">>, shutdown}
    ],
    
    case find_error_type_pattern(Str, Patterns) of
        {ok, Type} -> Type;
        nomatch -> unknown
    end.

find_error_type_pattern(_Str, []) ->
    nomatch;
find_error_type_pattern(Str, [{Pattern, Type} | Rest]) ->
    case binary:match(Str, Pattern) of
        nomatch -> find_error_type_pattern(Str, Rest);
        _ -> {ok, Type}
    end.

format_msg_for_analysis({string, Str}) ->
    iolist_to_binary(Str);
format_msg_for_analysis({report, Report}) ->
    iolist_to_binary(io_lib:format("~p", [Report]));
format_msg_for_analysis({Fmt, Args}) when is_list(Fmt) ->
    iolist_to_binary(io_lib:format(Fmt, Args));
format_msg_for_analysis(Msg) ->
    iolist_to_binary(io_lib:format("~p", [Msg])).

should_batch_error(Config) ->
    maps:get(batch_errors, Config, true).