-module(realtime_log_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0,
         add_log_source/2,
         remove_log_source/1,
         get_active_sources/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    sources = #{} :: map(),
    buffer = [] :: list(),
    buffer_timer :: reference() | undefined
}).

-record(log_source, {
    id :: binary(),
    type :: file | process | network,
    location :: term(),
    patterns :: list(binary()),
    active :: boolean()
}).

-define(BUFFER_TIMEOUT, 100). % milliseconds
-define(MAX_BUFFER_SIZE, 50).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_log_source(Id, Config) ->
    gen_server:call(?MODULE, {add_source, Id, Config}).

remove_log_source(Id) ->
    gen_server:call(?MODULE, {remove_source, Id}).

get_active_sources() ->
    gen_server:call(?MODULE, get_sources).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Set up standard error logger monitoring - comment out for now as it may be causing issues
    %% error_logger:add_report_handler(realtime_error_handler, self()),
    
    %% Monitor standard output/error
    start_stdio_monitor(),
    
    %% Monitor common log files
    add_default_log_sources(),
    
    {ok, #state{}}.

handle_call({add_source, Id, Config}, _From, State) ->
    Source = create_log_source(Id, Config),
    NewSources = maps:put(Id, Source, State#state.sources),
    start_monitoring_source(Source),
    {reply, ok, State#state{sources = NewSources}};

handle_call({remove_source, Id}, _From, State) ->
    case maps:find(Id, State#state.sources) of
        {ok, Source} ->
            stop_monitoring_source(Source),
            NewSources = maps:remove(Id, State#state.sources),
            {reply, ok, State#state{sources = NewSources}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_sources, _From, State) ->
    Sources = maps:values(State#state.sources),
    {reply, {ok, Sources}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({log_line, Line}, State) ->
    %% Add to buffer
    NewBuffer = [Line | State#state.buffer],
    
    %% Check if we should flush
    NewState = case length(NewBuffer) >= ?MAX_BUFFER_SIZE of
        true ->
            flush_buffer(NewBuffer),
            State#state{buffer = []};
        false ->
            %% Set/reset timer
            Timer = case State#state.buffer_timer of
                undefined ->
                    erlang:send_after(?BUFFER_TIMEOUT, self(), flush_buffer);
                OldTimer ->
                    erlang:cancel_timer(OldTimer),
                    erlang:send_after(?BUFFER_TIMEOUT, self(), flush_buffer)
            end,
            State#state{buffer = NewBuffer, buffer_timer = Timer}
    end,
    
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush_buffer, State) ->
    case State#state.buffer of
        [] -> 
            {noreply, State#state{buffer_timer = undefined}};
        Buffer ->
            flush_buffer(Buffer),
            {noreply, State#state{buffer = [], buffer_timer = undefined}}
    end;

handle_info({file_monitor, File, Line}, State) ->
    %% Log line from file monitoring
    process_log_line(Line, File),
    {noreply, State};

handle_info({io_request, _, _, {put_chars, _, Chars}}, State) ->
    %% Captured IO output
    Line = iolist_to_binary(Chars),
    process_log_line(Line, stdio),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Stop all monitoring
    maps:fold(fun(_, Source, _) ->
        stop_monitoring_source(Source)
    end, ok, State#state.sources),
    
    %% Remove error handler
    error_logger:delete_report_handler(realtime_error_handler),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

create_log_source(Id, Config) ->
    #log_source{
        id = Id,
        type = maps:get(type, Config, file),
        location = maps:get(location, Config),
        patterns = maps:get(patterns, Config, []),
        active = true
    }.

start_monitoring_source(#log_source{type = file, location = Path} = Source) ->
    %% Start file tail process
    spawn_link(fun() -> tail_file(Path, Source) end);

start_monitoring_source(#log_source{type = process, location = Pid}) ->
    %% Monitor process output
    erlang:trace(Pid, true, [send, 'receive', procs]);

start_monitoring_source(_) ->
    ok.

stop_monitoring_source(#log_source{type = file, id = Id}) ->
    %% Stop file monitoring - send stop signal to file monitor process
    %% Note: Would need to track PIDs for proper cleanup
    ok;

stop_monitoring_source(#log_source{type = process, location = Pid}) ->
    %% Stop process tracing
    erlang:trace(Pid, false, [send, 'receive', procs]);

stop_monitoring_source(_) ->
    ok.

tail_file(Path, Source) ->
    case file:open(Path, [read, raw, binary]) of
        {ok, File} ->
            %% Seek to end
            file:position(File, eof),
            tail_loop(File, Source);
        {error, Reason} ->
            error_logger:error_msg("Failed to open log file ~s: ~p~n", [Path, Reason])
    end.

tail_loop(File, Source) ->
    case file:read_line(File) of
        {ok, Line} ->
            self() ! {file_monitor, Source#log_source.location, Line},
            tail_loop(File, Source);
        eof ->
            %% Wait and retry
            timer:sleep(100),
            tail_loop(File, Source);
        {error, _} ->
            file:close(File)
    end.

process_log_line(Line, Source) ->
    %% Quick filter for error-like content
    case is_error_related(Line) of
        true ->
            %% Send to AI interpreter
            ai_error_interpreter:interpret_log_line(Line),
            
            %% Also send to crash processor if it looks like a crash
            case is_crash_related(Line) of
                true -> process_potential_crash(Line, Source);
                false -> ok
            end;
        false ->
            %% Check if it matches any configured patterns
            ok
    end.

is_error_related(Line) ->
    ErrorPatterns = [
        <<"ERROR">>, <<"Error">>, <<"error">>,
        <<"FAILED">>, <<"Failed">>, <<"failed">>,
        <<"CRASH">>, <<"Crash">>, <<"crash">>,
        <<"EXCEPTION">>, <<"Exception">>, <<"exception">>,
        <<"TERMINATED">>, <<"Terminated">>, <<"terminated">>,
        <<"=ERROR REPORT===">>, <<"=CRASH REPORT===">>,
        <<"badarg">>, <<"badmatch">>, <<"badarith">>,
        <<"function_clause">>, <<"case_clause">>,
        <<"timeout">>, <<"noproc">>, <<"killed">>
    ],
    
    lists:any(fun(Pattern) ->
        binary:match(Line, Pattern) =/= nomatch
    end, ErrorPatterns).

is_crash_related(Line) ->
    CrashPatterns = [
        <<"=CRASH REPORT===">>,
        <<"=SUPERVISOR REPORT===">>,
        <<"crasher:">>,
        <<"exception error:">>,
        <<"Error in process">>,
        <<"** Generic server">>,
        <<"terminating">>
    ],
    
    lists:any(fun(Pattern) ->
        binary:match(Line, Pattern) =/= nomatch
    end, CrashPatterns).

process_potential_crash(Line, Source) ->
    %% Extract crash information if possible
    CrashData = #{
        source => Source,
        line => Line,
        timestamp => erlang:timestamp(),
        type => extract_crash_type(Line)
    },
    
    crash_report_processor:process_crash_report(CrashData).

extract_crash_type(Line) ->
    case binary:match(Line, <<"=CRASH REPORT===">>) of
        {_, _} -> crash_report;
        nomatch ->
            case binary:match(Line, <<"=SUPERVISOR REPORT===">>) of
                {_, _} -> supervisor_report;
                nomatch -> generic_error
            end
    end.

flush_buffer(Buffer) ->
    %% Process buffered lines
    lists:foreach(fun(Line) ->
        gen_server:cast(self(), {log_line, Line})
    end, lists:reverse(Buffer)).

start_stdio_monitor() ->
    %% Set up group leader to capture stdio
    OldGL = group_leader(),
    spawn_link(fun() -> stdio_monitor_loop(OldGL) end).

stdio_monitor_loop(RealGL) ->
    receive
        {io_request, From, ReplyAs, Request} = Msg ->
            %% Forward to real group leader
            RealGL ! Msg,
            
            %% Capture output
            case Request of
                {put_chars, _, _} -> self() ! Msg;
                _ -> ok
            end,
            
            stdio_monitor_loop(RealGL);
        _ ->
            stdio_monitor_loop(RealGL)
    end.

add_default_log_sources() ->
    %% Add common Erlang log locations
    LogDirs = [
        "/var/log/erlang",
        "log",
        "_build/default/rel/agent_web/log",
        "crash_dumps"
    ],
    
    lists:foreach(fun(Dir) ->
        case filelib:is_dir(Dir) of
            true -> add_log_directory(Dir);
            false -> ok
        end
    end, LogDirs).

add_log_directory(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            LogFiles = [filename:join(Dir, F) || F <- Files,
                        is_log_file(F)],
            lists:foreach(fun(File) ->
                Id = list_to_binary(File),
                add_log_source(Id, #{
                    type => file,
                    location => File,
                    patterns => []
                })
            end, LogFiles);
        _ ->
            ok
    end.

is_log_file(Filename) ->
    Extensions = [".log", ".err", ".out", ".crash"],
    lists:any(fun(Ext) ->
        lists:suffix(Ext, Filename)
    end, Extensions).