-module(enhanced_logger).
-behaviour(gen_server).

%% Enhanced logging system with colors, structured logging, and log rotation
%% Provides centralized logging for the entire Agents.erl system

-export([start_link/0, log/3, log/4, log/5,
         error/2, error/3, warning/2, warning/3, 
         success/2, success/3, info/2, info/3, 
         debug/2, debug/3, health_check/2, health_check/3,
         api_call/3, api_call/4, critical/2, critical/3, 
         startup/2, startup/3, set_log_level/1, 
         get_log_stats/0, rotate_logs/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

%% Use colored_logger for advanced color support

-record(state, {
    log_level = info,
    log_file = "server.log",
    log_fd = undefined,
    stats = #{},
    max_file_size = 10485760, % 10MB
    max_files = 5,
    console_enabled = true,
    file_enabled = true
}).

-record(log_entry, {
    timestamp,
    level,
    module,
    component,
    message,
    metadata
}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Core logging functions
log(Level, Message, Args) ->
    log(Level, undefined, Message, Args, #{}).

log(Level, Component, Message, Args) ->
    log(Level, Component, Message, Args, #{}).

log(Level, Component, Message, Args, Metadata) ->
    Entry = #log_entry{
        timestamp = erlang:system_time(microsecond),
        level = Level,
        module = get_calling_module(),
        component = Component,
        message = format_message(Message, Args),
        metadata = Metadata
    },
    gen_server:cast(?MODULE, {log, Entry}).

%% Convenience functions
error(Message, Args) -> log(error, Message, Args).
error(Component, Message, Args) -> log(error, Component, Message, Args).

warning(Message, Args) -> log(warning, Message, Args).
warning(Component, Message, Args) -> log(warning, Component, Message, Args).

success(Message, Args) -> log(success, Message, Args).
success(Component, Message, Args) -> log(success, Component, Message, Args).

info(Message, Args) -> log(info, Message, Args).
info(Component, Message, Args) -> log(info, Component, Message, Args).

debug(Message, Args) -> log(debug, Message, Args).
debug(Component, Message, Args) -> log(debug, Component, Message, Args).

health_check(Message, Args) -> log(health_check, Message, Args).
health_check(Component, Message, Args) -> log(health_check, Component, Message, Args).

critical(Message, Args) -> log(critical, Message, Args).
critical(Component, Message, Args) -> log(critical, Component, Message, Args).

startup(Message, Args) -> log(startup, Message, Args).
startup(Component, Message, Args) -> log(startup, Component, Message, Args).

api_call(Method, Path, Duration) ->
    log(api_call, "API", "~s ~s completed in ~pms", [Method, Path, Duration]).

api_call(Component, Method, Path, Duration) ->
    log(api_call, Component, "~s ~s completed in ~pms", [Method, Path, Duration]).

%% Configuration functions
set_log_level(Level) ->
    gen_server:call(?MODULE, {set_log_level, Level}).

get_log_stats() ->
    gen_server:call(?MODULE, get_log_stats).

rotate_logs() ->
    gen_server:call(?MODULE, rotate_logs).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    
    % Open log file
    LogFile = application:get_env(agent_web, log_file, "server.log"),
    case file:open(LogFile, [write, append, {encoding, utf8}]) of
        {ok, Fd} ->
            % Schedule log rotation check
            erlang:send_after(300000, self(), check_log_rotation), % 5 minutes
            
            % Log startup
            Entry = #log_entry{
                timestamp = erlang:system_time(microsecond),
                level = startup,
                module = ?MODULE,
                component = "LOGGER",
                message = "Enhanced logger started",
                metadata = #{pid => self(), log_file => LogFile}
            },
            write_log_entry(Fd, Entry, true),
            
            {ok, #state{log_file = LogFile, log_fd = Fd}};
        {error, Reason} ->
            io:format("Failed to open log file ~s: ~p~n", [LogFile, Reason]),
            {ok, #state{log_file = LogFile, file_enabled = false}}
    end.

handle_call({set_log_level, Level}, _From, State) ->
    {reply, ok, State#state{log_level = Level}};

handle_call(get_log_stats, _From, State) ->
    {reply, State#state.stats, State};

handle_call(rotate_logs, _From, State) ->
    NewState = rotate_log_file(State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({log, Entry}, State) ->
    % Update statistics
    NewStats = update_stats(Entry, State#state.stats),
    
    % Check if we should log this entry
    ShouldLog = should_log(Entry#log_entry.level, State#state.log_level),
    
    case ShouldLog of
        true ->
            % Write to console if enabled
            case State#state.console_enabled of
                true -> write_console_entry(Entry);
                false -> ok
            end,
            
            % Write to file if enabled and file is open
            case {State#state.file_enabled, State#state.log_fd} of
                {true, Fd} when Fd =/= undefined ->
                    write_log_entry(Fd, Entry, false);
                _ -> ok
            end;
        false -> ok
    end,
    
    {noreply, State#state{stats = NewStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_log_rotation, State) ->
    NewState = maybe_rotate_logs(State),
    erlang:send_after(300000, self(), check_log_rotation), % Schedule next check
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.log_fd of
        undefined -> ok;
        Fd -> file:close(Fd)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

format_message(Message, []) -> Message;
format_message(Message, Args) -> io_lib:format(Message, Args).

get_calling_module() ->
    % Get the calling module from the stack trace
    case erlang:process_info(self(), current_stacktrace) of
        {current_stacktrace, [{?MODULE, _, _, _} | [{Module, _, _, _} | _]]} -> Module;
        _ -> unknown
    end.

should_log(Level, ConfigLevel) ->
    LevelNum = level_to_num(Level),
    ConfigNum = level_to_num(ConfigLevel),
    LevelNum >= ConfigNum.

level_to_num(debug) -> 1;
level_to_num(info) -> 2;
level_to_num(success) -> 2;
level_to_num(health_check) -> 2;
level_to_num(api_call) -> 2;
level_to_num(startup) -> 2;
level_to_num(warning) -> 3;
level_to_num(error) -> 4;
level_to_num(critical) -> 5;
level_to_num(_) -> 2.

apply_enhanced_color(Level, Text) ->
    case Level of
        error -> colored_logger:alarm(high, Text);
        warning -> colored_logger:alarm(medium, Text);
        success -> colored_logger:complete(success, Text);
        startup -> colored_logger:system(cpu, Text);
        health_check -> colored_logger:system(memory, Text);
        api_call -> colored_logger:network(connected, Text);
        critical -> colored_logger:fire(inferno, Text);
        info -> colored_logger:data(processed, Text);
        debug -> colored_logger:development(debugging, Text);
        neural -> colored_logger:neural(high, Text);
        quantum -> colored_logger:quantum(entangled, Text);
        matrix -> colored_logger:matrix(high, Text);
        cosmic -> colored_logger:cosmic(supernova, Text);
        performance -> colored_logger:performance(fast, Text);
        security -> colored_logger:security(safe, Text);
        production -> colored_logger:production(stable, Text);
        _ -> Text
    end.

format_timestamp(Microseconds) ->
    Seconds = Microseconds div 1000000,
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(
        Seconds + calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})),
    Millisec = (Microseconds rem 1000000) div 1000,
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~3..0w", 
                 [Year, Month, Day, Hour, Min, Sec, Millisec]).

write_console_entry(Entry) ->
    Timestamp = format_timestamp(Entry#log_entry.timestamp),
    LevelStr = string:uppercase(atom_to_list(Entry#log_entry.level)),
    
    % Apply enhanced colors
    ColoredTimestamp = colored_logger:data(input, Timestamp),
    ColoredLevel = apply_enhanced_color(Entry#log_entry.level, "[" ++ LevelStr ++ "]"),
    
    ComponentStr = case Entry#log_entry.component of
        undefined -> "";
        Component -> colored_logger:system(network, "[" ++ binary_to_list(iolist_to_binary(Component)) ++ "] ")
    end,
    
    ModuleStr = case Entry#log_entry.module of
        unknown -> "";
        Module -> colored_logger:development(coding, "[" ++ atom_to_list(Module) ++ "] ")
    end,
    
    ColoredMessage = apply_enhanced_color(Entry#log_entry.level, Entry#log_entry.message),
    
    % Add special effects for certain levels
    FinalMessage = case Entry#log_entry.level of
        critical -> colored_logger:celebration(fireworks, ColoredMessage);
        startup -> colored_logger:progress(completing, ColoredMessage);
        success -> colored_logger:celebration(party, ColoredMessage);
        _ -> ColoredMessage
    end,
    
    io:format("~s ~s ~s~s~s~n", 
              [ColoredTimestamp, ColoredLevel, ComponentStr, ModuleStr, FinalMessage]).

write_log_entry(Fd, Entry, Force) ->
    case {Fd, Force} of
        {undefined, false} -> ok;
        _ ->
            Timestamp = format_timestamp(Entry#log_entry.timestamp),
            LevelStr = string:uppercase(atom_to_list(Entry#log_entry.level)),
            
            ComponentStr = case Entry#log_entry.component of
                undefined -> "";
                Component -> io_lib:format("[~s] ", [Component])
            end,
            
            ModuleStr = case Entry#log_entry.module of
                unknown -> "";
                Module -> io_lib:format("[~s] ", [Module])
            end,
            
            LogLine = io_lib:format("~s [~s] ~s~s~s~n", 
                                   [Timestamp, LevelStr, ComponentStr, ModuleStr, 
                                    Entry#log_entry.message]),
            file:write(Fd, LogLine)
    end.

update_stats(Entry, Stats) ->
    Level = Entry#log_entry.level,
    LevelKey = {level, Level},
    ComponentKey = case Entry#log_entry.component of
        undefined -> {component, system};
        Component -> {component, Component}
    end,
    
    Stats1 = maps:update_with(LevelKey, fun(X) -> X + 1 end, 1, Stats),
    Stats2 = maps:update_with(ComponentKey, fun(X) -> X + 1 end, 1, Stats1),
    maps:update_with(total_logs, fun(X) -> X + 1 end, 1, Stats2).

maybe_rotate_logs(State) ->
    case {State#state.file_enabled, State#state.log_fd} of
        {true, Fd} when Fd =/= undefined ->
            case file:position(Fd, cur) of
                {ok, Size} when Size > State#state.max_file_size ->
                    rotate_log_file(State);
                _ -> State
            end;
        _ -> State
    end.

rotate_log_file(State) ->
    case State#state.log_fd of
        undefined -> State;
        Fd ->
            file:close(Fd),
            
            % Rotate log files
            LogFile = State#state.log_file,
            rotate_file_sequence(LogFile, State#state.max_files),
            
            % Open new log file
            case file:open(LogFile, [write, {encoding, utf8}]) of
                {ok, NewFd} ->
                    Entry = #log_entry{
                        timestamp = erlang:system_time(microsecond),
                        level = info,
                        module = ?MODULE,
                        component = "LOGGER",
                        message = "Log rotated",
                        metadata = #{}
                    },
                    write_log_entry(NewFd, Entry, true),
                    State#state{log_fd = NewFd};
                {error, Reason} ->
                    io:format("Failed to open rotated log file: ~p~n", [Reason]),
                    State#state{log_fd = undefined, file_enabled = false}
            end
    end.

rotate_file_sequence(BaseFile, MaxFiles) ->
    % Move BaseFile.N-1 to BaseFile.N
    lists:foreach(fun(N) ->
        OldFile = BaseFile ++ "." ++ integer_to_list(N-1),
        NewFile = BaseFile ++ "." ++ integer_to_list(N),
        case filelib:is_regular(OldFile) of
            true -> file:rename(OldFile, NewFile);
            false -> ok
        end
    end, lists:seq(MaxFiles, 2, -1)),
    
    % Move BaseFile to BaseFile.1
    case filelib:is_regular(BaseFile) of
        true -> file:rename(BaseFile, BaseFile ++ ".1");
        false -> ok
    end.