-module(colored_logger_handler).
-export([log/2, adding_handler/1, removing_handler/1, changing_config/2, install/0]).

%% Logger handler callbacks
adding_handler(Config) ->
    {ok, Config}.

removing_handler(_Config) ->
    %% Properly handle removal without causing errors
    ok.

changing_config(_OldConfig, NewConfig) ->
    {ok, NewConfig}.

%% Main log handler
log(#{level := Level, msg := Msg, meta := Meta} = LogEvent, _Config) ->
    try
        %% Filter out unwanted messages
        case should_ignore_message(Level, Msg, Meta) of
            true -> ok;
            false ->
                %% Only process if not from error tracking system to avoid loops
                case maps:get(domain, Meta, []) of
                    [error_tracking|_] -> ok;
                    _ ->
                        FormattedMsg = format_message(Level, Msg, Meta),
                        io:format("~s", [FormattedMsg]),
                        
                        %% Forward errors to error tracking system if available
                        case Level of
                            error -> maybe_track_error(LogEvent);
                            critical -> maybe_track_error(LogEvent);
                            alert -> maybe_track_error(LogEvent);
                            emergency -> maybe_track_error(LogEvent);
                            _ -> ok
                        end
                end
        end
    catch
        _:_ ->
            %% Fallback to simple format if formatting fails
            try
                io:format("~p: ~p~n", [Level, Msg])
            catch
                _:_ -> ok  %% Silently fail if even that doesn't work
            end
    end,
    ok.

%% Check if we should ignore certain messages
should_ignore_message(notice, {report, Report}, _Meta) when is_map(Report) ->
    %% Filter out OTP application notices 
    case maps:get(label, Report, undefined) of
        {application_controller, exit} -> true;
        {application_controller, start} -> true;
        {supervisor, start_error} -> false;  % Keep supervisor errors
        _ -> false
    end;
should_ignore_message(notice, {report, Report}, _Meta) when is_list(Report) ->
    %% Filter out OTP application notices 
    case proplists:get_value(label, Report) of
        {application_controller, exit} -> true;
        {application_controller, start} -> true;
        {supervisor, start_error} -> false;  % Keep supervisor errors
        _ -> false
    end;
should_ignore_message(info, {Format, _Args}, _Meta) when is_list(Format) ->
    %% Filter out specific info messages that are too verbose
    case Format of
        "[MCP_CLIENT] Attempting stdio connection: ~s ~p~n" -> true;
        "Running initial discovery~n" -> true;
        "All updates completed~n" -> true;
        "Running scheduled endpoint discovery~n" -> true;
        "Checking for model registry updates~n" -> true;
        "Model registry contains ~p models~n" -> true;
        "Discovered ~p endpoints: ~p~n" -> true;
        _ -> false
    end;
should_ignore_message(warning, {Format, _Args}, _Meta) when is_list(Format) ->
    %% Filter out specific warnings
    case Format of
        "Received response for unknown request: ~p~n" -> true;
        _ -> false
    end;
should_ignore_message(_Level, _Msg, _Meta) ->
    false.

%% Format the log message
format_message(Level, {report, Report}, Meta) ->
    Message = format_report(Report),
    format_log_line(Level, Message, Meta);
format_message(Level, {string, String}, Meta) ->
    format_log_line(Level, String, Meta);
format_message(Level, {Format, Args}, Meta) when is_list(Format) ->
    try
        Message = io_lib:format(Format, Args),
        format_log_line(Level, Message, Meta)
    catch
        _:_ ->
            %% If format fails, just show the format string
            format_log_line(Level, Format, Meta)
    end;
format_message(Level, Msg, Meta) ->
    format_log_line(Level, io_lib:format("~p", [Msg]), Meta).

%% Format a report
format_report(Report) when is_map(Report) ->
    case maps:get(label, Report, undefined) of
        undefined ->
            io_lib:format("~p", [Report]);
        Label ->
            io_lib:format("~s: ~p", [Label, maps:remove(label, Report)])
    end;
format_report(Report) when is_list(Report) ->
    case lists:keyfind(label, 1, Report) of
        {label, Label} ->
            io_lib:format("~s: ~p", [Label, proplists:delete(label, Report)]);
        false ->
            io_lib:format("~p", [Report])
    end;
format_report(Report) ->
    io_lib:format("~p", [Report]).

%% Format a single log line
format_log_line(Level, Message, Meta) ->
    Timestamp = format_timestamp(),
    LevelStr = format_level(Level),
    Module = get_module_name(Meta),
    
    %% Clean up the message - remove extra whitespace and newlines
    CleanMessage = string:trim(lists:flatten(Message)),
    
    %% Simple format with minimal coloring
    case application:get_env(agent_web, disable_colors, false) of
        true ->
            io_lib:format("~s ~s [~s] ~s~n", [Timestamp, LevelStr, Module, CleanMessage]);
        false ->
            ColoredLevel = color_level(Level, LevelStr),
            io_lib:format("~s ~s [~s] ~s~n", [Timestamp, ColoredLevel, Module, CleanMessage])
    end.

%% Get module name from metadata
get_module_name(#{mfa := {Module, _Function, _Arity}}) ->
    atom_to_list(Module);
get_module_name(#{file := File}) ->
    filename:basename(File, ".erl");
get_module_name(_) ->
    "unknown".

%% Format timestamp
format_timestamp() ->
    {_, {H, M, S}} = calendar:local_time(),
    io_lib:format("~2..0w:~2..0w:~2..0w", [H, M, S]).

%% Format level with fixed width
format_level(debug) -> "DEBUG";
format_level(info) -> "INFO ";
format_level(notice) -> "NOTE ";
format_level(warning) -> "WARN ";
format_level(error) -> "ERROR";
format_level(critical) -> "CRIT ";
format_level(alert) -> "ALERT";
format_level(emergency) -> "EMERG";
format_level(_) -> "???? ".

%% Apply minimal coloring to levels
color_level(error, Str) -> "\033[31m" ++ Str ++ "\033[0m";      % Red
color_level(critical, Str) -> "\033[31m" ++ Str ++ "\033[0m";   % Red
color_level(alert, Str) -> "\033[31m" ++ Str ++ "\033[0m";      % Red
color_level(emergency, Str) -> "\033[31m" ++ Str ++ "\033[0m";  % Red
color_level(warning, Str) -> "\033[33m" ++ Str ++ "\033[0m";    % Yellow
color_level(notice, Str) -> "\033[32m" ++ Str ++ "\033[0m";     % Green
color_level(info, Str) -> "\033[36m" ++ Str ++ "\033[0m";       % Cyan
color_level(debug, Str) -> "\033[90m" ++ Str ++ "\033[0m";      % Gray
color_level(_, Str) -> Str.

%% Forward error to tracking system if available
maybe_track_error(#{level := Level, msg := Msg, meta := Meta}) ->
    case whereis(error_tracking_system) of
        undefined -> ok;
        _Pid ->
            try
                Context = #{
                    level => Level,
                    meta => Meta,
                    timestamp => erlang:system_time(microsecond)
                },
                error_tracking_system:log_error(Msg, Context)
            catch
                _:_ -> ok
            end
    end.

%% Install the handler
install() ->
    %% Remove default handler safely
    try
        logger:remove_handler(default)
    catch
        _:_ -> ok
    end,
    
    %% Remove any existing colored_logger handler
    try
        logger:remove_handler(colored_logger)
    catch
        _:_ -> ok
    end,
    
    %% Add our custom handler
    try
        logger:add_handler(colored_logger, ?MODULE, #{
            level => all,
            config => #{disable_colors => false}
        }),
        colored_logger:success("Colored logger handler installed", []),
        ok
    catch
        error:Reason ->
            io:format("Failed to install colored logger: ~p~n", [Reason]),
            {error, Reason}
    end.