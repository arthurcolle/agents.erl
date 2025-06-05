-module(simple_logger).
-export([install/0, log/2, log/3, format_event/2]).

%% Install the simple logger handler
install() ->
    logger:remove_handler(default),
    logger:add_handler(simple_logger, ?MODULE, #{
        level => all,
        config => #{
            disable_colors => application:get_env(agent_web, disable_colors, false)
        }
    }),
    ok.

%% Log event formatter - called by the logger system
format_event(#{level := Level, msg := {report, Report}, meta := Meta}, _) ->
    format_log(Level, Report, Meta);
format_event(#{level := Level, msg := {string, Msg}, meta := Meta}, _) ->
    format_log(Level, Msg, Meta);
format_event(#{level := Level, msg := {Fmt, Args}, meta := Meta}, _) ->
    Msg = io_lib:format(Fmt, Args),
    format_log(Level, Msg, Meta).

%% Format a log entry
format_log(Level, Message, Meta) ->
    Timestamp = format_timestamp(),
    LevelStr = format_level(Level),
    Module = maps:get(mfa, Meta, {unknown, unknown, 0}),
    ModuleName = case Module of
        {M, _F, _A} -> atom_to_list(M);
        _ -> "unknown"
    end,
    
    %% Simple, readable format without unicode or complex formatting
    case application:get_env(agent_web, disable_colors, false) of
        true ->
            io_lib:format("~s ~s [~s] ~s~n", [Timestamp, LevelStr, ModuleName, Message]);
        false ->
            %% Minimal coloring - only critical levels
            ColoredLevel = case Level of
                error -> "\033[31m" ++ LevelStr ++ "\033[0m";     % Red
                critical -> "\033[31m" ++ LevelStr ++ "\033[0m";  % Red
                warning -> "\033[33m" ++ LevelStr ++ "\033[0m";   % Yellow
                notice -> "\033[32m" ++ LevelStr ++ "\033[0m";    % Green
                _ -> LevelStr
            end,
            io_lib:format("~s ~s [~s] ~s~n", [Timestamp, ColoredLevel, ModuleName, Message])
    end.

%% Format timestamp as HH:MM:SS
format_timestamp() ->
    {_, {H, M, S}} = calendar:local_time(),
    io_lib:format("~2..0w:~2..0w:~2..0w", [H, M, S]).

%% Format log level with fixed width
format_level(debug) -> "DEBUG";
format_level(info) -> "INFO ";
format_level(notice) -> "NOTE ";
format_level(warning) -> "WARN ";
format_level(error) -> "ERROR";
format_level(critical) -> "CRIT ";
format_level(alert) -> "ALERT";
format_level(emergency) -> "EMERG";
format_level(_) -> "UNKN ".

%% Simple log functions for direct use
log(Level, Message) ->
    logger:log(Level, Message).

log(Level, Format, Args) ->
    logger:log(Level, Format, Args).