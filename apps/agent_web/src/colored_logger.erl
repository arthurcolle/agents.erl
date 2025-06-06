-module(colored_logger).
-export([log/3, log/4, error/2, warning/2, success/2, info/2, debug/2, 
         health_check/2, api_call/3, critical/2, startup/2, 
         format_colored/3, get_color_code/1, colorize/2, rgb_color/2, true_color/2,
         rainbow/1, gradient/3, blink/1, bold/1, italic/1, underline/1,
         neural/2, quantum/2, matrix/2, cosmic/2, fire/2, ocean/2, forest/2,
         celebration/2, alarm/2, system/2, data/2, progress/2, complete/2,
         performance/2, network/2, security/2, development/2, production/2,
         set_timestamp_format/1, get_timestamp_format/0,
         error/1, warning/1, info/1, notice/1, user/2, fire/3, ocean/3, data/3, success/1]).

%% Enhanced color support using erlang-color library
%% Color mapping for different log levels and types
get_color_code(error) -> error;
get_color_code(warning) -> warning;
get_color_code(success) -> success;
get_color_code(startup) -> startup;
get_color_code(health_check) -> health_check;
get_color_code(api_call) -> api_call;
get_color_code(critical) -> critical;
get_color_code(info) -> info;
get_color_code(debug) -> debug;
get_color_code(neural) -> neural;
get_color_code(quantum) -> quantum;
get_color_code(matrix) -> matrix;
get_color_code(cosmic) -> cosmic;
get_color_code(fire) -> fire;
get_color_code(ocean) -> ocean;
get_color_code(forest) -> forest;
get_color_code(celebration) -> celebration;
get_color_code(alarm) -> alarm;
get_color_code(system) -> system;
get_color_code(data) -> data;
get_color_code(progress) -> progress;
get_color_code(complete) -> complete;
get_color_code(performance) -> performance;
get_color_code(network) -> network;
get_color_code(security) -> security;
get_color_code(development) -> development;
get_color_code(production) -> production;
get_color_code(_) -> info.

%% ANSI color codes
-define(RESET, "\033[0m").
-define(BOLD, "\033[1m").
-define(DIM, "\033[2m").
-define(BLINK, "\033[5m").

%% Text colors
-define(BLACK, "\033[30m").
-define(RED, "\033[31m").
-define(GREEN, "\033[32m").
-define(YELLOW, "\033[33m").
-define(BLUE, "\033[34m").
-define(MAGENTA, "\033[35m").
-define(CYAN, "\033[36m").
-define(WHITE, "\033[37m").
-define(BRIGHT_BLACK, "\033[90m").
-define(BRIGHT_RED, "\033[91m").
-define(BRIGHT_GREEN, "\033[92m").
-define(BRIGHT_YELLOW, "\033[93m").
-define(BRIGHT_BLUE, "\033[94m").
-define(BRIGHT_MAGENTA, "\033[95m").
-define(BRIGHT_CYAN, "\033[96m").
-define(BRIGHT_WHITE, "\033[97m").

%% Background colors
-define(BG_BLACK, "\033[40m").
-define(BG_RED, "\033[41m").
-define(BG_GREEN, "\033[42m").
-define(BG_YELLOW, "\033[43m").
-define(BG_BLUE, "\033[44m").
-define(BG_MAGENTA, "\033[45m").
-define(BG_CYAN, "\033[46m").
-define(BG_WHITE, "\033[47m").

%% Helper to apply color based on level
apply_color(error, Text) -> ?RED ++ ?BOLD ++ Text ++ ?RESET;
apply_color(warning, Text) -> ?YELLOW ++ ?BOLD ++ Text ++ ?RESET;
apply_color(success, Text) -> ?GREEN ++ ?BOLD ++ Text ++ ?RESET;
apply_color(startup, Text) -> ?BG_GREEN ++ ?WHITE ++ ?BOLD ++ Text ++ ?RESET;
apply_color(health_check, Text) -> ?BLUE ++ ?BOLD ++ Text ++ ?RESET;
apply_color(api_call, Text) -> ?CYAN ++ ?BOLD ++ Text ++ ?RESET;
apply_color(critical, Text) -> ?BG_RED ++ ?WHITE ++ ?BOLD ++ ?BLINK ++ Text ++ ?RESET;
apply_color(info, Text) -> ?WHITE ++ Text ++ ?RESET;
apply_color(debug, Text) -> ?BRIGHT_BLACK ++ Text ++ ?RESET;
apply_color(neural, Text) -> ?MAGENTA ++ ?BOLD ++ Text ++ ?RESET;
apply_color(quantum, Text) -> ?BG_BLUE ++ ?WHITE ++ ?BOLD ++ Text ++ ?RESET;
apply_color(matrix, Text) -> ?BG_BLACK ++ ?GREEN ++ ?BOLD ++ Text ++ ?RESET;
apply_color(cosmic, Text) -> ?BG_MAGENTA ++ ?WHITE ++ ?BOLD ++ Text ++ ?RESET;
apply_color(fire, Text) -> ?RED ++ ?BG_YELLOW ++ ?BOLD ++ Text ++ ?RESET;
apply_color(ocean, Text) -> ?BLUE ++ ?BG_CYAN ++ ?BOLD ++ Text ++ ?RESET;
apply_color(forest, Text) -> ?GREEN ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
apply_color(celebration, Text) -> ?YELLOW ++ ?BG_MAGENTA ++ ?BOLD ++ ?BLINK ++ Text ++ ?RESET;
apply_color(alarm, Text) -> ?RED ++ ?BG_WHITE ++ ?BOLD ++ ?BLINK ++ Text ++ ?RESET;
apply_color(system, Text) -> ?CYAN ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
apply_color(data, Text) -> ?BLUE ++ ?BG_WHITE ++ ?BOLD ++ Text ++ ?RESET;
apply_color(progress, Text) -> ?YELLOW ++ ?BG_GREEN ++ ?BOLD ++ Text ++ ?RESET;
apply_color(complete, Text) -> ?WHITE ++ ?BG_GREEN ++ ?BOLD ++ Text ++ ?RESET;
apply_color(performance, Text) -> ?MAGENTA ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
apply_color(network, Text) -> ?CYAN ++ ?BG_BLUE ++ ?BOLD ++ Text ++ ?RESET;
apply_color(security, Text) -> ?RED ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
apply_color(development, Text) -> ?YELLOW ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
apply_color(production, Text) -> ?GREEN ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
apply_color(_, Text) -> Text.

%% New functions for advanced coloring
colorize(Color, Text) when is_atom(Color) ->
    case Color of
        red -> ?RED ++ Text ++ ?RESET;
        green -> ?GREEN ++ Text ++ ?RESET;
        blue -> ?BLUE ++ Text ++ ?RESET;
        yellow -> ?YELLOW ++ Text ++ ?RESET;
        cyan -> ?CYAN ++ Text ++ ?RESET;
        magenta -> ?MAGENTA ++ Text ++ ?RESET;
        white -> ?WHITE ++ Text ++ ?RESET;
        black -> ?BLACK ++ Text ++ ?RESET;
        _ -> Text
    end.

%% RGB color support (xterm 256 colors)
rgb_color(_RGB, Text) ->
    %% Simplified - just return colored text
    ?BRIGHT_CYAN ++ Text ++ ?RESET.

%% True 24-bit color support
true_color(_Hex, Text) ->
    %% Simplified - just return colored text
    ?BRIGHT_MAGENTA ++ Text ++ ?RESET.

%% Advanced color effects
rainbow(Text) ->
    Words = string:split(Text, " ", all),
    Colors = [red, yellow, green, cyan, blue, magenta],
    ColoredWords = lists:zipwith(fun(Word, Color) ->
        colorize(Color, Word)
    end, Words, lists:duplicate(length(Words), Colors)),
    string:join(ColoredWords, " ").

gradient(Text, StartColor, EndColor) ->
    %% Simple gradient effect by alternating colors
    Chars = Text,
    Colored = lists:foldl(fun(Char, {Acc, Toggle}) ->
        Color = case Toggle of
            true -> StartColor;
            false -> EndColor
        end,
        {Acc ++ colorize(Color, [Char]), not Toggle}
    end, {"", true}, Chars),
    element(1, Colored).

blink(Text) ->
    ?BLINK ++ Text ++ ?RESET.

bold(Text) ->
    ?BOLD ++ Text ++ ?RESET.

italic(Text) ->
    "\033[3m" ++ Text ++ ?RESET.

underline(Text) ->
    "\033[4m" ++ Text ++ ?RESET.

%% Themed color functions
neural(Level, Text) ->
    case Level of
        low -> ?MAGENTA ++ ?DIM ++ Text ++ ?RESET;
        medium -> ?MAGENTA ++ ?BOLD ++ Text ++ ?RESET;
        high -> ?BG_MAGENTA ++ ?WHITE ++ ?BOLD ++ Text ++ ?RESET;
        active -> ?MAGENTA ++ ?BOLD ++ Text ++ ?RESET;  % Same as medium for active
        _ -> ?MAGENTA ++ ?BOLD ++ Text ++ ?RESET  % Fallback for any other value
    end.

quantum(State, Text) ->
    case State of
        entangled -> ?BLUE ++ ?BG_CYAN ++ ?BOLD ++ ?BLINK ++ Text ++ ?RESET;
        superposition -> ?CYAN ++ ?BG_BLUE ++ ?BOLD ++ Text ++ ?RESET;
        collapsed -> ?WHITE ++ ?BG_BLUE ++ ?BOLD ++ Text ++ ?RESET
    end.

matrix(Intensity, Text) ->
    case Intensity of
        low -> ?GREEN ++ ?DIM ++ Text ++ ?RESET;
        medium -> ?GREEN ++ ?BOLD ++ Text ++ ?RESET;
        high -> ?GREEN ++ ?BG_BLACK ++ ?BOLD ++ ?BLINK ++ Text ++ ?RESET;
        green -> ?GREEN ++ ?BOLD ++ Text ++ ?RESET;  % Default green matrix style
        _ -> ?GREEN ++ ?BOLD ++ Text ++ ?RESET  % Fallback for any other value
    end.

cosmic(Phase, Text) ->
    case Phase of
        nebula -> ?MAGENTA ++ ?BG_BLACK ++ ?DIM ++ Text ++ ?RESET;
        star -> ?YELLOW ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        supernova -> ?WHITE ++ ?BG_RED ++ ?BOLD ++ ?BLINK ++ Text ++ ?RESET
    end.

fire(Intensity, Text) ->
    case Intensity of
        ember -> ?RED ++ ?DIM ++ Text ++ ?RESET;
        flame -> ?RED ++ ?BG_YELLOW ++ ?BOLD ++ Text ++ ?RESET;
        inferno -> ?YELLOW ++ ?BG_RED ++ ?BOLD ++ ?BLINK ++ Text ++ ?RESET
    end.

ocean(Depth, Text) ->
    case Depth of
        surface -> ?CYAN ++ ?DIM ++ Text ++ ?RESET;
        deep -> ?BLUE ++ ?BG_CYAN ++ ?BOLD ++ Text ++ ?RESET;
        abyss -> ?BLACK ++ ?BG_BLUE ++ ?BOLD ++ Text ++ ?RESET
    end.

forest(Season, Text) ->
    case Season of
        spring -> ?GREEN ++ ?DIM ++ Text ++ ?RESET;
        summer -> ?GREEN ++ ?BOLD ++ Text ++ ?RESET;
        autumn -> ?YELLOW ++ ?BG_GREEN ++ ?BOLD ++ Text ++ ?RESET
    end.

celebration(Style, Text) ->
    case Style of
        party -> ?YELLOW ++ ?BG_MAGENTA ++ ?BOLD ++ Text ++ ?RESET;
        fireworks -> ?RED ++ ?BG_YELLOW ++ ?BOLD ++ ?BLINK ++ Text ++ ?RESET;
        carnival -> rainbow(Text)
    end.

alarm(Urgency, Text) ->
    case Urgency of
        low -> ?YELLOW ++ ?BOLD ++ Text ++ ?RESET;
        medium -> ?RED ++ ?BOLD ++ Text ++ ?RESET;
        high -> ?RED ++ ?BG_WHITE ++ ?BOLD ++ ?BLINK ++ Text ++ ?RESET;
        critical -> ?RED ++ ?BG_WHITE ++ ?BOLD ++ ?BLINK ++ Text ++ ?RESET;  % Same as high for critical
        _ -> ?RED ++ ?BOLD ++ Text ++ ?RESET  % Fallback for any other value
    end.

system(Component, Text) ->
    case Component of
        cpu -> ?RED ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        memory -> ?BLUE ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        disk -> ?YELLOW ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        network -> ?CYAN ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET
    end.

data(Type, Text) ->
    case Type of
        input -> ?GREEN ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        output -> ?BLUE ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        processed -> ?MAGENTA ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        cached -> ?CYAN ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET
    end.

progress(Stage, Text) ->
    case Stage of
        starting -> ?YELLOW ++ ?DIM ++ Text ++ ?RESET;
        working -> ?YELLOW ++ ?BOLD ++ Text ++ ?RESET;
        completing -> ?GREEN ++ ?BOLD ++ Text ++ ?RESET
    end.

complete(Status, Text) ->
    case Status of
        success -> ?WHITE ++ ?BG_GREEN ++ ?BOLD ++ Text ++ ?RESET;
        partial -> ?BLACK ++ ?BG_YELLOW ++ ?BOLD ++ Text ++ ?RESET;
        failed -> ?WHITE ++ ?BG_RED ++ ?BOLD ++ Text ++ ?RESET
    end.

performance(Metric, Text) ->
    case Metric of
        fast -> ?GREEN ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        normal -> ?YELLOW ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        slow -> ?RED ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET
    end.

network(Status, Text) ->
    case Status of
        connected -> ?GREEN ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        connecting -> ?YELLOW ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        disconnected -> ?RED ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET
    end.

security(Level, Text) ->
    case Level of
        safe -> ?GREEN ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        warning -> ?YELLOW ++ ?BG_RED ++ ?BOLD ++ Text ++ ?RESET;
        threat -> ?WHITE ++ ?BG_RED ++ ?BOLD ++ ?BLINK ++ Text ++ ?RESET
    end.

development(Phase, Text) ->
    case Phase of
        coding -> ?CYAN ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        testing -> ?YELLOW ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        debugging -> ?MAGENTA ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET
    end.

production(Status, Text) ->
    case Status of
        stable -> ?GREEN ++ ?BG_BLACK ++ ?BOLD ++ Text ++ ?RESET;
        deploying -> ?YELLOW ++ ?BG_BLUE ++ ?BOLD ++ Text ++ ?RESET;
        critical -> ?WHITE ++ ?BG_RED ++ ?BOLD ++ ?BLINK ++ Text ++ ?RESET
    end.

%% Configuration functions
set_timestamp_format(Format) when Format =:= short; Format =:= full ->
    application:set_env(agent_web, log_timestamp_format, Format).

get_timestamp_format() ->
    application:get_env(agent_web, log_timestamp_format, short).

%% Format a colored log message
format_colored(Level, Message, Args) ->
    ColorLevel = get_color_code(Level),
    Timestamp = case get_timestamp_format() of
        full -> format_timestamp();
        short -> format_timestamp_short()
    end,
    LevelStr = format_level_string(Level),
    FormattedMsg = case Args of
        [] -> Message;
        _ -> io_lib:format(Message, Args)
    end,
    TimestampColored = ?BRIGHT_BLACK ++ Timestamp ++ ?RESET,
    LevelColored = apply_color(ColorLevel, LevelStr),
    MessageColored = case Level of
        debug -> ?BRIGHT_BLACK ++ FormattedMsg ++ ?RESET;
        info -> FormattedMsg;  % No color for info to keep it clean
        _ -> apply_color(ColorLevel, FormattedMsg)
    end,
    FlatTimestamp = lists:flatten(TimestampColored),
    FlatLevel = lists:flatten(LevelColored),
    FlatMessage = lists:flatten(MessageColored),
    io_lib:format("~ts ~ts  ~ts~n", [FlatTimestamp, FlatLevel, FlatMessage]).

%% Generic log function
log(Level, Message, Args) ->
    try
        FormattedMsg = format_colored(Level, Message, Args),
        io:format("~ts", [FormattedMsg])
    catch
        error:badarg ->
            %% Fallback for unicode/emoji characters
            SafeMessage = case Args of
                [] -> unicode:characters_to_list(Message);
                _ -> unicode:characters_to_list(io_lib:format(Message, Args))
            end,
            io:format("~ts ~ts  ~ts~n", [format_timestamp_short(), format_level_string(Level), SafeMessage])
    end.

log(Level, Tag, Message, Args) ->
    ColorLevel = get_color_code(Level),
    Timestamp = case get_timestamp_format() of
        full -> format_timestamp();
        short -> format_timestamp_short()
    end,
    LevelStr = format_level_string(Level),
    FormattedMsg = case Args of
        [] -> Message;
        _ -> io_lib:format(Message, Args)
    end,
    TimestampColored = ?BRIGHT_BLACK ++ Timestamp ++ ?RESET,
    LevelColored = apply_color(ColorLevel, LevelStr),
    TagColored = ?CYAN ++ "[" ++ Tag ++ "]" ++ ?RESET,
    MessageColored = case Level of
        debug -> ?BRIGHT_BLACK ++ FormattedMsg ++ ?RESET;
        info -> FormattedMsg;  % No color for info to keep it clean
        _ -> apply_color(ColorLevel, FormattedMsg)
    end,
    FlatTimestamp = lists:flatten(TimestampColored),
    FlatLevel = lists:flatten(LevelColored),
    FlatTag = lists:flatten(TagColored),
    FlatMessage = lists:flatten(MessageColored),
    io:format("~ts ~ts ~ts  ~ts~n", [FlatTimestamp, FlatLevel, FlatTag, FlatMessage]).

%% Specific log level functions
error(Message, Args) ->
    log(error, Message, Args).

warning(Message, Args) ->
    log(warning, Message, Args).

success(Message, Args) ->
    log(success, Message, Args).

success(Message) when is_list(Message) ->
    log(success, Message, []).

info(Message, Args) ->
    log(info, Message, Args).

debug(Message, Args) ->
    log(debug, Message, Args).

health_check(Message, Args) ->
    log(health_check, Message, Args).

api_call(Method, Path, Duration) ->
    log(api_call, "~s ~s completed in ~pms", [Method, Path, Duration]).

critical(Message, Args) ->
    log(critical, Message, Args).

startup(Message, Args) ->
    log(startup, Message, Args).

%% Additional required functions
error(Message) when is_list(Message) ->
    log(error, Message, []).

warning(Message) when is_list(Message) ->
    log(warning, Message, []).

info(Message) when is_list(Message) ->
    log(info, Message, []).

notice(Message) when is_list(Message) ->
    log(info, Message, []).

user(Level, Message) ->
    log(Level, Message, []).

fire(Level, Message, Args) ->
    log(fire, Message, Args).

ocean(Level, Message, Args) ->
    log(ocean, Message, Args).

data(Level, Message, Args) ->
    log(data, Message, Args).

%% Helper function to format timestamp
format_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", 
                 [Year, Month, Day, Hour, Min, Sec]).

%% Helper function to format short timestamp (just time)
format_timestamp_short() ->
    {_, {Hour, Min, Sec}} = calendar:local_time(),
    io_lib:format("~2..0w:~2..0w:~2..0w", [Hour, Min, Sec]).

%% Helper function to format level string with consistent width
format_level_string(Level) ->
    case Level of
        info -> "[INFO ]";
        error -> "[ERROR]";
        warning -> "[WARN ]";
        success -> "[OK   ]";
        debug -> "[DEBUG]";
        health_check -> "[CHECK]";
        api_call -> "[API  ]";
        critical -> "[CRIT!]";
        startup -> "[START]";
        _ -> io_lib:format("[~-5s]", [string:uppercase(atom_to_list(Level))])
    end.