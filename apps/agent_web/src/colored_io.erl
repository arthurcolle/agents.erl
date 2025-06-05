%%%-------------------------------------------------------------------
%%% @doc
%%% Colored IO module that wraps standard io:format with automatic
%%% coloring and improved formatting
%%% @end
%%%-------------------------------------------------------------------
-module(colored_io).
-export([format/1, format/2, install_global_handler/0]).

%% Enhanced io:format that automatically colorizes output
format(Format) ->
    format(Format, []).

format(Format, Args) ->
    %% Detect the type of message and apply appropriate coloring
    ColoredFormat = detect_and_colorize(Format, Args),
    colored_logger:log(info, "IO", ColoredFormat, Args).

%% Install a global handler that intercepts io:format calls
install_global_handler() ->
    %% This would require a more complex approach using tracing
    %% For now, we'll just ensure our colored_logger is ready
    colored_logger:success("🎨 Enhanced IO formatting ready", []).

%% Detect message patterns and apply appropriate colors
detect_and_colorize(Format, Args) ->
    FormatStr = lists:flatten(io_lib:format(Format, Args)),
    LowerFormat = string:lowercase(FormatStr),
    
    %% Pattern matching for different log types
    case detect_log_type(LowerFormat) of
        error -> 
            colored_logger:fire(inferno, FormatStr);
        warning ->
            colored_logger:alarm(medium, FormatStr);
        success ->
            colored_logger:complete(success, FormatStr);
        info ->
            colored_logger:data(processed, FormatStr);
        debug ->
            colored_logger:development(debugging, FormatStr);
        startup ->
            colored_logger:startup(stable, FormatStr);
        network ->
            colored_logger:network(connected, FormatStr);
        _ ->
            %% Default colorful output
            colored_logger:cosmic(star, FormatStr)
    end,
    %% Return the original format to prevent double output
    "".

%% Detect log type from format string content
detect_log_type(FormatStr) ->
    case string:find(FormatStr, "error") of
        nomatch ->
            case string:find(FormatStr, "fail") of
                nomatch ->
                    case string:find(FormatStr, "crash") of
                        nomatch ->
                            case string:find(FormatStr, "warn") of
                                nomatch ->
                                    case string:find(FormatStr, "success") of
                                        nomatch ->
                                            case string:find(FormatStr, "ok") of
                                                nomatch ->
                                                    case string:find(FormatStr, "complete") of
                                                        nomatch ->
                                                            case string:find(FormatStr, "start") of
                                                                nomatch ->
                                                                    case string:find(FormatStr, "init") of
                                                                        nomatch ->
                                                                            case string:find(FormatStr, "connect") of
                                                                                nomatch ->
                                                                                    case string:find(FormatStr, "network") of
                                                                                        nomatch ->
                                                                                            case string:find(FormatStr, "debug") of
                                                                                                nomatch -> info;
                                                                                                _ -> debug
                                                                                            end;
                                                                                        _ -> network
                                                                                    end;
                                                                                _ -> network
                                                                            end;
                                                                        _ -> startup
                                                                    end;
                                                                _ -> startup
                                                            end;
                                                        _ -> success
                                                    end;
                                                _ -> success
                                            end;
                                        _ -> success
                                    end;
                                _ -> warning
                            end;
                        _ -> error
                    end;
                _ -> error
            end;
        _ -> error
    end.