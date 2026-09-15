%%%-------------------------------------------------------------------
%%% @doc OTP application callback for the HTTP/API layer.
%%%
%%% Dependencies are declared in agent_web.app.src and started by the
%%% application controller. Post-start work is owned by the supervised
%%% agent_web_bootstrap worker.
%%% @end
%%%-------------------------------------------------------------------
-module(agent_web_app).
-behaviour(application).

-export([start/2, stop/1, install_better_logging/0]).

start(_StartType, _StartArgs) ->
    ok = install_better_logging(),
    agent_web_sup:start_link().

stop(_State) ->
    ok.

install_better_logging() ->
    error_logger:tty(false),
    _ = remove_logger_handler(colored_logger),
    HandlerResult = try logger:add_handler(colored_logger, colored_logger_handler, #{
            level => all,
            config => #{disable_colors => false}
        })
    catch
        Class:Error -> {error, {Class, Error}}
    end,
    case HandlerResult of
        ok ->
            %% Keep the primary/default handler only when the replacement
            %% could not be installed; otherwise duplicate every log line.
            _ = remove_logger_handler(default),
            ok;
        {error, {already_exists, _}} ->
            _ = remove_logger_handler(default),
            ok;
        {error, HandlerError} ->
            io:format(
                "Warning: Could not install colored logger handler (~p); "
                "using basic logging~n",
                [HandlerError]
            )
    end,
    LoggerLevel = application:get_env(agent_web, logger_level, info),
    logger:set_primary_config(level, LoggerLevel),
    ok.

remove_logger_handler(Id) ->
    case logger:remove_handler(Id) of
        ok -> ok;
        {error, {not_found, _}} -> ok;
        {error, _Reason} -> ok
    end.
