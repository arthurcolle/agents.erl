#!/usr/bin/env escript
%% apply_focused_logging.erl
%% Apply focused logging configuration

main(_Args) ->
    io:format("🔧 Applying Focused Logging Configuration~n"),
    io:format("========================================~n~n"),
    
    %% Backup current config
    case file:copy("config/sys.config", "config/sys.config.backup") of
        {ok, _} -> 
            io:format("✅ Backed up current sys.config~n");
        {error, Reason} ->
            io:format("⚠️ Backup failed: ~p (continuing anyway)~n", [Reason])
    end,
    
    %% Copy focused logging config
    case file:copy("config/focused_logging.config", "config/sys.config") of
        {ok, _} ->
            io:format("✅ Applied focused logging configuration~n"),
            io:format("~n🎯 Focused logging will now:~n"),
            io:format("   ✅ Show agent-related messages~n"),
            io:format("   ✅ Show MCP protocol messages~n"),
            io:format("   ✅ Show tool execution messages~n"),
            io:format("   ✅ Show function calling messages~n"),
            io:format("   ✅ Show AI/chat messages~n"),
            io:format("   ❌ Hide Ranch/Cowboy noise~n"),
            io:format("   ❌ Hide OTP internal messages~n"),
            io:format("   ❌ Hide generic system messages~n"),
            io:format("~n⚡ Restart the system to apply changes~n");
        {error, CopyError} ->
            io:format("❌ Failed to apply focused logging: ~p~n", [CopyError])
    end,
    
    %% Also create a development logging config (even more verbose for debugging)
    DevConfig = create_dev_logging_config(),
    case file:write_file("config/dev_logging.config", DevConfig) of
        ok ->
            io:format("✅ Created development logging config (config/dev_logging.config)~n"),
            io:format("   💡 Use this for debugging with: cp config/dev_logging.config config/sys.config~n");
        {error, DevError} ->
            io:format("⚠️ Failed to create dev config: ~p~n", [DevError])
    end.

create_dev_logging_config() ->
<<"% Development logging - shows more detail for debugging
[
  {agent_web, [
    {port, 8080},
    {disable_colors, false},
    {log_timestamp_format, short}
  ]},
  {kernel, [
    {logger_level, debug},  % More verbose
    {logger, [
      {handler, default, undefined},
      {handler, dev_agent_handler, colored_logger_handler, #{
        level => all,
        config => #{disable_colors => false},
        filters => [
          % Still filter out OTP noise
          {domain, {fun logger_filters:domain/2, {stop, equal, [otp]}}},
          
          % Less aggressive Ranch/Cowboy filtering for dev
          {ranch_cowboy_dev_filter, {fun(LogEvent, _) ->
            case LogEvent of
              #{msg := {report, #{label := {error_logger, error_msg}, 
                                  format := Format}}} when 
                  Format =:= \\\"Ranch listener ~p, connection process ~p, stream ~p had its request process ~p exit with reason ~999999p and stacktrace ~999999p~n\\\" ->
                stop;  % Still filter the verbose Ranch errors
              _ ->
                ignore
            end
          end, ok}},
          
          % Allow most agent-related messages in dev mode
          {agent_dev_filter, {fun(LogEvent, _) ->
            case LogEvent of
              #{meta := #{module := Module}} when is_atom(Module) ->
                ModuleName = atom_to_list(Module),
                % Be more permissive in dev mode
                IsRelevant = lists:any(fun(Pattern) ->
                  string:str(string:to_lower(ModuleName), Pattern) > 0
                end, [\\\"agent\\\", \\\"mcp\\\", \\\"tool\\\", \\\"stream\\\", \\\"chat\\\", \\\"sup\\\", \\\"handler\\\"]),
                case IsRelevant of
                  true -> ignore;
                  false -> stop
                end;
              _ ->
                % More permissive in dev mode
                ignore
            end
          end, ok}}
        ]
      }}
    ]},
    {error_logger, silent}
  ]}
].
">>.