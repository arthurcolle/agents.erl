-module(claude_cli_handler).
-export([init/2]).

%% Colorful logging macros
-define(LOG_INFO(Msg), colored_logger:production(stable, "[CLI] " ++ Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:production(stable, io_lib:format("[CLI] " ++ Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, "[CLI] " ++ Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format("[CLI] " ++ Msg, Args))).

init(Req0, State) ->
    {Method, Req1} = cowboy_req:method(Req0),
    {ok, handle_request(Method, Req1), State}.

handle_request(<<"POST">>, Req) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    
    try
        ParsedBody = case Body of
            <<>> -> #{};
            _ -> jsx:decode(Body, [return_maps])
        end,
        Command = maps:get(<<"command">>, ParsedBody, <<"">>),
        Options = maps:get(<<"options">>, ParsedBody, #{}),
        
        ?LOG_INFO("Executing Claude CLI command: ~s", [Command]),
        
        Result = execute_claude_command(Command, Options),
        
        %% Ensure Result is always a map that can be JSON-encoded
        SafeResult = case Result of
            R when is_map(R) -> R;
            R when is_binary(R) -> #{<<"output">> => R};
            R when is_list(R) -> #{<<"output">> => list_to_binary(R)};
            R -> #{<<"output">> => list_to_binary(io_lib:format("~p", [R]))}
        end,
        
        Response = jsx:encode(#{
            <<"success">> => true,
            <<"result">> => SafeResult,
            <<"timestamp">> => erlang:system_time(second)
        }),
        
        cowboy_req:reply(200, #{
            <<"content-type">> => <<"application/json">>,
            <<"access-control-allow-origin">> => <<"*">>,
            <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
            <<"access-control-allow-headers">> => <<"content-type, authorization">>
        }, Response, Req1)
        
    catch
        Class:Error:Stack ->
            ?LOG_ERROR("Error in CLI handler: ~p:~p", [Class, Error]),
            ?LOG_ERROR("Stack: ~p", [Stack]),
            
            ErrorResponse = jsx:encode(#{
                <<"success">> => false,
                <<"error">> => <<"Command execution failed">>,
                <<"details">> => list_to_binary(io_lib:format("~p:~p", [Class, Error])),
                <<"timestamp">> => erlang:system_time(second)
            }),
            
            cowboy_req:reply(500, #{
                <<"content-type">> => <<"application/json">>,
                <<"access-control-allow-origin">> => <<"*">>
            }, ErrorResponse, Req1)
    end;

handle_request(<<"OPTIONS">>, Req) ->
    cowboy_req:reply(200, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
        <<"access-control-allow-headers">> => <<"content-type, authorization">>
    }, <<>>, Req);

handle_request(<<"GET">>, Req) ->
    %% Return available commands and help information
    Commands = #{
        <<"available_commands">> => [
            <<"claude -p \"Your prompt here\"">>,
            <<"claude --continue">>,
            <<"claude --resume <session_id>">>,
            <<"claude --output-format json">>,
            <<"claude --help">>,
            <<"claude --version">>,
            <<"system status">>,
            <<"system metrics">>,
            <<"list agents">>,
            <<"help">>
        ],
        <<"examples">> => [
            <<"claude -p \"Write a function to calculate Fibonacci numbers\"">>,
            <<"claude --continue \"Now add error handling\"">>,
            <<"system status">>,
            <<"list agents">>
        ]
    },
    
    Response = jsx:encode(Commands),
    
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>,
        <<"access-control-allow-origin">> => <<"*">>
    }, Response, Req);

handle_request(Method, Req) ->
    ?LOG_ERROR("Unsupported method: ~s", [Method]),
    cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req).

%% Execute Claude Code commands
execute_claude_command(Command, Options) ->
    CommandStr = binary_to_list(Command),
    
    case string:tokens(CommandStr, " ") of
        ["claude" | Args] ->
            execute_claude_with_args(Args, Options);
        ["system", "status"] ->
            get_system_status();
        ["system", "metrics"] ->
            get_system_metrics();
        ["list", "agents"] ->
            list_agents();
        ["help"] ->
            get_help();
        _ ->
            execute_generic_command(CommandStr, Options)
    end.

%% Execute Claude Code with specific arguments
execute_claude_with_args(Args, Options) ->
    try
        %% Check if claude command is available
        case os:find_executable("claude") of
            false ->
                %% Claude CLI not found, return helpful error
                #{
                    <<"error">> => true,
                    <<"message">> => <<"Claude CLI not found in PATH">>,
                    <<"suggestion">> => <<"Please install Claude Code CLI or use the built-in chat interface">>
                };
            ClaudePath ->
                %% Build the claude command
                FullCmd = ClaudePath ++ " " ++ string:join(Args, " "),
                
                ?LOG_INFO("Executing: ~s", [FullCmd]),
                
                %% Execute the command and capture output
                Port = open_port({spawn, FullCmd}, [stream, binary, exit_status, stderr_to_stdout]),
                
                %% Collect output with timeout
                case collect_port_output(Port, <<>>) of
                    {ok, Result} ->
                        %% Try to parse as JSON first, if that fails return as plain text
                        try
                            jsx:decode(Result, [return_maps])
                        catch
                            _:_ ->
                                #{
                                    <<"output">> => Result,
                                    <<"format">> => <<"text">>
                                }
                        end;
                    {error, ExitCode, ErrorOutput} ->
                        #{
                            <<"error">> => true,
                            <<"exit_code">> => ExitCode,
                            <<"output">> => ErrorOutput
                        }
                end
        end
        
    catch
        Class:Error ->
            ?LOG_ERROR("Failed to execute claude command: ~p:~p", [Class, Error]),
            #{
                <<"error">> => true,
                <<"message">> => <<"Failed to execute claude command">>,
                <<"details">> => list_to_binary(io_lib:format("~p:~p", [Class, Error]))
            }
    end.

%% Execute generic system command (with safety checks)
execute_generic_command(Command, _Options) ->
    %% Only allow safe commands
    SafeCommands = [
        "pwd", "ls", "whoami", "date", "uptime",
        "ps aux | grep beam", "df -h", "free -h",
        "netstat -tulpn | grep :8080"
    ],
    
    case lists:member(Command, SafeCommands) of
        true ->
            try
                Port = open_port({spawn, Command}, [stream, binary, exit_status, stderr_to_stdout]),
                collect_port_output(Port, <<>>)
            catch
                Class:Error ->
                    #{
                        <<"error">> => true,
                        <<"message">> => <<"Command execution failed">>,
                        <<"details">> => list_to_binary(io_lib:format("~p:~p", [Class, Error]))
                    }
            end;
        false ->
            #{
                <<"error">> => true,
                <<"message">> => <<"Command not allowed">>,
                <<"allowed_commands">> => SafeCommands
            }
    end.

%% Collect output from a port
collect_port_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port_output(Port, <<Acc/binary, Data/binary>>);
        {Port, {exit_status, 0}} ->
            port_close(Port),
            {ok, Acc};
        {Port, {exit_status, ExitCode}} ->
            port_close(Port),
            {error, ExitCode, Acc}
    after 30000 -> % 30 second timeout
        port_close(Port),
        {error, timeout, <<"Command timed out">>}
    end.

%% Get system status
get_system_status() ->
    try
        SystemStatus = system_health_handler:get_system_status(),
        SystemStatus
    catch
        _:_ ->
            #{
                <<"error">> => true,
                <<"message">> => <<"Unable to get system status">>
            }
    end.

%% Get system metrics
get_system_metrics() ->
    try
        Memory = erlang:memory(),
        Statistics = #{
            <<"memory_total">> => proplists:get_value(total, Memory),
            <<"memory_processes">> => proplists:get_value(processes, Memory),
            <<"memory_system">> => proplists:get_value(system, Memory),
            <<"process_count">> => erlang:system_info(process_count),
            <<"scheduler_utilization">> => get_scheduler_utilization(),
            <<"uptime_seconds">> => element(1, erlang:statistics(wall_clock)) div 1000
        },
        Statistics
    catch
        _:_ ->
            #{
                <<"error">> => true,
                <<"message">> => <<"Unable to get system metrics">>
            }
    end.

%% Get scheduler utilization
get_scheduler_utilization() ->
    try
        erlang:statistics(scheduler_wall_time_all)
    catch
        _:_ -> <<"unavailable">>
    end.

%% List agents
list_agents() ->
    try
        case agent:list_agents() of
            {ok, Agents} ->
                #{
                    <<"agents">> => Agents,
                    <<"count">> => length(Agents)
                };
            {error, Reason} ->
                #{
                    <<"error">> => true,
                    <<"message">> => <<"Unable to list agents">>,
                    <<"reason">> => list_to_binary(io_lib:format("~p", [Reason]))
                }
        end
    catch
        _:_ ->
            #{
                <<"error">> => true,
                <<"message">> => <<"Unable to list agents">>
            }
    end.

%% Get help information
get_help() ->
    #{
        <<"help">> => <<"Claude Code CLI Interface">>,
        <<"description">> => <<"Execute Claude Code commands directly from the web interface">>,
        <<"commands">> => [
            #{
                <<"command">> => <<"claude -p \"prompt\"">>,
                <<"description">> => <<"Execute a Claude prompt in non-interactive mode">>
            },
            #{
                <<"command">> => <<"claude --continue">>,
                <<"description">> => <<"Continue the most recent conversation">>
            },
            #{
                <<"command">> => <<"claude --resume <session_id>">>,
                <<"description">> => <<"Resume a specific conversation by session ID">>
            },
            #{
                <<"command">> => <<"claude --output-format json">>,
                <<"description">> => <<"Output in JSON format with metadata">>
            },
            #{
                <<"command">> => <<"system status">>,
                <<"description">> => <<"Get system health status">>
            },
            #{
                <<"command">> => <<"system metrics">>,
                <<"description">> => <<"Get system performance metrics">>
            },
            #{
                <<"command">> => <<"list agents">>,
                <<"description">> => <<"List all active agents">>
            }
        ],
        <<"examples">> => [
            <<"claude -p \"Write a hello world function in Erlang\"">>,
            <<"claude --continue \"Now add error handling\"">>,
            <<"system status">>,
            <<"list agents">>
        ]
    }.