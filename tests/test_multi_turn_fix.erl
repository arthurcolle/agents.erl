-module(test_multi_turn_fix).
-export([test/0, test_multi_turn/0]).

test() ->
    io:format("Starting multi-turn function calling test...~n~n"),
    
    % Start required applications
    application:ensure_all_started(agents),
    application:ensure_all_started(openai),
    application:ensure_all_started(agent_web),
    
    % Create an agent with jina_search tool
    Config = #{
        name => <<"Test Research Agent">>,
        model => <<"gpt-4.1-2025-04-14">>,
        tools => [jina_search, jina_deep_search],
        system_prompt => <<"You are a research assistant. Use the available tools to find information.">>,
        api_preference => responses
    },
    
    io:format("Creating agent...~n"),
    {ok, AgentPid} = agent_instance:start_link(Config),
    
    % Test message that should trigger function calling
    Message = <<"What's the current weather in Washington DC? Please search for it.">>,
    
    io:format("Sending message: ~s~n~n", [Message]),
    
    Result = try
        agent_instance:execute(AgentPid, #{action => <<"chat">>, message => Message})
    catch
        Error:Reason:Stack ->
            io:format("Error executing chat: ~p:~p~n", [Error, Reason]),
            io:format("Stack trace:~n"),
            lists:foreach(fun(Frame) ->
                io:format("  ~p~n", [Frame])
            end, Stack),
            {error, {Error, Reason}}
    end,
    
    case Result of
        {ok, #{message := ResponseMessage}} ->
            io:format("~nAgent response: ~s~n", [ResponseMessage]),
            ok;
        {error, ErrorReason} ->
            io:format("~nError: ~p~n", [ErrorReason]),
            error
    end.

test_multi_turn() ->
    io:format("Testing complex multi-turn function calling...~n~n"),
    
    % Start required applications
    application:ensure_all_started(agents),
    application:ensure_all_started(openai),
    application:ensure_all_started(agent_web),
    
    % Create an agent with multiple tools
    Config = #{
        name => <<"Multi-Turn Test Agent">>,
        model => <<"gpt-4.1-2025-04-14">>,
        tools => [jina_search, jina_deep_search, file_write, file_read],
        system_prompt => <<"You are a helpful assistant. Use tools as needed to complete tasks.">>,
        api_preference => responses
    },
    
    io:format("Creating agent...~n"),
    {ok, AgentPid} = agent_instance:start_link(Config),
    
    % Test message that should trigger multiple function calls
    Message = <<"Please search for the current weather in Washington DC and New York, then write a summary to weather_summary.txt">>,
    
    io:format("Sending message: ~s~n~n", [Message]),
    
    Result = try
        agent_instance:execute(AgentPid, #{action => <<"chat">>, message => Message})
    catch
        Error:Reason:Stack ->
            io:format("Error executing chat: ~p:~p~n", [Error, Reason]),
            io:format("Stack trace:~n"),
            lists:foreach(fun(Frame) ->
                io:format("  ~p~n", [Frame])
            end, Stack),
            {error, {Error, Reason}}
    end,
    
    case Result of
        {ok, #{message := ResponseMessage}} ->
            io:format("~nAgent response: ~s~n", [ResponseMessage]),
            
            % Check if file was created
            case file:read_file("weather_summary.txt") of
                {ok, Content} ->
                    io:format("~nFile content:~n~s~n", [Content]);
                _ ->
                    io:format("~nFile not found~n")
            end,
            ok;
        {error, ErrorReason2} ->
            io:format("~nError: ~p~n", [ErrorReason2]),
            error
    end.