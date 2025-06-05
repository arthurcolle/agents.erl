-module(test_multi_turn_simple).
-export([test/0]).

test() ->
    io:format("Starting simple multi-turn function calling test...~n~n"),
    
    % Start required applications
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(inets),
    application:start(jsx),
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    
    % Create an agent with jina_search_and_read tool
    Config = #{
        name => <<"Test Research Agent">>,
        model => <<"gpt-4.1-2025-04-14">>,
        tools => [jina_search_and_read],
        system_prompt => <<"You are a helpful research assistant. When asked about weather, use the jina_search_and_read tool to find current, detailed information by searching and reading web pages.">>,
        api_preference => responses
    },
    
    io:format("Creating agent...~n"),
    {ok, AgentPid} = agent_instance:start_link(Config),
    
    % Test message that should trigger function calling
    Message = <<"What's the current weather in Washington DC?">>,
    
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
            io:format("~n========================================~n"),
            io:format("Agent response:~n~s~n", [ResponseMessage]),
            io:format("========================================~n"),
            ok;
        {error, ErrorReason} ->
            io:format("~nError: ~p~n", [ErrorReason]),
            error
    end.