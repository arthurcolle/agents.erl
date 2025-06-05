%% Test application startup order
-module(test_app_startup).
-export([test_startup/0]).

test_startup() ->
    io:format("Testing application startup order...~n"),
    
    % Start dependencies first
    io:format("1. Starting dependencies...~n"),
    Dependencies = [kernel, stdlib, crypto, ssl, inets, jsx],
    lists:foreach(fun(App) ->
        case application:start(App) of
            ok -> io:format("✓ ~p started~n", [App]);
            {error, {already_started, _}} -> io:format("✓ ~p already running~n", [App]);
            Error -> io:format("✗ ~p failed: ~p~n", [App, Error])
        end
    end, Dependencies),
    
    % Start OpenAI app
    io:format("2. Starting openai app...~n"),
    case application:start(openai) of
        ok -> io:format("✓ OpenAI app started~n");
        {error, {already_started, _}} -> io:format("✓ OpenAI app already running~n");
        Error1 -> io:format("✗ OpenAI app failed: ~p~n", [Error1])
    end,
    
    % Start agents app
    io:format("3. Starting agents app...~n"),
    case application:start(agents) of
        ok -> io:format("✓ Agents app started~n");
        {error, {already_started, _}} -> io:format("✓ Agents app already running~n");
        Error2 -> io:format("✗ Agents app failed: ~p~n", [Error2])
    end,
    
    % Start agent_web app
    io:format("4. Starting agent_web app...~n"),
    case application:start(agent_web) of
        ok -> io:format("✓ Agent_web app started~n");
        {error, {already_started, _}} -> io:format("✓ Agent_web app already running~n");
        Error3 -> io:format("✗ Agent_web app failed: ~p~n", [Error3])
    end,
    
    io:format("Testing completed. System should be running now.~n"),
    
    % Keep the shell alive to test
    timer:sleep(5000),
    
    % Test web server
    Port = application:get_env(agent_web, port, 8080),
    case gen_tcp:connect("localhost", Port, [], 1000) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            io:format("✓ Web server responding on port ~p~n", [Port]);
        {error, ConnError} ->
            io:format("✗ Web server not responding on port ~p: ~p~n", [Port, ConnError])
    end,
    
    ok.