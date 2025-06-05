-module(test_jina_tools).
-export([test/0]).

test() ->
    % Start required applications
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    
    % Test 1: Check if Jina tools are registered
    io:format("~n=== Testing Jina Tools Registration ===~n"),
    AllTools = agent_tools:list_tools(),
    JinaTools = [T || T <- AllTools, 
                      binary:match(atom_to_binary(T, utf8), <<"jina_">>) =/= nomatch],
    io:format("Found ~p Jina tools: ~p~n", [length(JinaTools), JinaTools]),
    
    % Test 2: Test a simple Jina search
    io:format("~n=== Testing Jina Search ===~n"),
    SearchArgs = #{
        <<"query">> => <<"Erlang programming language">>,
        <<"num_results">> => 3
    },
    case agent_tools:execute_tool(jina_search, SearchArgs) of
        {ok, Result} ->
            io:format("Search successful!~n"),
            Content = maps:get(<<"content">>, Result, []),
            case Content of
                [#{<<"text">> := Text} | _] ->
                    io:format("Results preview: ~s...~n", 
                             [binary:part(Text, 0, min(200, byte_size(Text)))]);
                _ ->
                    io:format("Result: ~p~n", [Result])
            end;
        {error, #{<<"code">> := Code, <<"message">> := Msg}} ->
            io:format("Search failed: ~s - ~s~n", [Code, Msg]);
        Other ->
            io:format("Unexpected result: ~p~n", [Other])
    end,
    
    % Test 3: Create an agent with Jina tools and test it
    io:format("~n=== Testing Agent with Jina Tools ===~n"),
    Config = #{
        agent_id => <<"jina_test_agent">>,
        model => <<"gpt-4o-mini">>,
        name => <<"Jina Test Agent">>,
        tools => [jina_search, jina_read_webpage]
    },
    
    case agent_supervisor:start_agent(Config) of
        {ok, Pid} ->
            io:format("Agent started with PID: ~p~n", [Pid]),
            
            % Send a chat message that should use Jina tools
            ChatMessage = <<"Please search for 'Model Context Protocol' and tell me what it is.">>,
            io:format("Sending message: ~s~n", [ChatMessage]),
            
            case gen_server:call(Pid, {chat, ChatMessage}, 60000) of
                {ok, Response} ->
                    io:format("~nAgent response: ~s~n", [Response]);
                Error ->
                    io:format("Chat error: ~p~n", [Error])
            end,
            
            % Stop the agent
            gen_server:stop(Pid);
        Error ->
            io:format("Failed to start agent: ~p~n", [Error])
    end,
    
    ok.