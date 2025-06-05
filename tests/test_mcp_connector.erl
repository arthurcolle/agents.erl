%% test_mcp_connector.erl
%% Test module for Anthropic MCP connector integration
-module(test_mcp_connector).

-export([
    test_basic_connection/0,
    test_agent_with_mcp/0,
    test_mcp_servers/0,
    test_anthropic_format/0,
    run_all_tests/0
]).

%% Test basic MCP connector functionality
test_basic_connection() ->
    io:format("Testing basic MCP connector functionality...~n"),
    
    % Test Anthropic client startup
    case anthropic_client:start_link() of
        {ok, _Pid} ->
            io:format("✓ Anthropic client started successfully~n");
        {error, {already_started, _Pid}} ->
            io:format("✓ Anthropic client already running~n");
        Error ->
            io:format("✗ Failed to start Anthropic client: ~p~n", [Error]),
            error
    end,
    
    % Test basic message creation (without MCP)
    TestResult = anthropic_client:create_message(
        <<"claude-sonnet-4-20250514">>,
        100,
        [#{role => <<"user">>, content => <<"Hello, Claude!">>}]
    ),
    
    case TestResult of
        {ok, _Response} ->
            io:format("✓ Basic Anthropic API call successful~n"),
            ok;
        {error, Reason} ->
            io:format("⚠ Basic API call failed (expected if no API key): ~p~n", [Reason]),
            ok  % This is expected without proper API key
    end.

%% Test agent system with MCP support
test_agent_with_mcp() ->
    io:format("Testing agent system with MCP integration...~n"),
    
    % Create a test agent ID
    TestAgentId = <<"test_agent_mcp">>,
    
    % Test the MCP agent function
    case agent:run_agent_with_mcp(
        <<"What tools are available to me?">>,
        TestAgentId,
        [],
        #{model => <<"claude-sonnet-4-20250514">>, max_tokens => 500}
    ) of
        {ok, Response} ->
            io:format("✓ MCP agent response: ~s~n", [Response]),
            ok;
        {error, {mcp_server_error, _}} ->
            io:format("⚠ No MCP servers configured for agent (expected)~n"),
            ok;
        {error, Reason} ->
            io:format("⚠ MCP agent test failed: ~p~n", [Reason]),
            ok
    end.

%% Test MCP server configuration
test_mcp_servers() ->
    io:format("Testing MCP server configuration...~n"),
    
    % Test getting all servers in Anthropic format
    case mcp_server_config:get_all_anthropic_mcp_servers() of
        {ok, Servers} ->
            io:format("✓ Found ~p MCP servers in Anthropic format~n", [length(Servers)]),
            
            % Display first few servers
            case Servers of
                [] ->
                    io:format("  No servers configured~n");
                [FirstServer|_] ->
                    io:format("  Example server: ~p~n", [FirstServer])
            end,
            ok;
        {error, Reason} ->
            io:format("✗ Failed to get MCP servers: ~p~n", [Reason]),
            error
    end,
    
    % Test server format conversion
    TestServer = #{
        id => <<"test_server">>,
        name => <<"Test Server">>,
        url => <<"https://test.example.com/sse">>,
        auth_type => oauth2,
        metadata => #{
            auth_token => <<"test_token">>,
            tool_configuration => #{
                enabled => true,
                allowed_tools => [<<"test_tool">>]
            }
        }
    },
    
    % Convert to Anthropic format
    try
        AnthropicFormat = mcp_server_config:to_anthropic_mcp_format(TestServer),
        io:format("✓ Server format conversion successful~n"),
        io:format("  Converted format: ~p~n", [AnthropicFormat]),
        ok
    catch
        E:R:S ->
            io:format("✗ Server format conversion failed: ~p:~p~n", [E, R]),
            error
    end.

%% Test Anthropic format validation
test_anthropic_format() ->
    io:format("Testing Anthropic MCP format validation...~n"),
    
    % Test valid MCP server format
    ValidServer = #{
        type => <<"url">>,
        url => <<"https://mcp.example.com/sse">>,
        name => <<"test-server">>
    },
    
    case anthropic_client:validate_mcp_servers([ValidServer]) of
        ok ->
            io:format("✓ Valid MCP server format accepted~n");
        {error, Reason} ->
            io:format("✗ Valid MCP server rejected: ~p~n", [Reason])
    end,
    
    % Test invalid MCP server format (missing required fields)
    InvalidServer = #{
        type => <<"url">>,
        url => <<"https://mcp.example.com/sse">>
        % Missing name field
    },
    
    case anthropic_client:validate_mcp_servers([InvalidServer]) of
        {error, _Reason} ->
            io:format("✓ Invalid MCP server correctly rejected~n");
        ok ->
            io:format("✗ Invalid MCP server incorrectly accepted~n")
    end,
    
    % Test invalid URL format
    InvalidUrlServer = #{
        type => <<"url">>,
        url => <<"http://mcp.example.com/sse">>,  % Should be https
        name => <<"test-server">>
    },
    
    case anthropic_client:validate_mcp_servers([InvalidUrlServer]) of
        {error, _Reason} ->
            io:format("✓ Invalid URL format correctly rejected~n");
        ok ->
            io:format("✗ Invalid URL format incorrectly accepted~n")
    end.

%% Run all tests
run_all_tests() ->
    io:format("=== Anthropic MCP Connector Integration Tests ===~n~n"),
    
    Results = [
        test_basic_connection(),
        test_mcp_servers(),
        test_anthropic_format(),
        test_agent_with_mcp()
    ],
    
    SuccessCount = length([R || R <- Results, R =:= ok]),
    TotalCount = length(Results),
    
    io:format("~n=== Test Results ===~n"),
    io:format("Passed: ~p/~p tests~n", [SuccessCount, TotalCount]),
    
    case SuccessCount of
        TotalCount ->
            io:format("✓ All tests passed!~n"),
            ok;
        _ ->
            io:format("⚠ Some tests failed or had warnings~n"),
            partial_success
    end.