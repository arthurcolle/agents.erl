-module(human_error_formatter).

-export([
    format_error/1,
    format_anthropic_error/1,
    format_mcp_error/1,
    format_tool_error/1,
    format_agent_error/1
]).

%% Main error formatting function
format_error({error, Reason}) ->
    format_error(Reason);
format_error(timeout) ->
    <<"The operation took too long to complete. This might happen when the AI service is slow or the request is complex.">>;
format_error({anthropic_error, Error}) ->
    format_anthropic_error(Error);
format_error({mcp_server_error, Error}) ->
    format_mcp_error(Error);
format_error({tool_execution_error, Error}) ->
    format_tool_error(Error);
format_error({agent_error, Error}) ->
    format_agent_error(Error);
format_error({E, R, _S}) when is_atom(E) ->
    format_exception_error(E, R);
format_error(Error) when is_binary(Error) ->
    Error;
format_error(Error) when is_list(Error) ->
    list_to_binary(Error);
format_error(Error) when is_atom(Error) ->
    format_atom_error(Error);
format_error(Error) ->
    iolist_to_binary(io_lib:format("An unexpected error occurred: ~p", [Error])).

%% Format Anthropic API errors
format_anthropic_error(#{<<"error">> := #{<<"type">> := Type, <<"message">> := Message}}) ->
    case Type of
        <<"invalid_request_error">> ->
            iolist_to_binary([<<"Invalid request: ">>, Message, <<". Please check your input format and try again.">>]);
        <<"authentication_error">> ->
            <<"Authentication failed. Please check your Anthropic API key and ensure it's valid.">>;
        <<"permission_error">> ->
            <<"Access denied. Your API key may not have permission for this operation.">>;
        <<"not_found_error">> ->
            <<"The requested resource was not found. Please check your request parameters.">>;
        <<"rate_limit_error">> ->
            <<"Rate limit exceeded. Please wait a moment before making another request.">>;
        <<"api_error">> ->
            iolist_to_binary([<<"API error: ">>, Message, <<". Please try again or contact support if the problem persists.">>]);
        <<"overloaded_error">> ->
            <<"The AI service is currently overloaded. Please try again in a few moments.">>;
        _ ->
            iolist_to_binary([<<"API error (">>, Type, <<"): ">>, Message])
    end;
format_anthropic_error({connection_failed, _}) ->
    <<"Unable to connect to the Anthropic AI service. Please check your internet connection and try again.">>;
format_anthropic_error({http_error, Code, _}) when is_integer(Code) ->
    case Code of
        401 -> <<"Invalid API key. Please check your Anthropic API configuration.">>;
        403 -> <<"Access forbidden. Your API key may not have the required permissions.">>;
        429 -> <<"Rate limit exceeded. Please wait before making another request.">>;
        500 -> <<"The AI service is experiencing issues. Please try again later.">>;
        502 -> <<"The AI service is temporarily unavailable. Please try again in a moment.">>;
        503 -> <<"The AI service is overloaded. Please try again later.">>;
        _ -> iolist_to_binary(io_lib:format("HTTP error ~p from AI service", [Code]))
    end;
format_anthropic_error(Error) ->
    iolist_to_binary(io_lib:format("AI service error: ~p", [Error])).

%% Format MCP server errors
format_mcp_error({server_not_found, ServerName}) ->
    iolist_to_binary([<<"MCP server '">>, ServerName, <<"' not found. Please check if the server is configured and running.">>]);
format_mcp_error({connection_failed, ServerName}) ->
    iolist_to_binary([<<"Cannot connect to MCP server '">>, ServerName, <<"'. The server may be down or unreachable.">>]);
format_mcp_error({server_timeout, ServerName}) ->
    iolist_to_binary([<<"MCP server '">>, ServerName, <<"' timed out. The server may be overloaded or experiencing issues.">>]);
format_mcp_error({protocol_error, Reason}) ->
    iolist_to_binary([<<"MCP protocol error: ">>, format_error(Reason), <<". There may be a version mismatch or communication issue.">>]);
format_mcp_error(Error) ->
    iolist_to_binary(io_lib:format("MCP server error: ~p", [Error])).

%% Format tool execution errors
format_tool_error({tool_not_found, ToolName}) ->
    iolist_to_binary([<<"Tool '">>, ToolName, <<"' not found. Please check the tool name and ensure it's properly configured.">>]);
format_tool_error({tool_execution_failed, ToolName, Reason}) ->
    iolist_to_binary([<<"Tool '">>, ToolName, <<"' failed to execute: ">>, format_error(Reason)]);
format_tool_error({invalid_tool_input, ToolName}) ->
    iolist_to_binary([<<"Invalid input provided to tool '">>, ToolName, <<"'. Please check the tool's input requirements.">>]);
format_tool_error({tool_timeout, ToolName}) ->
    iolist_to_binary([<<"Tool '">>, ToolName, <<"' timed out. The operation may be taking longer than expected.">>]);
format_tool_error(Error) ->
    iolist_to_binary(io_lib:format("Tool execution error: ~p", [Error])).

%% Format agent-specific errors
format_agent_error({agent_not_found, AgentId}) ->
    iolist_to_binary([<<"Agent '">>, AgentId, <<"' not found. It may have been stopped or never existed.">>]);
format_agent_error({agent_crashed, AgentId}) ->
    iolist_to_binary([<<"Agent '">>, AgentId, <<"' has crashed. Please try creating a new agent.">>]);
format_agent_error({agent_timeout, AgentId}) ->
    iolist_to_binary([<<"Agent '">>, AgentId, <<"' timed out while processing your request.">>]);
format_agent_error({configuration_error, Reason}) ->
    iolist_to_binary([<<"Agent configuration error: ">>, format_error(Reason)]);
format_agent_error(Error) ->
    iolist_to_binary(io_lib:format("Agent error: ~p", [Error])).

%% Format exception-based errors
format_exception_error(throw, Reason) ->
    iolist_to_binary([<<"An error was thrown: ">>, format_error(Reason)]);
format_exception_error(error, undef) ->
    <<"A required function is not available. This may indicate a missing dependency or configuration issue.">>;
format_exception_error(error, badarg) ->
    <<"Invalid arguments were provided. Please check your input format.">>;
format_exception_error(error, {badmatch, _}) ->
    <<"Data format mismatch occurred. The response format may have changed unexpectedly.">>;
format_exception_error(error, function_clause) ->
    <<"Function clause error. The input format may not match what's expected.">>;
format_exception_error(error, badarith) ->
    <<"Arithmetic error occurred during processing.">>;
format_exception_error(error, timeout_value) ->
    <<"Invalid timeout value specified.">>;
format_exception_error(exit, normal) ->
    <<"Process exited normally.">>;
format_exception_error(exit, killed) ->
    <<"Process was terminated.">>;
format_exception_error(exit, Reason) ->
    iolist_to_binary([<<"Process exited: ">>, format_error(Reason)]);
format_exception_error(Error, Reason) ->
    iolist_to_binary(io_lib:format("~s: ~s", [format_atom_error(Error), format_error(Reason)])).

%% Format atomic errors
format_atom_error(noproc) ->
    <<"Process not found">>;
format_atom_error(timeout) ->
    <<"Operation timed out">>;
format_atom_error(badarg) ->
    <<"Invalid argument">>;
format_atom_error(undef) ->
    <<"Function not defined">>;
format_atom_error(badarith) ->
    <<"Arithmetic error">>;
format_atom_error(system_limit) ->
    <<"System limit reached">>;
format_atom_error(Other) ->
    atom_to_binary(Other, utf8).