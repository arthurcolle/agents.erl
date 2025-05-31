%% anthropic_api_structure.erl
%% Module to define the structure of Anthropic Claude API endpoints with MCP connector support
-module(anthropic_api_structure).

-export([
    get_api_groups/0,
    get_endpoints/1
]).

%% Define the main API groups/categories for Anthropic
-spec get_api_groups() -> [atom()].
get_api_groups() ->
    [
        models,
        messages,
        mcp_connector
    ].

%% Return endpoints for a specific group
-spec get_endpoints(atom()) -> #{atom() => map()}.

%% Models endpoints
get_endpoints(models) ->
    #{
        list_models => #{
            method => get,
            path => "/v1/models",
            description => "Lists the currently available models.",
            required_params => [],
            optional_params => [],
            base_url => "https://api.anthropic.com"
        }
    };

%% Messages endpoints with MCP connector support
get_endpoints(messages) ->
    #{
        create_message => #{
            method => post,
            path => "/v1/messages",
            description => "Creates a message with optional MCP server connections.",
            required_params => [model, max_tokens, messages],
            optional_params => [
                system, temperature, top_p, top_k, stop_sequences,
                stream, metadata, tool_choice, tools, mcp_servers
            ],
            base_url => "https://api.anthropic.com",
            beta_headers => ["mcp-client-2025-04-04"]
        }
    };

%% MCP Connector specific endpoints
get_endpoints(mcp_connector) ->
    #{
        create_message_with_mcp => #{
            method => post,
            path => "/v1/messages",
            description => "Creates a message with MCP servers for tool calling.",
            required_params => [model, max_tokens, messages, mcp_servers],
            optional_params => [
                system, temperature, top_p, top_k, stop_sequences,
                stream, metadata, tool_choice, tools
            ],
            base_url => "https://api.anthropic.com",
            beta_headers => ["mcp-client-2025-04-04"],
            mcp_support => true
        },
        validate_mcp_server => #{
            method => get,
            path => "/validate",
            description => "Validates an MCP server configuration (internal use).",
            required_params => [url],
            optional_params => [authorization_token],
            base_url => "internal",
            mcp_support => true
        }
    };

%% If an unknown group is requested, return an empty map
get_endpoints(_) ->
    #{}.