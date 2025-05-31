%%%-------------------------------------------------------------------
%%% @doc
%%% MCP Server Configuration Module
%%% Manages remote MCP (Model Context Protocol) servers and their capabilities
%%% Enhanced with comprehensive logging and error reporting
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_server_config).

-export([init/0, get_seed_servers/0, get_all_servers/0, add_server/1, 
         update_server/2, delete_server/1, get_server/1,
         discover_capabilities/1, get_server_capabilities/1,
         assign_server_to_agent/2, get_agent_servers/1]).

-include_lib("stdlib/include/qlc.hrl").

%% Enhanced logging utility with line numbers
-define(LOG(Level, Format, Args), 
    io:format("[~s:~p] [~s] " ++ Format ++ "~n", 
              [?MODULE, ?LINE, Level | Args])).

-define(LOG_INFO(Format, Args), ?LOG("INFO", Format, Args)).
-define(LOG_WARN(Format, Args), ?LOG("WARN", Format, Args)).
-define(LOG_ERROR(Format, Args), ?LOG("ERROR", Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG("DEBUG", Format, Args)).

-record(mcp_server, {
    id :: binary(),
    name :: binary(),
    category :: binary(),
    url :: binary(),
    auth_type :: oauth2 | api_key | open,
    maintainer :: binary(),
    description :: binary(),
    capabilities :: list(),
    status :: active | inactive,
    last_checked :: calendar:datetime(),
    metadata :: map()
}).

-record(agent_mcp_mapping, {
    agent_id :: binary(),
    server_ids :: [binary()]
}).

%%%===================================================================
%%% API
%%%===================================================================

init() ->
    ?LOG_INFO("Initializing MCP server configuration", []),
    
    % Ensure Mnesia is started first
    case mnesia:start() of
        ok -> 
            ?LOG_INFO("Mnesia started successfully", []);
        {error, {already_started, mnesia}} -> 
            ?LOG_INFO("Mnesia already started", []);
        {error, Reason} -> 
            ?LOG_ERROR("Failed to start Mnesia: ~p", [Reason]),
            error(Reason)
    end,
    
    % Wait for Mnesia tables to be ready
    ?LOG_DEBUG("Waiting for Mnesia schema tables", []),
    mnesia:wait_for_tables([schema], 5000),
    
    % Create tables if they don't exist
    % Use ram_copies for development when no proper node name is set
    TableType = case node() of
        nonode@nohost -> ram_copies;
        _ -> disc_copies
    end,
    ?LOG_INFO("Using table type: ~p for node: ~p", [TableType, node()]),
    
    case mnesia:create_table(mcp_server, 
        [{attributes, record_info(fields, mcp_server)},
         {TableType, [node()]}]) of
        {atomic, ok} -> 
            ?LOG_INFO("Created mcp_server table", []);
        {aborted, {already_exists, mcp_server}} -> 
            ?LOG_INFO("mcp_server table already exists", []);
        {aborted, Reason1} ->
            ?LOG_ERROR("Failed to create mcp_server table: ~p", [Reason1])
    end,
    
    case mnesia:create_table(agent_mcp_mapping,
        [{attributes, record_info(fields, agent_mcp_mapping)},
         {TableType, [node()]}]) of
        {atomic, ok} -> 
            ?LOG_INFO("Created agent_mcp_mapping table", []);
        {aborted, {already_exists, agent_mcp_mapping}} -> 
            ?LOG_INFO("agent_mcp_mapping table already exists", []);
        {aborted, Reason2} ->
            ?LOG_ERROR("Failed to create agent_mcp_mapping table: ~p", [Reason2])
    end,
    
    % Load seed servers
    ?LOG_INFO("Loading seed servers", []),
    load_seed_servers(),
    ?LOG_INFO("MCP server configuration initialization complete", []).

get_seed_servers() ->
    [
        #{id => <<"asana">>,
          name => <<"Asana">>,
          category => <<"Project Management">>,
          url => <<"https://mcp.asana.com/sse">>,
          auth_type => oauth2,
          maintainer => <<"Asana">>,
          description => <<"Interact with your Asana workspace through AI tools to keep projects on track">>,
          capabilities => [
              <<"task_management">>, <<"project_tracking">>, <<"team_collaboration">>
          ],
          metadata => #{
              official => true,
              documentation_url => <<"https://developers.asana.com/mcp">>,
              requires_auth => true
          }
        },
        #{id => <<"atlassian">>,
          name => <<"Atlassian">>,
          category => <<"Software Development">>,
          url => <<"https://mcp.atlassian.com/v1/sse">>,
          auth_type => oauth2,
          maintainer => <<"Atlassian">>,
          description => <<"Access Atlassian services including Jira and Confluence">>},
        
        
        #{id => <<"linear">>,
          name => <<"Linear">>,
          category => <<"Project Management">>,
          url => <<"https://mcp.linear.app/sse">>,
          auth_type => oauth2,
          maintainer => <<"Linear">>,
          description => <<"Issue tracking and project management for software teams">>},
        
        #{id => <<"intercom">>,
          name => <<"Intercom">>,
          category => <<"Customer Support">>,
          url => <<"https://mcp.intercom.com/sse">>,
          auth_type => oauth2,
          maintainer => <<"Intercom">>,
          description => <<"Access real-time customer conversations, tickets, and user data from Intercom">>,
          capabilities => [
              <<"conversation_management">>, <<"ticket_handling">>, <<"user_data_access">>
          ],
          metadata => #{
              official => true,
              documentation_url => <<"https://developers.intercom.com/mcp">>,
              requires_auth => true
          }
        },
        #{id => <<"invideo">>,
          name => <<"invideo">>,
          category => <<"Media Creation">>,
          url => <<"https://mcp.invideo.io/sse">>,
          auth_type => api_key,
          maintainer => <<"invideo">>,
          description => <<"Build video creation capabilities into your applications">>,
          capabilities => [
              <<"video_creation">>, <<"template_management">>, <<"media_processing">>
          ],
          metadata => #{
              official => true,
              documentation_url => <<"https://developer.invideo.io/mcp">>,
              requires_auth => true
          }
        },
        #{id => <<"paypal">>,
          name => <<"PayPal">>,
          category => <<"Finance">>,
          url => <<"https://mcp.paypal.com/sse">>,
          auth_type => oauth2,
          maintainer => <<"PayPal">>,
          description => <<"Integrate PayPal commerce capabilities">>,
          capabilities => [
              <<"payment_processing">>, <<"transaction_management">>, <<"merchant_services">>
          ],
          metadata => #{
              official => true,
              documentation_url => <<"https://developer.paypal.com/mcp">>,
              requires_auth => true
          }
        },
        #{id => <<"plaid">>,
          name => <<"Plaid">>,
          category => <<"Finance">>,
          url => <<"https://api.dashboard.plaid.com/mcp/sse">>,
          auth_type => api_key,
          maintainer => <<"Plaid">>,
          description => <<"Analyze, troubleshoot, and optimize Plaid integrations">>,
          capabilities => [
              <<"banking_data">>, <<"transaction_analysis">>, <<"financial_insights">>
          ],
          metadata => #{
              official => true,
              documentation_url => <<"https://plaid.com/docs/mcp">>,
              requires_auth => true
          }
        },
        #{id => <<"square">>,
          name => <<"Square">>,
          category => <<"Commerce">>,
          url => <<"https://mcp.squareup.com/sse">>,
          auth_type => oauth2,
          maintainer => <<"Square">>,
          description => <<"Use an agent to build on Square APIs. Payments, inventory, orders, and more">>,
          capabilities => [
              <<"payment_processing">>, <<"inventory_management">>, <<"order_management">>
          ],
          metadata => #{
              official => true,
              documentation_url => <<"https://developer.squareup.com/mcp">>,
              requires_auth => true
          }
        },
        #{id => <<"workato">>,
          name => <<"Workato">>,
          category => <<"Automation">>,
          url => <<"https://mcp.workato.com/sse">>,
          auth_type => oauth2,
          maintainer => <<"Workato">>,
          description => <<"Access any application, workflows or data via Workato, made accessible for AI">>,
          capabilities => [
              <<"workflow_automation">>, <<"data_integration">>, <<"app_connectivity">>
          ],
          metadata => #{
              official => true,
              documentation_url => <<"https://docs.workato.com/mcp">>,
              requires_auth => true,
              programmatic_generation => true
          }
        },
        #{id => <<"zapier">>,
          name => <<"Zapier">>,
          category => <<"Automation">>,
          url => <<"https://mcp.zapier.com/">>,
          auth_type => oauth2,
          maintainer => <<"Zapier">>,
          description => <<"Connect to nearly 8,000 apps through Zapier's automation platform">>,
          capabilities => [
              <<"app_integration">>, <<"workflow_automation">>, <<"trigger_management">>
          ],
          metadata => #{
              official => true,
              documentation_url => <<"https://zapier.com/developer/mcp">>,
              requires_auth => true,
              supported_apps => 8000
          }
        },
        
        #{id => <<"graphlit">>,
          name => <<"Graphlit">>,
          category => <<"Knowledge Management">>,
          url => <<"npx -y graphlit-mcp-server">>,
          auth_type => api_key,
          maintainer => <<"Graphlit">>,
          description => <<"Ingest anything from Slack, Discord, websites, Google Drive, email, Jira, Linear or GitHub into a searchable, RAG-ready knowledge base. Supports web crawling, search, and retrieval with no additional tools needed.">>,
          metadata => #{
              <<"command">> => <<"npx">>,
              <<"args">> => [<<"-y">>, <<"graphlit-mcp-server">>],
              <<"required_env">> => [<<"GRAPHLIT_ORGANIZATION_ID">>, <<"GRAPHLIT_ENVIRONMENT_ID">>, <<"GRAPHLIT_JWT_SECRET">>],
              <<"optional_env">> => [
                  <<"SLACK_BOT_TOKEN">>, <<"DISCORD_BOT_TOKEN">>, <<"TWITTER_TOKEN">>,
                  <<"GOOGLE_EMAIL_REFRESH_TOKEN">>, <<"GOOGLE_EMAIL_CLIENT_ID">>, <<"GOOGLE_EMAIL_CLIENT_SECRET">>,
                  <<"LINEAR_API_KEY">>, <<"GITHUB_PERSONAL_ACCESS_TOKEN">>, 
                  <<"JIRA_EMAIL">>, <<"JIRA_TOKEN">>, <<"NOTION_API_KEY">>
              ],
              <<"api_url">> => <<"https://data-scus.graphlit.io/api/v1/graphql">>,
              <<"organization_id">> => <<"1b591b0d-eb12-4f6d-be5a-ceb95b40e716">>,
              <<"environment_id">> => <<"42d0e0ef-8eeb-463d-921c-ef5119541eb9">>,
              <<"capabilities">> => [
                  <<"query_contents">>, <<"query_collections">>, <<"query_feeds">>,
                  <<"retrieve_relevant_sources">>, <<"extract_structured_json">>,
                  <<"ingest_files">>, <<"ingest_web_pages">>, <<"web_crawling">>,
                  <<"web_search">>, <<"slack_integration">>, <<"discord_integration">>,
                  <<"github_integration">>, <<"notion_integration">>, <<"linear_integration">>
              ]
          }},
        
        #{id => <<"github_mcp">>,
          name => <<"GitHub MCP">>,
          category => <<"Software Development">>,
          url => <<"npx -y @modelcontextprotocol/server-github">>,
          auth_type => api_key,
          maintainer => <<"Model Context Protocol">>,
          description => <<"Official GitHub MCP server for repository management, issue tracking, and code collaboration">>,
          capabilities => [
              <<"repository_access">>, <<"issue_management">>, <<"pull_requests">>, <<"code_search">>
          ],
          metadata => #{
              <<"command">> => <<"npx">>,
              <<"args">> => [<<"-y">>, <<"@modelcontextprotocol/server-github">>],
              <<"required_env">> => [<<"GITHUB_PERSONAL_ACCESS_TOKEN">>],
              <<"transport">> => <<"stdio">>,
              <<"official">> => true,
              <<"documentation_url">> => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/github">>
          }},
        
        #{id => <<"filesystem_mcp">>,
          name => <<"Filesystem MCP">>,
          category => <<"File Management">>,
          url => <<"npx -y @modelcontextprotocol/server-filesystem">>,
          auth_type => open,
          maintainer => <<"Model Context Protocol">>,
          description => <<"Official MCP server for safe filesystem operations with configurable access controls">>,
          capabilities => [
              <<"file_read">>, <<"file_write">>, <<"directory_listing">>, <<"file_search">>
          ],
          metadata => #{
              <<"command">> => <<"npx">>,
              <<"args">> => [<<"-y">>, <<"@modelcontextprotocol/server-filesystem">>, <<"/path/to/allowed/directory">>],
              <<"transport">> => <<"stdio">>,
              <<"official">> => true,
              <<"documentation_url">> => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/filesystem">>
          }},
        
        #{id => <<"postgres_mcp">>,
          name => <<"PostgreSQL MCP">>,
          category => <<"Database">>,
          url => <<"npx -y @modelcontextprotocol/server-postgres">>,
          auth_type => api_key,
          maintainer => <<"Model Context Protocol">>,
          description => <<"Official PostgreSQL MCP server for database operations and schema management">>,
          capabilities => [
              <<"sql_queries">>, <<"schema_inspection">>, <<"table_operations">>, <<"data_analysis">>
          ],
          metadata => #{
              <<"command">> => <<"npx">>,
              <<"args">> => [<<"-y">>, <<"@modelcontextprotocol/server-postgres">>],
              <<"required_env">> => [<<"POSTGRES_CONNECTION_STRING">>],
              <<"transport">> => <<"stdio">>,
              <<"official">> => true,
              <<"documentation_url">> => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/postgres">>
          }},
        
        #{id => <<"slack_mcp">>,
          name => <<"Slack MCP">>,
          category => <<"Communication">>,
          url => <<"npx -y @modelcontextprotocol/server-slack">>,
          auth_type => oauth2,
          maintainer => <<"Model Context Protocol">>,
          description => <<"Official Slack MCP server for team communication and workspace management">>,
          capabilities => [
              <<"message_sending">>, <<"channel_management">>, <<"user_lookup">>, <<"file_sharing">>
          ],
          metadata => #{
              <<"command">> => <<"npx">>,
              <<"args">> => [<<"-y">>, <<"@modelcontextprotocol/server-slack">>],
              <<"required_env">> => [<<"SLACK_BOT_TOKEN">>, <<"SLACK_TEAM_ID">>],
              <<"transport">> => <<"stdio">>,
              <<"official">> => true,
              <<"documentation_url">> => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/slack">>
          }},
        
        #{id => <<"brave_search_mcp">>,
          name => <<"Brave Search MCP">>,
          category => <<"Search">>,
          url => <<"npx -y @modelcontextprotocol/server-brave-search">>,
          auth_type => api_key,
          maintainer => <<"Model Context Protocol">>,
          description => <<"Official Brave Search MCP server for web search and information retrieval">>,
          capabilities => [
              <<"web_search">>, <<"news_search">>, <<"image_search">>, <<"video_search">>
          ],
          metadata => #{
              <<"command">> => <<"npx">>,
              <<"args">> => [<<"-y">>, <<"@modelcontextprotocol/server-brave-search">>],
              <<"required_env">> => [<<"BRAVE_API_KEY">>],
              <<"transport">> => <<"stdio">>,
              <<"official">> => true,
              <<"documentation_url">> => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/brave-search">>
          }},
        
        #{id => <<"puppeteer_mcp">>,
          name => <<"Puppeteer MCP">>,
          category => <<"Web Automation">>,
          url => <<"npx -y @modelcontextprotocol/server-puppeteer">>,
          auth_type => open,
          maintainer => <<"Model Context Protocol">>,
          description => <<"Official Puppeteer MCP server for web scraping and browser automation">>,
          capabilities => [
              <<"web_scraping">>, <<"screenshot_capture">>, <<"pdf_generation">>, <<"form_interaction">>
          ],
          metadata => #{
              <<"command">> => <<"npx">>,
              <<"args">> => [<<"-y">>, <<"@modelcontextprotocol/server-puppeteer">>],
              <<"transport">> => <<"stdio">>,
              <<"official">> => true,
              <<"documentation_url">> => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/puppeteer">>
          }},
        
        #{id => <<"memory_mcp">>,
          name => <<"Memory MCP">>,
          category => <<"Data Storage">>,
          url => <<"npx -y @modelcontextprotocol/server-memory">>,
          auth_type => open,
          maintainer => <<"Model Context Protocol">>,
          description => <<"Official Memory MCP server for persistent data storage and retrieval">>,
          capabilities => [
              <<"data_storage">>, <<"knowledge_base">>, <<"vector_search">>, <<"semantic_retrieval">>
          ],
          metadata => #{
              <<"command">> => <<"npx">>,
              <<"args">> => [<<"-y">>, <<"@modelcontextprotocol/server-memory">>],
              <<"transport">> => <<"stdio">>,
              <<"official">> => true,
              <<"documentation_url">> => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/memory">>
          }},
        
        #{id => <<"fetch_mcp">>,
          name => <<"Fetch MCP">>,
          category => <<"HTTP Client">>,
          url => <<"npx -y @modelcontextprotocol/server-fetch">>,
          auth_type => open,
          maintainer => <<"Model Context Protocol">>,
          description => <<"Official Fetch MCP server for HTTP requests and API interactions">>,
          capabilities => [
              <<"http_requests">>, <<"api_calls">>, <<"data_fetching">>, <<"webhook_handling">>
          ],
          metadata => #{
              <<"command">> => <<"npx">>,
              <<"args">> => [<<"-y">>, <<"@modelcontextprotocol/server-fetch">>],
              <<"transport">> => <<"stdio">>,
              <<"official">> => true,
              <<"documentation_url">> => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/fetch">>
          }}
    ].

get_all_servers() ->
    F = fun() ->
        qlc:e(qlc:q([Server || Server <- mnesia:table(mcp_server)]))
    end,
    case mnesia:transaction(F) of
        {atomic, Servers} -> {ok, Servers};
        {aborted, Reason} -> {error, Reason}
    end.

add_server(ServerMap) ->
    ServerId = maps:get(id, ServerMap, <<"unknown">>),
    ServerName = maps:get(name, ServerMap, <<"Unnamed Server">>),
    ?LOG_INFO("Adding server: ~s (~s)", [ServerName, ServerId]),
    ?LOG_DEBUG("Server map keys: ~p", [maps:keys(ServerMap)]),
    
    % Convert auth_type from binary to atom if needed
    RawAuthType = maps:get(auth_type, ServerMap, open),
    AuthType = case RawAuthType of
        <<"oauth2">> -> oauth2;
        <<"api_key">> -> api_key;
        <<"open">> -> open;
        Type when is_atom(Type) -> Type;
        Type when is_binary(Type) -> 
            try binary_to_atom(Type) catch _:_ -> open end;
        _ -> open  % default
    end,
    ?LOG_DEBUG("Converted auth_type ~p -> ~p", [RawAuthType, AuthType]),
    
    % Convert status from binary to atom if needed
    RawStatus = maps:get(status, ServerMap, active),
    Status = case RawStatus of
        <<"active">> -> active;
        <<"inactive">> -> inactive;
        StatusAtom when is_atom(StatusAtom) -> StatusAtom;
        StatusBin when is_binary(StatusBin) -> 
            try binary_to_atom(StatusBin) catch _:_ -> active end;
        _ -> active  % default
    end,
    ?LOG_DEBUG("Converted status ~p -> ~p", [RawStatus, Status]),
    
    % Validate required fields
    RequiredFields = [id, name, category, url, maintainer],
    case validate_required_fields(ServerMap, RequiredFields) of
        ok ->
            ?LOG_DEBUG("All required fields present", []);
        {missing, Fields} ->
            ?LOG_ERROR("Missing required fields: ~p", [Fields]),
            {error, {missing_fields, Fields}}
    end,
    
    Server = #mcp_server{
        id = ServerId,
        name = ServerName,
        category = maps:get(category, ServerMap),
        url = maps:get(url, ServerMap),
        auth_type = AuthType,
        maintainer = maps:get(maintainer, ServerMap),
        description = maps:get(description, ServerMap, <<>>),
        capabilities = maps:get(capabilities, ServerMap, []),
        status = Status,
        last_checked = calendar:local_time(),
        metadata = maps:get(metadata, ServerMap, #{})
    },
    
    ?LOG_DEBUG("Constructed server record for: ~s", [ServerId]),
    F = fun() -> mnesia:write(Server) end,
    
    case mnesia:transaction(F) of
        {atomic, ok} -> 
            ?LOG_INFO("Successfully added server to database: ~s", [ServerId]),
            {ok, Server};
        {aborted, {already_exists, _}} ->
            ?LOG_INFO("Server ~s already exists in database", [ServerId]),
            {ok, Server};
        {aborted, Reason} -> 
            ?LOG_ERROR("Failed to add server ~s to database: ~p", [ServerId, Reason]),
            % Return ok to continue startup even if database fails
            case Reason of
                {node_not_running, _} ->
                    ?LOG_WARN("Database not available, continuing without persistence", []),
                    {ok, Server};
                _ ->
                    {error, Reason}
            end
    end.

update_server(ServerId, Updates) ->
    F = fun() ->
        case mnesia:read(mcp_server, ServerId) of
            [Server] ->
                UpdatedServer = update_server_fields(Server, Updates),
                mnesia:write(UpdatedServer),
                {ok, UpdatedServer};
            [] ->
                {error, not_found}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

delete_server(ServerId) ->
    F = fun() -> mnesia:delete({mcp_server, ServerId}) end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

get_server(ServerId) ->
    F = fun() -> mnesia:read(mcp_server, ServerId) end,
    case mnesia:transaction(F) of
        {atomic, [Server]} -> {ok, Server};
        {atomic, []} -> {error, not_found};
        {aborted, Reason} -> {error, Reason}
    end.

discover_capabilities(ServerId) ->
    case get_server(ServerId) of
        {ok, Server} ->
            case fetch_server_capabilities(Server#mcp_server.url, Server#mcp_server.auth_type) of
                {ok, Capabilities} ->
                    update_server(ServerId, #{capabilities => Capabilities, last_checked => calendar:local_time()});
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

get_server_capabilities(ServerId) ->
    case get_server(ServerId) of
        {ok, Server} -> {ok, Server#mcp_server.capabilities};
        Error -> Error
    end.

assign_server_to_agent(AgentId, ServerIds) ->
    Mapping = #agent_mcp_mapping{
        agent_id = AgentId,
        server_ids = ServerIds
    },
    F = fun() -> mnesia:write(Mapping) end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

get_agent_servers(AgentId) ->
    F = fun() ->
        case mnesia:read(agent_mcp_mapping, AgentId) of
            [#agent_mcp_mapping{server_ids = ServerIds}] ->
                Servers = lists:foldl(fun(ServerId, Acc) ->
                    case mnesia:read(mcp_server, ServerId) of
                        [Server] -> [Server | Acc];
                        [] -> Acc
                    end
                end, [], ServerIds),
                {ok, lists:reverse(Servers)};
            [] ->
                {ok, []}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_seed_servers() ->
    % Wait for tables to be available before loading seed servers
    mnesia:wait_for_tables([mcp_server], 5000),
    
    lists:foreach(fun(ServerMap) ->
        try
            case add_server(ServerMap) of
                {ok, _Server} ->
                    io:format("[MCP_CONFIG] Loaded seed server: ~s~n", [maps:get(name, ServerMap, <<"Unknown">>)]);
                {error, {aborted, {already_exists, _}}} ->
                    % Server already exists, that's fine
                    ok;
                {error, AddReason} ->
                    io:format("[WARNING] Failed to load seed server ~s: ~p~n", 
                             [maps:get(name, ServerMap, <<"Unknown">>), AddReason])
            end
        catch
            Error:Reason:Stack ->
                io:format("[ERROR] Exception loading seed server ~s: ~p:~p~n", 
                         [maps:get(name, ServerMap, <<"Unknown">>), Error, Reason]),
                io:format("[ERROR] Stack trace: ~p~n", [Stack])
        end
    end, get_seed_servers()).

update_server_fields(Server, Updates) ->
    Server#mcp_server{
        name = maps:get(name, Updates, Server#mcp_server.name),
        category = maps:get(category, Updates, Server#mcp_server.category),
        url = maps:get(url, Updates, Server#mcp_server.url),
        auth_type = maps:get(auth_type, Updates, Server#mcp_server.auth_type),
        maintainer = maps:get(maintainer, Updates, Server#mcp_server.maintainer),
        description = maps:get(description, Updates, Server#mcp_server.description),
        capabilities = maps:get(capabilities, Updates, Server#mcp_server.capabilities),
        status = maps:get(status, Updates, Server#mcp_server.status),
        last_checked = maps:get(last_checked, Updates, Server#mcp_server.last_checked),
        metadata = maps:get(metadata, Updates, Server#mcp_server.metadata)
    }.

fetch_server_capabilities(Url, AuthType) ->
    ?LOG_INFO("Fetching capabilities from server: ~s", [Url]),
    %% TODO: Implement actual MCP capability discovery
    %% This would make an HTTP request to the server's capability endpoint
    %% For now, return mock capabilities based on category
    ?LOG_DEBUG("Using mock capabilities (auth_type: ~p)", [AuthType]),
    {ok, [
        #{
            name => <<"list_tools">>,
            description => <<"List available tools">>,
            input_schema => #{}
        },
        #{
            name => <<"get_tool_info">>,
            description => <<"Get information about a specific tool">>,
            input_schema => #{
                type => object,
                properties => #{
                    tool_name => #{type => string}
                },
                required => [tool_name]
            }
        }
    ]}.

%% Convert MCP server configuration to Anthropic MCP connector format
to_anthropic_mcp_format(#mcp_server{id = Id, name = Name, url = Url, auth_type = AuthType, metadata = Metadata}) ->
    BaseServer = #{
        type => <<"url">>,
        url => Url,
        name => Name
    },
    
    % Add authorization token if available
    ServerWithAuth = case AuthType of
        oauth2 ->
            case maps:get(auth_token, Metadata, undefined) of
                undefined -> BaseServer;
                Token -> maps:put(authorization_token, Token, BaseServer)
            end;
        api_key ->
            case maps:get(api_key, Metadata, undefined) of
                undefined -> BaseServer;
                ApiKey -> maps:put(authorization_token, ApiKey, BaseServer)
            end;
        _ -> BaseServer
    end,
    
    % Add tool configuration if available
    case maps:get(tool_configuration, Metadata, undefined) of
        undefined -> ServerWithAuth;
        ToolConfig -> maps:put(tool_configuration, ToolConfig, ServerWithAuth)
    end.

%% Convert list of server records to Anthropic MCP connector format
to_anthropic_mcp_format_list(Servers) when is_list(Servers) ->
    [to_anthropic_mcp_format(Server) || Server <- Servers].

%% Get MCP servers in Anthropic connector format for specific agents
get_anthropic_mcp_servers(AgentIds) when is_list(AgentIds) ->
    F = fun() ->
        % Get agent-server mappings for specified agents
        Mappings = lists:flatten([
            qlc:e(qlc:q([Mapping || Mapping <- mnesia:table(agent_mcp_mapping),
                                    Mapping#agent_mcp_mapping.agent_id =:= AgentId]))
            || AgentId <- AgentIds
        ]),
        
        % Get unique server IDs
        ServerIds = lists:usort(lists:flatten([M#agent_mcp_mapping.server_ids || M <- Mappings])),
        
        % Get server records
        Servers = lists:flatten([
            case mnesia:read(mcp_server, ServerId) of
                [Server] -> [Server];
                [] -> []
            end
            || ServerId <- ServerIds
        ]),
        
        % Convert to Anthropic format
        to_anthropic_mcp_format_list(Servers)
    end,
    
    case mnesia:transaction(F) of
        {atomic, McpServers} -> {ok, McpServers};
        {aborted, Reason} -> {error, Reason}
    end;

get_anthropic_mcp_servers(AgentId) when is_binary(AgentId) ->
    get_anthropic_mcp_servers([AgentId]).

%% Get all active MCP servers in Anthropic connector format
get_all_anthropic_mcp_servers() ->
    case get_all_servers() of
        {ok, Servers} ->
            ActiveServers = [S || S <- Servers, S#mcp_server.status =:= active],
            {ok, to_anthropic_mcp_format_list(ActiveServers)};
        Error -> Error
    end.

%% Helper function to validate required fields
validate_required_fields(ServerMap, RequiredFields) ->
    MissingFields = [Field || Field <- RequiredFields, 
                              not maps:is_key(Field, ServerMap)],
    case MissingFields of
        [] -> ok;
        _ -> {missing, MissingFields}
    end.