%%% @doc Comprehensive type specifications for all API endpoints
%%% This file defines all data types used across the API surface

-ifndef(API_TYPE_SPECS_HRL).
-define(API_TYPE_SPECS_HRL, true).

%% =============================================================================
%% CORE TYPES
%% =============================================================================

-type agent_id() :: binary().
-type agent_type() :: simple | ai | template | super | quantum.
-type agent_status() :: active | idle | error | dead | initializing.
-type timestamp() :: integer(). % Unix timestamp in milliseconds
-type binary_string() :: binary().
-type json_object() :: #{binary() => term()}.

%% =============================================================================
%% AGENT TYPES
%% =============================================================================

-type agent_meta() :: #{
    name => binary_string(),
    type => agent_type(),
    model => binary_string(),
    created_at => timestamp(),
    last_activity => timestamp(),
    tools => [binary_string()],
    capabilities => [binary_string()],
    config => json_object()
}.

-type agent_info() :: #{
    id := agent_id(),
    pid := pid(),
    status := agent_status(),
    memory => integer(),
    message_queue_len => integer(),
    health_score => 0..100,
    meta := agent_meta()
}.

-type create_agent_request() :: #{
    type := agent_type(),
    name => binary_string(),
    model => binary_string(),
    tools => [binary_string()],
    config => json_object()
}.

-type agent_response() :: #{
    success := boolean(),
    agent => agent_info(),
    error => binary_string(),
    error_type => binary_string()
}.

-type agents_list_response() :: #{
    agents := [agent_info()],
    total := integer(),
    page := integer(),
    page_size := integer(),
    has_next := boolean(),
    has_prev := boolean()
}.

%% =============================================================================
%% CHAT TYPES
%% =============================================================================

-type chat_message() :: #{
    role := user | assistant | system,
    content := binary_string(),
    timestamp => timestamp(),
    agent_id => agent_id(),
    conversation_id => binary_string(),
    metadata => json_object()
}.

-type chat_request() :: #{
    message := binary_string(),
    stream => boolean(),
    conversation_id => binary_string(),
    context => json_object(),
    tools => [binary_string()]
}.

-type chat_response() :: #{
    response := binary_string(),
    agent_id := agent_id(),
    timestamp := timestamp(),
    conversation_id => binary_string(),
    usage => usage_stats(),
    tool_calls => [tool_call()]
}.

-type tool_call() :: #{
    id := binary_string(),
    type := function,
    function := #{
        name := binary_string(),
        arguments := binary_string()
    }
}.

-type usage_stats() :: #{
    prompt_tokens => integer(),
    completion_tokens => integer(),
    total_tokens => integer(),
    cost => float()
}.

%% =============================================================================
%% WEBSOCKET MESSAGE TYPES
%% =============================================================================

-type ws_message_type() :: 
    create_stream | stream_chat | get_system_metrics | get_agent_metrics |
    subscribe_monitoring | run_example | client_log | button_click |
    log_interaction | log_error | ping.

-type ws_inbound_message() :: #{
    type := ws_message_type(),
    agent_id => agent_id(),
    message => binary_string(),
    data => json_object(),
    timestamp => timestamp()
}.

-type ws_outbound_message() :: #{
    type := binary_string(),
    data => term(),
    agent_id => agent_id(),
    timestamp => timestamp()
}.

%% =============================================================================
%% SYSTEM HEALTH TYPES
%% =============================================================================

-type system_status() :: healthy | degraded | critical.

-type system_metrics() :: #{
    node := binary_string(),
    uptime := integer(),
    total_memory := integer(),
    used_memory := integer(),
    process_count := integer(),
    run_queue := integer(),
    schedulers := integer(),
    cpu_usage := 0..100,
    memory_usage := 0..100
}.

-type system_health() :: #{
    status := system_status(),
    metrics := system_metrics(),
    agents_count := integer(),
    active_connections := integer(),
    errors_count := integer(),
    last_error => binary_string()
}.

%% =============================================================================
%% MCP (MODEL CONTEXT PROTOCOL) TYPES
%% =============================================================================

-type mcp_server_status() :: connected | disconnected | error | connecting.

-type mcp_capability() :: 
    tools | resources | prompts | logging | sampling | roots.

-type mcp_server() :: #{
    id := binary_string(),
    name := binary_string(),
    description => binary_string(),
    status := mcp_server_status(),
    capabilities := [mcp_capability()],
    config := json_object(),
    transport := stdio | sse | websocket,
    last_seen => timestamp(),
    error_count => integer()
}.

-type mcp_tool() :: #{
    name := binary_string(),
    description => binary_string(),
    input_schema := json_object()
}.

-type mcp_resource() :: #{
    uri := binary_string(),
    name := binary_string(),
    description => binary_string(),
    mime_type => binary_string()
}.

-type mcp_prompt() :: #{
    name := binary_string(),
    description => binary_string(),
    arguments => [json_object()]
}.

%% =============================================================================
%% FLEET MANAGEMENT TYPES
%% =============================================================================

-type fleet_metrics() :: #{
    total_agents := integer(),
    active_agents := integer(),
    idle_agents := integer(),
    error_agents := integer(),
    total_messages := integer(),
    messages_per_second := float(),
    avg_response_time := float(),
    memory_usage := integer(),
    cpu_usage := float()
}.

-type fleet_status() :: #{
    agents := [agent_info()],
    metrics := fleet_metrics(),
    timestamp := timestamp(),
    health_summary := #{
        healthy := integer(),
        degraded := integer(),
        critical := integer()
    }
}.

%% =============================================================================
%% ERROR AND MONITORING TYPES
%% =============================================================================

-type error_level() :: debug | info | warning | error | critical.

-type crash_report() :: #{
    id := binary_string(),
    timestamp := timestamp(),
    module := binary_string(),
    function := binary_string(),
    arity := integer(),
    reason := term(),
    stack_trace := [term()],
    process_info := json_object(),
    analysis => binary_string(),
    suggested_fixes => [binary_string()],
    severity := error_level()
}.

-type error_interpretation() :: #{
    id := binary_string(),
    error_data := json_object(),
    interpretation := binary_string(),
    confidence := float(),
    suggestions := [binary_string()],
    category := binary_string(),
    timestamp := timestamp()
}.

-type monitoring_update() :: #{
    agents := [agent_info()],
    system := system_metrics(),
    errors := [crash_report()],
    timestamp := timestamp()
}.

%% =============================================================================
%% WORKFLOW ORCHESTRATION TYPES
%% =============================================================================

-type workflow_step() :: #{
    id := binary_string(),
    type := agent_call | parallel | sequence | condition,
    agent_id => agent_id(),
    input := json_object(),
    output => json_object(),
    status := pending | running | completed | failed,
    error => binary_string()
}.

-type workflow() :: #{
    id := binary_string(),
    name := binary_string(),
    description => binary_string(),
    steps := [workflow_step()],
    status := pending | running | completed | failed,
    created_at := timestamp(),
    completed_at => timestamp(),
    result => json_object()
}.

%% =============================================================================
%% AUTHENTICATION AND API KEYS
%% =============================================================================

-type api_key_requirement() :: #{
    required := boolean(),
    description := binary_string(),
    env_var => binary_string(),
    service := binary_string()
}.

-type api_key_status() :: #{
    service := binary_string(),
    configured := boolean(),
    valid := boolean(),
    last_checked => timestamp(),
    error => binary_string()
}.

-type api_keys_response() :: #{
    requirements := #{binary_string() => api_key_requirement()},
    status := #{binary_string() => api_key_status()},
    missing := [binary_string()]
}.

%% =============================================================================
%% BULK OPERATIONS TYPES
%% =============================================================================

-type bulk_operation_type() :: 
    broadcast | system_prompt | callback | agent_management | 
    query | transform | batch_chat.

-type bulk_request() :: #{
    operation := bulk_operation_type(),
    targets := [agent_id()] | all,
    data := json_object(),
    options => json_object()
}.

-type bulk_response() :: #{
    operation_id := binary_string(),
    status := pending | running | completed | failed,
    results := [json_object()],
    errors := [json_object()],
    summary := #{
        total := integer(),
        successful := integer(),
        failed := integer(),
        duration_ms := integer()
    }
}.

%% =============================================================================
%% CONVERSATION TYPES
%% =============================================================================

-type conversation() :: #{
    id := binary_string(),
    title => binary_string(),
    agents := [agent_id()],
    messages := [chat_message()],
    created_at := timestamp(),
    updated_at := timestamp(),
    metadata => json_object()
}.

-type conversation_summary() :: #{
    id := binary_string(),
    title => binary_string(),
    agent_count := integer(),
    message_count := integer(),
    last_message_at => timestamp(),
    created_at := timestamp()
}.

%% =============================================================================
%% MODEL MANAGEMENT TYPES
%% =============================================================================

-type model_provider() :: openai | anthropic | azure | local.

-type model_definition() :: #{
    id := binary_string(),
    name := binary_string(),
    provider := model_provider(),
    max_tokens := integer(),
    supports_streaming := boolean(),
    supports_tools := boolean(),
    cost_per_token := #{
        input := float(),
        output := float()
    },
    capabilities := [binary_string()]
}.

%% =============================================================================
%% PAGINATION TYPES
%% =============================================================================

-type pagination_params() :: #{
    page := integer(),
    page_size := integer(),
    offset := integer(),
    total => integer()
}.

-type paginated_response(T) :: #{
    data := [T],
    pagination := #{
        page := integer(),
        page_size := integer(),
        total := integer(),
        has_next := boolean(),
        has_prev := boolean()
    }
}.

%% =============================================================================
%% HTTP RESPONSE TYPES
%% =============================================================================

-type http_method() :: get | post | put | patch | delete | options | head.

-type api_error() :: #{
    success := false,
    error := binary_string(),
    error_type := binary_string(),
    details => json_object(),
    timestamp := timestamp()
}.

-type api_success(T) :: #{
    success := true,
    data := T,
    timestamp => timestamp(),
    meta => json_object()
}.

-type api_response(T) :: api_success(T) | api_error().

%% =============================================================================
%% COST TRACKING TYPES
%% =============================================================================

-type cost_entry() :: #{
    id := binary_string(),
    agent_id := agent_id(),
    model := binary_string(),
    operation := binary_string(),
    input_tokens := integer(),
    output_tokens := integer(),
    cost := float(),
    timestamp := timestamp(),
    metadata => json_object()
}.

-type cost_summary() :: #{
    total_cost := float(),
    total_tokens := integer(),
    operations_count := integer(),
    by_model := #{binary_string() => float()},
    by_agent := #{agent_id() => float()},
    timeframe := {timestamp(), timestamp()}
}.

%% =============================================================================
%% OAUTH AND EXTERNAL INTEGRATIONS
%% =============================================================================

-type oauth_provider() :: github | google | pipedream | claude.

-type oauth_config() :: #{
    client_id := binary_string(),
    client_secret := binary_string(),
    redirect_uri := binary_string(),
    scopes := [binary_string()]
}.

-type oauth_token() :: #{
    access_token := binary_string(),
    refresh_token => binary_string(),
    expires_at => timestamp(),
    token_type := binary_string(),
    scope => binary_string()
}.

%% =============================================================================
%% STREAMING TYPES
%% =============================================================================

-type stream_event_type() :: 
    start | token | function_call | tool_call | error | complete.

-type stream_event() :: #{
    type := stream_event_type(),
    data => term(),
    timestamp := timestamp(),
    agent_id => agent_id(),
    conversation_id => binary_string()
}.

-endif. % API_TYPE_SPECS_HRL