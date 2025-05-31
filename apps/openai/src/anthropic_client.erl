%% anthropic_client.erl
%% Anthropic Claude API client with MCP connector support
-module(anthropic_client).

-behaviour(gen_server).

%% API exports
-export([
    start_link/0, 
    start_link/1, 
    stop/0,
    create_message/3,
    create_message/4,
    create_message_with_mcp/4,
    create_message_with_mcp/5
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(BASE_URL, "https://api.anthropic.com").
-define(ANTHROPIC_VERSION, "2023-06-01").
-define(MCP_BETA_HEADER, "mcp-client-2025-04-04").
-define(DEFAULT_TIMEOUT, 30000).
-define(DEFAULT_RETRY_COUNT, 3).
-define(DEFAULT_RETRY_DELAY, 1000).

-record(state, {
    api_key,
    base_url = ?BASE_URL,
    timeout = ?DEFAULT_TIMEOUT,
    retry_count = ?DEFAULT_RETRY_COUNT,
    retry_delay = ?DEFAULT_RETRY_DELAY,
    last_request_id,
    request_timestamp,
    active_requests = #{}
}).

%% =============================================================================
%% API Functions
%% =============================================================================

start_link() ->
    start_link(#{}).

start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

stop() ->
    gen_server:stop(?SERVER).

%% Create a message without MCP servers
-spec create_message(binary(), integer(), list()) -> {ok, map()} | {error, term()}.
create_message(Model, MaxTokens, Messages) ->
    create_message(Model, MaxTokens, Messages, #{}).

-spec create_message(binary(), integer(), list(), map()) -> {ok, map()} | {error, term()}.
create_message(Model, MaxTokens, Messages, Options) ->
    Params = maps:merge(#{
        model => Model,
        max_tokens => MaxTokens,
        messages => Messages
    }, Options),
    gen_server:call(?SERVER, {request, post, "/v1/messages", Params, []}, infinity).

%% Create a message with MCP servers
-spec create_message_with_mcp(binary(), integer(), list(), list()) -> {ok, map()} | {error, term()}.
create_message_with_mcp(Model, MaxTokens, Messages, McpServers) ->
    create_message_with_mcp(Model, MaxTokens, Messages, McpServers, #{}).

-spec create_message_with_mcp(binary(), integer(), list(), list(), map()) -> {ok, map()} | {error, term()}.
create_message_with_mcp(Model, MaxTokens, Messages, McpServers, Options) ->
    case validate_mcp_servers(McpServers) of
        ok ->
            Params = maps:merge(#{
                model => Model,
                max_tokens => MaxTokens,
                messages => Messages,
                mcp_servers => McpServers
            }, Options),
            gen_server:call(?SERVER, {mcp_request, post, "/v1/messages", Params, [?MCP_BETA_HEADER]}, infinity);
        {error, Reason} ->
            {error, {mcp_validation_failed, Reason}}
    end.

%% =============================================================================
%% Helper Functions
%% =============================================================================

validate_mcp_servers([]) -> ok;
validate_mcp_servers(McpServers) when is_list(McpServers) ->
    try
        lists:foreach(fun validate_mcp_server/1, McpServers),
        ok
    catch
        throw:{invalid_mcp_server, Reason} ->
            {error, Reason}
    end;
validate_mcp_servers(_) ->
    {error, mcp_servers_must_be_list}.

validate_mcp_server(#{type := <<"url">>, url := Url, name := Name} = Server) when is_binary(Url), is_binary(Name) ->
    % Validate URL format
    case binary:match(Url, <<"https://">>) of
        {0, _} -> ok;
        _ -> throw({invalid_mcp_server, {invalid_url, Url}})
    end,
    % Validate optional fields
    case maps:get(tool_configuration, Server, undefined) of
        undefined -> ok;
        ToolConfig when is_map(ToolConfig) -> ok;
        _ -> throw({invalid_mcp_server, invalid_tool_configuration})
    end;
validate_mcp_server(Server) ->
    throw({invalid_mcp_server, {missing_required_fields, Server}}).

replace_path_params(Path, Params) ->
    lists:foldl(
        fun({Key, Value}, AccPath) ->
            Pattern = ":" ++ atom_to_list(Key),
            re:replace(AccPath, Pattern, to_string(Value), [{return, list}, global])
        end,
        Path,
        maps:to_list(Params)
    ).

to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(V) when is_binary(V) -> binary_to_list(V);
to_string(V) when is_integer(V) -> integer_to_list(V);
to_string(V) when is_float(V) -> float_to_list(V, [{decimals, 10}, compact]);
to_string(V) when is_list(V) -> V.

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init(Options) ->
    ApiKey = maps:get(api_key, Options, os:getenv("ANTHROPIC_API_KEY")),
    BaseUrl = maps:get(base_url, Options, ?BASE_URL),
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    RetryCount = maps:get(retry_count, Options, ?DEFAULT_RETRY_COUNT),
    RetryDelay = maps:get(retry_delay, Options, ?DEFAULT_RETRY_DELAY),
    
    % Initialize HTTP clients
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    application:ensure_all_started(jsx),
    
    {ok, #state{
        api_key = ApiKey,
        base_url = BaseUrl,
        timeout = Timeout,
        retry_count = RetryCount,
        retry_delay = RetryDelay
    }}.

handle_call({request, Method, Path, Params, BetaHeaders}, From, State) ->
    handle_request(Method, Path, Params, BetaHeaders, From, State);

handle_call({mcp_request, Method, Path, Params, BetaHeaders}, From, State) ->
    handle_mcp_request(Method, Path, Params, BetaHeaders, From, State);

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_request(Method, Path, Params, BetaHeaders, From, State) ->
    RequestId = make_ref(),
    
    % Extract path parameters and replace in path
    PathParams = maps:filter(
        fun(K, _) -> lists:member($:, Path) andalso string:str(Path, ":" ++ atom_to_list(K)) > 0 end,
        Params
    ),
    ResolvedPath = replace_path_params(Path, PathParams),
    
    % Remove path parameters from the request body
    BodyParams = maps:without(maps:keys(PathParams), Params),
    
    % Construct request URL
    Url = State#state.base_url ++ ResolvedPath,
    
    % Prepare headers with Anthropic-specific headers
    BaseHeaders = [
        {"Content-Type", "application/json"},
        {"x-api-key", State#state.api_key},
        {"anthropic-version", ?ANTHROPIC_VERSION}
    ],
    
    % Add beta headers if specified
    BetaHeadersList = lists:map(
        fun(Header) -> {"anthropic-beta", Header} end,
        BetaHeaders
    ),
    
    Headers = BaseHeaders ++ BetaHeadersList,
    
    % Extract request options
    Timeout = maps:get(timeout, BodyParams, State#state.timeout),
    Stream = maps:get(stream, BodyParams, false),
    
    % Remove non-API parameters from body
    CleanBodyParams = maps:without([timeout, stream], BodyParams),
    
    % Make the request in a separate process
    Self = self(),
    spawn_link(fun() ->
        Result = make_anthropic_request(Method, Url, Headers, CleanBodyParams, Stream, Timeout, State#state.retry_count, State#state.retry_delay),
        gen_server:cast(Self, {request_complete, RequestId, From, Result})
    end),
    
    % Track active request
    NewActiveRequests = maps:put(RequestId, {From, os:timestamp()}, State#state.active_requests),
    {noreply, State#state{last_request_id = RequestId, active_requests = NewActiveRequests}}.

handle_mcp_request(Method, Path, Params, BetaHeaders, From, State) ->
    % Handle MCP-specific request with server validation and processing
    McpServers = maps:get(mcp_servers, Params, []),
    
    % Process MCP servers to ensure they're in the correct format
    ProcessedParams = process_mcp_params(Params),
    
    % Call regular request handler with processed parameters
    handle_request(Method, Path, ProcessedParams, BetaHeaders, From, State).

process_mcp_params(Params) ->
    case maps:get(mcp_servers, Params, []) of
        [] -> Params;
        McpServers ->
            % Ensure MCP servers are in correct format for API
            ProcessedServers = lists:map(fun process_mcp_server/1, McpServers),
            maps:put(mcp_servers, ProcessedServers, Params)
    end.

process_mcp_server(#{type := <<"url">>, url := Url, name := Name} = Server) ->
    % Ensure all required fields are present and in correct format
    BaseServer = #{type => <<"url">>, url => Url, name => Name},
    
    % Add optional fields if present
    ServerWithAuth = case maps:get(authorization_token, Server, undefined) of
        undefined -> BaseServer;
        Token -> maps:put(authorization_token, Token, BaseServer)
    end,
    
    case maps:get(tool_configuration, Server, undefined) of
        undefined -> ServerWithAuth;
        ToolConfig -> maps:put(tool_configuration, ToolConfig, ServerWithAuth)
    end.

handle_cast({request_complete, RequestId, From, Result}, State) ->
    % Reply to the caller
    gen_server:reply(From, Result),
    
    % Remove from active requests
    NewActiveRequests = maps:remove(RequestId, State#state.active_requests),
    {noreply, State#state{active_requests = NewActiveRequests}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internal Functions
%% =============================================================================

make_anthropic_request(Method, Url, Headers, Params, Stream, Timeout, MaxRetries, RetryDelay) ->
    make_anthropic_request(Method, Url, Headers, Params, Stream, Timeout, MaxRetries, RetryDelay, 0).

make_anthropic_request(_Method, _Url, _Headers, _Params, _Stream, _Timeout, MaxRetries, _RetryDelay, RetryCount) when RetryCount > MaxRetries ->
    {error, max_retries_exceeded};

make_anthropic_request(Method, Url, Headers, Params, Stream, Timeout, MaxRetries, RetryDelay, RetryCount) ->
    RequestBody = case Params of
        _ when map_size(Params) =:= 0 andalso (Method =:= get orelse Method =:= delete) -> "";
        _ -> jsx:encode(Params)
    end,
    
    % Convert method to uppercase string
    MethodStr = string:uppercase(atom_to_list(Method)),
    
    HttpOptions = [{timeout, Timeout}],
    Options = [],
    
    Result = case Method of
        get -> httpc:request(get, {Url, Headers}, HttpOptions, Options);
        delete -> httpc:request(delete, {Url, Headers}, HttpOptions, Options);
        _ -> httpc:request(MethodStr, {Url, Headers, "application/json", RequestBody}, HttpOptions, Options)
    end,
    
    case Result of
        {ok, {{_, 200, _}, ResponseHeaders, ResponseBody}} ->
            try
                ResponseJson = jsx:decode(list_to_binary(ResponseBody), [return_maps]),
                {ok, ResponseJson}
            catch
                _:_ -> {error, json_decode_failed}
            end;
        {ok, {{_, 429, _}, _, _}} ->
            % Rate limited, retry after delay
            timer:sleep(RetryDelay * (RetryCount + 1)),
            make_anthropic_request(Method, Url, Headers, Params, Stream, Timeout, MaxRetries, RetryDelay, RetryCount + 1);
        {ok, {{_, StatusCode, Reason}, _, ResponseBody}} ->
            % Try to parse error response as JSON
            ErrorDetails = try
                jsx:decode(list_to_binary(ResponseBody), [return_maps])
            catch
                _:_ -> ResponseBody
            end,
            {error, {StatusCode, Reason, ErrorDetails}};
        {error, Reason} ->
            case Reason of
                timeout ->
                    % Timeout, retry if not exceeding max retries
                    timer:sleep(RetryDelay),
                    make_anthropic_request(Method, Url, Headers, Params, Stream, Timeout, MaxRetries, RetryDelay, RetryCount + 1);
                _ ->
                    {error, Reason}
            end
    end.