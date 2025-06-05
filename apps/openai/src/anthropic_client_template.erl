%% anthropic_client_template.erl
%% Template module for generating Anthropic Claude API client modules with MCP connector support
-module(anthropic_client_template).

-export([
    generate_module/2,
    generate_module_source/2
]).

-define(BASE_URL, "https://api.anthropic.com").
-define(ANTHROPIC_VERSION, "2023-06-01").
-define(MCP_BETA_HEADER, "mcp-client-2025-04-04").

%% Generate and compile a module for the given endpoint group
-spec generate_module(atom(), string()) -> {ok, module()} | {error, term()}.
generate_module(GroupName, OutputDir) ->
    ModuleName = list_to_atom("anthropic_" ++ atom_to_list(GroupName)),
    Source = generate_module_source(GroupName, ModuleName),
    Filename = OutputDir ++ "/" ++ atom_to_list(ModuleName) ++ ".erl",
    
    % Ensure directory exists
    filelib:ensure_dir(Filename),
    
    % Write the module source to file
    case file:write_file(Filename, Source) of
        ok ->
            % Compile the module using standard compile:file
            case compile:file(Filename, [return_errors, binary]) of
                {ok, Module, Bin} -> 
                    % Load the binary
                    case code:load_binary(Module, atom_to_list(Module) ++ ".erl", Bin) of
                        {module, _} -> {ok, Module};
                        Error -> {error, {load_error, Error}}
                    end;
                {error, Errors, _Warnings} -> 
                    {error, {compile_errors, Errors}}
            end;
        {error, Reason} ->
            {error, {file_write_error, Reason}}
    end.

%% Generate the source code for a module
-spec generate_module_source(atom(), atom()) -> binary().
generate_module_source(GroupName, ModuleName) ->
    % Get all endpoints for this group
    Endpoints = anthropic_api_structure:get_endpoints(GroupName),
    
    % Generate the module header
    Header = module_header(ModuleName),
    
    % Generate exports
    Exports = generate_exports(Endpoints),
    
    % Generate function implementations
    Functions = generate_functions(Endpoints),
    
    % Generate helper functions
    Helpers = generate_helpers(),
    
    % Combine all parts
    list_to_binary([Header, Exports, Functions, Helpers]).

%% Generate the module header
module_header(ModuleName) ->
    list_to_binary([
        "%% ", atom_to_list(ModuleName), ".erl\n",
        "%% Auto-generated Anthropic Claude API client module with MCP connector support\n",
        "-module(", atom_to_list(ModuleName), ").\n\n",
        "-behaviour(gen_server).\n\n",
        "%% API exports\n",
        "-export([start_link/0, start_link/1, stop/0]).\n\n",
        "%% gen_server callbacks\n",
        "-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).\n\n",
        "-define(SERVER, ?MODULE).\n",
        "-define(BASE_URL, \"", ?BASE_URL, "\").\n",
        "-define(ANTHROPIC_VERSION, \"", ?ANTHROPIC_VERSION, "\").\n",
        "-define(MCP_BETA_HEADER, \"", ?MCP_BETA_HEADER, "\").\n",
        "-define(DEFAULT_TIMEOUT, 30000).\n",
        "-define(DEFAULT_RETRY_COUNT, 3).\n",
        "-define(DEFAULT_RETRY_DELAY, 1000).\n\n",
        "-record(state, {\n",
        "    api_key,\n",
        "    base_url = ?BASE_URL,\n",
        "    timeout = ?DEFAULT_TIMEOUT,\n",
        "    retry_count = ?DEFAULT_RETRY_COUNT,\n",
        "    retry_delay = ?DEFAULT_RETRY_DELAY,\n",
        "    last_request_id,\n",
        "    request_timestamp,\n",
        "    active_requests = #{},\n",
        "    mcp_servers = []\n",
        "}).\n\n"
    ]).

%% Generate the exports for each endpoint function
generate_exports(Endpoints) ->
    ExportLines = maps:fold(
        fun(EndpointName, EndpointSpec, Acc) ->
            % Calculate arity based on required and optional params
            ReqParams = maps:get(required_params, EndpointSpec, []),
            _OptParams = maps:get(optional_params, EndpointSpec, []),
            Arity = length(ReqParams) + 1, % +1 for options map
            
            % Create export line
            ExportLine = io_lib:format("-export([~s/~B]).\n", [EndpointName, Arity]),
            [ExportLine | Acc]
        end,
        [],
        Endpoints
    ),
    
    list_to_binary([
        "%% Endpoint function exports\n",
        ExportLines,
        "\n"
    ]).

%% Generate the function implementations for each endpoint
generate_functions(Endpoints) ->
    FunctionImpls = maps:fold(
        fun(EndpointName, EndpointSpec, Acc) ->
            Method = maps:get(method, EndpointSpec, get),
            Path = maps:get(path, EndpointSpec, ""),
            Description = maps:get(description, EndpointSpec, ""),
            ReqParams = maps:get(required_params, EndpointSpec, []),
            OptParams = maps:get(optional_params, EndpointSpec, []),
            BetaHeaders = maps:get(beta_headers, EndpointSpec, []),
            McpSupport = maps:get(mcp_support, EndpointSpec, false),
            
            % Generate function spec and implementation
            FuncImpl = generate_function(EndpointName, Method, Path, Description, ReqParams, OptParams, BetaHeaders, McpSupport),
            [FuncImpl | Acc]
        end,
        [],
        Endpoints
    ),
    
    % Combine all function implementations
    list_to_binary([
        "%% =============================================================================\n",
        "%% API Functions\n",
        "%% =============================================================================\n\n",
        "%% Server lifecycle functions\n",
        "start_link() ->\n",
        "    start_link(#{}).\n\n",
        "start_link(Options) ->\n",
        "    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).\n\n",
        "stop() ->\n",
        "    gen_server:stop(?SERVER).\n\n",
        "%% Endpoint functions\n",
        FunctionImpls,
        "\n"
    ]).

%% Generate a single function implementation
generate_function(EndpointName, Method, Path, Description, ReqParams, _OptParams, BetaHeaders, McpSupport) ->
    % Generate function arguments
    ArgNames = [atom_to_list(Param) || Param <- ReqParams],
    Args = string:join(ArgNames ++ ["Options"], ", "),
    
    % Generate function spec
    Spec = io_lib:format(
        "-spec ~s(~s) -> {ok, map()} | {error, term()}.\n",
        [EndpointName, string:join([atom_to_list(Param) ++ ":term()" || Param <- ReqParams] ++ ["Options::map()"], ", ")]
    ),
    
    % Create request map with actual variable names (not atoms)
    RequestMapItems = [io_lib:format("~s => ~s", [Param, atom_to_list(Param)]) || Param <- ReqParams],
    RequestMap = 
        case RequestMapItems of
            [] -> "OptionsWithDefaults";
            _ -> io_lib:format("maps:merge(OptionsWithDefaults, #{~s})", [string:join(RequestMapItems, ", ")])
        end,
    
    % Generate function body with MCP support
    Body = case McpSupport of
        true ->
            io_lib:format(
                "~s(~s) ->\n"
                "    %% ~s\n"
                "    OptionsWithDefaults = ensure_defaults(Options),\n"
                "    RequestParams = ~s,\n"
                "    McpServers = maps:get(mcp_servers, RequestParams, []),\n"
                "    % Validate MCP servers before making request\n"
                "    case validate_mcp_servers(McpServers) of\n"
                "        ok ->\n"
                "            gen_server:call(?SERVER, {mcp_request, ~p, ~p, RequestParams, ~p}, infinity);\n"
                "        {error, Reason} ->\n"
                "            {error, {mcp_validation_failed, Reason}}\n"
                "    end.\n\n",
                [EndpointName, Args, Description, RequestMap, Method, Path, BetaHeaders]
            );
        false ->
            io_lib:format(
                "~s(~s) ->\n"
                "    %% ~s\n"
                "    OptionsWithDefaults = ensure_defaults(Options),\n"
                "    RequestParams = ~s,\n"
                "    gen_server:call(?SERVER, {request, ~p, ~p, RequestParams, ~p}, infinity).\n\n",
                [EndpointName, Args, Description, RequestMap, Method, Path, BetaHeaders]
            )
    end,
    
    list_to_binary([Spec, Body]).

%% Generate helper functions
generate_helpers() ->
    list_to_binary([
        "%% =============================================================================\n",
        "%% Helper Functions\n",
        "%% =============================================================================\n\n",
        "ensure_defaults(Options) ->\n",
        "    maps:merge(#{stream => false, timeout => ?DEFAULT_TIMEOUT}, Options).\n\n",
        "validate_mcp_servers([]) -> ok;\n",
        "validate_mcp_servers(McpServers) when is_list(McpServers) ->\n",
        "    try\n",
        "        lists:foreach(fun validate_mcp_server/1, McpServers),\n",
        "        ok\n",
        "    catch\n",
        "        throw:{invalid_mcp_server, Reason} ->\n",
        "            {error, Reason}\n",
        "    end;\n",
        "validate_mcp_servers(_) ->\n",
        "    {error, mcp_servers_must_be_list}.\n\n",
        "validate_mcp_server(#{type := <<\"url\">>, url := Url, name := Name} = Server) when is_binary(Url), is_binary(Name) ->\n",
        "    % Validate URL format\n",
        "    case binary:match(Url, <<\\\"https://\\\">>) of\n",
        "        {0, _} -> ok;\n",
        "        _ -> throw({invalid_mcp_server, {invalid_url, Url}})\n",
        "    end,\n",
        "    % Validate optional fields\n",
        "    case maps:get(tool_configuration, Server, undefined) of\n",
        "        undefined -> ok;\n",
        "        ToolConfig when is_map(ToolConfig) -> ok;\n",
        "        _ -> throw({invalid_mcp_server, invalid_tool_configuration})\n",
        "    end;\n",
        "validate_mcp_server(Server) ->\n",
        "    throw({invalid_mcp_server, {missing_required_fields, Server}}).\n\n",
        "replace_path_params(Path, Params) ->\n",
        "    lists:foldl(\n",
        "        fun({Key, Value}, AccPath) ->\n",
        "            Pattern = \":\" ++ atom_to_list(Key),\n",
        "            re:replace(AccPath, Pattern, to_string(Value), [{return, list}, global])\n",
        "        end,\n",
        "        Path,\n",
        "        maps:to_list(Params)\n",
        "    ).\n\n",
        "to_string(V) when is_atom(V) -> atom_to_list(V);\n",
        "to_string(V) when is_binary(V) -> binary_to_list(V);\n",
        "to_string(V) when is_integer(V) -> integer_to_list(V);\n",
        "to_string(V) when is_float(V) -> float_to_list(V, [{decimals, 10}, compact]);\n",
        "to_string(V) when is_list(V) -> V.\n\n",
        "%% =============================================================================\n",
        "%% gen_server callbacks\n",
        "%% =============================================================================\n\n",
        "init(Options) ->\n",
        "    ApiKey = maps:get(api_key, Options, os:getenv(\"ANTHROPIC_API_KEY\")),\n",
        "    BaseUrl = maps:get(base_url, Options, ?BASE_URL),\n",
        "    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),\n",
        "    RetryCount = maps:get(retry_count, Options, ?DEFAULT_RETRY_COUNT),\n",
        "    RetryDelay = maps:get(retry_delay, Options, ?DEFAULT_RETRY_DELAY),\n",
        "    McpServers = maps:get(mcp_servers, Options, []),\n",
        "    \n",
        "    % Initialize HTTP clients\n",
        "    application:ensure_all_started(inets),\n",
        "    application:ensure_all_started(ssl),\n",
        "    application:ensure_all_started(jsx),\n",
        "    \n",
        "    {ok, #state{\n",
        "        api_key = ApiKey,\n",
        "        base_url = BaseUrl,\n",
        "        timeout = Timeout,\n",
        "        retry_count = RetryCount,\n",
        "        retry_delay = RetryDelay,\n",
        "        mcp_servers = McpServers\n",
        "    }}.\n\n",
        "handle_call({request, Method, Path, Params, BetaHeaders}, From, State) ->\n",
        "    handle_request(Method, Path, Params, BetaHeaders, From, State);\n\n",
        "handle_call({mcp_request, Method, Path, Params, BetaHeaders}, From, State) ->\n",
        "    handle_mcp_request(Method, Path, Params, BetaHeaders, From, State);\n\n",
        "handle_call(_Request, _From, State) ->\n",
        "    {reply, {error, unknown_call}, State}.\n\n",
        "handle_request(Method, Path, Params, BetaHeaders, From, State) ->\n",
        "    RequestId = make_ref(),\n",
        "    \n",
        "    % Extract path parameters and replace in path\n",
        "    PathParams = maps:filter(\n",
        "        fun(K, _) -> lists:member($:, Path) andalso string:str(Path, \":\" ++ atom_to_list(K)) > 0 end,\n",
        "        Params\n",
        "    ),\n",
        "    ResolvedPath = replace_path_params(Path, PathParams),\n",
        "    \n",
        "    % Remove path parameters from the request body\n",
        "    BodyParams = maps:without(maps:keys(PathParams), Params),\n",
        "    \n",
        "    % Construct request URL\n",
        "    Url = State#state.base_url ++ ResolvedPath,\n",
        "    \n",
        "    % Prepare headers with Anthropic-specific headers\n",
        "    BaseHeaders = [\n",
        "        {\"Content-Type\", \"application/json\"},\n",
        "        {\"x-api-key\", State#state.api_key},\n",
        "        {\"anthropic-version\", ?ANTHROPIC_VERSION}\n",
        "    ],\n",
        "    \n",
        "    % Add beta headers if specified\n",
        "    BetaHeadersList = lists:map(\n",
        "        fun(Header) -> {\"anthropic-beta\", Header} end,\n",
        "        BetaHeaders\n",
        "    ),\n",
        "    \n",
        "    Headers = BaseHeaders ++ BetaHeadersList,\n",
        "    \n",
        "    % Extract request options\n",
        "    Timeout = maps:get(timeout, BodyParams, State#state.timeout),\n",
        "    Stream = maps:get(stream, BodyParams, false),\n",
        "    \n",
        "    % Remove non-API parameters from body\n",
        "    CleanBodyParams = maps:without([timeout, stream], BodyParams),\n",
        "    \n",
        "    % Make the request in a separate process\n",
        "    Self = self(),\n",
        "    spawn_link(fun() ->\n",
        "        Result = make_anthropic_request(Method, Url, Headers, CleanBodyParams, Stream, Timeout, State#state.retry_count, State#state.retry_delay),\n",
        "        gen_server:cast(Self, {request_complete, RequestId, From, Result})\n",
        "    end),\n",
        "    \n",
        "    % Track active request\n",
        "    NewActiveRequests = maps:put(RequestId, {From, os:timestamp()}, State#state.active_requests),\n",
        "    {noreply, State#state{last_request_id = RequestId, active_requests = NewActiveRequests}}.\n\n",
        "handle_mcp_request(Method, Path, Params, BetaHeaders, From, State) ->\n",
        "    % Handle MCP-specific request with server validation and processing\n",
        "    McpServers = maps:get(mcp_servers, Params, []),\n",
        "    \n",
        "    % Process MCP servers to ensure they're in the correct format\n",
        "    ProcessedParams = process_mcp_params(Params),\n",
        "    \n",
        "    % Call regular request handler with processed parameters\n",
        "    handle_request(Method, Path, ProcessedParams, BetaHeaders, From, State).\n\n",
        "process_mcp_params(Params) ->\n",
        "    case maps:get(mcp_servers, Params, []) of\n",
        "        [] -> Params;\n",
        "        McpServers ->\n",
        "            % Ensure MCP servers are in correct format for API\n",
        "            ProcessedServers = lists:map(fun process_mcp_server/1, McpServers),\n",
        "            maps:put(mcp_servers, ProcessedServers, Params)\n",
        "    end.\n\n",
        "process_mcp_server(#{type := <<\"url\">>, url := Url, name := Name} = Server) ->\n",
        "    % Ensure all required fields are present and in correct format\n",
        "    BaseServer = #{type => <<\"url\">>, url => Url, name => Name},\n",
        "    \n",
        "    % Add optional fields if present\n",
        "    case maps:get(authorization_token, Server, undefined) of\n",
        "        undefined -> BaseServer;\n",
        "        Token -> maps:put(authorization_token, Token, BaseServer)\n",
        "    end,\n",
        "    \n",
        "    case maps:get(tool_configuration, Server, undefined) of\n",
        "        undefined -> BaseServer;\n",
        "        ToolConfig -> maps:put(tool_configuration, ToolConfig, BaseServer)\n",
        "    end.\n\n",
        "handle_cast({request_complete, RequestId, From, Result}, State) ->\n",
        "    % Reply to the caller\n",
        "    gen_server:reply(From, Result),\n",
        "    \n",
        "    % Remove from active requests\n",
        "    NewActiveRequests = maps:remove(RequestId, State#state.active_requests),\n",
        "    {noreply, State#state{active_requests = NewActiveRequests}};\n\n",
        "handle_cast(_Msg, State) ->\n",
        "    {noreply, State}.\n\n",
        "handle_info(_, State) ->\n",
        "    {noreply, State}.\n\n",
        "terminate(_Reason, _State) ->\n",
        "    ok.\n\n",
        "code_change(_OldVsn, State, _Extra) ->\n",
        "    {ok, State}.\n\n",
        "%% =============================================================================\n",
        "%% Internal Functions\n",
        "%% =============================================================================\n\n",
        "make_anthropic_request(Method, Url, Headers, Params, Stream, Timeout, MaxRetries, RetryDelay) ->\n",
        "    make_anthropic_request(Method, Url, Headers, Params, Stream, Timeout, MaxRetries, RetryDelay, 0).\n\n",
        "make_anthropic_request(_Method, _Url, _Headers, _Params, _Stream, _Timeout, MaxRetries, _RetryDelay, RetryCount) when RetryCount > MaxRetries ->\n",
        "    {error, max_retries_exceeded};\n\n",
        "make_anthropic_request(Method, Url, Headers, Params, Stream, Timeout, MaxRetries, RetryDelay, RetryCount) ->\n",
        "    RequestBody = case Params of\n",
        "        _ when map_size(Params) =:= 0 andalso (Method =:= get orelse Method =:= delete) -> \"\";\n",
        "        _ -> jsx:encode(Params)\n",
        "    end,\n",
        "    \n",
        "    % Convert method to uppercase string\n",
        "    MethodStr = string:uppercase(atom_to_list(Method)),\n",
        "    \n",
        "    HttpOptions = [{timeout, Timeout}],\n",
        "    Options = [],\n",
        "    \n",
        "    Result = case Method of\n",
        "        get -> httpc:request(get, {Url, Headers}, HttpOptions, Options);\n",
        "        delete -> httpc:request(delete, {Url, Headers}, HttpOptions, Options);\n",
        "        _ -> httpc:request(MethodStr, {Url, Headers, \"application/json\", RequestBody}, HttpOptions, Options)\n",
        "    end,\n",
        "    \n",
        "    case Result of\n",
        "        {ok, {{_, 200, _}, ResponseHeaders, ResponseBody}} ->\n",
        "            try\n",
        "                ResponseJson = jsx:decode(list_to_binary(ResponseBody), [return_maps]),\n",
        "                {ok, ResponseJson}\n",
        "            catch\n",
        "                _:_ -> {error, json_decode_failed}\n",
        "            end;\n",
        "        {ok, {{_, 429, _}, _, _}} ->\n",
        "            % Rate limited, retry after delay\n",
        "            timer:sleep(RetryDelay * (RetryCount + 1)),\n",
        "            make_anthropic_request(Method, Url, Headers, Params, Stream, Timeout, MaxRetries, RetryDelay, RetryCount + 1);\n",
        "        {ok, {{_, StatusCode, Reason}, _, ResponseBody}} ->\n",
        "            % Try to parse error response as JSON\n",
        "            ErrorDetails = try\n",
        "                jsx:decode(list_to_binary(ResponseBody), [return_maps])\n",
        "            catch\n",
        "                _:_ -> ResponseBody\n",
        "            end,\n",
        "            {error, {StatusCode, Reason, ErrorDetails}};\n",
        "        {error, Reason} ->\n",
        "            case Reason of\n",
        "                timeout ->\n",
        "                    % Timeout, retry if not exceeding max retries\n",
        "                    timer:sleep(RetryDelay),\n",
        "                    make_anthropic_request(Method, Url, Headers, Params, Stream, Timeout, MaxRetries, RetryDelay, RetryCount + 1);\n",
        "                _ ->\n",
        "                    {error, Reason}\n",
        "            end\n",
        "    end.\n"
    ]).