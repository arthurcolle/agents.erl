-module(pipedream_mcp_client).
-behaviour(gen_server).

-export([
    start_link/0,
    get_available_apps/0,
    connect_user_account/2,
    get_user_connection_status/2,
    call_tool/4,
    get_app_tools/2,
    discover_tools/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    client_id,
    client_secret,
    project_id,
    environment,
    access_token,
    base_url = "https://remote.mcp.pipedream.net"
}).

-define(SERVER, ?MODULE).

%% API Functions

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_available_apps() ->
    gen_server:call(?SERVER, get_available_apps).

connect_user_account(ExternalUserId, AppSlug) ->
    gen_server:call(?SERVER, {connect_user_account, ExternalUserId, AppSlug}).

get_user_connection_status(ExternalUserId, AppSlug) ->
    gen_server:call(?SERVER, {get_user_connection_status, ExternalUserId, AppSlug}).

call_tool(ExternalUserId, AppSlug, ToolName, Arguments) ->
    gen_server:call(?SERVER, {call_tool, ExternalUserId, AppSlug, ToolName, Arguments}, 30000).

get_app_tools(ExternalUserId, AppSlug) ->
    gen_server:call(?SERVER, {get_app_tools, ExternalUserId, AppSlug}).

discover_tools(ExternalUserId, AppSlug) ->
    gen_server:call(?SERVER, {discover_tools, ExternalUserId, AppSlug}).

%% Gen Server Callbacks

init([]) ->
    ClientId = os:getenv("PIPEDREAM_CLIENT_ID"),
    ClientSecret = os:getenv("PIPEDREAM_CLIENT_SECRET"),
    ProjectId = os:getenv("PIPEDREAM_PROJECT_ID"),
    Environment = case os:getenv("PIPEDREAM_ENVIRONMENT") of
        false -> "development";
        Env -> Env
    end,
    
    case {ClientId, ClientSecret, ProjectId} of
        {false, _, _} ->
            {stop, "PIPEDREAM_CLIENT_ID environment variable not set"};
        {_, false, _} ->
            {stop, "PIPEDREAM_CLIENT_SECRET environment variable not set"};
        {_, _, false} ->
            {stop, "PIPEDREAM_PROJECT_ID environment variable not set"};
        _ ->
            State = #state{
                client_id = ClientId,
                client_secret = ClientSecret,
                project_id = ProjectId,
                environment = Environment
            },
            case get_access_token(State) of
                {ok, Token} ->
                    {ok, State#state{access_token = Token}};
                {error, Reason} ->
                    {stop, {token_error, Reason}}
            end
    end.

handle_call(get_available_apps, _From, State) ->
    Result = fetch_available_apps(State),
    {reply, Result, State};

handle_call({connect_user_account, ExternalUserId, AppSlug}, _From, State) ->
    Result = generate_connection_link(ExternalUserId, AppSlug, State),
    {reply, Result, State};

handle_call({get_user_connection_status, ExternalUserId, AppSlug}, _From, State) ->
    Result = check_connection_status(ExternalUserId, AppSlug, State),
    {reply, Result, State};

handle_call({call_tool, ExternalUserId, AppSlug, ToolName, Arguments}, _From, State) ->
    Result = execute_tool_call(ExternalUserId, AppSlug, ToolName, Arguments, State),
    {reply, Result, State};

handle_call({get_app_tools, ExternalUserId, AppSlug}, _From, State) ->
    Result = fetch_app_tools(ExternalUserId, AppSlug, State),
    {reply, Result, State};

handle_call({discover_tools, ExternalUserId, AppSlug}, _From, State) ->
    Result = discover_app_tools(ExternalUserId, AppSlug, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

get_access_token(#state{client_id = ClientId, client_secret = ClientSecret}) ->
    URL = "https://api.pipedream.com/v1/oauth/token",
    Headers = [{"Content-Type", "application/x-www-form-urlencoded"}],
    Body = io_lib:format("grant_type=client_credentials&client_id=~s&client_secret=~s", 
                        [ClientId, ClientSecret]),
    
    case httpc:request(post, {URL, Headers, "application/x-www-form-urlencoded", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"access_token">> := Token} ->
                    {ok, binary_to_list(Token)};
                _ ->
                    {error, invalid_token_response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

fetch_available_apps(#state{access_token = Token, project_id = ProjectId, environment = Environment}) ->
    URL = "https://api.pipedream.com/v1/connect/apps",
    Headers = [
        {"Authorization", "Bearer " ++ Token},
        {"x-pd-project-id", ProjectId},
        {"x-pd-environment", Environment}
    ],
    
    case httpc:request(get, {URL, Headers}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"data">> := Apps} ->
                    {ok, Apps};
                Response ->
                    {ok, Response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

generate_connection_link(ExternalUserId, AppSlug, #state{project_id = ProjectId, environment = Environment}) ->
    Token = generate_connect_token(ExternalUserId, AppSlug, ProjectId, Environment),
    URL = io_lib:format("https://pipedream.com/_static/connect.html?token=~s&connectLink=true&app=~s", 
                       [Token, AppSlug]),
    {ok, URL}.

generate_connect_token(ExternalUserId, AppSlug, ProjectId, Environment) ->
    Data = #{
        external_user_id => ExternalUserId,
        app_slug => AppSlug,
        project_id => ProjectId,
        environment => Environment,
        timestamp => erlang:system_time(second)
    },
    base64:encode(jsx:encode(Data)).

check_connection_status(ExternalUserId, AppSlug, #state{access_token = Token, project_id = ProjectId, environment = Environment}) ->
    URL = io_lib:format("https://api.pipedream.com/v1/connect/users/~s/apps/~s/connections", 
                       [ExternalUserId, AppSlug]),
    Headers = [
        {"Authorization", "Bearer " ++ Token},
        {"x-pd-project-id", ProjectId},
        {"x-pd-environment", Environment}
    ],
    
    case httpc:request(get, {URL, Headers}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"connected">> := true} ->
                    {ok, connected};
                _ ->
                    {ok, not_connected}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            {ok, not_connected};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

execute_tool_call(ExternalUserId, AppSlug, ToolName, Arguments, 
                  #state{access_token = Token, project_id = ProjectId, environment = Environment, base_url = BaseURL}) ->
    URL = io_lib:format("~s/~s/~s/messages", [BaseURL, ExternalUserId, AppSlug]),
    Headers = [
        {"Authorization", "Bearer " ++ Token},
        {"x-pd-project-id", ProjectId},
        {"x-pd-environment", Environment},
        {"x-pd-external-user-id", ExternalUserId},
        {"x-pd-app-slug", AppSlug},
        {"x-pd-tool-mode", "tools-only"},
        {"Content-Type", "application/json"}
    ],
    
    MCPRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => list_to_binary(ToolName),
            <<"arguments">> => Arguments
        }
    },
    
    Body = jsx:encode(MCPRequest),
    
    case httpc:request(post, {URL, Headers, "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"result">> := Result} ->
                    {ok, Result};
                #{<<"error">> := Error} ->
                    {error, Error};
                Response ->
                    {ok, Response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

fetch_app_tools(ExternalUserId, AppSlug, 
                #state{access_token = Token, project_id = ProjectId, environment = Environment, base_url = BaseURL}) ->
    URL = io_lib:format("~s/~s/~s/messages", [BaseURL, ExternalUserId, AppSlug]),
    Headers = [
        {"Authorization", "Bearer " ++ Token},
        {"x-pd-project-id", ProjectId},
        {"x-pd-environment", Environment},
        {"x-pd-external-user-id", ExternalUserId},
        {"x-pd-app-slug", AppSlug},
        {"x-pd-tool-mode", "tools-only"},
        {"Content-Type", "application/json"}
    ],
    
    MCPRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/list">>,
        <<"params">> => #{}
    },
    
    Body = jsx:encode(MCPRequest),
    
    case httpc:request(post, {URL, Headers, "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"result">> := #{<<"tools">> := Tools}} ->
                    {ok, Tools};
                #{<<"error">> := Error} ->
                    {error, Error};
                Response ->
                    {ok, Response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            {error, {http_error, StatusCode, ResponseBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

discover_app_tools(ExternalUserId, AppSlug, State) ->
    case fetch_app_tools(ExternalUserId, AppSlug, State) of
        {ok, Tools} ->
            FormattedTools = format_tools_for_agents(Tools, AppSlug),
            {ok, FormattedTools};
        Error ->
            Error
    end.

format_tools_for_agents(Tools, AppSlug) ->
    lists:map(fun(Tool) ->
        #{
            <<"type">> => <<"function">>,
            <<"name">> => <<(list_to_binary(AppSlug))/binary, "_", (maps:get(<<"name">>, Tool))/binary>>,
            <<"description">> => maps:get(<<"description">>, Tool, <<"Pipedream tool for ", (list_to_binary(AppSlug))/binary>>),
            <<"parameters">> => maps:get(<<"inputSchema">>, Tool, #{}),
            <<"app_slug">> => list_to_binary(AppSlug),
            <<"original_name">> => maps:get(<<"name">>, Tool),
            <<"source">> => <<"pipedream">>,
            <<"strict">> => true
        }
    end, Tools).