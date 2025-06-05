%%%-------------------------------------------------------------------
%%% @doc OpenAPI Proxy
%%% Proxies requests to actual API endpoints
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_proxy).
-behaviour(gen_server).

-include("../include/openapi_scaffold.hrl").

-export([start_link/0, proxy_request/4, set_proxy_enabled/1, add_proxy_config/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(proxy_config, {
    base_url :: binary(),
    headers :: [{binary(), binary()}],
    timeout :: pos_integer(),
    ssl_options :: [term()]
}).

-record(state, {
    proxy_enabled = true :: boolean(),
    proxy_configs = #{} :: #{binary() => #proxy_config{}},
    http_client :: pid()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec proxy_request(binary(), cowboy_req:req(), #route{}, map()) -> 
    {ok, integer(), map(), binary()} | {error, term()}.
proxy_request(SpecId, Req, Route, Operation) ->
    gen_server:call(?MODULE, {proxy_request, SpecId, Req, Route, Operation}, 30000).

-spec set_proxy_enabled(boolean()) -> ok.
set_proxy_enabled(Enabled) ->
    gen_server:cast(?MODULE, {set_proxy_enabled, Enabled}).

-spec add_proxy_config(binary(), map()) -> ok.
add_proxy_config(SpecId, Config) ->
    gen_server:cast(?MODULE, {add_proxy_config, SpecId, Config}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Start HTTP client
    inets:start(),
    ssl:start(),
    
    Enabled = application:get_env(openapi_scaffold, proxy_enabled, true),
    {ok, #state{proxy_enabled = Enabled}}.

handle_call({proxy_request, _SpecId, _Req, _Route, _Operation}, _From,
            #state{proxy_enabled = false} = State) ->
    {reply, {error, proxy_disabled}, State};

handle_call({proxy_request, SpecId, Req, Route, Operation}, _From, State) ->
    Result = do_proxy_request(SpecId, Req, Route, Operation, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({set_proxy_enabled, Enabled}, State) ->
    {noreply, State#state{proxy_enabled = Enabled}};

handle_cast({add_proxy_config, SpecId, Config}, #state{proxy_configs = Configs} = State) ->
    ProxyConfig = parse_proxy_config(Config),
    NewConfigs = maps:put(SpecId, ProxyConfig, Configs),
    {noreply, State#state{proxy_configs = NewConfigs}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

do_proxy_request(SpecId, Req, Route, Operation, #state{proxy_configs = Configs} = State) ->
    case get_proxy_config(SpecId, Operation, Configs) of
        {ok, ProxyConfig} ->
            execute_proxy_request(ProxyConfig, Req, Route, Operation);
        {error, _} = Error ->
            Error
    end.

get_proxy_config(SpecId, Operation, Configs) ->
    case maps:find(SpecId, Configs) of
        {ok, Config} ->
            {ok, Config};
        error ->
            %% Try to extract from operation servers
            case maps:find(<<"servers">>, Operation) of
                {ok, [#{<<"url">> := Url} | _]} ->
                    {ok, #proxy_config{
                        base_url = Url,
                        headers = [],
                        timeout = 30000,
                        ssl_options = []
                    }};
                _ ->
                    {error, no_proxy_config}
            end
    end.

execute_proxy_request(#proxy_config{base_url = BaseUrl, 
                                   headers = ExtraHeaders,
                                   timeout = Timeout,
                                   ssl_options = SslOpts}, 
                     Req, Route, _Operation) ->
    %% Build target URL
    Path = Route#route.path,
    QueryString = cowboy_req:qs(Req),
    TargetUrl = build_target_url(BaseUrl, Path, QueryString),
    
    %% Get request method
    Method = binary_to_atom(string:lowercase(cowboy_req:method(Req)), utf8),
    
    %% Get headers
    Headers = prepare_headers(cowboy_req:headers(Req), ExtraHeaders),
    
    %% Get body if present
    {ok, Body, _} = read_body(Req),
    
    %% Prepare HTTP options
    HttpOptions = [
        {timeout, Timeout},
        {ssl, SslOpts}
    ],
    
    %% Prepare request options
    RequestOptions = case Body of
        <<>> ->
            [];
        _ ->
            [{body_format, binary}]
    end,
    
    %% Make the request
    case make_http_request(Method, TargetUrl, Headers, Body, HttpOptions, RequestOptions) of
        {ok, {{_, StatusCode, _}, RespHeaders, RespBody}} ->
            %% Convert headers to map
            HeaderMap = headers_to_map(RespHeaders),
            {ok, StatusCode, HeaderMap, iolist_to_binary(RespBody)};
        {error, Reason} ->
            {error, {proxy_failed, Reason}}
    end.

build_target_url(BaseUrl, Path, QueryString) ->
    %% Remove trailing slash from base URL
    CleanBase = case binary:last(BaseUrl) of
        $/ -> binary:part(BaseUrl, 0, byte_size(BaseUrl) - 1);
        _ -> BaseUrl
    end,
    
    %% Build full URL
    case QueryString of
        <<>> ->
            <<CleanBase/binary, Path/binary>>;
        _ ->
            <<CleanBase/binary, Path/binary, "?", QueryString/binary>>
    end.

prepare_headers(ReqHeaders, ExtraHeaders) ->
    %% Filter out hop-by-hop headers
    HopByHop = [<<"connection">>, <<"keep-alive">>, <<"proxy-authenticate">>, 
                <<"proxy-authorization">>, <<"te">>, <<"trailers">>, 
                <<"transfer-encoding">>, <<"upgrade">>],
    
    FilteredHeaders = maps:fold(fun(Name, Value, Acc) ->
        case lists:member(Name, HopByHop) of
            true -> Acc;
            false -> [{binary_to_list(Name), binary_to_list(Value)} | Acc]
        end
    end, [], ReqHeaders),
    
    %% Add extra headers
    ExtraHeadersList = [{binary_to_list(N), binary_to_list(V)} || {N, V} <- ExtraHeaders],
    
    FilteredHeaders ++ ExtraHeadersList.

make_http_request(get, Url, Headers, _Body, HttpOptions, Options) ->
    httpc:request(get, {binary_to_list(Url), Headers}, HttpOptions, Options);

make_http_request(head, Url, Headers, _Body, HttpOptions, Options) ->
    httpc:request(head, {binary_to_list(Url), Headers}, HttpOptions, Options);

make_http_request(delete, Url, Headers, _Body, HttpOptions, Options) ->
    httpc:request(delete, {binary_to_list(Url), Headers}, HttpOptions, Options);

make_http_request(Method, Url, Headers, Body, HttpOptions, Options) when Method =:= post;
                                                                         Method =:= put;
                                                                         Method =:= patch ->
    ContentType = proplists:get_value("content-type", Headers, "application/json"),
    HeadersWithoutCT = proplists:delete("content-type", Headers),
    
    Request = {
        binary_to_list(Url),
        HeadersWithoutCT,
        ContentType,
        Body
    },
    
    httpc:request(Method, Request, HttpOptions, Options).

headers_to_map(Headers) ->
    lists:foldl(fun({Name, Value}, Acc) ->
        BinName = iolist_to_binary(Name),
        BinValue = iolist_to_binary(Value),
        maps:put(BinName, BinValue, Acc)
    end, #{}, Headers).

parse_proxy_config(Config) when is_map(Config) ->
    #proxy_config{
        base_url = maps:get(<<"base_url">>, Config, <<"http://localhost">>),
        headers = maps:get(<<"headers">>, Config, []),
        timeout = maps:get(<<"timeout">>, Config, 30000),
        ssl_options = maps:get(<<"ssl_options">>, Config, [])
    };
parse_proxy_config(BaseUrl) when is_binary(BaseUrl) ->
    #proxy_config{
        base_url = BaseUrl,
        headers = [],
        timeout = 30000,
        ssl_options = []
    }.

read_body(Req0) ->
    read_body(Req0, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} ->
            {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} ->
            read_body(Req, <<Acc/binary, Data/binary>>)
    end.