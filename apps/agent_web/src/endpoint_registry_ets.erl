%%%-------------------------------------------------------------------
%%% @doc Endpoint Registry V2 - ETS version (no SQLite dependency)
%%% Stores and manages discovered API endpoints with their metadata
%%% @end
%%%-------------------------------------------------------------------
-module(endpoint_registry_ets).
-behaviour(gen_server).

-export([start_link/0]).
-export([
    register_endpoint/1,
    get_endpoint/2,
    get_all_endpoints/0,
    update_endpoint/1,
    remove_endpoint/2,
    get_endpoints_by_tag/1,
    log_request/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    endpoints_table :: ets:tid(),
    logs_table :: ets:tid()
}).

-record(endpoint, {
    key :: {binary(), binary()}, % {method, path}
    method :: binary(),
    path :: binary(),
    operation_id :: binary(),
    summary :: binary(),
    tag :: binary(),
    parameters :: list(),
    request_schema :: map(),
    response_schema :: map(),
    last_updated :: erlang:timestamp()
}).

-record(request_log, {
    timestamp :: erlang:timestamp(),
    endpoint :: binary(),
    method :: binary(),
    request_payload :: map(),
    response_payload :: map(),
    cost_estimate :: float(),
    prompt_tokens :: integer(),
    completion_tokens :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_endpoint(Data) when is_map(Data) ->
    gen_server:call(?MODULE, {register_endpoint, Data}).

get_endpoint(Method, Path) ->
    gen_server:call(?MODULE, {get_endpoint, Method, Path}).

get_all_endpoints() ->
    gen_server:call(?MODULE, get_all_endpoints).

update_endpoint(Data) when is_map(Data) ->
    gen_server:call(?MODULE, {update_endpoint, Data}).

remove_endpoint(Method, Path) ->
    gen_server:call(?MODULE, {remove_endpoint, Method, Path}).

get_endpoints_by_tag(Tag) ->
    gen_server:call(?MODULE, {get_endpoints_by_tag, Tag}).

log_request(LogEntry) when is_map(LogEntry) ->
    gen_server:cast(?MODULE, {log_request, LogEntry}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % Create ETS tables
    EndpointsTable = ets:new(endpoints, [set, protected, {keypos, #endpoint.key}]),
    LogsTable = ets:new(request_logs, [ordered_set, protected]),
    
    {ok, #state{
        endpoints_table = EndpointsTable,
        logs_table = LogsTable
    }}.

handle_call({register_endpoint, Data}, _From, State) ->
    #state{endpoints_table = Table} = State,
    Endpoint = data_to_endpoint(Data),
    ets:insert(Table, Endpoint),
    {reply, ok, State};

handle_call({get_endpoint, Method, Path}, _From, State) ->
    #state{endpoints_table = Table} = State,
    case ets:lookup(Table, {Method, Path}) of
        [Endpoint] -> {reply, {ok, endpoint_to_map(Endpoint)}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call(get_all_endpoints, _From, State) ->
    #state{endpoints_table = Table} = State,
    Endpoints = ets:tab2list(Table),
    Maps = [endpoint_to_map(E) || E <- Endpoints],
    {reply, {ok, Maps}, State};

handle_call({update_endpoint, Data}, _From, State) ->
    #state{endpoints_table = Table} = State,
    Endpoint = data_to_endpoint(Data),
    ets:insert(Table, Endpoint),
    {reply, ok, State};

handle_call({remove_endpoint, Method, Path}, _From, State) ->
    #state{endpoints_table = Table} = State,
    ets:delete(Table, {Method, Path}),
    {reply, ok, State};

handle_call({get_endpoints_by_tag, Tag}, _From, State) ->
    #state{endpoints_table = Table} = State,
    Match = ets:match_object(Table, #endpoint{tag = Tag, _ = '_'}),
    Maps = [endpoint_to_map(E) || E <- Match],
    {reply, {ok, Maps}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast({log_request, LogEntry}, State) ->
    #state{logs_table = Table} = State,
    Timestamp = erlang:timestamp(),
    Log = #request_log{
        timestamp = Timestamp,
        endpoint = maps:get(endpoint, LogEntry, <<>>),
        method = maps:get(method, LogEntry, <<>>),
        request_payload = maps:get(request_payload, LogEntry, #{}),
        response_payload = maps:get(response_payload, LogEntry, #{}),
        cost_estimate = maps:get(cost_estimate, LogEntry, 0.0),
        prompt_tokens = maps:get(prompt_tokens, LogEntry, 0),
        completion_tokens = maps:get(completion_tokens, LogEntry, 0)
    },
    ets:insert(Table, {Timestamp, Log}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

data_to_endpoint(Data) ->
    Method = maps:get(method, Data),
    Path = maps:get(path, Data),
    #endpoint{
        key = {Method, Path},
        method = Method,
        path = Path,
        operation_id = maps:get(operation_id, Data, <<>>),
        summary = maps:get(summary, Data, <<>>),
        tag = maps:get(tag, Data, <<>>),
        parameters = maps:get(parameters, Data, []),
        request_schema = maps:get(request_schema, Data, #{}),
        response_schema = maps:get(response_schema, Data, #{}),
        last_updated = erlang:timestamp()
    }.

endpoint_to_map(#endpoint{} = E) ->
    #{
        method => E#endpoint.method,
        path => E#endpoint.path,
        operation_id => E#endpoint.operation_id,
        summary => E#endpoint.summary,
        tag => E#endpoint.tag,
        parameters => E#endpoint.parameters,
        request_schema => E#endpoint.request_schema,
        response_schema => E#endpoint.response_schema,
        last_updated => E#endpoint.last_updated
    }.