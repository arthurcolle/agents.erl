%%%-------------------------------------------------------------------
%%% @doc Endpoint Discovery
%%% Discovers and registers API endpoints from OpenAPI schemas
%%% @end
%%%-------------------------------------------------------------------
-module(endpoint_discovery).
-behaviour(gen_server).

-export([start_link/0]).
-export([
    discover_endpoints/1,
    discover_all_endpoints/0,
    get_discovery_status/0,
    set_discovery_interval/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    discovery_status = idle :: idle | discovering | completed | error,
    last_discovery :: erlang:timestamp() | undefined,
    discovery_interval = 3600000 :: integer(), % 1 hour in milliseconds
    timer_ref :: reference() | undefined
}).

%% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

discover_endpoints(Source) ->
    gen_server:call(?MODULE, {discover_endpoints, Source}, 60000).

discover_all_endpoints() ->
    gen_server:call(?MODULE, discover_all_endpoints, 60000).

get_discovery_status() ->
    gen_server:call(?MODULE, get_discovery_status).

set_discovery_interval(IntervalMs) ->
    gen_server:call(?MODULE, {set_discovery_interval, IntervalMs}).

%% gen_server callbacks

init([]) ->
    State = #state{},
    
    % Schedule initial discovery after 10 seconds
    self() ! schedule_discovery,
    
    {ok, State}.

handle_call({discover_endpoints, Source}, _From, State) ->
    NewState = State#state{discovery_status = discovering},
    
    case discover_endpoints_from_source(Source) of
        {ok, Count} ->
            {reply, {ok, Count}, NewState#state{
                discovery_status = completed,
                last_discovery = erlang:timestamp()
            }};
        {error, Reason} ->
            {reply, {error, Reason}, NewState#state{
                discovery_status = error
            }}
    end;

handle_call(discover_all_endpoints, _From, State) ->
    NewState = State#state{discovery_status = discovering},
    
    % Get all configured sources
    Sources = [openai, anthropic, github],
    
    Results = lists:map(fun(Source) ->
        case discover_endpoints_from_source(Source) of
            {ok, Count} -> {Source, Count};
            {error, _} -> {Source, 0}
        end
    end, Sources),
    
    TotalEndpoints = lists:sum([Count || {_, Count} <- Results]),
    
    {reply, {ok, Results, TotalEndpoints}, NewState#state{
        discovery_status = completed,
        last_discovery = erlang:timestamp()
    }};

handle_call(get_discovery_status, _From, State) ->
    Status = #{
        status => State#state.discovery_status,
        last_discovery => State#state.last_discovery,
        discovery_interval => State#state.discovery_interval
    },
    {reply, Status, State};

handle_call({set_discovery_interval, IntervalMs}, _From, State) ->
    % Cancel existing timer
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    
    % Schedule new timer
    NewRef = erlang:send_after(IntervalMs, self(), discover_all),
    
    {reply, ok, State#state{
        discovery_interval = IntervalMs,
        timer_ref = NewRef
    }};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(schedule_discovery, State) ->
    % Schedule the first discovery
    erlang:send_after(10000, self(), discover_all),
    {noreply, State};

handle_info(discover_all, State) ->
    % Perform discovery in background
    spawn(fun() ->
        gen_server:call(?MODULE, discover_all_endpoints, 60000)
    end),
    
    % Schedule next discovery
    NewRef = erlang:send_after(State#state.discovery_interval, self(), discover_all),
    
    {noreply, State#state{timer_ref = NewRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Cancel timer if running
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

discover_endpoints_from_source(Source) ->
    case openapi_fetcher:fetch_schema(Source) of
        {ok, Schema} ->
            Endpoints = extract_endpoints_from_schema(Schema),
            
            % Register each endpoint
            lists:foreach(fun(Endpoint) ->
                endpoint_registry:register_endpoint(Endpoint)
            end, Endpoints),
            
            {ok, length(Endpoints)};
        {error, Reason} ->
            error_logger:error_msg("Failed to fetch schema for ~p: ~p~n", [Source, Reason]),
            {error, Reason}
    end.

extract_endpoints_from_schema(Schema) ->
    Paths = maps:get(paths, Schema, #{}),
    
    lists:flatten(
        maps:fold(fun(Path, PathOps, Acc) ->
            EndpointsForPath = lists:map(fun({Method, OpData}) ->
                #{
                    method => atom_to_binary(Method, utf8),
                    path => Path,
                    operation_id => maps:get(operation_id, OpData, <<>>),
                    summary => maps:get(summary, OpData, <<>>),
                    tag => get_primary_tag(maps:get(tags, OpData, [])),
                    parameters => maps:get(parameters, OpData, []),
                    request_schema => extract_request_schema(OpData),
                    response_schema => extract_response_schema(OpData)
                }
            end, PathOps),
            [EndpointsForPath | Acc]
        end, [], Paths)
    ).

get_primary_tag([]) -> <<"general">>;
get_primary_tag([Tag | _]) -> Tag.

extract_request_schema(OpData) ->
    case maps:get(request_body, OpData, undefined) of
        undefined -> #{};
        RequestBody ->
            Content = maps:get(<<"content">>, RequestBody, #{}),
            case maps:get(<<"application/json">>, Content, undefined) of
                undefined -> #{};
                JsonContent -> maps:get(<<"schema">>, JsonContent, #{})
            end
    end.

extract_response_schema(OpData) ->
    Responses = maps:get(responses, OpData, #{}),
    case maps:get(<<"200">>, Responses, undefined) of
        undefined -> 
            case maps:get(<<"201">>, Responses, undefined) of
                undefined -> #{};
                Created -> extract_schema_from_response(Created)
            end;
        Ok -> extract_schema_from_response(Ok)
    end.

extract_schema_from_response(Response) ->
    Content = maps:get(<<"content">>, Response, #{}),
    case maps:get(<<"application/json">>, Content, undefined) of
        undefined -> #{};
        JsonContent -> maps:get(<<"schema">>, JsonContent, #{})
    end.