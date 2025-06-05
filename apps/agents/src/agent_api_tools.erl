%%%-------------------------------------------------------------------
%%% @doc Agent API Tools
%%% Tools that allow agents to interact with discovered API endpoints
%%% @end
%%%-------------------------------------------------------------------
-module(agent_api_tools).

-export([
    init/0,
    discover_apis/1,
    call_api_endpoint/2,
    list_available_endpoints/1,
    get_endpoint_schema/2
]).

%% Initialize the API tools
init() ->
    % Ensure the self-scaffolding system is running
    case whereis(endpoint_registry) of
        undefined ->
            {error, scaffold_system_not_running};
        _ ->
            ok
    end.

%% Discover APIs from a specific source
discover_apis(Source) when is_atom(Source) ->
    case endpoint_discovery:discover_endpoints(Source) of
        {ok, Count} ->
            {ok, #{
                source => Source,
                endpoints_discovered => Count,
                message => <<"Successfully discovered endpoints">>
            }};
        {error, Reason} ->
            {error, #{
                source => Source,
                reason => Reason,
                message => <<"Failed to discover endpoints">>
            }}
    end.

%% Call a discovered API endpoint
call_api_endpoint(EndpointId, Params) ->
    case parse_endpoint_id(EndpointId) of
        {ok, Method, Path} ->
            case endpoint_registry:get_endpoint(Method, Path) of
                {ok, Endpoint} ->
                    execute_api_call(Endpoint, Params);
                {error, not_found} ->
                    {error, #{
                        reason => endpoint_not_found,
                        message => <<"Endpoint not found in registry">>
                    }}
            end;
        {error, Reason} ->
            {error, #{
                reason => invalid_endpoint_id,
                details => Reason
            }}
    end.

%% List available endpoints with optional filtering
list_available_endpoints(Filter) ->
    AllEndpoints = endpoint_registry:get_all_endpoints(),
    
    FilteredEndpoints = case Filter of
        #{tag := Tag} ->
            endpoint_registry:get_endpoints_by_tag(Tag);
        #{search := SearchTerm} ->
            filter_by_search(AllEndpoints, SearchTerm);
        _ ->
            AllEndpoints
    end,
    
    % Convert to agent-friendly format
    EndpointsList = lists:map(fun format_endpoint_for_agent/1, FilteredEndpoints),
    
    {ok, #{
        count => length(EndpointsList),
        endpoints => EndpointsList
    }}.

%% Get detailed schema for an endpoint
get_endpoint_schema(Method, Path) ->
    case endpoint_registry:get_endpoint(Method, Path) of
        {ok, Endpoint} ->
            {ok, #{
                method => element(2, Endpoint),
                path => element(3, Endpoint),
                operation_id => element(4, Endpoint),
                summary => element(5, Endpoint),
                parameters => element(7, Endpoint),
                request_schema => element(8, Endpoint),
                response_schema => element(9, Endpoint)
            }};
        {error, not_found} ->
            {error, #{
                reason => endpoint_not_found,
                message => <<"Endpoint not found in registry">>
            }}
    end.

%% Internal functions

parse_endpoint_id(EndpointId) when is_binary(EndpointId) ->
    case binary:split(EndpointId, <<" ">>) of
        [Method, Path] ->
            {ok, Method, Path};
        _ ->
            {error, invalid_format}
    end.

execute_api_call(_Endpoint, _Params) ->
    % This would implement the actual API call
    % For now, return a placeholder
    {ok, #{
        status => success,
        message => <<"API call would be executed here">>,
        note => <<"Integration with actual HTTP client pending">>
    }}.

filter_by_search(Endpoints, SearchTerm) ->
    LowerSearch = string:lowercase(binary_to_list(SearchTerm)),
    lists:filter(fun(Endpoint) ->
        Path = binary_to_list(element(3, Endpoint)),
        Summary = binary_to_list(element(5, Endpoint)),
        string:find(string:lowercase(Path), LowerSearch) =/= nomatch orelse
        string:find(string:lowercase(Summary), LowerSearch) =/= nomatch
    end, Endpoints).

format_endpoint_for_agent(Endpoint) ->
    #{
        id => iolist_to_binary([element(2, Endpoint), <<" ">>, element(3, Endpoint)]),
        method => element(2, Endpoint),
        path => element(3, Endpoint),
        summary => element(5, Endpoint),
        tag => element(6, Endpoint)
    }.