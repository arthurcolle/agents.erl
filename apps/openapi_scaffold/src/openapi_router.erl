%%%-------------------------------------------------------------------
%%% @doc OpenAPI Dynamic Router
%%% Manages dynamic routing based on OpenAPI specifications
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_router).
-behaviour(gen_server).

-include("../include/openapi_scaffold.hrl").

-export([start_link/0, load_spec/2, reload_spec/2, get_routes/1, match_route/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    routes = [] :: [#route{}],
    specs = #{} :: #{binary() => map()},
    compiled_routes = undefined :: term()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec load_spec(binary(), map()) -> ok | {error, term()}.
load_spec(SpecId, Spec) ->
    gen_server:call(?MODULE, {load_spec, SpecId, Spec}).

-spec reload_spec(binary(), map()) -> ok | {error, term()}.
reload_spec(SpecId, Spec) ->
    gen_server:call(?MODULE, {reload_spec, SpecId, Spec}).

-spec get_routes(binary()) -> {ok, [#route{}]} | {error, not_found}.
get_routes(SpecId) ->
    gen_server:call(?MODULE, {get_routes, SpecId}).

-spec match_route(binary(), binary()) -> {ok, #route{}, [{binary(), binary()}]} | {error, not_found}.
match_route(Path, Method) ->
    gen_server:call(?MODULE, {match_route, Path, Method}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call({load_spec, SpecId, Spec}, _From, State) ->
    case extract_routes(SpecId, Spec) of
        {ok, NewRoutes} ->
            %% Generate code for the spec
            spawn(fun() ->
                openapi_codegen:generate_from_spec(SpecId, Spec)
            end),
            
            UpdatedState = add_routes(SpecId, Spec, NewRoutes, State),
            {reply, ok, UpdatedState};
        Error ->
            {reply, Error, State}
    end;

handle_call({reload_spec, SpecId, Spec}, _From, State) ->
    %% Remove old routes and add new ones
    State1 = remove_routes(SpecId, State),
    case extract_routes(SpecId, Spec) of
        {ok, NewRoutes} ->
            %% Regenerate code for the spec
            spawn(fun() ->
                openapi_codegen:generate_from_spec(SpecId, Spec)
            end),
            
            UpdatedState = add_routes(SpecId, Spec, NewRoutes, State1),
            {reply, ok, UpdatedState};
        Error ->
            {reply, Error, State}
    end;

handle_call({get_routes, SpecId}, _From, #state{routes = Routes} = State) ->
    SpecRoutes = [R || R <- Routes, R#route.spec_id =:= SpecId],
    case SpecRoutes of
        [] -> {reply, {error, not_found}, State};
        _ -> {reply, {ok, SpecRoutes}, State}
    end;

handle_call({match_route, Path, Method}, _From, State) ->
    Result = do_match_route(Path, Method, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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

extract_routes(SpecId, Spec) ->
    try
        Paths = maps:get(<<"paths">>, Spec, #{}),
        BasePath = extract_base_path(Spec),
        
        Routes = maps:fold(fun(Path, PathItem, Acc) ->
            FullPath = <<BasePath/binary, Path/binary>>,
            Methods = extract_methods(PathItem),
            
            Route = #route{
                path = FullPath,
                path_pattern = parse_path_pattern(FullPath),
                methods = Methods,
                spec_id = SpecId
            },
            [Route | Acc]
        end, [], Paths),
        
        {ok, Routes}
    catch
        error:Reason ->
            {error, {route_extraction_failed, Reason}}
    end.

extract_base_path(Spec) ->
    case maps:get(<<"servers">>, Spec, []) of
        [#{<<"url">> := Url} | _] ->
            %% Extract path from URL
            case uri_string:parse(Url) of
                #{path := Path} when Path =/= <<>> -> Path;
                _ -> <<>>
            end;
        _ ->
            <<>>
    end.

extract_methods(PathItem) ->
    HttpMethods = [<<"get">>, <<"post">>, <<"put">>, <<"delete">>, 
                   <<"patch">>, <<"head">>, <<"options">>],
    
    maps:fold(fun(Key, Value, Acc) ->
        case lists:member(Key, HttpMethods) of
            true ->
                maps:put(string:uppercase(Key), Value, Acc);
            false ->
                Acc
        end
    end, #{}, PathItem).

parse_path_pattern(Path) ->
    %% Split path and identify parameters
    Parts = binary:split(Path, <<"/">>, [global, trim]),
    lists:map(fun(Part) ->
        case Part of
            <<"{", Rest/binary>> ->
                ParamName = binary:part(Rest, 0, byte_size(Rest) - 1),
                {param, ParamName};
            _ ->
                Part
        end
    end, Parts).

add_routes(SpecId, Spec, NewRoutes, #state{routes = Routes, specs = Specs} = State) ->
    UpdatedRoutes = lists:sort(fun route_priority/2, NewRoutes ++ Routes),
    State#state{
        routes = UpdatedRoutes,
        specs = maps:put(SpecId, Spec, Specs),
        compiled_routes = compile_routes(UpdatedRoutes)
    }.

remove_routes(SpecId, #state{routes = Routes, specs = Specs} = State) ->
    FilteredRoutes = [R || R <- Routes, R#route.spec_id =/= SpecId],
    State#state{
        routes = FilteredRoutes,
        specs = maps:remove(SpecId, Specs),
        compiled_routes = compile_routes(FilteredRoutes)
    }.

route_priority(#route{path_pattern = P1}, #route{path_pattern = P2}) ->
    %% Routes with fewer parameters have higher priority
    Params1 = length([P || {param, _} = P <- P1]),
    Params2 = length([P || {param, _} = P <- P2]),
    
    if
        Params1 < Params2 -> true;
        Params1 > Params2 -> false;
        true ->
            %% Same number of params, longer paths have priority
            length(P1) >= length(P2)
    end.

compile_routes(Routes) ->
    %% Create an optimized structure for route matching
    %% For now, just return the routes as-is
    Routes.

do_match_route(Path, Method, #state{routes = Routes}) ->
    PathParts = binary:split(Path, <<"/">>, [global, trim]),
    MethodUpper = string:uppercase(Method),
    
    case find_matching_route(PathParts, MethodUpper, Routes) of
        {ok, Route, Params} ->
            {ok, Route, Params};
        error ->
            {error, not_found}
    end.

find_matching_route(_PathParts, _Method, []) ->
    error;
find_matching_route(PathParts, Method, [Route | Rest]) ->
    case match_path_pattern(PathParts, Route#route.path_pattern) of
        {true, Params} ->
            case maps:is_key(Method, Route#route.methods) of
                true ->
                    {ok, Route, Params};
                false ->
                    find_matching_route(PathParts, Method, Rest)
            end;
        false ->
            find_matching_route(PathParts, Method, Rest)
    end.

match_path_pattern(PathParts, Pattern) when length(PathParts) =:= length(Pattern) ->
    match_path_pattern(PathParts, Pattern, []);
match_path_pattern(_, _) ->
    false.

match_path_pattern([], [], Params) ->
    {true, lists:reverse(Params)};
match_path_pattern([Part | RestParts], [{param, Name} | RestPattern], Params) ->
    %% Parameter matches any value
    match_path_pattern(RestParts, RestPattern, [{Name, Part} | Params]);
match_path_pattern([Part | RestParts], [Part | RestPattern], Params) ->
    %% Exact match
    match_path_pattern(RestParts, RestPattern, Params);
match_path_pattern(_, _, _) ->
    false.