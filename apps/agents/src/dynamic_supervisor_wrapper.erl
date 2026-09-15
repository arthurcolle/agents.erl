-module(dynamic_supervisor_wrapper).
-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

%% Dynamic supervisor names are metadata, not process-registration names.
%% Keeping the supervisor anonymous avoids turning untrusted/user-generated
%% names into atoms and still lets the manager address it through its child id.
start_link(_Name, SupFlags, InitialChildren) ->
    supervisor:start_link(?MODULE, {SupFlags, InitialChildren}).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init({SupFlags, InitialChildren}) ->
    % Validate and normalize supervisor flags
    ValidatedFlags = validate_sup_flags(SupFlags),
    
    % Convert child specs to proper format
    ChildSpecs = lists:map(fun normalize_child_spec/1, InitialChildren),
    
    {ok, {ValidatedFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================

validate_sup_flags(SupFlags) when is_map(SupFlags) ->
    SupFlags;
validate_sup_flags({RestartStrategy, MaxR, MaxT}) ->
    #{
        strategy => RestartStrategy,
        intensity => MaxR,
        period => MaxT
    };
validate_sup_flags(_) ->
    % Default supervisor flags
    #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    }.

normalize_child_spec(ChildSpec) when is_map(ChildSpec) ->
    % Ensure all required fields are present
    #{
        id => maps:get(id, ChildSpec, make_ref()),
        start => maps:get(start, ChildSpec),
        restart => maps:get(restart, ChildSpec, permanent),
        shutdown => maps:get(shutdown, ChildSpec, 5000),
        type => maps:get(type, ChildSpec, worker),
        modules => maps:get(modules, ChildSpec, dynamic)
    };
normalize_child_spec({Id, StartFunc, Restart, Shutdown, Type, Modules}) ->
    % Old tuple format
    #{
        id => Id,
        start => StartFunc,
        restart => Restart,
        shutdown => Shutdown,
        type => Type,
        modules => Modules
    }.
