%%%-------------------------------------------------------------------
%%% @doc OpenAPI Scaffold Header File
%%% @end
%%%-------------------------------------------------------------------

-ifndef(OPENAPI_SCAFFOLD_HRL).
-define(OPENAPI_SCAFFOLD_HRL, true).

%% Route record definition
-record(route, {
    path :: binary(),
    path_pattern :: [binary() | {param, binary()}],
    methods :: #{binary() => map()},
    spec_id :: binary()
}).

-endif. % OPENAPI_SCAFFOLD_HRL