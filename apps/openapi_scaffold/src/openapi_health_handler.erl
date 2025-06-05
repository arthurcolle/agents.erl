%%%-------------------------------------------------------------------
%%% @doc OpenAPI Health Check Handler
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_health_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    %% Get system status
    Status = check_system_health(),
    
    Response = #{
        <<"status">> => <<"healthy">>,
        <<"timestamp">> => iso8601_timestamp(),
        <<"components">> => #{
            <<"parser">> => check_process(openapi_parser),
            <<"codegen">> => check_process(openapi_codegen),
            <<"router">> => check_process(openapi_router),
            <<"validator">> => check_process(openapi_validator),
            <<"proxy">> => check_process(openapi_proxy),
            <<"introspection">> => check_process(openapi_introspection)
        },
        <<"stats">> => Status
    },
    
    {ok, cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Response), Req0), State}.

check_system_health() ->
    %% Get loaded specs count
    SpecCount = case openapi_introspection:get_loaded_specs() of
        {ok, Specs} -> length(Specs);
        _ -> 0
    end,
    
    #{
        <<"loaded_specs">> => SpecCount,
        <<"memory_usage">> => erlang:memory(total),
        <<"process_count">> => erlang:system_info(process_count),
        <<"uptime_seconds">> => element(1, erlang:statistics(wall_clock)) div 1000
    }.

check_process(Name) ->
    case whereis(Name) of
        undefined -> <<"down">>;
        Pid when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                true -> <<"up">>;
                false -> <<"down">>
            end
    end.

iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                  [Year, Month, Day, Hour, Minute, Second])).