-module(supervision_tree_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    SupervisionTree = get_supervision_tree(),
    Json = jsx:encode(SupervisionTree),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req2, State}.

get_supervision_tree() ->
    RootSups = [
        {agent_web_sup, agent_web_sup},
        {agent_supervisor, agent_supervisor},
        {openai_sup, openai_sup}
    ],
    
    Tree = lists:map(fun({Name, Sup}) ->
        try
            Children = get_supervisor_children(Sup),
            #{
                name => atom_to_binary(Name),
                type => <<"supervisor">>,
                pid => list_to_binary(pid_to_list(whereis(Sup))),
                status => get_process_status(whereis(Sup)),
                children => Children
            }
        catch
            _:_ ->
                #{
                    name => atom_to_binary(Name),
                    type => <<"supervisor">>,
                    pid => <<"undefined">>,
                    status => <<"not_running">>,
                    children => []
                }
        end
    end, RootSups),
    
    #{
        timestamp => erlang:system_time(millisecond),
        tree => Tree
    }.

get_supervisor_children(Sup) when is_atom(Sup) ->
    case whereis(Sup) of
        undefined -> [];
        Pid -> get_supervisor_children(Pid)
    end;
get_supervisor_children(Sup) when is_pid(Sup) ->
    try
        Children = supervisor:which_children(Sup),
        lists:map(fun({Id, ChildPid, Type, Modules}) ->
            #{
                id => format_child_id(Id),
                pid => format_pid(ChildPid),
                type => atom_to_binary(Type),
                modules => format_modules(Modules),
                status => get_process_status(ChildPid),
                children => case Type of
                    supervisor when is_pid(ChildPid) ->
                        get_supervisor_children(ChildPid);
                    _ -> []
                end
            }
        end, Children)
    catch
        _:_ -> []
    end.

format_child_id(Id) when is_atom(Id) -> atom_to_binary(Id);
format_child_id(Id) when is_binary(Id) -> Id;
format_child_id(Id) -> list_to_binary(io_lib:format("~p", [Id])).

format_pid(undefined) -> <<"undefined">>;
format_pid(restarting) -> <<"restarting">>;
format_pid(Pid) when is_pid(Pid) -> list_to_binary(pid_to_list(Pid));
format_pid(Other) -> list_to_binary(io_lib:format("~p", [Other])).

format_modules([]) -> [];
format_modules(dynamic) -> [<<"dynamic">>];
format_modules(Modules) when is_list(Modules) ->
    [atom_to_binary(M) || M <- Modules];
format_modules(Module) when is_atom(Module) ->
    [atom_to_binary(Module)].

get_process_status(undefined) -> <<"undefined">>;
get_process_status(restarting) -> <<"restarting">>;
get_process_status(Pid) when is_pid(Pid) ->
    try
        case erlang:process_info(Pid, status) of
            {status, Status} -> atom_to_binary(Status);
            undefined -> <<"terminated">>
        end
    catch
        _:_ -> <<"unknown">>
    end;
get_process_status(_) -> <<"unknown">>.

atom_to_binary(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
atom_to_binary(Other) ->
    list_to_binary(io_lib:format("~p", [Other])).