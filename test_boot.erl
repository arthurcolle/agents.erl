#!/usr/bin/env escript

main(_) ->
    %% Add all ebin directories to code path
    {ok, Cwd} = file:get_cwd(),
    BuildDir = filename:join(Cwd, "_build/default/lib"),
    {ok, Libs} = file:list_dir(BuildDir),
    lists:foreach(fun(Lib) ->
        EbinDir = filename:join([BuildDir, Lib, "ebin"]),
        code:add_patha(EbinDir)
    end, Libs),
    io:format("Testing application boot sequence...~n"),
    
    %% Test each app
    Apps = [quickrand, uuid, agents, accept],
    
    lists:foreach(fun(App) ->
        case application:ensure_all_started(App) of
            {ok, Started} ->
                io:format("✓ ~p booted successfully (started: ~p)~n", [App, Started]);
            {error, Reason} ->
                io:format("✗ ~p failed to boot: ~p~n", [App, Reason])
        end
    end, Apps),
    
    io:format("~nBoot test complete.~n").