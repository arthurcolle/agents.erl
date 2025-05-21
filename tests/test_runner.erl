-module(test_runner).
-export([run_all/0]).

run_all() ->
    io:format("~n========== RUNNING ALL TESTS ==========~n~n"),
    
    % List of test modules to run
    TestModules = [
        test_agent,
        test_app,
        test_fix,
        test_shell,
        test_start
    ],
    
    % Run each test module
    lists:foreach(fun(Module) ->
        io:format("~n----- Running ~p tests -----~n", [Module]),
        
        % Ensure module is loaded
        case code:ensure_loaded(Module) of
            {module, _} ->
                % Check exported functions
                Exports = Module:module_info(exports),
                
                try
                    case lists:member({test, 0}, Exports) of
                        true ->
                            io:format("Running ~p:test()~n", [Module]),
                            Module:test();
                        false ->
                            case lists:member({start, 0}, Exports) of
                                true ->
                                    io:format("Running ~p:start()~n", [Module]),
                                    Module:start();
                                false ->
                                    case lists:member({main, 1}, Exports) of
                                        true ->
                                            io:format("Running ~p:main([])~n", [Module]),
                                            Module:main([]);
                                        false ->
                                            io:format("ERROR: No test function found in module ~p~n", [Module])
                                    end
                            end
                    end,
                    
            io:format("~p tests completed successfully~n", [Module])
                catch
                    E:R:S ->
                        io:format("ERROR in ~p tests: ~p:~p~n", [Module, E, R]),
                        io:format("Stack trace: ~p~n", [S])
                end;
            Error ->
                io:format("ERROR: Could not load module ~p: ~p~n", [Module, Error])
        end
    end, TestModules),
    
    io:format("~n========== ALL TESTS COMPLETED ==========~n~n"),
    ok.