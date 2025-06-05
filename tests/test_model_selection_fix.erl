#!/usr/bin/env escript
%% Test the model selection strategy fix

main(_) ->
    % Test the fixed logic directly
    ScoredModels = [
        {<<"gpt-4-turbo">>, 0.85},
        {<<"gpt-4.1-nano">>, 0.91},
        {<<"o3">>, 0.89},
        {<<"gpt-4.1-mini">>, 0.88}
    ],
    
    % This is the fixed logic that was causing the crash
    {BestModel, BestScore} = lists:foldl(fun({Model, Score}, {CurrentModel, CurrentScore}) ->
        if Score > CurrentScore -> {Model, Score};
           true -> {CurrentModel, CurrentScore}
        end
    end, hd(ScoredModels), tl(ScoredModels)),
    
    io:format("Best model selected: ~p with score: ~p~n", [BestModel, BestScore]),
    
    % Verify it selected the highest score
    ExpectedBest = <<"gpt-4.1-nano">>,
    if BestModel =:= ExpectedBest ->
        io:format("SUCCESS: Correctly selected highest scoring model~n"),
        halt(0);
    true ->
        io:format("ERROR: Expected ~p but got ~p~n", [ExpectedBest, BestModel]),
        halt(1)
    end.