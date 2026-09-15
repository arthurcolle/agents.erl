#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).

main(["--check", Path]) ->
    Seen = ets:new(timeline_repair_seen, [set, private]),
    case process_file(Path, undefined, Seen) of
        {ok, Stats} ->
            print_stats("check", Path, Stats),
            halt(0);
        {error, Reason} ->
            io:format(standard_error, "timeline check failed: ~p~n", [Reason]),
            halt(1)
    end;
main(["--repair", Path]) ->
    Seen = ets:new(timeline_repair_seen, [set, private]),
    Suffix = integer_to_list(erlang:unique_integer([positive])),
    TempPath = Path ++ ".repair-" ++ Suffix,
    BackupPath = Path ++ ".pre-repair-" ++ Suffix,
    case file:open(TempPath, [write, binary, exclusive]) of
        {ok, Output} ->
            Result = process_file(Path, Output, Seen),
            SyncResult = file:sync(Output),
            CloseResult = file:close(Output),
            finish_repair(
                Path,
                TempPath,
                BackupPath,
                Result,
                SyncResult,
                CloseResult
            );
        {error, Reason} ->
            io:format(
                standard_error,
                "cannot create repair file ~s: ~p~n",
                [TempPath, Reason]
            ),
            halt(1)
    end;
main(_) ->
    io:format(
        standard_error,
        "usage: repair_timeline.escript (--check | --repair) PATH~n",
        []
    ),
    halt(2).

process_file(Path, Output, Seen) ->
    case file:open(Path, [read, binary, {read_ahead, 65536}]) of
        {ok, Input} ->
            try
                {ok, process_lines(Input, Output, Seen, empty_stats())}
            after
                file:close(Input)
            end;
        Error ->
            Error
    end.

process_lines(Input, Output, Seen, Stats) ->
    case io:get_line(Input, "") of
        eof ->
            Stats;
        {error, Reason} ->
            erlang:error({read_failed, Reason});
        Line when is_binary(Line) ->
            {OutputLine, Key, Depth, Valid} = normalize_line(Line),
            NewStats0 = Stats#{
                total => maps:get(total, Stats) + 1,
                wrapped => maps:get(wrapped, Stats) +
                    case Depth > 0 of true -> 1; false -> 0 end,
                max_depth => max(Depth, maps:get(max_depth, Stats)),
                malformed => maps:get(malformed, Stats) +
                    case Valid of true -> 0; false -> 1 end
            },
            NewStats = case ets:insert_new(Seen, {Key}) of
                true ->
                    ok = maybe_write(Output, OutputLine),
                    NewStats0#{unique => maps:get(unique, NewStats0) + 1};
                false ->
                    NewStats0#{duplicates => maps:get(duplicates, NewStats0) + 1}
            end,
            process_lines(Input, Output, Seen, NewStats)
    end.

normalize_line(Line) ->
    try
        Map = json:decode(Line),
        {Canonical, Depth} = unwrap_replay(Map, 0),
        Encoded = iolist_to_binary(json:encode(Canonical)),
        Key = case maps:get(<<"id">>, Canonical, undefined) of
            undefined -> {content, erlang:md5(Encoded)};
            Id -> {id, Id}
        end,
        {<<Encoded/binary, "\n">>, Key, Depth, true}
    catch
        _:_ ->
            {ensure_newline(Line), {malformed, erlang:md5(Line)}, 0, false}
    end.

unwrap_replay(Map, Depth) when is_map(Map) ->
    case {
        lists:sort(maps:keys(Map)),
        maps:get(<<"type">>, Map, undefined),
        maps:get(<<"agent_id">>, Map, undefined),
        maps:get(<<"data">>, Map, undefined)
    } of
        {
            [<<"agent_id">>, <<"data">>, <<"id">>, <<"timestamp">>, <<"type">>],
            <<"unknown">>,
            <<"undefined">>,
            Data
        } when is_map(Data) ->
            unwrap_replay(Data, Depth + 1);
        _ ->
            {Map, Depth}
    end;
unwrap_replay(Value, Depth) ->
    {Value, Depth}.

ensure_newline(<<>>) ->
    <<"\n">>;
ensure_newline(Line) ->
    case binary:last(Line) of
        $\n -> Line;
        _ -> <<Line/binary, "\n">>
    end.

maybe_write(undefined, _Line) ->
    ok;
maybe_write(Output, Line) ->
    file:write(Output, Line).

finish_repair(
    Path,
    TempPath,
    BackupPath,
    {ok, Stats},
    ok,
    ok
) ->
    case file:rename(Path, BackupPath) of
        ok ->
            case file:rename(TempPath, Path) of
                ok ->
                    case file:delete(BackupPath) of
                        ok ->
                            print_stats("repair", Path, Stats),
                            halt(0);
                        {error, Reason} ->
                            io:format(
                                standard_error,
                                "repair succeeded but backup remains at ~s: ~p~n",
                                [BackupPath, Reason]
                            ),
                            halt(1)
                    end;
                {error, Reason} ->
                    _ = file:rename(BackupPath, Path),
                    io:format(
                        standard_error,
                        "cannot install repaired timeline: ~p~n",
                        [Reason]
                    ),
                    halt(1)
            end;
        {error, Reason} ->
            _ = file:delete(TempPath),
            io:format(
                standard_error,
                "cannot stage original timeline: ~p~n",
                [Reason]
            ),
            halt(1)
    end;
finish_repair(_Path, TempPath, _BackupPath, Result, Sync, Close) ->
    _ = file:delete(TempPath),
    io:format(
        standard_error,
        "repair aborted: process=~p sync=~p close=~p~n",
        [Result, Sync, Close]
    ),
    halt(1).

empty_stats() ->
    #{
        total => 0,
        unique => 0,
        duplicates => 0,
        wrapped => 0,
        max_depth => 0,
        malformed => 0
    }.

print_stats(Action, Path, Stats) ->
    io:format(
        "~s ~s: total=~B unique=~B duplicates=~B wrapped=~B "
        "max_depth=~B malformed=~B~n",
        [
            Action,
            Path,
            maps:get(total, Stats),
            maps:get(unique, Stats),
            maps:get(duplicates, Stats),
            maps:get(wrapped, Stats),
            maps:get(max_depth, Stats),
            maps:get(malformed, Stats)
        ]
    ).
