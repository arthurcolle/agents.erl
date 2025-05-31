-module(mcp_json_decoder).

-export([init/0,
         decode_line/2,
         decode_chunk/2]).

-record(decoder_state, {
    buffer = <<>>,
    partial_json = <<>>
}).

init() ->
    #decoder_state{}.

decode_line(Line, State) ->
    % Try to decode a complete JSON line
    case jsx:is_json(Line) of
        true ->
            try
                Message = jsx:decode(Line, [return_maps]),
                {ok, Message, State#decoder_state{buffer = <<>>}}
            catch
                error:Reason ->
                    {error, {decode_error, Reason}, State}
            end;
        false ->
            % Not valid JSON, accumulate
            NewBuffer = <<(State#decoder_state.buffer)/binary, Line/binary, "\n">>,
            {continue, State#decoder_state{buffer = NewBuffer}}
    end.

decode_chunk(Chunk, State) ->
    % Handle streaming JSON chunks
    Buffer = <<(State#decoder_state.buffer)/binary, Chunk/binary>>,
    
    % Try to find complete JSON objects
    case extract_json_objects(Buffer) of
        {[], Remaining} ->
            {continue, State#decoder_state{buffer = Remaining}};
        {Objects, Remaining} ->
            {ok, Objects, State#decoder_state{buffer = Remaining}}
    end.

extract_json_objects(Buffer) ->
    extract_json_objects(Buffer, [], 0, 0, false, false).

extract_json_objects(<<>>, Acc, _Depth, _Start, _InString, _Escape) ->
    {lists:reverse(Acc), <<>>};
    
extract_json_objects(<<$\\, Rest/binary>>, Acc, Depth, Start, true, false) ->
    % Escape character in string
    extract_json_objects(Rest, Acc, Depth, Start, true, true);
    
extract_json_objects(<<_C, Rest/binary>>, Acc, Depth, Start, true, true) ->
    % Character after escape
    extract_json_objects(Rest, Acc, Depth, Start, true, false);
    
extract_json_objects(<<$", Rest/binary>>, Acc, Depth, Start, InString, false) ->
    % Toggle string state
    extract_json_objects(Rest, Acc, Depth, Start, not InString, false);
    
extract_json_objects(<<${, Rest/binary>>, Acc, 0, 0, false, false) ->
    % Start of object at root level
    extract_json_objects(Rest, Acc, 1, 0, false, false);
    
extract_json_objects(<<${, Rest/binary>>, Acc, Depth, Start, false, false) when Depth > 0 ->
    % Nested object
    extract_json_objects(Rest, Acc, Depth + 1, Start, false, false);
    
extract_json_objects(<<$}, Rest/binary>>, Acc, 1, Start, false, false) ->
    % End of root object
    Pos = byte_size(<<${, Rest/binary>>) - byte_size(Rest),
    ObjectBin = binary:part(<<${, Rest/binary>>, Start, Pos - Start),
    
    case jsx:is_json(ObjectBin) of
        true ->
            try
                Object = jsx:decode(ObjectBin, [return_maps]),
                extract_json_objects(Rest, [Object | Acc], 0, 0, false, false)
            catch
                _:_ ->
                    % Skip malformed JSON
                    extract_json_objects(Rest, Acc, 0, 0, false, false)
            end;
        false ->
            extract_json_objects(Rest, Acc, 0, 0, false, false)
    end;
    
extract_json_objects(<<$}, Rest/binary>>, Acc, Depth, Start, false, false) when Depth > 1 ->
    % End of nested object
    extract_json_objects(Rest, Acc, Depth - 1, Start, false, false);
    
extract_json_objects(<<_C, Rest/binary>>, Acc, Depth, Start, InString, false) ->
    % Any other character
    extract_json_objects(Rest, Acc, Depth, Start, InString, false).