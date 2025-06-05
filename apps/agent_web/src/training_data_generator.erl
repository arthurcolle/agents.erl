-module(training_data_generator).
-export([generate_training_data/0, generate_training_data/1, 
         get_conversation_training_data/1, format_for_training/1,
         export_training_data/1, schedule_generation/0]).

-record(training_sample, {
    id,
    timestamp,
    conversation_id,
    agent_id,
    agent_name,
    input_message,
    output_message,
    context,
    metadata,
    quality_score
}).

%% Generate training data from all interactions
generate_training_data() ->
    generate_training_data(#{}).

generate_training_data(Options) ->
    colored_logger:info("Generating training data from all interactions"),
    
    % Get all timeline events
    Events = timeline_handler:get_all_events(),
    
    % Group by conversation ID
    ConversationGroups = group_by_conversation(Events),
    
    % Generate training samples from each conversation
    TrainingSamples = lists:flatten([
        generate_conversation_samples(ConvId, ConvEvents, Options)
        || {ConvId, ConvEvents} <- ConversationGroups
    ]),
    
    % Apply filters and quality scoring
    FilteredSamples = apply_filters(TrainingSamples, Options),
    
    % Store training data
    store_training_data(FilteredSamples),
    
    colored_logger:success(io_lib:format("Generated ~p training samples", [length(FilteredSamples)])),
    {ok, length(FilteredSamples)}.

%% Generate training data for a specific conversation
get_conversation_training_data(ConversationId) ->
    Events = timeline_handler:get_all_events(),
    ConversationEvents = [E || E <- Events, 
        maps:get(<<"conversationId">>, E, null) =:= ConversationId],
    
    SortedEvents = lists:sort(fun(A, B) ->
        maps:get(<<"timestamp">>, A) =< maps:get(<<"timestamp">>, B)
    end, ConversationEvents),
    
    generate_conversation_samples(ConversationId, SortedEvents, #{}).

%% Group events by conversation ID
group_by_conversation(Events) ->
    Groups = lists:foldl(fun(Event, Acc) ->
        ConvId = maps:get(<<"conversationId">>, Event, <<"unknown">>),
        Current = maps:get(ConvId, Acc, []),
        Acc#{ConvId => [Event | Current]}
    end, #{}, Events),
    
    % Sort events within each conversation by timestamp
    maps:fold(fun(ConvId, ConvEvents, Acc) ->
        SortedEvents = lists:sort(fun(A, B) ->
            maps:get(<<"timestamp">>, A) =< maps:get(<<"timestamp">>, B)
        end, ConvEvents),
        [{ConvId, SortedEvents} | Acc]
    end, [], Groups).

%% Generate training samples from a conversation
generate_conversation_samples(ConversationId, Events, Options) ->
    MessageEvents = [E || E <- Events, 
        maps:get(<<"type">>, E) =:= <<"message">>],
    
    generate_pairs_from_messages(MessageEvents, ConversationId, Options).

%% Generate input-output pairs from message events
generate_pairs_from_messages(Messages, ConversationId, _Options) ->
    generate_pairs_from_messages_acc(Messages, ConversationId, []).

generate_pairs_from_messages_acc([UserMsg, AgentMsg | Rest], ConversationId, Acc) ->
    % Check if we have a user->agent pair
    case {maps:get(<<"source">>, UserMsg), maps:get(<<"source">>, AgentMsg)} of
        {<<"user">>, <<"agent">>} ->
            Sample = create_training_sample(UserMsg, AgentMsg, ConversationId),
            generate_pairs_from_messages_acc([AgentMsg | Rest], ConversationId, [Sample | Acc]);
        _ ->
            % Skip if not a proper user->agent pair
            generate_pairs_from_messages_acc([AgentMsg | Rest], ConversationId, Acc)
    end;
generate_pairs_from_messages_acc([_SingleMsg], _ConversationId, Acc) ->
    % Can't create pair from single message
    Acc;
generate_pairs_from_messages_acc([], _ConversationId, Acc) ->
    lists:reverse(Acc).

%% Create a training sample from user and agent messages
create_training_sample(UserMsg, AgentMsg, ConversationId) ->
    Now = os:system_time(millisecond),
    Id = iolist_to_binary(io_lib:format("training_~p_~p", [ConversationId, Now])),
    
    #training_sample{
        id = Id,
        timestamp = Now,
        conversation_id = ConversationId,
        agent_id = maps:get(<<"agentId">>, AgentMsg, null),
        agent_name = maps:get(<<"agentName">>, AgentMsg, null),
        input_message = maps:get(<<"content">>, UserMsg),
        output_message = maps:get(<<"content">>, AgentMsg),
        context = extract_context(UserMsg, AgentMsg),
        metadata = merge_metadata(UserMsg, AgentMsg),
        quality_score = calculate_quality_score(UserMsg, AgentMsg)
    }.

%% Extract context information from messages
extract_context(UserMsg, AgentMsg) ->
    #{
        user_timestamp => maps:get(<<"timestamp">>, UserMsg),
        agent_timestamp => maps:get(<<"timestamp">>, AgentMsg),
        user_format => maps:get(<<"format">>, maps:get(<<"metadata">>, UserMsg, #{}), <<"plain">>),
        agent_format => maps:get(<<"format">>, maps:get(<<"metadata">>, AgentMsg, #{}), <<"plain">>),
        response_time_ms => calculate_response_time(UserMsg, AgentMsg)
    }.

%% Merge metadata from both messages
merge_metadata(UserMsg, AgentMsg) ->
    UserMeta = maps:get(<<"metadata">>, UserMsg, #{}),
    AgentMeta = maps:get(<<"metadata">>, AgentMsg, #{}),
    #{
        user_metadata => UserMeta,
        agent_metadata => AgentMeta
    }.

%% Calculate response time between messages
calculate_response_time(UserMsg, AgentMsg) ->
    try
        UserTime = parse_timestamp(maps:get(<<"timestamp">>, UserMsg)),
        AgentTime = parse_timestamp(maps:get(<<"timestamp">>, AgentMsg)),
        AgentTime - UserTime
    catch
        _:_ -> 0
    end.

%% Parse timestamp (handles both ISO and millisecond formats)
parse_timestamp(Timestamp) when is_binary(Timestamp) ->
    try
        % Try parsing as ISO 8601
        case re:run(Timestamp, "^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})\\.(\\d{3})Z$", 
                   [{capture, all_but_first, list}]) of
            {match, [Year, Month, Day, Hour, Min, Sec, Ms]} ->
                DateTime = {{list_to_integer(Year), list_to_integer(Month), list_to_integer(Day)},
                           {list_to_integer(Hour), list_to_integer(Min), list_to_integer(Sec)}},
                calendar:datetime_to_gregorian_seconds(DateTime) * 1000 + list_to_integer(Ms);
            nomatch ->
                % Try as milliseconds
                binary_to_integer(Timestamp)
        end
    catch
        _:_ -> 0
    end;
parse_timestamp(Timestamp) when is_integer(Timestamp) ->
    Timestamp;
parse_timestamp(_) ->
    0.

%% Calculate quality score for a training sample
calculate_quality_score(UserMsg, AgentMsg) ->
    UserContent = maps:get(<<"content">>, UserMsg, <<"">>),
    AgentContent = maps:get(<<"content">>, AgentMsg, <<"">>),
    
    Score = 0.0,
    
    % Length factors
    UserLen = byte_size(UserContent),
    AgentLen = byte_size(AgentContent),
    
    % Base score for having content
    Score1 = if 
        UserLen > 0 andalso AgentLen > 0 -> Score + 0.3;
        true -> Score
    end,
    
    % Penalize very short responses
    Score2 = if
        AgentLen < 10 -> Score1 - 0.2;
        true -> Score1
    end,
    
    % Reward detailed responses
    Score3 = if
        AgentLen > 100 -> Score2 + 0.2;
        AgentLen > 500 -> Score2 + 0.3;
        true -> Score2
    end,
    
    % Check for error patterns
    Score4 = case binary:match(AgentContent, <<"Error:">>) of
        nomatch -> Score3 + 0.2;
        _ -> Score3 - 0.3
    end,
    
    % Ensure score is between 0 and 1
    max(0.0, min(1.0, Score4)).

%% Apply filters to training samples
apply_filters(Samples, Options) ->
    MinQuality = maps:get(min_quality, Options, 0.3),
    MaxSamples = maps:get(max_samples, Options, 10000),
    
    % Filter by quality
    QualityFiltered = [S || S <- Samples, 
        S#training_sample.quality_score >= MinQuality],
    
    % Sort by quality and take top N
    Sorted = lists:sort(fun(A, B) ->
        A#training_sample.quality_score >= B#training_sample.quality_score
    end, QualityFiltered),
    
    lists:sublist(Sorted, MaxSamples).

%% Store training data
store_training_data(Samples) ->
    FilePath = get_training_data_file_path(),
    Data = [format_for_storage(S) || S <- Samples],
    JsonData = jsx:encode(#{
        <<"generated_at">> => format_iso8601(calendar:local_time()),
        <<"sample_count">> => length(Samples),
        <<"samples">> => Data
    }),
    
    file:write_file(FilePath, JsonData).

%% Format sample for storage
format_for_storage(#training_sample{} = Sample) ->
    #{
        <<"id">> => Sample#training_sample.id,
        <<"timestamp">> => Sample#training_sample.timestamp,
        <<"conversation_id">> => Sample#training_sample.conversation_id,
        <<"agent_id">> => Sample#training_sample.agent_id,
        <<"agent_name">> => Sample#training_sample.agent_name,
        <<"input">> => Sample#training_sample.input_message,
        <<"output">> => Sample#training_sample.output_message,
        <<"context">> => Sample#training_sample.context,
        <<"metadata">> => Sample#training_sample.metadata,
        <<"quality_score">> => Sample#training_sample.quality_score
    }.

%% Format training data for different ML frameworks
format_for_training(Format) ->
    FilePath = get_training_data_file_path(),
    case file:read_file(FilePath) of
        {ok, Data} ->
            TrainingData = jsx:decode(Data, [return_maps]),
            Samples = maps:get(<<"samples">>, TrainingData, []),
            
            case Format of
                openai ->
                    format_for_openai(Samples);
                huggingface ->
                    format_for_huggingface(Samples);
                jsonl ->
                    format_for_jsonl(Samples);
                _ ->
                    {error, unsupported_format}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Format for OpenAI fine-tuning
format_for_openai(Samples) ->
    Formatted = [#{
        <<"messages">> => [
            #{<<"role">> => <<"user">>, <<"content">> => maps:get(<<"input">>, S)},
            #{<<"role">> => <<"assistant">>, <<"content">> => maps:get(<<"output">>, S)}
        ]
    } || S <- Samples],
    
    JsonlLines = [jsx:encode(Line) || Line <- Formatted],
    iolist_to_binary(lists:join(<<"\n">>, JsonlLines)).

%% Format for HuggingFace
format_for_huggingface(Samples) ->
    Formatted = [#{
        <<"instruction">> => maps:get(<<"input">>, S),
        <<"output">> => maps:get(<<"output">>, S),
        <<"input">> => <<"">>
    } || S <- Samples],
    
    jsx:encode(Formatted).

%% Format as JSONL
format_for_jsonl(Samples) ->
    JsonlLines = [jsx:encode(S) || S <- Samples],
    iolist_to_binary(lists:join(<<"\n">>, JsonlLines)).

%% Export training data in specified format
export_training_data(Format) ->
    case format_for_training(Format) of
        {error, Reason} ->
            {error, Reason};
        FormattedData ->
            Filename = io_lib:format("training_data_~s_~p.~s", 
                [atom_to_list(Format), os:system_time(second), 
                 case Format of
                     jsonl -> "jsonl";
                     openai -> "jsonl";
                     _ -> "json"
                 end]),
            FilePath = filename:join([code:priv_dir(agent_web), "data", Filename]),
            file:write_file(FilePath, FormattedData),
            {ok, FilePath}
    end.

%% Schedule periodic training data generation
schedule_generation() ->
    case whereis(training_data_scheduler) of
        undefined ->
            Pid = spawn(fun() -> 
                register(training_data_scheduler, self()),
                generation_loop()
            end),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

%% Periodic generation loop
generation_loop() ->
    % Generate training data every hour
    timer:sleep(3600000), % 1 hour
    try
        generate_training_data()
    catch
        _:Error ->
            colored_logger:error(io_lib:format("Training data generation failed: ~p", [Error]))
    end,
    generation_loop().

%% Helper functions
get_training_data_file_path() ->
    filename:join([code:priv_dir(agent_web), "data", "training_data.json"]).

%% Fallback for iso8601 formatting if not available
format_iso8601(DateTime) ->
    {{Y, M, D}, {H, Min, S}} = DateTime,
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ", 
        [Y, M, D, H, Min, S])).