-module(conversation_stats_logger).
-behaviour(gen_server).

%% API
-export([start_link/0, log_message/4, log_conversation_start/2, log_conversation_end/2,
         get_stats/0, reset_stats/0, enable_periodic_display/0, disable_periodic_display/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Colorful logging macros
-define(LOG_CONVERSATION(Msg), colored_logger:ocean(deep, "[CONVERSATION] " ++ Msg)).
-define(LOG_CONVERSATION(Msg, Args), colored_logger:ocean(deep, io_lib:format("[CONVERSATION] " ++ Msg, Args))).
-define(LOG_STATS(Msg), colored_logger:matrix(green, "[STATS] " ++ Msg)).
-define(LOG_STATS(Msg, Args), colored_logger:matrix(green, io_lib:format("[STATS] " ++ Msg, Args))).
-define(LOG_MESSAGE(Msg), colored_logger:neural(active, "[MESSAGE] " ++ Msg)).
-define(LOG_MESSAGE(Msg, Args), colored_logger:neural(active, io_lib:format("[MESSAGE] " ++ Msg, Args))).

-record(state, {
    conversations = #{}, % ConversationId -> {AgentId, StartTime, MessageCount, LastActivity}
    messages = [], % {Timestamp, ConversationId, Sender, MessagePreview, AgentId}
    stats = #{
        total_conversations => 0,
        active_conversations => 0,
        total_messages => 0,
        messages_per_minute => 0,
        avg_conversation_length => 0,
        most_active_agent => undefined,
        peak_concurrent_conversations => 0,
        agents_stats => #{}
    },
    periodic_display = true,
    last_display = 0
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log_message(ConversationId, AgentId, Sender, Message) ->
    gen_server:cast(?MODULE, {log_message, ConversationId, AgentId, Sender, Message}).

log_conversation_start(ConversationId, AgentId) ->
    gen_server:cast(?MODULE, {conversation_start, ConversationId, AgentId}).

log_conversation_end(ConversationId, AgentId) ->
    gen_server:cast(?MODULE, {conversation_end, ConversationId, AgentId}).

get_stats() ->
    gen_server:call(?MODULE, get_stats).

reset_stats() ->
    gen_server:cast(?MODULE, reset_stats).

enable_periodic_display() ->
    gen_server:cast(?MODULE, enable_periodic_display).

disable_periodic_display() ->
    gen_server:cast(?MODULE, disable_periodic_display).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ?LOG_STATS("Starting conversation stats logger"),
    % Schedule periodic stats display every 30 seconds
    erlang:send_after(30000, self(), display_stats),
    {ok, #state{}}.

handle_call(get_stats, _From, State) ->
    {reply, State#state.stats, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({log_message, ConversationId, AgentId, Sender, Message}, State) ->
    Now = erlang:system_time(millisecond),
    
    % Update conversation info
    Conversations = maps:update_with(ConversationId, 
        fun({AId, StartTime, Count, _LastActivity}) ->
            {AId, StartTime, Count + 1, Now}
        end,
        {AgentId, Now, 1, Now},
        State#state.conversations),
    
    % Create message preview (first 100 chars)
    MessagePreview = case Message of
        MsgBin when is_binary(MsgBin) -> 
            Preview = binary:part(MsgBin, 0, min(100, byte_size(MsgBin))),
            case byte_size(MsgBin) > 100 of
                true -> <<Preview/binary, "...">>;
                false -> Preview
            end;
        MsgList when is_list(MsgList) ->
            StrMsg = lists:flatten(MsgList),
            case length(StrMsg) > 100 of
                true -> lists:sublist(StrMsg, 100) ++ "...";
                false -> StrMsg
            end;
        _ -> 
            io_lib:format("~p", [Message])
    end,
    
    % Add to messages log (keep last 100 messages)
    MessageEntry = {Now, ConversationId, Sender, MessagePreview, AgentId},
    Messages = lists:sublist([MessageEntry | State#state.messages], 100),
    
    % Log the message with beautiful formatting
    ConvIdShort = case ConversationId of
        ConvBin when is_binary(ConvBin) -> binary:part(ConvBin, 0, min(8, byte_size(ConvBin)));
        _ -> ConversationId
    end,
    
    AgentIdShort = case AgentId of
        AgentBin when is_binary(AgentBin) -> binary:part(AgentBin, 0, min(12, byte_size(AgentBin)));
        _ -> AgentId
    end,
    
    ?LOG_MESSAGE("Conv:~s Agent:~s [~s] ~s", [ConvIdShort, AgentIdShort, Sender, MessagePreview]),
    
    % Update stats
    NewStats = update_stats(State#state.stats, Conversations, Messages),
    
    NewState = State#state{
        conversations = Conversations,
        messages = Messages,
        stats = NewStats
    },
    
    {noreply, NewState};

handle_cast({conversation_start, ConversationId, AgentId}, State) ->
    Now = erlang:system_time(millisecond),
    ?LOG_CONVERSATION("Starting conversation ~s with agent ~s", [ConversationId, AgentId]),
    
    Conversations = (State#state.conversations)#{ConversationId => {AgentId, Now, 0, Now}},
    NewStats = update_stats(State#state.stats, Conversations, State#state.messages),
    
    {noreply, State#state{conversations = Conversations, stats = NewStats}};

handle_cast({conversation_end, ConversationId, AgentId}, State) ->
    case maps:get(ConversationId, State#state.conversations, undefined) of
        undefined ->
            {noreply, State};
        {AgentId, StartTime, MessageCount, _LastActivity} ->
            Duration = erlang:system_time(millisecond) - StartTime,
            ?LOG_CONVERSATION("Ending conversation ~s with agent ~s (~p messages, ~p seconds)", 
                [ConversationId, AgentId, MessageCount, Duration div 1000]),
            
            Conversations = maps:remove(ConversationId, State#state.conversations),
            NewStats = update_stats(State#state.stats, Conversations, State#state.messages),
            
            {noreply, State#state{conversations = Conversations, stats = NewStats}}
    end;

handle_cast(reset_stats, State) ->
    ?LOG_STATS("Resetting all conversation statistics"),
    {noreply, State#state{
        conversations = #{},
        messages = [],
        stats = #{
            total_conversations => 0,
            active_conversations => 0,
            total_messages => 0,
            messages_per_minute => 0,
            avg_conversation_length => 0,
            most_active_agent => undefined,
            peak_concurrent_conversations => 0,
            agents_stats => #{}
        }
    }};

handle_cast(enable_periodic_display, State) ->
    ?LOG_STATS("Enabling periodic stats display"),
    {noreply, State#state{periodic_display = true}};

handle_cast(disable_periodic_display, State) ->
    ?LOG_STATS("Disabling periodic stats display"),
    {noreply, State#state{periodic_display = false}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(display_stats, State) ->
    case State#state.periodic_display of
        true ->
            display_periodic_stats(State);
        false ->
            ok
    end,
    
    % Schedule next display
    erlang:send_after(30000, self(), display_stats),
    {noreply, State#state{last_display = erlang:system_time(millisecond)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

update_stats(Stats, Conversations, Messages) ->
    ActiveConversations = maps:size(Conversations),
    TotalMessages = length(Messages),
    
    % Calculate messages per minute
    Now = erlang:system_time(millisecond),
    RecentMessages = lists:filter(fun({Timestamp, _, _, _, _}) ->
        Now - Timestamp < 60000 % Last minute
    end, Messages),
    MessagesPerMinute = length(RecentMessages),
    
    % Find most active agent
    AgentStats = calculate_agent_stats(Messages),
    MostActiveAgent = find_most_active_agent(AgentStats),
    
    % Calculate average conversation length
    ConversationLengths = [Count || {_, _, Count, _} <- maps:values(Conversations)],
    AvgConvLength = case ConversationLengths of
        [] -> 0;
        Lengths -> lists:sum(Lengths) / length(Lengths)
    end,
    
    PeakConcurrent = max(maps:get(peak_concurrent_conversations, Stats, 0), ActiveConversations),
    TotalConversations = max(maps:get(total_conversations, Stats, 0), ActiveConversations),
    
    Stats#{
        total_conversations => TotalConversations,
        active_conversations => ActiveConversations,
        total_messages => TotalMessages,
        messages_per_minute => MessagesPerMinute,
        avg_conversation_length => round(AvgConvLength * 100) / 100,
        most_active_agent => MostActiveAgent,
        peak_concurrent_conversations => PeakConcurrent,
        agents_stats => AgentStats
    }.

calculate_agent_stats(Messages) ->
    AgentCounts = lists:foldl(fun({_, _, _, _, AgentId}, Acc) ->
        maps:update_with(AgentId, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, Messages),
    
    maps:map(fun(AgentId, Count) ->
        RecentCount = length(lists:filter(fun({Timestamp, _, _, _, AId}) ->
            AId =:= AgentId andalso 
            erlang:system_time(millisecond) - Timestamp < 300000 % Last 5 minutes
        end, Messages)),
        #{total_messages => Count, recent_messages => RecentCount}
    end, AgentCounts).

find_most_active_agent(AgentStats) ->
    case maps:to_list(AgentStats) of
        [] -> undefined;
        AgentList ->
            {MostActive, _Stats} = lists:foldl(fun({AgentId, #{total_messages := Count}}, {BestAgent, #{total_messages := BestCount}}) ->
                case Count > BestCount of
                    true -> {AgentId, #{total_messages => Count}};
                    false -> {BestAgent, #{total_messages => BestCount}}
                end
            end, hd(AgentList), tl(AgentList)),
            MostActive
    end.

display_periodic_stats(State) ->
    Stats = State#state.stats,
    
    % Create a beautiful stats table
    colored_logger:celebration(party, ""),
    colored_logger:celebration(party, "╔══════════════════════════════════════════════════════════════════════════════╗"),
    colored_logger:celebration(party, "║                           🤖 CONVERSATION STATISTICS 📊                     ║"),
    colored_logger:celebration(party, "╠══════════════════════════════════════════════════════════════════════════════╣"),
    
    Now = calendar:system_time_to_rfc3339(erlang:system_time(second)),
    colored_logger:matrix(green, io_lib:format("║ Timestamp: ~s                                       ║", [Now])),
    colored_logger:celebration(party, "╠══════════════════════════════════════════════════════════════════════════════╣"),
    
    % Main stats
    colored_logger:ocean(deep, io_lib:format("║ 💬 Active Conversations: ~-10p  📈 Peak Concurrent: ~-10p        ║", 
        [maps:get(active_conversations, Stats), maps:get(peak_concurrent_conversations, Stats)])),
    
    colored_logger:neural(active, io_lib:format("║ 📝 Total Messages: ~-15p  ⚡ Messages/Min: ~-10p           ║", 
        [maps:get(total_messages, Stats), maps:get(messages_per_minute, Stats)])),
    
    colored_logger:system(cpu, io_lib:format("║ 📊 Avg Conv Length: ~-12.1f  🎯 Total Conversations: ~-8p     ║", 
        [maps:get(avg_conversation_length, Stats), maps:get(total_conversations, Stats)])),
    
    MostActive = maps:get(most_active_agent, Stats),
    MostActiveStr = case MostActive of
        undefined -> "None";
        MostActiveBin when is_binary(MostActiveBin) -> binary_to_list(binary:part(MostActiveBin, 0, min(20, byte_size(MostActiveBin))));
        _ -> io_lib:format("~p", [MostActive])
    end,
    colored_logger:fire(bright, io_lib:format("║ 🏆 Most Active Agent: ~-20s                                    ║", [MostActiveStr])),
    
    colored_logger:celebration(party, "╠══════════════════════════════════════════════════════════════════════════════╣"),
    
    % Recent conversations
    ActiveConvs = maps:to_list(State#state.conversations),
    if length(ActiveConvs) > 0 ->
        colored_logger:ocean(deep, "║                            🔄 ACTIVE CONVERSATIONS                            ║"),
        colored_logger:celebration(party, "╠══════════════════════════════════════════════════════════════════════════════╣"),
        
        lists:foreach(fun({ConvId, {AgentId, StartTime, MsgCount, LastActivity}}) ->
            ConvIdShort = case ConvId of
                ConvBin when is_binary(ConvBin) -> binary_to_list(binary:part(ConvBin, 0, min(8, byte_size(ConvBin))));
                _ -> io_lib:format("~p", [ConvId])
            end,
            AgentIdShort = case AgentId of
                AgentBin when is_binary(AgentBin) -> binary_to_list(binary:part(AgentBin, 0, min(15, byte_size(AgentBin))));
                _ -> io_lib:format("~p", [AgentId])
            end,
            Duration = (erlang:system_time(millisecond) - StartTime) div 1000,
            LastActivitySecs = (erlang:system_time(millisecond) - LastActivity) div 1000,
            
            colored_logger:neural(active, io_lib:format("║ Conv:~s Agent:~-15s Msgs:~-3p Duration:~-4ps Last:~-3ps ║", 
                [ConvIdShort, AgentIdShort, MsgCount, Duration, LastActivitySecs]))
        end, lists:sublist(ActiveConvs, 5)); % Show max 5 active conversations
    true ->
        colored_logger:data(processed, "║                           No active conversations                           ║")
    end,
    
    % Recent messages
    RecentMessages = lists:sublist(State#state.messages, 5),
    if length(RecentMessages) > 0 ->
        colored_logger:celebration(party, "╠══════════════════════════════════════════════════════════════════════════════╣"),
        colored_logger:neural(active, "║                              📝 RECENT MESSAGES                              ║"),
        colored_logger:celebration(party, "╠══════════════════════════════════════════════════════════════════════════════╣"),
        
        lists:foreach(fun({Timestamp, ConvId, Sender, Preview, _}) ->
            SecsAgo = (erlang:system_time(millisecond) - Timestamp) div 1000,
            ConvShort = case ConvId of
                ConvShortBin when is_binary(ConvShortBin) -> binary_to_list(binary:part(ConvShortBin, 0, min(6, byte_size(ConvShortBin))));
                _ -> "unknown"
            end,
            PreviewShort = case Preview of
                PreviewBin when is_binary(PreviewBin) -> binary_to_list(binary:part(PreviewBin, 0, min(40, byte_size(PreviewBin))));
                PreviewList when is_list(PreviewList) -> lists:sublist(PreviewList, 40);
                _ -> "..."
            end,
            
            colored_logger:ocean(deep, io_lib:format("║ ~-4ps ago [~s] ~-6s: ~-40s              ║", 
                [SecsAgo, ConvShort, Sender, PreviewShort]))
        end, RecentMessages);
    true ->
        colored_logger:data(processed, "║                              No recent messages                              ║")
    end,
    
    colored_logger:celebration(party, "╚══════════════════════════════════════════════════════════════════════════════╝"),
    colored_logger:celebration(party, "").