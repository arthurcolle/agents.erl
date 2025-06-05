-module(agent_quorum).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    propose_decision/5,
    cast_vote/3,
    get_decision/1,
    list_decisions/0,
    get_agent_voting_history/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(vote, {
    vote :: yes | no | abstain,
    reason :: binary(),
    timestamp :: integer()
}).

-type vote() :: #vote{}.

-record(decision, {
    id :: binary(),
    topic :: binary(),
    proposer :: binary(),
    participants :: [binary()],
    votes = #{} :: #{binary() => vote()},
    threshold :: float(),
    status = proposed :: proposed | voting | decided | failed,
    result :: undefined | approved | rejected,
    deadline :: integer(),
    created_at :: integer()
}).

-type decision() :: #decision{}.

-record(state, {
    decisions = #{} :: #{binary() => decision()},
    voting_history = #{} :: #{binary() => [binary()]}
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

propose_decision(Topic, Proposer, Participants, Threshold, Deadline) ->
    gen_server:call(?MODULE, {propose, Topic, Proposer, Participants, Threshold, Deadline}).

cast_vote(DecisionId, AgentId, Vote) when Vote =:= yes; Vote =:= no; Vote =:= abstain ->
    gen_server:call(?MODULE, {vote, DecisionId, AgentId, Vote}).

get_decision(DecisionId) ->
    gen_server:call(?MODULE, {get_decision, DecisionId}).

list_decisions() ->
    gen_server:call(?MODULE, list_decisions).

get_agent_voting_history(AgentId) ->
    gen_server:call(?MODULE, {voting_history, AgentId}).

%% gen_server callbacks
init([]) ->
    % Start periodic cleanup of expired decisions
    timer:send_interval(5000, cleanup_expired),
    {ok, #state{}}.

handle_call({propose, Topic, Proposer, Participants, Threshold, Deadline}, _From, State) ->
    DecisionId = generate_decision_id(),
    Decision = #decision{
        id = DecisionId,
        topic = Topic,
        proposer = Proposer,
        participants = Participants,
        threshold = Threshold,
        status = voting,
        deadline = Deadline,
        created_at = erlang:system_time(millisecond)
    },
    
    NewState = State#state{
        decisions = maps:put(DecisionId, Decision, State#state.decisions)
    },
    
    % Notify agents about new decision
    notify_participants(Decision, proposal),
    broadcast_quorum_update(Decision),
    
    {reply, {ok, decision_to_map(Decision)}, NewState};

handle_call({vote, DecisionId, AgentId, {VoteValue, Reason}}, _From, State) when is_binary(Reason) ->
    process_vote(DecisionId, AgentId, VoteValue, Reason, State);

handle_call({vote, DecisionId, AgentId, VoteValue}, _From, State) ->
    case maps:get(DecisionId, State#state.decisions, undefined) of
        undefined ->
            {reply, {error, decision_not_found}, State};
        Decision when Decision#decision.status =/= voting ->
            {reply, {error, voting_closed}, State};
        Decision ->
            case lists:member(AgentId, Decision#decision.participants) of
                false ->
                    {reply, {error, not_participant}, State};
                true ->
                    Vote = #vote{
                        vote = VoteValue,
                        reason = <<"No reason provided">>,
                        timestamp = erlang:system_time(millisecond)
                    },
                    
                    UpdatedVotes = maps:put(AgentId, Vote, Decision#decision.votes),
                    UpdatedDecision = Decision#decision{votes = UpdatedVotes},
                    
                    % Check if we have enough votes to decide
                    FinalDecision = maybe_finalize_decision(UpdatedDecision),
                    
                    NewState = State#state{
                        decisions = maps:put(DecisionId, FinalDecision, State#state.decisions),
                        voting_history = update_voting_history(AgentId, DecisionId, State#state.voting_history)
                    },
                    
                    broadcast_quorum_update(FinalDecision),
                    
                    {reply, {ok, decision_to_map(FinalDecision)}, NewState}
            end
    end;

handle_call({get_decision, DecisionId}, _From, State) ->
    case maps:get(DecisionId, State#state.decisions, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Decision -> {reply, {ok, decision_to_map(Decision)}, State}
    end;

handle_call(list_decisions, _From, State) ->
    Decisions = [decision_to_map(D) || D <- maps:values(State#state.decisions)],
    {reply, {ok, Decisions}, State};

handle_call({voting_history, AgentId}, _From, State) ->
    History = maps:get(AgentId, State#state.voting_history, []),
    {reply, {ok, History}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired, State) ->
    Now = erlang:system_time(millisecond),
    UpdatedDecisions = maps:map(fun(_Id, Decision) ->
        case Decision#decision.status of
            voting when Decision#decision.deadline < Now ->
                finalize_expired_decision(Decision);
            _ ->
                Decision
        end
    end, State#state.decisions),
    {noreply, State#state{decisions = UpdatedDecisions}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
process_vote(DecisionId, AgentId, VoteValue, Reason, State) ->
    case maps:get(DecisionId, State#state.decisions, undefined) of
        undefined ->
            {reply, {error, decision_not_found}, State};
        Decision when Decision#decision.status =/= voting ->
            {reply, {error, voting_closed}, State};
        Decision ->
            case lists:member(AgentId, Decision#decision.participants) of
                false ->
                    {reply, {error, not_participant}, State};
                true ->
                    Vote = #vote{
                        vote = VoteValue,
                        reason = Reason,
                        timestamp = erlang:system_time(millisecond)
                    },
                    
                    UpdatedVotes = maps:put(AgentId, Vote, Decision#decision.votes),
                    UpdatedDecision = Decision#decision{votes = UpdatedVotes},
                    
                    % Check if we have enough votes to decide
                    FinalDecision = maybe_finalize_decision(UpdatedDecision),
                    
                    NewState = State#state{
                        decisions = maps:put(DecisionId, FinalDecision, State#state.decisions),
                        voting_history = update_voting_history(AgentId, DecisionId, State#state.voting_history)
                    },
                    
                    broadcast_quorum_update(FinalDecision),
                    
                    {reply, {ok, decision_to_map(FinalDecision)}, NewState}
            end
    end.

generate_decision_id() ->
    list_to_binary(io_lib:format("decision_~p_~p", [
        erlang:system_time(millisecond),
        rand:uniform(10000)
    ])).

maybe_finalize_decision(Decision) ->
    TotalParticipants = length(Decision#decision.participants),
    VotesCast = maps:size(Decision#decision.votes),
    
    % Check if all votes are in
    if
        VotesCast =:= TotalParticipants ->
            finalize_decision(Decision);
        true ->
            % Check if we already have enough votes to meet or fail threshold
            YesVotes = count_votes(Decision#decision.votes, yes),
            
            RequiredVotes = ceiling(TotalParticipants * Decision#decision.threshold),
            MaxPossibleYes = YesVotes + (TotalParticipants - VotesCast),
            
            if
                YesVotes >= RequiredVotes ->
                    % Already met threshold
                    Decision#decision{
                        status = decided,
                        result = approved
                    };
                MaxPossibleYes < RequiredVotes ->
                    % Cannot meet threshold even with remaining votes
                    Decision#decision{
                        status = decided,
                        result = rejected
                    };
                true ->
                    Decision
            end
    end.

finalize_decision(Decision) ->
    YesVotes = count_votes(Decision#decision.votes, yes),
    TotalParticipants = length(Decision#decision.participants),
    RequiredVotes = ceiling(TotalParticipants * Decision#decision.threshold),
    
    Result = if
        YesVotes >= RequiredVotes -> approved;
        true -> rejected
    end,
    
    FinalDecision = Decision#decision{
        status = decided,
        result = Result
    },
    
    notify_participants(FinalDecision, result),
    FinalDecision.

finalize_expired_decision(Decision) ->
    FinalDecision = finalize_decision(Decision#decision{status = failed}),
    broadcast_quorum_update(FinalDecision),
    FinalDecision.

count_votes(Votes, VoteType) ->
    maps:fold(fun(_AgentId, Vote, Acc) ->
        case Vote#vote.vote of
            VoteType -> Acc + 1;
            _ -> Acc
        end
    end, 0, Votes).

ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

update_voting_history(AgentId, DecisionId, History) ->
    AgentHistory = maps:get(AgentId, History, []),
    maps:put(AgentId, [DecisionId | AgentHistory], History).

notify_participants(Decision, Type) ->
    % Send notification to each participating agent
    lists:foreach(fun(AgentId) ->
        case agent_registry:find_agent(AgentId) of
            {ok, Pid} ->
                Pid ! {quorum_notification, Type, decision_to_map(Decision)};
            _ ->
                ok
        end
    end, Decision#decision.participants).

broadcast_quorum_update(Decision) ->
    Event = #{
        type => quorum_update,
        decision => decision_to_map(Decision)
    },
    case catch gproc:send({p, l, websocket}, {broadcast, jsx:encode(Event)}) of
        {'EXIT', _} -> ok;
        _ -> ok
    end.

decision_to_map(#decision{votes = Votes} = Decision) ->
    #{
        id => Decision#decision.id,
        topic => Decision#decision.topic,
        proposer => Decision#decision.proposer,
        participants => Decision#decision.participants,
        votes => maps:map(fun(_K, V) -> vote_to_map(V) end, Votes),
        threshold => Decision#decision.threshold,
        status => atom_to_binary(Decision#decision.status, utf8),
        result => case Decision#decision.result of
            undefined -> null;
            R -> atom_to_binary(R, utf8)
        end,
        deadline => Decision#decision.deadline,
        createdAt => Decision#decision.created_at
    }.

vote_to_map(#vote{} = Vote) ->
    #{
        vote => atom_to_binary(Vote#vote.vote, utf8),
        reason => Vote#vote.reason,
        timestamp => Vote#vote.timestamp
    }.