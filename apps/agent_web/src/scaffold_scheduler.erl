%%%-------------------------------------------------------------------
%%% @doc Scaffold Scheduler
%%% Manages scheduled updates and ensures endpoints are refreshed hourly
%%% @end
%%%-------------------------------------------------------------------
-module(scaffold_scheduler).
-behaviour(gen_server).

-export([start_link/0]).
-export([
    schedule_update/2,
    cancel_update/1,
    force_update_now/0,
    get_schedule_info/0,
    set_update_interval/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    schedules = #{} :: map(),
    default_interval = 3600000 :: integer(), % 1 hour in milliseconds
    last_update :: erlang:timestamp() | undefined,
    update_count = 0 :: integer()
}).

-record(schedule, {
    name :: atom(),
    interval :: integer(),
    timer_ref :: reference(),
    last_run :: erlang:timestamp() | undefined,
    run_count = 0 :: integer()
}).

%% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

schedule_update(Name, IntervalMs) ->
    gen_server:call(?MODULE, {schedule_update, Name, IntervalMs}).

cancel_update(Name) ->
    gen_server:call(?MODULE, {cancel_update, Name}).

force_update_now() ->
    gen_server:call(?MODULE, force_update_now).

get_schedule_info() ->
    gen_server:call(?MODULE, get_schedule_info).

set_update_interval(IntervalMs) ->
    gen_server:call(?MODULE, {set_update_interval, IntervalMs}).

%% gen_server callbacks

init([]) ->
    State = #state{},
    
    % Schedule default endpoint discovery
    self() ! {schedule_default_updates},
    
    {ok, State}.

handle_call({schedule_update, Name, IntervalMs}, _From, State) ->
    % Cancel existing schedule if any
    NewSchedules = case maps:get(Name, State#state.schedules, undefined) of
        undefined -> State#state.schedules;
        #schedule{timer_ref = OldRef} ->
            erlang:cancel_timer(OldRef),
            State#state.schedules
    end,
    
    % Create new schedule
    TimerRef = erlang:send_after(IntervalMs, self(), {run_scheduled, Name}),
    Schedule = #schedule{
        name = Name,
        interval = IntervalMs,
        timer_ref = TimerRef,
        last_run = undefined,
        run_count = 0
    },
    
    UpdatedSchedules = maps:put(Name, Schedule, NewSchedules),
    {reply, ok, State#state{schedules = UpdatedSchedules}};

handle_call({cancel_update, Name}, _From, State) ->
    case maps:get(Name, State#state.schedules, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #schedule{timer_ref = Ref} ->
            erlang:cancel_timer(Ref),
            NewSchedules = maps:remove(Name, State#state.schedules),
            {reply, ok, State#state{schedules = NewSchedules}}
    end;

handle_call(force_update_now, _From, State) ->
    % Run all discovery tasks immediately
    spawn(fun() -> run_all_updates() end),
    
    NewState = State#state{
        last_update = erlang:timestamp(),
        update_count = State#state.update_count + 1
    },
    {reply, ok, NewState};

handle_call(get_schedule_info, _From, State) ->
    Info = #{
        schedules => maps:map(fun(_Name, Schedule) ->
            #{
                interval => Schedule#schedule.interval,
                last_run => Schedule#schedule.last_run,
                run_count => Schedule#schedule.run_count
            }
        end, State#state.schedules),
        default_interval => State#state.default_interval,
        last_update => State#state.last_update,
        total_updates => State#state.update_count
    },
    {reply, Info, State};

handle_call({set_update_interval, IntervalMs}, _From, State) ->
    {reply, ok, State#state{default_interval = IntervalMs}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({schedule_default_updates}, State) ->
    % Schedule hourly endpoint discovery
    self() ! {schedule_update, endpoint_discovery, State#state.default_interval},
    
    % Schedule hourly model registry update
    self() ! {schedule_update, model_registry_update, State#state.default_interval},
    
    % Run initial discovery after 5 seconds
    erlang:send_after(5000, self(), {run_scheduled, initial_discovery}),
    
    {noreply, State};

handle_info({schedule_update, Name, IntervalMs}, State) ->
    % Handle internal schedule updates
    case handle_call({schedule_update, Name, IntervalMs}, {self(), make_ref()}, State) of
        {reply, _Reply, NewState} -> {noreply, NewState};
        Other -> Other
    end;

handle_info({run_scheduled, Name}, State) ->
    % Execute the scheduled task
    NewSchedules = case maps:get(Name, State#state.schedules, undefined) of
        undefined ->
            % One-time task
            run_update_task(Name),
            State#state.schedules;
        Schedule ->
            % Recurring task
            run_update_task(Name),
            
            % Reschedule
            NewRef = erlang:send_after(Schedule#schedule.interval, self(), {run_scheduled, Name}),
            UpdatedSchedule = Schedule#schedule{
                timer_ref = NewRef,
                last_run = erlang:timestamp(),
                run_count = Schedule#schedule.run_count + 1
            },
            maps:put(Name, UpdatedSchedule, State#state.schedules)
    end,
    
    NewState = State#state{
        schedules = NewSchedules,
        last_update = erlang:timestamp(),
        update_count = State#state.update_count + 1
    },
    
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Cancel all timers
    maps:foreach(fun(_Name, #schedule{timer_ref = Ref}) ->
        erlang:cancel_timer(Ref)
    end, State#state.schedules),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

run_update_task(endpoint_discovery) ->
    spawn(fun() ->
        error_logger:info_msg("Running scheduled endpoint discovery~n"),
        case endpoint_discovery:discover_all_endpoints() of
            {ok, Results, Total} ->
                error_logger:info_msg("Discovered ~p endpoints: ~p~n", [Total, Results]);
            {error, Reason} ->
                error_logger:error_msg("Endpoint discovery failed: ~p~n", [Reason])
        end
    end);

run_update_task(model_registry_update) ->
    spawn(fun() ->
        error_logger:info_msg("Checking for model registry updates~n"),
        % In a real implementation, this would check for new models
        % For now, just log
        Models = model_registry:get_all_models(),
        error_logger:info_msg("Model registry contains ~p models~n", [length(Models)])
    end);

run_update_task(initial_discovery) ->
    spawn(fun() ->
        error_logger:info_msg("Running initial discovery~n"),
        run_all_updates()
    end);

run_update_task(Name) ->
    error_logger:info_msg("Running scheduled task: ~p~n", [Name]).

run_all_updates() ->
    % Run endpoint discovery
    run_update_task(endpoint_discovery),
    
    % Run model registry update
    run_update_task(model_registry_update),
    
    % Log completion
    error_logger:info_msg("All updates completed~n").