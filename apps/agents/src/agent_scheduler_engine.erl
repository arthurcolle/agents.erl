-module(agent_scheduler_engine).
-behaviour(gen_server).

%% API
-export([start_link/0,
         schedule_task/4,
         cancel_task/1,
         get_agent_schedule/1,
         get_upcoming_tasks/1,
         reschedule_task/3,
         execute_now/1,
         get_task_status/1,
         list_all_tasks/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TASK_CHECK_INTERVAL, 1000). % Check every second

-record(scheduled_task, {
    id :: binary(),
    agent_id :: binary(),
    task :: map(),
    scheduled_time :: erlang:timestamp(),
    status :: pending | executing | completed | failed | cancelled,
    created_at :: erlang:timestamp(),
    result :: any(),
    retries :: non_neg_integer(),
    max_retries :: non_neg_integer()
}).

-record(state, {
    tasks :: #{binary() => #scheduled_task{}},
    timer_ref :: reference()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Schedule a task for an agent to execute in the future
schedule_task(AgentId, Task, ScheduledTime, Options) ->
    gen_server:call(?SERVER, {schedule_task, AgentId, Task, ScheduledTime, Options}).

%% Cancel a scheduled task
cancel_task(TaskId) ->
    gen_server:call(?SERVER, {cancel_task, TaskId}).

%% Get all scheduled tasks for an agent
get_agent_schedule(AgentId) ->
    gen_server:call(?SERVER, {get_agent_schedule, AgentId}).

%% Get upcoming tasks within a time window
get_upcoming_tasks(TimeWindow) ->
    gen_server:call(?SERVER, {get_upcoming_tasks, TimeWindow}).

%% Reschedule an existing task
reschedule_task(TaskId, NewTime, Options) ->
    gen_server:call(?SERVER, {reschedule_task, TaskId, NewTime, Options}).

%% Execute a scheduled task immediately
execute_now(TaskId) ->
    gen_server:call(?SERVER, {execute_now, TaskId}).

%% Get the status of a task
get_task_status(TaskId) ->
    gen_server:call(?SERVER, {get_task_status, TaskId}).

%% List all tasks in the system
list_all_tasks() ->
    gen_server:call(?SERVER, list_all_tasks).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    TimerRef = erlang:send_after(?TASK_CHECK_INTERVAL, self(), check_scheduled_tasks),
    {ok, #state{tasks = #{}, timer_ref = TimerRef}}.

handle_call({schedule_task, AgentId, Task, ScheduledTime, Options}, _From, State) ->
    TaskId = generate_task_id(),
    MaxRetries = maps:get(max_retries, Options, 3),
    
    ScheduledTask = #scheduled_task{
        id = TaskId,
        agent_id = AgentId,
        task = Task,
        scheduled_time = normalize_time(ScheduledTime),
        status = pending,
        created_at = erlang:timestamp(),
        retries = 0,
        max_retries = MaxRetries
    },
    
    NewTasks = maps:put(TaskId, ScheduledTask, State#state.tasks),
    
    %% Persist to timeline events
    persist_timeline_event(#{
        type => task_scheduled,
        task_id => TaskId,
        agent_id => AgentId,
        scheduled_time => ScheduledTime,
        task => Task
    }),
    
    {reply, {ok, TaskId}, State#state{tasks = NewTasks}};

handle_call({cancel_task, TaskId}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} ->
            UpdatedTask = Task#scheduled_task{status = cancelled},
            NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
            
            persist_timeline_event(#{
                type => task_cancelled,
                task_id => TaskId,
                agent_id => Task#scheduled_task.agent_id
            }),
            
            {reply, ok, State#state{tasks = NewTasks}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_agent_schedule, AgentId}, _From, State) ->
    AgentTasks = maps:filter(
        fun(_Id, Task) -> 
            Task#scheduled_task.agent_id =:= AgentId andalso
            Task#scheduled_task.status =:= pending
        end,
        State#state.tasks
    ),
    {reply, {ok, maps:values(AgentTasks)}, State};

handle_call({get_upcoming_tasks, TimeWindow}, _From, State) ->
    Now = erlang:timestamp(),
    FutureTime = add_seconds_to_timestamp(Now, TimeWindow),
    
    UpcomingTasks = maps:filter(
        fun(_Id, Task) ->
            Task#scheduled_task.status =:= pending andalso
            timestamp_less_than(Task#scheduled_task.scheduled_time, FutureTime)
        end,
        State#state.tasks
    ),
    
    SortedTasks = lists:sort(
        fun(A, B) ->
            timestamp_less_than(A#scheduled_task.scheduled_time, 
                              B#scheduled_task.scheduled_time)
        end,
        maps:values(UpcomingTasks)
    ),
    
    {reply, {ok, SortedTasks}, State};

handle_call({reschedule_task, TaskId, NewTime, _Options}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} when Task#scheduled_task.status =:= pending ->
            UpdatedTask = Task#scheduled_task{
                scheduled_time = normalize_time(NewTime)
            },
            NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
            
            persist_timeline_event(#{
                type => task_rescheduled,
                task_id => TaskId,
                agent_id => Task#scheduled_task.agent_id,
                new_time => NewTime
            }),
            
            {reply, ok, State#state{tasks = NewTasks}};
        {ok, _Task} ->
            {reply, {error, task_not_pending}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({execute_now, TaskId}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} when Task#scheduled_task.status =:= pending ->
            spawn(fun() -> execute_task(Task) end),
            UpdatedTask = Task#scheduled_task{status = executing},
            NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
            {reply, ok, State#state{tasks = NewTasks}};
        {ok, _Task} ->
            {reply, {error, task_not_pending}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_task_status, TaskId}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} ->
            {reply, {ok, Task#scheduled_task.status}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_all_tasks, _From, State) ->
    Tasks = maps:values(State#state.tasks),
    {reply, {ok, Tasks}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({task_completed, TaskId, Result}, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} ->
            UpdatedTask = Task#scheduled_task{
                status = completed,
                result = Result
            },
            NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
            
            persist_timeline_event(#{
                type => task_completed,
                task_id => TaskId,
                agent_id => Task#scheduled_task.agent_id,
                result => Result
            }),
            
            {noreply, State#state{tasks = NewTasks}};
        error ->
            {noreply, State}
    end;

handle_cast({task_failed, TaskId, Reason}, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} ->
            if
                Task#scheduled_task.retries < Task#scheduled_task.max_retries ->
                    %% Retry the task
                    UpdatedTask = Task#scheduled_task{
                        retries = Task#scheduled_task.retries + 1,
                        status = pending,
                        scheduled_time = add_seconds_to_timestamp(erlang:timestamp(), 60)
                    },
                    NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
                    {noreply, State#state{tasks = NewTasks}};
                true ->
                    %% Max retries reached
                    UpdatedTask = Task#scheduled_task{
                        status = failed,
                        result = Reason
                    },
                    NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
                    
                    persist_timeline_event(#{
                        type => task_failed,
                        task_id => TaskId,
                        agent_id => Task#scheduled_task.agent_id,
                        reason => Reason
                    }),
                    
                    {noreply, State#state{tasks = NewTasks}}
            end;
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_scheduled_tasks, State) ->
    %% Check for tasks that need to be executed
    Now = erlang:timestamp(),
    
    TasksToExecute = maps:filter(
        fun(_Id, Task) ->
            Task#scheduled_task.status =:= pending andalso
            timestamp_less_than(Task#scheduled_task.scheduled_time, Now)
        end,
        State#state.tasks
    ),
    
    %% Execute each task
    lists:foreach(
        fun({TaskId, Task}) ->
            spawn(fun() -> execute_task(Task) end),
            gen_server:cast(?SERVER, {task_executing, TaskId})
        end,
        maps:to_list(TasksToExecute)
    ),
    
    %% Update task statuses
    UpdatedTasks = lists:foldl(
        fun({TaskId, _Task}, Acc) ->
            case maps:find(TaskId, Acc) of
                {ok, T} ->
                    maps:put(TaskId, T#scheduled_task{status = executing}, Acc);
                error ->
                    Acc
            end
        end,
        State#state.tasks,
        maps:to_list(TasksToExecute)
    ),
    
    %% Schedule next check
    erlang:cancel_timer(State#state.timer_ref),
    NewTimerRef = erlang:send_after(?TASK_CHECK_INTERVAL, self(), check_scheduled_tasks),
    
    {noreply, State#state{tasks = UpdatedTasks, timer_ref = NewTimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    erlang:cancel_timer(State#state.timer_ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

execute_task(#scheduled_task{id = TaskId, agent_id = AgentId, task = Task}) ->
    try
        %% Execute the task through the agent
        Result = case agent_instance:execute_task(AgentId, Task) of
            {ok, R} -> R;
            {error, Reason} -> throw({task_error, Reason})
        end,
        
        gen_server:cast(?SERVER, {task_completed, TaskId, Result})
    catch
        Type:Error ->
            gen_server:cast(?SERVER, {task_failed, TaskId, {Type, Error}})
    end.

generate_task_id() ->
    agent_uuid:generate().

normalize_time({Mega, Sec, Micro}) ->
    {Mega, Sec, Micro};
normalize_time(Seconds) when is_integer(Seconds) ->
    %% Convert seconds from now to timestamp
    add_seconds_to_timestamp(erlang:timestamp(), Seconds);
normalize_time(DateTime) when is_binary(DateTime) ->
    %% Parse ISO 8601 datetime
    parse_datetime(DateTime).

add_seconds_to_timestamp({Mega, Sec, Micro}, Seconds) ->
    TotalMicro = (Mega * 1000000 + Sec) * 1000000 + Micro + (Seconds * 1000000),
    NewMega = TotalMicro div 1000000000000,
    NewSec = (TotalMicro rem 1000000000000) div 1000000,
    NewMicro = TotalMicro rem 1000000,
    {NewMega, NewSec, NewMicro}.

timestamp_less_than({M1, S1, U1}, {M2, S2, U2}) ->
    (M1 < M2) orelse 
    (M1 =:= M2 andalso S1 < S2) orelse
    (M1 =:= M2 andalso S1 =:= S2 andalso U1 < U2).

parse_datetime(Binary) ->
    %% Simple ISO 8601 parser (can be enhanced)
    erlang:timestamp().

persist_timeline_event(Event) ->
    timeline_event_store:store(Event).