# Fix for System Metrics Calculation

## Problem
The frontend expects `cpuUsage` and `memoryUsage` as percentages, but the backend is not providing these values.

## Solution

Replace the `get_system_metrics()` function in `agent_ws_handler.erl` (starting at line 241) with:

```erlang
get_system_metrics() ->
    % Get memory info
    TotalMemory = erlang:memory(total),
    UsedMemory = erlang:memory(processes) + erlang:memory(system),
    MemoryUsagePercent = round((UsedMemory / TotalMemory) * 100),
    
    % Get CPU usage (simplified - based on scheduler utilization)
    SchedulerUsage = scheduler_usage(),
    CpuUsagePercent = round(SchedulerUsage * 100),
    
    #{
        node => node(),
        uptime => erlang:statistics(wall_clock),
        total_memory => TotalMemory,
        used_memory => UsedMemory,
        process_count => erlang:system_info(process_count),
        run_queue => erlang:statistics(run_queue),
        schedulers => erlang:system_info(schedulers_online),
        % Add the percentage fields expected by frontend
        cpuUsage => CpuUsagePercent,
        memoryUsage => MemoryUsagePercent
    }.

%% Helper function to calculate scheduler usage
scheduler_usage() ->
    case erlang:statistics(scheduler_wall_time) of
        undefined ->
            % Enable scheduler wall time if not enabled
            erlang:system_flag(scheduler_wall_time, true),
            timer:sleep(100),
            scheduler_usage();
        OldTimes ->
            timer:sleep(100),
            NewTimes = erlang:statistics(scheduler_wall_time),
            calculate_scheduler_usage(OldTimes, NewTimes)
    end.

calculate_scheduler_usage(OldTimes, NewTimes) ->
    TotalUsage = lists:foldl(fun({{I, A0, T0}, {I, A1, T1}}, Acc) ->
        Active = A1 - A0,
        Total = T1 - T0,
        case Total of
            0 -> Acc;
            _ -> Acc + (Active / Total)
        end
    end, 0.0, lists:zip(OldTimes, NewTimes)),
    TotalUsage / length(NewTimes).
```

## Additional Fixes Needed

### 1. Update App.tsx to properly handle system metrics:

In the `handleWebSocketMessage` function, ensure the metrics are properly extracted:

```javascript
case 'system_metrics':
  setSystemMetrics({
    cpuUsage: data.data.cpuUsage || 0,
    memoryUsage: data.data.memoryUsage || 0,
    processCount: data.data.process_count || 0
  })
  break
```

### 2. Add periodic system metrics updates:

In `agent_ws_handler.erl`, add a function to send periodic metrics:

```erlang
%% Add this to websocket_init/1:
websocket_init(State) ->
    self() ! send_system_metrics,
    {ok, State}.

%% Add this to websocket_info/2:
websocket_info(send_system_metrics, State) ->
    Metrics = get_system_metrics(),
    Response = jsx:encode(#{
        type => <<"system_metrics">>,
        data => Metrics
    }),
    erlang:send_after(5000, self(), send_system_metrics), % Update every 5 seconds
    {reply, {text, Response}, State};
```

### 3. Fix the Dashboard hardcoded data:

Replace the hardcoded `performanceData` with a state variable that accumulates real metrics:

```javascript
const [performanceHistory, setPerformanceHistory] = useState<Array<{time: string, cpu: number, memory: number}>>([])

useEffect(() => {
  if (systemMetrics.cpuUsage > 0) {
    setPerformanceHistory(prev => {
      const newData = [...prev, {
        time: new Date().toLocaleTimeString('en-US', { hour12: false, hour: '2-digit', minute: '2-digit' }),
        cpu: systemMetrics.cpuUsage,
        memory: systemMetrics.memoryUsage
      }]
      // Keep only last 20 data points
      return newData.slice(-20)
    })
  }
}, [systemMetrics])

// Use performanceHistory instead of performanceData in the LineChart
```

### 4. Fix non-functional buttons:

Add proper onClick handlers for all buttons. For example:

```javascript
// In App.tsx for the Settings button:
<Button variant="outline" size="icon" onClick={() => {
  // TODO: Open settings dialog
  console.log('Settings clicked')
}}>
  <Settings className="h-4 w-4" />
</Button>

// For the Stop button:
<Button size="sm" variant="destructive" onClick={async () => {
  try {
    await fetch(`/api/agents/${agent.id}`, { method: 'DELETE' })
    await loadAgents()
  } catch (error) {
    console.error('Failed to stop agent:', error)
  }
}}>
  Stop
</Button>
```