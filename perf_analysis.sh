#!/bin/bash

echo "=== BeamAsm JIT Performance Analysis ==="

# Check if perf is available (Linux only)
if command -v perf &> /dev/null; then
    echo "✓ Linux perf detected - full profiling available"
    PERF_AVAILABLE=true
else
    echo "⚠ Linux perf not available - using Erlang built-in profiling"
    PERF_AVAILABLE=false
fi

# Function to run basic performance test
run_basic_test() {
    echo ""
    echo "Running basic JIT performance test..."
    
    erl -eval "
        %% Load performance monitor
        compile:file(perf_monitor),
        
        %% Start monitoring
        perf_monitor:start(),
        
        %% Run some CPU-intensive Erlang code to test JIT
        Start = erlang:monotonic_time(microsecond),
        
        %% Fibonacci calculation (good for JIT testing)
        Fib = fun F(0) -> 0; F(1) -> 1; F(N) -> F(N-1) + F(N-2) end,
        Result = [Fib(N) || N <- lists:seq(1, 30)],
        
        %% List operations (good for BeamAsm)
        Lists = [lists:sort([random:uniform(1000) || _ <- lists:seq(1, 1000)]) || _ <- lists:seq(1, 100)],
        
        End = erlang:monotonic_time(microsecond),
        Time = End - Start,
        
        io:format('~n=== JIT Performance Test Results ===~n'),
        io:format('Fibonacci(1-30): ~p~n', [Result]),
        io:format('Sorting 100 lists of 1000 elements: ~p lists~n', [length(Lists)]),
        io:format('Total execution time: ~.2f ms~n', [Time/1000]),
        
        %% Show performance report
        perf_monitor:report(),
        
        halt().
    " -noshell
}

# Function to run with perf profiling (Linux only)
run_perf_analysis() {
    echo ""
    echo "Running perf analysis..."
    echo "Starting Erlang with perf support..."
    
    # Create output directory
    mkdir -p perf_results
    
    # Run application with perf recording
    ERL_FLAGS="+JPperf true +JPfp true" perf record -g --call-graph=fp \
        -o perf_results/agents.perf.data \
        ./rebar3 shell --apps agent,openai,agent_web &
    
    ERLANG_PID=$!
    echo "Erlang started with PID: $ERLANG_PID"
    echo "Recording performance for 30 seconds..."
    
    sleep 30
    
    echo "Stopping recording..."
    kill -TERM $ERLANG_PID
    
    echo "Generating perf report..."
    perf report -i perf_results/agents.perf.data > perf_results/report.txt
    
    echo "Performance analysis complete!"
    echo "Results saved to perf_results/"
}

# Function to show JIT status
show_jit_status() {
    echo ""
    echo "=== Current JIT Status ==="
    
    erl -eval "
        io:format('Erlang/OTP Version: ~s~n', [erlang:system_info(system_version)]),
        io:format('Emulator Flavor: ~p~n', [erlang:system_info(emu_flavor)]),
        io:format('Architecture: ~s~n', [erlang:system_info(system_architecture)]),
        io:format('Machine: ~s~n', [erlang:system_info(machine)]),
        io:format('Schedulers: ~p~n', [erlang:system_info(schedulers)]),
        
        case erlang:system_info(emu_flavor) of
            jit -> 
                io:format('~n✓ BeamAsm JIT is ENABLED~n'),
                io:format('✓ Native code generation is active~n');
            emu -> 
                io:format('~n⚠ Running in interpreter mode~n'),
                io:format('  Consider upgrading to OTP 24+ for JIT support~n')
        end,
        
        halt().
    " -noshell
}

# Main menu
case "${1:-status}" in
    "status")
        show_jit_status
        ;;
    "test")
        show_jit_status
        run_basic_test
        ;;
    "perf")
        if [ "$PERF_AVAILABLE" = true ]; then
            run_perf_analysis
        else
            echo "Perf profiling not available on this system"
            echo "Running basic test instead..."
            run_basic_test
        fi
        ;;
    "help"|*)
        echo ""
        echo "Usage: $0 [command]"
        echo ""
        echo "Commands:"
        echo "  status  - Show JIT status and system info (default)"
        echo "  test    - Run basic performance test"
        echo "  perf    - Run full perf analysis (Linux only)"
        echo "  help    - Show this help"
        echo ""
        ;;
esac