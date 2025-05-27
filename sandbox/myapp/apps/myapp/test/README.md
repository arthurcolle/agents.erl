# MyApp Test Suite

This directory contains comprehensive tests for the MyApp agent system.

## Test Organization

### Unit Tests
- `agent_discovery_tests.erl` - Tests for agent discovery with capability-based lookup
- `agent_messenger_tests.erl` - Tests for inter-agent messaging
- `agent_tools_tests.erl` - Tests for tool registration and execution
- `agent_protocol_tests.erl` - Tests for message encoding/decoding
- `uuid_tests.erl` - Tests for UUID generation

### Integration Tests
- `integration_tests.erl` - End-to-end integration tests for the complete system

### Property-Based Tests
- `property_tests.erl` - Property-based tests using PropEr for invariant checking

### Performance Tests
- `performance_tests.erl` - Performance benchmarks and load tests

### Test Utilities
- `test_helpers.erl` - Common test utilities and helper functions
- `run_tests.escript` - Test runner script

## Running Tests

### Using Make

```bash
# Run all tests
make test

# Run specific test categories
make test-unit          # Unit tests only
make test-integration   # Integration tests
make test-property      # Property-based tests
make test-performance   # Performance tests

# Run specific module
make test-module MODULE=agent_tools_tests

# Run with coverage
make test-coverage

# Run in verbose mode
make test-verbose
```

### Using the Test Runner Script

```bash
# Run all tests
./run_tests.escript

# Run specific module
./run_tests.escript -m agent_discovery_tests

# Quick mode (skip slow tests)
./run_tests.escript -q

# Include performance tests
./run_tests.escript -p

# Verbose output
./run_tests.escript -v

# Show help
./run_tests.escript -h
```

### Using Rebar3

```bash
# From the project root
rebar3 eunit
rebar3 ct
rebar3 proper
```

## Test Coverage

Each test module includes:

1. **Basic functionality tests** - Core feature validation
2. **Edge case testing** - Boundary conditions and unusual inputs
3. **Error handling tests** - Failure scenarios and recovery
4. **Concurrent operation tests** - Thread safety and race conditions
5. **Performance benchmarks** - Speed and scalability metrics

## Writing New Tests

### Using Test Helpers

```erlang
-module(my_new_tests).
-include_lib("eunit/include/eunit.hrl").

my_test_() ->
    test_helpers:with_test_env(fun() ->
        % Your test code here
        {ok, AgentId, Pid} = test_helpers:create_test_agent(<<"test_agent">>),
        ?assert(is_pid(Pid))
    end).
```

### Test Structure

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% Module description
%%% @end
%%%-------------------------------------------------------------------
-module(module_name_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test generators
module_name_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Test description", fun test_function/0},
      ...
     ]}.

%% Setup and teardown
setup() ->
    test_helpers:setup_test_env().

teardown(_) ->
    test_helpers:cleanup_test_env().

%% Individual tests
test_function() ->
    % Test implementation
    ?assertEqual(expected, actual).
```

## Performance Testing

Performance tests measure:
- **Throughput** - Operations per second
- **Latency** - Response times
- **Scalability** - Performance under load
- **Memory usage** - Resource consumption

Results are output to console with detailed metrics.

## Property-Based Testing

Property tests verify system invariants:
- UUID uniqueness and format
- Message protocol correctness
- Discovery consistency
- Tool execution safety

## Continuous Integration

Tests are designed to run in CI environments:
- Exit codes indicate success/failure
- Output is parseable
- Tests are isolated and repeatable
- No external dependencies required

## Debugging Tests

### Verbose Mode
```bash
./run_tests.escript -v
```

### Individual Test Debugging
```erlang
% In Erlang shell
c(agent_discovery_tests).
agent_discovery_tests:test_basic_registration().
```

### Using Debugger
```erlang
debugger:start().
int:i(agent_discovery_tests).
```

## Test Metrics

Current test coverage includes:
- **Unit Tests**: 8 modules, ~50 test cases each
- **Integration Tests**: 9 comprehensive scenarios
- **Property Tests**: 15+ properties
- **Performance Tests**: 10 benchmarks

Total: 400+ individual test cases

## Contributing

When adding new features:
1. Write unit tests in the appropriate `*_tests.erl` file
2. Add integration tests if the feature involves multiple components
3. Consider adding property tests for invariants
4. Include performance tests for critical paths
5. Update this README if adding new test files