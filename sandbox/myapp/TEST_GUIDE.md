# MyApp Test Suite Guide

## Overview

The MyApp test suite provides comprehensive testing coverage for all components of the distributed agent system. The tests are organized into unit tests, integration tests, property-based tests, and performance tests.

## Test Structure

```
apps/myapp/test/
├── test_helpers.erl         # Common test utilities
├── agent_tests.erl          # Core agent functionality tests
├── agent_registry_tests.erl # Agent registry tests
├── agent_discovery_tests.erl # Service discovery tests
├── agent_messenger_tests.erl # Inter-agent messaging tests
├── agent_tools_tests.erl    # Tool system tests
├── agent_protocol_tests.erl # Protocol encoding/decoding tests
├── uuid_tests.erl           # UUID generation tests
├── integration_tests.erl    # End-to-end integration tests
├── property_tests.erl       # Property-based tests
└── performance_tests.erl    # Performance benchmarks
```

## Running Tests

### Quick Start

```bash
# Run all tests
make test

# Run specific test module
make test-module MODULE=agent_tests

# Run with coverage
make cover

# Run full test suite with analysis
make full-test
```

### Using Rebar3 Directly

```bash
# Run EUnit tests
rebar3 as test eunit

# Run specific module
rebar3 as test eunit --module=agent_registry_tests

# Run with coverage
rebar3 as test cover

# Run Common Test suites
rebar3 as test ct
```

### Test Categories

1. **Unit Tests** - Test individual functions and modules
   ```bash
   make eunit
   ```

2. **Integration Tests** - Test component interactions
   ```bash
   make integration
   ```

3. **Property Tests** - Test invariants with random data
   ```bash
   make property
   ```

4. **Performance Tests** - Benchmark and load tests
   ```bash
   make performance
   ```

## Test Helpers

The `test_helpers` module provides utilities for:

- **Environment Setup**: `setup_test_env/0`, `cleanup_test_env/0`
- **Mock Management**: `with_mock_openai/1`, `mock_tool_response/2`
- **Agent Testing**: `with_test_agent/2`, `create_test_tool/1`
- **Assertions**: `assert_message_received/1`, `eventually/1`
- **Utilities**: `generate_test_id/0`, `with_timeout/2`

### Example Usage

```erlang
-module(my_test).
-include_lib("eunit/include/eunit.hrl").
-import(test_helpers, [setup_test_env/0, create_test_tool/1]).

my_test() ->
    setup_test_env(),
    
    % Create a test tool
    ToolName = create_test_tool(my_tool, #{
        response => fun(Args) -> {ok, <<"response">>} end
    }),
    
    % Test with mock OpenAI
    test_helpers:with_mock_openai(fun() ->
        % Your test code here
    end).
```

## Writing Tests

### Unit Test Template

```erlang
-module(module_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test fixture
module_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Test description", fun test_function/0},
      {"Another test", fun another_test/0}
     ]
    }.

setup() ->
    test_helpers:setup_test_env().

cleanup(_) ->
    test_helpers:cleanup_test_env().

test_function() ->
    ?assertEqual(expected, actual).
```

### Property Test Template

```erlang
-module(module_prop_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_test_() ->
    {timeout, 60, fun() ->
        ?assert(proper:quickcheck(prop_invariant()))
    end}.

prop_invariant() ->
    ?FORALL(Input, generator(),
        begin
            Result = function_under_test(Input),
            check_invariant(Result)
        end).
```

## Test Coverage

### Generating Coverage Reports

```bash
# Run tests with coverage
make cover

# View coverage summary
cat _build/test/cover/index.html
```

### Coverage Goals

- Unit test coverage: >80%
- Integration test coverage: >70%
- Overall coverage: >75%

## Continuous Integration

The test suite is designed to run in CI/CD pipelines:

```bash
# CI target runs compile, dialyzer, xref, and tests
make ci
```

### GitHub Actions Example

```yaml
name: Test Suite
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '24.0'
          rebar3-version: '3.18.0'
      - run: make ci
```

## Debugging Tests

### Verbose Output

```bash
# Run with verbose output
rebar3 as test eunit -v

# Run specific test with debugging
ERL_FLAGS="-s debugger start" rebar3 as test eunit --module=agent_tests
```

### Common Issues

1. **Test Timeout**: Increase timeout in test definition
   ```erlang
   {timeout, 60, fun test_function/0}
   ```

2. **Process Cleanup**: Ensure processes are stopped
   ```erlang
   cleanup(_) ->
       test_helpers:cleanup_test_env().
   ```

3. **Mock Conflicts**: Unload mocks after use
   ```erlang
   meck:unload(module_name)
   ```

## Performance Testing

### Running Benchmarks

```bash
# Run performance tests
make performance

# Run with profiling
rebar3 as test eunit --module=performance_tests -- -prof
```

### Interpreting Results

Performance tests output:
- Throughput (operations/second)
- Latency percentiles (p50, p95, p99)
- Memory usage statistics

## Best Practices

1. **Isolation**: Each test should be independent
2. **Cleanup**: Always clean up resources
3. **Mocking**: Mock external dependencies
4. **Timeouts**: Set appropriate timeouts
5. **Naming**: Use descriptive test names
6. **Assertions**: Use specific assertions
7. **Coverage**: Aim for high coverage
8. **Documentation**: Document complex tests

## Extending the Test Suite

### Adding New Test Modules

1. Create test file in `test/` directory
2. Follow naming convention: `module_tests.erl`
3. Include standard test structure
4. Add to Makefile if needed

### Adding Test Utilities

Add utilities to `test_helpers.erl`:

```erlang
%% Add new utility function
my_test_utility(Args) ->
    % Implementation
    ok.

%% Export it
-export([my_test_utility/1]).
```

## Troubleshooting

### Test Failures

1. Check test output for error details
2. Run failing test in isolation
3. Enable verbose logging
4. Check for race conditions
5. Verify test environment setup

### Environment Issues

```bash
# Clean environment
make clean
rm -rf _build

# Rebuild
make compile
make test
```

## Additional Resources

- [EUnit Documentation](http://erlang.org/doc/apps/eunit/chapter.html)
- [Common Test Documentation](http://erlang.org/doc/apps/common_test/introduction.html)
- [PropEr Documentation](https://proper-testing.github.io/)
- [Meck Documentation](https://github.com/eproxus/meck)