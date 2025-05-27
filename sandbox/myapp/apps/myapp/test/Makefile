# Test Makefile

.PHONY: all compile test test-unit test-integration test-property test-performance clean help

REBAR3 = ../../../rebar3
TEST_DIR = .
EBIN_DIR = ../ebin

# Default target
all: compile test

# Compile test modules
compile:
	@echo "Compiling test modules..."
	@erlc -o $(EBIN_DIR) -I ../include -I ../src +debug_info *.erl

# Run all tests
test: test-unit test-integration

# Run unit tests only
test-unit:
	@echo "Running unit tests..."
	@./run_tests.escript -q

# Run integration tests
test-integration:
	@echo "Running integration tests..."
	@./run_tests.escript -m integration_tests

# Run property-based tests
test-property:
	@echo "Running property-based tests..."
	@./run_tests.escript -m property_tests

# Run performance tests
test-performance:
	@echo "Running performance tests..."
	@./run_tests.escript -m performance_tests

# Run specific test module
test-module:
	@echo "Running tests for module $(MODULE)..."
	@./run_tests.escript -m $(MODULE)

# Run tests with coverage
test-coverage:
	@echo "Running tests with coverage..."
	@$(REBAR3) cover -v

# Run tests in verbose mode
test-verbose:
	@echo "Running tests in verbose mode..."
	@./run_tests.escript -v

# Clean compiled test files
clean:
	@echo "Cleaning test artifacts..."
	@rm -f $(EBIN_DIR)/*_tests.beam
	@rm -f $(EBIN_DIR)/test_helpers.beam
	@rm -f erl_crash.dump

# Show help
help:
	@echo "Test Makefile targets:"
	@echo "  make test              - Run all tests (unit + integration)"
	@echo "  make test-unit         - Run unit tests only"
	@echo "  make test-integration  - Run integration tests"
	@echo "  make test-property     - Run property-based tests"
	@echo "  make test-performance  - Run performance tests"
	@echo "  make test-module MODULE=name - Run specific test module"
	@echo "  make test-coverage     - Run tests with coverage analysis"
	@echo "  make test-verbose      - Run tests in verbose mode"
	@echo "  make clean            - Clean test artifacts"
	@echo "  make help             - Show this help message"