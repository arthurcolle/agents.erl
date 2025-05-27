.PHONY: all compile clean test eunit ct dialyzer xref cover docs rel run shell

REBAR = ./rebar3

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -rf _build
	@rm -rf logs
	@rm -f rebar3.crashdump
	@rm -f eunit.xml

deps:
	@$(REBAR) deps

# Testing targets
test: eunit ct

eunit:
	@echo "Running EUnit tests..."
	@$(REBAR) as test eunit

ct:
	@echo "Running Common Test suites..."
	@$(REBAR) as test ct

# Run specific test module
test-module:
	@echo "Running tests for module $(MODULE)..."
	@$(REBAR) as test eunit --module=$(MODULE)

# Run integration tests only
integration:
	@$(REBAR) as test eunit --module=integration_tests

# Run performance tests only
performance:
	@$(REBAR) as test eunit --module=performance_tests

# Run property-based tests
property:
	@$(REBAR) as test eunit --module=property_tests

# Code quality targets
dialyzer:
	@echo "Running Dialyzer..."
	@$(REBAR) dialyzer

xref:
	@echo "Running Xref..."
	@$(REBAR) xref

# Coverage analysis
cover:
	@echo "Running tests with coverage..."
	@$(REBAR) as test cover
	@$(REBAR) as test cover --verbose

# Documentation
docs:
	@$(REBAR) edoc

# Release targets
rel:
	@$(REBAR) release

rel-prod:
	@$(REBAR) as prod release

# Running targets
run: rel
	@_build/default/rel/myapp/bin/myapp console

shell:
	@$(REBAR) shell

# Development helpers
dev-setup:
	@echo "Setting up development environment..."
	@$(REBAR) deps
	@$(REBAR) compile
	@echo "Creating config directory..."
	@mkdir -p config
	@echo "Development setup complete!"

# Quick test - runs fast unit tests only
quick-test:
	@$(REBAR) as test eunit --module=uuid_tests
	@$(REBAR) as test eunit --module=agent_protocol_tests
	@$(REBAR) as test eunit --module=agent_tools_tests

# Full test suite with coverage
full-test: clean compile dialyzer xref test cover

# CI/CD target
ci: compile dialyzer xref test

# Watch for changes and run tests
watch:
	@echo "Watching for file changes..."
	@while true; do \
		inotifywait -e modify,create,delete -r src/ test/ 2>/dev/null; \
		clear; \
		echo "Files changed, running tests..."; \
		make quick-test; \
	done

# Help target
help:
	@echo "Available targets:"
	@echo "  make compile    - Compile the project"
	@echo "  make test       - Run all tests"
	@echo "  make eunit      - Run EUnit tests"
	@echo "  make ct         - Run Common Test suites"
	@echo "  make cover      - Run tests with coverage analysis"
	@echo "  make dialyzer   - Run Dialyzer for type checking"
	@echo "  make xref       - Run Xref for cross-reference checks"
	@echo "  make docs       - Generate documentation"
	@echo "  make rel        - Build a release"
	@echo "  make run        - Run the application"
	@echo "  make shell      - Start an Erlang shell with the app loaded"
	@echo "  make clean      - Clean build artifacts"
	@echo "  make full-test  - Run complete test suite with analysis"
	@echo ""
	@echo "Test-specific targets:"
	@echo "  make test-module MODULE=<name> - Run tests for specific module"
	@echo "  make integration              - Run integration tests only"
	@echo "  make performance              - Run performance tests only"
	@echo "  make property                 - Run property-based tests only"
	@echo "  make quick-test              - Run quick unit tests"
	@echo ""
	@echo "Development targets:"
	@echo "  make dev-setup  - Set up development environment"
	@echo "  make watch      - Watch files and run tests on change"