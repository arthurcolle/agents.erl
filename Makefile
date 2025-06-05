.PHONY: compile shell clean release run clean-start clean-start-full frontend frontend-install frontend-build frontend-clean build-all release-prod

compile:
	./rebar3 compile

shell:
	./rebar3 shell

shell-streaming:
	./rebar3 shell --config=config/streaming_tokens.config

# Clean start - kills existing ERTS processes first
clean-start:
	@./scripts/clean_start.sh

# Clean start with Mnesia cleanup
clean-start-full:
	@./scripts/clean_start.sh --clean-mnesia

clean:
	./rebar3 clean
	$(MAKE) frontend-clean

# Frontend targets
frontend-install:
	cd apps/agent_web/frontend && npm install

frontend-build: frontend-install
	cd apps/agent_web/frontend && npm run build

frontend-clean:
	rm -rf apps/agent_web/frontend/node_modules
	rm -rf apps/agent_web/frontend/dist
	rm -rf apps/agent_web/priv/static/dist

frontend-dev:
	cd apps/agent_web/frontend && npm run dev

# Combined build targets
build-all: frontend-build compile

release: build-all
	./rebar3 release

release-prod: build-all
	./rebar3 as prod release

release-tar: build-all
	./rebar3 as prod tar

run:
	./start.sh

test:
	./rebar3 eunit

dialyzer:
	./rebar3 dialyzer

docs:
	./rebar3 edoc

all: compile test

help:
	@echo "Erlang Agent Makefile"
	@echo "---------------------"
	@echo "Backend targets:"
	@echo "  make compile        - Compile the Erlang project"
	@echo "  make shell          - Start an Erlang shell with the project loaded"
	@echo "  make shell-streaming - Start shell with streaming tokens logging only"
	@echo "  make clean-start    - Kill existing ERTS processes and start fresh"
	@echo "  make clean-start-full - Clean start with Mnesia data cleanup"
	@echo "  make clean          - Clean all build artifacts (backend + frontend)"
	@echo "  make test           - Run the tests"
	@echo "  make dialyzer       - Run dialyzer"
	@echo "  make docs           - Generate documentation"
	@echo ""
	@echo "Frontend targets:"
	@echo "  make frontend-install - Install npm dependencies"
	@echo "  make frontend-build - Build the frontend"
	@echo "  make frontend-clean - Clean frontend build artifacts"
	@echo "  make frontend-dev   - Start frontend dev server"
	@echo ""
	@echo "Combined targets:"
	@echo "  make build-all      - Build frontend and compile backend"
	@echo "  make release        - Build complete release (frontend + backend)"
	@echo "  make release-prod   - Build production release"
	@echo "  make release-tar    - Build release tarball"
	@echo ""
	@echo "Other:"
	@echo "  make run            - Run the application"
	@echo "  make all            - Compile and test"
	@echo "  make help           - Show this help message"