.PHONY: compile shell clean release run clean-start clean-start-full

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

release:
	./rebar3 release

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
	@echo "make compile        - Compile the project"
	@echo "make shell          - Start an Erlang shell with the project loaded"
	@echo "make shell-streaming - Start shell with streaming tokens logging only"
	@echo "make clean-start    - Kill existing ERTS processes and start fresh"
	@echo "make clean-start-full - Clean start with Mnesia data cleanup"
	@echo "make clean          - Clean build artifacts"
	@echo "make release        - Build a release"
	@echo "make run            - Run the application"
	@echo "make test           - Run the tests"
	@echo "make dialyzer       - Run dialyzer"
	@echo "make docs           - Generate documentation"
	@echo "make all            - Compile and test"
	@echo "make help           - Show this help message"