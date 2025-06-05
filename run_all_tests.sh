#!/bin/bash

# Master Test Runner for Agents.erl
# Runs all test files in a logical order with proper error handling and reporting

set -e  # Exit on any error

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Test result tracking
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0
START_TIME=$(date +%s)

# Arrays to store test results
declare -a PASSED_LIST
declare -a FAILED_LIST
declare -a SKIPPED_LIST

echo -e "${CYAN}🧪 Agents.erl Master Test Runner${NC}"
echo -e "${CYAN}=================================${NC}"
echo ""

# Function to print section headers
print_section() {
    echo ""
    echo -e "${MAGENTA}📋 $1${NC}"
    echo -e "${MAGENTA}$(echo "$1" | sed 's/./=/g')${NC}"
}

# Function to run a test and track results
run_test() {
    local test_file="$1"
    local test_description="$2"
    local test_type="$3"  # erlang, shell, or escript
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    echo -n "   Testing $test_description... "
    
    # Skip if file doesn't exist
    if [[ ! -f "$test_file" ]]; then
        echo -e "${YELLOW}SKIPPED${NC} (file not found)"
        SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
        SKIPPED_LIST+=("$test_description (file not found)")
        return
    fi
    
    # Create log file for this test
    local log_file="test_logs/$(basename "$test_file" .erl)_$(date +%H%M%S).log"
    mkdir -p test_logs
    
    # Run the test based on type
    case "$test_type" in
        "shell")
            if timeout 60 bash "$test_file" > "$log_file" 2>&1; then
                echo -e "${GREEN}PASSED${NC}"
                PASSED_TESTS=$((PASSED_TESTS + 1))
                PASSED_LIST+=("$test_description")
            else
                echo -e "${RED}FAILED${NC}"
                FAILED_TESTS=$((FAILED_TESTS + 1))
                FAILED_LIST+=("$test_description")
                echo "   Log: $log_file"
            fi
            ;;
        "escript")
            if timeout 60 escript "$test_file" > "$log_file" 2>&1; then
                echo -e "${GREEN}PASSED${NC}"
                PASSED_TESTS=$((PASSED_TESTS + 1))
                PASSED_LIST+=("$test_description")
            else
                echo -e "${RED}FAILED${NC}"
                FAILED_TESTS=$((FAILED_TESTS + 1))
                FAILED_LIST+=("$test_description")
                echo "   Log: $log_file"
            fi
            ;;
        "erlang")
            # Compile and run Erlang test
            local module_name=$(basename "$test_file" .erl)
            if timeout 60 ./rebar3 shell --eval "
                try 
                    case c:c('$test_file') of
                        {ok, Module} -> 
                            case erlang:function_exported(Module, test, 0) of
                                true -> Module:test();
                                false -> 
                                    case erlang:function_exported(Module, main, 1) of
                                        true -> Module:main([]);
                                        false -> io:format('No test/0 or main/1 function found~n')
                                    end
                            end;
                        error -> 
                            io:format('Compilation failed~n')
                    end
                catch E:R:S ->
                    io:format('Test error: ~p:~p~n~p~n', [E, R, S])
                end,
                halt().
            " > "$log_file" 2>&1; then
                echo -e "${GREEN}PASSED${NC}"
                PASSED_TESTS=$((PASSED_TESTS + 1))
                PASSED_LIST+=("$test_description")
            else
                echo -e "${RED}FAILED${NC}"
                FAILED_TESTS=$((FAILED_TESTS + 1))
                FAILED_LIST+=("$test_description")
                echo "   Log: $log_file"
            fi
            ;;
        *)
            echo -e "${YELLOW}SKIPPED${NC} (unknown type)"
            SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
            SKIPPED_LIST+=("$test_description (unknown type)")
            ;;
    esac
}

# Change to project directory
cd "$(dirname "$0")"

# Pre-flight checks
print_section "Pre-flight Checks"
echo -n "   Checking project structure... "
if [[ -f "rebar.config" && -d "_build" && -d "apps" ]]; then
    echo -e "${GREEN}OK${NC}"
else
    echo -e "${RED}FAILED${NC}"
    echo "   Missing required project files. Run 'make compile' first."
    exit 1
fi

echo -n "   Checking build status... "
if [[ -d "_build/default/lib" ]]; then
    echo -e "${GREEN}OK${NC}"
else
    echo -e "${YELLOW}WARNING${NC} - No build artifacts found"
    echo "   Running compilation..."
    if make compile; then
        echo -e "   ${GREEN}Compilation successful${NC}"
    else
        echo -e "   ${RED}Compilation failed${NC}"
        exit 1
    fi
fi

# Test Categories

print_section "1. Basic System Tests"
run_test "tests/test_basic_startup.erl" "Basic startup sequence" "escript"
run_test "tests/minimal_test.erl" "Minimal system test" "escript"
run_test "tests/simple_test.erl" "Simple system validation" "escript"
run_test "tests/test_app_startup.erl" "Application startup" "escript"
run_test "tests/test_system_startup.erl" "System initialization" "escript"

print_section "2. Agent Core Tests"
run_test "tests/test_agents_simple.erl" "Simple agent functionality" "escript"
run_test "tests/test_agent_startup.erl" "Agent initialization" "escript"
run_test "tests/test_agents_startup.erl" "Agents application startup" "escript"
run_test "tests/test_agent_fleet.erl" "Agent fleet management" "escript"
run_test "tests/test_diverse_agents.erl" "Diverse agent types" "escript"

print_section "3. Function Calling Tests"
run_test "tests/test_function_calling_simple.erl" "Simple function calling" "escript"
run_test "tests/test_function_calling.erl" "Function calling framework" "escript"
run_test "tests/test_function_calling_final.erl" "Final function calling tests" "escript"
run_test "tests/test_parallel_function_calling.erl" "Parallel function calling" "escript"
run_test "tests/test_parallel_function_calling_comprehensive.erl" "Comprehensive parallel calling" "escript"
run_test "tests/test_real_function_calling.erl" "Real-world function calling" "escript"

print_section "4. Tool Integration Tests"
run_test "tests/test_full_agent_tools.erl" "Full agent tools suite" "escript"
run_test "tests/test_agent_tools_fix.erl" "Agent tools fixes" "escript"
run_test "tests/test_parallel_tools.erl" "Parallel tool execution" "escript"
run_test "tests/test_jina_tools.erl" "Jina AI tool integration" "escript"
run_test "tests/test_jina_direct.erl" "Direct Jina integration" "escript"
run_test "tests/test_enhanced_jina_search.erl" "Enhanced Jina search" "escript"

print_section "5. Streaming & Communication Tests"
run_test "tests/test_streaming_function_calls.erl" "Streaming function calls" "escript"
run_test "tests/test_streaming_agents.erl" "Streaming agent communication" "escript"
run_test "tests/test_streaming_demo.erl" "Streaming demonstration" "escript"
run_test "tests/test_token_streaming.erl" "Token streaming" "escript"
run_test "tests/test_bytes_streaming_fix.erl" "Byte streaming fixes" "escript"
run_test "tests/test_advanced_streaming.erl" "Advanced streaming features" "escript"

print_section "6. MCP (Model Context Protocol) Tests"
run_test "tests/test_mcp_client.erl" "MCP client functionality" "escript"
run_test "tests/test_mcp_client_direct.erl" "Direct MCP client tests" "escript"
run_test "tests/test_mcp_connector.erl" "MCP connector tests" "escript"
run_test "tests/test_mcp_server.erl" "MCP server functionality" "escript"
run_test "tests/test_mcp_https.erl" "MCP HTTPS integration" "escript"

print_section "7. Cost Tracking & Monitoring Tests"
run_test "tests/test_cost_tracking.erl" "Cost tracking system" "escript"
run_test "tests/test_cost_tracking_final.erl" "Final cost tracking tests" "escript"
run_test "tests/test_complete_cost_tracking_system.erl" "Complete cost tracking" "escript"
run_test "tests/test_real_agent_costs.erl" "Real agent cost analysis" "escript"
run_test "tests/test_cost_api_direct.erl" "Direct cost API tests" "escript"

print_section "8. Advanced System Features"
run_test "tests/test_autonomous_simple.erl" "Simple autonomous behavior" "escript"
run_test "tests/test_autonomous_system.erl" "Autonomous system features" "escript"
run_test "tests/test_complete_autonomous_system.erl" "Complete autonomous system" "escript"
run_test "tests/test_self_aware_agents.erl" "Self-aware agent behavior" "escript"
run_test "tests/test_consciousness_demo.erl" "Consciousness demonstration" "escript"

print_section "9. Multi-turn & Complex Interactions"
run_test "tests/test_multi_turn_simple.erl" "Simple multi-turn conversations" "escript"
run_test "tests/test_multi_turn_fix.erl" "Multi-turn fixes" "escript"
run_test "tests/test_multi_turn_logging.erl" "Multi-turn logging" "escript"
run_test "tests/test_autonomous_multi_turn_function_calling.erl" "Autonomous multi-turn calling" "escript"

print_section "10. Knowledge Base & Intelligence Tests"
run_test "tests/test_knowledge_agents.erl" "Knowledge base agents" "escript"
run_test "tests/test_knowledge_base_fix.erl" "Knowledge base fixes" "escript"
run_test "tests/test_intelligent_agent_demo.erl" "Intelligent agent demo" "escript"
run_test "tests/test_deep_reflection.erl" "Deep reflection capabilities" "escript"

print_section "11. Self-Scaffolding & Hot Reloading"
run_test "tests/test_self_scaffold.erl" "Self-scaffolding system" "escript"
run_test "tests/test_self_scaffold_integration.erl" "Self-scaffold integration" "escript"
run_test "tests/test_hot_reload.erl" "Hot code reloading" "escript"
run_test "tests/test_hot_reload_fix.erl" "Hot reload fixes" "escript"

print_section "12. Weather & Domain-Specific Tests"
run_test "tests/test_weather_simple.erl" "Simple weather functionality" "escript"
run_test "tests/test_weather_agent.erl" "Weather agent system" "escript"
run_test "tests/test_weather_analysis.erl" "Weather analysis features" "escript"
run_test "tests/test_weather_fix.erl" "Weather system fixes" "escript"
run_test "tests/test_weather_recursive.erl" "Recursive weather processing" "escript"

print_section "13. Logging & Debugging Tests"
run_test "tests/test_colored_logging.erl" "Colored logging system" "escript"
run_test "tests/test_better_logging.erl" "Enhanced logging features" "escript"
run_test "tests/test_simple_logging.erl" "Simple logging functionality" "escript"
run_test "tests/test_improved_logging.erl" "Improved logging system" "escript"

print_section "14. System Integration & Fleet Management"
run_test "tests/test_enhanced_system.erl" "Enhanced system features" "escript"
run_test "tests/test_fleet_management_complete.erl" "Complete fleet management" "escript"
run_test "tests/test_discovery_systems.erl" "Service discovery systems" "escript"
run_test "tests/test_dynamic_supervisors.erl" "Dynamic supervisor management" "escript"

print_section "15. Advanced Features & Experimental"
run_test "tests/test_digital_twins_system.erl" "Digital twins system" "escript"
run_test "tests/test_digital_twins_advanced.erl" "Advanced digital twins" "escript"
run_test "tests/test_training_data_system.erl" "Training data system" "escript"
run_test "tests/test_workflow_system.erl" "Workflow orchestration" "escript"

print_section "16. Error Handling & Recovery"
run_test "tests/test_auto_error_fixing.erl" "Automatic error fixing" "escript"
run_test "tests/test_simple_error_fixing.erl" "Simple error fixing" "escript"
run_test "tests/test_error_debug.erl" "Error debugging features" "escript"

print_section "17. Shell Script Tests"
run_test "tests/test_web_startup.sh" "Web system startup" "shell"
run_test "tests/test_fix_simple.sh" "Simple fix validation" "shell"
run_test "tests/test_multi_turn_simple.sh" "Multi-turn shell test" "shell"

print_section "18. Reflection & Introspection Tests"
run_test "tests/quick_reflection_test.erl" "Quick reflection test" "escript"
run_test "tests/standalone_reflection_test.erl" "Standalone reflection" "escript"

print_section "19. Color & Display Tests"
run_test "tests/simple_color_test.erl" "Simple color display" "escript"
run_test "tests/final_color_test.erl" "Final color system test" "escript"
run_test "tests/test_colors_working.erl" "Color system validation" "escript"

print_section "20. Miscellaneous & Edge Cases"
run_test "tests/runtime_test.erl" "Runtime system test" "escript"
run_test "tests/test_startup.erl" "General startup test" "escript"
run_test "tests/test_system.erl" "System validation test" "escript"
run_test "tests/test_demo.erl" "Demo functionality test" "escript"
run_test "tests/test_chat.erl" "Chat system test" "escript"

# Calculate test duration
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# Final Summary
echo ""
echo -e "${CYAN}📊 Test Summary${NC}"
echo -e "${CYAN}==============${NC}"
echo ""
echo -e "Total Tests:    ${BLUE}$TOTAL_TESTS${NC}"
echo -e "Passed:         ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed:         ${RED}$FAILED_TESTS${NC}"
echo -e "Skipped:        ${YELLOW}$SKIPPED_TESTS${NC}"
echo -e "Duration:       ${BLUE}${DURATION}s${NC}"
echo ""

# Success rate calculation
if [[ $TOTAL_TESTS -gt 0 ]]; then
    SUCCESS_RATE=$(( (PASSED_TESTS * 100) / TOTAL_TESTS ))
    echo -e "Success Rate:   ${BLUE}${SUCCESS_RATE}%${NC}"
fi

# Detailed results if there are failures
if [[ $FAILED_TESTS -gt 0 ]]; then
    echo ""
    echo -e "${RED}❌ Failed Tests:${NC}"
    for test in "${FAILED_LIST[@]}"; do
        echo -e "   ${RED}•${NC} $test"
    done
fi

if [[ $SKIPPED_TESTS -gt 0 ]]; then
    echo ""
    echo -e "${YELLOW}⏭️  Skipped Tests:${NC}"
    for test in "${SKIPPED_LIST[@]}"; do
        echo -e "   ${YELLOW}•${NC} $test"
    done
fi

if [[ $PASSED_TESTS -gt 0 ]]; then
    echo ""
    echo -e "${GREEN}✅ Recent Passed Tests:${NC}"
    # Show last 5 passed tests to avoid too much output
    for i in "${!PASSED_LIST[@]}"; do
        if [[ $i -ge $((${#PASSED_LIST[@]} - 5)) ]]; then
            echo -e "   ${GREEN}•${NC} ${PASSED_LIST[$i]}"
        fi
    done
    if [[ ${#PASSED_LIST[@]} -gt 5 ]]; then
        echo -e "   ${GREEN}•${NC} ... and $((${#PASSED_LIST[@]} - 5)) more"
    fi
fi

echo ""
echo -e "${CYAN}📁 Test logs saved in: test_logs/${NC}"
echo ""

# Final status
if [[ $FAILED_TESTS -eq 0 ]]; then
    echo -e "${GREEN}🎉 ALL TESTS COMPLETED SUCCESSFULLY!${NC}"
    exit 0
else
    echo -e "${RED}❌ SOME TESTS FAILED - CHECK LOGS FOR DETAILS${NC}"
    exit 1
fi