#!/bin/bash

echo "=================================="
echo "COMPREHENSIVE COST TRACKING TEST"
echo "=================================="
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Base URL
BASE_URL="http://localhost:8080"

# Function to test endpoint
test_endpoint() {
    local method=$1
    local endpoint=$2
    local description=$3
    local data=$4
    
    echo -e "${BLUE}Testing: ${description}${NC}"
    echo "Endpoint: $method $endpoint"
    
    if [ "$method" = "GET" ]; then
        response=$(curl -s -X GET "${BASE_URL}${endpoint}")
    elif [ "$method" = "POST" ]; then
        response=$(curl -s -X POST "${BASE_URL}${endpoint}" \
            -H "Content-Type: application/json" \
            -d "$data")
    elif [ "$method" = "DELETE" ]; then
        response=$(curl -s -X DELETE "${BASE_URL}${endpoint}")
    fi
    
    if [ $? -eq 0 ] && [ ! -z "$response" ]; then
        echo -e "${GREEN}✓ Success${NC}"
        echo "Response:"
        echo "$response" | jq . 2>/dev/null || echo "$response"
    else
        echo -e "${RED}✗ Failed${NC}"
    fi
    echo "---"
    echo ""
}

# Wait for server to be ready
echo "Waiting for server to start..."
sleep 3

# Test 1: Check if server is running
echo -e "${YELLOW}=== TEST 1: Server Health Check ===${NC}"
curl -s "${BASE_URL}" > /dev/null
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Server is running${NC}"
else
    echo -e "${RED}✗ Server is not running${NC}"
    exit 1
fi
echo ""

# Test 2: Reset costs to start fresh
echo -e "${YELLOW}=== TEST 2: Reset Cost Tracking ===${NC}"
test_endpoint "DELETE" "/api/costs/reset" "Reset all cost tracking data"

# Test 3: Get initial cost summary (should be empty)
echo -e "${YELLOW}=== TEST 3: Initial Cost Summary ===${NC}"
test_endpoint "GET" "/api/costs" "Get cost summary (should show zero costs)"

# Test 4: Track some usage via API
echo -e "${YELLOW}=== TEST 4: Track Usage via API ===${NC}"

# GPT-4.1 usage
test_endpoint "POST" "/api/costs" "Track GPT-4.1 usage" '{
    "agent_id": "test-agent-1",
    "model": "gpt-4.1",
    "usage": {
        "prompt_tokens": 10000,
        "completion_tokens": 2000,
        "total_tokens": 12000
    },
    "metadata": {
        "task": "Complex analysis",
        "user": "test-user"
    }
}'

# GPT-4.1-mini usage
test_endpoint "POST" "/api/costs" "Track GPT-4.1-mini usage" '{
    "agent_id": "test-agent-2",
    "model": "gpt-4.1-mini",
    "usage": {
        "prompt_tokens": 50000,
        "completion_tokens": 10000,
        "total_tokens": 60000
    },
    "metadata": {
        "task": "Bulk processing",
        "user": "test-user"
    }
}'

# o4-mini usage
test_endpoint "POST" "/api/costs" "Track o4-mini usage" '{
    "agent_id": "test-agent-3",
    "model": "o4-mini",
    "usage": {
        "prompt_tokens": 5000,
        "completion_tokens": 3000,
        "total_tokens": 8000
    },
    "metadata": {
        "task": "Reasoning problem",
        "user": "test-user"
    }
}'

# Test 5: Get updated cost summary
echo -e "${YELLOW}=== TEST 5: Updated Cost Summary ===${NC}"
test_endpoint "GET" "/api/costs" "Get cost summary after tracking usage"

# Test 6: Get detailed summary
echo -e "${YELLOW}=== TEST 6: Detailed Cost Summary ===${NC}"
test_endpoint "GET" "/api/costs/summary" "Get detailed summary with projections"

# Test 7: Get costs by agent
echo -e "${YELLOW}=== TEST 7: Agent-Specific Costs ===${NC}"
test_endpoint "GET" "/api/costs/agent/test-agent-1" "Get costs for test-agent-1"
test_endpoint "GET" "/api/costs/agent/test-agent-2" "Get costs for test-agent-2"
test_endpoint "GET" "/api/costs/agent/test-agent-3" "Get costs for test-agent-3"

# Test 8: Get costs by model
echo -e "${YELLOW}=== TEST 8: Model-Specific Costs ===${NC}"
test_endpoint "GET" "/api/costs/model/gpt-4.1" "Get costs for GPT-4.1"
test_endpoint "GET" "/api/costs/model/gpt-4.1-mini" "Get costs for GPT-4.1-mini"
test_endpoint "GET" "/api/costs/model/o4-mini" "Get costs for o4-mini"

# Test 9: Get costs by time range
echo -e "${YELLOW}=== TEST 9: Time Range Costs ===${NC}"
START_TIME=$(date -v-1H +%s 2>/dev/null || date -d '1 hour ago' +%s)
END_TIME=$(date +%s)
test_endpoint "GET" "/api/costs/timerange?start=${START_TIME}&end=${END_TIME}" "Get costs for last hour"

# Test 10: Get comprehensive report
echo -e "${YELLOW}=== TEST 10: Comprehensive Cost Report ===${NC}"
test_endpoint "GET" "/api/costs/report" "Get comprehensive cost report"

# Test 11: Test with period parameter
echo -e "${YELLOW}=== TEST 11: Cost Summary with Period ===${NC}"
test_endpoint "GET" "/api/costs/summary?period=3600" "Get costs for last hour (3600 seconds)"

# Test 12: Track usage with cached tokens
echo -e "${YELLOW}=== TEST 12: Track Usage with Cached Tokens ===${NC}"
test_endpoint "POST" "/api/costs" "Track usage with cached tokens" '{
    "agent_id": "test-agent-1",
    "model": "gpt-4.1",
    "usage": {
        "prompt_tokens": 5000,
        "cached_tokens": 3000,
        "completion_tokens": 1000,
        "total_tokens": 6000
    },
    "metadata": {
        "task": "Cached query",
        "cache_hit": true
    }
}'

# Test 13: Final summary
echo -e "${YELLOW}=== TEST 13: Final Cost Summary ===${NC}"
test_endpoint "GET" "/api/costs/summary" "Get final summary with all usage"

# Calculate expected costs
echo -e "${YELLOW}=== COST VERIFICATION ===${NC}"
echo "Expected costs calculation:"
echo ""
echo "GPT-4.1:"
echo "  - First call: 10,000 input + 2,000 output"
echo "  - Cost: (10,000/1M * \$2) + (2,000/1M * \$8) = \$0.02 + \$0.016 = \$0.036"
echo "  - Second call: 5,000 input (2,000 cached) + 1,000 output"
echo "  - Cost: (2,000/1M * \$2) + (3,000/1M * \$0.50) + (1,000/1M * \$8) = \$0.004 + \$0.0015 + \$0.008 = \$0.0135"
echo "  - Total: \$0.0495"
echo ""
echo "GPT-4.1-mini:"
echo "  - 50,000 input + 10,000 output"
echo "  - Cost: (50,000/1M * \$0.10) + (10,000/1M * \$0.30) = \$0.005 + \$0.003 = \$0.008"
echo ""
echo "o4-mini:"
echo "  - 5,000 input + 3,000 output"
echo "  - Cost: (5,000/1M * \$3) + (3,000/1M * \$12) = \$0.015 + \$0.036 = \$0.051"
echo ""
echo "TOTAL EXPECTED: \$0.0495 + \$0.008 + \$0.051 = \$0.1085"
echo ""

echo -e "${GREEN}=== ALL TESTS COMPLETED ===${NC}"