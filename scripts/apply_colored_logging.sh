#!/bin/bash

# Script to apply colored logging throughout the Agents.erl codebase
# This script updates existing logging calls to use the new colored_logger module

# Color codes for script output
RED='\033[31m'
GREEN='\033[32m'
YELLOW='\033[33m'
BLUE='\033[34m'
RESET='\033[0m'

echo -e "${GREEN}Applying colored logging to Agents.erl codebase...${RESET}"

# Function to update logging in a file
update_logging_in_file() {
    local file="$1"
    local backup_file="${file}.backup"
    
    if [[ ! -f "$file" ]]; then
        echo -e "${YELLOW}Warning: File $file not found, skipping...${RESET}"
        return 1
    fi
    
    echo -e "${BLUE}Updating logging in: $file${RESET}"
    
    # Create backup
    cp "$file" "$backup_file"
    
    # Apply transformations using sed
    sed -E \
        -e 's/io:format\("\[ERROR\][^"]*"/colored_logger:error("/g' \
        -e 's/io:format\("\[error\][^"]*"/colored_logger:error("/g' \
        -e 's/io:format\("\[Error\][^"]*"/colored_logger:error("/g' \
        -e 's/io:format\("\[WARNING\][^"]*"/colored_logger:warning("/g' \
        -e 's/io:format\("\[warning\][^"]*"/colored_logger:warning("/g' \
        -e 's/io:format\("\[Warning\][^"]*"/colored_logger:warning("/g' \
        -e 's/io:format\("\[SUCCESS\][^"]*"/colored_logger:success("/g' \
        -e 's/io:format\("\[success\][^"]*"/colored_logger:success("/g' \
        -e 's/io:format\("\[Success\][^"]*"/colored_logger:success("/g' \
        -e 's/io:format\("\[INFO\][^"]*"/colored_logger:info("/g' \
        -e 's/io:format\("\[info\][^"]*"/colored_logger:info("/g' \
        -e 's/io:format\("\[Info\][^"]*"/colored_logger:info("/g' \
        -e 's/io:format\("\[DEBUG\][^"]*"/colored_logger:debug("/g' \
        -e 's/io:format\("\[debug\][^"]*"/colored_logger:debug("/g' \
        -e 's/io:format\("\[Debug\][^"]*"/colored_logger:debug("/g' \
        -e 's/io:format\("\[HEALTH\][^"]*"/colored_logger:health_check("/g' \
        -e 's/io:format\("\[health\][^"]*"/colored_logger:health_check("/g' \
        -e 's/io:format\("\[Health\][^"]*"/colored_logger:health_check("/g' \
        -e 's/io:format\("\[CRITICAL\][^"]*"/colored_logger:critical("/g' \
        -e 's/io:format\("\[critical\][^"]*"/colored_logger:critical("/g' \
        -e 's/io:format\("\[Critical\][^"]*"/colored_logger:critical("/g' \
        "$backup_file" > "$file"
    
    # Check if changes were made
    if ! diff -q "$file" "$backup_file" > /dev/null 2>&1; then
        echo -e "${GREEN}  ✓ Updated logging calls in $file${RESET}"
        rm "$backup_file"
        return 0
    else
        echo -e "${YELLOW}  - No changes needed in $file${RESET}"
        mv "$backup_file" "$file"  # Restore original
        return 1
    fi
}

# Function to add colored_logger module to files that use logging
add_colored_logger_reference() {
    local file="$1"
    
    if [[ ! -f "$file" ]]; then
        return 1
    fi
    
    # Check if file uses colored_logger functions
    if grep -q "colored_logger:" "$file"; then
        echo -e "${BLUE}  Adding colored_logger reference to: $file${RESET}"
        
        # Add the module reference near the top of the file
        # This is a simple approach - in practice you might want more sophisticated handling
        sed -i.bak '/-module(/a\
%% Colored logging support\
' "$file"
        
        rm "${file}.bak" 2>/dev/null || true
    fi
}

# Find all Erlang files in the apps directory
echo -e "${BLUE}Finding Erlang source files...${RESET}"
erl_files=$(find apps/ -name "*.erl" -type f 2>/dev/null)

total_files=0
updated_files=0

for file in $erl_files; do
    ((total_files++))
    if update_logging_in_file "$file"; then
        ((updated_files++))
        add_colored_logger_reference "$file"
    fi
done

echo ""
echo -e "${GREEN}=== Summary ===${RESET}"
echo -e "${BLUE}Total files processed: $total_files${RESET}"
echo -e "${GREEN}Files updated: $updated_files${RESET}"

# Create a simple test to verify colored logging works
echo ""
echo -e "${BLUE}Creating colored logging test...${RESET}"

cat > test_colored_logging.erl << 'EOF'
-module(test_colored_logging).
-export([test/0]).

test() ->
    io:format("Testing colored logging module...~n"),
    
    colored_logger:error("This is an error message", []),
    colored_logger:warning("This is a warning message", []),
    colored_logger:success("This is a success message", []),
    colored_logger:info("This is an info message", []),
    colored_logger:debug("This is a debug message", []),
    colored_logger:health_check("System health check passed", []),
    colored_logger:api_call("GET", "/api/test", 42),
    colored_logger:critical("This is a critical system event", []),
    colored_logger:startup("System startup complete", []),
    
    io:format("Colored logging test complete!~n").
EOF

echo -e "${GREEN}✓ Created test_colored_logging.erl${RESET}"
echo ""
echo -e "${GREEN}To test colored logging, run:${RESET}"
echo -e "${BLUE}  erl -pa apps/*/ebin -eval \"test_colored_logging:test().\" -s init stop${RESET}"
echo ""
echo -e "${GREEN}To view logs with colors, use:${RESET}"
echo -e "${BLUE}  ./view_colored_logs.sh server${RESET}"
echo -e "${BLUE}  ./view_colored_logs.sh tail server.log${RESET}"
echo ""
echo -e "${GREEN}Colored logging setup complete!${RESET}"