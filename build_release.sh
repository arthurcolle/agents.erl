#!/bin/bash

# Build script for agents.erl with frontend
set -e

echo "=== Building agents.erl Release ==="
echo

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${GREEN}[BUILD]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if we're in the right directory
if [ ! -f "rebar.config" ]; then
    print_error "Not in agents.erl root directory!"
    exit 1
fi

# Step 1: Clean previous builds
print_status "Cleaning previous builds..."
rm -rf _build/prod
rm -rf apps/agent_web/priv/static/dist

# Step 2: Build frontend
print_status "Building frontend..."
cd apps/agent_web/frontend

# Check if node_modules exists
if [ ! -d "node_modules" ]; then
    print_status "Installing npm dependencies..."
    npm install
fi

# Build the frontend
print_status "Running npm build..."
npm run build

# Copy build output to priv/static
print_status "Copying frontend build to priv/static..."
cd ..
rm -rf priv/static/dist
cp -r frontend/dist priv/static/

# Return to root
cd ../..

# Step 3: Compile Erlang code
print_status "Compiling Erlang applications..."
./rebar3 compile

# Step 4: Create release
print_status "Creating production release..."
./rebar3 as prod release

# Step 5: Create tarball (optional)
if [ "$1" == "--tar" ]; then
    print_status "Creating release tarball..."
    ./rebar3 as prod tar
    print_status "Tarball created in _build/prod/rel/agents/"
fi

# Step 6: Display results
echo
print_status "Build complete!"
echo

# Check if release was created successfully
if [ -d "_build/prod/rel/agents" ]; then
    print_status "Release location: _build/prod/rel/agents"
    print_status "Frontend assets: apps/agent_web/priv/static/dist"
    echo
    print_status "To start the release:"
    echo "  _build/prod/rel/agents/bin/agents start"
    echo
    print_status "To start with console:"
    echo "  _build/prod/rel/agents/bin/agents console"
    echo
    print_status "To start as daemon:"
    echo "  _build/prod/rel/agents/bin/agents daemon"
else
    print_error "Release build failed!"
    exit 1
fi

# Optional: Check for required environment variables
echo
print_status "Checking environment variables..."
if [ -z "$OPENAI_API_KEY" ]; then
    print_warning "OPENAI_API_KEY not set"
fi
if [ -z "$ANTHROPIC_API_KEY" ]; then
    print_warning "ANTHROPIC_API_KEY not set"
fi

echo
print_status "Build script completed successfully!"