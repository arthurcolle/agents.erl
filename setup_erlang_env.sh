#!/bin/bash

# Set up Erlang environment for compilation
export ERL_TOP="/Users/agent/www.erlang.org/pdf_collection/otp"
export ERLANG_ROOT_DIR="$ERL_TOP"
export ERL_EI_INCLUDE_DIR="$ERL_TOP/lib/erl_interface/include"
export ERL_EI_LIBDIR="$ERL_TOP/lib/erl_interface/lib"

# Set C/C++ include paths
export C_INCLUDE_PATH="$ERL_TOP/erts/emulator/beam:$ERL_TOP/erts/emulator:$ERL_TOP/erts/include/aarch64-apple-darwin24.5.0:$ERL_EI_INCLUDE_DIR:$C_INCLUDE_PATH"
export CPLUS_INCLUDE_PATH="$C_INCLUDE_PATH"

# Set library paths
export LIBRARY_PATH="$ERL_EI_LIBDIR:$LIBRARY_PATH"
export LD_LIBRARY_PATH="$ERL_EI_LIBDIR:$LD_LIBRARY_PATH"

echo "Erlang environment configured:"
echo "ERL_TOP: $ERL_TOP"
echo "C_INCLUDE_PATH: $C_INCLUDE_PATH"
echo "LIBRARY_PATH: $LIBRARY_PATH"