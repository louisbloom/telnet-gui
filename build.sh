#!/bin/bash
# Build script for Git Bash/MINGW64 users
# Invokes UCRT64 shell to ensure correct environment

set -e

TARGET="${1:-all}"
CLEAN=0
INSTALL=0
PREFIX="$HOME/.local"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
  --clean)
    CLEAN=1
    shift
    ;;
  --install)
    INSTALL=1
    shift
    ;;
  --prefix)
    PREFIX="$2"
    shift 2
    ;;
  *)
    TARGET="$1"
    shift
    ;;
  esac
done

# Detect MSYS2 location
if [[ -d "$USERPROFILE/scoop/apps/msys2/current" ]]; then
  MSYS2_PATH="$USERPROFILE/scoop/apps/msys2/current"
else
  MSYS2_PATH="/c/msys64"
fi

SHELL_CMD="$MSYS2_PATH/msys2_shell.cmd"

if [[ $CLEAN -eq 1 ]]; then
  echo "Cleaning build directory..."
  rm -rf build
fi

if [[ ! -d build ]]; then
  echo "Configuring with CMake..."
  "$SHELL_CMD" -defterm -here -no-start -ucrt64 -c "cmake -B build -G Ninja"
fi

echo "Building target: $TARGET"
"$SHELL_CMD" -defterm -here -no-start -ucrt64 -c "cmake --build build --target $TARGET"

if [[ $INSTALL -eq 1 ]]; then
  echo "Installing to: $PREFIX"
  "$SHELL_CMD" -defterm -here -no-start -ucrt64 -c "cmake --install build --prefix '$PREFIX'"
fi
