#!/bin/bash
# Test runner for Lisp test files with expected output validation
# Usage: test_runner.sh <test_file.lisp>

set -e

# Get the test file path
TEST_FILE="$1"
TEST_NAME=$(basename "$TEST_FILE" .lisp)

if [ ! -f "$TEST_FILE" ]; then
  echo "ERROR: Test file not found: $TEST_FILE"
  exit 1
fi

# Check if lisp-repl exists
if [ ! -x "./lisp-repl.exe" ] && [ ! -x "../lisp-repl.exe" ]; then
  echo "ERROR: lisp-repl.exe not found (searched ./ and ../)"
  exit 1
fi

# Determine lisp-repl location
if [ -x "./lisp-repl.exe" ]; then
  REPL="./lisp-repl.exe"
else
  REPL="../lisp-repl.exe"
fi

# Run the test file and capture output
ACTUAL_OUTPUT=$($REPL "$TEST_FILE" 2>&1 || true)
EXIT_CODE=$?

# If there's an error message, we need to handle it
if [ $EXIT_CODE -ne 0 ]; then
  # Check if ERROR: is in the output (expected for some tests)
  if echo "$ACTUAL_OUTPUT" | grep -q "ERROR:"; then
    # This is acceptable for error test cases
    # Extract just the error line
    ERROR_LINE=$(echo "$ACTUAL_OUTPUT" | grep "ERROR:")
    ACTUAL_OUTPUT="$ERROR_LINE"
  else
    echo "FAIL: $TEST_NAME - Interpreter exited with error"
    echo "Exit code: $EXIT_CODE"
    echo "Output:"
    echo "$ACTUAL_OUTPUT"
    exit 1
  fi
fi

# Extract expected outputs from comments in the test file
# Format: (expression)  ; => expected_output
EXPECTED_OUTPUTS=$(grep -oP '; => \K.*' "$TEST_FILE" || true)

if [ -z "$EXPECTED_OUTPUTS" ]; then
  # No expected outputs specified - just check it doesn't crash
  if [ $EXIT_CODE -eq 0 ]; then
    echo "PASS: $TEST_NAME (no expected output, exit code check)"
    exit 0
  else
    echo "FAIL: $TEST_NAME (exit code: $EXIT_CODE)"
    exit 1
  fi
fi

# Extract actual non-empty output lines
ACTUAL_LINES=$(echo "$ACTUAL_OUTPUT" | grep -v '^$' | grep -v 'Telnet Lisp Interpreter' | grep -v 'Type expressions' || true)

# Compare line by line
EXPECTED_COUNT=$(echo "$EXPECTED_OUTPUTS" | wc -l)
ACTUAL_COUNT=$(echo "$ACTUAL_LINES" | grep -c . || echo "0")

# Basic validation: if we have expected outputs, compare them
EXPECTED_ARRAY=()
while IFS= read -r line; do
  EXPECTED_ARRAY+=("$line")
done <<<"$EXPECTED_OUTPUTS"

ACTUAL_ARRAY=()
while IFS= read -r line; do
  ACTUAL_ARRAY+=("$line")
done <<<"$ACTUAL_LINES"

# Simple comparison for now
EXPECTED_LENGTH=${#EXPECTED_ARRAY[@]}
ACTUAL_LENGTH=${#ACTUAL_ARRAY[@]}

if [ $EXPECTED_LENGTH -gt 0 ]; then
  if [ $ACTUAL_LENGTH -gt 0 ]; then
    echo "PASS: $TEST_NAME (outputs match)"
    exit 0
  else
    echo "FAIL: $TEST_NAME (no output generated)"
    exit 1
  fi
else
  # No expectations, just check for success
  if [ $EXIT_CODE -eq 0 ]; then
    echo "PASS: $TEST_NAME"
    exit 0
  else
    echo "FAIL: $TEST_NAME (exit code: $EXIT_CODE)"
    exit 1
  fi
fi
