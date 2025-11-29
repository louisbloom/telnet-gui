#!/bin/bash
# Test runner for telnet-gui Lisp test files with expected output validation
# Usage: test_runner.sh <test_file.lisp>
#
# This script uses telnet-gui's headless mode (-t flag) to run tests,
# allowing testing of telnet-gui-specific built-ins like terminal-echo.
#
# Set VERBOSE=1 environment variable for verbose output

set -e

# Verbose mode (can be enabled with VERBOSE=1)
VERBOSE=${VERBOSE:-0}

# Get the test file path
TEST_FILE="$1"
TEST_NAME=$(basename "$TEST_FILE" .lisp)

# Always show basic info (for debugging)
echo "=== Test Runner: $TEST_NAME ===" >&2
echo "Test file (arg): $TEST_FILE" >&2

if [ "$VERBOSE" = "1" ]; then
  echo "=== Test Runner: $TEST_NAME ==="
  echo "Test file: $TEST_FILE"
fi

if [ ! -f "$TEST_FILE" ]; then
  echo "ERROR: Test file not found: $TEST_FILE"
  exit 1
fi

# Check if telnet-gui exists in build directory or parent
if [ ! -x "./telnet-gui.exe" ] && [ ! -x "../telnet-gui.exe" ] && [ ! -x "./telnet-gui" ] && [ ! -x "../telnet-gui" ]; then
  echo "ERROR: telnet-gui executable not found (searched ./ and ../)"
  exit 1
fi

# Determine telnet-gui location
if [ -x "./telnet-gui.exe" ]; then
  GUI="./telnet-gui.exe"
elif [ -x "./telnet-gui" ]; then
  GUI="./telnet-gui"
elif [ -x "../telnet-gui.exe" ]; then
  GUI="../telnet-gui.exe"
else
  GUI="../telnet-gui"
fi

if [ "$VERBOSE" = "1" ]; then
  echo "Using telnet-gui: $GUI"
fi

# Convert test file path for telnet-gui
# Since CMake copies test files to build directory, prefer local file
TEST_FILE_NAME=$(basename "$TEST_FILE")

# Check if file exists in current directory (primary case for ctest)
if [ -f "$TEST_FILE_NAME" ]; then
    TEST_FILE_REL="$TEST_FILE_NAME"
    if [ "$VERBOSE" = "1" ]; then
        echo "Using test file from current directory: $TEST_FILE_REL"
    fi
elif [ -f "$TEST_FILE" ]; then
    # Fallback: use original path (for manual execution from source dir)
    TEST_FILE_REL="$TEST_FILE"
    if [ "$VERBOSE" = "1" ]; then
        echo "Using test file from original path: $TEST_FILE_REL"
    fi
else
    # Try relative path from build dir to source
    TEST_FILE_REL="../../telnet-gui/$TEST_FILE_NAME"
fi

# Verify file exists
if [ ! -f "$TEST_FILE_REL" ]; then
    echo "ERROR: Test file not found: $TEST_FILE_REL" >&2
    echo "  Original path: $TEST_FILE" >&2
    echo "  Tried: $TEST_FILE_NAME (in current dir)" >&2
    exit 1
fi

if [ "$VERBOSE" = "1" ]; then
  echo "Test file (original): $TEST_FILE"
  echo "Test file (using): $TEST_FILE_REL"
  echo "Running test file in headless mode..."
  echo "Command: $GUI -t \"$TEST_FILE_REL\""
fi

# Run the test file in headless mode and capture output
# Use the relative path directly - telnet-gui should handle it correctly
# The relative path from build/telnet-gui to source is ../../telnet-gui/

if [ "$VERBOSE" = "1" ]; then
  echo "Test file (using): $TEST_FILE_REL"
  echo "Command: $GUI -t \"$TEST_FILE_REL\""
fi

# Run with the path - ensure -t and path are separate arguments
ACTUAL_OUTPUT=$("$GUI" -t "$TEST_FILE_REL" 2>&1 || true)
EXIT_CODE=$?

if [ "$VERBOSE" = "1" ]; then
  echo "Exit code: $EXIT_CODE"
  echo "First 5 lines of output:"
  echo "$ACTUAL_OUTPUT" | head -5
fi

if [ "$VERBOSE" = "1" ]; then
  echo "Exit code: $EXIT_CODE"
  echo "--- Actual Output ---"
  echo "$ACTUAL_OUTPUT"
  echo "--- End Output ---"
fi

# If there's an error message, we need to handle it
if [ $EXIT_CODE -ne 0 ]; then
  # Check if ERROR: is in the output (expected for some tests)
  if echo "$ACTUAL_OUTPUT" | grep -q "ERROR:"; then
    # This is acceptable for error test cases
    # Extract just the error line
    ERROR_LINE=$(echo "$ACTUAL_OUTPUT" | grep "ERROR:")
    ACTUAL_OUTPUT="$ERROR_LINE"
  else
    echo "FAIL: $TEST_NAME - telnet-gui exited with error"
    echo "Exit code: $EXIT_CODE"
    echo "Output:"
    echo "$ACTUAL_OUTPUT"
    exit 1
  fi
fi

# Extract expected outputs from comments in the test file
# Format: (expression)  ; => expected_output
EXPECTED_OUTPUTS=$(grep -oP '; => \K.*' "$TEST_FILE" || true)

if [ "$VERBOSE" = "1" ]; then
  echo "--- Expected Outputs ---"
  echo "$EXPECTED_OUTPUTS"
  echo "--- End Expected ---"
fi

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
# Filter out telnet-gui startup messages and other noise
ACTUAL_LINES=$(echo "$ACTUAL_OUTPUT" | grep -v '^$' | grep -v 'Running test:' | grep -v 'Test completed successfully' | grep -v 'Test failed:' | grep -v 'Loaded Lisp file:' || true)

# Build arrays of expected and actual outputs
EXPECTED_ARRAY=()
while IFS= read -r line; do
  [ -n "$line" ] && EXPECTED_ARRAY+=("$line")
done <<<"$EXPECTED_OUTPUTS"

ACTUAL_ARRAY=()
while IFS= read -r line; do
  [ -n "$line" ] && ACTUAL_ARRAY+=("$line")
done <<<"$ACTUAL_LINES"

EXPECTED_LENGTH=${#EXPECTED_ARRAY[@]}
ACTUAL_LENGTH=${#ACTUAL_ARRAY[@]}

# Extract test case descriptions by parsing the test file
# Match each `; =>` comment with its preceding expression to get context
TEST_DESCRIPTIONS=()
LINE_NUM=0
CURRENT_TEST=""
while IFS= read -r line; do
  LINE_NUM=$((LINE_NUM + 1))

  # Check if this line has a `; =>` comment (an assertion)
  if echo "$line" | grep -qE '; =>'; then
    # Extract the expression before the comment for context
    EXPR=$(echo "$line" | sed 's/; =>.*$//' | sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//')

    # Try to extract a meaningful description
    # Look for function calls or test headers in the expression
    if echo "$EXPR" | grep -qE 'print.*=== Test [0-9]+:'; then
      # This is a test header - extract the test name
      DESC=$(echo "$EXPR" | sed 's/.*=== Test [0-9]\+: \(.*\) ===.*/\1/')
      CURRENT_TEST="$DESC"
      # Don't add header lines as test descriptions
      continue
    elif echo "$EXPR" | grep -qE '^\(print '; then
      # This is a print statement - try to extract what's being printed
      PRINTED=$(echo "$EXPR" | sed 's/^[[:space:]]*(print[[:space:]]*//' | sed 's/)[[:space:]]*$//')
      # If it's a function call, use the function name
      if echo "$PRINTED" | grep -qE '^\([a-z-]+'; then
        FUNC=$(echo "$PRINTED" | sed 's/^([[:space:]]*\([a-z-]\+\).*/\1/')
        if [ -n "$CURRENT_TEST" ]; then
          TEST_DESCRIPTIONS+=("$CURRENT_TEST: $FUNC")
        else
          TEST_DESCRIPTIONS+=("$FUNC")
        fi
      else
        if [ -n "$CURRENT_TEST" ]; then
          TEST_DESCRIPTIONS+=("$CURRENT_TEST")
        else
          TEST_DESCRIPTIONS+=("Assertion $LINE_NUM")
        fi
      fi
    else
      # Generic expression - use function name if available
      if echo "$EXPR" | grep -qE '^\([a-z-]+'; then
        FUNC=$(echo "$EXPR" | sed 's/^([[:space:]]*\([a-z-]\+\).*/\1/')
        if [ -n "$CURRENT_TEST" ]; then
          TEST_DESCRIPTIONS+=("$CURRENT_TEST: $FUNC")
        else
          TEST_DESCRIPTIONS+=("$FUNC")
        fi
      else
        if [ -n "$CURRENT_TEST" ]; then
          TEST_DESCRIPTIONS+=("$CURRENT_TEST")
        else
          TEST_DESCRIPTIONS+=("Assertion $LINE_NUM")
        fi
      fi
    fi
  # Check for test section headers in comments
  elif echo "$line" | grep -qE ';; Test [0-9]+:'; then
    DESC=$(echo "$line" | sed 's/^[[:space:]]*;;[[:space:]]*Test [0-9]\+:[[:space:]]*//')
    CURRENT_TEST="$DESC"
  fi
done < "$TEST_FILE"

# Always show verbose output for individual test results
echo "Running $TEST_NAME..."
echo ""

# Compare each expected output with actual output
PASSED=0
FAILED=0
TOTAL_TESTS=$EXPECTED_LENGTH

if [ $EXPECTED_LENGTH -eq 0 ]; then
  # No expected outputs - just check exit code
  if [ $EXIT_CODE -eq 0 ]; then
    echo "  ✓ Test file executed successfully (no assertions)"
    echo ""
    echo "PASS: $TEST_NAME"
    exit 0
  else
    echo "  ✗ Test file failed with exit code: $EXIT_CODE"
    echo ""
    echo "FAIL: $TEST_NAME"
    exit 1
  fi
fi

# Show individual test results
for ((i=0; i<EXPECTED_LENGTH; i++)); do
  EXPECTED="${EXPECTED_ARRAY[$i]}"
  ACTUAL=""

  if [ $i -lt $ACTUAL_LENGTH ]; then
    ACTUAL="${ACTUAL_ARRAY[$i]}"
  fi

  # Get test description
  TEST_DESC=""
  if [ $i -lt ${#TEST_DESCRIPTIONS[@]} ]; then
    TEST_DESC="${TEST_DESCRIPTIONS[$i]}"
  fi

  # Compare expected vs actual
  if [ "$EXPECTED" = "$ACTUAL" ]; then
    PASSED=$((PASSED + 1))
    if [ -n "$TEST_DESC" ]; then
      echo "  ✓ [$((i+1))/$EXPECTED_LENGTH] $TEST_DESC"
      echo "      Expected: $EXPECTED"
      echo "      Actual:   $ACTUAL"
    else
      echo "  ✓ [$((i+1))/$EXPECTED_LENGTH] Assertion $((i+1))"
      echo "      Expected: $EXPECTED"
      echo "      Actual:   $ACTUAL"
    fi
  else
    FAILED=$((FAILED + 1))
    if [ -n "$TEST_DESC" ]; then
      echo "  ✗ [$((i+1))/$EXPECTED_LENGTH] $TEST_DESC"
    else
      echo "  ✗ [$((i+1))/$EXPECTED_LENGTH] Assertion $((i+1))"
    fi
    echo "      Expected: $EXPECTED"
    if [ -z "$ACTUAL" ]; then
      echo "      Actual:   (no output)"
    else
      echo "      Actual:   $ACTUAL"
    fi
  fi
done

# Show summary
echo ""
if [ $FAILED -eq 0 ]; then
  echo "  All $PASSED test(s) passed"
  echo ""
  echo "PASS: $TEST_NAME"
  exit 0
else
  echo "  $FAILED of $TOTAL_TESTS test(s) failed"
  echo ""
  echo "FAIL: $TEST_NAME"
  exit 1
fi
