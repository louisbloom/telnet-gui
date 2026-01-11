#!/bin/bash
#
# Automated profiling test runner for telnet-gui
#
# Usage: ./scripts/run-profile-test.sh <logfile> <test-name> [telnet-gui-flags...]
#
# Examples:
#   ./scripts/run-profile-test.sh ~/telnet-logs/session.log baseline
#   ./scripts/run-profile-test.sh ~/telnet-logs/session.log tintin-only -l tintin.lisp
#   ./scripts/run-profile-test.sh ~/telnet-logs/session.log full-stack \
#       -l tintin.lisp -l contrib/practice.lisp -l contrib/scan-tracker.lisp
#
# The script:
#   1. Starts mock-telnet-server.py in background (burst mode)
#   2. Runs telnet-gui with --profile --exit-on-disconnect
#   3. Saves output to profile-results/<test-name>-<timestamp>.txt
#   4. Exits when test completes
#

set -e

if [ $# -lt 2 ]; then
  echo "Usage: $0 <logfile> <test-name> [telnet-gui-flags...]"
  echo ""
  echo "Examples:"
  echo "  $0 ~/telnet-logs/session.log baseline"
  echo "  $0 ~/telnet-logs/session.log tintin-only -l tintin.lisp"
  exit 1
fi

LOGFILE="$1"
TEST_NAME="$2"
shift 2
GUI_FLAGS="$@"

# Check logfile exists
if [ ! -f "$LOGFILE" ]; then
  echo "Error: Log file not found: $LOGFILE"
  exit 1
fi

# Get script directory and project root
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Create output directory
PROFILE_DIR="$PROJECT_ROOT/profile-results"
mkdir -p "$PROFILE_DIR"

TIMESTAMP=$(date +%Y%m%d-%H%M%S)
OUTPUT_FILE="$PROFILE_DIR/${TEST_NAME}-${TIMESTAMP}.txt"

PORT=9999

echo "=== Profiling Test: $TEST_NAME ===" | tee "$OUTPUT_FILE"
echo "Log file: $LOGFILE" | tee -a "$OUTPUT_FILE"
echo "GUI flags: $GUI_FLAGS" | tee -a "$OUTPUT_FILE"
echo "Output: $OUTPUT_FILE" | tee -a "$OUTPUT_FILE"
echo "" | tee -a "$OUTPUT_FILE"

# Convert MSYS path to Windows path for Python
if [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]]; then
  LOGFILE_WIN=$(cygpath -w "$LOGFILE" 2>/dev/null || echo "$LOGFILE")
else
  LOGFILE_WIN="$LOGFILE"
fi

# Start mock server in background
echo "Starting mock server..." | tee -a "$OUTPUT_FILE"
python "$SCRIPT_DIR/mock-telnet-server.py" "$LOGFILE_WIN" --timing burst --port $PORT &
SERVER_PID=$!

# Wait for server to start
sleep 2

# Check if server started
if ! kill -0 $SERVER_PID 2>/dev/null; then
  echo "Error: Mock server failed to start" | tee -a "$OUTPUT_FILE"
  exit 1
fi

echo "Mock server started (PID: $SERVER_PID)" | tee -a "$OUTPUT_FILE"
echo "" | tee -a "$OUTPUT_FILE"

# Run telnet-gui with profiling
echo "Running telnet-gui..." | tee -a "$OUTPUT_FILE"
echo "Command: $PROJECT_ROOT/build/telnet-gui --profile --exit-on-disconnect $GUI_FLAGS localhost $PORT" | tee -a "$OUTPUT_FILE"
echo "" | tee -a "$OUTPUT_FILE"

# Run the GUI - capture stdout and stderr
"$PROJECT_ROOT/build/telnet-gui" --profile --exit-on-disconnect $GUI_FLAGS localhost $PORT 2>&1 | tee -a "$OUTPUT_FILE" || true

# Wait for server to finish (it exits after client disconnects)
wait $SERVER_PID 2>/dev/null || true

echo "" | tee -a "$OUTPUT_FILE"
echo "=== Test Complete ===" | tee -a "$OUTPUT_FILE"
echo "Results written to: $OUTPUT_FILE"
