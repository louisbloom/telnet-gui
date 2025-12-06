#!/bin/bash
# Format Lisp file in place using stdin/stdout formatter (same as VSCode)
# Usage: format-lisp-file-stdin.sh <file.lisp>

if [ $# -ne 1 ]; then
    echo "Usage: $0 <file.lisp>" >&2
    exit 1
fi

FILE="$1"

if [ ! -f "$FILE" ]; then
    echo "Error: File not found: $FILE" >&2
    exit 1
fi

# Get relative path for display
# CMake sets WORKING_DIRECTORY to CMAKE_SOURCE_DIR, so make paths relative to current dir
PWD_ABS=$(cd . && pwd)
# Normalize paths: convert backslashes to forward slashes for Windows
FILE_NORM="${FILE//\\//}"
PWD_NORM="${PWD_ABS//\\//}"

# Convert Windows path (C:/Users/...) to MSYS path (/c/Users/...) if needed
# This handles the case where CMake passes Windows paths but we're in MSYS
if [[ "$FILE_NORM" == [A-Za-z]:/* ]]; then
    # Windows absolute path: C:/Users/... -> /c/Users/...
    DRIVE_LETTER=$(echo "${FILE_NORM:0:1}" | tr '[:upper:]' '[:lower:]')
    FILE_NORM="/${DRIVE_LETTER}${FILE_NORM:2}"
fi
if [[ "$PWD_NORM" == [A-Za-z]:/* ]]; then
    # Windows absolute path: C:/Users/... -> /c/Users/...
    DRIVE_LETTER=$(echo "${PWD_NORM:0:1}" | tr '[:upper:]' '[:lower:]')
    PWD_NORM="/${DRIVE_LETTER}${PWD_NORM:2}"
fi

if [[ "$FILE_NORM" == /* ]]; then
    # Absolute path - make it relative to current working directory
    # Try Python first (most reliable cross-platform)
    if command -v python3 >/dev/null 2>&1; then
        REL_FILE=$(python3 -c "import os, sys; print(os.path.relpath(sys.argv[1]))" "$FILE_NORM" 2>/dev/null)
    fi
    # If Python failed or not available, try realpath
    if [ -z "$REL_FILE" ] && command -v realpath >/dev/null 2>&1; then
        REL_FILE=$(realpath --relative-to=. "$FILE_NORM" 2>/dev/null)
    fi
    # Fallback: manually remove PWD prefix
    if [ -z "$REL_FILE" ]; then
        if [[ "$FILE_NORM" == "$PWD_NORM"/* ]]; then
            REL_FILE="${FILE_NORM#$PWD_NORM/}"
        elif [[ "$FILE_NORM" == "$PWD_NORM" ]]; then
            REL_FILE="."
        else
            # Try case-insensitive match for Windows
            FILE_LOWER=$(echo "$FILE_NORM" | tr '[:upper:]' '[:lower:]')
            PWD_LOWER=$(echo "$PWD_NORM" | tr '[:upper:]' '[:lower:]')
            if [[ "$FILE_LOWER" == "$PWD_LOWER"/* ]]; then
                REL_FILE="${FILE_NORM#$PWD_NORM/}"
            else
                REL_FILE="$FILE_NORM"
            fi
        fi
    fi
else
    # Already relative
    REL_FILE="$FILE_NORM"
fi
# Remove leading ./ if present
REL_FILE="${REL_FILE#./}"

# Use stdin/stdout formatter and write back to file
# Keep errors separate - don't let them corrupt the output
cat "$FILE" | bash "$(dirname "$0")/emacs-format-lisp.sh" > "$FILE.tmp" 2>/dev/null

# Check if formatting succeeded by looking at exit code AND file size
if [ $? -eq 0 ] && [ -f "$FILE.tmp" ] && [ -s "$FILE.tmp" ]; then
    # Check if output looks like Lisp code (skip leading whitespace)
    FIRST_NON_WS=$(head -1 "$FILE.tmp" 2>/dev/null | sed 's/^[[:space:]]*//' | head -c 1)
    if [[ "$FIRST_NON_WS" == ";" || "$FIRST_NON_WS" == "(" || "$FIRST_NON_WS" == "" ]]; then
        # Check if file changed
        if cmp -s "$FILE" "$FILE.tmp" 2>/dev/null; then
            STATUS="(unchanged)"
        else
            STATUS="(changed)"
        fi
        mv "$FILE.tmp" "$FILE"
        # Show progress to stderr (CMake will display this)
        # Use printf instead of echo and flush for real-time output
        printf "  Formatting: %s %s\n" "$REL_FILE" "$STATUS" >&2
        # Force flush stderr (works on most systems)
        sync 2>/dev/null || true
    else
        # Output doesn't look like Lisp - probably an error message
        rm -f "$FILE.tmp"
        printf "  Formatting: %s (error - invalid output)\n" "$REL_FILE" >&2
        sync 2>/dev/null || true
        exit 1
    fi
else
    # Error occurred or empty file
    rm -f "$FILE.tmp"
    printf "  Formatting: %s (error)\n" "$REL_FILE" >&2
    sync 2>/dev/null || true
    exit 1
fi

exit 0
