#!/bin/bash
# Format Lisp file using Emacs in batch mode
# Reads from stdin, writes to stdout

# Create temporary file (force using /tmp directory with proper template)
# Using explicit template ensures proper path format: /tmp/tmp.XXXXXX.lisp
TEMP_FILE=$(mktemp /tmp/tmp.XXXXXX.lisp 2>/dev/null || mktemp --suffix=.lisp)

# Read from stdin and write to temp file
cat > "$TEMP_FILE"

# Convert MSYS2 path to Windows path for Emacs on Windows
# On Windows, emacs.exe doesn't understand /tmp/ paths
if command -v cygpath >/dev/null 2>&1; then
    # Use cygpath to convert, then change backslashes to forward slashes for Emacs
    TEMP_FILE_WIN=$(cygpath -m "$TEMP_FILE")
else
    # Fallback: Convert MSYS2 path manually (e.g., /tmp/foo -> C:/msys64/tmp/foo)
    # This handles common MSYS2 path formats
    case "$TEMP_FILE" in
        /tmp/*)
            # For /tmp paths, use %TEMP% on Windows
            TEMP_FILE_WIN="$TEMP/${TEMP_FILE#/tmp/}"
            # Convert backslashes to forward slashes for Emacs
            TEMP_FILE_WIN="${TEMP_FILE_WIN//\\//}"
            ;;
        /c/*|/d/*|/e/*|/f/*|/g/*|/h/*|/i/*|/j/*|/k/*|/l/*|/m/*|/n/*|/o/*|/p/*|/q/*|/r/*|/s/*|/t/*|/u/*|/v/*|/w/*|/x/*|/y/*|/z/*)
            # Drive letter path: /c/foo -> C:/foo
            DRIVE_LETTER=${TEMP_FILE:1:1}
            TEMP_FILE_WIN="${DRIVE_LETTER^^}:${TEMP_FILE:2}"
            ;;
        *)
            # Already a Windows path or other format
            TEMP_FILE_WIN="$TEMP_FILE"
            ;;
    esac
fi

# Use Emacs in batch mode to indent the entire buffer
# --quick: Skip loading user config (faster, avoids hooks)
# find-file-literally: Avoid auto-mode hooks and processing
# Disable hooks that might cause hangs
emacs --batch --quick \
    --eval "(setq inhibit-startup-message t)" \
    --eval "(setq inhibit-startup-echo-area-message t)" \
    --eval "(setq enable-local-variables nil)" \
    --eval "(setq enable-local-eval nil)" \
    --eval "(setq indent-tabs-mode nil)" \
    --eval "(setq lisp-indent-offset 2)" \
    --eval "(setq lisp-body-indent 2)" \
    --eval "(find-file-literally \"$TEMP_FILE_WIN\")" \
    --eval "(lisp-mode)" \
    --eval "(untabify (point-min) (point-max))" \
    --eval "(indent-region (point-min) (point-max))" \
    --eval "(save-buffer)" \
    --eval "(kill-buffer)" \
    --eval "(kill-emacs)" 2>/dev/null

# Output the formatted file
cat "$TEMP_FILE"

# Clean up
rm -f "$TEMP_FILE"
