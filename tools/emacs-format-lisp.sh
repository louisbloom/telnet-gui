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
            # For /tmp paths, get Windows TEMP directory
            # Try to get it from Windows environment
            if [ -n "$TEMP" ]; then
                # $TEMP is set (Windows environment variable)
                WIN_TEMP="$TEMP"
                # Convert backslashes to forward slashes
                WIN_TEMP="${WIN_TEMP//\\//}"
                TEMP_FILE_WIN="$WIN_TEMP/${TEMP_FILE#/tmp/}"
            else
                # $TEMP not set, try cmd.exe
                WIN_TEMP=$(cmd.exe /c echo %TEMP% 2>/dev/null | tr -d '\r' | sed 's/\\/\//g')
                if [ -n "$WIN_TEMP" ] && [ "$WIN_TEMP" != "%TEMP%" ]; then
                    TEMP_FILE_WIN="$WIN_TEMP/${TEMP_FILE#/tmp/}"
                else
                    # Last resort: use common Windows temp path
                    TEMP_FILE_WIN="C:/Users/$USERNAME/AppData/Local/Temp/${TEMP_FILE#/tmp/}"
                fi
            fi
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

# Check which emacs we're using and adjust path format accordingly
EMACS_PATH=$(command -v emacs)

# Detect if we're using WSL emacs (looks for /mnt/ or /usr/bin in path)
if [[ "$EMACS_PATH" =~ ^/mnt/ ]] || [[ "$EMACS_PATH" =~ ^/usr/bin ]] || [[ "$EMACS_PATH" =~ ^/usr/local/bin ]]; then
    # Convert Windows path C:/Users/... to WSL path /mnt/c/Users/...
    if [[ "$TEMP_FILE_WIN" =~ ^([A-Za-z]):(.*)$ ]]; then
        DRIVE_LETTER=${BASH_REMATCH[1]}
        REST_OF_PATH=${BASH_REMATCH[2]}
        TEMP_FILE_WIN="/mnt/${DRIVE_LETTER,,}${REST_OF_PATH}"
    fi
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
