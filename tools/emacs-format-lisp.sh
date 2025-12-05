#!/bin/bash
# Format Lisp file using Emacs in batch mode
# Reads from stdin, writes to stdout

# Create temporary file
TEMP_FILE=$(mktemp /tmp/lisp-format-XXXXXX.lisp)

# Read from stdin and write to temp file
cat > "$TEMP_FILE"

# Use Emacs in batch mode to indent the entire buffer
# --quick: Skip loading user config (faster, avoids hooks)
# find-file-literally: Avoid auto-mode hooks and processing
# Disable hooks that might cause hangs
emacs --batch --quick \
    --eval "(progn (setq inhibit-startup-message t inhibit-startup-echo-area-message t)" \
    --eval "(setq enable-local-variables nil)" \
    --eval "(setq enable-local-eval nil)" \
    --eval "(setq indent-tabs-mode nil)" \
    --eval "(find-file-literally \"$TEMP_FILE\")" \
    --eval "(lisp-mode)" \
    --eval "(untabify (point-min) (point-max))" \
    --eval "(indent-region (point-min) (point-max))" \
    --eval "(save-buffer)" \
    --eval "(kill-buffer)" \
    --eval "(kill-emacs))" 2>/dev/null

# Output the formatted file
cat "$TEMP_FILE"

# Clean up
rm -f "$TEMP_FILE"
