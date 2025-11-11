#!/bin/bash
# Format Lisp file using Emacs in batch mode
# Reads from stdin, writes to stdout

# Create temporary file
TEMP_FILE=$(mktemp /tmp/lisp-format-XXXXXX.lisp)

# Read from stdin and write to temp file
cat > "$TEMP_FILE"

# Use Emacs in batch mode to indent the entire buffer
emacs --batch \
    --eval "(progn (find-file \"$TEMP_FILE\")" \
    --eval "(lisp-mode)" \
    --eval "(indent-region (point-min) (point-max))" \
    --eval "(save-buffer)" \
    --eval "(kill-emacs))" 2>/dev/null

# Output the formatted file
cat "$TEMP_FILE"

# Clean up
rm -f "$TEMP_FILE"
