#!/bin/bash
# Format Lisp file in place using Emacs batch mode
# Usage: format-lisp-file.sh <file.lisp>

if [ $# -ne 1 ]; then
    echo "Usage: $0 <file.lisp>" >&2
    exit 1
fi

FILE="$1"

if [ ! -f "$FILE" ]; then
    echo "Error: File not found: $FILE" >&2
    exit 1
fi

# Use Emacs in batch mode to indent the entire buffer
# Suppress stderr output (Emacs warnings)
emacs --batch --eval "(progn (find-file \"$FILE\") (lisp-mode) (indent-region (point-min) (point-max)) (save-buffer) (kill-emacs))" 2>/dev/null

exit 0
