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
# --quick: Skip loading user config (faster, avoids hooks)
# find-file-literally: Avoid auto-mode hooks and processing
# Disable hooks that might cause hangs
# Suppress stderr output (Emacs warnings)
emacs --batch --quick \
    --eval "(setq inhibit-startup-message t)" \
    --eval "(setq inhibit-startup-echo-area-message t)" \
    --eval "(setq enable-local-variables nil)" \
    --eval "(setq enable-local-eval nil)" \
    --eval "(setq indent-tabs-mode nil)" \
    --eval "(setq lisp-indent-offset 2)" \
    --eval "(setq lisp-body-indent 2)" \
    --eval "(find-file-literally \"$FILE\")" \
    --eval "(lisp-mode)" \
    --eval "(untabify (point-min) (point-max))" \
    --eval "(indent-region (point-min) (point-max))" \
    --eval "(save-buffer)" \
    --eval "(kill-buffer)" \
    --eval "(kill-emacs)" 2>/dev/null

exit 0
