#!/bin/bash
# Format multiple Lisp files in a single Emacs batch session
# Usage: format-all-lisp-files.sh file1.lisp file2.lisp ...
#    or: cat files.txt | xargs format-all-lisp-files.sh

if [ $# -eq 0 ]; then
    echo "Usage: $0 <file1.lisp> [file2.lisp ...]" >&2
    exit 1
fi

# Build the Emacs Lisp code to format all files
EMACS_CODE='(progn
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message t)
  (setq enable-local-variables nil)
  (setq enable-local-eval nil)
  (setq indent-tabs-mode nil)
  (setq lisp-indent-offset 2)
  (setq lisp-body-indent 2)
'

# Add each file to the Emacs code
for file in "$@"; do
    # Check if file exists
    if [ ! -f "$file" ]; then
        echo "Error: File not found: $file" >&2
        continue
    fi

    # Convert path for Emacs (handle MSYS2/Windows paths)
    FILE_WIN="$file"

    # Convert MSYS2 path to Windows path if needed
    if command -v cygpath >/dev/null 2>&1; then
        FILE_WIN=$(cygpath -m "$file")
    else
        # Manual conversion for common MSYS2 paths
        case "$file" in
            /c/*|/d/*|/e/*|/f/*|/g/*|/h/*|/i/*|/j/*|/k/*|/l/*|/m/*|/n/*|/o/*|/p/*|/q/*|/r/*|/s/*|/t/*|/u/*|/v/*|/w/*|/x/*|/y/*|/z/*)
                # Drive letter path: /c/foo -> C:/foo
                DRIVE_LETTER=${file:1:1}
                FILE_WIN="${DRIVE_LETTER^^}:${file:2}"
                ;;
        esac
    fi

    # Get relative path for display
    REL_FILE="${file}"

    # Add formatting commands for this file to Emacs code
    EMACS_CODE+="
  (let ((file \"$FILE_WIN\")
        (rel-file \"$REL_FILE\")
        (original-content nil))
    (condition-case err
        (progn
          (find-file-literally file)
          (setq original-content (buffer-string))
          (lisp-mode)
          (untabify (point-min) (point-max))
          (indent-region (point-min) (point-max))
          (if (string= original-content (buffer-string))
              (message \"  Formatting: %s (unchanged)\" rel-file)
            (progn
              (save-buffer)
              (message \"  Formatting: %s (changed)\" rel-file)))
          (kill-buffer))
      (error
        (message \"  Formatting: %s (error: %s)\" rel-file (error-message-string err))
        (kill-buffer (current-buffer)))))
"
done

EMACS_CODE+='
  (kill-emacs 0))'

# Run Emacs with all the formatting commands
emacs --batch --quick --eval "$EMACS_CODE" 2>&1

exit 0
