#!/bin/bash
# Format multiple Lisp files in batches using Emacs
# Usage: format-all-lisp-files.sh file1.lisp file2.lisp ...
#    or: cat files.txt | xargs format-all-lisp-files.sh

if [ $# -eq 0 ]; then
    echo "Usage: $0 <file1.lisp> [file2.lisp ...]" >&2
    exit 1
fi

# Batch size (max files per emacs invocation)
BATCH_SIZE=10

# Initialize counters
CURRENT_BATCH=()
BATCH_COUNT=0
TOTAL_FILES=$#
PROCESSED_FILES=0

echo "Formatting Lisp files in batches of $BATCH_SIZE..."

# Process files in batches
for file in "$@"; do
    # Check if file exists
    if [ ! -f "$file" ]; then
        echo "Error: File not found: $file" >&2
        continue
    fi

    # Add file to current batch
    CURRENT_BATCH+=("$file")

    # Check if batch is full or this is the last file
    if [ ${#CURRENT_BATCH[@]} -eq $BATCH_SIZE ] || [ $((PROCESSED_FILES + ${#CURRENT_BATCH[@]})) -eq $TOTAL_FILES ]; then
        # Build Emacs code for this batch
        EMACS_CODE='(progn
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message t)
  (setq enable-local-variables nil)
  (setq enable-local-eval nil)
  (setq indent-tabs-mode nil)
  (setq lisp-indent-offset 2)
  (setq lisp-body-indent 2)
'

        # Add each file in current batch
        for batch_file in "${CURRENT_BATCH[@]}"; do
            # Convert path for Emacs (handle MSYS2/Windows paths)
            FILE_WIN="$batch_file"

            # Convert MSYS2 path to Windows path if needed
            if command -v cygpath >/dev/null 2>&1; then
                FILE_WIN=$(cygpath -m "$batch_file")
            else
                # Manual conversion for common MSYS2 paths
                case "$batch_file" in
                    /c/*|/d/*|/e/*|/f/*|/g/*|/h/*|/i/*|/j/*|/k/*|/l/*|/m/*|/n/*|/o/*|/p/*|/q/*|/r/*|/s/*|/t/*|/u/*|/v/*|/w/*|/x/*|/y/*|/z/*)
                        # Drive letter path: /c/foo -> C:/foo
                        DRIVE_LETTER=${batch_file:1:1}
                        FILE_WIN="${DRIVE_LETTER^^}:${batch_file:2}"
                        ;;
                esac
            fi

            # Get relative path for display
            REL_FILE="${batch_file}"

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

        # Execute emacs for this batch
        BATCH_COUNT=$((BATCH_COUNT + 1))
        echo "Processing batch $BATCH_COUNT (${#CURRENT_BATCH[@]} files)..."
        emacs --batch --quick --eval "$EMACS_CODE" 2>&1

        # Update counters
        PROCESSED_FILES=$((PROCESSED_FILES + ${#CURRENT_BATCH[@]}))

        # Clear batch for next iteration
        CURRENT_BATCH=()
    fi
done

echo "Formatted $PROCESSED_FILES files in $BATCH_COUNT batches"

exit 0
