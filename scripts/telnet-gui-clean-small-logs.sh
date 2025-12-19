#!/bin/sh

# Default values
DRY_RUN=0
MAX_LINES=5

# Parse command-line arguments
while [ $# -gt 0 ]; do
  case "$1" in
  -n | --dry-run)
    DRY_RUN=1
    shift
    ;;
  -l | --lines)
    MAX_LINES="$2"
    shift 2
    ;;
  -h | --help)
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Delete log files with fewer than the specified number of lines."
    echo ""
    echo "Options:"
    echo "  -n, --dry-run    Show what would be deleted without actually deleting"
    echo "  -l, --lines N    Consider files with N or fewer lines as small (default: 5)"
    echo "  -h, --help       Show this help message"
    exit 0
    ;;
  *)
    echo "Unknown option: $1" >&2
    echo "Use -h or --help for usage information" >&2
    exit 1
    ;;
  esac
done

# Validate MAX_LINES is a positive integer
if ! [ "$MAX_LINES" -gt 0 ] 2>/dev/null; then
  echo "Error: --lines must be a positive integer" >&2
  exit 1
fi

# Process log files
ls *.log 2>/dev/null | while read -r f; do
  l=$(wc -l <"$f" 2>/dev/null)
  if [ "$l" -le "$MAX_LINES" ]; then
    if [ "$DRY_RUN" -eq 1 ]; then
      echo "Would delete: $f ($l lines)"
    else
      rm -- "$f" && echo "Deleted: $f ($l lines)"
    fi
  fi
done
