#!/bin/bash
# Create a release tag
#
# Usage: ./scripts/release.sh MAJOR.MINOR
# Example: ./scripts/release.sh 0.2
#
# This script:
# 1. Validates the version format (MAJOR.MINOR)
# 2. Checks the working directory is clean
# 3. Checks the tag doesn't already exist
# 4. Creates an annotated tag vMAJOR.MINOR
# 5. Prints next steps

set -e

# Check argument
if [ -z "$1" ]; then
  echo "Error: Version not specified"
  echo ""
  echo "Usage: $0 MAJOR.MINOR"
  echo "Example: $0 0.2"
  exit 1
fi

VERSION="$1"
TAG_NAME="v$VERSION"

# Validate version format (MAJOR.MINOR)
if ! echo "$VERSION" | grep -qE '^[0-9]+\.[0-9]+$'; then
  echo "Error: Invalid version format: $VERSION"
  echo "Expected format: MAJOR.MINOR (e.g., 0.2)"
  exit 1
fi

# Check working directory is clean
if [ -n "$(git status --porcelain)" ]; then
  echo "Error: Working directory is not clean!"
  echo "Please commit or stash your changes before creating a release."
  echo ""
  echo "Uncommitted changes:"
  git status --porcelain
  exit 1
fi

# Check tag doesn't already exist
if git tag -l "$TAG_NAME" | grep -q "$TAG_NAME"; then
  echo "Error: Tag $TAG_NAME already exists!"
  exit 1
fi

# Create annotated tag
echo "Creating annotated tag: $TAG_NAME"
git tag -a "$TAG_NAME" -m "Release $VERSION"

echo ""
echo "========================================"
echo "Release tag $TAG_NAME created!"
echo "========================================"
echo ""
echo "Next steps:"
echo "  1. Push the tag: git push origin $TAG_NAME"
echo "  2. Reconfigure CMake to pick up new version:"
echo "     cmake -B build"
echo "  3. Build: cmake --build build"
echo ""
