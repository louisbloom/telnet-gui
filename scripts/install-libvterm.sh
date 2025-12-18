#!/bin/bash
# Install libvterm from source (neovim branch) for MSYS2 UCRT64
# Usage: ./scripts/install-libvterm.sh [--prefix /path]
#
# The neovim branch has additional features like sb_pushline4 for text reflow.

set -e

INSTALL_PREFIX="/usr/local"
TEMP_DIR="/tmp/libvterm-build-$$"
BRANCH="nvim"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --prefix)
            INSTALL_PREFIX="$2"
            shift 2
            ;;
        --branch)
            BRANCH="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [--prefix /path] [--branch name]"
            echo "  --prefix   Installation prefix (default: /usr/local)"
            echo "  --branch   Git branch to use (default: nvim)"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "=== Installing libvterm from source ==="
echo "Branch: $BRANCH"
echo "Install prefix: $INSTALL_PREFIX"
echo ""

# Check for required tools
for cmd in git make libtool; do
    if ! command -v $cmd &> /dev/null; then
        echo "Error: $cmd is required but not installed."
        if [ "$cmd" = "libtool" ]; then
            echo "Install with: pacman -S libtool"
        else
            echo "Install with: pacman -S $cmd"
        fi
        exit 1
    fi
done

# Create temp directory
mkdir -p "$TEMP_DIR"
cd "$TEMP_DIR"

# Clone libvterm
echo "=== Cloning libvterm ($BRANCH branch) ==="
git clone --depth 1 -b "$BRANCH" https://github.com/neovim/libvterm.git
cd libvterm

# Build library (ignore bin failures on Windows - termios.h not available)
echo "=== Building libvterm ==="
# First build the .inc files and library
make src/encoding/DECdrawing.inc src/encoding/uk.inc 2>/dev/null || true

# Build just the library, not the binaries (which need termios.h)
# We compile each source file and create the library manually if make fails
if ! make libvterm.la 2>&1; then
    echo "Full make failed, trying to build library only..."

    # Generate encoding tables first
    for tbl in src/encoding/*.tbl; do
        inc="${tbl%.tbl}.inc"
        if [ ! -f "$inc" ]; then
            echo "TBL $tbl"
            perl -CSD tbl2inc_c.pl "$tbl" > "$inc"
        fi
    done

    # Compile source files
    for src in src/*.c; do
        obj="${src%.c}.lo"
        echo "CC $src"
        libtool --mode=compile --tag=CC gcc -Wall -Iinclude -std=c99 -c "$src" -o "$obj"
    done

    # Link library
    echo "LINK libvterm.la"
    libtool --mode=link --tag=CC gcc -o libvterm.la src/*.lo \
        -rpath "$INSTALL_PREFIX/lib" \
        -version-info 0:0:0
fi

# Verify library was built
if [ ! -f ".libs/libvterm.a" ]; then
    echo "Error: Failed to build libvterm.a"
    exit 1
fi

# Install headers and library (skip binaries)
echo "=== Installing libvterm ==="
make install-inc install-lib PREFIX="$INSTALL_PREFIX"

# Verify installation
echo "=== Verifying installation ==="
if [ ! -f "$INSTALL_PREFIX/lib/libvterm.a" ]; then
    echo "Error: libvterm.a not found after installation"
    exit 1
fi

if [ ! -f "$INSTALL_PREFIX/include/vterm.h" ]; then
    echo "Error: vterm.h not found after installation"
    exit 1
fi

if [ ! -f "$INSTALL_PREFIX/lib/pkgconfig/vterm.pc" ]; then
    echo "Error: vterm.pc not found after installation"
    exit 1
fi

# Get version from installed pc file
INSTALLED_VERSION=$(grep "^Version:" "$INSTALL_PREFIX/lib/pkgconfig/vterm.pc" | cut -d' ' -f2)

# Cleanup
echo "=== Cleaning up ==="
cd /
rm -rf "$TEMP_DIR"

echo ""
echo "=== libvterm installed successfully ==="
echo ""
echo "Installed files:"
echo "  Headers:    $INSTALL_PREFIX/include/vterm.h"
echo "              $INSTALL_PREFIX/include/vterm_keycodes.h"
echo "  Library:    $INSTALL_PREFIX/lib/libvterm.a"
echo "  pkg-config: $INSTALL_PREFIX/lib/pkgconfig/vterm.pc"
echo "  Version:    $INSTALLED_VERSION"
echo ""
echo "Features (nvim branch):"
echo "  - sb_pushline4 support for text reflow"
echo "  - vterm_screen_enable_reflow()"
echo ""
echo "To use in your project, reconfigure with cmake:"
echo "  rm -rf build && cmake -B build -G Ninja"
