#!/bin/bash
# install.sh - Cross-platform installation script for Telnet Lisp

set -e

echo "Telnet Lisp Interpreter - Installation Script"
echo "============================================="

# Detect OS
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    OS="Linux"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    OS="macOS"
elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]]; then
    OS="Windows"
else
    OS="Unknown"
fi

echo "Detected OS: $OS"

# Check for dependencies
echo "Checking dependencies..."

# Check for GCC
if ! command -v gcc &>/dev/null; then
    echo "Error: GCC compiler not found. Please install GCC."
    exit 1
fi

# Check for Make
if ! command -v make &>/dev/null; then
    echo "Error: Make not found. Please install Make."
    exit 1
fi

# Check for Boehm GC
if ! pkg-config --exists libgc; then
    echo "Warning: Boehm GC not found via pkg-config."
    echo "Please install Boehm GC:"
    case $OS in
    "Linux")
        echo "  Ubuntu/Debian: sudo apt-get install libgc-dev"
        echo "  Fedora/RHEL:   sudo dnf install gc-devel"
        echo "  Arch:          sudo pacman -S gc"
        ;;
    "macOS")
        echo "  Homebrew:      brew install bdw-gc"
        echo "  MacPorts:      sudo port install boehmgc"
        ;;
    "Windows")
        echo "  MSYS2:         pacman -S mingw-w64-ucrt-x86_64-gc"
        ;;
    esac
    exit 1
fi

echo "Dependencies OK!"

# Build
echo "Building Telnet Lisp..."
make clean
make

if [ $? -eq 0 ]; then
    echo "Build successful!"
else
    echo "Build failed!"
    exit 1
fi

# Install
echo "Installing..."
sudo make install

if [ $? -eq 0 ]; then
    echo ""
    echo "Installation complete!"
    echo "You can now run 'lisp-repl' from anywhere."
    echo ""
    echo "Test it with:"
    echo "  lisp-repl"
    echo "  echo '(+ 1 2 3)' | lisp-repl"
else
    echo "Installation failed!"
    exit 1
fi
