#!/bin/bash
# Install rlottie from source for MSYS2 UCRT64
# Usage: ./scripts/install-rlottie.sh [--shared]
#
# By default, builds a static library. Use --shared for dynamic library.

set -e

INSTALL_PREFIX="/usr/local"
BUILD_SHARED="OFF"
TEMP_DIR="/tmp/rlottie-build-$$"

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
  --shared)
    BUILD_SHARED="ON"
    shift
    ;;
  --prefix)
    INSTALL_PREFIX="$2"
    shift 2
    ;;
  -h | --help)
    echo "Usage: $0 [--shared] [--prefix /path]"
    echo "  --shared   Build shared library (default: static)"
    echo "  --prefix   Installation prefix (default: /usr/local)"
    exit 0
    ;;
  *)
    echo "Unknown option: $1"
    exit 1
    ;;
  esac
done

echo "=== Installing rlottie from source ==="
echo "Build type: $([ "$BUILD_SHARED" = "ON" ] && echo "shared" || echo "static")"
echo "Install prefix: $INSTALL_PREFIX"
echo ""

# Check for required tools
for cmd in git cmake ninja; do
  if ! command -v $cmd &>/dev/null; then
    echo "Error: $cmd is required but not installed."
    echo "Install with: pacman -S mingw-w64-ucrt-x86_64-$cmd"
    exit 1
  fi
done

# Create temp directory
mkdir -p "$TEMP_DIR"
cd "$TEMP_DIR"

# Clone rlottie
echo "=== Cloning rlottie ==="
git clone --depth 1 https://github.com/Samsung/rlottie.git
cd rlottie

# Patch CMakeLists.txt for MinGW compatibility (static builds)
# The original adds /MT flag which is MSVC-only
echo "=== Patching CMakeLists.txt for MinGW compatibility ==="
if grep -q "if (WIN32 AND NOT BUILD_SHARED_LIBS)" CMakeLists.txt; then
  sed -i 's/if (WIN32 AND NOT BUILD_SHARED_LIBS)/if (WIN32 AND NOT BUILD_SHARED_LIBS)\n    target_compile_definitions(rlottie PUBLIC -DRLOTTIE_BUILD=0)\nendif()\n\nif (WIN32 AND NOT BUILD_SHARED_LIBS AND MSVC)/' CMakeLists.txt || echo "Warning: sed patch may have failed, continuing..."
else
  echo "Warning: CMakeLists.txt pattern not found, skipping patch"
fi

# Detect C++ compiler
if command -v g++ &>/dev/null; then
  CXX_COMPILER=$(which g++)
  echo "Found C++ compiler: $CXX_COMPILER"
elif command -v clang++ &>/dev/null; then
  CXX_COMPILER=$(which clang++)
  echo "Found C++ compiler: $CXX_COMPILER"
else
  echo "Warning: No C++ compiler found, CMake will try to detect one"
  CXX_COMPILER=""
fi

# Configure
echo "=== Configuring rlottie ==="
CMAKE_ARGS=(
  -B build
  -G Ninja
  -DCMAKE_BUILD_TYPE=Release
  -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX"
  -DLIB_INSTALL_DIR="$INSTALL_PREFIX/lib"
  -DBUILD_SHARED_LIBS="$BUILD_SHARED"
  -DCMAKE_POLICY_VERSION_MINIMUM=3.5
)

# Add C++ compiler if found
if [ -n "$CXX_COMPILER" ]; then
  CMAKE_ARGS+=(-DCMAKE_CXX_COMPILER="$CXX_COMPILER")
fi

cmake "${CMAKE_ARGS[@]}"

# Build
echo "=== Building rlottie ==="
cmake --build build

# Install
echo "=== Installing rlottie ==="
cmake --install build

# Copy DLL if shared build (rlottie's install doesn't copy it)
if [ "$BUILD_SHARED" = "ON" ]; then
  echo "=== Copying DLL ==="
  cp build/librlottie.dll "$INSTALL_PREFIX/bin/"
fi

# Patch header for RLOTTIE_STATIC support (needed for static linking on Windows)
echo "=== Patching rlottiecommon.h for static linking support ==="
HEADER="$INSTALL_PREFIX/include/rlottiecommon.h"
if [ -f "$HEADER" ]; then
  # Add RLOTTIE_STATIC support to the header
  sed -i 's/#ifdef RLOTTIE_BUILD/#ifdef RLOTTIE_STATIC\n    #define RLOTTIE_API\n  #elif defined RLOTTIE_BUILD/' "$HEADER"
  echo "Patched $HEADER"
else
  echo "Warning: Header not found at $HEADER"
fi

# Cleanup
echo "=== Cleaning up ==="
cd /
rm -rf "$TEMP_DIR"

echo ""
echo "=== rlottie installed successfully ==="
echo ""
echo "Installed files:"
echo "  Headers:    $INSTALL_PREFIX/include/rlottie*.h"
if [ "$BUILD_SHARED" = "ON" ]; then
  echo "  Library:    $INSTALL_PREFIX/lib/librlottie.dll.a (import)"
  echo "  DLL:        $INSTALL_PREFIX/bin/librlottie.dll"
else
  echo "  Library:    $INSTALL_PREFIX/lib/librlottie.a (static)"
fi
echo "  pkg-config: $INSTALL_PREFIX/lib/pkgconfig/rlottie.pc"
echo ""
echo "To use in your project, reconfigure with cmake:"
echo "  rm -rf build && cmake -B build -G Ninja"
