# Telnet Lisp Repository

A multi-project repository focused on **telnet-lisp** integration. This repository contains independent projects that share a common theme: integration with the telnet-lisp Lisp interpreter library.

## Repository Overview

This repository is organized around **telnet-lisp**, the core embeddable Lisp interpreter library. All projects in this repository integrate with or use telnet-lisp in various ways:

- **telnet-lisp** - The core library: A minimal, embeddable Lisp interpreter written in C with automatic garbage collection, regex support, and UTF-8 handling
- **telnet-gui** - GUI application: A graphical telnet client that integrates telnet-lisp for scriptable automation and command processing

While these projects are independent and can be built separately, they share the common foundation of telnet-lisp integration.

## Repository Structure

```
telnet-lisp/
├── telnet-lisp/    # Core Lisp interpreter library (foundation)
│   ├── include/       # Public headers (lisp.h, utf8.h)
│   ├── src/          # Library source files
│   ├── repl/         # REPL application
│   ├── tests/        # Test suite
│   ├── CMakeLists.txt
│   ├── README.md     # Library documentation
│   └── PACKAGING.md  # Integration guide
├── telnet-gui/       # GUI application (integrates telnet-lisp)
│   ├── src/          # GUI source files
│   ├── CMakeLists.txt
│   └── README.md     # GUI-specific documentation
├── CMakeLists.txt    # Root build configuration
└── build/            # Build directory (generated)
```

## Quick Start

### Prerequisites

**Windows (MSYS2 UCRT64):**

```bash
# Core library dependencies (required for all projects)
pacman -S mingw-w64-ucrt-x86_64-gcc mingw-w64-ucrt-x86_64-cmake
pacman -S mingw-w64-ucrt-x86_64-bdw-gc mingw-w64-ucrt-x86_64-pcre2

# GUI project dependencies (optional, only for telnet-gui)
pacman -S mingw-w64-ucrt-x86_64-SDL2
pacman -S mingw-w64-ucrt-x86_64-SDL2_ttf
pacman -S mingw-w64-ucrt-x86_64-libvterm
```

**Linux:**

```bash
# Core library dependencies
sudo apt install build-essential cmake pkg-config libgc-dev libpcre2-dev

# GUI project dependencies (optional)
sudo apt install libsdl2-dev libsdl2-ttf-dev
# libvterm: install from source (see telnet-gui/README.md)
```

**macOS:**

```bash
# Core library dependencies
brew install cmake pkg-config bdw-gc pcre2

# GUI project dependencies (optional)
brew install sdl2 sdl2_ttf
# libvterm: install from source (see telnet-gui/README.md)
```

### Building

Build all projects from the repository root:

```bash
# Create build directory
mkdir build && cd build

# Configure CMake
cmake .. -G Ninja

# Build everything
cmake --build .

# Or build specific targets
cmake --build . --target liblisp      # Core library only
cmake --build . --target lisp-repl     # REPL application
cmake --build . --target telnet-gui    # GUI application (requires GUI deps)
```

**Alternative with MinGW Makefiles (Windows):**

```bash
cmake .. -G "MinGW Makefiles"
cmake --build .
```

### Running the REPL

```bash
# From build directory
./lisp-repl

# Or from project root
./build/lisp-repl
```

Try some Lisp:

```lisp
>>> (+ 1 2 3)
6
>>> (define square (lambda (x) (* x x)))
#<lambda>
>>> (square 5)
25
>>> :quit
```

## Core Library: telnet-lisp

**telnet-lisp** is the core embeddable Lisp interpreter library that serves as the foundation for all projects in this repository. It provides a complete Lisp implementation with modern features, designed for integration into applications.

### Key Features

- **Complete Lisp implementation** with lexical scoping, closures, and first-class functions
- **Rich data types**: Numbers, integers, strings (UTF-8), lists, vectors, hash tables, booleans
- **PCRE2 regex support** with UTF-8 and Unicode character properties
- **Automatic memory management** with Boehm GC
- **File I/O** with cross-platform line ending support
- **REPL** for interactive development
- **Built-in functions**: Arithmetic, string operations, list/vector manipulation, regex matching

### Quick Example

```lisp
; Define a function
(define square (lambda (x) (* x x)))
(square 5)         ; => 25

; String operations with UTF-8 support
(string-length "Hello, 世界!")  ; => 11

; Regex pattern matching
(regex-match "\\d+" "hello123")  ; => 1
```

### Documentation

For complete documentation including:
- All available functions and special forms
- Detailed API reference
- Examples and usage patterns
- Embedding guide

See **[telnet-lisp/README.md](telnet-lisp/README.md)**.

To integrate telnet-lisp into your project, see **[telnet-lisp/PACKAGING.md](telnet-lisp/PACKAGING.md)**.

## Projects

### telnet-gui

A graphical telnet client with terminal emulation, built using SDL2 and libvterm. This project integrates with **telnet-lisp** to provide scriptable automation and command processing capabilities.

**Integration with telnet-lisp:**
- Uses telnet-lisp for Lisp-based automation and scripting
- Provides a GUI interface for telnet connections
- Enables Lisp scripting within the telnet client

**Key Features:**
- Full VT100-compatible terminal emulation using libvterm
- SDL2-based graphical interface with font rendering
- Real-time rendering with efficient screen updates and glyph caching
- Keyboard and mouse input support

**Dependencies:**
- SDL2 (windowing and rendering)
- SDL2_ttf (font rendering)
- libvterm (terminal emulation) - install from source
- **telnet-lisp** (core library - included)

**Building:**

```bash
# Install GUI dependencies (MSYS2)
pacman -S mingw-w64-ucrt-x86_64-SDL2
pacman -S mingw-w64-ucrt-x86_64-SDL2_ttf
# libvterm: install from source (see telnet-gui/README.md)

# Build from repository root
cd build
cmake --build . --target telnet-gui
```

**Usage:**

```bash
./build/telnet-gui/telnet-gui.exe <hostname> <port>
```

**Documentation:** See [`telnet-gui/README.md`](telnet-gui/README.md) for detailed documentation, including libvterm integration notes and troubleshooting.

## CMake Targets

Repository-level targets:

- `liblisp` - Static library (core)
- `lisp-repl` - REPL application (uses telnet-lisp)
- `telnet-gui` - GUI application (uses telnet-lisp)
- `format` - Format all source files
- `test` - Run test suite
- `install` - Install to system directories

View all targets:

```bash
cd build
cmake .. -G Ninja
cmake --build . --target help
```

## Formatting Code

Format all sources (library, REPL, and GUI):

```bash
cd build
cmake --build . --target format
```

Or from the project root:

```bash
cmake --build build --target format
```

## Platform Support

- **Windows**: MSYS2 UCRT64 (GCC)
- **Linux**: GCC with standard libraries
- **macOS**: Clang/GCC with Homebrew

## Development

### Build System

The repository uses **CMake** as the primary build system, providing:

- Cross-platform support
- Multiple generator support (Ninja, Makefiles, etc.)
- Integrated testing with CTest
- Code formatting with clang-format
- Install targets for system integration

### IDE Support

The project generates `compile_commands.json` for clangd and other LSP servers. After building, copy it from the build directory:

```bash
cp build/compile_commands.json .
```

## Documentation

### Repository Documentation

- **[telnet-lisp/README.md](telnet-lisp/README.md)** - Complete library documentation with all features, API reference, and examples
- **[telnet-lisp/PACKAGING.md](telnet-lisp/PACKAGING.md)** - Integration guide for embedding telnet-lisp
- **[telnet-lisp/tests/README.md](telnet-lisp/tests/README.md)** - Test documentation
- **[telnet-gui/README.md](telnet-gui/README.md)** - GUI application documentation, libvterm integration notes

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Contributing

See the library documentation in `telnet-lisp/README.md` for API reference and examples. All projects in this repository integrate with the core **telnet-lisp** library.
