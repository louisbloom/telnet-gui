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

First, launch the UCRT64 shell to run pacman commands:

```bash
# Standard MSYS2 installation
C:\msys64\msys2_shell.cmd -defterm -here -no-start -ucrt64

# Or for Scoop installation
C:\Users\<username>\scoop\apps\msys2\current\msys2_shell.cmd -defterm -here -no-start -ucrt64
```

Then install dependencies:

```bash
# Core build dependencies (required for all projects)
pacman -S mingw-w64-ucrt-x86_64-{gcc,cmake,ninja,gc,pcre2}

# GUI project dependencies (optional, only for telnet-gui)
pacman -S mingw-w64-ucrt-x86_64-{SDL2,SDL2_ttf,libvterm}

# Development dependencies (optional, for code formatting)
pacman -S mingw-w64-ucrt-x86_64-clang-tools-extra  # Provides clang-format
```

**Note:** MSYS2 upgrades may remove installed packages. If packages are missing after an upgrade, reinstall them using the commands above.

**Linux:**

```bash
# Core build dependencies
sudo apt install build-essential cmake ninja-build pkg-config libgc-dev libpcre2-dev

# GUI project dependencies (optional)
sudo apt install libsdl2-dev libsdl2-ttf-dev
# libvterm: package may not be available, install from source (see telnet-gui/README.md)

# Development dependencies (optional, for code formatting)
sudo apt install clang-format
```

**macOS:**

```bash
# Core build dependencies
brew install cmake ninja pkg-config bdw-gc pcre2

# GUI project dependencies (optional)
brew install sdl2 sdl2_ttf
# libvterm: package may not be available, install from source (see telnet-gui/README.md)

# Development dependencies (optional, for code formatting)
brew install clang-format
```

### Building

Build all projects from the repository root:

**Using CMake Presets (recommended):**

```bash
# Configure and build
cmake --preset default
cmake --build build

# Or build specific targets
cmake --build build --target liblisp      # Core library only
cmake --build build --target lisp-repl    # REPL application
cmake --build build --target telnet-gui   # GUI application (requires GUI deps)
```

**Using Ninja directly:**

```bash
# Configure with Ninja generator
cmake -B build -G Ninja

# Build everything
cmake --build build

# Or build specific targets
cmake --build build --target liblisp      # Core library only
cmake --build build --target lisp-repl    # REPL application
cmake --build build --target telnet-gui   # GUI application (requires GUI deps)
```

**Notes for Windows:**

- CMake automatically detects and uses UCRT64 toolchain when available (Scoop or MSYS2 installation)
- Works from PowerShell, cmd, or MSYS2 shells without manual environment configuration
- If CMake defaults to NMake Makefiles, use `-G Ninja` or the CMake preset

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

### Installing

Install to system directories or custom location:

**Default installation (user-local, no admin privileges):**

```bash
# Install to ~/telnet-lisp (Windows) or ~/telnet-lisp (Linux/macOS)
cmake --build build
cmake --install build
```

**Custom installation prefix:**

```bash
# Install to ~/.local
cmake --install build --prefix ~/.local

# Or install to /usr/local (may require sudo on Linux/macOS)
cmake --install build --prefix /usr/local

# Or any custom location
cmake --install build --prefix /opt/telnet
```

**Installation structure (POSIX-compliant):**

```
$PREFIX/
├── bin/                     # Executables
│   ├── telnet-gui
│   ├── lisp-repl
│   └── telnet-gui-log2html
├── lib/                     # Libraries
│   └── liblisp.a
├── include/                 # Headers
│   ├── lisp.h
│   └── utf8.h
└── share/telnet-gui/        # Application data
    ├── fonts/               # Embedded fonts
    ├── lisp/                # Lisp scripts
    └── icons/               # Application icons
```

The application automatically finds resources in installed locations using runtime path resolution.

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
- libvterm (terminal emulation) - MSYS2 package or source build
- **telnet-lisp** (core library - included)

**Building:**

```bash
# Install GUI dependencies (MSYS2 UCRT64)
pacman -S mingw-w64-ucrt-x86_64-SDL2
pacman -S mingw-w64-ucrt-x86_64-SDL2_ttf
pacman -S mingw-w64-ucrt-x86_64-libvterm

# Build from repository root
cmake --preset default           # or: cmake -B build -G Ninja
cmake --build build --target telnet-gui
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
cmake --preset default              # or: cmake -B build -G Ninja
cmake --build build --target help
```

## Formatting Code

**IMPORTANT:** Always format code before committing:

```bash
cmake --build build --target format
```

This formats all sources (C, shell scripts, Lisp files, Markdown, Python) using:
- `clang-format` for C code
- `shfmt` for shell scripts
- `emacs` for Lisp files
- `prettier` for Markdown files
- `black` for Python files

**Troubleshooting:**

If the format target fails with "unknown target 'format'", install clang-format:

```bash
# Windows (MSYS2 UCRT64)
pacman -S mingw-w64-ucrt-x86_64-clang-tools-extra

# Linux
sudo apt install clang-format

# macOS
brew install clang-format

# Then reconfigure CMake
cmake -B build -G Ninja
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
- CMake Presets for simplified configuration (CMakePresets.json)
- Automatic UCRT64 toolchain detection on Windows
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
