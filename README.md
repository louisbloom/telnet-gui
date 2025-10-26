# Telnet Lisp Interpreter

A minimal, embeddable Lisp interpreter library written in C, designed for integration into GUI applications. Features automatic garbage collection with Boehm GC, powerful regex support with PCRE2, and a comprehensive REPL for testing and demonstration.

## Project Structure

```
telnet-lisp/
├── libtelnet-lisp/    # Self-contained library
│   ├── include/       # Public headers (lisp.h, utf8.h)
│   ├── src/          # Library source files
│   ├── repl/         # REPL application
│   ├── tests/        # Test suite
│   ├── CMakeLists.txt
│   ├── README.md     # Full library documentation
│   └── PACKAGING.md  # Integration guide
├── telnet-gui/       # GUI application (SDL2, libvterm)
│   ├── src/          # GUI source files
│   └── CMakeLists.txt
├── CMakeLists.txt    # Root build configuration
└── build/            # Build directory (generated)
```

## Quick Start

### Prerequisites

**Windows (MSYS2 UCRT64):**

```bash
# Install dependencies
pacman -S mingw-w64-ucrt-x86_64-gcc mingw-w64-ucrt-x86_64-cmake
pacman -S mingw-w64-ucrt-x86_64-bdw-gc mingw-w64-ucrt-x86_64-pcre2
```

**Linux:**

```bash
# Ubuntu/Debian
sudo apt install build-essential cmake pkg-config libgc-dev libpcre2-dev

# Fedora
sudo dnf install gcc cmake pkg-config gc-devel pcre2-devel
```

**macOS:**

```bash
# Install dependencies via Homebrew
brew install cmake pkg-config bdw-gc pcre2
```

### Building

```bash
# Create build directory
mkdir build && cd build

# Configure CMake
cmake .. -G Ninja

# Build everything
cmake --build .

# Or build specific targets
cmake --build . --target lisp-repl
cmake --build . --target telnet-gui
```

**Alternative with MinGW Makefiles:**

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

### Building and Running Tests

```bash
cd build
cmake .. -G Ninja
cmake --build .
ctest
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

## Project Components

### libtelnet-lisp Library

The core Lisp interpreter library - see [`libtelnet-lisp/README.md`](libtelnet-lisp/README.md) for complete documentation.

**Features:**
- Numbers, Strings, Lists, Vectors, Hash Tables
- Lexical scoping with lambdas
- Pattern matching and regex (PCRE2)
- Automatic garbage collection (Boehm GC)
- UTF-8 support

### REPL Application

Interactive Lisp shell for experimentation. Run with:

```bash
./build/lisp-repl
```

**Commands:**
- `:quit` - Exit the REPL
- `:load <file>` - Load and execute a Lisp file

### Telnet GUI Application

Modern GUI client with SDL2 and libvterm for terminal emulation.

**Dependencies:**
- SDL2 (windowing and rendering)
- SDL2_ttf (font rendering)
- libvterm (terminal emulation)

**Building:**

```bash
# Install GUI dependencies (MSYS2)
pacman -S mingw-w64-ucrt-x86_64-SDL2
pacman -S mingw-w64-ucrt-x86_64-SDL2_ttf
pacman -S mingw-w64-ucrt-x86_64-libvterm

# Build
cd build
cmake --build . --target telnet-gui
```

## CMake Targets

- `lisp-repl` - REPL application (default)
- `liblisp` - Static library
- `telnet-gui` - GUI application
- `format` - Format all source files
- `test` - Run test suite
- `install` - Install to system directories

View all targets:

```bash
cd build
cmake .. -G Ninja
cmake --build . --target help
```

## Using the Library

### Quick Integration

The library is designed to be easily embedded. See [`libtelnet-lisp/PACKAGING.md`](libtelnet-lisp/PACKAGING.md) for detailed integration instructions.

**Simple integration:**

```c
#include "libtelnet-lisp/include/lisp.h"

int main() {
    lisp_init();  // Initializes GC automatically
    Environment* env = env_create_global();

    LispObject* result = lisp_eval_string("(+ 1 2)", env);
    char* output = lisp_print(result);
    printf("%s\n", output);  // Outputs: 3

    env_free(env);
    lisp_cleanup();
    return 0;
}
```

**Linking with CMake:**

```cmake
add_subdirectory(libtelnet-lisp)
target_link_libraries(myapp liblisp)
target_include_directories(myapp PRIVATE libtelnet-lisp/include)
```

## Documentation

- **[libtelnet-lisp/README.md](libtelnet-lisp/README.md)** - Complete library documentation
- **[libtelnet-lisp/PACKAGING.md](libtelnet-lisp/PACKAGING.md)** - Integration guide for embedding
- **[libtelnet-lisp/tests/README.md](libtelnet-lisp/tests/README.md)** - Test documentation

## Features

### Data Types
- Numbers (integers and floats), Booleans (#t, #f)
- Strings, Lists, Vectors, Hash Tables
- Symbols, Lambdas, Closures

### Control Flow
- `quote`, `if`, `define`, `set!`
- `lambda`, `let`, `let*`
- `progn`, `do`, `cond`, `case`

### Functions
- Arithmetic, comparisons, string operations
- List operations (car, cdr, cons, list)
- Regex with PCRE2 (match, find, extract, replace)
- Vector operations (ref, set!, push!, pop!)
- Hash table operations (ref, set!, remove!)

See [`libtelnet-lisp/README.md`](libtelnet-lisp/README.md) for the complete API reference.

## Platform Support

- **Windows**: MSYS2 UCRT64 (GCC)
- **Linux**: GCC with standard libraries
- **macOS**: Clang/GCC with Homebrew

## Development

### Project Layout

- `libtelnet-lisp/` - Self-contained library (can be embedded independently)
- `telnet-gui/` - GUI application using the library
- `build/` - Generated build artifacts

### Build System

The project uses **CMake** as the primary build system, providing:

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

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Contributing

See the library documentation in `libtelnet-lisp/README.md` for API reference and examples.
