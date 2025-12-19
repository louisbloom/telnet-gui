# Telnet GUI

A graphical telnet client with terminal emulation, built using SDL2 and libvterm, powered by an embedded Lisp interpreter for scripting and automation.

## Overview

Telnet GUI provides a feature-rich graphical interface for telnet connections with:

- **Terminal Emulation**: Full VT100-compatible terminal emulation using libvterm
- **Lisp Scripting**: Embedded Lisp interpreter for automation, scripting, and configuration
- **Native Window Controls**: Standard OS titlebar with native close/minimize/maximize buttons
- **Graphical Interface**: SDL2-based rendering with font caching and efficient screen updates
- **TinTin++ Support**: MUD client scripting with aliases, variables, and speedwalking
- **Lottie Animations**: Optional background animations via rlottie

## Quick Start

### Prerequisites

**Windows (MSYS2 UCRT64):**

First, launch the UCRT64 shell:

```bash
# Standard MSYS2 installation
C:\msys64\msys2_shell.cmd -defterm -here -no-start -ucrt64

# Or for Scoop installation
C:\Users\<username>\scoop\apps\msys2\current\msys2_shell.cmd -defterm -here -no-start -ucrt64
```

Then install dependencies:

```bash
# All dependencies
pacman -S mingw-w64-ucrt-x86_64-{gcc,cmake,ninja,gc,pcre2,SDL2,SDL2_ttf} libtool

# Install libvterm from source (recommended for full features)
./scripts/install-libvterm.sh

# Optional: Install rlottie for Lottie animations
./scripts/install-rlottie.sh
```

**Linux:**

```bash
# Core dependencies
sudo apt install build-essential cmake ninja-build pkg-config libgc-dev libpcre2-dev libsdl2-dev libsdl2-ttf-dev

# libvterm: install from source for full features
./scripts/install-libvterm.sh

# Optional: rlottie
sudo apt install librlottie-dev
```

**macOS:**

```bash
# Core dependencies
brew install cmake ninja pkg-config bdw-gc pcre2 sdl2 sdl2_ttf

# libvterm: install from source for full features
./scripts/install-libvterm.sh

# Optional: rlottie
brew install rlottie
```

### Building

```bash
# Configure and build
cmake -B build -G Ninja
cmake --build build

# Or build specific targets
cmake --build build --target telnet-gui   # GUI application
cmake --build build --target lisp-repl    # Lisp REPL
```

### Running

```bash
# Connect to a server
./build/telnet-gui <hostname> <port>

# Example
./build/telnet-gui telnet-server 4449

# Start in unconnected mode
./build/telnet-gui
```

### Installation

```bash
# Install to ~/telnet-gui (default)
cmake --install build

# Or custom location
cmake --install build --prefix ~/.local
```

## Usage

### Command-Line Options

```bash
./telnet-gui [OPTIONS] [hostname] [port]
```

**Font Options:**

- `-f, --font-size SIZE` - Set font size in points (default: 12)
- `-F<letter>` - Select font: s=System, m=Cascadia, i=Inconsolata, p=Plex, d=DejaVu, c=Courier
- `--font <name>` - Select font by name
- `-H, --hinting MODE` - Font hinting: none (default), light, normal, mono
- `-a, --antialiasing MODE` - Anti-aliasing: linear (default), nearest

**Terminal Options:**

- `-g, --geometry COLSxROWS` - Set terminal size (default: 80x40)
- `-L, --line-height HEIGHT` - Line height multiplier (default: 1.2)

**Other Options:**

- `-l, --lisp-file FILE` - Load Lisp configuration file
- `-t, --test FILE` - Run Lisp test file in headless mode
- `-h, --help` - Show help message

### Built-in Features

**Word Completion:** Press TAB to complete words from server output.

**Input Modes** (Shift+Tab to toggle):

- Normal mode - Input sent to telnet server
- Eval mode - Input evaluated as Lisp code

**Colon Commands:**

- `:help` - Show available commands
- `:connect <server> <port>` - Connect to server
- `:disconnect` - Disconnect from server
- `:load <filepath>` - Load Lisp file
- `:repl <code>` - Evaluate Lisp code and show result
- `:quit` - Exit application

**TinTin++ Scripting:**

```bash
./telnet-gui -l lisp/tintin.lisp <host> <port>
```

Features aliases, variables, speedwalking, and command chaining. See [TINTIN.md](TINTIN.md) for full documentation.

**Background Animations:**

```lisp
(define anim (animation-load "animations/fireworks.json"))
(animation-set-loop anim t)
(animation-play anim)  ; automatically sets as active
```

## Embedded Lisp Interpreter

Telnet GUI includes **telnet-lisp**, a complete embeddable Lisp interpreter with:

- Lexical scoping, closures, and first-class functions
- Rich data types: numbers, strings (UTF-8), lists, vectors, hash tables
- PCRE2 regex support with Unicode
- Automatic memory management with Boehm GC
- 150+ built-in functions

For complete language documentation, see **[telnet-lisp/README.md](telnet-lisp/README.md)**.

### Interactive REPL

```bash
./build/telnet-lisp/lisp-repl

>>> (+ 1 2 3)
6
>>> (define square (lambda (x) (* x x)))
#<lambda>
>>> (square 5)
25
>>> :quit
```

## Dependencies

### Required

- **SDL2**: Graphics and windowing
- **SDL2_ttf**: TrueType font rendering
- **libvterm**: Terminal emulation (source build recommended for text reflow)
- **Boehm GC**: Garbage collector
- **PCRE2**: Regular expressions

### Optional

- **rlottie**: Lottie animation support

### libvterm Installation

**Distribution packages (v0.3.3)**: Basic scrollback only
**Source build (nvim branch)**: Full text reflow support

```bash
# Use the install script (recommended)
./scripts/install-libvterm.sh

# Or manually
git clone -b nvim https://github.com/neovim/libvterm.git
cd libvterm && make && make install PREFIX=/usr/local
```

## Repository Structure

```
telnet-gui/
├── telnet-lisp/          # Embedded Lisp interpreter
│   ├── src/              # Interpreter source
│   ├── include/          # Public headers
│   ├── repl/             # REPL application
│   ├── tests/            # Test suite
│   └── README.md         # Language documentation
├── src/                  # GUI source files
├── lisp/                 # Lisp scripts (bootstrap, tintin)
├── fonts/                # Bundled fonts
├── animations/           # Sample Lottie animations
├── icons/                # Application icons
├── scripts/              # Install scripts
├── docs/                 # Technical documentation
├── TINTIN.md             # TinTin++ scripting guide
├── KNOWN_ISSUES.md       # Known bugs and limitations
└── CMakeLists.txt        # Build configuration
```

## Build Targets

- `telnet-gui` - GUI application
- `lisp-repl` - Interactive Lisp REPL
- `lisp` - Static Lisp library
- `format` - Format all source files
- `test` - Run test suite

## Platform Support

- **Windows**: MSYS2 UCRT64 (GCC) - requires UCRT64, not MINGW64/MSVC
- **Linux**: GCC with standard libraries
- **macOS**: Clang/GCC with Homebrew

## Documentation

- **[telnet-lisp/README.md](telnet-lisp/README.md)** - Lisp language documentation
- **[telnet-lisp/LANGUAGE_REFERENCE.md](telnet-lisp/LANGUAGE_REFERENCE.md)** - Complete language specification
- **[TINTIN.md](TINTIN.md)** - TinTin++ scripting guide
- **[KNOWN_ISSUES.md](KNOWN_ISSUES.md)** - Known bugs and limitations
- **[docs/](docs/)** - Technical documentation (ANSI sequences, terminal emulation)

## Development

### Code Formatting

```bash
cmake --build build --target format
```

Formats C, shell, Lisp, Markdown, and Python files.

### Testing

```bash
cd build && ctest
```

### IDE Support

The build generates `compile_commands.json` for clangd:

```bash
cp build/compile_commands.json .
```

## License

MIT License - see [LICENSE](LICENSE) file for details.
