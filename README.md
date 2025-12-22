# Telnet GUI

A graphical telnet client with terminal emulation, built using SDL2 and libvterm, powered by an embedded Lisp interpreter for scripting and automation.

## Features

- **Terminal Emulation**: Modern xterm-compatible terminal with true color (24-bit), 256-color palette, UTF-8, text attributes, scrollback, text selection, and copy/paste
- **Word Completion**: TAB completes words from server output
- **Input History**: Navigate previous commands with up/down arrows
- **TinTin++ Scripting**: Aliases, variables, speedwalking, and command chaining for MUDs
- **Background Animations**: Lottie animations behind terminal content
- **Emoji Support**: Color emoji rendering via system fonts
- **Multiple Fonts**: Bundled monospace fonts with configurable size and line height
- **Native Controls**: Standard OS titlebar with close/minimize/maximize buttons

## Quick Start

```bash
# Connect to a server
./telnet-gui <hostname> <port>

# Example
./telnet-gui mud.example.com 4000

# Start in unconnected mode
./telnet-gui
```

See [Building from Source](#building-from-source) for installation instructions.

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

- `-l, --lisp-file FILE` - Load Lisp configuration file. Supports relative paths (e.g., `contrib/practice.lisp`). Searches current directory first, then standard locations. Absolute paths are loaded directly.
- `-t, --test FILE` - Run Lisp test file in headless mode
- `-h, --help` - Show help message

### Built-in Features

**Word Completion:** Press TAB to complete words from server output.

**Input Modes** (Shift+Tab to toggle):

- Normal mode - Input sent to telnet server
- Eval mode - Input evaluated as Lisp code

**Divider Mode Indicators:**

The divider line between terminal and input area displays status indicators:

- ⚡ - Eval mode active (Lisp evaluation)
- ▶️ - Animation playing

**Colon Commands:**

- `:help` - Show available commands
- `:connect <server> <port>` - Connect to server
- `:disconnect` - Disconnect from server
- `:load <filepath>` - Load Lisp file
- `:repl <code>` - Evaluate Lisp code and show result
- `:quit` - Exit application

**TinTin++ Scripting:**

```bash
# Load TinTin++ scripting support (searches lisp/ automatically)
./telnet-gui -l tintin.lisp <host> <port>

# Load contrib scripts (e.g., practice mode for MUDs)
./telnet-gui -l contrib/practice.lisp <host> <port>

# Load multiple files
./telnet-gui -l tintin.lisp -l contrib/practice.lisp <host> <port>
```

Features aliases, variables, speedwalking, and command chaining. See [TINTIN.md](TINTIN.md) for full documentation.

**Lisp File Loading:**

The `-l` flag searches for files in this order:

1. Current working directory (PWD)
2. Build/development paths (`lisp/` subdirectory)
3. Installed data directory

Relative paths like `contrib/practice.lisp` are resolved the same way as simple filenames like `tintin.lisp`. Absolute paths (e.g., `C:\Users\...\file.lisp` on Windows, `/home/user/file.lisp` on Unix) are loaded directly without searching.

**Emoji Support:**

The terminal supports color emoji rendering via system emoji fonts (e.g., NotoColorEmoji). Most emoji render correctly at 2-cell width, but some emoji with variation selectors (like 🗡️ ⚔️ ▶️) need an extra space after them to prevent overlap.

**Background Animations:**

```lisp
(define anim (animation-load "animations/fireworks.json"))
(animation-set-loop anim #t)  ; #t enables looping
(animation-play anim)  ; automatically sets as active
```

## Platform Support

- **Windows**: MSYS2 UCRT64 (GCC) - requires UCRT64, not MINGW64/MSVC
- **Linux**: GCC with standard libraries
- **macOS**: Clang/GCC with Homebrew

## Documentation

- **[TINTIN.md](TINTIN.md)** - TinTin++ scripting guide
- **[KNOWN_ISSUES.md](KNOWN_ISSUES.md)** - Known bugs and limitations
- **[telnet-lisp/README.md](telnet-lisp/README.md)** - Lisp language documentation
- **[telnet-lisp/LANGUAGE_REFERENCE.md](telnet-lisp/LANGUAGE_REFERENCE.md)** - Complete language specification
- **[docs/](docs/)** - Technical documentation (ANSI sequences, terminal emulation)

## Lisp Scripting

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

### Hook System

Telnet GUI uses an Emacs-style hook system for extensibility. Hooks allow multiple handler functions to respond to events:

```lisp
;; Add a handler to a hook
(add-hook 'telnet-input-hook
  (lambda (text)
    (princ "[LOG] ")
    (princ text)))

;; Remove a handler (must be the same function object)
(define my-handler (lambda (text) (princ text)))
(add-hook 'telnet-input-hook my-handler)
(remove-hook 'telnet-input-hook my-handler)

;; Run all handlers in a hook
(run-hook 'telnet-input-hook "Hello from server")
```

**Built-in hooks:**

| Hook                       | Arguments           | Description                                                                             |
| -------------------------- | ------------------- | --------------------------------------------------------------------------------------- |
| `telnet-input-hook`        | `(text)`            | Called when data received from server (ANSI stripped). Side-effect only.                |
| `telnet-input-filter-hook` | `(text)`            | Transform telnet output before display (includes ANSI codes). Returns transformed text. |
| `user-input-hook`          | `(text cursor-pos)` | Transform user input before sending to server. Returns transformed text or `nil`.       |
| `completion-hook`          | `(text)`            | Provide tab completion candidates. Returns list of completion strings.                  |

**Default handlers on `telnet-input-hook`:**

- Word collection for tab completion
- Scroll-lock notification animation

### Divider Mode Indicators

Indicators are controllable from Lisp:

```lisp
;; Add a custom indicator (priority controls order, lower = leftmost)
(divider-mode-set 'custom "★" 50)

;; Remove an indicator
(divider-mode-remove 'custom)

;; View current modes
*divider-modes*
;; => ((10 . (eval . "⚡")) (50 . (custom . "★")) (90 . (animation . "▶️")))
```

### Timer System

Emacs-style timers for scheduling Lisp code:

```lisp
;; Run once after 5 seconds
(run-at-time 5 nil (lambda () (terminal-echo "Hello!\r\n")))

;; Run every 60 seconds (first run after 60s)
(define keepalive (run-at-time 60 60 (lambda () (telnet-send "PING"))))

;; Cancel a timer
(cancel-timer keepalive)

;; Pass arguments to callback
(run-at-time 10 nil (lambda (msg) (terminal-echo msg)) "Delayed!\r\n")

;; Cancel all timers for a function
(defun my-ping () (telnet-send "PING"))
(run-at-time 30 30 my-ping)
(run-at-time 60 60 my-ping)
(cancel-function-timers my-ping)  ; => 2

;; Inspect active timers
(list-timers)
```

**Timer functions:**

| Function                                   | Description                                        |
| ------------------------------------------ | -------------------------------------------------- |
| `(run-at-time delay repeat fn &rest args)` | Schedule fn after delay seconds; repeat if non-nil |
| `(cancel-timer timer)`                     | Cancel a timer, returns `#t` if found              |
| `(cancel-function-timers fn)`              | Cancel all timers for fn, returns count            |
| `(list-timers)`                            | Return list of active timers                       |

## Building from Source

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

### Dependencies

**Required:**

- **SDL2**: Graphics and windowing
- **SDL2_ttf**: TrueType font rendering
- **libvterm**: Terminal emulation (source build recommended for text reflow)
- **Boehm GC**: Garbage collector
- **PCRE2**: Regular expressions

**Optional:**

- **rlottie**: Lottie animation support

**libvterm Installation:**

Distribution packages (v0.3.3) provide basic scrollback only. Source build (nvim branch) enables full text reflow support:

```bash
# Use the install script (recommended)
./scripts/install-libvterm.sh

# Or manually
git clone -b nvim https://github.com/neovim/libvterm.git
cd libvterm && make && make install PREFIX=/usr/local
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

### Installation

```bash
# Install to ~/telnet-gui (default)
cmake --install build

# Or custom location
cmake --install build --prefix ~/.local
```

### Build Targets

- `telnet-gui` - GUI application
- `lisp-repl` - Interactive Lisp REPL
- `lisp` - Static Lisp library
- `format` - Format all source files
- `test` - Run test suite

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
│   └── contrib/          # Contrib scripts (practice, seaport, etc.)
├── fonts/                # Bundled fonts
├── animations/           # Sample Lottie animations
├── icons/                # Application icons
├── scripts/              # Install scripts
├── docs/                 # Technical documentation
├── TINTIN.md             # TinTin++ scripting guide
├── KNOWN_ISSUES.md       # Known bugs and limitations
└── CMakeLists.txt        # Build configuration
```

## License

MIT License - see [LICENSE](LICENSE) file for details.
