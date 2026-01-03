# Telnet GUI

[![Build](https://github.com/louisbloom/telnet-gui/actions/workflows/build.yml/badge.svg)](https://github.com/louisbloom/telnet-gui/actions/workflows/build.yml)

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

- `-s, --font-size SIZE` - Set font size in points (default: 12)
- `-f, --font NAME` - Select font by name (system, cascadia, inconsolata, plex, dejavu, courier)
- `--hinting MODE` - Font hinting: none (default), light, normal, mono
- `--antialiasing MODE` - Anti-aliasing: nearest (default), linear

**Windows Font Options:**

- `--font-backend BACKEND` - Font rendering: directwrite (default), sdl
- `--cleartype` / `--no-cleartype` - ClearType subpixel rendering (default: off)

**Terminal Options:**

- `-g, --geometry COLSxROWS` - Set terminal size (default: 80x40)
- `--line-height HEIGHT` - Line height multiplier (default: 1.0)

**Other Options:**

- `-l, --lisp-file FILE` - Load Lisp configuration file. Supports relative paths (e.g., `contrib/practice.lisp`). Searches current directory first, then standard locations. Absolute paths are loaded directly.
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

Lottie animations can render behind terminal content. See [Animation System](#animation-system) in Lisp Scripting for complete documentation.

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

;; Cancel a timer using the timer object returned by run-at-time
(define keepalive (run-at-time 60 60 (lambda () (telnet-send "PING"))))
(cancel-timer keepalive)  ; => #t

;; Inspect active timers
(list-timers)
```

**Timer functions:**

| Function       | Arguments                      | Description                                                             |
| -------------- | ------------------------------ | ----------------------------------------------------------------------- |
| `run-at-time`  | `(delay repeat fn &rest args)` | Schedule fn after delay seconds; repeat if non-nil. Returns timer list. |
| `cancel-timer` | `(timer)`                      | Cancel a timer, returns `#t` if found                                   |
| `list-timers`  | `()`                           | Return list of active timers                                            |

**Timer object structure:**

`run-at-time` returns a list: `(id fire-time-ms repeat-ms callback args)`

- `id` - Timer ID (integer)
- `fire-time-ms` - When timer fires (milliseconds since epoch)
- `repeat-ms` - Repeat interval in ms (0 for one-shot)
- `callback` - Function to call
- `args` - Arguments list for callback

### Notification System

Temporary notifications displayed below the terminal divider:

```lisp
;; Show notification for 3 seconds
(notify "Connection established!" 3000)

;; Use default timeout (5 seconds)
(notify "New message from Djurden")

;; With ANSI colors
(notify "\033[32m✓ Connected\033[0m to server")

;; Clear current notification and queue
(notification-clear)
```

**Notification functions:**

| Function               | Arguments                     | Description                                  |
| ---------------------- | ----------------------------- | -------------------------------------------- |
| `notify`               | `(message &optional timeout)` | Display notification with auto-dismiss       |
| `notification-clear`   | `()`                          | Clear current notification and pending queue |
| `notification-active?` | `()`                          | Check if notification is showing             |

**Configuration:**

- `*notification-timeout*` - Default display time in milliseconds (5000)

Notifications queue automatically - if one is showing when another is requested, the new one displays after the current one expires.

### Animation System

Lottie animations render behind terminal content with configurable visibility modes:

```lisp
;; Load and play an animation
(define anim (animation-load "animations/fireworks.json"))
(animation-set-loop anim #t)  ; Enable looping
(animation-play anim)  ; Start playing and set as active

;; Control playback
(animation-pause anim)  ; Pause
(animation-play anim)   ; Resume
(animation-stop anim)   ; Stop and reset to beginning
(animation-seek anim 0.5)  ; Seek to middle (0.0 to 1.0)

;; Adjust speed
(animation-set-speed anim 2.0)  ; Double speed
(animation-set-speed anim 0.5)  ; Half speed

;; Visibility modes
(animation-set-dim-mode anim 0.7)  ; Dim mode with overlay (default 0.7)
(animation-set-transparent-mode anim 0.85)  ; Transparent terminal (default 0.85)

;; Query state
(animation-playing? anim)  ; => #t or nil
(animation-loaded? anim)   ; => #t or nil
(animation-position anim) ; => 0.0 to 1.0
(animation-duration anim)  ; => seconds

;; Set active animation (only one active at a time)
(animation-set-active anim)  ; Set as active
(animation-set-active nil)   ; Clear active animation
```

**Animation functions:**

| Function                         | Arguments           | Description                                                 |
| -------------------------------- | ------------------- | ----------------------------------------------------------- |
| `animation-load`                 | `(path)`            | Load Lottie animation file, returns animation object        |
| `animation-unload`               | `(anim)`            | Unload animation                                            |
| `animation-play`                 | `(anim)`            | Start playing and set as active background animation        |
| `animation-pause`                | `(anim)`            | Pause playback                                              |
| `animation-stop`                 | `(anim)`            | Stop and reset to beginning                                 |
| `animation-set-speed`            | `(anim multiplier)` | Set playback speed (1.0 = normal, 0.5 = half, 2.0 = double) |
| `animation-set-loop`             | `(anim [enabled])`  | Enable/disable looping (default #t)                         |
| `animation-seek`                 | `(anim position)`   | Seek to position (0.0 to 1.0)                               |
| `animation-playing?`             | `(anim)`            | Check if currently playing, returns #t or nil               |
| `animation-loaded?`              | `(anim)`            | Check if file is loaded, returns #t or nil                  |
| `animation-position`             | `(anim)`            | Get current position (0.0 to 1.0)                           |
| `animation-duration`             | `(anim)`            | Get duration in seconds                                     |
| `animation-set-dim-mode`         | `(anim [alpha])`    | Set dim mode with overlay (alpha 0.0-1.0, default 0.7)      |
| `animation-set-transparent-mode` | `(anim [alpha])`    | Set transparent mode (terminal alpha 0.0-1.0, default 0.85) |
| `animation-set-active`           | `(anim)`            | Set active animation (nil to clear)                         |

**Note:** Animation functions require rlottie support (optional dependency). Animations render behind terminal text and can be dimmed or made transparent for readability.

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
