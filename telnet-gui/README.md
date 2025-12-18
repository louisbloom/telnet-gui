# Telnet GUI

A graphical telnet client with terminal emulation, built using SDL2 and libvterm.

## Overview

The `telnet-gui` application provides a graphical interface for connecting to telnet servers. It features:

- **Terminal Emulation**: Full VT100-compatible terminal emulation using libvterm
- **Native Window Controls**: Standard OS titlebar with native close/minimize/maximize buttons and resize handles
- **Graphical Interface**: SDL2-based rendering with font caching and efficient screen updates
- **Input Handling**: Keyboard and mouse input support with selection and clipboard

## Dependencies

### Required Libraries

- **SDL2**: Graphics and windowing (`mingw-w64-ucrt-x86_64-SDL2`)
- **SDL2_ttf**: TrueType font rendering (`mingw-w64-ucrt-x86_64-SDL2_ttf`)
- **libvterm**: Terminal emulation library
  - **Distribution packages (≤0.3.3)**: Basic scrollback only, no text reflow
  - **Upstream source (current)**: Full text reflow support (recommended)
- **Boehm GC**: Garbage collector for Lisp backend (`mingw-w64-ucrt-x86_64-gc`)
- **PCRE2**: Regular expression support (`mingw-w64-ucrt-x86_64-pcre2`)

### Optional Libraries

- **rlottie**: Lottie animation support for background animations
  - Linux: `apt install librlottie-dev`
  - MSYS2: `pacman -S mingw-w64-ucrt-x86_64-rlottie`
  - macOS: `brew install rlottie`
  - If not installed, the build continues without animation support

### Installation (MSYS2 UCRT64)

#### libvterm Installation Priority

**Option 1: Build from Upstream Source (Recommended for full features)**

```bash
# Install other dependencies first
pacman -S mingw-w64-ucrt-x86_64-SDL2 \
          mingw-w64-ucrt-x86_64-SDL2_ttf \
          mingw-w64-ucrt-x86_64-gc \
          mingw-w64-ucrt-x86_64-pcre2

# Build and install libvterm to /usr/local (checked first by CMake)
git clone https://github.com/neovim/libvterm.git
cd libvterm
make
make install  # Installs to /usr/local
```

**Option 2: Use Distribution Package (Limited features)**

```bash
pacman -S mingw-w64-ucrt-x86_64-SDL2 \
          mingw-w64-ucrt-x86_64-SDL2_ttf \
          mingw-w64-ucrt-x86_64-libvterm \
          mingw-w64-ucrt-x86_64-gc \
          mingw-w64-ucrt-x86_64-pcre2
```

**Feature Detection:**

CMake automatically detects libvterm capabilities at build time:

- **Distribution packages (v0.3.3 and older)**: Basic scrollback only
- **Upstream source (post-2024)**: Full text reflow when terminal resizes
- **CMake priority**: Checks /usr/local first, then falls back to system packages

If CMake detects v0.3.3 or older, it will suggest building from upstream source.

## Build

### Debug Build (Default)

Debug builds stay attached to the console, making it easier to see debug output and error messages:

```bash
# Configure for Debug build (default)
cmake -B build -G Ninja

# Build telnet-gui
cmake --build build --target telnet-gui
```

The executable will be created at `build/telnet-gui/telnet-gui.exe`.

**Debug Build Behavior:**

- Console window remains visible
- Debug output and error messages appear in console
- Compiled with `-O0 -g` flags (no optimization, debug symbols included)
- Useful for development and troubleshooting

### Release Build

Release builds detach from the console, running as a pure GUI application:

```bash
# Configure for Release build
cmake -B build -G Ninja -DCMAKE_BUILD_TYPE=Release

# Build telnet-gui
cmake --build build --target telnet-gui
```

**Release Build Behavior:**

- No console window (pure GUI application)
- Optimized for performance (`-O2`)
- Suitable for end-user distribution

### Switching Between Build Types

To switch between Debug and Release builds, reconfigure CMake:

```bash
# Switch to Release
cmake -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build build --target telnet-gui

# Switch back to Debug
cmake -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug
cmake --build build --target telnet-gui
```

## Installation

### User Directory Installation

The project installs to your user directory by default (no administrator privileges required):

**Windows:**

- Default install location: `%USERPROFILE%\telnet-lisp\`
- Example: `C:\Users\YourName\telnet-lisp\`

**Unix/Linux/macOS:**

- Default install location: `~/telnet-lisp/`
- Example: `/home/yourname/telnet-lisp/`

### Installing the Application

From the project root:

```bash
# Build first (Debug or Release)
cmake -B build -G Ninja
cmake --build build --target telnet-gui

# Install to user directory
cmake --install build
```

This installs:

- **Executable**: `bin/telnet-gui.exe` (Windows) or `bin/telnet-gui` (Unix)
- **Lisp files**: `share/telnet-gui/lisp/` (bootstrap.lisp, tintin.lisp)
- **Fonts**: `share/telnet-gui/fonts/` (all bundled fonts)
- **Runtime DLLs**: `bin/` (Windows only: libpcre2-8-0.dll, libwinpthread-1.dll)

### Custom Install Location

To install to a different location:

```bash
# Configure with custom install prefix (CMake 3.21+)
cmake -B build -G Ninja --install-prefix /path/to/install

# Or use the traditional -D option (all CMake versions)
# cmake -B build -G Ninja -DCMAKE_INSTALL_PREFIX=/path/to/install

# Build and install
cmake --build build --target telnet-gui
cmake --install build
```

### Running Installed Application

After installation, you can run telnet-gui from the install directory:

**Windows:**

```cmd
%USERPROFILE%\telnet-lisp\bin\telnet-gui.exe <hostname> <port>
```

**Unix/Linux/macOS:**

```bash
~/telnet-lisp/bin/telnet-gui <hostname> <port>
```

Or add the `bin` directory to your PATH for system-wide access.

## Usage

### Basic Usage

```bash
./build/telnet-gui/telnet-gui.exe <hostname> <port>
```

Example:

```bash
./build/telnet-gui/telnet-gui.exe telnet-server 4449
```

### Command-Line Options

```bash
./build/telnet-gui/telnet-gui.exe [OPTIONS] [hostname] [port]
```

**Font Options:**

- `-f, --font-size SIZE` - Set font size in points (default: 12)
- `-F<letter>` - Select font (default: s for System monospace font):
  - `s` = System monospace font (best for platform), `m` = Cascadia Mono, `i` = Inconsolata, `p` = IBM Plex Mono, `d` = DejaVu Sans Mono, `c` = Courier Prime
- `--font <name>` - Select font by name (system, cascadia, inconsolata, plex, dejavu, courier)
- `-H, --hinting MODE` - Font hinting mode: none (default), light, normal, mono
- `-a, --antialiasing MODE` - Anti-aliasing mode: linear (default), nearest

**Terminal Options:**

- `-g, --geometry COLSxROWS` - Set terminal size (default: 80x40)
- `-L, --line-height HEIGHT` - Set line height multiplier (default: 1.2, range: 0.5 to 3.0)

**Other Options:**

- `-l, --lisp-file FILE` - Load Lisp configuration file on startup (can be specified multiple times)
- `-t, --test FILE` - Run a Lisp test file in headless mode and exit (returns 0 on success)
- `--debug-exit` - Exit after initialization (for capturing debug output)
- `-h, --help` - Show help message

**Examples:**

```bash
# Start in unconnected mode
./build/telnet-gui/telnet-gui.exe

# Connect with larger font (uses default System monospace font)
./build/telnet-gui/telnet-gui.exe -f 20 telnet-server 4449

# Use Inconsolata font
./build/telnet-gui/telnet-gui.exe -Fi telnet-server 4449

# Use IBM Plex Mono font with custom geometry
./build/telnet-gui/telnet-gui.exe -Fp -g 100x40 telnet-server 4449

# Use DejaVu Sans Mono font with long form
./build/telnet-gui/telnet-gui.exe --font dejavu telnet-server 4449

# Use System font with custom line height
./build/telnet-gui/telnet-gui.exe --font system -L 1.5 telnet-server 4449

# Load custom Lisp configuration (word completion is built-in via bootstrap.lisp)
./build/telnet-gui/telnet-gui.exe -l custom-config.lisp telnet-server 4449

# Debug mode (exits after first render, capture initialization output)
./build/telnet-gui/telnet-gui.exe --debug-exit 2>&1
```

### Bundled Fonts

The application includes five monospace fonts optimized for terminal use, plus uses the system monospace font by default:

- **System Monospace Font** (default) - Uses the platform's native monospace font (best compatibility and rendering)
- **DejaVu Sans Mono** - Bitstream Vera derivative with excellent Unicode coverage. [Bitstream Vera License]
- **Cascadia Mono** - Microsoft's programming font designed specifically for terminal applications. Clean rendering without ligatures. [SIL OFL 1.1]
- **Inconsolata** - Compact monospace font with excellent readability. [SIL OFL 1.1]
- **IBM Plex Mono** - IBM's corporate font family, part of the Plex typeface system. [SIL OFL 1.1]
- **Courier Prime** - Modern interpretation of Courier, designed for screenwriting. [SIL OFL 1.1]

All bundled fonts are licensed under permissive open-source licenses that allow redistribution.

## Built-in Features

### Word Completion

The telnet-gui includes automatic word completion powered by Lisp scripting:

**Features:**

- **Automatic word collection** - Words from server output are automatically collected
- **Tab completion** - Press TAB to complete words based on prefix
- **Case-insensitive matching** - Matches words regardless of case
- **Smart filtering** - Only words >= 3 characters are stored
- **Bounded storage** - Circular buffer with FIFO eviction (configurable, default 10,000 words)
- **Newest-first results** - Most recently seen words appear first in completions

**Usage:**

1. Connect to a server and let it send output
2. Type a partial word (e.g., "hel")
3. Press TAB to see completions
4. Use arrow keys to select, Enter to accept, Escape to cancel

**Configuration:**

Word completion is configured in `bootstrap.lisp` (automatically loaded). You can customize:

```lisp
;; Store size (max words to keep)
(define *completion-word-store-size* 10000)

;; Max results to show per prefix
(define *completion-max-results* 64)

;; Pattern to match for completion (regex)
(define *completion-pattern* "\\S+$")
```

To disable completion, override in your custom config:

```bash
# Create disable-completion.lisp
echo '(defun completion-hook (text) ())' > disable-completion.lisp
echo '(defun telnet-input-hook (text) ())' >> disable-completion.lisp

# Load it
./build/telnet-gui/telnet-gui.exe -l disable-completion.lisp server 4449
```

### Input Area Modes

- **Normal mode** - Input sent directly to telnet server (press Ctrl+E to toggle)
- **Eval mode** - Input evaluated as Lisp code (press Ctrl+E to toggle)

### Colon Commands

Available in Normal mode (commands start with `:`):

- `:help` - Show available commands
- `:connect <server> <port>` or `:connect <server>:<port>` - Connect to server
- `:disconnect` - Disconnect from server
- `:load <filepath>` - Load and execute a Lisp file
- `:test <filepath>` - Run a Lisp test file
- `:repl <code>` - Evaluate Lisp code and show result
- `:quit` - Exit application

### TinTin++ Scripting

The telnet-gui supports TinTin++ scripting features including aliases, variables, speedwalking, and command chaining.

**Quick Start:**

```bash
./build/telnet-gui/telnet-gui.exe -l telnet-gui/tintin.lisp <host> <port>
```

**Features:**

- Command aliases with pattern matching (`#alias {k} {kill %1}`)
- Variables with substitution (`#variable {target} {orc}`, then `kill $target`)
- Speedwalk notation (`3n2e` expands to `n;n;n;e;e`)
- Command separation with semicolons (`north;get gold;south`)
- Save/load configuration (`#save {config.lisp}`, `#load {config.lisp}`)

**Full documentation:** See [TINTIN.md](TINTIN.md) for complete usage guide, examples, and implementation details

### Background Animations (Lottie)

The telnet-gui supports Lottie vector animations rendered behind the terminal text. This feature requires the optional rlottie library.

**Features:**

- **Lottie JSON format** - Industry standard from Airbnb/After Effects
- **Background rendering** - Animation renders behind terminal text
- **Lisp scripting control** - Full playback control from Lisp
- **Visibility modes** - Configurable dim overlay or transparent terminal backgrounds
- **Thousands of free animations** - Available at [lottiefiles.com](https://lottiefiles.com)

**Basic Usage:**

```lisp
;; Load and play an animation
(animation-load "path/to/animation.json")
(animation-set-loop t)
(animation-play)
```

**Visibility Modes:**

```lisp
;; Option 1: Dim mode (default) - dark overlay on animation
(animation-set-dim-mode 0.7)  ; 70% overlay opacity (30% animation visible)

;; Option 2: Transparent mode - see-through terminal backgrounds
(animation-set-transparent-mode 0.85)  ; 85% terminal background opacity
```

**Playback Control:**

```lisp
(animation-play)           ; Start playback
(animation-pause)          ; Pause
(animation-stop)           ; Stop and reset to beginning
(animation-set-speed 0.5)  ; Half speed (range: 0.0-10.0)
(animation-set-loop t)     ; Enable looping
(animation-seek 0.5)       ; Jump to middle (0.0-1.0)
```

**State Queries:**

```lisp
(animation-playing?)   ; => t or nil
(animation-loaded?)    ; => t or nil
(animation-position)   ; => 0.0-1.0 (current position)
(animation-duration)   ; => seconds (total duration)
```

**Sample Animation:**

A sample fireworks animation is included in `animations/fireworks.json`. Try it with:

```lisp
(animation-load "animations/fireworks.json")
(animation-set-loop t)
(animation-play)
(animation-set-dim-mode 0.8)  ; Adjust visibility as needed
```

**Note:** Animation support is optional. If rlottie is not installed, the animation functions will not be available and the build continues normally.

## Testing Lisp Configuration Files

The telnet-gui directory contains Lisp scripts that can be loaded and tested independently:

### Test Files

- **tintin-test.lisp** - Test suite for TinTin++ scripting features
- **completion-test.lisp** - Test suite for tab completion

### Test Format

Test files use inline expectations:

```lisp
(expression)        ; => expected-result
(define x 10)       ; ignore
(+ x 5)             ; => 15
```

- `; => value` - Expression should evaluate to this value
- `; ignore` - Expression is evaluated but result not checked (used for setup)

### Running Tests

**Option 1: Using test_runner.sh (automated validation)**

The test runner automatically validates output against expected results:

```bash
# From telnet-lisp directory
./telnet-lisp/test_runner.sh telnet-gui/tintin-test.lisp

# Or from build directory
cd build
../telnet-lisp/test_runner.sh ../telnet-gui/tintin-test.lisp

# Verbose output
VERBOSE=1 ../telnet-lisp/test_runner.sh ../telnet-gui/tintin-test.lisp
```

Output:

```bash
PASS: tintin-test (outputs match)
```

**Option 2: Using lisp-repl directly (manual verification)**

Run the test file and manually check output:

```bash
# From project root
./build/lisp-repl telnet-gui/tintin-test.lisp
./build/lisp-repl telnet-gui/completion-test.lisp

# Or from build directory
cd build
./lisp-repl ../telnet-gui/tintin-test.lisp
```

Output:

```bash
#<lambda:terminal-echo>
#<lambda:telnet-send>
("cmd1" "cmd2")
"s;s;w;w;w;w;w"
...
```

Compare the output with `; => expected` comments in the test file.

### Creating Your Own Tests

Create a `.lisp` file with test expressions:

```lisp
;; My test file
(load "tintin.lisp")  ; ignore

;; Test alias creation
(tintin-process-command "#alias {n} {north}")  ; ignore
(tintin-process-command "n")                   ; => "north"
```

Run with test_runner.sh:

```bash
./telnet-lisp/test_runner.sh my-test.lisp
```

Or manually with lisp-repl:

```bash
./build/lisp-repl my-test.lisp
```

## libvterm Integration

### Initialization

When working with libvterm 0.3.x or later, follow this initialization order:

#### Initialization Order

1. Create VTerm: `vterm_new(rows, cols)`
2. **Set UTF-8 mode**: `vterm_set_utf8(vterm, 1)` **BEFORE getting screen**
3. (Optional) Set output callback: `vterm_output_set_callback(vterm, callback, user_data)` - for capturing terminal output
4. Get screen: `vterm_obtain_screen(vterm)` (this creates state internally)
5. Initialize callbacks structure (stored in persistent struct, see below)
6. Set screen callbacks: `vterm_screen_set_callbacks(screen, &callbacks, user_data)`
7. (Optional) For text reflow support: `vterm_screen_callbacks_has_pushline4(screen)` and `vterm_screen_enable_reflow(screen, 1)`
8. Reset screen: `vterm_screen_reset(screen, 1)` (this resets state internally and initializes encoding arrays)

**Note:** The code follows this order, but some steps are optional (output callback, text reflow features). The order ensures proper initialization of internal state.

#### Callback Structure Lifetime

**Best Practice:** Store `VTermScreenCallbacks` in a persistent struct (e.g., Terminal struct), not as a local variable.

This ensures the callbacks structure remains valid for the lifetime of the VTerm instance. While libvterm may copy the structure internally, storing it in your state struct is a safe practice that avoids potential lifetime issues.

**Correct Pattern:**

```c
struct Terminal {
    VTerm *vterm;
    VTermScreen *screen;
    VTermScreenCallbacks callbacks;  // ← Store here, not as local variable
    int rows, cols;
    int needs_redraw;
    // ... other fields
};

Terminal *terminal_create(int rows, int cols) {
    Terminal *term = malloc(sizeof(Terminal));
    if (!term)
        return NULL;

    // Create VTerm instance
    term->vterm = vterm_new(rows, cols);
    if (!term->vterm) {
        free(term);
        return NULL;
    }

    // Set UTF-8 mode BEFORE getting screen
    vterm_set_utf8(term->vterm, 1);

    // (Optional) Set output callback for capturing terminal output
    // vterm_output_set_callback(term->vterm, output_callback, term);

    // Get screen (creates state internally)
    term->screen = vterm_obtain_screen(term->vterm);
    if (!term->screen) {
        vterm_free(term->vterm);
        free(term);
        return NULL;
    }

    // Initialize callbacks to zero first
    memset(&term->callbacks, 0, sizeof(term->callbacks));

    // Set callback functions
    term->callbacks.damage = damage_callback;
    term->callbacks.movecursor = movecursor_callback;
    term->callbacks.settermprop = settermprop_callback;
    term->callbacks.bell = bell_callback;
    term->callbacks.resize = resize_callback;
    // Scrollback callbacks (required for scrollback support)
    term->callbacks.sb_pushline = sb_pushline_callback;
    term->callbacks.sb_popline = sb_popline_callback;
    term->callbacks.sb_clear = sb_clear_callback;
    // Optional: sb_pushline4 for text reflow (if available)
    // term->callbacks.sb_pushline4 = sb_pushline4_callback;

    // Set callbacks (callbacks struct stays in scope as long as Terminal exists)
    vterm_screen_set_callbacks(term->screen, &term->callbacks, term);

    // (Optional) Enable text reflow if pushline4 is available
    // vterm_screen_callbacks_has_pushline4(term->screen);
    // vterm_screen_enable_reflow(term->screen, 1);

    // Reset screen (initializes encoding arrays)
    vterm_screen_reset(term->screen, 1);

    return term;
}
```

**Alternative Pattern (may work, but not recommended):**

```c
Terminal *terminal_create(int rows, int cols) {
    Terminal *term = malloc(sizeof(Terminal));

    // ⚠️ Local variable - may work if libvterm copies the structure
    // but storing in struct is safer and clearer
    VTermScreenCallbacks callbacks = {0};
    callbacks.damage = damage_callback;
    // ...

    vterm_screen_set_callbacks(term->screen, &callbacks, term);
    // If libvterm stores a pointer, this would cause issues
    // If libvterm copies, this works but is less clear

    return term;
}
```

#### Color Handling

Use the `VTERM_COLOR_IS_*` macros to check color types:

- `VTERM_COLOR_IS_RGB(&cell.bg)` - check if RGB color
- `VTERM_COLOR_IS_INDEXED(&cell.bg)` - check if indexed color
- `VTERM_COLOR_IS_DEFAULT_FG(&cell.fg)` - check if default foreground
- `VTERM_COLOR_IS_DEFAULT_BG(&cell.bg)` - check if default background

**Example from actual implementation:**

```c
static void vterm_color_to_term_color(const VTermColor *vcolor, GenericColor *tcolor) {
    if (VTERM_COLOR_IS_DEFAULT_FG(vcolor) || VTERM_COLOR_IS_DEFAULT_BG(vcolor)) {
        tcolor->type = TERM_COLOR_DEFAULT;
    } else if (VTERM_COLOR_IS_INDEXED(vcolor)) {
        tcolor->type = TERM_COLOR_INDEXED;
        tcolor->color.idx = vcolor->indexed.idx;
    } else if (VTERM_COLOR_IS_RGB(vcolor)) {
        tcolor->type = TERM_COLOR_RGB;
        tcolor->color.rgb.r = vcolor->rgb.red;
        tcolor->color.rgb.g = vcolor->rgb.green;
        tcolor->color.rgb.b = vcolor->rgb.blue;
    } else {
        tcolor->type = TERM_COLOR_DEFAULT;
    }
}
```

**Note:** The implementation uses the screen layer and doesn't access state directly. The screen layer handles color conversion automatically.

#### Version Checking

The implementation requires libvterm 0.3.x or later:

```c
#include <vterm.h>

// Check for 0.3.x (returns 1 if version >= 0.3.0, 0 otherwise)
vterm_check_version(0, 3);
```

**Note:** The actual implementation calls `vterm_check_version(0, 3)` but doesn't check the return value. For production code, you should check the return value and handle version mismatches appropriately.

### References

- libvterm documentation: https://github.com/neovim/libvterm
- libvterm demo code: https://github.com/neovim/libvterm/blob/master/demo/main.c
- libvterm source: https://github.com/neovim/libvterm/tree/master/src

## Architecture

### Components

- **main.c**: Application entry point, SDL initialization, event loop
- **terminal.c**: Terminal emulation wrapper with backend abstraction
- **terminal_backend.h**: Terminal backend interface (function pointer vtable)
- **terminal_backend_vterm.c**: libvterm terminal backend implementation
- **term_cell.h**: Generic terminal cell structure (backend-independent)
- **ansi_sequences.h/c**: ANSI escape sequence constants and helpers (universal, reusable)
- **renderer.c**: Renderer wrapper that delegates to backend
- **renderer_backend.h**: Renderer backend interface (function pointer vtable)
- **renderer_backend_sdl.c**: SDL2 renderer backend implementation (permanent)
- **box_drawing.c**: Pixel-perfect box drawing character rendering
- **glyph_cache.c**: Font glyph caching for performance
- **window.c**: SDL window management (simplified - delegates to OS for chrome)
- **input.c**: Keyboard and mouse input handling
- **input_area.c**: Command input area with history and editing
- **telnet.c**: Telnet protocol client implementation
- **commands.c**: Slash command processor (/help, /connect, /disconnect, /test)
- **lisp.c**: Lisp scripting integration (hooks, colors, configuration)
- **animation.c**: Lottie animation support via rlottie (optional, requires rlottie library)

### Backend Architecture

The application uses a pluggable backend architecture that separates terminal emulation and rendering concerns:

**Terminal Backend (terminal_backend.h, terminal_backend_vterm.c):**

- Abstracts terminal emulation layer using function pointer vtables
- Current implementation: libvterm backend (only backend currently available)
- Provides: cell access, scrollback, viewport management, data feeding
- Runtime backend selection via `terminal_create_with_backend()` (currently only "vterm" supported)
- Enables future alternative terminal emulators without breaking existing code

**ANSI Sequences (ansi_sequences.h, ansi_sequences.c):**

- Static ANSI escape sequence table compatible with xterm/screen/linux
- Symbolic constants for readability: `ANSI_CURSOR_SAVE`, `ANSI_SGR_REVERSE`, etc.
- Helper functions for parameterized sequences: cursor positioning, scroll regions, colors
- **Universal design**: Reusable for both GUI mode (libvterm control) and future custom TUI rendering
- No terminfo dependencies, cross-platform compatibility

**Renderer Backend (renderer_backend.h, renderer_backend_sdl.c):**

- Abstracts rendering layer using function pointer vtables
- Current implementation: SDL2 backend (only backend currently available)
- Provides: frame lifecycle, cell rendering, color handling
- Backend is selected at compile-time (hardcoded to SDL2 in `renderer_create()`)
- **Future work**: Custom TUI renderer backend for terminal-based output
  - Would require adding `renderer_create_with_backend()` function
  - Would use ANSI escape sequences from ansi_sequences.h for rendering
  - Custom implementation (no termios/ncurses/Windows Console API)
  - Would enable dual-mode operation: GUI (SDL2) or TUI (custom ANSI renderer)
  - Would reuse existing terminal backend (libvterm) for VT100 parsing

**Generic Types (term_cell.h):**

- `TermCell` structure: backend-independent cell representation
- Contains: UTF-32 characters, text attributes (bold/italic/underline), RGB/indexed/default colors
- Used as common interface between terminal and renderer backends

This architecture enables:

- Clean separation of concerns
- Easy testing and mocking
- Code reusability between backends
- Future dual-mode operation (when renderer backend selection is implemented)

### Terminal Emulation Flow

**Telnet Input (Server → Terminal):**

1. Data arrives via telnet connection
2. Lisp `telnet-input-filter-hook` transforms data (optional)
3. `terminal_feed_data()` writes to `vterm_input_write()`
4. libvterm parses ANSI escape sequences
5. Screen callbacks are triggered (damage, movecursor, etc.)
6. Lisp `telnet-input-hook` collects words for completion (default)
7. `renderer_render()` iterates through damaged cells
8. SDL renders glyphs to screen

**User Input (Terminal → Telnet):**

1. User types in input area or presses keys
2. Lisp `user-input-hook` transforms input (optional)
3. Input sent to telnet connection
4. Local echo to terminal (if enabled)

### Lisp Scripting System

**Bootstrap Process:**

1. `bootstrap.lisp` is automatically loaded on startup from build directory
2. Sets up word completion, hooks, colors, and default configuration
3. Optional user config file loaded via `-l` flag (overrides defaults)

**Available Hooks:**

- `completion-hook` - Called for TAB completion (default: word store lookup)
- `telnet-input-hook` - Called on server output (default: collect words)
- `telnet-input-filter-hook` - Transform server output before display (default: pass-through)
- `user-input-hook` - Transform user input before sending (default: pass-through)
- `mode-render-hook` - Render custom mode display (default: emoji symbols)

**Configuration Variables:**

- Completion: `*completion-word-store-size*`, `*completion-max-results*`, `*completion-pattern*`
- Scrolling: `*scroll-lines-per-click*`, `*smooth-scrolling-enabled*`, `*max-scrollback-lines*`
- Colors: `*terminal-fg-color*`, `*terminal-bg-color*`, `*input-area-fg-color*`, etc.
- History: `*input-history-size*`
- Mode: `*connection-mode*`, `*input-mode*`, `*mode*`

All variables can be customized in user config files loaded with `-l`.

### Memory Management

- Terminal struct uses standard `malloc()`/`free()` (not Boehm GC)
- Lisp objects use Boehm GC (automatic memory management)
- libvterm manages its own internal memory
- SDL handles graphics memory

## Troubleshooting

### Segmentation Fault on Startup

If you get a segmentation fault:

1. **Check callback structure lifetime**: Ensure `VTermScreenCallbacks` is stored in Terminal struct, not as local variable
2. **Verify initialization order**: UTF-8 mode → get screen → set callbacks → reset screen
3. **Check libvterm version**: Must be 0.3.3 or later
4. **Verify encoding initialization**: State encoding arrays are initialized during `vterm_screen_reset()`

### Colors Not Rendering Correctly

- Ensure you're using `VTERM_COLOR_IS_*` macros, not direct type checks
- Convert indexed colors to RGB before rendering
- Initialize default colors to safe fallback values

### Build Issues

- Ensure all dependencies are UCRT64-compatible (no MINGW64 or MSYS)
- Check that libvterm is installed (MSYS2 package in `/ucrt64` or source build in `/usr/local`)
- Verify `PKG_CONFIG_PATH` includes libvterm's pkgconfig directory (CMakeLists.txt auto-detects both locations)

## Development

### Debugging

#### Quick Debug Mode

Use `--debug-exit` to capture initialization output without manually killing the process:

**From MSYS2 UCRT64 shell (recommended - DLLs automatically available):**

```bash
cd build/telnet-gui
./telnet-gui.exe --debug-exit 2>&1
```

**From Git Bash (requires PATH configuration):**

```bash
export PATH="/c/msys64/ucrt64/bin:$PATH"
cd build/telnet-gui
./telnet-gui.exe --debug-exit 2>&1
```

**From PowerShell:**

```powershell
$env:PATH = "C:\msys64\ucrt64\bin;$env:PATH"
cd build\telnet-gui
.\telnet-gui.exe --debug-exit 2>&1
```

**From CMD:**

```cmd
set PATH=C:\msys64\ucrt64\bin;%PATH%
cd build\telnet-gui
telnet-gui.exe --debug-exit 2>&1
```

The `--debug-exit` flag makes telnet-gui exit after the first frame is rendered, which is useful for:

- Verifying font loading and emoji support
- Checking mode area rendering and sizing
- Capturing bootstrap and initialization logs
- Testing Lisp configuration without running the full application

**Note:** The PATH must include the MSYS2 UCRT64 bin directory for SDL2 DLLs. Adjust paths above if your MSYS2 is installed elsewhere (e.g., `C:\tools\msys64`, scoop installations at `C:\Users\<username>\scoop\apps\msys2\...`, etc.).

#### GDB Debugging

Use GDB to debug segmentation faults and runtime issues (requires MSYS2 UCRT64 shell or GDB with proper PATH):

**From MSYS2 UCRT64 shell:**

```bash
cd build/telnet-gui
gdb ./telnet-gui.exe

# Run with arguments
(gdb) run telnet-server 4449

# Or run with debug exit
(gdb) run --debug-exit

# After crash
(gdb) bt              # Print backtrace
(gdb) info registers  # Show register values
(gdb) frame 0         # Select stack frame
(gdb) print variable  # Inspect variable values
```

**Common debugging scenarios:**

- Segfault on startup → Check callback structure lifetime (see libvterm integration above)
- Segfault during rendering → Check glyph cache and texture creation
- Connection issues → Check telnet protocol negotiation

#### Debug Output

The application prints debug information to stderr:

- Bootstrap file resolution paths
- Font loading and fallback attempts
- Emoji font detection
- libvterm initialization
- Mode area rendering (when using `--debug-exit`)

Redirect stderr to capture logs:

**Git Bash / MSYS2:**

```bash
./telnet-gui.exe 2>debug.log
./telnet-gui.exe --debug-exit 2>&1 | tee debug.log
```

**PowerShell:**

```powershell
.\telnet-gui.exe 2>&1 | Out-File debug.log
.\telnet-gui.exe --debug-exit 2>&1 | Tee-Object debug.log
```

**CMD:**

```cmd
telnet-gui.exe 2>debug.log
```

### Code Style

- C99 standard
- K&R braces style
- 4-space indentation
- Unix line endings (LF, not CRLF)

### Code Formatting

The project uses clang-format for consistent code style. To use the format target:

1. **Ensure clang-format is in PATH before running cmake** (during configuration):

   ```bash
   # MSYS2 UCRT64 environment
   export PATH="/ucrt64/bin:$PATH"
   # Or if not running from MSYS2 shell:
   export PATH="/c/msys64/ucrt64/bin:$PATH"
   ```

2. **Configure CMake** (it will detect clang-format and create the format target):

   ```bash
   cd build
   cmake ..
   ```

3. **Format all sources**:

   ```bash
   cmake --build . --target format
   ```

**Note:** If the format target doesn't exist (`ninja: error: unknown target 'format'`), clang-format wasn't found during CMake configuration. Add clang-format to PATH and reconfigure CMake.

**Alternative:** Format files manually with clang-format:

```bash
clang-format -i src/*.c src/*.h
```

## Documentation

Additional technical documentation is available in the `docs/` directory:

- **[ansi-escape-sequences.md](docs/ansi-escape-sequences.md)** - Reference for ANSI/VT100 escape sequences
  - Cursor control (CUP, CUU, CUD, CUF, CUB)
  - Scrolling regions (DECSTBM)
  - Screen manipulation (ED, EL)
  - Text attributes and colors (SGR)
  - Implementation notes and common pitfalls

- **[terminal-emulation.md](docs/terminal-emulation.md)** - Terminal emulation concepts and libvterm usage
  - libvterm architecture and initialization
  - Scrolling region implementation
  - Cursor management (separate vterm and input area cursors)
  - Screen layout and row indexing
  - Color handling and conversion
  - Scrollback buffer management
