# Terminal Emulation with libvterm

This document explains terminal emulation concepts and how telnet-gui uses libvterm for VT100/ANSI terminal emulation.

## Table of Contents

- [Terminal Emulation Basics](#terminal-emulation-basics)
- [libvterm Architecture](#libvterm-architecture)
- [Initialization Sequence](#initialization-sequence)
- [Scrolling Regions](#scrolling-regions)
- [Cursor Management](#cursor-management)
- [Screen Layout](#screen-layout)
- [Color Handling](#color-handling)

## Terminal Emulation Basics

A terminal emulator interprets ANSI escape sequences and maintains a screen buffer of cells. Each cell contains:

- Character(s) (UTF-8 support)
- Foreground color
- Background color
- Text attributes (bold, italic, underline, etc.)

### Data Flow

```
User Input → telnet-gui → Telnet Server
                ↓
        ANSI Sequences → libvterm → Screen Buffer → SDL Renderer
```

**Components**:

1. **Input**: User types text in input area
2. **Processing**: Text sent to telnet server
3. **Response**: Server sends ANSI-formatted text
4. **Parsing**: libvterm parses ANSI sequences and updates screen buffer
5. **Rendering**: SDL reads screen buffer and renders to window

## libvterm Architecture

libvterm provides three layers:

### 1. VTerm (Bottom Layer)

Raw terminal state and input/output handling.

```c
VTerm *vterm = vterm_new(rows, cols);
vterm_set_utf8(vterm, 1);  // Enable UTF-8
```

### 2. VTermState (Middle Layer)

Interprets escape sequences and maintains cursor position, attributes, etc.

```c
VTermState *state = vterm_obtain_state(vterm);
vterm_state_get_cursorpos(state, &cursorpos);
```

### 3. VTermScreen (Top Layer)

High-level screen buffer with damage tracking and scrollback.

```c
VTermScreen *screen = vterm_obtain_screen(vterm);
vterm_screen_set_callbacks(screen, &callbacks, user_data);
```

**telnet-gui uses VTermScreen** for its high-level interface and scrollback support.

## Initialization Sequence

**CRITICAL**: Initialization order matters! Incorrect order causes segfaults.

### Correct Initialization Order

```c
// 1. Create VTerm
VTerm *vterm = vterm_new(rows, cols);
if (!vterm) return NULL;

// 2. Enable UTF-8 BEFORE obtaining screen
vterm_set_utf8(vterm, 1);

// 3. Set output callback (for user input)
vterm_output_set_callback(vterm, output_callback, user_data);

// 4. Obtain screen
VTermScreen *screen = vterm_obtain_screen(vterm);
if (!screen) {
    vterm_free(vterm);
    return NULL;
}

// 5. Set up callbacks (MUST store in struct, not local variable!)
VTermScreenCallbacks callbacks = {0};
callbacks.damage = damage_callback;
callbacks.movecursor = movecursor_callback;
callbacks.sb_pushline = sb_pushline_callback;
// ... more callbacks

// 6. Register callbacks
vterm_screen_set_callbacks(screen, &callbacks, user_data);

// 7. Enable optional features
#if HAVE_VTERM_PUSHLINE4
vterm_screen_callbacks_has_pushline4(screen);
#endif

#if HAVE_VTERM_REFLOW
vterm_screen_enable_reflow(screen, 1);
#endif

// 8. Reset screen
vterm_screen_reset(screen, 1);

// 9. Set scrolling region via ANSI sequence
char seq[32];
snprintf(seq, sizeof(seq), "\033[1;%dr", rows);
vterm_input_write(vterm, seq, strlen(seq));
vterm_screen_flush_damage(screen);
```

### Common Mistakes

**❌ BAD: Callbacks in local variable**

```c
void init() {
    VTermScreenCallbacks callbacks = {0};  // Local variable!
    callbacks.damage = damage_callback;
    vterm_screen_set_callbacks(screen, &callbacks, user_data);
    // callbacks goes out of scope → SEGFAULT
}
```

**✅ GOOD: Callbacks in struct**

```c
typedef struct {
    VTerm *vterm;
    VTermScreen *screen;
    VTermScreenCallbacks callbacks;  // Persistent storage
} TerminalState;

void init(TerminalState *state) {
    memset(&state->callbacks, 0, sizeof(state->callbacks));
    state->callbacks.damage = damage_callback;
    vterm_screen_set_callbacks(state->screen, &state->callbacks, state);
}
```

## Scrolling Regions

### Concept

A scrolling region defines which lines are affected by scroll operations. Lines outside the region are "protected" from scrolling.

### DECSTBM - Set Top and Bottom Margins

```c
// Set scrolling region to rows 1-40
char seq[32];
snprintf(seq, sizeof(seq), "\033[1;40r");
vterm_input_write(vterm, seq, strlen(seq));
```

**Side Effect**: DECSTBM moves cursor to (1,1)!

### telnet-gui Layout

```
┌────────────────────────────────────┐
│ Row 1: Terminal output             │
│ Row 2: Terminal output             │
│  ...                               │
│ Row 40: Terminal output            │  ← Scrolling Region (rows 1-40)
├────────────────────────────────────┤  ← Row 41: Separator
│ Row 42: Input area (user types)   │  ← Protected (outside scrolling region)
└────────────────────────────────────┘
```

**Benefits**:

- Server output scrolls in rows 1-40
- Input area (row 42) never gets overwritten by scrolling
- Separator (row 41) stays fixed

## Cursor Management

### Two Cursor Positions

telnet-gui maintains **two logical cursors**:

1. **VTerm Cursor**: Position in terminal buffer (for output)
2. **Input Area Cursor**: Position in input text (for typing)

### Why Two Cursors?

**Problem**: If vterm cursor is in input area (row 42):

- Server output via `vterm_input_write()` goes to row 42
- Input area gets overwritten by terminal output

**Solution**:

1. Keep vterm cursor at row 40 (bottom of scrolling region)
2. Render input area cursor separately in SDL

### Implementation

```c
// After rendering input area text
char seq[1024];
int len = 0;

// 1. Write input area text and separator
len += snprintf(seq + len, sizeof(seq) - len, "\033[41;1H\033[K");
len += snprintf(seq + len, sizeof(seq) - len, "────────");  // Separator

len += snprintf(seq + len, sizeof(seq) - len, "\033[42;1H\033[K");
len += snprintf(seq + len, sizeof(seq) - len, "%s", input_text);

// 2. Set scrolling region (cursor moves to 1,1)
len += snprintf(seq + len, sizeof(seq) - len, "\033[1;40r");

// 3. Move cursor to row 40 (so output goes there)
len += snprintf(seq + len, sizeof(seq) - len, "\033[40;1H");

vterm_input_write(vterm, seq, len);

// 4. Render input area cursor in SDL separately
// See renderer.c: render_cursor() called at row 42, col input_cursor_pos
```

### Cursor Tracking

VTerm provides a `movecursor` callback to track cursor changes:

```c
static int movecursor_callback(VTermPos pos, VTermPos oldpos,
                               int visible, void *user) {
    TerminalState *state = (TerminalState *)user;

    // Track position for SDL rendering
    state->cursor_row = pos.row;
    state->cursor_col = pos.col;
    state->cursor_visible = visible;

    state->needs_redraw = 1;
    return 1;
}
```

## Screen Layout

### Virtual Terminal Size

telnet-gui uses a **larger vterm** than the visible scrolling area:

```
User requests: 40 rows × 80 cols

VTerm created: 42 rows × 80 cols
                ↑
                Additional 2 rows for separator + input
```

### Row Indexing

**0-indexed** (C arrays):

- Row 0-39: Scrolling region (terminal output)
- Row 40: Separator
- Row 41: Input area

**1-indexed** (ANSI sequences):

- Row 1-40: Scrolling region
- Row 41: Separator
- Row 42: Input area

### Example

```c
int scrolling_rows = 40;  // User-visible rows

// Create vterm with extra rows
int total_rows = scrolling_rows + 2;  // 42 total
VTerm *vterm = vterm_new(total_rows, cols);

// Set scrolling region (1-indexed for ANSI)
char seq[32];
snprintf(seq, sizeof(seq), "\033[1;%dr", scrolling_rows);  // "\033[1;40r"
vterm_input_write(vterm, seq, strlen(seq));

// Position cursor at input area (1-indexed)
snprintf(seq, sizeof(seq), "\033[%d;1H", scrolling_rows + 2);  // "\033[42;1H"
vterm_input_write(vterm, seq, strlen(seq));
```

## Color Handling

### libvterm Color Types

libvterm represents colors with `VTermColor`:

```c
typedef struct {
    uint8_t type;
    union {
        uint8_t idx;         // For indexed colors
        struct {
            uint8_t r, g, b;  // For RGB colors
        } rgb;
    };
} VTermColor;
```

**Types**:

- `VTERM_COLOR_DEFAULT_FG` - Default foreground
- `VTERM_COLOR_DEFAULT_BG` - Default background
- `VTERM_COLOR_INDEXED` - Palette index (0-255)
- `VTERM_COLOR_RGB` - True color (24-bit RGB)

### Color Detection

**❌ WRONG**: Never check `color.type` directly

```c
if (cell.fg.type == VTERM_COLOR_INDEXED) {  // BAD! Undefined behavior
    // ...
}
```

**✅ CORRECT**: Use libvterm macros

```c
if (VTERM_COLOR_IS_INDEXED(&cell.fg)) {
    uint8_t index = cell.fg.indexed.idx;
    // Convert to RGB...
}
else if (VTERM_COLOR_IS_RGB(&cell.fg)) {
    uint8_t r = cell.fg.rgb.red;
    uint8_t g = cell.fg.rgb.green;
    uint8_t b = cell.fg.rgb.blue;
}
else if (VTERM_COLOR_IS_DEFAULT_FG(&cell.fg)) {
    // Use terminal default foreground
}
```

### Color Conversion

Convert indexed colors to RGB using vterm:

```c
VTermColor vc;
vc.type = VTERM_COLOR_INDEXED;
vc.indexed.idx = cell.fg.color.idx;

// Convert to RGB
vterm_screen_convert_color_to_rgb(screen, &vc);

// Now use RGB values
uint8_t r = vc.rgb.red;
uint8_t g = vc.rgb.green;
uint8_t b = vc.rgb.blue;
```

## Scrollback Management

### Scrollback Callbacks

libvterm provides callbacks for scrollback management:

```c
typedef struct {
    // Called when a line scrolls off top of screen
    int (*sb_pushline)(int cols, const VTermScreenCell *cells, void *user);

    // Called when scrolling back brings a line back
    int (*sb_popline)(int cols, VTermScreenCell *cells, void *user);

    // Called when scrollback should be cleared
    int (*sb_clear)(void *user);

    // Extended version with line continuation info (libvterm 0.3.3+)
#if HAVE_VTERM_PUSHLINE4
    int (*sb_pushline4)(int cols, const VTermScreenCell *cells,
                       bool continuation, void *user);
#endif
} VTermScreenCallbacks;
```

### Scrollback Storage

Store scrollback in a circular buffer:

```c
typedef struct {
    VTermScreenCell *cells;  // Cell array
    int cols;                // Column count when stored
    bool continuation;       // Is this a wrapped line?
} ScrollbackLine;

typedef struct {
    ScrollbackLine *scrollback;
    int scrollback_size;      // Number of lines stored
    int scrollback_start;     // Circular buffer start index
    int scrollback_capacity;  // Buffer capacity
    int max_scrollback_lines; // Maximum allowed (0 = unlimited)
} TerminalState;
```

### Viewport Management

Track viewport offset to implement scrolling:

```c
typedef struct {
    int viewport_offset;  // 0 = showing live screen, N = scrolled back N lines
} TerminalState;

// Scroll up (view older content)
void scroll_up(TerminalState *state, int lines) {
    state->viewport_offset += lines;
    if (state->viewport_offset > state->scrollback_size)
        state->viewport_offset = state->scrollback_size;
}

// Scroll down (view newer content)
void scroll_down(TerminalState *state, int lines) {
    state->viewport_offset -= lines;
    if (state->viewport_offset < 0)
        state->viewport_offset = 0;
}
```

## References

- libvterm source: https://github.com/neovim/libvterm
- libvterm demo: https://github.com/neovim/libvterm/blob/master/demo/main.c
- VT100 User Guide: https://vt100.net/docs/vt100-ug/
- ANSI Escape Sequences: See `ansi-escape-sequences.md`

## Implementation Files

See these source files for examples:

- `telnet-gui/src/terminal_backend_vterm.c` - libvterm backend implementation
- `telnet-gui/src/terminal.c` - Backend-independent terminal wrapper
- `telnet-gui/src/renderer.c` - Screen rendering with cursor handling
