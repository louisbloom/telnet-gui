# ANSI Escape Sequences Reference

This document provides a reference for ANSI escape sequences used in the telnet-gui terminal emulator.

## Table of Contents

- [Introduction](#introduction)
- [Cursor Control](#cursor-control)
- [Scrolling Region](#scrolling-region)
- [Screen Manipulation](#screen-manipulation)
- [Text Attributes](#text-attributes)
- [Colors](#colors)
- [Box Drawing Characters](#box-drawing-characters)

## Introduction

ANSI escape sequences are special character sequences that control cursor position, color, and other terminal features. They begin with the ESC character (ASCII 27, `\033` in C).

**Format**: `ESC [ <parameters> <command>`

Where:

- `ESC` is the escape character (`\033`)
- `[` is the Control Sequence Introducer (CSI)
- `<parameters>` are numeric parameters separated by semicolons
- `<command>` is a single letter indicating the operation

**Example**: `\033[2J` - Clear entire screen

## Cursor Control

### CUP - Cursor Position

**Sequence**: `ESC [ row ; col H`
**C Format**: `\033[%d;%dH`

Moves cursor to specified position (1-indexed).

**Examples**:

- `\033[1;1H` - Move to top-left (home position)
- `\033[10;20H` - Move to row 10, column 20

**Notes**:

- Row and column are 1-indexed (row 1 = first row)
- Omitting parameters defaults to `\033[1;1H`

### CUU - Cursor Up

**Sequence**: `ESC [ n A`
**C Format**: `\033[%dA`

Moves cursor up `n` lines (default: 1).

### CUD - Cursor Down

**Sequence**: `ESC [ n B`
**C Format**: `\033[%dB`

Moves cursor down `n` lines (default: 1).

### CUF - Cursor Forward

**Sequence**: `ESC [ n C`
**C Format**: `\033[%dC`

Moves cursor forward (right) `n` columns (default: 1).

### CUB - Cursor Backward

**Sequence**: `ESC [ n D`
**C Format**: `\033[%dD`

Moves cursor backward (left) `n` columns (default: 1).

### DECSC - Save Cursor

**Sequence**: `ESC 7`
**C Format**: `\0337`

Saves the current cursor position, character attributes, and character set selections.

**Use Case**: Save cursor before drawing UI elements (like input area), then restore to continue output.

### DECRC - Restore Cursor

**Sequence**: `ESC 8`
**C Format**: `\0338`

Restores the cursor position, character attributes, and character set selections previously saved with DECSC.

**Example**:

```c
// Save cursor, draw status line, restore cursor
char seq[256];
int len = 0;

len += snprintf(seq + len, sizeof(seq) - len, "\0337");      // Save cursor
len += snprintf(seq + len, sizeof(seq) - len, "\033[1;1H");  // Move to top-left
len += snprintf(seq + len, sizeof(seq) - len, "Status");     // Draw status
len += snprintf(seq + len, sizeof(seq) - len, "\0338");      // Restore cursor

vterm_input_write(vterm, seq, len);
```

## Scrolling Region

### DECSTBM - Set Top and Bottom Margins

**Sequence**: `ESC [ top ; bottom r`
**C Format**: `\033[%d;%dr`

Sets the scrolling region (margins) for the terminal.

**Parameters**:

- `top` - Top margin line number (1-indexed)
- `bottom` - Bottom margin line number (1-indexed)

**Behavior**:

1. **Scrolling Region**: Defines which lines are affected by scrolling operations
2. **Text Placement**: Text can be written anywhere on screen, but scrolling only occurs within margins
3. **Cursor Movement**: **IMPORTANT**: DECSTBM moves cursor to home position (1,1) as a side effect
4. **Protected Area**: Lines outside the margins are not affected by scroll operations

**Example**:

```c
// Set scrolling region to rows 1-40 (protects rows 41-42 for input area)
char seq[32];
snprintf(seq, sizeof(seq), "\033[1;%dr", 40);
vterm_input_write(vterm, seq, strlen(seq));

// IMPORTANT: Cursor is now at (1,1)!
// If you need cursor elsewhere, reposition it AFTER setting margins:
snprintf(seq, sizeof(seq), "\033[42;1H");  // Move to row 42
vterm_input_write(vterm, seq, strlen(seq));
```

**Usage in telnet-gui**:

- Scrolling region: rows 1-40 (terminal output)
- Protected area: rows 41-42 (separator + input area)
- After DECSTBM, cursor is repositioned to row 40 so new output doesn't overwrite input area

**Reference**: VT510 Programmer Reference Manual, DECSTBM command

## Screen Manipulation

### ED - Erase in Display

**Sequence**: `ESC [ n J`
**C Format**: `\033[%dJ`

Clears part or all of the screen.

**Parameters**:

- `0` (or omitted) - Clear from cursor to end of screen
- `1` - Clear from cursor to beginning of screen
- `2` - Clear entire screen (cursor position unchanged)
- `3` - Clear entire screen and scrollback buffer (xterm extension)

**Examples**:

- `\033[2J` - Clear entire screen
- `\033[J` - Clear from cursor to end

### EL - Erase in Line

**Sequence**: `ESC [ n K`
**C Format**: `\033[%dK`

Clears part or all of current line.

**Parameters**:

- `0` (or omitted) - Clear from cursor to end of line
- `1` - Clear from cursor to beginning of line
- `2` - Clear entire line

**Examples**:

- `\033[K` - Clear to end of line
- `\033[2K` - Clear entire line

### IL - Insert Lines

**Sequence**: `ESC [ n L`
**C Format**: `\033[%dL`

Inserts `n` blank lines at cursor position (default: 1).

### DL - Delete Lines

**Sequence**: `ESC [ n M`
**C Format**: `\033[%dM`

Deletes `n` lines at cursor position (default: 1).

## Text Attributes

### SGR - Select Graphic Rendition

**Sequence**: `ESC [ n m`
**C Format**: `\033[%dm`

Sets text attributes (color, bold, etc.). Multiple attributes can be combined with semicolons.

**Common Attributes**:

- `0` - Reset all attributes to default
- `1` - Bold
- `2` - Dim/faint
- `3` - Italic
- `4` - Underline
- `5` - Blink (slow)
- `7` - Reverse video (invert fg/bg)
- `8` - Concealed/hidden
- `9` - Strikethrough
- `22` - Normal intensity (not bold/dim)
- `23` - Not italic
- `24` - Not underlined
- `25` - Not blinking
- `27` - Not reversed
- `28` - Not concealed
- `29` - Not strikethrough

**Examples**:

```c
// Bold text
printf("\033[1mBold\033[0m");

// Bold + underline
printf("\033[1;4mBold Underline\033[0m");

// Reverse video (for selection highlighting)
printf("\033[7mSelected\033[27m");
```

## Colors

### Foreground Colors (30-37, 90-97)

**Standard Colors (30-37)**:

- `30` - Black
- `31` - Red
- `32` - Green
- `33` - Yellow
- `34` - Blue
- `35` - Magenta
- `36` - Cyan
- `37` - White

**Bright Colors (90-97)**:

- `90` - Bright Black (Gray)
- `91` - Bright Red
- `92` - Bright Green
- `93` - Bright Yellow
- `94` - Bright Blue
- `95` - Bright Magenta
- `96` - Bright Cyan
- `97` - Bright White

### Background Colors (40-47, 100-107)

**Standard Colors (40-47)**:

- `40` - Black background
- `41` - Red background
- `42` - Green background
- `43` - Yellow background
- `44` - Blue background
- `45` - Magenta background
- `46` - Cyan background
- `47` - White background

**Bright Colors (100-107)**:

- `100` - Bright Black (Gray) background
- `101` - Bright Red background
- `102` - Bright Green background
- `103` - Bright Yellow background
- `104` - Bright Blue background
- `105` - Bright Magenta background
- `106` - Bright Cyan background
- `107` - Bright White background

### 256 Color Mode

**Foreground**: `ESC [ 38 ; 5 ; n m`
**Background**: `ESC [ 48 ; 5 ; n m`

Where `n` is a color index (0-255):

- 0-15: Standard and bright colors
- 16-231: 6×6×6 RGB cube
- 232-255: Grayscale ramp

**Examples**:

```c
// Foreground color 196 (bright red)
printf("\033[38;5;196mBright Red\033[0m");

// Background color 21 (blue)
printf("\033[48;5;21mBlue Background\033[0m");
```

### True Color (24-bit RGB)

**Foreground**: `ESC [ 38 ; 2 ; r ; g ; b m`
**Background**: `ESC [ 48 ; 2 ; r ; g ; b m`

Where `r`, `g`, `b` are RGB values (0-255).

**Examples**:

```c
// Orange foreground (255, 165, 0)
printf("\033[38;2;255;165;0mOrange\033[0m");

// Purple background (128, 0, 128)
printf("\033[48;2;128;0;128mPurple BG\033[0m");
```

### Default Colors

- `39` - Default foreground color
- `49` - Default background color

## Box Drawing Characters

Box drawing characters are Unicode characters used to draw borders and tables.

**Common Box Drawing Characters**:

- `\u2500` - Horizontal line (─)
- `\u2502` - Vertical line (│)
- `\u250C` - Top-left corner (┌)
- `\u2510` - Top-right corner (┐)
- `\u2514` - Bottom-left corner (└)
- `\u2518` - Bottom-right corner (┘)
- `\u251C` - Left T-junction (├)
- `\u2524` - Right T-junction (┤)
- `\u252C` - Top T-junction (┬)
- `\u2534` - Bottom T-junction (┴)
- `\u253C` - Cross junction (┼)

**Usage in telnet-gui**:

```c
// Draw horizontal separator line
len += snprintf(seq + len, sizeof(seq) - len, "\033[7m");  // Reverse video
for (int i = 0; i < cols; i++) {
    len += snprintf(seq + len, sizeof(seq) - len, "\u2500");
}
len += snprintf(seq + len, sizeof(seq) - len, "\033[27m");  // Normal video
```

See `telnet-gui/src/box_drawing.c` for pixel-perfect rendering of box drawing characters.

## Implementation Notes

### Escape Sequence Ordering

**CRITICAL**: Some escape sequences have side effects on cursor position:

1. **DECSTBM moves cursor to (1,1)**: Always reposition cursor AFTER setting scrolling region, or use DECSC/DECRC
2. **CUP (cursor position) should be last**: Position cursor after all other operations

**Method 1: Position cursor after DECSTBM**

```c
char seq[1024];
int len = 0;

// 1. Clear line and write text
len += snprintf(seq + len, sizeof(seq) - len, "\033[42;1H\033[K");
len += snprintf(seq + len, sizeof(seq) - len, "Text here");

// 2. Set scrolling region (cursor moves to 1,1 as side effect)
len += snprintf(seq + len, sizeof(seq) - len, "\033[1;40r");

// 3. Position cursor LAST (overrides DECSTBM side effect)
len += snprintf(seq + len, sizeof(seq) - len, "\033[40;1H");

vterm_input_write(vterm, seq, len);
```

**Method 2: Save/restore cursor (recommended for preserving server cursor position)**

```c
char seq[1024];
int len = 0;

// 1. Save current cursor position
len += snprintf(seq + len, sizeof(seq) - len, "\0337");  // DECSC

// 2. Draw UI elements (input area, separator, etc.)
len += snprintf(seq + len, sizeof(seq) - len, "\033[42;1H\033[K");
len += snprintf(seq + len, sizeof(seq) - len, "Input area text");

// 3. Set scrolling region (cursor moves to 1,1 as side effect)
len += snprintf(seq + len, sizeof(seq) - len, "\033[1;40r");

// 4. Restore original cursor position
len += snprintf(seq + len, sizeof(seq) - len, "\0338");  // DECRC

vterm_input_write(vterm, seq, len);
// Cursor is now back where it was before drawing UI
```

**Why Method 2 is better**: When the telnet server positions the cursor (e.g., at end of last line), we want to preserve that position so new server output appears in the right place. Method 2 preserves the server's cursor positioning.

**Incorrect Example** (cursor position lost):

```c
// BAD: Cursor positioned before DECSTBM without save/restore
len += snprintf(seq + len, sizeof(seq) - len, "\033[42;1H");  // Position cursor
len += snprintf(seq + len, sizeof(seq) - len, "\033[1;40r");  // Oops! Cursor now at (1,1)
// Cursor is now at (1,1), not (42,1)!
// Server output will appear at wrong location
```

### Buffer Accumulation

For performance, accumulate multiple escape sequences in a buffer before sending to vterm:

```c
char seq[8192];
int len = 0;

len += snprintf(seq + len, sizeof(seq) - len, "\033[41;1H\033[K");  // Position + clear
len += snprintf(seq + len, sizeof(seq) - len, "\033[7m");           // Reverse video
len += snprintf(seq + len, sizeof(seq) - len, "Separator");         // Text
len += snprintf(seq + len, sizeof(seq) - len, "\033[27m");          // Normal video

vterm_input_write(vterm, seq, len);  // Single write
```

### Safety Checks

Always check buffer size when accumulating sequences:

```c
if (len < (int)sizeof(seq) - 100) {  // Leave safety margin
    len += snprintf(seq + len, sizeof(seq) - len, "\033[1;1H");
}
```

## References

- VT100 User Guide: https://vt100.net/docs/vt100-ug/
- VT510 Programmer Reference: https://vt100.net/docs/vt510-rm/
- ECMA-48 (Control Functions): https://www.ecma-international.org/publications/standards/Ecma-048.htm
- XTerm Control Sequences: https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
- libvterm: https://github.com/neovim/libvterm

## Usage in telnet-gui

See these source files for practical examples:

- `telnet-gui/src/terminal_backend_vterm.c` - Input area rendering with ANSI sequences
- `telnet-gui/src/box_drawing.c` - Box drawing character rendering
- `telnet-gui/src/renderer_backend_sdl.c` - Color conversion and text attributes
