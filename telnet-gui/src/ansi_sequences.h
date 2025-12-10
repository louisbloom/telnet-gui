/* ansi_sequences.h - Static ANSI escape sequence table and helpers
 *
 * This header provides symbolic constants for ANSI escape sequences compatible
 * with xterm, screen, and linux terminals. This improves code readability and
 * prepares for future dual-mode operation (GUI + custom TUI).
 *
 * Design principles:
 * - Static constants (no terminfo dependencies)
 * - Universal sequences that work across xterm/screen/linux
 * - Reusable for both libvterm control (GUI mode) and future custom TUI rendering
 * - Helper functions for parameterized sequences (cursor positioning, scroll regions)
 * - Uses standard VT100/ANSI reference names (CUP, DECSC, DECRC, ED, EL, SGR, etc.)
 */

#ifndef ANSI_SEQUENCES_H
#define ANSI_SEQUENCES_H

#include <stddef.h>

/* Base control characters and sequences */
#define ESC "\033"      /* Escape character */
#define CSI ESC "["     /* CSI (Control Sequence Introducer) = ESC [ */

/* Final characters for control sequences */
#define CUP_FINAL "H"        /* CUP (Cursor Position) */
#define ED_FINAL "J"         /* ED (Erase in Display) */
#define EL_FINAL "K"         /* EL (Erase in Line) */
#define SGR_FINAL "m"        /* SGR (Select Graphic Rendition) */
#define DECSTBM_FINAL "r"    /* DECSTBM (Set Top and Bottom Margins) */

/* DEC private sequences (not using CSI) */
#define DECSC ESC "7"        /* DECSC - Save cursor position and attributes */
#define DECRC ESC "8"        /* DECRC - Restore cursor position and attributes */

/* Cursor movement sequences */
#define CUP_HOME            CSI "H"           /* CUP - Move cursor to home position (1,1) */
#define ANSI_CURSOR_HOME    CUP_HOME          /* Alias for compatibility */
#define ANSI_CURSOR_SAVE    DECSC             /* DECSC - Save cursor */
#define ANSI_CURSOR_RESTORE DECRC             /* DECRC - Restore cursor */

/* Erase sequences (ED - Erase in Display) */
#define ED_TO_END           CSI ED_FINAL      /* ED - Erase from cursor to end of screen */
#define ED_ENTIRE           CSI "2" ED_FINAL  /* ED2 - Erase entire screen */
#define ANSI_ERASE_TO_EOS   ED_TO_END         /* Alias for compatibility */
#define ANSI_ERASE_SCREEN   ED_ENTIRE         /* Alias for compatibility */
#define ANSI_CLEAR_SCREEN   CUP_HOME ED_ENTIRE /* Home + erase entire screen */

/* Erase sequences (EL - Erase in Line) */
#define EL_TO_END           CSI EL_FINAL      /* EL - Erase from cursor to end of line */
#define ANSI_ERASE_TO_EOL   EL_TO_END         /* Alias for compatibility */

/* SGR (Select Graphic Rendition) sequences - Text attributes */
#define SGR_RESET           CSI "0" SGR_FINAL       /* SGR 0 - Reset all attributes */
#define SGR_BOLD            CSI "1" SGR_FINAL       /* SGR 1 - Bold */
#define SGR_DIM             CSI "2" SGR_FINAL       /* SGR 2 - Dim */
#define SGR_ITALIC          CSI "3" SGR_FINAL       /* SGR 3 - Italic */
#define SGR_UNDERLINE       CSI "4" SGR_FINAL       /* SGR 4 - Underline */
#define SGR_BLINK           CSI "5" SGR_FINAL       /* SGR 5 - Blink */
#define SGR_REVERSE         CSI "7" SGR_FINAL       /* SGR 7 - Reverse video */
#define SGR_HIDDEN          CSI "8" SGR_FINAL       /* SGR 8 - Hidden */
#define SGR_STRIKETHROUGH   CSI "9" SGR_FINAL       /* SGR 9 - Strikethrough */

/* SGR attribute reset sequences */
#define SGR_BOLD_OFF        CSI "22" SGR_FINAL      /* SGR 22 - Normal intensity */
#define SGR_ITALIC_OFF      CSI "23" SGR_FINAL      /* SGR 23 - Not italic */
#define SGR_UNDERLINE_OFF   CSI "24" SGR_FINAL      /* SGR 24 - Not underlined */
#define SGR_BLINK_OFF       CSI "25" SGR_FINAL      /* SGR 25 - Not blinking */
#define SGR_REVERSE_OFF     CSI "27" SGR_FINAL      /* SGR 27 - Not reversed */
#define SGR_HIDDEN_OFF      CSI "28" SGR_FINAL      /* SGR 28 - Not hidden */
#define SGR_STRIKETHROUGH_OFF CSI "29" SGR_FINAL    /* SGR 29 - Not strikethrough */

/* SGR RGB color prefixes (for use with ansi_format_fg/bg_color_rgb) */
#define SGR_FG_RGB_PREFIX "38;2;"  /* SGR 38;2;r;g;b - Set foreground RGB color */
#define SGR_BG_RGB_PREFIX "48;2;"  /* SGR 48;2;r;g;b - Set background RGB color */

/* Compatibility aliases using ANSI_ prefix */
#define ANSI_SGR_RESET          SGR_RESET
#define ANSI_SGR_BOLD           SGR_BOLD
#define ANSI_SGR_DIM            SGR_DIM
#define ANSI_SGR_ITALIC         SGR_ITALIC
#define ANSI_SGR_UNDERLINE      SGR_UNDERLINE
#define ANSI_SGR_BLINK          SGR_BLINK
#define ANSI_SGR_REVERSE        SGR_REVERSE
#define ANSI_SGR_HIDDEN         SGR_HIDDEN
#define ANSI_SGR_STRIKETHROUGH  SGR_STRIKETHROUGH
#define ANSI_SGR_BOLD_OFF       SGR_BOLD_OFF
#define ANSI_SGR_ITALIC_OFF     SGR_ITALIC_OFF
#define ANSI_SGR_UNDERLINE_OFF  SGR_UNDERLINE_OFF
#define ANSI_SGR_BLINK_OFF      SGR_BLINK_OFF
#define ANSI_SGR_REVERSE_OFF    SGR_REVERSE_OFF
#define ANSI_SGR_HIDDEN_OFF     SGR_HIDDEN_OFF
#define ANSI_SGR_STRIKETHROUGH_OFF SGR_STRIKETHROUGH_OFF

/* SGR basic 8-color foreground (30-37) */
#define SGR_FG_BLACK        CSI "30" SGR_FINAL      /* SGR 30 */
#define SGR_FG_RED          CSI "31" SGR_FINAL      /* SGR 31 */
#define SGR_FG_GREEN        CSI "32" SGR_FINAL      /* SGR 32 */
#define SGR_FG_YELLOW       CSI "33" SGR_FINAL      /* SGR 33 */
#define SGR_FG_BLUE         CSI "34" SGR_FINAL      /* SGR 34 */
#define SGR_FG_MAGENTA      CSI "35" SGR_FINAL      /* SGR 35 */
#define SGR_FG_CYAN         CSI "36" SGR_FINAL      /* SGR 36 */
#define SGR_FG_WHITE        CSI "37" SGR_FINAL      /* SGR 37 */
#define SGR_FG_DEFAULT      CSI "39" SGR_FINAL      /* SGR 39 - Default foreground */

/* SGR basic 8-color background (40-47) */
#define SGR_BG_BLACK        CSI "40" SGR_FINAL      /* SGR 40 */
#define SGR_BG_RED          CSI "41" SGR_FINAL      /* SGR 41 */
#define SGR_BG_GREEN        CSI "42" SGR_FINAL      /* SGR 42 */
#define SGR_BG_YELLOW       CSI "43" SGR_FINAL      /* SGR 43 */
#define SGR_BG_BLUE         CSI "44" SGR_FINAL      /* SGR 44 */
#define SGR_BG_MAGENTA      CSI "45" SGR_FINAL      /* SGR 45 */
#define SGR_BG_CYAN         CSI "46" SGR_FINAL      /* SGR 46 */
#define SGR_BG_WHITE        CSI "47" SGR_FINAL      /* SGR 47 */
#define SGR_BG_DEFAULT      CSI "49" SGR_FINAL      /* SGR 49 - Default background */

/* SGR color reset */
#define SGR_COLOR_RESET     CSI "39;49" SGR_FINAL   /* SGR 39;49 - Reset both fg and bg */

/* Compatibility aliases using ANSI_ prefix */
#define ANSI_FG_BLACK           SGR_FG_BLACK
#define ANSI_FG_RED             SGR_FG_RED
#define ANSI_FG_GREEN           SGR_FG_GREEN
#define ANSI_FG_YELLOW          SGR_FG_YELLOW
#define ANSI_FG_BLUE            SGR_FG_BLUE
#define ANSI_FG_MAGENTA         SGR_FG_MAGENTA
#define ANSI_FG_CYAN            SGR_FG_CYAN
#define ANSI_FG_WHITE           SGR_FG_WHITE
#define ANSI_FG_DEFAULT         SGR_FG_DEFAULT
#define ANSI_BG_BLACK           SGR_BG_BLACK
#define ANSI_BG_RED             SGR_BG_RED
#define ANSI_BG_GREEN           SGR_BG_GREEN
#define ANSI_BG_YELLOW          SGR_BG_YELLOW
#define ANSI_BG_BLUE            SGR_BG_BLUE
#define ANSI_BG_MAGENTA         SGR_BG_MAGENTA
#define ANSI_BG_CYAN            SGR_BG_CYAN
#define ANSI_BG_WHITE           SGR_BG_WHITE
#define ANSI_BG_DEFAULT         SGR_BG_DEFAULT
#define ANSI_COLOR_RESET        SGR_COLOR_RESET

/* Scrolling region control */
/* Note: Use ansi_format_scroll_region() for setting custom regions */

/* Helper functions for parameterized ANSI sequences */

/* Format cursor position sequence (CUP - Cursor Position)
 * Parameters:
 *   buf: Output buffer (must be at least 16 bytes)
 *   size: Size of output buffer
 *   row: Row position (1-indexed)
 *   col: Column position (1-indexed)
 * Result: "\033[<row>;<col>H"
 */
void ansi_format_cursor_pos(char *buf, size_t size, int row, int col);

/* Format scrolling region sequence (DECSTBM - Set Top and Bottom Margins)
 * Parameters:
 *   buf: Output buffer (must be at least 24 bytes)
 *   size: Size of output buffer
 *   top: Top margin line number (1-indexed, first scrollable line)
 *   bottom: Bottom margin line number (1-indexed, last scrollable line)
 * Result: "\033[<top>;<bottom>r"
 * Note: Lines outside this region remain fixed during scrolling
 */
void ansi_format_scroll_region(char *buf, size_t size, int top, int bottom);

/* Format 256-color foreground sequence (SGR 38;5)
 * Parameters:
 *   buf: Output buffer (must be at least 16 bytes)
 *   size: Size of output buffer
 *   color: Color index (0-255)
 * Result: "\033[38;5;<color>m"
 */
void ansi_format_fg_color_256(char *buf, size_t size, int color);

/* Format 256-color background sequence (SGR 48;5)
 * Parameters:
 *   buf: Output buffer (must be at least 16 bytes)
 *   size: Size of output buffer
 *   color: Color index (0-255)
 * Result: "\033[48;5;<color>m"
 */
void ansi_format_bg_color_256(char *buf, size_t size, int color);

/* Format RGB foreground color sequence (SGR 38;2)
 * Parameters:
 *   buf: Output buffer (must be at least 24 bytes)
 *   size: Size of output buffer
 *   r, g, b: RGB color components (0-255)
 * Result: "\033[38;2;<r>;<g>;<b>m"
 */
void ansi_format_fg_color_rgb(char *buf, size_t size, int r, int g, int b);

/* Format RGB background color sequence (SGR 48;2)
 * Parameters:
 *   buf: Output buffer (must be at least 24 bytes)
 *   size: Size of output buffer
 *   r, g, b: RGB color components (0-255)
 * Result: "\033[48;2;<r>;<g>;<b>m"
 */
void ansi_format_bg_color_rgb(char *buf, size_t size, int r, int g, int b);

#endif /* ANSI_SEQUENCES_H */
