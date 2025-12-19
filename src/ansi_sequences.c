/* ansi_sequences.c - Helper functions for parameterized ANSI sequences */

#include "ansi_sequences.h"
#include <stdio.h>

/* SGR color parameter prefixes for 256-color and RGB modes */
#define SGR_FG_256_PREFIX "38;5;" /* 256-color foreground - CSI 38;5;color m */
#define SGR_BG_256_PREFIX "48;5;" /* 256-color background - CSI 48;5;color m */
#define SGR_FG_RGB_PREFIX "38;2;" /* RGB foreground - CSI 38;2;r;g;b m */
#define SGR_BG_RGB_PREFIX "48;2;" /* RGB background - CSI 48;2;r;g;b m */

void ansi_format_cursor_pos(char *buf, size_t size, int row, int col) {
    if (!buf || size < 16)
        return;
    snprintf(buf, size, CSI "%d;%d" CUP_FINAL, row, col);
}

void ansi_format_scroll_region(char *buf, size_t size, int top, int bottom) {
    if (!buf || size < 24)
        return;
    snprintf(buf, size, CSI "%d;%d" DECSTBM_FINAL, top, bottom);
}

void ansi_format_fg_color_256(char *buf, size_t size, int color) {
    if (!buf || size < 16)
        return;
    snprintf(buf, size, CSI SGR_FG_256_PREFIX "%d" SGR_FINAL, color);
}

void ansi_format_bg_color_256(char *buf, size_t size, int color) {
    if (!buf || size < 16)
        return;
    snprintf(buf, size, CSI SGR_BG_256_PREFIX "%d" SGR_FINAL, color);
}

void ansi_format_fg_color_rgb(char *buf, size_t size, int r, int g, int b) {
    if (!buf || size < 24)
        return;
    snprintf(buf, size, CSI SGR_FG_RGB_PREFIX "%d;%d;%d" SGR_FINAL, r, g, b);
}

void ansi_format_bg_color_rgb(char *buf, size_t size, int r, int g, int b) {
    if (!buf || size < 24)
        return;
    snprintf(buf, size, CSI SGR_BG_RGB_PREFIX "%d;%d;%d" SGR_FINAL, r, g, b);
}
