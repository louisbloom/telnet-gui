/* Generic terminal cell structure - backend-independent */

#ifndef TERM_CELL_H
#define TERM_CELL_H

#include <stdint.h>

/* Generic terminal cell that can represent any terminal emulator's cell format
 * This structure is backend-independent and allows swapping terminal backends
 * (libvterm, simple terminal, ncurses, etc.) without changing renderer code.
 *
 * Design matches VTermScreenCell layout for compatibility but is backend-agnostic.
 */
typedef struct TermCell {
    /* Character content - up to 6 UTF-32 codepoints for combining characters */
    uint32_t chars[6];

    /* Character width in cells (1 for normal, 2 for wide chars like emoji) */
    char width;

    /* Text attributes */
    struct {
        unsigned int bold : 1;
        unsigned int italic : 1;
        unsigned int underline : 2;  /* 0=none, 1=single, 2=double */
        unsigned int reverse : 1;     /* Reverse video (swap fg/bg) */
        unsigned int strike : 1;      /* Strikethrough */
        unsigned int blink : 1;       /* Blinking text */
    } attrs;

    /* Foreground color */
    struct {
        uint8_t type;  /* 0=default, 1=indexed (256-color), 2=RGB (24-bit) */
        union {
            uint8_t idx;               /* Indexed color (0-255) */
            struct {
                uint8_t r, g, b;
            } rgb;  /* RGB color */
        } color;
    } fg;

    /* Background color */
    struct {
        uint8_t type;  /* 0=default, 1=indexed (256-color), 2=RGB (24-bit) */
        union {
            uint8_t idx;               /* Indexed color (0-255) */
            struct {
                uint8_t r, g, b;
            } rgb;  /* RGB color */
        } color;
    } bg;
} TermCell;

/* Color type constants */
#define TERM_COLOR_DEFAULT 0
#define TERM_COLOR_INDEXED 1
#define TERM_COLOR_RGB 2

#endif /* TERM_CELL_H */
