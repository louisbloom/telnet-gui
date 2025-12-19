/* Manual box drawing for pixel-perfect terminal alignment */

#ifndef BOX_DRAWING_H
#define BOX_DRAWING_H

#include <SDL2/SDL.h>
#include <stdint.h>

/* Check if a codepoint is a box drawing character (U+2500 - U+257F) */
int is_box_drawing_char(uint32_t codepoint);

/* Manually render a box drawing character using geometric primitives */
/* Guaranteed to fill the cell without gaps */
void render_box_drawing_char(SDL_Renderer *renderer, uint32_t codepoint, int x, int y, int w, int h, SDL_Color color);

#endif /* BOX_DRAWING_H */
