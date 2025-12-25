/* Terminal renderer using SDL2 */

#ifndef RENDERER_H
#define RENDERER_H

#include <SDL2/SDL.h>
#include <vterm.h>
#include "glyph_cache.h"
#include "terminal.h"
#include "dock.h"

typedef struct Renderer Renderer;

/* Create a new renderer */
Renderer *renderer_create(SDL_Renderer *sdl_renderer, GlyphCache *glyph_cache, int cell_width, int cell_height);

/* Render a frame - selection coordinates are viewport-relative with saved offsets and scrollback sizes */
void renderer_render(Renderer *r, Terminal *term, const char *title, int selection_active, int sel_start_row,
                     int sel_start_col, int sel_start_offset, int sel_start_scrollback, int sel_end_row,
                     int sel_end_col, int sel_end_offset, int sel_end_scrollback, Dock *dock,
                     int terminal_cols);

/* Clean up */
void renderer_destroy(Renderer *r);

#endif /* RENDERER_H */
