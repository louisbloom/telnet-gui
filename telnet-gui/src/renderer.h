/* Terminal renderer using SDL2 */

#ifndef RENDERER_H
#define RENDERER_H

#include <SDL2/SDL.h>
#include <vterm.h>
#include "glyph_cache.h"
#include "terminal.h"

typedef struct Renderer Renderer;

/* Create a new renderer */
Renderer *renderer_create(SDL_Renderer *sdl_renderer, GlyphCache *glyph_cache, int cell_width, int cell_height,
                          int titlebar_height);

/* Render a frame */
void renderer_render(Renderer *r, Terminal *term, const char *title);

/* Clean up */
void renderer_destroy(Renderer *r);

#endif /* RENDERER_H */
