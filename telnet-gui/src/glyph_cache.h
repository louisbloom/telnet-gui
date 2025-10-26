/* Glyph cache for terminal rendering */

#ifndef GLYPH_CACHE_H
#define GLYPH_CACHE_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

typedef struct GlyphCache GlyphCache;

/* Create a new glyph cache with the given font */
GlyphCache *glyph_cache_create(SDL_Renderer *renderer, const char *font_path, int font_size);

/* Get a cached texture for a glyph */
SDL_Texture *glyph_cache_get(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color, int bold,
                             int italic);

/* Get cell dimensions (width and height) */
void glyph_cache_get_cell_size(GlyphCache *cache, int *cell_w, int *cell_h);

/* Clean up and destroy the cache */
void glyph_cache_destroy(GlyphCache *cache);

#endif /* GLYPH_CACHE_H */
