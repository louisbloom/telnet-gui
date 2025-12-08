/* Glyph cache for terminal rendering */

#ifndef GLYPH_CACHE_H
#define GLYPH_CACHE_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

typedef struct GlyphCache GlyphCache;

/* Create a new glyph cache with the given font */
/* hinting_mode: TTF_HINTING_NONE, TTF_HINTING_LIGHT, TTF_HINTING_NORMAL, or TTF_HINTING_MONO */
/* scale_mode: SDL_ScaleModeNearest or SDL_ScaleModeLinear */
GlyphCache *glyph_cache_create(SDL_Renderer *renderer, const char *font_path, const char *font_name,
                                int font_size, int hinting_mode, SDL_ScaleMode scale_mode);

/* Get a cached texture for a glyph */
SDL_Texture *glyph_cache_get(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color, int bold,
                             int italic);

/* Get cell dimensions (width and height) */
void glyph_cache_get_cell_size(GlyphCache *cache, int *cell_w, int *cell_h);

/* Get font path */
const char *glyph_cache_get_font_path(GlyphCache *cache);

/* Get font name */
const char *glyph_cache_get_font_name(GlyphCache *cache);

/* Get the actual rendered width of a glyph (returns cached width if available, or renders and caches it) */
int glyph_cache_get_glyph_width(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color);

/* Clean up and destroy the cache */
void glyph_cache_destroy(GlyphCache *cache);

#endif /* GLYPH_CACHE_H */
