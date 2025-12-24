/* Glyph cache for terminal rendering */

#ifndef GLYPH_CACHE_H
#define GLYPH_CACHE_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

typedef struct GlyphCache GlyphCache;

/* Create a new glyph cache with the given font */
/* hinting_mode: TTF_HINTING_NONE, TTF_HINTING_LIGHT, TTF_HINTING_NORMAL, or TTF_HINTING_MONO */
/* scale_mode: SDL_ScaleModeNearest or SDL_ScaleModeLinear */
/* hdpi, vdpi: Horizontal and vertical DPI for font rendering (e.g., 96, 96 for Windows default) */
GlyphCache *glyph_cache_create(SDL_Renderer *renderer, const char *font_path, const char *font_name,
                                int font_size, int hinting_mode, SDL_ScaleMode scale_mode, int hdpi, int vdpi);

/* Get a cached texture for a glyph */
/* is_emoji: if true and emoji font available, use it even if main font has the glyph */
SDL_Texture *glyph_cache_get(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color, int bold,
                             int italic, int is_emoji);

/* Get cell dimensions (width and height) */
void glyph_cache_get_cell_size(GlyphCache *cache, int *cell_w, int *cell_h);

/* Get font path */
const char *glyph_cache_get_font_path(GlyphCache *cache);

/* Get font name */
const char *glyph_cache_get_font_name(GlyphCache *cache);

/* Get the actual rendered width of a glyph (returns cached width if available, or renders and caches it) */
int glyph_cache_get_glyph_width(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color);

/* Get the left bearing (minx) of a glyph for precise positioning */
int glyph_cache_get_minx(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color);

/* Clean up and destroy the cache */
void glyph_cache_destroy(GlyphCache *cache);

/* Check if codepoint is in a symbol/dingbat range (rendered by symbol font, not emoji font).
 * These are monochrome symbols that need the symbol font fallback and vertical centering. */
static inline int is_symbol_range(uint32_t codepoint) {
    /* Miscellaneous Symbols (U+2600-U+26FF) */
    if (codepoint >= 0x2600 && codepoint <= 0x26FF)
        return 1;
    /* Dingbats (U+2700-U+27BF) - includes U+2726 (four-pointed star) */
    if (codepoint >= 0x2700 && codepoint <= 0x27BF)
        return 1;
    return 0;
}

#endif /* GLYPH_CACHE_H */
