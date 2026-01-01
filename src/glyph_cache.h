/* Glyph cache for terminal rendering */

#ifndef GLYPH_CACHE_H
#define GLYPH_CACHE_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include "../../telnet-lisp/include/utf8.h"

typedef struct GlyphCache GlyphCache;

/* Font rendering backend type */
typedef enum {
    GLYPH_CACHE_BACKEND_SDL_TTF,    /* SDL2_ttf (default, cross-platform) */
    GLYPH_CACHE_BACKEND_DIRECTWRITE /* DirectWrite (Windows only) */
} GlyphCacheBackendType;

/* Create a new glyph cache with the given font */
/* hinting_mode: TTF_HINTING_NONE, TTF_HINTING_LIGHT, TTF_HINTING_NORMAL, or TTF_HINTING_MONO */
/* scale_mode: SDL_ScaleModeNearest or SDL_ScaleModeLinear */
/* hdpi, vdpi: Horizontal and vertical DPI for font rendering (e.g., 96, 96 for Windows default) */
GlyphCache *glyph_cache_create(SDL_Renderer *renderer, const char *font_path, const char *font_name,
                                int font_size, int hinting_mode, SDL_ScaleMode scale_mode, int hdpi, int vdpi);

/* Create a new glyph cache with a specific backend */
/* backend: GLYPH_CACHE_BACKEND_SDL_TTF or GLYPH_CACHE_BACKEND_DIRECTWRITE */
/* use_cleartype: Enable ClearType rendering (DirectWrite only, ignored for SDL_ttf) */
/* metrics_only: If true, only load main font for cell metrics (skip bold/emoji/symbol) */
GlyphCache *glyph_cache_create_with_backend(GlyphCacheBackendType backend, SDL_Renderer *renderer,
                                             const char *font_path, const char *font_name, int font_size,
                                             int hinting_mode, SDL_ScaleMode scale_mode, int hdpi, int vdpi,
                                             int use_cleartype, int metrics_only);

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

#endif /* GLYPH_CACHE_H */
