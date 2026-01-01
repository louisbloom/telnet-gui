/* DirectWrite glyph cache backend (Windows only) */

#ifndef GLYPH_CACHE_DIRECTWRITE_H
#define GLYPH_CACHE_DIRECTWRITE_H

#ifdef _WIN32

#include <SDL2/SDL.h>
#include "glyph_cache.h"

/* Initialize DirectWrite subsystem (call once at startup) */
int directwrite_init(void);

/* Shutdown DirectWrite subsystem */
void directwrite_shutdown(void);

/* Create a glyph cache using DirectWrite backend */
GlyphCache *glyph_cache_create_directwrite(SDL_Renderer *renderer, const char *font_path, const char *font_name,
                                            int font_size, int hinting_mode, SDL_ScaleMode scale_mode, int hdpi,
                                            int vdpi, int use_cleartype);

/* Get a glyph texture using DirectWrite (called via dispatch from glyph_cache_get) */
SDL_Texture *glyph_cache_directwrite_get(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color,
                                          int bold, int italic, int is_emoji);

/* Destroy DirectWrite glyph cache resources */
void glyph_cache_directwrite_destroy(GlyphCache *cache);

#endif /* _WIN32 */
#endif /* GLYPH_CACHE_DIRECTWRITE_H */
