/* Internal glyph cache definitions - shared between backends */

#ifndef GLYPH_CACHE_INTERNAL_H
#define GLYPH_CACHE_INTERNAL_H

#include "glyph_cache.h"
#include "dynamic_buffer.h"
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

/* Simple LRU hash map node */
typedef struct CacheNode {
    uint32_t key; /* codepoint | (fg << 24) | (bg << 16) | (bold << 8) | italic */
    SDL_Texture *texture;
    int cell_width;  /* Advance width of this glyph */
    int cell_height; /* Height of the glyph */
    int minx;        /* Left bearing of the glyph for positioning */
} CacheNode;

struct GlyphCache {
    GlyphCacheBackendType backend_type; /* Which backend is being used */
    TTF_Font *font;
    TTF_Font *bold_font;   /* Bold font (NULL if not available, uses algorithmic bold) */
    TTF_Font *emoji_font;  /* Emoji font (color emoji) */
    TTF_Font *symbol_font; /* Symbol font for dingbats/symbols */
    SDL_Renderer *renderer;
    CacheNode *cache;
    int cache_size;
    int cell_w, cell_h;
    int space_minx; /* Baseline minx from space character for consistent positioning */
    SDL_ScaleMode scale_mode;
    char *font_path; /* Path to loaded font file */
    char *font_name; /* Display name of font */
#ifdef _WIN32
    /* DirectWrite backend state (Windows only) */
    void *dw_factory;          /* IDWriteFactory* */
    void *dw_gdi_interop;      /* IDWriteGdiInterop* */
    void *dw_font_face;        /* IDWriteFontFace* */
    void *dw_bold_face;        /* IDWriteFontFace* for bold */
    void *dw_emoji_face;       /* IDWriteFontFace* for emoji */
    void *dw_symbol_face;      /* IDWriteFontFace* for symbols */
    void *dw_render_target;    /* IDWriteBitmapRenderTarget* */
    void *dw_render_params;    /* IDWriteRenderingParams* */
    float dw_em_size;          /* Font em size in DIPs */
    int dw_use_cleartype;      /* Whether to use ClearType */
    DynamicBuffer *dw_pixel_buf; /* Reusable buffer for bitmap extraction */
#endif
};

/* Hash function for cache key (shared between backends) */
static inline uint32_t glyph_cache_hash_key(uint32_t codepoint, SDL_Color fg, SDL_Color bg, int bold, int italic) {
    uint32_t hash = codepoint;
    hash = hash * 31 + fg.r;
    hash = hash * 31 + fg.g;
    hash = hash * 31 + fg.b;
    hash = hash * 31 + bg.r;
    hash = hash * 31 + bg.g;
    hash = hash * 31 + bg.b;
    hash = hash * 31 + (bold ? 1 : 0);
    hash = hash * 31 + (italic ? 1 : 0);
    return hash;
}

#endif /* GLYPH_CACHE_INTERNAL_H */
