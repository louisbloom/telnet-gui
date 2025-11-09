/* Glyph cache implementation */

#include "glyph_cache.h"
#include <stdlib.h>
#include <string.h>

/* Simple LRU hash map node */
typedef struct CacheNode {
    uint32_t key; /* codepoint | (fg << 24) | (bg << 16) | (bold << 8) | italic */
    SDL_Texture *texture;
    int cell_width;  /* Advance width of this glyph */
    int cell_height; /* Height of the glyph */
} CacheNode;

struct GlyphCache {
    TTF_Font *font;
    SDL_Renderer *renderer;
    CacheNode *cache;
    int cache_size;
    int cell_w, cell_h;
    SDL_ScaleMode scale_mode;
};

/* Hash function for cache key */
static uint32_t hash_key(uint32_t codepoint, SDL_Color fg, SDL_Color bg, int bold, int italic) {
    /* Use a proper hash function to avoid collisions */
    /* Combine all values using a simple hash */
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

GlyphCache *glyph_cache_create(SDL_Renderer *renderer, const char *font_path, int font_size, int hinting_mode,
                               SDL_ScaleMode scale_mode) {
    GlyphCache *cache = (GlyphCache *)malloc(sizeof(GlyphCache));
    if (!cache)
        return NULL;

    cache->renderer = renderer;

    /* Try to load the requested font */
    cache->font = TTF_OpenFont(font_path, font_size);
    if (!cache->font) {
        /* Log the error */
        fprintf(stderr, "Font loading error: Failed to load font from '%s': %s\n", font_path, TTF_GetError());
        free(cache);
        return NULL;
    }

    /* Verify font loaded successfully */
    fprintf(stderr, "Font loaded successfully from: %s\n", font_path);
    fprintf(stderr, "Font size: %dpt\n", font_size);

    /* Try to get font style to verify it loaded correctly */
    int font_style = TTF_GetFontStyle(cache->font);
    fprintf(stderr, "Font style flags: %d (normal=0, bold=1, italic=2, underline=4, strikethrough=8)\n", font_style);

    /* Test rendering a character to verify font is working */
    SDL_Color test_color = {255, 255, 255, 255};
    SDL_Surface *test_surface = TTF_RenderGlyph_Blended(cache->font, 'M', test_color);
    if (test_surface) {
        fprintf(stderr, "Font test render successful: glyph size %dx%d\n", test_surface->w, test_surface->h);
        SDL_FreeSurface(test_surface);
    } else {
        fprintf(stderr, "Font test render failed: %s\n", TTF_GetError());
    }

    /* Set font rendering style - use provided hinting mode */
    TTF_SetFontHinting(cache->font, hinting_mode);

    /* Store scale mode for texture creation */
    cache->scale_mode = scale_mode;

    /* Allocate cache (1024 entries) */
    cache->cache_size = 1024;
    cache->cache = (CacheNode *)calloc(cache->cache_size, sizeof(CacheNode));

    /* Measure cell dimensions using space character (monospace fonts have same width for all chars) */
    int minx, maxx, miny, maxy, advance;
    TTF_GlyphMetrics(cache->font, ' ', &minx, &maxx, &miny, &maxy, &advance);

    /* Get font height metrics */
    int font_height = TTF_FontHeight(cache->font);

    /* Cell width: use advance width from space character (monospace) */
    cache->cell_w = advance;

    /* Cell height: use font height (no extra spacing) */
    cache->cell_h = font_height;

    return cache;
}

SDL_Texture *glyph_cache_get(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color, int bold,
                             int italic) {
    /* Create cache key */
    uint32_t key = hash_key(codepoint, fg_color, bg_color, bold, italic);
    int slot = key % cache->cache_size;

    /* Check if cached */
    if (cache->cache[slot].key == key && cache->cache[slot].texture) {
        return cache->cache[slot].texture;
    }

    /* Cache miss - render glyph with smooth anti-aliasing using Blended mode */
    /* TTF_RenderGlyph_Blended provides best quality with full alpha channel support */
    SDL_Surface *surface = TTF_RenderGlyph_Blended(cache->font, (uint16_t)codepoint, fg_color);
    if (!surface)
        return NULL;

    /* Create texture with alpha channel for smooth anti-aliased rendering */
    /* Use SDL_TEXTUREACCESS_STATIC for best performance with cached glyphs */
    SDL_Texture *texture = SDL_CreateTextureFromSurface(cache->renderer, surface);
    if (texture) {
        /* Use provided scale mode for font rendering */
        SDL_SetTextureScaleMode(texture, cache->scale_mode);
        /* Enable alpha blending for smooth edges */
        SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_BLEND);
    }
    SDL_FreeSurface(surface);

    if (texture) {
        /* Get actual texture dimensions */
        int tex_w, tex_h;
        SDL_QueryTexture(texture, NULL, NULL, &tex_w, &tex_h);

        /* Store in cache */
        if (cache->cache[slot].texture) {
            SDL_DestroyTexture(cache->cache[slot].texture);
        }
        cache->cache[slot].key = key;
        cache->cache[slot].texture = texture;
        cache->cache[slot].cell_width = tex_w;
        cache->cache[slot].cell_height = tex_h;
    }

    return texture;
}

void glyph_cache_get_cell_size(GlyphCache *cache, int *cell_w, int *cell_h) {
    *cell_w = cache->cell_w;
    *cell_h = cache->cell_h;
}

void glyph_cache_destroy(GlyphCache *cache) {
    if (!cache)
        return;

    if (cache->font) {
        TTF_CloseFont(cache->font);
    }

    /* Clean up cache textures */
    for (int i = 0; i < cache->cache_size; i++) {
        if (cache->cache[i].texture) {
            SDL_DestroyTexture(cache->cache[i].texture);
        }
    }

    free(cache->cache);
    free(cache);
}
