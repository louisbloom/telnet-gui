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
};

/* Hash function for cache key */
static uint32_t hash_key(uint32_t codepoint, SDL_Color fg, SDL_Color bg, int bold, int italic) {
    return codepoint | (fg.r << 24) | (fg.g << 16) | (fg.b << 8) | bg.r | (bold << 7) | (italic << 6);
}

GlyphCache *glyph_cache_create(SDL_Renderer *renderer, const char *font_path, int font_size) {
    GlyphCache *cache = (GlyphCache *)malloc(sizeof(GlyphCache));
    if (!cache)
        return NULL;

    cache->renderer = renderer;
    cache->font = TTF_OpenFont(font_path, font_size);
    if (!cache->font) {
        /* Fallback to default font */
        cache->font = TTF_OpenFont("/ucrt64/share/fonts/TTF/DejaVuSansMono.ttf", font_size);
        if (!cache->font) {
            free(cache);
            return NULL;
        }
    }

    /* Allocate cache (1024 entries) */
    cache->cache_size = 1024;
    cache->cache = (CacheNode *)calloc(cache->cache_size, sizeof(CacheNode));

    /* Measure cell dimensions */
    int minx, maxx, miny, maxy, advance;
    TTF_GlyphMetrics(cache->font, ' ', &minx, &maxx, &miny, &maxy, &advance);
    cache->cell_w = advance + 1;                     /* Add 1 pixel for spacing */
    cache->cell_h = TTF_FontHeight(cache->font) + 2; /* Add 2 pixels for spacing */

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

    /* Cache miss - render glyph */
    SDL_Surface *surface = TTF_RenderGlyph_Blended(cache->font, (uint16_t)codepoint, fg_color);
    if (!surface)
        return NULL;

    SDL_Texture *texture = SDL_CreateTextureFromSurface(cache->renderer, surface);
    SDL_FreeSurface(surface);

    if (texture) {
        /* Store in cache */
        if (cache->cache[slot].texture) {
            SDL_DestroyTexture(cache->cache[slot].texture);
        }
        cache->cache[slot].key = key;
        cache->cache[slot].texture = texture;
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
