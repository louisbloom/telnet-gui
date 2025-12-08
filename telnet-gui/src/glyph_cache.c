/* Glyph cache implementation */

#include "glyph_cache.h"
#include "../../telnet-lisp/include/utf8.h"
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
    TTF_Font *emoji_font; /* Fallback font for emoji characters */
    SDL_Renderer *renderer;
    CacheNode *cache;
    int cache_size;
    int cell_w, cell_h;
    SDL_ScaleMode scale_mode;
    char *font_path; /* Path to loaded font file */
    char *font_name; /* Display name of font */
};

/* Check if a codepoint is likely an emoji */
static int is_emoji_codepoint(uint32_t codepoint) {
    /* Common emoji ranges */
    if (codepoint >= 0x1F300 && codepoint <= 0x1F9FF)
        return 1; /* Emoticons, symbols, pictographs */
    if (codepoint >= 0x2600 && codepoint <= 0x26FF)
        return 1; /* Miscellaneous symbols */
    if (codepoint >= 0x2700 && codepoint <= 0x27BF)
        return 1; /* Dingbats */
    if (codepoint >= 0x1F600 && codepoint <= 0x1F64F)
        return 1; /* Emoticons */
    if (codepoint >= 0x1F680 && codepoint <= 0x1F6FF)
        return 1; /* Transport and map symbols */
    return 0;
}

/* Try to find system emoji font */
static const char *find_emoji_font(void) {
#ifdef _WIN32
    /* Windows: Try Segoe UI Emoji */
    const char *emoji_fonts[] = {"C:/Windows/Fonts/seguiemj.ttf", /* Segoe UI Emoji */
                                 "C:\\Windows\\Fonts\\seguiemj.ttf", NULL};

    for (int i = 0; emoji_fonts[i] != NULL; i++) {
        FILE *test = fopen(emoji_fonts[i], "rb");
        if (test) {
            fclose(test);
            return emoji_fonts[i];
        }
    }
#elif defined(__APPLE__)
    /* macOS: Apple Color Emoji */
    return "/System/Library/Fonts/Apple Color Emoji.ttc";
#else
    /* Linux: Try common emoji fonts */
    const char *emoji_fonts[] = {"/usr/share/fonts/truetype/noto/NotoColorEmoji.ttf",
                                 "/usr/share/fonts/noto-emoji/NotoColorEmoji.ttf", NULL};

    for (int i = 0; emoji_fonts[i] != NULL; i++) {
        FILE *test = fopen(emoji_fonts[i], "rb");
        if (test) {
            fclose(test);
            return emoji_fonts[i];
        }
    }
#endif
    return NULL;
}

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

GlyphCache *glyph_cache_create(SDL_Renderer *renderer, const char *font_path, const char *font_name, int font_size,
                               int hinting_mode, SDL_ScaleMode scale_mode) {
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

    /* Store font path and name */
    cache->font_path = strdup(font_path);
    cache->font_name = strdup(font_name);

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

    /* Get font metrics without line gap (terminal standard) */
    int ascent = TTF_FontAscent(cache->font);
    int descent = TTF_FontDescent(cache->font); /* Returns negative value */

    /* Cell width: use advance width from space character (monospace) */
    cache->cell_w = advance;

    /* Cell height: use ascent + descent (no line gap) for compact terminal appearance */
    cache->cell_h = ascent + abs(descent);

    /* Try to load emoji font as fallback */
    cache->emoji_font = NULL;
    const char *emoji_font_path = find_emoji_font();
    if (emoji_font_path) {
        cache->emoji_font = TTF_OpenFont(emoji_font_path, font_size);
        if (cache->emoji_font) {
            fprintf(stderr, "Emoji font loaded successfully from: %s\n", emoji_font_path);
        } else {
            fprintf(stderr, "Failed to load emoji font from: %s (%s)\n", emoji_font_path, TTF_GetError());
        }
    } else {
        fprintf(stderr, "No emoji font found on system\n");
    }

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

    /* Compute font style flags based on bold/italic parameters */
    int style = TTF_STYLE_NORMAL;
    if (bold)
        style |= TTF_STYLE_BOLD;
    if (italic)
        style |= TTF_STYLE_ITALIC;

    /* Try emoji font first if this looks like an emoji codepoint */
    SDL_Surface *surface = NULL;

    if (is_emoji_codepoint(codepoint) && cache->emoji_font) {
        /* Apply style to emoji font */
        TTF_SetFontStyle(cache->emoji_font, style);

        /* Convert codepoint to UTF-8 for proper emoji rendering */
        char utf8[5];
        utf8_put_codepoint(codepoint, utf8);

        /* Use UTF8 rendering for emoji (supports 32-bit codepoints) */
        surface = TTF_RenderUTF8_Blended(cache->emoji_font, utf8, fg_color);

        /* Reset emoji font style */
        TTF_SetFontStyle(cache->emoji_font, TTF_STYLE_NORMAL);
    }

    /* If emoji font failed or not an emoji, try main font with glyph rendering */
    if (!surface) {
        /* Apply style to main font */
        TTF_SetFontStyle(cache->font, style);

        /* For BMP characters (< 0x10000), use glyph rendering */
        if (codepoint < 0x10000) {
            surface = TTF_RenderGlyph_Blended(cache->font, (uint16_t)codepoint, fg_color);
        } else {
            /* For higher codepoints, use UTF-8 rendering */
            char utf8[5];
            utf8_put_codepoint(codepoint, utf8);
            surface = TTF_RenderUTF8_Blended(cache->font, utf8, fg_color);
        }

        /* Reset main font style */
        TTF_SetFontStyle(cache->font, TTF_STYLE_NORMAL);
    }

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

int glyph_cache_get_glyph_width(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color) {
    (void)bg_color; /* Unused - we use black background for width measurement */
    if (!cache)
        return 0;

    /* Ensure glyph is cached by calling glyph_cache_get */
    SDL_Color black_bg = {0, 0, 0, 255};
    glyph_cache_get(cache, codepoint, fg_color, black_bg, 0, 0);

    /* Look up the cached entry */
    uint32_t key = hash_key(codepoint, fg_color, black_bg, 0, 0);
    uint32_t slot = key % cache->cache_size;

    if (cache->cache[slot].key == key && cache->cache[slot].texture) {
        return cache->cache[slot].cell_width;
    }

    /* Fallback to default cell width if not found */
    return cache->cell_w;
}

void glyph_cache_get_cell_size(GlyphCache *cache, int *cell_w, int *cell_h) {
    *cell_w = cache->cell_w;
    *cell_h = cache->cell_h;
}

const char *glyph_cache_get_font_path(GlyphCache *cache) {
    if (!cache)
        return NULL;
    return cache->font_path;
}

const char *glyph_cache_get_font_name(GlyphCache *cache) {
    if (!cache)
        return NULL;
    return cache->font_name;
}

void glyph_cache_destroy(GlyphCache *cache) {
    if (!cache)
        return;

    if (cache->font) {
        TTF_CloseFont(cache->font);
    }

    if (cache->emoji_font) {
        TTF_CloseFont(cache->emoji_font);
    }

    /* Clean up cache textures */
    for (int i = 0; i < cache->cache_size; i++) {
        if (cache->cache[i].texture) {
            SDL_DestroyTexture(cache->cache[i].texture);
        }
    }

    /* Free font path and name strings */
    if (cache->font_path) {
        free(cache->font_path);
    }
    if (cache->font_name) {
        free(cache->font_name);
    }

    free(cache->cache);
    free(cache);
}
