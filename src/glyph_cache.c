/* Glyph cache implementation */

#include "glyph_cache_internal.h"
#include "../../telnet-lisp/include/utf8.h"
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include "glyph_cache_directwrite.h"
#endif

/* Find first existing font from a NULL-terminated list of paths */
static const char *find_first_existing_font(const char *fonts[]) {
    for (int i = 0; fonts[i] != NULL; i++) {
        FILE *test = fopen(fonts[i], "rb");
        if (test) {
            fclose(test);
            return fonts[i];
        }
    }
    return NULL;
}

/* Try to find system symbol font for dingbats/symbols */
static const char *find_symbol_font(void) {
#ifdef _WIN32
    static const char *fonts[] = {"C:/Windows/Fonts/seguisym.ttf", "C:\\Windows\\Fonts\\seguisym.ttf", NULL};
#elif defined(__APPLE__)
    static const char *fonts[] = {"/System/Library/Fonts/Apple Symbols.ttf", NULL};
#else
    static const char *fonts[] = {"/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
                                  "/usr/share/fonts/TTF/DejaVuSans.ttf", NULL};
#endif
    return find_first_existing_font(fonts);
}

/* Try to find system emoji font for color emoji fallback */
static const char *find_emoji_font(void) {
#ifdef _WIN32
    static const char *fonts[] = {"C:/Windows/Fonts/seguiemj.ttf", "C:\\Windows\\Fonts\\seguiemj.ttf", NULL};
#elif defined(__APPLE__)
    static const char *fonts[] = {"/System/Library/Fonts/Apple Color Emoji.ttc", NULL};
#else
    static const char *fonts[] = {
        "/usr/share/fonts/truetype/noto/NotoColorEmoji.ttf", "/usr/share/fonts/noto-emoji/NotoColorEmoji.ttf",
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf", "/usr/share/fonts/TTF/DejaVuSans.ttf", NULL};
#endif
    return find_first_existing_font(fonts);
}

/* Derive bold font path from regular font path.
 * Supports multiple naming conventions:
 * - Distributed fonts: {Name}-Regular.ttf -> {Name}-Bold.ttf
 * - DejaVu style: DejaVuSansMono.ttf -> DejaVuSansMono-Bold.ttf
 * - Windows system fonts: consola.ttf -> consolab.ttf, cour.ttf -> courbd.ttf
 * Returns allocated string (caller must free) or NULL if not found. */
static char *find_bold_font_path(const char *regular_path) {
    if (!regular_path)
        return NULL;

    size_t path_len = strlen(regular_path);
    if (path_len < 5)
        return NULL; /* Too short for .ttf */

    /* Allocate buffer for bold path (extra space for "-Bold" suffix) */
    char *bold_path = (char *)malloc(path_len + 16);
    if (!bold_path)
        return NULL;

    /* Strategy 1: Replace -Regular.ttf with -Bold.ttf */
    const char *regular_suffix = strstr(regular_path, "-Regular.ttf");
    if (!regular_suffix)
        regular_suffix = strstr(regular_path, "-Regular.TTF");
    if (regular_suffix) {
        size_t prefix_len = regular_suffix - regular_path;
        memcpy(bold_path, regular_path, prefix_len);
        strcpy(bold_path + prefix_len, "-Bold.ttf");
        FILE *test = fopen(bold_path, "rb");
        if (test) {
            fclose(test);
            return bold_path;
        }
    }

    /* Strategy 2: Insert -Bold before .ttf (for fonts like DejaVuSansMono.ttf) */
    const char *ttf_ext = strstr(regular_path, ".ttf");
    if (!ttf_ext)
        ttf_ext = strstr(regular_path, ".TTF");
    if (ttf_ext && !strstr(regular_path, "-Regular")) {
        size_t prefix_len = ttf_ext - regular_path;
        memcpy(bold_path, regular_path, prefix_len);
        strcpy(bold_path + prefix_len, "-Bold.ttf");
        FILE *test = fopen(bold_path, "rb");
        if (test) {
            fclose(test);
            return bold_path;
        }
    }

    /* Strategy 3: Windows system font pattern - insert 'b' before .ttf (consola -> consolab) */
    if (ttf_ext) {
        size_t prefix_len = ttf_ext - regular_path;
        memcpy(bold_path, regular_path, prefix_len);
        bold_path[prefix_len] = 'b';
        strcpy(bold_path + prefix_len + 1, ".ttf");
        FILE *test = fopen(bold_path, "rb");
        if (test) {
            fclose(test);
            return bold_path;
        }
    }

    /* Strategy 4: Windows system font pattern - insert 'bd' before .ttf (cour -> courbd) */
    if (ttf_ext) {
        size_t prefix_len = ttf_ext - regular_path;
        memcpy(bold_path, regular_path, prefix_len);
        bold_path[prefix_len] = 'b';
        bold_path[prefix_len + 1] = 'd';
        strcpy(bold_path + prefix_len + 2, ".ttf");
        FILE *test = fopen(bold_path, "rb");
        if (test) {
            fclose(test);
            return bold_path;
        }
    }

    free(bold_path);
    return NULL;
}

/* Load an emoji/symbol font at fixed size, returns NULL if not found or failed */
static TTF_Font *load_emoji_font(const char *(*find_func)(void), const char *name, int size, int hdpi, int vdpi) {
    (void)name; /* Used only for debugging if needed */
    const char *path = find_func();
    if (!path) {
        return NULL;
    }
#if HAVE_SDL_TTF_DPI
    TTF_Font *font = TTF_OpenFontDPI(path, size, hdpi, vdpi);
#else
    (void)hdpi; /* Not used when DPI support is not available */
    (void)vdpi; /* Not used when DPI support is not available */
    TTF_Font *font = TTF_OpenFont(path, size);
#endif
    return font;
}

GlyphCache *glyph_cache_create(SDL_Renderer *renderer, const char *font_path, const char *font_name, int font_size,
                               int hinting_mode, SDL_ScaleMode scale_mode, int hdpi, int vdpi, int metrics_only) {
    GlyphCache *cache = (GlyphCache *)malloc(sizeof(GlyphCache));
    if (!cache)
        return NULL;

    memset(cache, 0, sizeof(GlyphCache));
    cache->backend_type = GLYPH_CACHE_BACKEND_SDL_TTF;
    cache->renderer = renderer;

    /* Try to load the requested font with DPI awareness if available */
#if HAVE_SDL_TTF_DPI
    cache->font = TTF_OpenFontDPI(font_path, font_size, hdpi, vdpi);
    if (!cache->font) {
        fprintf(stderr, "SDL_ttf: Failed to load font '%s': %s\n", font_path, TTF_GetError());
        free(cache);
        return NULL;
    }
#else
    /* Fallback to non-DPI version for older SDL_ttf */
    cache->font = TTF_OpenFont(font_path, font_size);
    if (!cache->font) {
        fprintf(stderr, "SDL_ttf: Failed to load font '%s': %s\n", font_path, TTF_GetError());
        free(cache);
        return NULL;
    }
#endif

    /* Store font path and name */
    cache->font_path = strdup(font_path);
    cache->font_name = strdup(font_name);

    /* Set font rendering style - use provided hinting mode */
    TTF_SetFontHinting(cache->font, hinting_mode);

    /* Disable kerning for monospaced fonts to ensure uniform spacing */
    TTF_SetFontKerning(cache->font, 0);

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

    /* Store space character's minx as baseline for consistent positioning */
    cache->space_minx = minx;

    /* Store font size for fallback font scaling */
    cache->font_size = font_size;

#if HAVE_SDL_TTF_DPI
    fprintf(stderr, "SDL_ttf: Loaded %s (cell %dx%d, DPI %dx%d)%s\n", font_name, cache->cell_w, cache->cell_h, hdpi,
            vdpi, metrics_only ? " [metrics only]" : "");
#else
    fprintf(stderr, "SDL_ttf: Loaded %s (cell %dx%d, no DPI)%s\n", font_name, cache->cell_w, cache->cell_h,
            metrics_only ? " [metrics only]" : "");
#endif

    /* Skip loading fallback fonts if only metrics are needed */
    if (!metrics_only) {
        /* Load bold font if available */
        char *bold_path = find_bold_font_path(font_path);
        if (bold_path) {
#if HAVE_SDL_TTF_DPI
            cache->bold_font = TTF_OpenFontDPI(bold_path, font_size, hdpi, vdpi);
#else
            cache->bold_font = TTF_OpenFont(bold_path, font_size);
#endif
            if (cache->bold_font) {
                TTF_SetFontHinting(cache->bold_font, hinting_mode);
                TTF_SetFontKerning(cache->bold_font, 0);
            }
            free(bold_path);
        }

        /* Load emoji/symbol fonts at same size as main font (renders at correct scale) */
        cache->emoji_font = load_emoji_font(find_emoji_font, "Emoji", font_size, hdpi, vdpi);
        cache->symbol_font = load_emoji_font(find_symbol_font, "Symbol", font_size, hdpi, vdpi);

        fprintf(stderr, "SDL_ttf: Fallback fonts: bold=%s, emoji=%s, symbol=%s\n", cache->bold_font ? "yes" : "no",
                cache->emoji_font ? "yes" : "no", cache->symbol_font ? "yes" : "no");
    }

    return cache;
}

GlyphCache *glyph_cache_create_with_backend(GlyphCacheBackendType backend, SDL_Renderer *renderer,
                                            const char *font_path, const char *font_name, int font_size,
                                            int hinting_mode, SDL_ScaleMode scale_mode, int hdpi, int vdpi,
                                            int use_cleartype, int metrics_only) {
#ifdef _WIN32
    if (backend == GLYPH_CACHE_BACKEND_DIRECTWRITE) {
        return glyph_cache_create_directwrite(renderer, font_path, font_name, font_size, hinting_mode, scale_mode, hdpi,
                                              vdpi, use_cleartype, metrics_only);
    }
#else
    (void)use_cleartype; /* Unused on non-Windows */
#endif
    (void)backend; /* Default to SDL_ttf */
    return glyph_cache_create(renderer, font_path, font_name, font_size, hinting_mode, scale_mode, hdpi, vdpi,
                              metrics_only);
}

SDL_Texture *glyph_cache_get(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color, int bold,
                             int italic, int is_emoji) {
#ifdef _WIN32
    /* Dispatch to DirectWrite backend if active */
    if (cache->backend_type == GLYPH_CACHE_BACKEND_DIRECTWRITE) {
        return glyph_cache_directwrite_get(cache, codepoint, fg_color, bg_color, bold, italic, is_emoji);
    }
#endif
    /* Create cache key - include is_emoji in key to cache both versions */
    uint32_t key = glyph_cache_hash_key(codepoint, fg_color, bg_color, bold, italic);
    if (is_emoji)
        key ^= 0x80000000; /* Flip high bit for emoji variant */
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

    /* Render the glyph - try main font first, but only if it provides the glyph */
    /* If is_emoji is set and emoji font is available, skip main font */
    SDL_Surface *surface = NULL;
    int main_font_has_glyph = 0;
    int use_main_font = 1;

    /* Determine if we should skip main font and use a fallback */
    int use_symbol_font = 0;
    int use_emoji_font = 0;

    /* Emoji/symbols: prefer emoji font, fall back to symbol font */
    if (is_emoji) {
        use_main_font = 0;
        use_emoji_font = 1;  /* Try emoji font first */
        use_symbol_font = 1; /* Fall back to symbol font if emoji doesn't have it */
    }

    /* Check if main font provides this glyph */
    if (use_main_font) {
#if SDL_TTF_MAJOR_VERSION > 2 || (SDL_TTF_MAJOR_VERSION == 2 && SDL_TTF_MINOR_VERSION >= 20)
        main_font_has_glyph = TTF_GlyphIsProvided32(cache->font, codepoint);
#else
        if (codepoint < 0x10000) {
            main_font_has_glyph = TTF_GlyphIsProvided(cache->font, (uint16_t)codepoint);
        }
#endif
        /* If main font doesn't have glyph, allow fallback to symbol font */
        if (!main_font_has_glyph) {
            use_symbol_font = 1;
        }
    }

    if (main_font_has_glyph) {
        /* Select font: use bold font file if available and bold requested, otherwise main font */
        TTF_Font *render_font = (bold && cache->bold_font) ? cache->bold_font : cache->font;

        /* Apply style: only use algorithmic bold if bold requested but no bold font file */
        int render_style = TTF_STYLE_NORMAL;
        if (bold && !cache->bold_font)
            render_style |= TTF_STYLE_BOLD; /* Algorithmic fallback */
        if (italic)
            render_style |= TTF_STYLE_ITALIC;
        TTF_SetFontStyle(render_font, render_style);

        /* For BMP characters (< 0x10000), use glyph rendering */
        if (codepoint < 0x10000) {
            surface = TTF_RenderGlyph_Blended(render_font, (uint16_t)codepoint, fg_color);
        } else {
            /* For higher codepoints, use UTF-8 rendering */
            char utf8[5];
            utf8_put_codepoint(codepoint, utf8);
            surface = TTF_RenderUTF8_Blended(render_font, utf8, fg_color);
        }

        /* Reset font style */
        TTF_SetFontStyle(render_font, TTF_STYLE_NORMAL);
    }

    /* Track if we used an emoji/symbol font (for scale mode selection) */
    int used_emoji_font = 0;

    /* Try emoji font first for emoji and symbols, but only if it has the glyph */
    if (!surface && use_emoji_font && cache->emoji_font) {
        /* Check if emoji font actually has this glyph before trying to render */
        if (TTF_GlyphIsProvided32(cache->emoji_font, codepoint)) {
            TTF_SetFontStyle(cache->emoji_font, style);
            char utf8[5];
            utf8_put_codepoint(codepoint, utf8);
            surface = TTF_RenderUTF8_Blended(cache->emoji_font, utf8, fg_color);
            TTF_SetFontStyle(cache->emoji_font, TTF_STYLE_NORMAL);
            used_emoji_font = 1;
        }
    }

    /* Fall back to symbol font if emoji font didn't have the glyph */
    if (!surface && use_symbol_font && cache->symbol_font) {
        TTF_SetFontStyle(cache->symbol_font, style);

        /* For BMP characters, use glyph rendering for tight bounds (like main font) */
        if (codepoint < 0x10000) {
            surface = TTF_RenderGlyph_Blended(cache->symbol_font, (uint16_t)codepoint, fg_color);
        } else {
            char utf8[5];
            utf8_put_codepoint(codepoint, utf8);
            surface = TTF_RenderUTF8_Blended(cache->symbol_font, utf8, fg_color);
        }

        TTF_SetFontStyle(cache->symbol_font, TTF_STYLE_NORMAL);
        used_emoji_font = 1;
    }

    if (!surface)
        return NULL;

    /* Create texture with alpha channel for smooth anti-aliased rendering */
    /* Use SDL_TEXTUREACCESS_STATIC for best performance with cached glyphs */
    SDL_Texture *texture = SDL_CreateTextureFromSurface(cache->renderer, surface);
    if (texture) {
        /* Use linear scaling for any remaining scaling during render */
        /* Use configured scale mode for main font (usually nearest for pixel-perfect text) */
        SDL_SetTextureScaleMode(texture, used_emoji_font ? SDL_ScaleModeLinear : cache->scale_mode);
        /* Enable alpha blending for smooth edges */
        SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_BLEND);
    }
    SDL_FreeSurface(surface);

    if (texture) {
        /* Get actual texture dimensions */
        int tex_w, tex_h;
        SDL_QueryTexture(texture, NULL, NULL, &tex_w, &tex_h);

        /* Get glyph metrics for positioning */
        int minx = 0, maxx, miny, maxy, advance;
        if (codepoint < 0x10000) {
            TTF_GlyphMetrics(cache->font, (uint16_t)codepoint, &minx, &maxx, &miny, &maxy, &advance);
        } else {
            /* TTF_GlyphMetrics32 is available in newer SDL_ttf versions, or use fallback */
#if SDL_TTF_MAJOR_VERSION > 2 || (SDL_TTF_MAJOR_VERSION == 2 && SDL_TTF_MINOR_VERSION >= 20)
            TTF_GlyphMetrics32(cache->font, codepoint, &minx, &maxx, &miny, &maxy, &advance);
#else
            /* Fallback for older SDL_ttf: Emoji usually don't need minx adjust */
            minx = 0;
#endif
        }

        /* Store in cache */
        if (cache->cache[slot].texture) {
            SDL_DestroyTexture(cache->cache[slot].texture);
        }
        cache->cache[slot].key = key;
        cache->cache[slot].texture = texture;
        cache->cache[slot].cell_width = tex_w;
        cache->cache[slot].cell_height = tex_h;
        cache->cache[slot].minx = minx;
    }

    return texture;
}

int glyph_cache_get_glyph_width(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color) {
    (void)bg_color; /* Unused - we use black background for width measurement */
    if (!cache)
        return 0;

    /* Ensure glyph is cached by calling glyph_cache_get */
    SDL_Color black_bg = {0, 0, 0, 255};
    glyph_cache_get(cache, codepoint, fg_color, black_bg, 0, 0, 0);

    /* Look up the cached entry */
    uint32_t key = glyph_cache_hash_key(codepoint, fg_color, black_bg, 0, 0);
    uint32_t slot = key % cache->cache_size;

    if (cache->cache[slot].key == key && cache->cache[slot].texture) {
        return cache->cache[slot].cell_width;
    }

    /* Fallback to default cell width if not found */
    return cache->cell_w;
}

int glyph_cache_get_minx(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color) {
    (void)bg_color;
    if (!cache)
        return 0;

    /* Ensure glyph is cached */
    SDL_Color black_bg = {0, 0, 0, 255};
    glyph_cache_get(cache, codepoint, fg_color, black_bg, 0, 0, 0);

    /* Look up cache entry */
    uint32_t key = glyph_cache_hash_key(codepoint, fg_color, black_bg, 0, 0);
    int slot = key % cache->cache_size;

    if (cache->cache[slot].key == key && cache->cache[slot].texture) {
        /* Return normalized minx relative to space character baseline for consistent spacing */
        return cache->cache[slot].minx - cache->space_minx;
    }
    return 0;
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

#ifdef _WIN32
    /* Dispatch to DirectWrite backend if active */
    if (cache->backend_type == GLYPH_CACHE_BACKEND_DIRECTWRITE) {
        glyph_cache_directwrite_destroy(cache);
        return;
    }
#endif

    if (cache->font) {
        TTF_CloseFont(cache->font);
    }

    if (cache->bold_font) {
        TTF_CloseFont(cache->bold_font);
    }

    if (cache->emoji_font) {
        TTF_CloseFont(cache->emoji_font);
    }

    if (cache->symbol_font) {
        TTF_CloseFont(cache->symbol_font);
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
