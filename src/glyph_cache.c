/* Glyph cache implementation */

#include "glyph_cache_internal.h"
#include "../../telnet-lisp/include/utf8.h"
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include "glyph_cache_directwrite.h"
#endif

/* Box filter (area averaging) downscaling for high-quality emoji rendering */
/* This produces much better results than bilinear for large downscaling ratios */
static SDL_Surface *box_filter_scale(SDL_Surface *src, int dst_w, int dst_h) {
    if (!src || dst_w <= 0 || dst_h <= 0)
        return NULL;

    /* Create destination surface with same format as source */
    SDL_Surface *dst = SDL_CreateRGBSurfaceWithFormat(0, dst_w, dst_h, src->format->BitsPerPixel, src->format->format);
    if (!dst)
        return NULL;

    /* Lock surfaces for direct pixel access */
    if (SDL_MUSTLOCK(src))
        SDL_LockSurface(src);
    if (SDL_MUSTLOCK(dst))
        SDL_LockSurface(dst);

    int src_w = src->w;
    int src_h = src->h;

    /* Get color masks for proper channel extraction */
    SDL_PixelFormat *fmt = src->format;
    uint32_t rmask = fmt->Rmask;
    uint32_t gmask = fmt->Gmask;
    uint32_t bmask = fmt->Bmask;
    uint32_t amask = fmt->Amask;
    uint8_t rshift = fmt->Rshift;
    uint8_t gshift = fmt->Gshift;
    uint8_t bshift = fmt->Bshift;
    uint8_t ashift = fmt->Ashift;

    /* Calculate scaling ratios */
    float scale_x = (float)src_w / dst_w;
    float scale_y = (float)src_h / dst_h;

    uint32_t *dst_pixels = (uint32_t *)dst->pixels;
    uint32_t *src_pixels = (uint32_t *)src->pixels;
    int src_pitch = src->pitch / 4; /* pitch in uint32_t units */
    int dst_pitch = dst->pitch / 4;

    for (int dy = 0; dy < dst_h; dy++) {
        for (int dx = 0; dx < dst_w; dx++) {
            /* Calculate source region for this destination pixel */
            int sx_start = (int)(dx * scale_x);
            int sy_start = (int)(dy * scale_y);
            int sx_end = (int)((dx + 1) * scale_x);
            int sy_end = (int)((dy + 1) * scale_y);

            /* Clamp to source bounds */
            if (sx_end > src_w)
                sx_end = src_w;
            if (sy_end > src_h)
                sy_end = src_h;

            /* Accumulate color values */
            uint32_t r_sum = 0, g_sum = 0, b_sum = 0, a_sum = 0;
            int count = 0;

            for (int sy = sy_start; sy < sy_end; sy++) {
                for (int sx = sx_start; sx < sx_end; sx++) {
                    uint32_t pixel = src_pixels[sy * src_pitch + sx];
                    /* Extract channels using format masks */
                    r_sum += (pixel & rmask) >> rshift;
                    g_sum += (pixel & gmask) >> gshift;
                    b_sum += (pixel & bmask) >> bshift;
                    if (amask)
                        a_sum += (pixel & amask) >> ashift;
                    else
                        a_sum += 255;
                    count++;
                }
            }

            /* Average and write destination pixel */
            if (count > 0) {
                uint32_t r = r_sum / count;
                uint32_t g = g_sum / count;
                uint32_t b = b_sum / count;
                uint32_t a = a_sum / count;
                dst_pixels[dy * dst_pitch + dx] = (r << rshift) | (g << gshift) | (b << bshift) | (a << ashift);
            }
        }
    }

    if (SDL_MUSTLOCK(dst))
        SDL_UnlockSurface(dst);
    if (SDL_MUSTLOCK(src))
        SDL_UnlockSurface(src);

    return dst;
}

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

/* Load an emoji/symbol font at fixed size, returns NULL if not found or failed */
static TTF_Font *load_emoji_font(const char *(*find_func)(void), const char *name, int size, int hdpi, int vdpi) {
    const char *path = find_func();
    if (!path) {
        fprintf(stderr, "No %s font found on system\n", name);
        return NULL;
    }
#if HAVE_SDL_TTF_DPI
    TTF_Font *font = TTF_OpenFontDPI(path, size, hdpi, vdpi);
#else
    TTF_Font *font = TTF_OpenFont(path, size);
#endif
    if (font) {
        fprintf(stderr, "%s font loaded at %dpt: %s\n", name, size, path);
    } else {
        fprintf(stderr, "Failed to load %s font: %s (%s)\n", name, path, TTF_GetError());
    }
    return font;
}

GlyphCache *glyph_cache_create(SDL_Renderer *renderer, const char *font_path, const char *font_name, int font_size,
                               int hinting_mode, SDL_ScaleMode scale_mode, int hdpi, int vdpi) {
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
        fprintf(stderr, "Font loading error: Failed to load font from '%s' with DPI (%d, %d): %s\n", font_path, hdpi,
                vdpi, TTF_GetError());
        free(cache);
        return NULL;
    }
    fprintf(stderr, "Font loaded with DPI: %dx%d at %dpt\n", hdpi, vdpi, font_size);
#else
    /* Fallback to non-DPI version for older SDL_ttf */
    cache->font = TTF_OpenFont(font_path, font_size);
    if (!cache->font) {
        fprintf(stderr, "Font loading error: Failed to load font from '%s': %s\n", font_path, TTF_GetError());
        free(cache);
        return NULL;
    }
    fprintf(stderr, "Font loaded (no DPI support) at %dpt\n", font_size);
#endif

    /* Verify font loaded successfully */
    fprintf(stderr, "Font loaded successfully from: %s\n", font_path);

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

    /* Load emoji/symbol fonts at large fixed size (128pt) for quality scaling */
    cache->emoji_font = load_emoji_font(find_emoji_font, "Emoji", 128, hdpi, vdpi);
    cache->symbol_font = load_emoji_font(find_symbol_font, "Symbol", 128, hdpi, vdpi);

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
    (void)backend;      /* Default to SDL_ttf */
    (void)metrics_only; /* SDL_ttf backend doesn't use this yet */
    return glyph_cache_create(renderer, font_path, font_name, font_size, hinting_mode, scale_mode, hdpi, vdpi);
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
        /* Apply style to font */
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

    /* For emoji font, pre-scale using box filter for better quality */
    /* This is much better than SDL's bilinear for large downscaling ratios */
    SDL_Surface *final_surface = surface;
    if (used_emoji_font && surface->h > cache->cell_h * 2) {
        /* Scale to approximately 2x cell height, maintaining aspect ratio */
        int target_h = cache->cell_h * 2;
        float scale = (float)target_h / surface->h;
        int target_w = (int)(surface->w * scale);

        SDL_Surface *scaled = box_filter_scale(surface, target_w, target_h);
        if (scaled) {
            SDL_FreeSurface(surface);
            final_surface = scaled;
        }
    }

    /* Create texture with alpha channel for smooth anti-aliased rendering */
    /* Use SDL_TEXTUREACCESS_STATIC for best performance with cached glyphs */
    SDL_Texture *texture = SDL_CreateTextureFromSurface(cache->renderer, final_surface);
    if (texture) {
        /* Use linear scaling for any remaining scaling during render */
        /* Use configured scale mode for main font (usually nearest for pixel-perfect text) */
        SDL_SetTextureScaleMode(texture, used_emoji_font ? SDL_ScaleModeLinear : cache->scale_mode);
        /* Enable alpha blending for smooth edges */
        SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_BLEND);
    }
    SDL_FreeSurface(final_surface);

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
