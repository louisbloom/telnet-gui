/* DirectWrite glyph cache backend (Windows only) */

#ifdef _WIN32

#define COBJMACROS /* Enable C-style COM macros */
#define INITGUID   /* Define GUIDs in this compilation unit */
#define WIN32_LEAN_AND_MEAN

#include "glyph_cache_directwrite.h"
#include "glyph_cache_internal.h"
#include "../../telnet-lisp/include/file_utils.h"
#include "../../telnet-lisp/include/utf8.h"

#include <windows.h>
#include <initguid.h>
#include <dwrite.h>
#include <dwrite_2.h> /* For color glyph support (Windows 8.1+) */
#include <d2d1.h>
#include <d2d1_1.h>   /* For ID2D1DeviceContext */
#include <wincodec.h> /* For WIC bitmap */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

/* Global DirectWrite factory (initialized once) */
static IDWriteFactory *g_dwrite_factory = NULL;
static IDWriteFactory2 *g_dwrite_factory2 = NULL; /* For color glyph support */
static int g_dwrite_initialized = 0;
static int g_color_glyphs_supported = 0;

/* Global Direct2D and WIC factories for color emoji rendering */
static ID2D1Factory *g_d2d_factory = NULL;
static IWICImagingFactory *g_wic_factory = NULL;

int directwrite_init(void) {
    if (g_dwrite_initialized)
        return 0;

    HRESULT hr = DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, &IID_IDWriteFactory, (IUnknown **)&g_dwrite_factory);

    if (FAILED(hr)) {
        fprintf(stderr, "DirectWrite: Failed to create factory (HRESULT=0x%08lx)\n", hr);
        return -1;
    }

    /* Try to get IDWriteFactory2 for color glyph support (Windows 8.1+) */
    hr = IDWriteFactory_QueryInterface(g_dwrite_factory, &IID_IDWriteFactory2, (void **)&g_dwrite_factory2);
    if (SUCCEEDED(hr) && g_dwrite_factory2) {
        g_color_glyphs_supported = 1;
        fprintf(stderr, "DirectWrite: Color glyph support available (IDWriteFactory2)\n");
    } else {
        g_dwrite_factory2 = NULL;
        g_color_glyphs_supported = 0;
        fprintf(stderr, "DirectWrite: Color glyph support not available (Windows 8.1+ required)\n");
    }

    /* Create Direct2D factory for color emoji rendering with proper alpha */
    D2D1_FACTORY_OPTIONS d2d_options = {D2D1_DEBUG_LEVEL_NONE};
    hr = D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, &IID_ID2D1Factory, &d2d_options, (void **)&g_d2d_factory);
    if (FAILED(hr) || !g_d2d_factory) {
        fprintf(stderr, "DirectWrite: Failed to create D2D factory (HRESULT=0x%08lx)\n", hr);
        g_d2d_factory = NULL;
    } else {
        fprintf(stderr, "DirectWrite: D2D factory created for color emoji\n");
    }

    /* Create WIC imaging factory for bitmap creation */
    hr = CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
    if (SUCCEEDED(hr) || hr == S_FALSE || hr == RPC_E_CHANGED_MODE) {
        hr = CoCreateInstance(&CLSID_WICImagingFactory, NULL, CLSCTX_INPROC_SERVER, &IID_IWICImagingFactory,
                              (void **)&g_wic_factory);
        if (FAILED(hr) || !g_wic_factory) {
            fprintf(stderr, "DirectWrite: Failed to create WIC factory (HRESULT=0x%08lx)\n", hr);
            g_wic_factory = NULL;
        } else {
            fprintf(stderr, "DirectWrite: WIC factory created for bitmap rendering\n");
        }
    }

    g_dwrite_initialized = 1;
    fprintf(stderr, "DirectWrite: Factory initialized successfully\n");
    return 0;
}

void directwrite_shutdown(void) {
    if (g_wic_factory) {
        IWICImagingFactory_Release(g_wic_factory);
        g_wic_factory = NULL;
    }
    if (g_d2d_factory) {
        ID2D1Factory_Release(g_d2d_factory);
        g_d2d_factory = NULL;
    }
    if (g_dwrite_factory2) {
        IDWriteFactory2_Release(g_dwrite_factory2);
        g_dwrite_factory2 = NULL;
    }
    if (g_dwrite_factory) {
        IDWriteFactory_Release(g_dwrite_factory);
        g_dwrite_factory = NULL;
    }
    g_dwrite_initialized = 0;
    g_color_glyphs_supported = 0;
}

/* Load a font from a file path and return an IDWriteFontFace */
static IDWriteFontFace *load_font_from_path(const char *utf8_path) {
    if (!utf8_path || !g_dwrite_factory)
        return NULL;

    /* Convert UTF-8 path to UTF-16 */
    wchar_t *wpath = utf8_to_utf16(utf8_path);
    if (!wpath)
        return NULL;

    IDWriteFontFile *font_file = NULL;
    HRESULT hr = IDWriteFactory_CreateFontFileReference(g_dwrite_factory, wpath, NULL, &font_file);
    free(wpath);

    if (FAILED(hr) || !font_file) {
        fprintf(stderr, "DirectWrite: Failed to create font file reference for '%s'\n", utf8_path);
        return NULL;
    }

    /* Check if font is supported */
    BOOL is_supported = FALSE;
    DWRITE_FONT_FILE_TYPE file_type;
    DWRITE_FONT_FACE_TYPE face_type;
    UINT32 num_faces = 0;

    hr = IDWriteFontFile_Analyze(font_file, &is_supported, &file_type, &face_type, &num_faces);
    if (FAILED(hr) || !is_supported || num_faces == 0) {
        fprintf(stderr, "DirectWrite: Font file '%s' is not supported\n", utf8_path);
        IDWriteFontFile_Release(font_file);
        return NULL;
    }

    /* Create font face */
    IDWriteFontFace *font_face = NULL;
    IDWriteFontFile *files[] = {font_file};
    hr = IDWriteFactory_CreateFontFace(g_dwrite_factory, face_type, 1, files, 0, DWRITE_FONT_SIMULATIONS_NONE,
                                       &font_face);
    IDWriteFontFile_Release(font_file);

    if (FAILED(hr) || !font_face) {
        fprintf(stderr, "DirectWrite: Failed to create font face for '%s'\n", utf8_path);
        return NULL;
    }

    fprintf(stderr, "DirectWrite: Loaded font face from '%s'\n", utf8_path);
    return font_face;
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

/* Try to find system emoji font path */
static const char *find_emoji_font_path(void) {
    static const char *fonts[] = {"C:/Windows/Fonts/seguiemj.ttf", "C:\\Windows\\Fonts\\seguiemj.ttf", NULL};
    return find_first_existing_font(fonts);
}

/* Try to find system symbol font path */
static const char *find_symbol_font_path(void) {
    static const char *fonts[] = {"C:/Windows/Fonts/seguisym.ttf", "C:\\Windows\\Fonts\\seguisym.ttf", NULL};
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

/* Render color emoji using Direct2D with proper alpha channel support.
 * Creates a WIC bitmap, renders with D2D, and returns an SDL_Surface with correct alpha.
 * Returns NULL if color rendering failed or not available. */
static SDL_Surface *render_color_emoji_d2d(const DWRITE_GLYPH_RUN *glyph_run, float baseline_x, float baseline_y,
                                           int width, int height) {
    if (!g_color_glyphs_supported || !g_dwrite_factory2 || !g_d2d_factory || !g_wic_factory)
        return NULL;

    /* Check if this glyph has color layers */
    DWRITE_MATRIX world_transform = {1.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f};
    IDWriteColorGlyphRunEnumerator *color_enumerator = NULL;
    HRESULT hr =
        IDWriteFactory2_TranslateColorGlyphRun(g_dwrite_factory2, baseline_x, baseline_y, glyph_run, NULL,
                                               DWRITE_MEASURING_MODE_NATURAL, &world_transform, 0, &color_enumerator);

    if (hr == DWRITE_E_NOCOLOR || FAILED(hr) || !color_enumerator) {
        return NULL; /* No color layers */
    }

    /* Create WIC bitmap with premultiplied alpha */
    IWICBitmap *wic_bitmap = NULL;
    hr = IWICImagingFactory_CreateBitmap(g_wic_factory, width, height, &GUID_WICPixelFormat32bppPBGRA,
                                         WICBitmapCacheOnDemand, &wic_bitmap);
    if (FAILED(hr) || !wic_bitmap) {
        IDWriteColorGlyphRunEnumerator_Release(color_enumerator);
        return NULL;
    }

    /* Create D2D render target from WIC bitmap */
    D2D1_RENDER_TARGET_PROPERTIES rt_props = {D2D1_RENDER_TARGET_TYPE_DEFAULT,
                                              {DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED},
                                              0.0f,
                                              0.0f,
                                              D2D1_RENDER_TARGET_USAGE_NONE,
                                              D2D1_FEATURE_LEVEL_DEFAULT};

    ID2D1RenderTarget *d2d_rt = NULL;
    hr = ID2D1Factory_CreateWicBitmapRenderTarget(g_d2d_factory, wic_bitmap, &rt_props, &d2d_rt);
    if (FAILED(hr) || !d2d_rt) {
        IWICBitmap_Release(wic_bitmap);
        IDWriteColorGlyphRunEnumerator_Release(color_enumerator);
        return NULL;
    }

    /* Begin drawing */
    ID2D1RenderTarget_BeginDraw(d2d_rt);

    /* Clear to transparent */
    D2D1_COLOR_F clear_color;
    clear_color.r = 0.0f;
    clear_color.g = 0.0f;
    clear_color.b = 0.0f;
    clear_color.a = 0.0f;
    ID2D1RenderTarget_Clear(d2d_rt, &clear_color);

    /* Render each color layer */
    BOOL has_run = FALSE;
    while (SUCCEEDED(IDWriteColorGlyphRunEnumerator_MoveNext(color_enumerator, &has_run)) && has_run) {
        const DWRITE_COLOR_GLYPH_RUN *color_run = NULL;
        hr = IDWriteColorGlyphRunEnumerator_GetCurrentRun(color_enumerator, &color_run);
        if (FAILED(hr) || !color_run)
            continue;

        /* Create brush with layer color */
        D2D1_COLOR_F layer_color;
        if (color_run->paletteIndex == 0xFFFF) {
            /* Foreground color placeholder - use white */
            layer_color.r = 1.0f;
            layer_color.g = 1.0f;
            layer_color.b = 1.0f;
            layer_color.a = 1.0f;
        } else {
            layer_color.r = color_run->runColor.r;
            layer_color.g = color_run->runColor.g;
            layer_color.b = color_run->runColor.b;
            layer_color.a = color_run->runColor.a;
        }

        ID2D1SolidColorBrush *brush = NULL;
        hr = ID2D1RenderTarget_CreateSolidColorBrush(d2d_rt, &layer_color, NULL, &brush);
        if (SUCCEEDED(hr) && brush) {
            D2D1_POINT_2F origin = {color_run->baselineOriginX, color_run->baselineOriginY};
            ID2D1RenderTarget_DrawGlyphRun(d2d_rt, origin, &color_run->glyphRun, (ID2D1Brush *)brush,
                                           DWRITE_MEASURING_MODE_NATURAL);
            ID2D1SolidColorBrush_Release(brush);
        }
    }

    /* End drawing */
    hr = ID2D1RenderTarget_EndDraw(d2d_rt, NULL, NULL);
    ID2D1RenderTarget_Release(d2d_rt);
    IDWriteColorGlyphRunEnumerator_Release(color_enumerator);

    if (FAILED(hr)) {
        IWICBitmap_Release(wic_bitmap);
        return NULL;
    }

    /* Lock WIC bitmap and copy to SDL surface */
    IWICBitmapLock *lock = NULL;
    WICRect lock_rect = {0, 0, width, height};
    hr = IWICBitmap_Lock(wic_bitmap, &lock_rect, WICBitmapLockRead, &lock);
    if (FAILED(hr) || !lock) {
        IWICBitmap_Release(wic_bitmap);
        return NULL;
    }

    UINT buffer_size = 0;
    BYTE *wic_data = NULL;
    hr = IWICBitmapLock_GetDataPointer(lock, &buffer_size, &wic_data);
    if (FAILED(hr) || !wic_data) {
        IWICBitmapLock_Release(lock);
        IWICBitmap_Release(wic_bitmap);
        return NULL;
    }

    UINT wic_stride = 0;
    IWICBitmapLock_GetStride(lock, &wic_stride);

    /* Create SDL surface */
    SDL_Surface *surface = SDL_CreateRGBSurfaceWithFormat(0, width, height, 32, SDL_PIXELFORMAT_BGRA32);
    if (!surface) {
        IWICBitmapLock_Release(lock);
        IWICBitmap_Release(wic_bitmap);
        return NULL;
    }

    /* Copy pixels, converting from premultiplied to straight alpha */
    SDL_LockSurface(surface);
    uint32_t *dst_pixels = (uint32_t *)surface->pixels;
    int dst_pitch = surface->pitch / 4;

    for (int y = 0; y < height; y++) {
        uint32_t *src_row = (uint32_t *)(wic_data + y * wic_stride);
        for (int x = 0; x < width; x++) {
            uint32_t pixel = src_row[x];

            /* WIC format is PBGRA (premultiplied) */
            uint8_t b = (pixel >> 0) & 0xFF;
            uint8_t g = (pixel >> 8) & 0xFF;
            uint8_t r = (pixel >> 16) & 0xFF;
            uint8_t a = (pixel >> 24) & 0xFF;

            /* Convert from premultiplied to straight alpha */
            if (a > 0 && a < 255) {
                r = (uint8_t)((r * 255) / a);
                g = (uint8_t)((g * 255) / a);
                b = (uint8_t)((b * 255) / a);
            }

            /* Output as BGRA */
            dst_pixels[y * dst_pitch + x] = ((uint32_t)a << 24) | ((uint32_t)r << 16) | ((uint32_t)g << 8) | b;
        }
    }

    SDL_UnlockSurface(surface);
    IWICBitmapLock_Release(lock);
    IWICBitmap_Release(wic_bitmap);

    return surface;
}

/* Render a glyph using DirectWrite and return an SDL_Surface */
static SDL_Surface *render_glyph_directwrite(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, int bold,
                                             int italic, int is_emoji) {
    (void)italic; /* Not yet implemented */

    if (!cache || !cache->dw_font_face)
        return NULL;

    /* Select primary font face: bold face if requested and available, otherwise regular */
    IDWriteFontFace *primary_face;
    if (bold && cache->dw_bold_face && !is_emoji) {
        primary_face = (IDWriteFontFace *)cache->dw_bold_face;
    } else {
        primary_face = (IDWriteFontFace *)cache->dw_font_face;
    }

    IDWriteFontFace *face = primary_face;
    IDWriteBitmapRenderTarget *render_target = (IDWriteBitmapRenderTarget *)cache->dw_render_target;
    IDWriteRenderingParams *render_params = (IDWriteRenderingParams *)cache->dw_render_params;
    int used_fallback_font = 0;

    /* Get glyph index for codepoint */
    UINT16 glyph_index = 0;
    UINT32 cp = codepoint;
    HRESULT hr;

    if (is_emoji) {
        /* For emoji, try color emoji font FIRST (skip main font which has monochrome glyphs) */
        IDWriteFontFace *emoji_face = (IDWriteFontFace *)cache->dw_emoji_face;
        if (emoji_face) {
            hr = IDWriteFontFace_GetGlyphIndices(emoji_face, &cp, 1, &glyph_index);
            if (SUCCEEDED(hr) && glyph_index != 0) {
                face = emoji_face;
                used_fallback_font = 1;
            }
        }

        /* Try symbol font as second choice */
        if (glyph_index == 0) {
            IDWriteFontFace *symbol_face = (IDWriteFontFace *)cache->dw_symbol_face;
            if (symbol_face) {
                hr = IDWriteFontFace_GetGlyphIndices(symbol_face, &cp, 1, &glyph_index);
                if (SUCCEEDED(hr) && glyph_index != 0) {
                    face = symbol_face;
                    used_fallback_font = 1;
                }
            }
        }

        /* Last resort: main font (monochrome fallback) */
        if (glyph_index == 0) {
            face = (IDWriteFontFace *)cache->dw_font_face;
            hr = IDWriteFontFace_GetGlyphIndices(face, &cp, 1, &glyph_index);
            used_fallback_font = 0;
        }
    } else {
        /* For non-emoji, try primary font first (bold or regular based on selection above) */
        hr = IDWriteFontFace_GetGlyphIndices(face, &cp, 1, &glyph_index);

        /* If main font doesn't have it, try fallback fonts */
        if (FAILED(hr) || glyph_index == 0) {
            IDWriteFontFace *emoji_face = (IDWriteFontFace *)cache->dw_emoji_face;
            if (emoji_face) {
                hr = IDWriteFontFace_GetGlyphIndices(emoji_face, &cp, 1, &glyph_index);
                if (SUCCEEDED(hr) && glyph_index != 0) {
                    face = emoji_face;
                    used_fallback_font = 1;
                }
            }

            if (glyph_index == 0) {
                IDWriteFontFace *symbol_face = (IDWriteFontFace *)cache->dw_symbol_face;
                if (symbol_face) {
                    hr = IDWriteFontFace_GetGlyphIndices(symbol_face, &cp, 1, &glyph_index);
                    if (SUCCEEDED(hr) && glyph_index != 0) {
                        face = symbol_face;
                        used_fallback_font = 1;
                    }
                }
            }
        }
    }

    if (glyph_index == 0) {
        return NULL; /* Glyph not available in any font */
    }

    /* Get font metrics */
    DWRITE_FONT_METRICS font_metrics;
    IDWriteFontFace_GetMetrics(face, &font_metrics);

    /* Calculate em_size for this specific font face
     * For fallback fonts, we need to scale them to fit our cell height */
    float em_size;
    if (used_fallback_font) {
        /* Calculate em_size that makes this font's ascent+descent = cell_h */
        float font_height_design = (float)(font_metrics.ascent + font_metrics.descent);
        em_size = cache->cell_h * font_metrics.designUnitsPerEm / font_height_design;
    } else {
        em_size = cache->dw_em_size;
    }
    float design_to_px = em_size / (float)font_metrics.designUnitsPerEm;

    /* Get glyph metrics to determine width and headroom */
    DWRITE_GLYPH_METRICS glyph_metrics;
    hr = IDWriteFontFace_GetDesignGlyphMetrics(face, &glyph_index, 1, &glyph_metrics, FALSE);
    if (FAILED(hr))
        return NULL;

    /* Auto-detect wide glyphs from font metrics.
     * If the glyph's advance width is significantly larger than cell_w,
     * render it in double-width mode. The renderer handles overlap protection. */
    float glyph_advance_px = glyph_metrics.advanceWidth * design_to_px;
    int is_wide_glyph = is_emoji || (glyph_advance_px > cache->cell_w * 1.5f);

    /* Determine output dimensions - must match cell grid expectations */
    int output_w = is_wide_glyph ? cache->cell_w * 2 : cache->cell_w;
    int output_h = cache->cell_h;

    /* For internal rendering, use extra headroom to capture glyphs that extend
     * beyond normal bounds. We'll crop to output_h when creating the SDL surface. */
    int render_headroom = (cache->cell_h + 4) / 5; /* 20% headroom for rendering */
    int render_h = cache->cell_h + render_headroom * 2;
    int glyph_w = output_w;
    int glyph_h = render_h;

    /* Ensure render target is large enough */
    SIZE current_size;
    hr = IDWriteBitmapRenderTarget_GetSize(render_target, &current_size);
    if (FAILED(hr) || current_size.cx < glyph_w || current_size.cy < glyph_h) {
        int new_w = glyph_w > current_size.cx ? glyph_w : current_size.cx;
        int new_h = glyph_h > current_size.cy ? glyph_h : current_size.cy;
        hr = IDWriteBitmapRenderTarget_Resize(render_target, new_w, new_h);
        if (FAILED(hr)) {
            fprintf(stderr, "DirectWrite: Failed to resize render target\n");
            return NULL;
        }
    }

    /* Get the DC for clearing and rendering */
    HDC hdc = IDWriteBitmapRenderTarget_GetMemoryDC(render_target);
    if (!hdc)
        return NULL;

    /* Clear the entire render target area to black */
    RECT clear_rect = {0, 0, glyph_w, glyph_h};
    HBRUSH black_brush = CreateSolidBrush(RGB(0, 0, 0));
    FillRect(hdc, &clear_rect, black_brush);
    DeleteObject(black_brush);

    /* Set up glyph run */
    DWRITE_GLYPH_RUN glyph_run = {0};
    glyph_run.fontFace = face;
    glyph_run.fontEmSize = em_size;
    glyph_run.glyphCount = 1;
    glyph_run.glyphIndices = &glyph_index;

    FLOAT advance = glyph_metrics.advanceWidth * design_to_px;
    glyph_run.glyphAdvances = &advance;

    DWRITE_GLYPH_OFFSET glyph_offset = {0, 0};
    glyph_run.glyphOffsets = &glyph_offset;

    /* Calculate baseline position - baseline at ascent distance from top */
    /* Add render_headroom offset to capture glyphs extending above ascent */
    float ascent_px = font_metrics.ascent * design_to_px;
    float baseline_y = ceilf(ascent_px) + (float)render_headroom;

    /* Position glyph horizontally */
    float baseline_x = 0.0f;
    if (is_wide_glyph) {
        /* Center wide glyphs in their double-width cell space */
        baseline_x = (glyph_w - glyph_advance_px) / 2.0f;
        if (baseline_x < 0)
            baseline_x = 0;
    }

    /* Apply bold simulation if requested */
    if (bold) {
        /* Could implement bold by rendering twice with slight offset */
    }

    /* Try Direct2D color emoji rendering first (proper alpha channel support) */
    if (is_emoji && used_fallback_font) {
        SDL_Surface *color_surface = render_color_emoji_d2d(&glyph_run, baseline_x, baseline_y, glyph_w, glyph_h);
        if (color_surface) {
            /* D2D rendered with proper alpha - crop to output dimensions */
            if (color_surface->w == output_w && color_surface->h == output_h) {
                return color_surface;
            }

            /* Need to crop from render size to output size */
            SDL_Surface *cropped = SDL_CreateRGBSurfaceWithFormat(0, output_w, output_h, 32, SDL_PIXELFORMAT_BGRA32);
            if (cropped) {
                SDL_LockSurface(color_surface);
                SDL_LockSurface(cropped);

                uint32_t *src_pixels = (uint32_t *)color_surface->pixels;
                uint32_t *dst_pixels = (uint32_t *)cropped->pixels;
                int src_pitch = color_surface->pitch / 4;
                int dst_pitch = cropped->pitch / 4;

                /* Copy with headroom offset */
                for (int y = 0; y < output_h && (y + render_headroom) < color_surface->h; y++) {
                    int src_y = y + render_headroom;
                    for (int x = 0; x < output_w && x < color_surface->w; x++) {
                        dst_pixels[y * dst_pitch + x] = src_pixels[src_y * src_pitch + x];
                    }
                }

                SDL_UnlockSurface(cropped);
                SDL_UnlockSurface(color_surface);
                SDL_FreeSurface(color_surface);
                return cropped;
            }
            SDL_FreeSurface(color_surface);
        }
        /* D2D failed, fall through to GDI rendering */
    }

    /* GDI-based rendering (monochrome or fallback)
     * IMPORTANT: Always render with WHITE regardless of fg_color. This ensures consistent
     * alpha coverage for all colors. If we render with the actual fg_color, colored text
     * gets lower alpha at edges because max(R,G,B) only sees the channels being used.
     * Example: Cyan (0,255,255) edge pixel might be (0,180,180) → alpha=180
     *          White (255,255,255) edge pixel might be (200,255,200) → alpha=255
     * By rendering white and applying fg_color later, all text gets equal antialiasing. */
    COLORREF text_color = RGB(255, 255, 255);
    hr = IDWriteBitmapRenderTarget_DrawGlyphRun(render_target, baseline_x, baseline_y, DWRITE_MEASURING_MODE_NATURAL,
                                                &glyph_run, render_params, text_color, NULL);
    if (FAILED(hr)) {
        fprintf(stderr, "DirectWrite: Failed to draw glyph run (HRESULT=0x%08lx)\n", hr);
        return NULL;
    }

    /* Get the actual bitmap from the render target */
    HBITMAP hbitmap = (HBITMAP)GetCurrentObject(hdc, OBJ_BITMAP);
    if (!hbitmap)
        return NULL;

    /* Get bitmap info to determine actual dimensions */
    BITMAP bm;
    GetObject(hbitmap, sizeof(bm), &bm);

    /* Create SDL surface matching OUTPUT dimensions (cell-sized, not render-sized) */
    SDL_Surface *surface = SDL_CreateRGBSurfaceWithFormat(0, output_w, output_h, 32, SDL_PIXELFORMAT_BGRA32);
    if (!surface)
        return NULL;

    /* Set up BITMAPINFO for GetDIBits - request our glyph size from top-left corner */
    BITMAPINFO bmi = {0};
    bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    bmi.bmiHeader.biWidth = bm.bmWidth;
    bmi.bmiHeader.biHeight = -bm.bmHeight; /* Negative for top-down */
    bmi.bmiHeader.biPlanes = 1;
    bmi.bmiHeader.biBitCount = 32;
    bmi.bmiHeader.biCompression = BI_RGB;

    /* Ensure pixel buffer is large enough (reusable, avoids repeated alloc/free) */
    size_t required_size = bm.bmWidth * bm.bmHeight * 4;
    if (dynamic_buffer_ensure_size(cache->dw_pixel_buf, required_size) < 0) {
        SDL_FreeSurface(surface);
        return NULL;
    }
    uint32_t *temp_pixels = (uint32_t *)cache->dw_pixel_buf->data;

    /* Get all bitmap data */
    int result = GetDIBits(hdc, hbitmap, 0, bm.bmHeight, temp_pixels, &bmi, DIB_RGB_COLORS);
    if (result == 0) {
        SDL_FreeSurface(surface);
        return NULL;
    }

    /* Copy and convert - extract cell-sized portion from center of render surface
     * Skip the top render_headroom rows to get the cell-aligned content */
    SDL_LockSurface(surface);
    uint32_t *dst_pixels = (uint32_t *)surface->pixels;
    int dst_pitch = surface->pitch / 4;

    for (int y = 0; y < output_h && (y + render_headroom) < bm.bmHeight; y++) {
        int src_y = y + render_headroom; /* Offset into render surface */
        for (int x = 0; x < output_w && x < bm.bmWidth; x++) {
            uint32_t pixel = temp_pixels[src_y * bm.bmWidth + x];

            /* Extract RGB (rendered on black background) */
            uint8_t r = (pixel >> 16) & 0xFF;
            uint8_t g = (pixel >> 8) & 0xFF;
            uint8_t b = pixel & 0xFF;

            /* Calculate alpha from max RGB component (text rendered on black) */
            uint8_t alpha = (r > g) ? ((r > b) ? r : b) : ((g > b) ? g : b);

            if (alpha > 0) {
                /* Monochrome: apply the requested foreground color with calculated alpha */
                dst_pixels[y * dst_pitch + x] =
                    ((uint32_t)alpha << 24) | ((uint32_t)fg_color.r << 16) | ((uint32_t)fg_color.g << 8) | fg_color.b;
            } else {
                dst_pixels[y * dst_pitch + x] = 0; /* Fully transparent */
            }
        }
    }
    SDL_UnlockSurface(surface);

    return surface;
}

/* External function to render a glyph (called from glyph_cache.c dispatch) */
SDL_Texture *glyph_cache_directwrite_get(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color,
                                         int bold, int italic, int is_emoji) {
    if (!cache || cache->backend_type != GLYPH_CACHE_BACKEND_DIRECTWRITE)
        return NULL;

    /* Create cache key */
    uint32_t key = glyph_cache_hash_key(codepoint, fg_color, bg_color, bold, italic);
    if (is_emoji)
        key ^= 0x80000000;

    int slot = key % cache->cache_size;

    /* Check if cached */
    if (cache->cache[slot].key == key && cache->cache[slot].texture) {
        return cache->cache[slot].texture;
    }

    /* Cache miss - render the glyph */
    SDL_Surface *surface = render_glyph_directwrite(cache, codepoint, fg_color, bold, italic, is_emoji);
    if (!surface)
        return NULL;

    /* Create texture */
    SDL_Texture *texture = SDL_CreateTextureFromSurface(cache->renderer, surface);
    SDL_FreeSurface(surface);

    if (texture) {
        SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_BLEND);
        SDL_SetTextureScaleMode(texture, cache->scale_mode);

        /* Store in cache */
        if (cache->cache[slot].texture) {
            SDL_DestroyTexture(cache->cache[slot].texture);
        }
        cache->cache[slot].key = key;
        cache->cache[slot].texture = texture;

        /* Query texture dimensions for metrics */
        int tex_w, tex_h;
        SDL_QueryTexture(texture, NULL, NULL, &tex_w, &tex_h);
        cache->cache[slot].cell_width = tex_w;
        cache->cache[slot].cell_height = tex_h;
        cache->cache[slot].minx = 0; /* DirectWrite handles positioning differently */
    }

    return texture;
}

GlyphCache *glyph_cache_create_directwrite(SDL_Renderer *renderer, const char *font_path, const char *font_name,
                                           int font_size, int hinting_mode, SDL_ScaleMode scale_mode, int hdpi,
                                           int vdpi, int use_cleartype, int metrics_only) {
    (void)hinting_mode; /* Not used for DirectWrite - it has its own rendering modes */
    (void)hdpi;         /* Only vdpi used for vertical font scaling */

    if (!g_dwrite_initialized) {
        if (directwrite_init() < 0)
            return NULL;
    }

    /* Allocate cache structure */
    GlyphCache *cache = (GlyphCache *)malloc(sizeof(GlyphCache));
    if (!cache)
        return NULL;

    memset(cache, 0, sizeof(GlyphCache));
    cache->backend_type = GLYPH_CACHE_BACKEND_DIRECTWRITE;
    cache->renderer = renderer;
    cache->scale_mode = scale_mode;

    /* Load main font */
    IDWriteFontFace *font_face = load_font_from_path(font_path);
    if (!font_face) {
        fprintf(stderr, "DirectWrite: Failed to load font '%s'\n", font_path);
        free(cache);
        return NULL;
    }
    cache->dw_font_face = font_face;

    /* Store font path and name */
    cache->font_path = strdup(font_path);
    cache->font_name = strdup(font_name);

    /* Calculate em size (pixels) from point size and display DPI.
     * Typography defines 1 point = 1/72 inch, so:
     *   pixels = points × (display_DPI / 72)
     * Example: 12pt font at 96 DPI = 12 × (96/72) = 16 pixels
     * The 72 is not an assumption - it's the definition of a typographic point. */
    float dpi_scale = (float)vdpi / 72.0f;
    cache->dw_em_size = (float)font_size * dpi_scale;
    cache->dw_use_cleartype = use_cleartype;

    /* Get font metrics to calculate cell dimensions */
    DWRITE_FONT_METRICS metrics;
    IDWriteFontFace_GetMetrics(font_face, &metrics);

    float design_to_px = cache->dw_em_size / (float)metrics.designUnitsPerEm;

    /* Cell dimensions */
    /* For monospace fonts, use average character width or 'M' width */
    /* Get glyph metrics for space character */
    UINT32 space_cp = ' ';
    UINT16 space_glyph;
    IDWriteFontFace_GetGlyphIndices(font_face, &space_cp, 1, &space_glyph);

    DWRITE_GLYPH_METRICS glyph_metrics;
    IDWriteFontFace_GetDesignGlyphMetrics(font_face, &space_glyph, 1, &glyph_metrics, FALSE);

    cache->cell_w = (int)ceilf(glyph_metrics.advanceWidth * design_to_px);
    cache->cell_h = (int)ceilf((metrics.ascent + metrics.descent) * design_to_px);
    cache->space_minx = 0; /* DirectWrite handles positioning differently */

    fprintf(stderr, "DirectWrite: Cell size %dx%d, em_size=%.1f\n", cache->cell_w, cache->cell_h, cache->dw_em_size);

    /* Create reusable pixel buffer for bitmap extraction */
    cache->dw_pixel_buf = dynamic_buffer_create(cache->cell_w * cache->cell_h * 4);
    if (!cache->dw_pixel_buf) {
        IDWriteFontFace_Release(font_face);
        free(cache->font_path);
        free(cache->font_name);
        free(cache);
        return NULL;
    }

    /* Allocate glyph cache */
    cache->cache_size = 1024;
    cache->cache = (CacheNode *)calloc(cache->cache_size, sizeof(CacheNode));
    if (!cache->cache) {
        IDWriteFontFace_Release(font_face);
        dynamic_buffer_destroy(cache->dw_pixel_buf);
        free(cache->font_path);
        free(cache->font_name);
        free(cache);
        return NULL;
    }

    /* Create GDI interop for bitmap rendering */
    IDWriteGdiInterop *gdi_interop = NULL;
    HRESULT hr = IDWriteFactory_GetGdiInterop(g_dwrite_factory, &gdi_interop);
    if (FAILED(hr) || !gdi_interop) {
        fprintf(stderr, "DirectWrite: Failed to get GDI interop\n");
        IDWriteFontFace_Release(font_face);
        free(cache->cache);
        free(cache->font_path);
        free(cache->font_name);
        free(cache);
        return NULL;
    }
    cache->dw_gdi_interop = gdi_interop;

    /* Create bitmap render target */
    IDWriteBitmapRenderTarget *render_target = NULL;
    HDC screen_dc = GetDC(NULL);
    hr = IDWriteGdiInterop_CreateBitmapRenderTarget(gdi_interop, screen_dc, cache->cell_w * 4, cache->cell_h * 2,
                                                    &render_target);
    ReleaseDC(NULL, screen_dc);

    if (FAILED(hr) || !render_target) {
        fprintf(stderr, "DirectWrite: Failed to create bitmap render target\n");
        IDWriteGdiInterop_Release(gdi_interop);
        IDWriteFontFace_Release(font_face);
        free(cache->cache);
        free(cache->font_path);
        free(cache->font_name);
        free(cache);
        return NULL;
    }
    cache->dw_render_target = render_target;

    /* Set pixels per DIP to 1.0 - we already account for DPI in em_size calculation */
    IDWriteBitmapRenderTarget_SetPixelsPerDip(render_target, 1.0f);

    /* Create rendering params */
    IDWriteRenderingParams *render_params = NULL;
    if (use_cleartype) {
        /* Use default system rendering params (includes ClearType settings) */
        hr = IDWriteFactory_CreateRenderingParams(g_dwrite_factory, &render_params);
    } else {
        /* Create custom params for grayscale rendering */
        IDWriteRenderingParams *default_params = NULL;
        hr = IDWriteFactory_CreateRenderingParams(g_dwrite_factory, &default_params);
        if (SUCCEEDED(hr) && default_params) {
            hr = IDWriteFactory_CreateCustomRenderingParams(
                g_dwrite_factory, IDWriteRenderingParams_GetGamma(default_params),
                IDWriteRenderingParams_GetEnhancedContrast(default_params),
                IDWriteRenderingParams_GetClearTypeLevel(default_params), DWRITE_PIXEL_GEOMETRY_FLAT,
                DWRITE_RENDERING_MODE_NATURAL_SYMMETRIC, &render_params);
            IDWriteRenderingParams_Release(default_params);
        }
    }

    if (FAILED(hr) || !render_params) {
        /* Fallback to default params */
        IDWriteFactory_CreateRenderingParams(g_dwrite_factory, &render_params);
    }
    cache->dw_render_params = render_params;

    /* Skip loading fallback fonts if only metrics are needed */
    if (!metrics_only) {
        /* Load bold font (try to find bold variant of main font) */
        char *bold_path = find_bold_font_path(font_path);
        if (bold_path) {
            cache->dw_bold_face = load_font_from_path(bold_path);
            if (cache->dw_bold_face) {
                fprintf(stderr, "DirectWrite: Loaded bold font from '%s'\n", bold_path);
            }
            free(bold_path);
        }
        if (!cache->dw_bold_face) {
            fprintf(stderr, "DirectWrite: Bold font not found, will use algorithmic bold\n");
        }

        /* Load emoji and symbol fonts */
        const char *emoji_path = find_emoji_font_path();
        if (emoji_path) {
            cache->dw_emoji_face = load_font_from_path(emoji_path);
        }

        const char *symbol_path = find_symbol_font_path();
        if (symbol_path) {
            cache->dw_symbol_face = load_font_from_path(symbol_path);
        }

        fprintf(stderr, "DirectWrite: Glyph cache created successfully (ClearType=%s, Bold=%s)\n",
                use_cleartype ? "enabled" : "disabled", cache->dw_bold_face ? "file" : "algorithmic");
    }
    return cache;
}

/* Destroy DirectWrite resources */
void glyph_cache_directwrite_destroy(GlyphCache *cache) {
    if (!cache || cache->backend_type != GLYPH_CACHE_BACKEND_DIRECTWRITE)
        return;

    /* Free pixel buffer */
    if (cache->dw_pixel_buf) {
        dynamic_buffer_destroy(cache->dw_pixel_buf);
    }

    /* Release DirectWrite objects */
    if (cache->dw_render_params) {
        IDWriteRenderingParams_Release((IDWriteRenderingParams *)cache->dw_render_params);
    }
    if (cache->dw_render_target) {
        IDWriteBitmapRenderTarget_Release((IDWriteBitmapRenderTarget *)cache->dw_render_target);
    }
    if (cache->dw_gdi_interop) {
        IDWriteGdiInterop_Release((IDWriteGdiInterop *)cache->dw_gdi_interop);
    }
    if (cache->dw_symbol_face) {
        IDWriteFontFace_Release((IDWriteFontFace *)cache->dw_symbol_face);
    }
    if (cache->dw_emoji_face) {
        IDWriteFontFace_Release((IDWriteFontFace *)cache->dw_emoji_face);
    }
    if (cache->dw_bold_face) {
        IDWriteFontFace_Release((IDWriteFontFace *)cache->dw_bold_face);
    }
    if (cache->dw_font_face) {
        IDWriteFontFace_Release((IDWriteFontFace *)cache->dw_font_face);
    }

    /* Free cache textures */
    if (cache->cache) {
        for (int i = 0; i < cache->cache_size; i++) {
            CacheNode *nodes = (CacheNode *)cache->cache;
            if (nodes[i].texture) {
                SDL_DestroyTexture(nodes[i].texture);
            }
        }
        free(cache->cache);
    }

    /* Free strings */
    if (cache->font_path)
        free(cache->font_path);
    if (cache->font_name)
        free(cache->font_name);

    free(cache);
}

#endif /* _WIN32 */
