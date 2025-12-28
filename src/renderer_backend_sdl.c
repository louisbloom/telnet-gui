/* SDL renderer backend implementation */

#include "renderer_backend.h"
#include "box_drawing.h"
#include "glyph_cache.h"
#include "lisp.h"
#if HAVE_RLOTTIE
#include "animation.h"
#endif
#include <SDL2/SDL.h>
#include <vterm.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

/* Global animation pointer (registered from main.c) */
#if HAVE_RLOTTIE
static Animation *g_animation = NULL;

void renderer_set_animation(Animation *anim) {
    g_animation = anim;
}

Animation *renderer_get_animation(void) {
    return g_animation;
}
#endif

/* Padding around terminal area (including input area) - must match renderer.c */
#define PADDING_X 8
#define PADDING_Y 8

/* Calculate scaled destination rectangle for glyphs */
/* For regular text: only scales down if significantly oversized (>1.5x) */
/* For emoji (scale_to_fit=1): always scales to fill the cell width */
/* For symbols (center_vertical=1): center vertically without width scaling */
static SDL_Rect calc_scaled_glyph_rect(int dst_x, int dst_y, int tex_w, int tex_h, int cell_w, int cell_h,
                                       int scale_to_fit, int center_vertical) {
    int final_w = tex_w;
    int final_h = tex_h;
    int final_x = dst_x;
    int final_y = dst_y;

    if (scale_to_fit) {
        /* Box scale: fit within cell bounds while maintaining aspect ratio, then center */
        float scale_x = (float)cell_w / tex_w;
        float scale_y = (float)cell_h / tex_h;
        float scale = (scale_x < scale_y) ? scale_x : scale_y;

        final_w = (int)(tex_w * scale);
        final_h = (int)(tex_h * scale);

        /* Center within cell */
        final_x = dst_x + (cell_w - final_w) / 2;
        final_y = dst_y + (cell_h - final_h) / 2;
    } else {
        /* Only scale regular glyphs that are significantly oversized (>1.5x cell size) */
        /* This avoids scaling regular glyphs that are just slightly taller due to font metrics */
        int threshold_w = cell_w + cell_w / 2; /* 1.5x cell width */
        int threshold_h = cell_h + cell_h / 2; /* 1.5x cell height */

        if (tex_w > threshold_w || tex_h > threshold_h) {
            float scale_x = (float)cell_w / tex_w;
            float scale_y = (float)cell_h / tex_h;
            float scale = (scale_x < scale_y) ? scale_x : scale_y;

            final_w = (int)(tex_w * scale);
            final_h = (int)(tex_h * scale);

            /* Center within cell */
            final_x = dst_x + (cell_w - final_w) / 2;
            final_y = dst_y + (cell_h - final_h) / 2;
        }

        /* For symbol glyphs: always center vertically in cell (based on final height after any scaling) */
        if (center_vertical) {
            final_y = dst_y + (cell_h - final_h) / 2;
        }
    }

    return (SDL_Rect){final_x, final_y, final_w, final_h};
}

/* Draw a filled rounded rectangle with given corner radius */
static void sdl_render_rounded_rect(SDL_Renderer *renderer, SDL_Rect *rect, int radius) {
    if (radius <= 0) {
        SDL_RenderFillRect(renderer, rect);
        return;
    }

    int x = rect->x;
    int y = rect->y;
    int w = rect->w;
    int h = rect->h;

    /* Clamp radius to half of smallest dimension */
    if (radius > w / 2)
        radius = w / 2;
    if (radius > h / 2)
        radius = h / 2;

    /* Draw center horizontal rectangle */
    SDL_Rect center_h = {x + radius, y, w - 2 * radius, h};
    SDL_RenderFillRect(renderer, &center_h);

    /* Draw center vertical rectangle */
    SDL_Rect center_v = {x, y + radius, w, h - 2 * radius};
    SDL_RenderFillRect(renderer, &center_v);

    /* Draw filled circles at corners using midpoint circle algorithm */
    int cx, cy;
    int px, py, d;

    /* Top-left corner */
    cx = x + radius;
    cy = y + radius;
    px = 0;
    py = radius;
    d = 1 - radius;
    while (px <= py) {
        SDL_RenderDrawLine(renderer, cx - py, cy - px, cx, cy - px);
        SDL_RenderDrawLine(renderer, cx - px, cy - py, cx, cy - py);
        px++;
        if (d < 0) {
            d += 2 * px + 1;
        } else {
            py--;
            d += 2 * (px - py) + 1;
        }
    }

    /* Top-right corner */
    cx = x + w - radius - 1;
    cy = y + radius;
    px = 0;
    py = radius;
    d = 1 - radius;
    while (px <= py) {
        SDL_RenderDrawLine(renderer, cx, cy - px, cx + py, cy - px);
        SDL_RenderDrawLine(renderer, cx, cy - py, cx + px, cy - py);
        px++;
        if (d < 0) {
            d += 2 * px + 1;
        } else {
            py--;
            d += 2 * (px - py) + 1;
        }
    }

    /* Bottom-left corner */
    cx = x + radius;
    cy = y + h - radius - 1;
    px = 0;
    py = radius;
    d = 1 - radius;
    while (px <= py) {
        SDL_RenderDrawLine(renderer, cx - py, cy + px, cx, cy + px);
        SDL_RenderDrawLine(renderer, cx - px, cy + py, cx, cy + py);
        px++;
        if (d < 0) {
            d += 2 * px + 1;
        } else {
            py--;
            d += 2 * (px - py) + 1;
        }
    }

    /* Bottom-right corner */
    cx = x + w - radius - 1;
    cy = y + h - radius - 1;
    px = 0;
    py = radius;
    d = 1 - radius;
    while (px <= py) {
        SDL_RenderDrawLine(renderer, cx, cy + px, cx + py, cy + px);
        SDL_RenderDrawLine(renderer, cx, cy + py, cx + px, cy + py);
        px++;
        if (d < 0) {
            d += 2 * px + 1;
        } else {
            py--;
            d += 2 * (px - py) + 1;
        }
    }
}

/* SDL backend state - all SDL-specific data */
typedef struct {
    SDL_Renderer *sdl_renderer;
    GlyphCache *glyph_cache;
    int actual_cell_h;           /* Actual font cell height (before line_height multiplier) */
    int cached_effective_cell_h; /* Cached effective cell height for current frame */
    int cached_vertical_offset;  /* Cached vertical offset for current frame */
} SDLBackendState;

/* Backend interface implementation */

static void *sdl_create(void *platform_context, int cell_w, int cell_h) {
    (void)cell_w; /* Cell width managed by renderer wrapper */

    SDLBackendState *state = (SDLBackendState *)malloc(sizeof(SDLBackendState));
    if (!state)
        return NULL;

    /* Platform context contains SDL_Renderer* and GlyphCache* as array of pointers */
    void **ctx = (void **)platform_context;

    state->sdl_renderer = (SDL_Renderer *)ctx[0];
    state->glyph_cache = (GlyphCache *)ctx[1];
    state->actual_cell_h = cell_h;       /* Store actual font cell height for vertical centering */
    state->cached_effective_cell_h = -1; /* Initialize to invalid value to force first calculation */
    state->cached_vertical_offset = 0;

    return state;
}

static void sdl_destroy(void *vstate) {
    SDLBackendState *state = (SDLBackendState *)vstate;
    if (!state)
        return;
    free(state);
}

static void sdl_begin_frame(void *vstate, int window_width, int window_height) {
    SDLBackendState *state = (SDLBackendState *)vstate;
    if (!state)
        return;

    (void)window_width; /* Unused - get real size from SDL */
    (void)window_height;

    /* Get actual window size from SDL renderer */
    int actual_width, actual_height;
    SDL_GetRendererOutputSize(state->sdl_renderer, &actual_width, &actual_height);

    /* Clear screen to terminal background color */
    int bg_r, bg_g, bg_b;
    lisp_x_get_terminal_bg_color(&bg_r, &bg_g, &bg_b);
    SDL_SetRenderDrawColor(state->sdl_renderer, bg_r, bg_g, bg_b, 255);
    SDL_Rect terminal_area = {0, 0, actual_width, actual_height};
    SDL_RenderFillRect(state->sdl_renderer, &terminal_area);

#if HAVE_RLOTTIE
    /* Render animation behind terminal text (if loaded) */
    if (g_animation && animation_is_loaded(g_animation)) {
        /* Render animation to fill the window */
        animation_render(g_animation, 0, 0, actual_width, actual_height);

        /* Apply dim overlay if in DIM mode */
        if (animation_get_visibility_mode(g_animation) == ANIMATION_VISIBILITY_DIM) {
            animation_render_dim_overlay(g_animation, 0, 0, actual_width, actual_height);
        }
    }
#endif

    /* Reset cached values at start of frame - will be recalculated on first cell render */
    state->cached_effective_cell_h = -1;
}

static void sdl_end_frame(void *vstate) {
    SDLBackendState *state = (SDLBackendState *)vstate;
    if (!state)
        return;

    /* Present frame - handled by main.c, not here */
    /* SDL_RenderPresent() called externally */
}

static void sdl_render_cell(void *vstate, Terminal *term, int row, int col, const TermCell *cell, int in_selection,
                            int cell_w, int cell_h) {
    SDLBackendState *state = (SDLBackendState *)vstate;

    if (!state || !cell)
        return;

    /* Skip continuation cells - these are the "second half" of wide characters */
    /* The wide character in the previous cell will render across both cells */
    /* VTerm marks continuation cells with chars[0] = 0xFFFFFFFF (or width=0 on some platforms) */
    /* 0x10FFFF is the maximum valid Unicode codepoint */
    /* Note: Don't draw selection background here - it would obscure the emoji's right half */
    /* and falsely suggest 2 cells are selected when only 1 character will be copied */
    if (cell->width == 0 || cell->chars[0] > 0x10FFFF) {
        return;
    }

    /* Skip cells that follow emoji/emoji-style characters we render as 2-wide */
    /* Without this check, this cell's background would overwrite the emoji's right half */
    int skip_background = 0;
    if (col > 0) {
        TermCell prev_cell;
        if (terminal_get_cell_at(term, row, col - 1, &prev_cell)) {
            if (prev_cell.width == 1) {
                uint32_t pcp = prev_cell.chars[0];
                /* Skip for both true emoji and emoji-style (both render at 2-cell width) */
                int prev_is_2wide = utf8_is_emoji_2wide(pcp, prev_cell.chars[1]);
                if (prev_is_2wide) {
                    /* Only skip if this cell is empty (the overflow area) */
                    /* If this cell has its own character, render it (consecutive emoji) */
                    if (cell->chars[0] == 0 || cell->chars[0] == ' ') {
                        return;
                    }
                    /* If current cell is also 2-wide emoji, skip background to avoid erasing prev emoji's overflow */
                    if (utf8_is_emoji_2wide(cell->chars[0], cell->chars[1])) {
                        skip_background = 1;
                    }
                }
            }
        }
    }

    /* Get vterm screen for color conversion */
    VTermScreen *screen = terminal_get_screen(term);
    if (!screen)
        return;

    /* Calculate vertical offset once per frame (cell_h is constant for all cells in a frame) */
    /* cell_h parameter is effective_cell_h (line_height-adjusted), state->actual_cell_h is the actual font height */
    if (state->cached_effective_cell_h != cell_h) {
        state->cached_effective_cell_h = cell_h;
        state->cached_vertical_offset = (cell_h - state->actual_cell_h) / 2;
    }
    int vertical_offset = state->cached_vertical_offset;

    /* Check if we need transparent backgrounds for animation */
#if HAVE_RLOTTIE
    int use_transparent_bg = 0;
    Uint8 bg_alpha = 255;
    if (g_animation && animation_is_loaded(g_animation)) {
        if (animation_get_visibility_mode(g_animation) == ANIMATION_VISIBILITY_TRANSPARENT) {
            /* Only make DEFAULT background transparent (cells with explicit bg stay opaque) */
            if (cell->bg.type == TERM_COLOR_DEFAULT && !in_selection) {
                use_transparent_bg = 1;
                bg_alpha = (Uint8)(animation_get_terminal_bg_alpha(g_animation) * 255);
            }
        }
    }
#endif

    /* Draw background - use selection color if in selection, otherwise cell bg */
    /* Skip background when consecutive emoji-style chars to avoid erasing previous emoji's overflow */
    if (!skip_background) {
        if (in_selection) {
            /* Get selection background color from Lisp config */
            int sel_bg_r, sel_bg_g, sel_bg_b;
            lisp_x_get_selection_bg_color(&sel_bg_r, &sel_bg_g, &sel_bg_b);
            SDL_SetRenderDrawColor(state->sdl_renderer, sel_bg_r, sel_bg_g, sel_bg_b, 255);
            SDL_Rect bg_rect = {col * cell_w + PADDING_X, row * cell_h + PADDING_Y, cell_w, cell_h};
            SDL_RenderFillRect(state->sdl_renderer, &bg_rect);
        }
#if HAVE_RLOTTIE
        else if (use_transparent_bg) {
            /* TRANSPARENT mode: draw semi-transparent background over animation */
            int term_bg_r, term_bg_g, term_bg_b;
            lisp_x_get_terminal_bg_color(&term_bg_r, &term_bg_g, &term_bg_b);
            SDL_SetRenderDrawColor(state->sdl_renderer, term_bg_r, term_bg_g, term_bg_b, bg_alpha);
            SDL_Rect bg_rect = {col * cell_w + PADDING_X, row * cell_h + PADDING_Y, cell_w, cell_h};
            SDL_RenderFillRect(state->sdl_renderer, &bg_rect);
        }
#endif
        else if (cell->bg.type != TERM_COLOR_DEFAULT) {
            /* Draw non-default background color */
            uint8_t bg_r, bg_g, bg_b;

            if (cell->bg.type == TERM_COLOR_RGB) {
                /* Use RGB color directly */
                bg_r = cell->bg.color.rgb.r;
                bg_g = cell->bg.color.rgb.g;
                bg_b = cell->bg.color.rgb.b;
            } else if (cell->bg.type == TERM_COLOR_INDEXED) {
                /* Convert indexed color to RGB using vterm */
                VTermColor vc;
                vc.type = VTERM_COLOR_INDEXED;
                vc.indexed.idx = cell->bg.color.idx;
                vterm_screen_convert_color_to_rgb(screen, &vc);
                bg_r = vc.rgb.red;
                bg_g = vc.rgb.green;
                bg_b = vc.rgb.blue;
            } else {
                /* Shouldn't reach here, but use default bg from Lisp config */
                int term_bg_r, term_bg_g, term_bg_b;
                lisp_x_get_terminal_bg_color(&term_bg_r, &term_bg_g, &term_bg_b);
                bg_r = term_bg_r;
                bg_g = term_bg_g;
                bg_b = term_bg_b;
            }

#if HAVE_RLOTTIE
            /* In DIM mode, skip drawing backgrounds that match terminal default */
            if (g_animation && animation_is_loaded(g_animation) &&
                animation_get_visibility_mode(g_animation) == ANIMATION_VISIBILITY_DIM) {
                int term_bg_r, term_bg_g, term_bg_b;
                lisp_x_get_terminal_bg_color(&term_bg_r, &term_bg_g, &term_bg_b);
                /* Only draw if background differs from terminal default */
                if (bg_r != (uint8_t)term_bg_r || bg_g != (uint8_t)term_bg_g || bg_b != (uint8_t)term_bg_b) {
                    SDL_SetRenderDrawColor(state->sdl_renderer, bg_r, bg_g, bg_b, 255);
                    SDL_Rect bg_rect = {col * cell_w + PADDING_X, row * cell_h + PADDING_Y, cell_w, cell_h};
                    SDL_RenderFillRect(state->sdl_renderer, &bg_rect);
                }
            } else
#endif
            {
                SDL_SetRenderDrawColor(state->sdl_renderer, bg_r, bg_g, bg_b, 255);
                SDL_Rect bg_rect = {col * cell_w + PADDING_X, row * cell_h + PADDING_Y, cell_w, cell_h};
                SDL_RenderFillRect(state->sdl_renderer, &bg_rect);
            }
        }
    }

    /* Draw foreground/characters */
    if (cell->chars[0]) {
        SDL_Color fg_color, bg_color;

        /* Use selection colors if in selection */
        if (in_selection) {
            int sel_fg_r, sel_fg_g, sel_fg_b, sel_bg_r, sel_bg_g, sel_bg_b;
            lisp_x_get_selection_fg_color(&sel_fg_r, &sel_fg_g, &sel_fg_b);
            lisp_x_get_selection_bg_color(&sel_bg_r, &sel_bg_g, &sel_bg_b);
            fg_color.r = sel_fg_r;
            fg_color.g = sel_fg_g;
            fg_color.b = sel_fg_b;
            fg_color.a = 255;
            bg_color.r = sel_bg_r;
            bg_color.g = sel_bg_g;
            bg_color.b = sel_bg_b;
            bg_color.a = 255;
        } else {
            /* Get default terminal colors from Lisp config */
            int term_fg_r, term_fg_g, term_fg_b, term_bg_r, term_bg_g, term_bg_b;
            lisp_x_get_terminal_fg_color(&term_fg_r, &term_fg_g, &term_fg_b);
            lisp_x_get_terminal_bg_color(&term_bg_r, &term_bg_g, &term_bg_b);

            /* Process foreground color */
            if (cell->fg.type == TERM_COLOR_DEFAULT) {
                /* Use default foreground color from Lisp config */
                fg_color.r = term_fg_r;
                fg_color.g = term_fg_g;
                fg_color.b = term_fg_b;
                fg_color.a = 255;
            } else if (cell->fg.type == TERM_COLOR_RGB) {
                /* Use RGB color directly */
                fg_color.r = cell->fg.color.rgb.r;
                fg_color.g = cell->fg.color.rgb.g;
                fg_color.b = cell->fg.color.rgb.b;
                fg_color.a = 255;
            } else if (cell->fg.type == TERM_COLOR_INDEXED) {
                /* Convert indexed color to RGB using vterm */
                VTermColor vc;
                vc.type = VTERM_COLOR_INDEXED;
                vc.indexed.idx = cell->fg.color.idx;
                vterm_screen_convert_color_to_rgb(screen, &vc);
                fg_color.r = vc.rgb.red;
                fg_color.g = vc.rgb.green;
                fg_color.b = vc.rgb.blue;
                fg_color.a = 255;
            } else {
                /* Fallback to default */
                fg_color.r = term_fg_r;
                fg_color.g = term_fg_g;
                fg_color.b = term_fg_b;
                fg_color.a = 255;
            }

            /* Process background color */
            if (cell->bg.type == TERM_COLOR_DEFAULT) {
                /* Use default background color from Lisp config */
                bg_color.r = term_bg_r;
                bg_color.g = term_bg_g;
                bg_color.b = term_bg_b;
                bg_color.a = 255;
            } else if (cell->bg.type == TERM_COLOR_RGB) {
                /* Use RGB color directly */
                bg_color.r = cell->bg.color.rgb.r;
                bg_color.g = cell->bg.color.rgb.g;
                bg_color.b = cell->bg.color.rgb.b;
                bg_color.a = 255;
            } else if (cell->bg.type == TERM_COLOR_INDEXED) {
                /* Convert indexed color to RGB using vterm */
                VTermColor vc;
                vc.type = VTERM_COLOR_INDEXED;
                vc.indexed.idx = cell->bg.color.idx;
                vterm_screen_convert_color_to_rgb(screen, &vc);
                bg_color.r = vc.rgb.red;
                bg_color.g = vc.rgb.green;
                bg_color.b = vc.rgb.blue;
                bg_color.a = 255;
            } else {
                /* Fallback to default */
                bg_color.r = term_bg_r;
                bg_color.g = term_bg_g;
                bg_color.b = term_bg_b;
                bg_color.a = 255;
            }
        }

        /* Handle box drawing characters manually for pixel-perfect alignment */
        if (is_box_drawing_char(cell->chars[0])) {
            int dst_x = col * cell_w + PADDING_X;
            int dst_y = row * cell_h + PADDING_Y + vertical_offset;
            render_box_drawing_char(state->sdl_renderer, cell->chars[0], dst_x, dst_y, cell_w, state->actual_cell_h,
                                    fg_color);
        } else {
            /* Detect character types:
             * - Symbol: Dingbats/misc symbols (U+2600-U+27BF) - needs vertical centering, no width scaling
             * - Emoji: Variation selector U+FE0F or high Unicode emoji ranges - needs full scaling
             */
            uint32_t cp = cell->chars[0];
            /* True emoji: Emoji_Presentation=Yes, variation selector, or emoji ranges - occupy 2 cells */
            int has_variation_selector = (cell->chars[1] == 0xFE0F);
            int is_true_emoji = has_variation_selector || utf8_is_emoji_presentation(cp) || /* Emoji_Presentation=Yes */
                                (cp >= 0x1F300 && cp <= 0x1F5FF) || /* Misc Symbols and Pictographs */
                                (cp >= 0x1F600 && cp <= 0x1F64F) || /* Emoticons */
                                (cp >= 0x1F680 && cp <= 0x1F6FF) || /* Transport/Map */
                                (cp >= 0x1F900 && cp <= 0x1F9FF) || /* Supplemental Symbols */
                                (cp >= 0x1FA00 && cp <= 0x1FAFF);   /* Chess, symbols */
            /* Emoji property chars: render at 2-cell size but occupy 1 cell (overflow into next) */
            int is_emoji_style = !is_true_emoji && utf8_is_emoji_property(cp);
            /* Symbols need emoji font but are 1-cell wide and 1-cell render */
            int is_symbol = utf8_is_symbol_range(cp) && !is_true_emoji && !is_emoji_style;

            SDL_Texture *glyph =
                glyph_cache_get(state->glyph_cache, cell->chars[0], fg_color, bg_color, cell->attrs.bold,
                                cell->attrs.italic, is_true_emoji || is_emoji_style || is_symbol);
            if (glyph) {
                /* Get actual texture size */
                int tex_w, tex_h;
                SDL_QueryTexture(glyph, NULL, NULL, &tex_w, &tex_h);

                int dst_x = col * cell_w + PADDING_X;
                int dst_y;
                int char_width = (cell->width > 0) ? cell->width : 1;
                int scale_height;
                int scale_to_fit;
                int center_vertical;

                if (is_true_emoji) {
                    /* True emoji (Emoji_Presentation=Yes): box scale to fit 2 cells, occupy 2 cells */
                    dst_y = row * cell_h + PADDING_Y;
                    scale_height = cell_h;
                    scale_to_fit = 1;
                    center_vertical = 0; /* scale_to_fit already centers */
                    if (char_width < 2) {
                        char_width = 2;
                    }
                    SDL_Rect dst = calc_scaled_glyph_rect(dst_x, dst_y, tex_w, tex_h, cell_w * char_width, scale_height,
                                                          scale_to_fit, center_vertical);
                    SDL_RenderCopy(state->sdl_renderer, glyph, NULL, &dst);
                } else if (is_emoji_style) {
                    /* Emoji style (Emoji=Yes, Emoji_Presentation=No): render at 2-cell size, occupy 1 cell */
                    /* These overflow into the next cell but don't claim it */
                    dst_y = row * cell_h + PADDING_Y;
                    scale_height = cell_h;
                    scale_to_fit = 1;
                    center_vertical = 0;
                    /* Render at 2-cell width but don't modify char_width (stays 1) */
                    SDL_Rect dst = calc_scaled_glyph_rect(dst_x, dst_y, tex_w, tex_h, cell_w * 2, scale_height,
                                                          scale_to_fit, center_vertical);
                    SDL_RenderCopy(state->sdl_renderer, glyph, NULL, &dst);
                } else if (is_symbol) {
                    /* Symbols: box scale to fit 1 cell, center both ways */
                    dst_y = row * cell_h + PADDING_Y;
                    scale_height = cell_h;
                    scale_to_fit = 1;
                    center_vertical = 0;
                    SDL_Rect dst = calc_scaled_glyph_rect(dst_x, dst_y, tex_w, tex_h, cell_w * char_width, scale_height,
                                                          scale_to_fit, center_vertical);
                    SDL_RenderCopy(state->sdl_renderer, glyph, NULL, &dst);
                } else if (utf8_is_block_element(cp)) {
                    /* Block elements: draw procedurally for pixel-perfect cell filling */
                    int cx = dst_x;
                    int cy = row * cell_h + PADDING_Y;
                    int cw = cell_w;
                    int ch = cell_h;
                    int hw = cw / 2, hh = ch / 2; /* half width/height */
#if HAVE_RLOTTIE
                    /* Set clip rect to prevent block elements from drawing outside cell bounds */
                    if (g_animation && animation_is_loaded(g_animation)) {
                        SDL_Rect block_clip = {cx, cy, cw, ch};
                        SDL_RenderSetClipRect(state->sdl_renderer, &block_clip);
                    }
#endif
                    SDL_SetRenderDrawColor(state->sdl_renderer, fg_color.r, fg_color.g, fg_color.b, 255);
                    SDL_Rect r;
                    switch (cp) {
                    /* Upper/lower fractional blocks */
                    case 0x2580:
                        r = (SDL_Rect){cx, cy, cw, hh};
                        break; /* ▀ Upper 1/2 */
                    case 0x2581:
                        r = (SDL_Rect){cx, cy + ch * 7 / 8, cw, ch - ch * 7 / 8};
                        break; /* ▁ Lower 1/8 */
                    case 0x2582:
                        r = (SDL_Rect){cx, cy + ch * 3 / 4, cw, ch - ch * 3 / 4};
                        break; /* ▂ Lower 1/4 */
                    case 0x2583:
                        r = (SDL_Rect){cx, cy + ch * 5 / 8, cw, ch - ch * 5 / 8};
                        break; /* ▃ Lower 3/8 */
                    case 0x2584:
                        r = (SDL_Rect){cx, cy + hh, cw, ch - hh};
                        break; /* ▄ Lower 1/2 */
                    case 0x2585:
                        r = (SDL_Rect){cx, cy + ch * 3 / 8, cw, ch - ch * 3 / 8};
                        break; /* ▅ Lower 5/8 */
                    case 0x2586:
                        r = (SDL_Rect){cx, cy + ch / 4, cw, ch - ch / 4};
                        break; /* ▆ Lower 3/4 */
                    case 0x2587:
                        r = (SDL_Rect){cx, cy + ch / 8, cw, ch - ch / 8};
                        break; /* ▇ Lower 7/8 */
                    case 0x2588:
                        r = (SDL_Rect){cx, cy, cw, ch};
                        break; /* █ Full */
                    /* Left fractional blocks */
                    case 0x2589:
                        r = (SDL_Rect){cx, cy, cw * 7 / 8, ch};
                        break; /* ▉ Left 7/8 */
                    case 0x258A:
                        r = (SDL_Rect){cx, cy, cw * 3 / 4, ch};
                        break; /* ▊ Left 3/4 */
                    case 0x258B:
                        r = (SDL_Rect){cx, cy, cw * 5 / 8, ch};
                        break; /* ▋ Left 5/8 */
                    case 0x258C:
                        r = (SDL_Rect){cx, cy, hw, ch};
                        break; /* ▌ Left 1/2 */
                    case 0x258D:
                        r = (SDL_Rect){cx, cy, cw * 3 / 8, ch};
                        break; /* ▍ Left 3/8 */
                    case 0x258E:
                        r = (SDL_Rect){cx, cy, cw / 4, ch};
                        break; /* ▎ Left 1/4 */
                    case 0x258F:
                        r = (SDL_Rect){cx, cy, cw / 8, ch};
                        break; /* ▏ Left 1/8 */
                    /* Right half and edges */
                    case 0x2590:
                        r = (SDL_Rect){cx + hw, cy, cw - hw, ch};
                        break; /* ▐ Right 1/2 */
                    case 0x2594:
                        r = (SDL_Rect){cx, cy, cw, ch / 8};
                        break; /* ▔ Upper 1/8 */
                    case 0x2595:
                        r = (SDL_Rect){cx + cw - cw / 8, cy, cw / 8, ch};
                        break; /* ▕ Right 1/8 */
                    /* Shade blocks - use alpha blending */
                    case 0x2591: /* ░ Light shade 25% */
                        SDL_SetRenderDrawColor(state->sdl_renderer, fg_color.r, fg_color.g, fg_color.b, 64);
                        r = (SDL_Rect){cx, cy, cw, ch};
                        break;
                    case 0x2592: /* ▒ Medium shade 50% */
                        SDL_SetRenderDrawColor(state->sdl_renderer, fg_color.r, fg_color.g, fg_color.b, 128);
                        r = (SDL_Rect){cx, cy, cw, ch};
                        break;
                    case 0x2593: /* ▓ Dark shade 75% */
                        SDL_SetRenderDrawColor(state->sdl_renderer, fg_color.r, fg_color.g, fg_color.b, 192);
                        r = (SDL_Rect){cx, cy, cw, ch};
                        break;
                    /* Quadrants - draw multiple rectangles */
                    case 0x2596: /* ▖ Lower left */
                        r = (SDL_Rect){cx, cy + hh, hw, ch - hh};
                        break;
                    case 0x2597: /* ▗ Lower right */
                        r = (SDL_Rect){cx + hw, cy + hh, cw - hw, ch - hh};
                        break;
                    case 0x2598: /* ▘ Upper left */
                        r = (SDL_Rect){cx, cy, hw, hh};
                        break;
                    case 0x259D: /* ▝ Upper right */
                        r = (SDL_Rect){cx + hw, cy, cw - hw, hh};
                        break;
                    case 0x2599: /* ▙ Upper left + lower left + lower right */
                        SDL_RenderFillRect(state->sdl_renderer, &(SDL_Rect){cx, cy, hw, hh});
                        r = (SDL_Rect){cx, cy + hh, cw, ch - hh};
                        break;
                    case 0x259A: /* ▚ Upper left + lower right (diagonal) */
                        SDL_RenderFillRect(state->sdl_renderer, &(SDL_Rect){cx, cy, hw, hh});
                        r = (SDL_Rect){cx + hw, cy + hh, cw - hw, ch - hh};
                        break;
                    case 0x259B: /* ▛ Upper left + upper right + lower left */
                        SDL_RenderFillRect(state->sdl_renderer, &(SDL_Rect){cx, cy, cw, hh});
                        r = (SDL_Rect){cx, cy + hh, hw, ch - hh};
                        break;
                    case 0x259C: /* ▜ Upper left + upper right + lower right */
                        SDL_RenderFillRect(state->sdl_renderer, &(SDL_Rect){cx, cy, cw, hh});
                        r = (SDL_Rect){cx + hw, cy + hh, cw - hw, ch - hh};
                        break;
                    case 0x259E: /* ▞ Upper right + lower left (diagonal) */
                        SDL_RenderFillRect(state->sdl_renderer, &(SDL_Rect){cx + hw, cy, cw - hw, hh});
                        r = (SDL_Rect){cx, cy + hh, hw, ch - hh};
                        break;
                    case 0x259F: /* ▟ Upper right + lower left + lower right */
                        SDL_RenderFillRect(state->sdl_renderer, &(SDL_Rect){cx + hw, cy, cw - hw, hh});
                        r = (SDL_Rect){cx, cy + hh, cw, ch - hh};
                        break;
                    default:
                        r = (SDL_Rect){cx, cy, cw, ch};
                        break;
                    }
                    SDL_RenderFillRect(state->sdl_renderer, &r);
#if HAVE_RLOTTIE
                    /* Reset clip rect after block element rendering */
                    if (g_animation && animation_is_loaded(g_animation)) {
                        SDL_RenderSetClipRect(state->sdl_renderer, NULL);
                    }
#endif
                } else {
                    /* Regular text: use vertical offset for line height centering */
                    dst_y = row * cell_h + PADDING_Y + vertical_offset;
                    scale_height = state->actual_cell_h;
                    scale_to_fit = 0;
                    center_vertical = 0;

                    SDL_Rect dst = calc_scaled_glyph_rect(dst_x, dst_y, tex_w, tex_h, cell_w * char_width, scale_height,
                                                          scale_to_fit, center_vertical);
                    SDL_RenderCopy(state->sdl_renderer, glyph, NULL, &dst);
                }
            }
        }
    }
}

static void sdl_render_cursor(void *vstate, int row, int col, uint32_t cursor_char, int cell_w, int cell_h) {
    SDLBackendState *state = (SDLBackendState *)vstate;
    if (!state)
        return;

    /* Use cached vertical offset (calculated once per frame in render_cell) */
    /* If cache is invalid, calculate it now (shouldn't happen, but be safe) */
    int vertical_offset;
    if (state->cached_effective_cell_h != cell_h) {
        state->cached_effective_cell_h = cell_h;
        state->cached_vertical_offset = (cell_h - state->actual_cell_h) / 2;
    }
    vertical_offset = state->cached_vertical_offset;

    /* Get cursor background color from Lisp config */
    int cursor_r, cursor_g, cursor_b;
    lisp_x_get_cursor_color(&cursor_r, &cursor_g, &cursor_b);

    /* Draw cursor background with slightly rounded corners, vertically centered within line height */
    SDL_SetRenderDrawColor(state->sdl_renderer, cursor_r, cursor_g, cursor_b, 255);
    SDL_Rect cursor_rect = {col * cell_w + PADDING_X, row * cell_h + PADDING_Y + vertical_offset, cell_w,
                            state->actual_cell_h};
    sdl_render_rounded_rect(state->sdl_renderer, &cursor_rect, 2);

    /* If there's a character at the cursor position, render it on top with normal foreground color */
    /* Skip newline characters (they don't render visually) */
    if (cursor_char != 0 && cursor_char != '\n') {
        /* Get terminal foreground color for the character */
        int fg_r, fg_g, fg_b;
        lisp_x_get_terminal_fg_color(&fg_r, &fg_g, &fg_b);
        SDL_Color fg_color = {fg_r, fg_g, fg_b, 255};
        SDL_Color bg_color = {cursor_r, cursor_g, cursor_b, 255}; /* Use cursor color as background */

        /* Handle box drawing characters manually for pixel-perfect alignment */
        if (is_box_drawing_char(cursor_char)) {
            int dst_x = col * cell_w + PADDING_X;
            int dst_y = row * cell_h + PADDING_Y + vertical_offset;
            render_box_drawing_char(state->sdl_renderer, cursor_char, dst_x, dst_y, cell_w, state->actual_cell_h,
                                    fg_color);
        } else {
            /* Use glyph cache for rendering */
            SDL_Texture *glyph = glyph_cache_get(state->glyph_cache, cursor_char, fg_color, bg_color, 0, 0, 0);
            if (glyph) {
                /* Get actual texture size */
                int tex_w, tex_h;
                SDL_QueryTexture(glyph, NULL, NULL, &tex_w, &tex_h);

                /* Position glyph vertically centered within line height */
                int dst_x = col * cell_w + PADDING_X;
                int dst_y = row * cell_h + PADDING_Y + vertical_offset;

                SDL_Rect dst = calc_scaled_glyph_rect(dst_x, dst_y, tex_w, tex_h, cell_w, state->actual_cell_h, 0, 0);
                SDL_RenderCopy(state->sdl_renderer, glyph, NULL, &dst);
            }
        }
    }
}

/* Backend registration */
const RendererBackend renderer_backend_sdl = {
    .create = sdl_create,
    .destroy = sdl_destroy,
    .begin_frame = sdl_begin_frame,
    .end_frame = sdl_end_frame,
    .render_cell = sdl_render_cell,
    .render_cursor = sdl_render_cursor,
};
