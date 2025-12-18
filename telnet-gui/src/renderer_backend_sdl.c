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
            SDL_Texture *glyph = glyph_cache_get(state->glyph_cache, cell->chars[0], fg_color, bg_color,
                                                 cell->attrs.bold, cell->attrs.italic);
            if (glyph) {
                /* Get actual texture size */
                int tex_w, tex_h;
                SDL_QueryTexture(glyph, NULL, NULL, &tex_w, &tex_h);

                /* Position glyph vertically centered within line height */
                int dst_x = col * cell_w + PADDING_X;
                int dst_y = row * cell_h + PADDING_Y + vertical_offset;
                SDL_Rect dst = {dst_x, dst_y, tex_w, tex_h};

                SDL_RenderCopy(state->sdl_renderer, glyph, NULL, &dst);
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
            SDL_Texture *glyph = glyph_cache_get(state->glyph_cache, cursor_char, fg_color, bg_color, 0, 0);
            if (glyph) {
                /* Get actual texture size */
                int tex_w, tex_h;
                SDL_QueryTexture(glyph, NULL, NULL, &tex_w, &tex_h);

                /* Position glyph vertically centered within line height */
                int dst_x = col * cell_w + PADDING_X;
                int dst_y = row * cell_h + PADDING_Y + vertical_offset;
                SDL_Rect dst = {dst_x, dst_y, tex_w, tex_h};

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
