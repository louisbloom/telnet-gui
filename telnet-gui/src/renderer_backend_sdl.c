/* SDL renderer backend implementation */

#include "renderer_backend.h"
#include "box_drawing.h"
#include "glyph_cache.h"
#include "lisp.h"
#include <SDL2/SDL.h>
#include <vterm.h>
#include <stdlib.h>
#include <stdio.h>

/* Padding around terminal area (including input area) - must match renderer.c */
#define PADDING_X 8
#define PADDING_Y 8

/* SDL backend state - all SDL-specific data */
typedef struct {
    SDL_Renderer *sdl_renderer;
    GlyphCache *glyph_cache;
} SDLBackendState;

/* Backend interface implementation */

static void *sdl_create(void *platform_context, int cell_w, int cell_h) {
    (void)cell_w; /* Cell dimensions managed by renderer wrapper */
    (void)cell_h;

    SDLBackendState *state = (SDLBackendState *)malloc(sizeof(SDLBackendState));
    if (!state)
        return NULL;

    /* Platform context contains SDL_Renderer* and GlyphCache* as array of pointers */
    void **ctx = (void **)platform_context;

    state->sdl_renderer = (SDL_Renderer *)ctx[0];
    state->glyph_cache = (GlyphCache *)ctx[1];

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

    /* Clear screen to black - matches original renderer.c behavior */
    /* This happens AFTER main.c clears with terminal bg color, but that's how the original worked */
    SDL_SetRenderDrawColor(state->sdl_renderer, 0, 0, 0, 255);
    SDL_Rect terminal_area = {0, 0, actual_width, actual_height};
    SDL_RenderFillRect(state->sdl_renderer, &terminal_area);
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

    /* Draw background - use selection color if in selection, otherwise cell bg */
    if (in_selection) {
        /* Get selection background color from Lisp config */
        int sel_bg_r, sel_bg_g, sel_bg_b;
        lisp_x_get_selection_bg_color(&sel_bg_r, &sel_bg_g, &sel_bg_b);
        SDL_SetRenderDrawColor(state->sdl_renderer, sel_bg_r, sel_bg_g, sel_bg_b, 255);
        SDL_Rect bg_rect = {col * cell_w + PADDING_X, row * cell_h + PADDING_Y, cell_w, cell_h};
        SDL_RenderFillRect(state->sdl_renderer, &bg_rect);
    } else if (cell->bg.type != TERM_COLOR_DEFAULT) {
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

        SDL_SetRenderDrawColor(state->sdl_renderer, bg_r, bg_g, bg_b, 255);
        SDL_Rect bg_rect = {col * cell_w + PADDING_X, row * cell_h + PADDING_Y, cell_w, cell_h};
        SDL_RenderFillRect(state->sdl_renderer, &bg_rect);
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
            int dst_y = row * cell_h + PADDING_Y;
            render_box_drawing_char(state->sdl_renderer, cell->chars[0], dst_x, dst_y, cell_w, cell_h, fg_color);
        } else {
            SDL_Texture *glyph = glyph_cache_get(state->glyph_cache, cell->chars[0], fg_color, bg_color,
                                                 cell->attrs.bold, cell->attrs.italic);
            if (glyph) {
                /* Get actual texture size */
                int tex_w, tex_h;
                SDL_QueryTexture(glyph, NULL, NULL, &tex_w, &tex_h);

                /* Position glyph at cell boundary - no minx adjustment for monospace alignment */
                int dst_x = col * cell_w + PADDING_X;
                int dst_y = row * cell_h + PADDING_Y;
                SDL_Rect dst = {dst_x, dst_y, tex_w, tex_h};

                SDL_RenderCopy(state->sdl_renderer, glyph, NULL, &dst);
            }
        }
    }
}

static void sdl_render_cursor(void *vstate, int row, int col, int cell_w, int cell_h) {
    SDLBackendState *state = (SDLBackendState *)vstate;
    if (!state)
        return;

    /* Get cursor color from Lisp config */
    int cursor_r, cursor_g, cursor_b;
    lisp_x_get_cursor_color(&cursor_r, &cursor_g, &cursor_b);

    /* Render cursor as filled block */
    SDL_SetRenderDrawColor(state->sdl_renderer, cursor_r, cursor_g, cursor_b, 255);
    SDL_Rect cursor_rect = {col * cell_w + PADDING_X, row * cell_h + PADDING_Y, cell_w, cell_h};
    SDL_RenderFillRect(state->sdl_renderer, &cursor_rect);
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
