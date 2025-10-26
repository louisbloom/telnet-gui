/* Terminal renderer implementation */

#include "renderer.h"
#include <stdlib.h>
#include <string.h>

struct Renderer {
    SDL_Renderer *sdl_renderer;
    GlyphCache *glyph_cache;
    int cell_w, cell_h;
    int titlebar_h;
};

Renderer *renderer_create(SDL_Renderer *sdl_renderer, GlyphCache *glyph_cache, int cell_width, int cell_height,
                          int titlebar_height) {
    Renderer *r = (Renderer *)malloc(sizeof(Renderer));
    if (!r)
        return NULL;

    r->sdl_renderer = sdl_renderer;
    r->glyph_cache = glyph_cache;
    r->cell_w = cell_width;
    r->cell_h = cell_height;
    r->titlebar_h = titlebar_height;

    return r;
}

void renderer_render(Renderer *r, Terminal *term, const char *title) {
    if (!r || !term)
        return;
    (void)title; /* unused for now */

    /* Clear screen */
    SDL_SetRenderDrawColor(r->sdl_renderer, 0, 0, 0, 255);
    SDL_RenderClear(r->sdl_renderer);

    VTerm *vterm = terminal_get_vterm(term);
    VTermScreen *screen = terminal_get_screen(term);
    if (!screen || !vterm)
        return;

    int rows, cols;
    terminal_get_size(term, &rows, &cols);

    /* Render each cell */
    for (int row = 0; row < rows; row++) {
        for (int col = 0; col < cols; col++) {
            VTermPos pos = {row, col};
            VTermScreenCell cell;
            vterm_screen_get_cell(screen, pos, &cell);

            /* Draw background if needed */
            if (cell.bg.type == VTERM_COLOR_RGB) {
                SDL_SetRenderDrawColor(r->sdl_renderer, cell.bg.rgb.red, cell.bg.rgb.green, cell.bg.rgb.blue, 255);
                SDL_Rect bg_rect = {col * r->cell_w, row * r->cell_h + r->titlebar_h, r->cell_w, r->cell_h};
                SDL_RenderFillRect(r->sdl_renderer, &bg_rect);
            }

            /* Draw foreground/characters */
            if (cell.chars[0]) {
                SDL_Color fg_color = {255, 255, 255, 255};
                if (cell.fg.type == VTERM_COLOR_RGB) {
                    fg_color.r = cell.fg.rgb.red;
                    fg_color.g = cell.fg.rgb.green;
                    fg_color.b = cell.fg.rgb.blue;
                }

                SDL_Color bg_color = {0, 0, 0, 255};
                if (cell.bg.type == VTERM_COLOR_RGB) {
                    bg_color.r = cell.bg.rgb.red;
                    bg_color.g = cell.bg.rgb.green;
                    bg_color.b = cell.bg.rgb.blue;
                }

                SDL_Texture *glyph = glyph_cache_get(r->glyph_cache, cell.chars[0], fg_color, bg_color, cell.attrs.bold,
                                                     cell.attrs.italic);
                if (glyph) {
                    SDL_Rect dst = {col * r->cell_w, row * r->cell_h + r->titlebar_h, r->cell_w, r->cell_h};
                    SDL_RenderCopy(r->sdl_renderer, glyph, NULL, &dst);
                }
            }
        }
    }

    /* Draw cursor */
    VTermState *state = vterm_obtain_state(vterm);
    if (state) {
        VTermPos cursor;
        vterm_state_get_cursorpos(state, &cursor);
        SDL_SetRenderDrawColor(r->sdl_renderer, 200, 200, 200, 255);
        SDL_Rect cursor_rect = {cursor.col * r->cell_w, cursor.row * r->cell_h + r->titlebar_h, r->cell_w, r->cell_h};
        SDL_RenderFillRect(r->sdl_renderer, &cursor_rect);
    }
    SDL_RenderPresent(r->sdl_renderer);
}

void renderer_destroy(Renderer *r) {
    if (!r)
        return;
    free(r);
}
