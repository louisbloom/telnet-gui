/* Terminal renderer implementation */

#include "renderer.h"
#include <stdlib.h>

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

    /* Get default colors for fallback */
    VTermState *state = vterm_obtain_state(vterm);
    VTermColor default_fg, default_bg;
    /* Initialize default colors to safe values */
    vterm_color_rgb(&default_fg, 255, 255, 255);
    vterm_color_rgb(&default_bg, 0, 0, 0);
    if (state) {
        vterm_state_get_default_colors(state, &default_fg, &default_bg);
    }

    /* Render each cell */
    for (int row = 0; row < rows; row++) {
        for (int col = 0; col < cols; col++) {
            VTermPos pos = {row, col};
            VTermScreenCell cell;
            vterm_screen_get_cell(screen, pos, &cell);

            /* Handle background color */
            VTermColor bg_color_processed = cell.bg;
            if (VTERM_COLOR_IS_DEFAULT_BG(&cell.bg)) {
                bg_color_processed = default_bg;
            }
            if (VTERM_COLOR_IS_INDEXED(&bg_color_processed)) {
                vterm_screen_convert_color_to_rgb(screen, &bg_color_processed);
            }

            /* Draw background if not default or if RGB */
            if (!VTERM_COLOR_IS_DEFAULT_BG(&cell.bg) && VTERM_COLOR_IS_RGB(&bg_color_processed)) {
                SDL_SetRenderDrawColor(r->sdl_renderer, bg_color_processed.rgb.red, bg_color_processed.rgb.green,
                                       bg_color_processed.rgb.blue, 255);
                SDL_Rect bg_rect = {col * r->cell_w, row * r->cell_h + r->titlebar_h, r->cell_w, r->cell_h};
                SDL_RenderFillRect(r->sdl_renderer, &bg_rect);
            }

            /* Draw foreground/characters */
            if (cell.chars[0]) {
                /* Handle foreground color */
                VTermColor fg_color_processed = cell.fg;
                if (VTERM_COLOR_IS_DEFAULT_FG(&cell.fg)) {
                    fg_color_processed = default_fg;
                }
                if (VTERM_COLOR_IS_INDEXED(&fg_color_processed)) {
                    vterm_screen_convert_color_to_rgb(screen, &fg_color_processed);
                }

                SDL_Color fg_color = {255, 255, 255, 255};
                if (VTERM_COLOR_IS_RGB(&fg_color_processed)) {
                    fg_color.r = fg_color_processed.rgb.red;
                    fg_color.g = fg_color_processed.rgb.green;
                    fg_color.b = fg_color_processed.rgb.blue;
                }

                SDL_Color bg_color = {0, 0, 0, 255};
                if (VTERM_COLOR_IS_RGB(&bg_color_processed)) {
                    bg_color.r = bg_color_processed.rgb.red;
                    bg_color.g = bg_color_processed.rgb.green;
                    bg_color.b = bg_color_processed.rgb.blue;
                }

                SDL_Texture *glyph = glyph_cache_get(r->glyph_cache, cell.chars[0], fg_color, bg_color, cell.attrs.bold,
                                                     cell.attrs.italic);
                if (glyph) {
                    /* Get actual texture size */
                    int tex_w, tex_h;
                    SDL_QueryTexture(glyph, NULL, NULL, &tex_w, &tex_h);

                    /* Center glyph horizontally, align to baseline vertically */
                    int dst_x = col * r->cell_w + (r->cell_w - tex_w) / 2;
                    int dst_y = row * r->cell_h + r->titlebar_h;
                    SDL_Rect dst = {dst_x, dst_y, tex_w, tex_h};

                    SDL_RenderCopy(r->sdl_renderer, glyph, NULL, &dst);
                }
            }
        }
    }

    /* Draw cursor */
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
