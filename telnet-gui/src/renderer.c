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

    /* Clear screen (but not titlebar area - it will be rendered separately) */
    SDL_SetRenderDrawColor(r->sdl_renderer, 0, 0, 0, 255);
    /* Clear only the terminal area, not the titlebar */
    int window_width, window_height;
    SDL_GetRendererOutputSize(r->sdl_renderer, &window_width, &window_height);
    SDL_Rect terminal_area = {0, r->titlebar_h, window_width, window_height - r->titlebar_h};
    SDL_RenderFillRect(r->sdl_renderer, &terminal_area);

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
            VTermScreenCell cell;
            /* Get cell considering viewport offset */
            if (!terminal_get_cell_at(term, row, col, &cell))
                continue;

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

    /* Don't draw terminal cursor - user input is handled by input area at bottom */
}

/* Render input area at bottom of screen */
void renderer_render_input_area(Renderer *r, const char *text, int text_len, int cursor_pos, int window_width,
                                int window_height, int input_area_height, const char *visual_text, int visual_length) {
    if (!r)
        return;

    int input_area_y = window_height - input_area_height;

    /* Draw separator line between terminal and input area */
    SDL_SetRenderDrawColor(r->sdl_renderer, 100, 100, 100, 255);
    SDL_RenderDrawLine(r->sdl_renderer, 0, input_area_y, window_width, input_area_y);

    /* Draw input area background - dark blueish (same as titlebar) */
    SDL_SetRenderDrawColor(r->sdl_renderer, 25, 40, 60, 255);
    SDL_Rect input_bg = {0, input_area_y, window_width, input_area_height};
    SDL_RenderFillRect(r->sdl_renderer, &input_bg);

    int y = input_area_y;
    int visual_area_width = 0;

    /* Render visual area on the left side */
    if (visual_text && visual_length > 0) {
        /* Calculate visual area width in pixels - add one extra cell for empty space at end */
        visual_area_width = (visual_length + 1) * r->cell_w;

        /* Draw visual area background - lighter version of input area */
        SDL_SetRenderDrawColor(r->sdl_renderer, 45, 65, 85, 255);
        SDL_Rect visual_bg = {0, input_area_y, visual_area_width, input_area_height};
        SDL_RenderFillRect(r->sdl_renderer, &visual_bg);

        /* Render visual area text - greenish */
        SDL_Color visual_fg_color = {150, 255, 150, 255};
        SDL_Color visual_bg_color = {45, 65, 85, 255};
        int visual_x = 0;

        /* Render visual text character by character (UTF-8 aware) */
        int i = 0;
        while (i < visual_length) {
            unsigned char byte = (unsigned char)visual_text[i];
            uint32_t codepoint = 0;
            int bytes_consumed = 1;

            /* Decode UTF-8 */
            if (byte < 0x80) {
                codepoint = byte;
            } else if ((byte & 0xE0) == 0xC0 && i + 1 < visual_length) {
                codepoint = ((byte & 0x1F) << 6) | ((unsigned char)visual_text[i + 1] & 0x3F);
                bytes_consumed = 2;
            } else if ((byte & 0xF0) == 0xE0 && i + 2 < visual_length) {
                codepoint = ((byte & 0x0F) << 12) | (((unsigned char)visual_text[i + 1] & 0x3F) << 6) |
                            ((unsigned char)visual_text[i + 2] & 0x3F);
                bytes_consumed = 3;
            } else if ((byte & 0xF8) == 0xF0 && i + 3 < visual_length) {
                codepoint = ((byte & 0x07) << 18) | (((unsigned char)visual_text[i + 1] & 0x3F) << 12) |
                            (((unsigned char)visual_text[i + 2] & 0x3F) << 6) |
                            ((unsigned char)visual_text[i + 3] & 0x3F);
                bytes_consumed = 4;
            } else {
                i++;
                continue;
            }

            /* Render the character */
            SDL_Texture *glyph = glyph_cache_get(r->glyph_cache, codepoint, visual_fg_color, visual_bg_color, 0, 0);
            if (glyph) {
                int tex_w, tex_h;
                SDL_QueryTexture(glyph, NULL, NULL, &tex_w, &tex_h);
                SDL_Rect dst = {visual_x + (r->cell_w - tex_w) / 2, y, tex_w, tex_h};
                SDL_RenderCopy(r->sdl_renderer, glyph, NULL, &dst);
            }
            visual_x += r->cell_w;
            i += bytes_consumed;
        }
    }

    /* Render input text - yellow, starting after visual area */
    SDL_Color fg_color = {255, 255, 0, 255};
    SDL_Color bg_color = {25, 40, 60, 255};
    int x = visual_area_width;

    /* Calculate cursor character position (cursor_pos is in bytes, same as characters for ASCII) */
    int cursor_char_pos = cursor_pos;

    /* Render characters character by character (UTF-8 aware) */
    /* Only render if we have text */
    if (text && text_len > 0) {
        /* Ensure we don't read past the buffer */
        int max_len = text_len < 1000 ? text_len : 1000;
        int i = 0;
        while (i < max_len) {
            /* Bounds check - ensure we don't read past text_len */
            if (i >= text_len)
                break;

            unsigned char byte = (unsigned char)text[i];
            uint32_t codepoint = 0;
            int bytes_consumed = 1; /* How many bytes this character consumed */

            /* Decode UTF-8 */
            if (byte < 0x80) {
                /* ASCII character - use byte value directly */
                codepoint = byte;
            } else if ((byte & 0xE0) == 0xC0 && i + 1 < text_len) {
                /* 2-byte UTF-8 */
                codepoint = ((byte & 0x1F) << 6) | ((unsigned char)text[i + 1] & 0x3F);
                bytes_consumed = 2;
            } else if ((byte & 0xF0) == 0xE0 && i + 2 < text_len) {
                /* 3-byte UTF-8 */
                codepoint = ((byte & 0x0F) << 12) | (((unsigned char)text[i + 1] & 0x3F) << 6) |
                            ((unsigned char)text[i + 2] & 0x3F);
                bytes_consumed = 3;
            } else if ((byte & 0xF8) == 0xF0 && i + 3 < text_len) {
                /* 4-byte UTF-8 */
                codepoint = ((byte & 0x07) << 18) | (((unsigned char)text[i + 1] & 0x3F) << 12) |
                            (((unsigned char)text[i + 2] & 0x3F) << 6) | ((unsigned char)text[i + 3] & 0x3F);
                bytes_consumed = 4;
            } else {
                /* Invalid UTF-8, skip byte */
                i++;
                continue;
            }

            /* Render the character */
            SDL_Texture *glyph = glyph_cache_get(r->glyph_cache, codepoint, fg_color, bg_color, 0, 0);
            if (glyph) {
                int tex_w, tex_h;
                SDL_QueryTexture(glyph, NULL, NULL, &tex_w, &tex_h);
                SDL_Rect dst = {x + (r->cell_w - tex_w) / 2, y, tex_w, tex_h};
                SDL_RenderCopy(r->sdl_renderer, glyph, NULL, &dst);
            }
            x += r->cell_w;
            i += bytes_consumed; /* Advance by number of bytes consumed */
        }
    }

    /* Draw cursor at character position - always draw cursor even if text is empty */
    /* Cursor position accounts for visual area offset */
    int cursor_x = visual_area_width + (cursor_char_pos * r->cell_w);
    SDL_SetRenderDrawColor(r->sdl_renderer, 200, 200, 200, 255);
    SDL_Rect cursor_rect = {cursor_x, y, r->cell_w, r->cell_h};
    SDL_RenderFillRect(r->sdl_renderer, &cursor_rect);
}

void renderer_destroy(Renderer *r) {
    if (!r)
        return;
    free(r);
}
