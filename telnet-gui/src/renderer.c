/* Terminal renderer implementation */

#include "renderer.h"
#include "lisp_bridge.h"
#include "../../libtelnet-lisp/include/utf8.h"
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

                /* Get default terminal colors from Lisp config */
                int term_fg_r, term_fg_g, term_fg_b, term_bg_r, term_bg_g, term_bg_b;
                lisp_bridge_get_terminal_fg_color(&term_fg_r, &term_fg_g, &term_fg_b);
                lisp_bridge_get_terminal_bg_color(&term_bg_r, &term_bg_g, &term_bg_b);

                SDL_Color fg_color = {term_fg_r, term_fg_g, term_fg_b, 255};
                if (VTERM_COLOR_IS_RGB(&fg_color_processed)) {
                    fg_color.r = fg_color_processed.rgb.red;
                    fg_color.g = fg_color_processed.rgb.green;
                    fg_color.b = fg_color_processed.rgb.blue;
                }

                SDL_Color bg_color = {term_bg_r, term_bg_g, term_bg_b, 255};
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
                                int window_height, int input_area_height, const char *mode_text, int mode_length,
                                int selection_start, int selection_end) {
    if (!r)
        return;

    int input_area_y = window_height - input_area_height;

    /* Get colors from Lisp config */
    int fg_r, fg_g, fg_b, bg_r, bg_g, bg_b;
    int sel_fg_r, sel_fg_g, sel_fg_b, sel_bg_r, sel_bg_g, sel_bg_b;
    int cursor_r, cursor_g, cursor_b, sep_r, sep_g, sep_b;

    lisp_bridge_get_input_area_fg_color(&fg_r, &fg_g, &fg_b);
    lisp_bridge_get_input_area_bg_color(&bg_r, &bg_g, &bg_b);
    lisp_bridge_get_selection_fg_color(&sel_fg_r, &sel_fg_g, &sel_fg_b);
    lisp_bridge_get_selection_bg_color(&sel_bg_r, &sel_bg_g, &sel_bg_b);
    lisp_bridge_get_cursor_color(&cursor_r, &cursor_g, &cursor_b);
    lisp_bridge_get_input_separator_color(&sep_r, &sep_g, &sep_b);

    /* Draw separator line between terminal and input area */
    SDL_SetRenderDrawColor(r->sdl_renderer, sep_r, sep_g, sep_b, 255);
    SDL_RenderDrawLine(r->sdl_renderer, 0, input_area_y, window_width, input_area_y);

    /* Draw input area background */
    SDL_SetRenderDrawColor(r->sdl_renderer, bg_r, bg_g, bg_b, 255);
    SDL_Rect input_bg = {0, input_area_y, window_width, input_area_height};
    SDL_RenderFillRect(r->sdl_renderer, &input_bg);

    int y = input_area_y;

    /* Normalize selection range (ensure start < end) */
    int sel_start = selection_start < selection_end ? selection_start : selection_end;
    int sel_end = selection_start < selection_end ? selection_end : selection_start;
    int has_selection = (sel_start != sel_end);

    /* Create SDL color structs */
    SDL_Color fg_color = {fg_r, fg_g, fg_b, 255};
    SDL_Color bg_color = {bg_r, bg_g, bg_b, 255};
    SDL_Color sel_fg_color = {sel_fg_r, sel_fg_g, sel_fg_b, 255};
    SDL_Color sel_bg_color = {sel_bg_r, sel_bg_g, sel_bg_b, 255};
    int x = 0;

    /* Calculate cursor character position (cursor_pos is in bytes, same as characters for ASCII) */
    int cursor_char_pos = cursor_pos;

    /* Draw selection background as a filled rectangle */
    if (has_selection) {
        int sel_x_start = sel_start * r->cell_w;
        int sel_width = (sel_end - sel_start) * r->cell_w;
        SDL_SetRenderDrawColor(r->sdl_renderer, sel_bg_color.r, sel_bg_color.g, sel_bg_color.b, sel_bg_color.a);
        SDL_Rect sel_rect = {sel_x_start, y, sel_width, r->cell_h};
        SDL_RenderFillRect(r->sdl_renderer, &sel_rect);
    }

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

            /* Decode UTF-8 using utf8 library */
            int codepoint = utf8_get_codepoint(&text[i]);
            if (codepoint < 0) {
                /* Invalid UTF-8, skip byte */
                i++;
                continue;
            }
            int bytes_consumed = utf8_char_bytes(&text[i]);

            /* Check if character is in selection - use black text for visibility on orange bg */
            SDL_Color char_fg_color = fg_color;
            if (has_selection && i >= sel_start && i < sel_end) {
                char_fg_color = sel_fg_color; /* Black text on orange background */
            }

            /* Render the character (bg_color not used by glyph cache, just for cache key) */
            SDL_Texture *glyph = glyph_cache_get(r->glyph_cache, codepoint, char_fg_color, bg_color, 0, 0);
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

    /* Draw cursor as a vertical line at character position */
    int cursor_x = cursor_char_pos * r->cell_w;
    SDL_SetRenderDrawColor(r->sdl_renderer, cursor_r, cursor_g, cursor_b, 255);
    /* Draw a 2-pixel wide vertical line for the cursor */
    SDL_Rect cursor_line = {cursor_x, y, 2, r->cell_h};
    SDL_RenderFillRect(r->sdl_renderer, &cursor_line);

    /* Render mode display area on the right side */
    if (mode_text && mode_length > 0) {
        /* Get mode display area colors from Lisp config */
        int mode_fg_r, mode_fg_g, mode_fg_b, mode_bg_r, mode_bg_g, mode_bg_b;
        lisp_bridge_get_mode_fg_color(&mode_fg_r, &mode_fg_g, &mode_fg_b);
        lisp_bridge_get_mode_bg_color(&mode_bg_r, &mode_bg_g, &mode_bg_b);

        /* Calculate mode display area width with left and right padding */
        /* Count visual characters and estimate width (emojis are ~2.2x wider than cells) */
        int visual_char_count = 0;
        int emoji_count = 0;
        int char_idx = 0;
        while (char_idx < mode_length) {
            int bytes = utf8_char_bytes(&mode_text[char_idx]);
            if (bytes > 0) {
                visual_char_count++;
                /* 4-byte UTF-8 is likely an emoji */
                if (bytes == 4) {
                    emoji_count++;
                }
                char_idx += bytes;
            } else {
                /* Invalid UTF-8, skip byte */
                char_idx++;
            }
        }

        int mode_padding = 8; /* pixels of padding on each side */
        /* Calculate text width: rendering loop advances by cell_w for each character
         * After rendering all characters, cursor is at: start + (visual_char_count * cell_w)
         * But glyphs may extend beyond their cell boundaries (emojis are ~22px vs 10px cell)
         * Emojis are centered, so they extend (emoji_width - cell_w)/2 on each side
         * The rightmost pixel is: cursor_pos + right_overhang_of_last_glyph
         * For now, estimate that last glyph adds ~6px overhang (half of 22-10=12) */
        int mode_text_width = visual_char_count * r->cell_w;
        if (emoji_count > 0) {
            /* Emoji glyphs are ~22px wide, cells are ~10px, so they overhang by ~6px on right */
            mode_text_width += r->cell_w * 6 / 10; /* Add 60% of cell_w for emoji overhang */
        }
        int mode_area_width = mode_text_width + (mode_padding * 2);
        int mode_x = window_width - mode_area_width;

        /* Draw mode display area background */
        SDL_SetRenderDrawColor(r->sdl_renderer, mode_bg_r, mode_bg_g, mode_bg_b, 255);
        SDL_Rect mode_bg = {mode_x, input_area_y, mode_area_width, input_area_height};
        SDL_RenderFillRect(r->sdl_renderer, &mode_bg);

        /* Create mode color structs */
        SDL_Color mode_fg_color = {mode_fg_r, mode_fg_g, mode_fg_b, 255};
        SDL_Color mode_bg_color = {mode_bg_r, mode_bg_g, mode_bg_b, 255};
        int text_x = mode_x + mode_padding; /* Start text after left padding */

        /* Render mode text character by character (UTF-8 aware) */
        int i = 0;
        while (i < mode_length) {
            /* Decode UTF-8 using utf8 library */
            int codepoint = utf8_get_codepoint(&mode_text[i]);
            if (codepoint < 0) {
                /* Invalid UTF-8, skip byte */
                i++;
                continue;
            }
            int bytes_consumed = utf8_char_bytes(&mode_text[i]);

            /* Render the character */
            SDL_Texture *glyph = glyph_cache_get(r->glyph_cache, codepoint, mode_fg_color, mode_bg_color, 0, 0);
            if (glyph) {
                int tex_w, tex_h;
                SDL_QueryTexture(glyph, NULL, NULL, &tex_w, &tex_h);
                SDL_Rect dst = {text_x + (r->cell_w - tex_w) / 2, y, tex_w, tex_h};
                SDL_RenderCopy(r->sdl_renderer, glyph, NULL, &dst);
            }
            text_x += r->cell_w;
            i += bytes_consumed;
        }
    }
}

void renderer_destroy(Renderer *r) {
    if (!r)
        return;
    free(r);
}
