/* Terminal renderer implementation */

#include "renderer.h"
#include "renderer_backend.h"
#include "terminal.h"
#include <stdlib.h>
#include <stdio.h>
#include <SDL2/SDL.h>

struct Renderer {
    const RendererBackend *backend; /* Backend vtable */
    void *backend_state;            /* Opaque backend state */
    int cell_w, cell_h;
};

/* Factory function - creates SDL renderer by default */
Renderer *renderer_create(SDL_Renderer *sdl_renderer, GlyphCache *glyph_cache, int cell_width, int cell_height) {
    Renderer *r = (Renderer *)malloc(sizeof(Renderer));
    if (!r)
        return NULL;

    /* Select SDL backend */
    r->backend = &renderer_backend_sdl;

    /* Platform context for SDL backend - array of pointers */
    void *ctx[2] = {sdl_renderer, glyph_cache};

    /* Initialize backend */
    r->backend_state = r->backend->create(ctx, cell_width, cell_height);
    if (!r->backend_state) {
        free(r);
        return NULL;
    }

    r->cell_w = cell_width;
    r->cell_h = cell_height;

    return r;
}

void renderer_render(Renderer *r, Terminal *term, const char *title, int selection_active, int sel_start_row,
                     int sel_start_col, int sel_start_offset, int sel_start_scrollback, int sel_end_row,
                     int sel_end_col, int sel_end_offset, int sel_end_scrollback) {
    if (!r || !term)
        return;
    (void)title; /* unused for now */

    /* Get terminal size (scrolling area) and calculate total display size */
    int scrolling_rows, cols;
    terminal_get_size(term, &scrolling_rows, &cols);

    /* Total display includes scrolling area + 2 rows for separator and input area */
    int total_rows = scrolling_rows + 2;
    int window_width = cols * r->cell_w;
    int window_height = total_rows * r->cell_h;

    /* Begin frame - clear screen */
    if (r->backend && r->backend->begin_frame)
        r->backend->begin_frame(r->backend_state, window_width, window_height);

    /* Calculate current viewport rows for selection using absolute scrollback positions */
    int current_offset = terminal_get_viewport_offset(term);
    int current_scrollback = terminal_get_scrollback_size(term);

    /* Calculate the absolute scrollback index where the selection is located */
    /* When offset > 0: viewport row R shows scrollback[scrollback_size - offset + R] */
    /* When offset = 0: viewport row R is on live screen at position scrollback_size + R */
    int start_scrollback_index = sel_start_scrollback - sel_start_offset + sel_start_row;
    int end_scrollback_index = sel_end_scrollback - sel_end_offset + sel_end_row;

    /* Convert absolute scrollback indices to current viewport rows */
    /* Current viewport row 0 shows scrollback[current_scrollback - current_offset] */
    int display_start_row = start_scrollback_index - (current_scrollback - current_offset);
    int display_end_row = end_scrollback_index - (current_scrollback - current_offset);

    /* Normalize selection (ensure start < end) for rendering */
    int norm_start_row = display_start_row;
    int norm_start_col = sel_start_col;
    int norm_end_row = display_end_row;
    int norm_end_col = sel_end_col;
    if (selection_active && (display_start_row > display_end_row ||
                             (display_start_row == display_end_row && sel_start_col > sel_end_col))) {
        norm_start_row = display_end_row;
        norm_start_col = sel_end_col;
        norm_end_row = display_start_row;
        norm_end_col = sel_start_col;
    }

    /* Render each cell (includes scrolling area + input area) */
    for (int row = 0; row < total_rows; row++) {
        for (int col = 0; col < cols; col++) {
            TermCell cell;
            /* Get cell considering viewport offset */
            if (!terminal_get_cell_at(term, row, col, &cell))
                continue;

            /* Check if this cell is in selection */
            int in_selection = 0;
            if (selection_active) {
                if (row > norm_start_row && row < norm_end_row) {
                    in_selection = 1;
                } else if (row == norm_start_row && row == norm_end_row) {
                    in_selection = (col >= norm_start_col && col <= norm_end_col);
                } else if (row == norm_start_row) {
                    in_selection = (col >= norm_start_col);
                } else if (row == norm_end_row) {
                    in_selection = (col <= norm_end_col);
                }
            }

            /* Delegate cell rendering to backend */
            if (r->backend && r->backend->render_cell) {
                r->backend->render_cell(r->backend_state, term, row, col, &cell, in_selection, r->cell_w, r->cell_h);
            }
        }
    }

    /* End frame */
    if (r->backend && r->backend->end_frame)
        r->backend->end_frame(r->backend_state);

    /* Don't draw terminal cursor - user input is handled by input area at bottom */
}

void renderer_destroy(Renderer *r) {
    if (!r)
        return;
    if (r->backend && r->backend->destroy)
        r->backend->destroy(r->backend_state);
    free(r);
}
