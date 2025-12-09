/* Renderer backend interface - makes rendering backends swappable */

#ifndef RENDERER_BACKEND_H
#define RENDERER_BACKEND_H

#include "term_cell.h"
#include "terminal.h"

/* Renderer backend interface using function pointers (vtable pattern)
 * This allows swapping rendering backends (SDL, TUI, etc.)
 * without changing the rest of the codebase.
 *
 * Each backend implements these functions and registers them in the RendererBackend struct.
 * The Renderer wrapper in renderer.c delegates to the active backend.
 */
typedef struct RendererBackend {
    /* Lifecycle management */
    void* (*create)(void *platform_context, int cell_w, int cell_h);
    void (*destroy)(void *state);

    /* Frame lifecycle */
    void (*begin_frame)(void *state, int window_width, int window_height);
    void (*end_frame)(void *state);

    /* Cell rendering - renders a single cell at screen position (row, col)
     * Parameters:
     *   state - backend state
     *   term - terminal (for accessing screen/vterm if needed)
     *   row, col - screen position
     *   cell - cell data (character, attributes, colors)
     *   in_selection - whether this cell is selected
     *   cell_w, cell_h - cell dimensions in pixels (for SDL) or ignored (for TUI)
     */
    void (*render_cell)(void *state, Terminal *term, int row, int col,
                       const TermCell *cell, int in_selection,
                       int cell_w, int cell_h);
} RendererBackend;

/* Backend registry - implemented backends */
extern const RendererBackend renderer_backend_sdl;
/* Future backends would be declared here */
/* extern const RendererBackend renderer_backend_tui; */

#endif /* RENDERER_BACKEND_H */
