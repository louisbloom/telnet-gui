/* Window implementation */

#include "window.h"
#include <stdlib.h>

struct Window {
    SDL_Window *window;
    SDL_Renderer *renderer;
};

Window *window_create(const char *title, int width, int height) {
    Window *w = (Window *)malloc(sizeof(Window));
    if (!w)
        return NULL;

    /* Set SDL hints BEFORE creating renderer for proper effect */
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1"); /* Linear scaling for smooth text */

    w->window =
        SDL_CreateWindow(title, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width, height, SDL_WINDOW_RESIZABLE);
    if (!w->window) {
        free(w);
        return NULL;
    }

    w->renderer = SDL_CreateRenderer(w->window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (!w->renderer) {
        SDL_DestroyWindow(w->window);
        free(w);
        return NULL;
    }

    /* Set blend mode for proper alpha blending */
    SDL_SetRenderDrawBlendMode(w->renderer, SDL_BLENDMODE_BLEND);

    /* Use linear filtering for best quality text rendering (0=nearest, 1=linear) */
    /* Note: SDL_HINT_RENDER_INTEGER_SCALE requires SDL 2.0.5+, not available in this version */
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1");

    return w;
}

SDL_Window *window_get_sdl_window(Window *w) {
    return w->window;
}

SDL_Renderer *window_get_sdl_renderer(Window *w) {
    return w->renderer;
}

void window_get_size(Window *w, int *width, int *height) {
    SDL_GetWindowSize(w->window, width, height);
}

void window_set_title(Window *w, const char *title) {
    SDL_SetWindowTitle(w->window, title);
}

void window_destroy(Window *w) {
    if (!w)
        return;
    if (w->renderer)
        SDL_DestroyRenderer(w->renderer);
    if (w->window)
        SDL_DestroyWindow(w->window);
    free(w);
}
