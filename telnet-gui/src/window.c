/* Borderless window implementation */

#include "window.h"
#include <stdlib.h>
#include <string.h>

#define TITLEBAR_HEIGHT 30
#define BUTTON_SIZE 16
#define BUTTON_PADDING 4

typedef struct Button {
    int x, y, w, h;
    WindowTitlebarAction action;
} Button;

struct Window {
    SDL_Window *window;
    SDL_Renderer *renderer;
    Button close_button;
    Button minimize_button;
};

static SDL_HitTestResult hit_test(SDL_Window *win, const SDL_Point *area, void *data) {
    int x = area->x, y = area->y;
    int w, h;
    SDL_GetWindowSize(win, &w, &h);

    /* Top 30px is draggable */
    if (y < TITLEBAR_HEIGHT) {
        /* Check for close/minimize buttons */
        if (x > w - BUTTON_SIZE - BUTTON_PADDING && x < w - BUTTON_PADDING && y < BUTTON_SIZE + BUTTON_PADDING) {
            return SDL_HITTEST_RESIZE_TOPRIGHT;
        }
        if (x > w - (BUTTON_SIZE * 2) - (BUTTON_PADDING * 2) && x < w - BUTTON_SIZE - BUTTON_PADDING &&
            y < BUTTON_SIZE + BUTTON_PADDING) {
            return SDL_HITTEST_RESIZE_TOPRIGHT;
        }
        return SDL_HITTEST_DRAGGABLE;
    }

    /* Bottom edge */
    if (y > h - 3) {
        if (x < 3)
            return SDL_HITTEST_RESIZE_BOTTOMLEFT;
        if (x > w - 3)
            return SDL_HITTEST_RESIZE_BOTTOMRIGHT;
        return SDL_HITTEST_RESIZE_BOTTOM;
    }

    /* Left/Right edges */
    if (x < 3)
        return SDL_HITTEST_RESIZE_LEFT;
    if (x > w - 3)
        return SDL_HITTEST_RESIZE_RIGHT;

    return SDL_HITTEST_NORMAL;
}

Window *window_create(const char *title, int width, int height) {
    Window *w = (Window *)malloc(sizeof(Window));
    if (!w)
        return NULL;

    w->window = SDL_CreateWindow(title, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, width, height,
                                 SDL_WINDOW_BORDERLESS | SDL_WINDOW_RESIZABLE);
    if (!w->window) {
        free(w);
        return NULL;
    }

    SDL_SetWindowHitTest(w->window, hit_test, w);

    w->renderer = SDL_CreateRenderer(w->window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (!w->renderer) {
        SDL_DestroyWindow(w->window);
        free(w);
        return NULL;
    }

    /* Initialize buttons */
    w->close_button.x = width - BUTTON_SIZE - BUTTON_PADDING;
    w->close_button.y = BUTTON_PADDING;
    w->close_button.w = BUTTON_SIZE;
    w->close_button.h = BUTTON_SIZE;
    w->close_button.action = WINDOW_TITLEBAR_ACTION_CLOSE;

    w->minimize_button.x = width - (BUTTON_SIZE * 2) - (BUTTON_PADDING * 2);
    w->minimize_button.y = BUTTON_PADDING;
    w->minimize_button.w = BUTTON_SIZE;
    w->minimize_button.h = BUTTON_SIZE;
    w->minimize_button.action = WINDOW_TITLEBAR_ACTION_MINIMIZE;

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

WindowTitlebarAction window_check_titlebar_click(Window *w, int mouse_x, int mouse_y) {
    /* Check close button */
    if (mouse_x >= w->close_button.x && mouse_x < w->close_button.x + w->close_button.w &&
        mouse_y >= w->close_button.y && mouse_y < w->close_button.y + w->close_button.h) {
        return WINDOW_TITLEBAR_ACTION_CLOSE;
    }

    /* Check minimize button */
    if (mouse_x >= w->minimize_button.x && mouse_x < w->minimize_button.x + w->minimize_button.w &&
        mouse_y >= w->minimize_button.y && mouse_y < w->minimize_button.y + w->minimize_button.h) {
        return WINDOW_TITLEBAR_ACTION_MINIMIZE;
    }

    return WINDOW_TITLEBAR_ACTION_NONE;
}

void window_set_title(Window *w, const char *title) {
    SDL_SetWindowTitle(w->window, title);
}

void window_render_titlebar(Window *w, SDL_Renderer *renderer, const char *text) {
    int w_width, w_height;
    SDL_GetWindowSize(w->window, &w_width, &w_height);

    /* Draw titlebar background */
    SDL_SetRenderDrawColor(renderer, 30, 30, 30, 255);
    SDL_Rect titlebar = {0, 0, w_width, TITLEBAR_HEIGHT};
    SDL_RenderFillRect(renderer, &titlebar);

    /* Draw close button */
    SDL_SetRenderDrawColor(renderer, 200, 50, 50, 255);
    SDL_Rect close_rect = {w->close_button.x, w->close_button.y, w->close_button.w, w->close_button.h};
    SDL_RenderFillRect(renderer, &close_rect);

    /* Draw minimize button */
    SDL_SetRenderDrawColor(renderer, 100, 100, 100, 255);
    SDL_Rect min_rect = {w->minimize_button.x, w->minimize_button.y, w->minimize_button.w, w->minimize_button.h};
    SDL_RenderFillRect(renderer, &min_rect);
}

int window_get_titlebar_height(void) {
    return TITLEBAR_HEIGHT;
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
