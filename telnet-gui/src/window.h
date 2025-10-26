/* Borderless window with custom titlebar */

#ifndef WINDOW_H
#define WINDOW_H

#include <SDL2/SDL.h>

typedef struct Window Window;

typedef enum {
    WINDOW_TITLEBAR_ACTION_NONE,
    WINDOW_TITLEBAR_ACTION_CLOSE,
    WINDOW_TITLEBAR_ACTION_MINIMIZE,
} WindowTitlebarAction;

/* Create a borderless window */
Window *window_create(const char *title, int width, int height);

/* Get the SDL window */
SDL_Window *window_get_sdl_window(Window *w);

/* Get the SDL renderer */
SDL_Renderer *window_get_sdl_renderer(Window *w);

/* Get window dimensions */
void window_get_size(Window *w, int *width, int *height);

/* Get titlebar height */
int window_get_titlebar_height(void);

/* Check if close button was clicked */
WindowTitlebarAction window_check_titlebar_click(Window *w, int mouse_x, int mouse_y);

/* Update titlebar with connection info */
void window_set_title(Window *w, const char *title);

/* Render the titlebar */
void window_render_titlebar(Window *w, SDL_Renderer *renderer, const char *text);

/* Clean up */
void window_destroy(Window *w);

#endif /* WINDOW_H */
