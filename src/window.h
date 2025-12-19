/* Window management */

#ifndef WINDOW_H
#define WINDOW_H

#include <SDL2/SDL.h>

typedef struct Window Window;

/* Create a window */
Window *window_create(const char *title, int width, int height);

/* Get the SDL window */
SDL_Window *window_get_sdl_window(Window *w);

/* Get the SDL renderer */
SDL_Renderer *window_get_sdl_renderer(Window *w);

/* Get window dimensions */
void window_get_size(Window *w, int *width, int *height);

/* Update titlebar with connection info */
void window_set_title(Window *w, const char *title);

/* Clean up */
void window_destroy(Window *w);

#endif /* WINDOW_H */
