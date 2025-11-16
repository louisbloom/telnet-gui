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

typedef enum {
    RESIZE_NONE,
    RESIZE_LEFT,
    RESIZE_RIGHT,
    RESIZE_BOTTOM,
    RESIZE_BOTTOMLEFT,
    RESIZE_BOTTOMRIGHT,
} ResizeMode;

/* Create a borderless window */
Window *window_create(const char *title, int width, int height);

/* Get the SDL window */
SDL_Window *window_get_sdl_window(Window *w);

/* Get the SDL renderer */
SDL_Renderer *window_get_sdl_renderer(Window *w);

/* Get window dimensions */
void window_get_size(Window *w, int *width, int *height);

/* Update button positions after window resize */
void window_update_button_positions(Window *w);

/* Check if point is in resize area and return resize mode */
ResizeMode window_check_resize_area(Window *w, int x, int y);

/* Start resize operation */
void window_start_resize(Window *w, ResizeMode resize_mode, int mouse_x, int mouse_y);

/* Update resize operation */
void window_update_resize(Window *w, int mouse_x, int mouse_y);

/* End resize operation */
void window_end_resize(Window *w);

/* Check if resize is active */
int window_is_resizing(Window *w);

/* Get titlebar height */
int window_get_titlebar_height(void);

/* Get resize bar height */
int window_get_resize_bar_height(void);

/* Check if close button was clicked */
WindowTitlebarAction window_check_titlebar_click(Window *w, int mouse_x, int mouse_y);

/* Update titlebar with connection info */
void window_set_title(Window *w, const char *title);

/* Render the titlebar */
void window_render_titlebar(Window *w, SDL_Renderer *renderer, const char *text);

/* Render the resize bar at bottom of window */
void window_render_resize_bar(Window *w, SDL_Renderer *renderer);

/* Clean up */
void window_destroy(Window *w);

#endif /* WINDOW_H */
