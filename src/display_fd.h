/* Display file descriptor helpers for event loop */

#ifndef DISPLAY_FD_H
#define DISPLAY_FD_H

#include <SDL2/SDL.h>

/* Get display file descriptor for select() - returns -1 if unavailable */
int get_display_fd(SDL_Window *window);

/* Wayland-specific pre-wait handling (flush outgoing requests) */
void wayland_pre_wait(SDL_Window *window);

/* Wayland-specific post-wait handling (dispatch pending events) */
void wayland_post_wait(SDL_Window *window);

#endif /* DISPLAY_FD_H */
