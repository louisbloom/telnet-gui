/* Display file descriptor helpers for event loop
 *
 * This file is separate from main.c to avoid typedef conflicts between
 * X11's "typedef XID Window" and our "typedef struct Window Window".
 */

#include "display_fd.h"
#include <SDL2/SDL_syswm.h>

#ifndef _WIN32
/* Wayland client for wl_display_flush/dispatch_pending */
#ifdef SDL_VIDEO_DRIVER_WAYLAND
#include <wayland-client.h>
#endif
#endif

int get_display_fd(SDL_Window *window) {
#ifdef _WIN32
    (void)window;
    return -1; /* Windows: no fd, use fallback */
#else
    SDL_SysWMinfo info;
    SDL_VERSION(&info.version);

    if (!SDL_GetWindowWMInfo(window, &info)) {
        return -1;
    }

    switch (info.subsystem) {
#ifdef SDL_VIDEO_DRIVER_X11
    case SDL_SYSWM_X11:
        return ConnectionNumber(info.info.x11.display);
#endif
#ifdef SDL_VIDEO_DRIVER_WAYLAND
    case SDL_SYSWM_WAYLAND:
        return wl_display_get_fd(info.info.wl.display);
#endif
    default:
        return -1;
    }
#endif
}

void wayland_pre_wait(SDL_Window *window) {
#if !defined(_WIN32) && defined(SDL_VIDEO_DRIVER_WAYLAND)
    SDL_SysWMinfo info;
    SDL_VERSION(&info.version);
    if (SDL_GetWindowWMInfo(window, &info) && info.subsystem == SDL_SYSWM_WAYLAND) {
        wl_display_flush(info.info.wl.display);
    }
#else
    (void)window;
#endif
}

void wayland_post_wait(SDL_Window *window) {
#if !defined(_WIN32) && defined(SDL_VIDEO_DRIVER_WAYLAND)
    SDL_SysWMinfo info;
    SDL_VERSION(&info.version);
    if (SDL_GetWindowWMInfo(window, &info) && info.subsystem == SDL_SYSWM_WAYLAND) {
        wl_display_dispatch_pending(info.info.wl.display);
    }
#else
    (void)window;
#endif
}
