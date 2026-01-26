/* Platform-agnostic event waiting API
 *
 * Abstracts the platform-specific event waiting mechanism behind a clean
 * interface. On Unix, uses select() with display and socket file descriptors.
 * On Windows, uses MsgWaitForMultipleObjectsEx with WSA events.
 */

#ifndef EVENT_WAIT_H
#define EVENT_WAIT_H

#include <SDL2/SDL.h>

/* Result flags from event_wait() */
#define EVENT_WAIT_TIMEOUT     0
#define EVENT_WAIT_DISPLAY     (1 << 0)  /* Display/GUI events ready */
#define EVENT_WAIT_SOCKET_READ (1 << 1)  /* Socket has data to read */
#define EVENT_WAIT_SOCKET_ERR  (1 << 2)  /* Socket exception/error */

/* Opaque context for event waiting */
typedef struct EventWaitCtx EventWaitCtx;

/* Create event wait context
 * window: SDL window (for display fd on Unix)
 * Returns: context pointer, or NULL on error */
EventWaitCtx *event_wait_create(SDL_Window *window);

/* Destroy event wait context */
void event_wait_destroy(EventWaitCtx *ctx);

/* Set the socket to monitor (call when connection state changes)
 * sock: socket fd, or -1 to disable socket monitoring */
void event_wait_set_socket(EventWaitCtx *ctx, int sock);

/* Wait for events
 * timeout_ms: timeout in milliseconds, -1 for infinite
 * Returns: bitmask of EVENT_WAIT_* flags */
int event_wait(EventWaitCtx *ctx, int timeout_ms);

/* Wayland-specific: call before blocking */
void event_wait_pre_wait(EventWaitCtx *ctx);

/* Wayland-specific: call after waking */
void event_wait_post_wait(EventWaitCtx *ctx);

#endif /* EVENT_WAIT_H */
