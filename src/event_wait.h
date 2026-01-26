/* Event waiting API using SDL's event queue with background socket monitoring
 *
 * Uses SDL_WaitEventTimeout() in the main loop with a background thread
 * that monitors sockets and pushes custom SDL events when data arrives.
 * This allows proper handling of key repeats (each event rendered separately).
 */

#ifndef EVENT_WAIT_H
#define EVENT_WAIT_H

#include <SDL2/SDL.h>

/* Opaque context for socket monitoring thread */
typedef struct EventWaitCtx EventWaitCtx;

/* Initialize event system (registers SDL user events)
 * Must be called after SDL_Init() */
void event_wait_init(void);

/* Get SDL event types for socket notifications */
Uint32 event_wait_get_socket_read_event(void);
Uint32 event_wait_get_socket_error_event(void);

/* Create event wait context */
EventWaitCtx *event_wait_create(void);

/* Destroy event wait context (stops monitoring thread) */
void event_wait_destroy(EventWaitCtx *ctx);

/* Start/stop socket monitoring thread
 * sock: socket fd to monitor, or -1 to stop monitoring */
void event_wait_set_socket(EventWaitCtx *ctx, int sock);

#endif /* EVENT_WAIT_H */
