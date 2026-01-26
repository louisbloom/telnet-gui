/* Platform-specific event waiting implementation
 *
 * Unix: Uses select() with display fd and optional socket fd
 * Windows: Uses MsgWaitForMultipleObjectsEx with WSA events
 */

#include "event_wait.h"
#include "display_fd.h"
#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
#include <winsock2.h>
#include <windows.h>

struct EventWaitCtx {
    SDL_Window *window;
    SOCKET socket_fd;
    WSAEVENT socket_event;
};

EventWaitCtx *event_wait_create(SDL_Window *window) {
    EventWaitCtx *ctx = calloc(1, sizeof(*ctx));
    if (!ctx)
        return NULL;

    ctx->window = window;
    ctx->socket_fd = INVALID_SOCKET;
    ctx->socket_event = WSA_INVALID_EVENT;
    return ctx;
}

void event_wait_destroy(EventWaitCtx *ctx) {
    if (!ctx)
        return;
    if (ctx->socket_event != WSA_INVALID_EVENT)
        WSACloseEvent(ctx->socket_event);
    free(ctx);
}

void event_wait_set_socket(EventWaitCtx *ctx, int sock) {
    if (!ctx)
        return;

    /* Clean up old event if any */
    if (ctx->socket_event != WSA_INVALID_EVENT) {
        WSACloseEvent(ctx->socket_event);
        ctx->socket_event = WSA_INVALID_EVENT;
    }

    ctx->socket_fd = (sock >= 0) ? (SOCKET)sock : INVALID_SOCKET;
    if (ctx->socket_fd != INVALID_SOCKET) {
        ctx->socket_event = WSACreateEvent();
        if (ctx->socket_event != WSA_INVALID_EVENT) {
            WSAEventSelect(ctx->socket_fd, ctx->socket_event, FD_READ | FD_CLOSE);
        }
    }
}

int event_wait(EventWaitCtx *ctx, int timeout_ms) {
    if (!ctx)
        return EVENT_WAIT_TIMEOUT;

    HANDLE handles[1];
    DWORD nCount = 0;

    if (ctx->socket_event != WSA_INVALID_EVENT)
        handles[nCount++] = ctx->socket_event;

    DWORD wait_ms = (timeout_ms < 0) ? INFINITE : (DWORD)timeout_ms;
    DWORD result = MsgWaitForMultipleObjectsEx(nCount, handles, wait_ms, QS_ALLINPUT, MWMO_INPUTAVAILABLE);

    int flags = 0;

    if (result == WAIT_OBJECT_0 && nCount > 0) {
        /* Socket event */
        WSANETWORKEVENTS netEvents;
        if (WSAEnumNetworkEvents(ctx->socket_fd, ctx->socket_event, &netEvents) == 0) {
            if (netEvents.lNetworkEvents & FD_READ)
                flags |= EVENT_WAIT_SOCKET_READ;
            if (netEvents.lNetworkEvents & FD_CLOSE)
                flags |= EVENT_WAIT_SOCKET_ERR;
        }
    } else if (result == WAIT_OBJECT_0 + nCount) {
        /* Windows message available */
        flags |= EVENT_WAIT_DISPLAY;
    }
    /* WAIT_TIMEOUT or other results return 0 (timeout) */

    return flags;
}

void event_wait_pre_wait(EventWaitCtx *ctx) {
    (void)ctx;
}

void event_wait_post_wait(EventWaitCtx *ctx) {
    (void)ctx;
}

#else /* Unix implementation */

#include <sys/select.h>
#include <errno.h>

struct EventWaitCtx {
    SDL_Window *window;
    int display_fd;
    int socket_fd;
};

EventWaitCtx *event_wait_create(SDL_Window *window) {
    EventWaitCtx *ctx = calloc(1, sizeof(*ctx));
    if (!ctx)
        return NULL;

    ctx->window = window;
    ctx->display_fd = get_display_fd(window);
    ctx->socket_fd = -1;

    if (ctx->display_fd < 0) {
        fprintf(stderr, "Warning: Could not get display file descriptor, using polling fallback\n");
        /* Don't fail - we can still work with polling */
    }
    return ctx;
}

void event_wait_destroy(EventWaitCtx *ctx) {
    free(ctx);
}

void event_wait_set_socket(EventWaitCtx *ctx, int sock) {
    if (ctx)
        ctx->socket_fd = sock;
}

int event_wait(EventWaitCtx *ctx, int timeout_ms) {
    if (!ctx)
        return EVENT_WAIT_TIMEOUT;

    /* If no display fd, use polling fallback */
    if (ctx->display_fd < 0) {
        int delay = (timeout_ms >= 0 && timeout_ms < 16) ? timeout_ms : 16;
        if (delay > 0)
            SDL_Delay(delay);
        return EVENT_WAIT_DISPLAY; /* Always check for display events after delay */
    }

    fd_set readfds, exceptfds;
    FD_ZERO(&readfds);
    FD_ZERO(&exceptfds);
    FD_SET(ctx->display_fd, &readfds);

    int max_fd = ctx->display_fd;
    if (ctx->socket_fd >= 0) {
        FD_SET(ctx->socket_fd, &readfds);
        FD_SET(ctx->socket_fd, &exceptfds);
        if (ctx->socket_fd > max_fd)
            max_fd = ctx->socket_fd;
    }

    struct timeval tv, *tvp = NULL;
    if (timeout_ms >= 0) {
        tv.tv_sec = timeout_ms / 1000;
        tv.tv_usec = (timeout_ms % 1000) * 1000;
        tvp = &tv;
    }

    int result = select(max_fd + 1, &readfds, NULL, &exceptfds, tvp);
    if (result < 0 && errno != EINTR)
        return EVENT_WAIT_TIMEOUT;

    int flags = 0;
    if (FD_ISSET(ctx->display_fd, &readfds))
        flags |= EVENT_WAIT_DISPLAY;
    if (ctx->socket_fd >= 0 && FD_ISSET(ctx->socket_fd, &readfds))
        flags |= EVENT_WAIT_SOCKET_READ;
    if (ctx->socket_fd >= 0 && FD_ISSET(ctx->socket_fd, &exceptfds))
        flags |= EVENT_WAIT_SOCKET_ERR;

    return flags;
}

void event_wait_pre_wait(EventWaitCtx *ctx) {
    if (ctx && ctx->window)
        wayland_pre_wait(ctx->window);
}

void event_wait_post_wait(EventWaitCtx *ctx) {
    if (ctx && ctx->window)
        wayland_post_wait(ctx->window);
}

#endif /* _WIN32 */
