/* Thread-based socket monitoring for SDL event loop
 *
 * Uses a background thread to monitor sockets with select() and push
 * custom SDL events when data arrives. This integrates cleanly with
 * SDL_WaitEventTimeout() in the main loop.
 */

#include "event_wait.h"
#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
#include <winsock2.h>
#include <windows.h>
#else
#include <sys/select.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#endif

/* Registered SDL event types for socket notifications */
static Uint32 socket_read_event_type = 0;
static Uint32 socket_error_event_type = 0;

void event_wait_init(void) {
    /* Register two custom event types: socket read and socket error */
    Uint32 base = SDL_RegisterEvents(2);
    if (base == (Uint32)-1) {
        fprintf(stderr, "Warning: Failed to register SDL events for socket monitoring\n");
        return;
    }
    socket_read_event_type = base;
    socket_error_event_type = base + 1;
}

Uint32 event_wait_get_socket_read_event(void) {
    return socket_read_event_type;
}

Uint32 event_wait_get_socket_error_event(void) {
    return socket_error_event_type;
}

#ifdef _WIN32

/* Windows implementation using WSAEventSelect */
struct EventWaitCtx {
    SDL_Thread *thread;
    SDL_mutex *mutex;
    SOCKET socket_fd;
    WSAEVENT shutdown_event; /* Signaled to stop the thread */
    int running;
};

static int socket_monitor_thread(void *data) {
    EventWaitCtx *ctx = (EventWaitCtx *)data;

    while (1) {
        SOCKET sock;
        WSAEVENT shutdown_ev;

        /* Get current state under lock */
        SDL_LockMutex(ctx->mutex);
        if (!ctx->running) {
            SDL_UnlockMutex(ctx->mutex);
            break;
        }
        sock = ctx->socket_fd;
        shutdown_ev = ctx->shutdown_event;
        SDL_UnlockMutex(ctx->mutex);

        if (sock == INVALID_SOCKET) {
            /* No socket to monitor, wait for shutdown or socket change */
            WaitForSingleObject(shutdown_ev, 100);
            continue;
        }

        /* Create event for socket */
        WSAEVENT sock_event = WSACreateEvent();
        if (sock_event == WSA_INVALID_EVENT) {
            SDL_Delay(100);
            continue;
        }

        /* Associate socket with event for read and close notifications */
        if (WSAEventSelect(sock, sock_event, FD_READ | FD_CLOSE) != 0) {
            WSACloseEvent(sock_event);
            SDL_Delay(100);
            continue;
        }

        /* Wait for either socket event or shutdown */
        HANDLE handles[2] = {sock_event, shutdown_ev};
        DWORD result = WaitForMultipleObjects(2, handles, FALSE, INFINITE);

        if (result == WAIT_OBJECT_0) {
            /* Socket event - check what happened */
            WSANETWORKEVENTS netEvents;
            if (WSAEnumNetworkEvents(sock, sock_event, &netEvents) == 0) {
                SDL_Event event;
                SDL_memset(&event, 0, sizeof(event));

                if (netEvents.lNetworkEvents & FD_READ) {
                    event.type = socket_read_event_type;
                    SDL_PushEvent(&event);
                }
                if (netEvents.lNetworkEvents & FD_CLOSE) {
                    event.type = socket_error_event_type;
                    SDL_PushEvent(&event);
                }
            }
        }
        /* WAIT_OBJECT_0 + 1 = shutdown event, or WAIT_FAILED - just loop and check running */

        WSACloseEvent(sock_event);

        /* Brief sleep to avoid tight loop on errors */
        SDL_LockMutex(ctx->mutex);
        int still_running = ctx->running;
        SDL_UnlockMutex(ctx->mutex);
        if (!still_running)
            break;
    }

    return 0;
}

EventWaitCtx *event_wait_create(void) {
    EventWaitCtx *ctx = calloc(1, sizeof(*ctx));
    if (!ctx)
        return NULL;

    ctx->mutex = SDL_CreateMutex();
    if (!ctx->mutex) {
        free(ctx);
        return NULL;
    }

    ctx->shutdown_event = WSACreateEvent();
    if (ctx->shutdown_event == WSA_INVALID_EVENT) {
        SDL_DestroyMutex(ctx->mutex);
        free(ctx);
        return NULL;
    }

    ctx->socket_fd = INVALID_SOCKET;
    ctx->running = 1;

    ctx->thread = SDL_CreateThread(socket_monitor_thread, "SocketMonitor", ctx);
    if (!ctx->thread) {
        WSACloseEvent(ctx->shutdown_event);
        SDL_DestroyMutex(ctx->mutex);
        free(ctx);
        return NULL;
    }

    return ctx;
}

void event_wait_destroy(EventWaitCtx *ctx) {
    if (!ctx)
        return;

    /* Signal thread to stop */
    SDL_LockMutex(ctx->mutex);
    ctx->running = 0;
    SDL_UnlockMutex(ctx->mutex);
    WSASetEvent(ctx->shutdown_event);

    /* Wait for thread to finish */
    SDL_WaitThread(ctx->thread, NULL);

    WSACloseEvent(ctx->shutdown_event);
    SDL_DestroyMutex(ctx->mutex);
    free(ctx);
}

void event_wait_set_socket(EventWaitCtx *ctx, int sock) {
    if (!ctx)
        return;

    SDL_LockMutex(ctx->mutex);
    ctx->socket_fd = (sock >= 0) ? (SOCKET)sock : INVALID_SOCKET;
    SDL_UnlockMutex(ctx->mutex);

    /* Wake up the monitoring thread to notice the change */
    WSASetEvent(ctx->shutdown_event);
    /* Reset the event so it can be used again */
    WSAResetEvent(ctx->shutdown_event);
}

#else /* Unix implementation */

struct EventWaitCtx {
    SDL_Thread *thread;
    SDL_mutex *mutex;
    int socket_fd;
    int shutdown_pipe[2]; /* Write to [1] to wake thread, read from [0] in select */
    int running;
};

static int socket_monitor_thread(void *data) {
    EventWaitCtx *ctx = (EventWaitCtx *)data;

    while (1) {
        int sock;
        int shutdown_fd;

        /* Get current state under lock */
        SDL_LockMutex(ctx->mutex);
        if (!ctx->running) {
            SDL_UnlockMutex(ctx->mutex);
            break;
        }
        sock = ctx->socket_fd;
        shutdown_fd = ctx->shutdown_pipe[0];
        SDL_UnlockMutex(ctx->mutex);

        /* Set up select */
        fd_set readfds, exceptfds;
        FD_ZERO(&readfds);
        FD_ZERO(&exceptfds);

        FD_SET(shutdown_fd, &readfds);
        int max_fd = shutdown_fd;

        if (sock >= 0) {
            FD_SET(sock, &readfds);
            FD_SET(sock, &exceptfds);
            if (sock > max_fd)
                max_fd = sock;
        }

        /* Use a timeout so we can check running flag periodically */
        struct timeval tv;
        tv.tv_sec = 0;
        tv.tv_usec = 100000; /* 100ms */

        int result = select(max_fd + 1, &readfds, NULL, &exceptfds, &tv);

        if (result < 0 && errno != EINTR) {
            /* Select error - brief delay and retry */
            SDL_Delay(10);
            continue;
        }

        /* Check if we should stop */
        SDL_LockMutex(ctx->mutex);
        int still_running = ctx->running;
        SDL_UnlockMutex(ctx->mutex);
        if (!still_running)
            break;

        /* Check shutdown pipe */
        if (FD_ISSET(shutdown_fd, &readfds)) {
            /* Drain the pipe */
            char buf[64];
            while (read(shutdown_fd, buf, sizeof(buf)) > 0)
                ;
            /* Check if we should stop or just socket changed */
            SDL_LockMutex(ctx->mutex);
            still_running = ctx->running;
            SDL_UnlockMutex(ctx->mutex);
            if (!still_running)
                break;
            continue; /* Socket changed, loop back */
        }

        /* Check socket */
        if (sock >= 0) {
            SDL_Event event;
            SDL_memset(&event, 0, sizeof(event));

            if (FD_ISSET(sock, &exceptfds)) {
                event.type = socket_error_event_type;
                SDL_PushEvent(&event);
            }
            if (FD_ISSET(sock, &readfds)) {
                event.type = socket_read_event_type;
                SDL_PushEvent(&event);
            }
        }
    }

    return 0;
}

EventWaitCtx *event_wait_create(void) {
    EventWaitCtx *ctx = calloc(1, sizeof(*ctx));
    if (!ctx)
        return NULL;

    ctx->mutex = SDL_CreateMutex();
    if (!ctx->mutex) {
        free(ctx);
        return NULL;
    }

    /* Create shutdown pipe */
    if (pipe(ctx->shutdown_pipe) < 0) {
        SDL_DestroyMutex(ctx->mutex);
        free(ctx);
        return NULL;
    }

    /* Make pipe non-blocking */
    fcntl(ctx->shutdown_pipe[0], F_SETFL, O_NONBLOCK);
    fcntl(ctx->shutdown_pipe[1], F_SETFL, O_NONBLOCK);

    ctx->socket_fd = -1;
    ctx->running = 1;

    ctx->thread = SDL_CreateThread(socket_monitor_thread, "SocketMonitor", ctx);
    if (!ctx->thread) {
        close(ctx->shutdown_pipe[0]);
        close(ctx->shutdown_pipe[1]);
        SDL_DestroyMutex(ctx->mutex);
        free(ctx);
        return NULL;
    }

    return ctx;
}

void event_wait_destroy(EventWaitCtx *ctx) {
    if (!ctx)
        return;

    /* Signal thread to stop */
    SDL_LockMutex(ctx->mutex);
    ctx->running = 0;
    SDL_UnlockMutex(ctx->mutex);

    /* Wake up select */
    char c = 1;
    if (write(ctx->shutdown_pipe[1], &c, 1) < 0) {
        /* Ignore write errors - thread will timeout anyway */
    }

    /* Wait for thread to finish */
    SDL_WaitThread(ctx->thread, NULL);

    close(ctx->shutdown_pipe[0]);
    close(ctx->shutdown_pipe[1]);
    SDL_DestroyMutex(ctx->mutex);
    free(ctx);
}

void event_wait_set_socket(EventWaitCtx *ctx, int sock) {
    if (!ctx)
        return;

    SDL_LockMutex(ctx->mutex);
    ctx->socket_fd = sock;
    SDL_UnlockMutex(ctx->mutex);

    /* Wake up the monitoring thread to notice the change */
    char c = 1;
    if (write(ctx->shutdown_pipe[1], &c, 1) < 0) {
        /* Ignore write errors */
    }
}

#endif /* _WIN32 */
