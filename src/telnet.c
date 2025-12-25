/* Telnet protocol implementation (RFC 854) */

#include "telnet.h"
#include "lisp.h"
#include "dynamic_buffer.h"
#include "../../telnet-lisp/include/lisp.h"
#include "../../telnet-lisp/include/file_utils.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <time.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <direct.h>
#define mkdir _mkdir
#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <mstcpip.h> /* For tcp_keepalive struct and SIO_KEEPALIVE_VALS */
#define close(s) closesocket(s)
#define sleep(s) Sleep(s * 1000)
#else
#include <netinet/tcp.h> /* For TCP_KEEPIDLE, TCP_KEEPINTVL, TCP_KEEPCNT */
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <fcntl.h>
#endif

#define IAC ((unsigned char)255)
#define SE 240
#define NOP 241
#define DM 242
#define BRK 243
#define IP 244
#define AO 245
#define AYT 246
#define EC 247
#define EL 248
#define GA 249
#define SB 250
#define WILL 251
#define WONT 252
#define DO 253
#define DONT 254

#define OPT_ECHO 1
#define OPT_SUPPRESS_GO_AHEAD 3
#define OPT_TERMINAL_TYPE 24
#define OPT_NAWS 31

typedef enum {
    TELNET_DATA_NORMAL,
    TELNET_DATA_IAC,
    TELNET_DATA_WILL,
    TELNET_DATA_WONT,
    TELNET_DATA_DO,
    TELNET_DATA_DONT,
} TelnetDataState;

struct Telnet {
    int socket;
    TelnetState state;
    int rows, cols;
    FILE *log_file;                   /* Log file handle for I/O logging */
    char log_filename[1024];          /* Path to current log file */
    DynamicBuffer *send_buffer;       /* Buffer for IAC escaping (reused across calls) */
    DynamicBuffer *crlf_buffer;       /* Buffer for adding CRLF to Lisp sends (reused across calls) */
    DynamicBuffer *user_input_buffer; /* Buffer for user input LF->CRLF conversion */
};

/* Send Telnet command */
static int telnet_send_command(int socket, int cmd, int opt) {
    unsigned char buf[3];
    buf[0] = IAC;
    buf[1] = cmd;
    buf[2] = opt;
    return send(socket, (char *)buf, 3, 0);
}

/* ===================================================================
 * TELNET I/O LOGGING HELPER FUNCTIONS
 * =================================================================== */

/* Get current timestamp in ISO 8601 filesystem-safe format: YYYY-MM-DDTHH-MM-SS */
static void get_timestamp_iso(char *buffer, size_t size) {
    time_t now = time(NULL);
    struct tm *tm_info = localtime(&now);
    strftime(buffer, size, "%Y-%m-%dT%H-%M-%S", tm_info);
}

/* Open log file for telnet session */
static int telnet_open_log(Telnet *t, const char *log_dir) {
    Environment *lisp_env = (Environment *)lisp_x_get_environment();
    if (!lisp_env) {
        t->log_file = NULL;
        return 0;
    }

    /* Check if logging is enabled via Lisp variable */
    LispObject *enable_logging = env_lookup(lisp_env, "*enable-telnet-logging*");
    if (!enable_logging || enable_logging == NIL || !lisp_is_truthy(enable_logging)) {
        t->log_file = NULL;
        return 0; /* Logging disabled */
    }

    /* Expand log directory path (handles ~/...) using Lisp expand-path function */
    char expanded_dir[512];
    if (log_dir[0] == '~') {
        /* Call Lisp (expand-path "~/...") to ensure consistent expansion logic */
        char eval_buf[600];
        snprintf(eval_buf, sizeof(eval_buf), "(expand-path \"%s\")", log_dir);
        LispObject *expanded_obj = lisp_eval_string(eval_buf, lisp_env);

        if (expanded_obj && expanded_obj->type == LISP_STRING) {
            snprintf(expanded_dir, sizeof(expanded_dir), "%s", expanded_obj->value.string);
        } else {
            /* Fallback if expand-path fails */
            snprintf(expanded_dir, sizeof(expanded_dir), "%s", log_dir);
        }
    } else {
        snprintf(expanded_dir, sizeof(expanded_dir), "%s", log_dir);
    }

    /* Create log directory if it doesn't exist */
    file_mkdir(expanded_dir);

    /* Generate log filename: <timestamp>-telnet-session-<socket>.log */
    char timestamp[32];
    get_timestamp_iso(timestamp, sizeof(timestamp));

    /* For telnet connections, we need to get host/port from somewhere.
     * Since we don't have host/port stored yet, we'll just use socket number */
    snprintf(t->log_filename, sizeof(t->log_filename), "%s/%s-telnet-session-%d.log", expanded_dir, timestamp,
             t->socket);

    /* Open log file in append mode */
    t->log_file = file_open(t->log_filename, "a");
    if (!t->log_file) {
        fprintf(stderr, "Warning: Failed to open telnet log file: %s\n", t->log_filename);
        return -1;
    }

    /* Write session header */
    char header_time[32];
    get_timestamp_iso(header_time, sizeof(header_time));
    fprintf(t->log_file, "=== Telnet session started: %s (socket %d) ===\n", header_time, t->socket);
    fflush(t->log_file);

    return 0;
}

/* Close log file and write footer */
static void telnet_close_log(Telnet *t) {
    if (!t->log_file) {
        return;
    }

    /* Write session footer */
    char timestamp[32];
    get_timestamp_iso(timestamp, sizeof(timestamp));
    fprintf(t->log_file, "=== Telnet session ended: %s ===\n", timestamp);
    fflush(t->log_file);

    fclose(t->log_file);
    t->log_file = NULL;
}

/* Log raw telnet data (send or receive) */
static void telnet_log_data(Telnet *t, const char *direction, const unsigned char *data, size_t len) {
    if (!t->log_file || len == 0) {
        return;
    }

    /* Get timestamp for this log line */
    char timestamp[32];
    time_t now = time(NULL);
    struct tm *tm_info = localtime(&now);
    strftime(timestamp, sizeof(timestamp), "%Y-%m-%dT%H:%M:%S", tm_info);

    /* Write log line: [timestamp] DIRECTION: <data> */
    fprintf(t->log_file, "[%s] %s: ", timestamp, direction);

    /* Write data, escaping non-printable characters */
    for (size_t i = 0; i < len; i++) {
        unsigned char c = data[i];
        if (c >= 32 && c < 127) {
            fputc(c, t->log_file);
        } else if (c == '\n') {
            fprintf(t->log_file, "\\n");
        } else if (c == '\r') {
            fprintf(t->log_file, "\\r");
        } else if (c == '\t') {
            fprintf(t->log_file, "\\t");
        } else if (c == 255) {
            fprintf(t->log_file, "<IAC>");
        } else {
            fprintf(t->log_file, "\\x%02x", c);
        }
    }

    fprintf(t->log_file, "\n");
    fflush(t->log_file);
}

/* ===================================================================
 * END TELNET I/O LOGGING HELPER FUNCTIONS
 * =================================================================== */

Telnet *telnet_create(void) {
    Telnet *t = (Telnet *)malloc(sizeof(Telnet));
    if (!t)
        return NULL;

    t->socket = -1;
    t->state = TELNET_STATE_DISCONNECTED;
    t->rows = 40;
    t->cols = 80;

    /* Create reusable buffers for send operations */
    t->send_buffer = dynamic_buffer_create(4096);
    t->crlf_buffer = dynamic_buffer_create(4096);
    t->user_input_buffer = dynamic_buffer_create(8192); /* Enough for max input with LF->CRLF expansion */
    if (!t->send_buffer || !t->crlf_buffer || !t->user_input_buffer) {
        if (t->send_buffer)
            dynamic_buffer_destroy(t->send_buffer);
        if (t->crlf_buffer)
            dynamic_buffer_destroy(t->crlf_buffer);
        if (t->user_input_buffer)
            dynamic_buffer_destroy(t->user_input_buffer);
        free(t);
        return NULL;
    }

    return t;
}

int telnet_connect(Telnet *t, const char *hostname, int port) {
    if (!t || !hostname)
        return -1;

    t->state = TELNET_STATE_CONNECTING;

#ifdef _WIN32
    WSADATA wsa;
    if (WSAStartup(MAKEWORD(2, 2), &wsa) != 0) {
        return -1;
    }
#endif

    t->socket = socket(AF_INET, SOCK_STREAM, 0);
    if (t->socket < 0) {
        t->state = TELNET_STATE_DISCONNECTED;
        return -1;
    }

    struct sockaddr_in server;
    memset(&server, 0, sizeof(server));
    server.sin_family = AF_INET;
    server.sin_port = htons(port);

    if (inet_pton(AF_INET, hostname, &server.sin_addr) <= 0) {
        /* Try getaddrinfo for hostname resolution */
        struct addrinfo hints, *result;
        memset(&hints, 0, sizeof(hints));
        hints.ai_family = AF_INET;
        hints.ai_socktype = SOCK_STREAM;

        if (getaddrinfo(hostname, NULL, &hints, &result) == 0) {
            struct sockaddr_in *addr_in = (struct sockaddr_in *)result->ai_addr;
            server.sin_addr = addr_in->sin_addr;
            freeaddrinfo(result);
        } else {
            close(t->socket);
            t->socket = -1;
            t->state = TELNET_STATE_DISCONNECTED;
            return -1;
        }
    }

    if (connect(t->socket, (struct sockaddr *)&server, sizeof(server)) < 0) {
        close(t->socket);
        t->socket = -1;
        t->state = TELNET_STATE_DISCONNECTED;
        return -1;
    }

    /* Enable TCP keepalive if configured via Lisp variable */
    Environment *keepalive_env = (Environment *)lisp_x_get_environment();
    if (keepalive_env) {
        LispObject *enable_keepalive = env_lookup(keepalive_env, "*tcp-keepalive-enabled*");
        if (enable_keepalive && enable_keepalive != NIL && lisp_is_truthy(enable_keepalive)) {
            /* Get keepalive timing from Lisp variables (defaults: 60s idle, 10s interval) */
            int keepalive_time = 60;
            int keepalive_interval = 10;

            LispObject *time_obj = env_lookup(keepalive_env, "*tcp-keepalive-time*");
            if (time_obj && time_obj->type == LISP_INTEGER) {
                keepalive_time = (int)time_obj->value.integer;
            }

            LispObject *interval_obj = env_lookup(keepalive_env, "*tcp-keepalive-interval*");
            if (interval_obj && interval_obj->type == LISP_INTEGER) {
                keepalive_interval = (int)interval_obj->value.integer;
            }

#ifdef _WIN32
            /* Enable SO_KEEPALIVE first */
            BOOL optval = TRUE;
            if (setsockopt(t->socket, SOL_SOCKET, SO_KEEPALIVE, (char *)&optval, sizeof(optval)) < 0) {
                fprintf(stderr, "Warning: Failed to enable TCP keepalive (error %d)\n", WSAGetLastError());
            } else {
                /* Set keepalive timing via WSAIoctl */
                struct tcp_keepalive ka;
                ka.onoff = 1;
                ka.keepalivetime = keepalive_time * 1000;         /* Convert to milliseconds */
                ka.keepaliveinterval = keepalive_interval * 1000; /* Convert to milliseconds */

                DWORD bytes_returned;
                if (WSAIoctl(t->socket, SIO_KEEPALIVE_VALS, &ka, sizeof(ka), NULL, 0, &bytes_returned, NULL, NULL) !=
                    0) {
                    fprintf(stderr, "Warning: Failed to set TCP keepalive timing (error %d)\n", WSAGetLastError());
                }
            }
#else
            /* Enable SO_KEEPALIVE */
            int optval = 1;
            if (setsockopt(t->socket, SOL_SOCKET, SO_KEEPALIVE, &optval, sizeof(optval)) < 0) {
                fprintf(stderr, "Warning: Failed to enable TCP keepalive: %s\n", strerror(errno));
            } else {
                /* Set keepalive timing (Linux/BSD) */
#ifdef TCP_KEEPIDLE
                if (setsockopt(t->socket, IPPROTO_TCP, TCP_KEEPIDLE, &keepalive_time, sizeof(keepalive_time)) < 0) {
                    fprintf(stderr, "Warning: Failed to set TCP_KEEPIDLE: %s\n", strerror(errno));
                }
#endif
#ifdef TCP_KEEPINTVL
                if (setsockopt(t->socket, IPPROTO_TCP, TCP_KEEPINTVL, &keepalive_interval, sizeof(keepalive_interval)) <
                    0) {
                    fprintf(stderr, "Warning: Failed to set TCP_KEEPINTVL: %s\n", strerror(errno));
                }
#endif
            }
#endif
        }
    }

    /* Set socket to non-blocking mode */
#ifdef _WIN32
    u_long mode = 1;
    ioctlsocket(t->socket, FIONBIO, &mode);
#else
    int flags = fcntl(t->socket, F_GETFL, 0);
    fcntl(t->socket, F_SETFL, flags | O_NONBLOCK);
#endif

    /* Send initial Telnet options */
    telnet_send_command(t->socket, WONT, OPT_ECHO);
    telnet_send_command(t->socket, WILL, OPT_SUPPRESS_GO_AHEAD);
    telnet_send_command(t->socket, DO, OPT_ECHO);
    telnet_send_command(t->socket, DO, OPT_SUPPRESS_GO_AHEAD);
    telnet_send_command(t->socket, DO, OPT_TERMINAL_TYPE);
    telnet_send_command(t->socket, DO, OPT_NAWS);

    /* Open log file if logging is enabled */
    Environment *lisp_env = (Environment *)lisp_x_get_environment();
    const char *log_dir = "~/telnet-logs";
    if (lisp_env) {
        LispObject *log_dir_obj = env_lookup(lisp_env, "*telnet-log-directory*");
        if (log_dir_obj && log_dir_obj->type == LISP_STRING) {
            log_dir = log_dir_obj->value.string;
        }
    }
    telnet_open_log(t, log_dir);

    t->state = TELNET_STATE_CONNECTED;
    return 0;
}

void telnet_disconnect(Telnet *t) {
    if (!t || t->socket < 0)
        return;

    /* Close log file if open */
    telnet_close_log(t);

    close(t->socket);
    t->socket = -1;
    t->state = TELNET_STATE_DISCONNECTED;
}

int telnet_send(Telnet *t, const char *data, size_t len) {
    if (!t || t->socket < 0)
        return -1;

    /* Reuse send_buffer for IAC escaping */
    DynamicBuffer *buf = t->send_buffer;
    dynamic_buffer_clear(buf);

    /* IAC escaping - double any IAC bytes */
    for (size_t i = 0; i < len; i++) {
        if ((unsigned char)data[i] == IAC) {
            dynamic_buffer_append(buf, (const char[]){IAC, IAC}, 2);
        } else {
            dynamic_buffer_append(buf, &data[i], 1);
        }
    }

    int result = send(t->socket, dynamic_buffer_data(buf), dynamic_buffer_len(buf), 0);

    /* Log raw sent data */
    if (result > 0) {
        telnet_log_data(t, "SEND", (const unsigned char *)dynamic_buffer_data(buf), result);
    }

    return result;
}

int telnet_send_with_crlf(Telnet *t, const char *data, size_t len) {
    if (!t || t->socket < 0)
        return -1;

    /* Reuse crlf_buffer to append CRLF */
    DynamicBuffer *buf = t->crlf_buffer;
    dynamic_buffer_clear(buf);

    /* Append data and CRLF */
    dynamic_buffer_append(buf, data, len);
    dynamic_buffer_append(buf, "\r\n", 2);

    /* Send via telnet_send which handles IAC escaping */
    return telnet_send(t, dynamic_buffer_data(buf), dynamic_buffer_len(buf));
}

/* Get user input buffer for LF->CRLF conversion (used by main.c) */
DynamicBuffer *telnet_get_user_input_buffer(Telnet *t) {
    return t ? t->user_input_buffer : NULL;
}

int telnet_receive(Telnet *t, char *buffer, size_t bufsize) {
    if (!t || t->socket < 0)
        return -1;

    int received = recv(t->socket, buffer, bufsize, 0);

    /* Log raw received data */
    if (received > 0) {
        telnet_log_data(t, "RECV", (unsigned char *)buffer, received);
    }

    if (received < 0) {
        /* Error from recv() */
#ifdef _WIN32
        int error = WSAGetLastError();
        if (error == WSAEWOULDBLOCK || error == WSAEINPROGRESS || error == WSAEINTR) {
            /* Would block or interrupted - no data available, not an error */
            return 0;
        }
        fprintf(stderr, "telnet_receive: recv() returned -1, WSAGetLastError=%d\n", error);
#else
        if (errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR) {
            /* Would block or interrupted - no data available, not an error */
            return 0;
        }
        fprintf(stderr, "telnet_receive: recv() returned -1, errno=%d (%s)\n", errno, strerror(errno));
#endif
        /* Connection error */
        telnet_disconnect(t);
        return -1;
    } else if (received == 0) {
        /* recv() returning 0 means graceful close (peer sent FIN) */
        fprintf(stderr, "telnet_receive: recv() returned 0 (peer closed connection)\n");
        telnet_disconnect(t);
        return -1;
    }

    /* Parse Telnet commands */
    size_t pos = 0;
    TelnetDataState state = TELNET_DATA_NORMAL;

    for (int i = 0; i < received; i++) {
        unsigned char c = buffer[i];

        switch (state) {
        case TELNET_DATA_NORMAL:
            if (c == IAC) {
                state = TELNET_DATA_IAC;
            } else {
                buffer[pos++] = c;
            }
            break;

        case TELNET_DATA_IAC:
            switch (c) {
            case IAC:
                buffer[pos++] = IAC;
                state = TELNET_DATA_NORMAL;
                break;
            case WILL:
                state = TELNET_DATA_WILL;
                break;
            case WONT:
                state = TELNET_DATA_WONT;
                break;
            case DO:
                state = TELNET_DATA_DO;
                break;
            case DONT:
                state = TELNET_DATA_DONT;
                break;
            case SE:
            case NOP:
            default:
                state = TELNET_DATA_NORMAL;
                break;
            }
            break;

        case TELNET_DATA_WILL:
        case TELNET_DATA_WONT:
        case TELNET_DATA_DO:
        case TELNET_DATA_DONT:
            /* Ignore option for now */
            state = TELNET_DATA_NORMAL;
            break;
        }
    }

    buffer[pos] = '\0';
    return pos;
}

int telnet_get_socket(Telnet *t) {
    return t ? t->socket : -1;
}

TelnetState telnet_get_state(Telnet *t) {
    return t ? t->state : TELNET_STATE_DISCONNECTED;
}

void telnet_set_terminal_size(Telnet *t, int cols, int rows) {
    if (!t || t->socket < 0)
        return;

    t->cols = cols;
    t->rows = rows;

    /* Send NAWS (Negotiate About Window Size) */
    unsigned char naws[4];
    naws[0] = cols >> 8;
    naws[1] = cols & 0xFF;
    naws[2] = rows >> 8;
    naws[3] = rows & 0xFF;

    unsigned char buf[9];
    buf[0] = IAC;
    buf[1] = SB;
    buf[2] = OPT_NAWS;
    memcpy(&buf[3], naws, 4);
    buf[7] = IAC;
    buf[8] = SE;

    send(t->socket, (char *)buf, 9, 0);
}

void telnet_destroy(Telnet *t) {
    if (!t)
        return;
    telnet_disconnect(t);
    if (t->send_buffer)
        dynamic_buffer_destroy(t->send_buffer);
    if (t->crlf_buffer)
        dynamic_buffer_destroy(t->crlf_buffer);
    if (t->user_input_buffer)
        dynamic_buffer_destroy(t->user_input_buffer);
    free(t);
}
