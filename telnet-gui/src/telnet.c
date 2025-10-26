/* Telnet protocol implementation (RFC 854) */

#include "telnet.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#ifdef _WIN32
#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#define close(s) closesocket(s)
#define sleep(s) Sleep(s * 1000)
#else
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
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
};

/* Send Telnet command */
static int telnet_send_command(int socket, int cmd, int opt) {
    unsigned char buf[3];
    buf[0] = IAC;
    buf[1] = cmd;
    buf[2] = opt;
    return send(socket, (char *)buf, 3, 0);
}

Telnet *telnet_create(void) {
    Telnet *t = (Telnet *)malloc(sizeof(Telnet));
    if (!t)
        return NULL;

    t->socket = -1;
    t->state = TELNET_STATE_DISCONNECTED;
    t->rows = 24;
    t->cols = 80;

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

    /* Send initial Telnet options */
    telnet_send_command(t->socket, WILL, OPT_ECHO);
    telnet_send_command(t->socket, WILL, OPT_SUPPRESS_GO_AHEAD);
    telnet_send_command(t->socket, DO, OPT_ECHO);
    telnet_send_command(t->socket, DO, OPT_SUPPRESS_GO_AHEAD);
    telnet_send_command(t->socket, DO, OPT_TERMINAL_TYPE);
    telnet_send_command(t->socket, DO, OPT_NAWS);

    t->state = TELNET_STATE_CONNECTED;
    return 0;
}

void telnet_disconnect(Telnet *t) {
    if (!t || t->socket < 0)
        return;
    close(t->socket);
    t->socket = -1;
    t->state = TELNET_STATE_DISCONNECTED;
}

int telnet_send(Telnet *t, const char *data, size_t len) {
    if (!t || t->socket < 0)
        return -1;

    /* IAC escaping */
    size_t send_len = 0;
    char *buf = malloc(len * 2);
    if (!buf)
        return -1;

    for (size_t i = 0; i < len; i++) {
        if (data[i] == IAC) {
            buf[send_len++] = IAC;
            buf[send_len++] = IAC;
        } else {
            buf[send_len++] = data[i];
        }
    }

    int result = send(t->socket, buf, send_len, 0);
    free(buf);
    return result;
}

int telnet_receive(Telnet *t, char *buffer, size_t bufsize) {
    if (!t || t->socket < 0)
        return -1;

    int received = recv(t->socket, buffer, bufsize, 0);
    if (received <= 0) {
        telnet_disconnect(t);
        return received;
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

    unsigned char buf[8];
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
    free(t);
}
