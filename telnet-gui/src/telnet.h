/* Telnet protocol client (RFC 854) */

#ifndef TELNET_H
#define TELNET_H

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#include "dynamic_buffer.h"

typedef struct Telnet Telnet;

typedef enum {
    TELNET_STATE_DISCONNECTED,
    TELNET_STATE_CONNECTING,
    TELNET_STATE_CONNECTED,
} TelnetState;

/* Create a new Telnet client */
Telnet *telnet_create(void);

/* Connect to a server */
int telnet_connect(Telnet *t, const char *hostname, int port);

/* Disconnect from server */
void telnet_disconnect(Telnet *t);

/* Send data to server */
int telnet_send(Telnet *t, const char *data, size_t len);

/* Send data to server with CRLF appended (for line-based protocols) */
int telnet_send_with_crlf(Telnet *t, const char *data, size_t len);

/* Get user input buffer for LF->CRLF conversion */
DynamicBuffer *telnet_get_user_input_buffer(Telnet *t);

/* Receive data from server */
int telnet_receive(Telnet *t, char *buffer, size_t bufsize);

/* Get socket file descriptor for select/poll */
int telnet_get_socket(Telnet *t);

/* Get connection state */
TelnetState telnet_get_state(Telnet *t);

/* Set terminal size for NAWS option */
void telnet_set_terminal_size(Telnet *t, int cols, int rows);

/* Destroy telnet client */
void telnet_destroy(Telnet *t);

#endif /* TELNET_H */
