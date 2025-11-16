/* Special slash commands implementation */

#include "commands.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Helper: Handle disconnection - update state, mode display, and show message */
static void handle_disconnection(int *connected_mode, InputArea *area, Terminal *term, const char *message) {
    *connected_mode = 0;
    input_area_update_mode(area, *connected_mode);
    terminal_feed_data(term, message, strlen(message));
}

/* Process special commands (starting with /) */
int process_command(const char *text, Telnet *telnet, Terminal *term, int *connected_mode, InputArea *area) {
    if (!text || text[0] != '/')
        return 0; /* Not a command */

    /* Skip the leading '/' */
    const char *cmd = text + 1;

    /* /help command */
    if (strcmp(cmd, "help") == 0) {
        const char *help_msg = "\r\n"
                               "Available commands:\r\n"
                               "  /help                         - Show this help message\r\n"
                               "  /connect <server> <port>      - Connect to a telnet server\r\n"
                               "  /connect <server>:<port>      - Connect to a telnet server\r\n"
                               "  /disconnect                   - Disconnect from current server\r\n"
                               "\r\n";
        terminal_feed_data(term, help_msg, strlen(help_msg));
        return 1; /* Command processed */
    }

    /* /disconnect command */
    if (strcmp(cmd, "disconnect") == 0) {
        if (*connected_mode) {
            telnet_disconnect(telnet);
            handle_disconnection(connected_mode, area, term, "\r\n*** Disconnected ***\r\n");
        } else {
            const char *msg = "\r\n*** Not connected ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
        }
        return 1; /* Command processed */
    }

    /* /connect command */
    if (strncmp(cmd, "connect ", 8) == 0) {
        const char *args = cmd + 8; /* Skip "connect " */

        /* Skip leading spaces */
        while (*args == ' ')
            args++;

        if (*args == '\0') {
            const char *msg = "\r\n*** Usage: /connect <server> <port> or /connect <server>:<port> ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
            return 1;
        }

        /* Parse hostname and port */
        char hostname[256] = {0};
        int port = 0;

        /* Check for <server>:<port> format */
        const char *colon = strchr(args, ':');
        if (colon) {
            /* Format: server:port */
            size_t hostname_len = colon - args;
            if (hostname_len >= sizeof(hostname)) {
                const char *msg = "\r\n*** Error: Hostname too long ***\r\n";
                terminal_feed_data(term, msg, strlen(msg));
                return 1;
            }
            memcpy(hostname, args, hostname_len);
            hostname[hostname_len] = '\0';
            port = atoi(colon + 1);
        } else {
            /* Format: server port */
            const char *space = strchr(args, ' ');
            if (!space) {
                const char *msg = "\r\n*** Usage: /connect <server> <port> or /connect <server>:<port> ***\r\n";
                terminal_feed_data(term, msg, strlen(msg));
                return 1;
            }
            size_t hostname_len = space - args;
            if (hostname_len >= sizeof(hostname)) {
                const char *msg = "\r\n*** Error: Hostname too long ***\r\n";
                terminal_feed_data(term, msg, strlen(msg));
                return 1;
            }
            memcpy(hostname, args, hostname_len);
            hostname[hostname_len] = '\0';

            /* Skip spaces before port */
            const char *port_str = space + 1;
            while (*port_str == ' ')
                port_str++;
            port = atoi(port_str);
        }

        /* Validate port */
        if (port <= 0 || port > 65535) {
            const char *msg = "\r\n*** Error: Invalid port number (must be 1-65535) ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
            return 1;
        }

        /* Disconnect if already connected */
        if (*connected_mode) {
            telnet_disconnect(telnet);
            *connected_mode = 0;
            input_area_update_mode(area, *connected_mode);
        }

        /* Attempt connection */
        char connecting_msg[512];
        snprintf(connecting_msg, sizeof(connecting_msg), "\r\n*** Connecting to %s:%d... ***\r\n", hostname, port);
        terminal_feed_data(term, connecting_msg, strlen(connecting_msg));

        if (telnet_connect(telnet, hostname, port) < 0) {
            char error_msg[512];
            snprintf(error_msg, sizeof(error_msg), "\r\n*** Failed to connect to %s:%d ***\r\n", hostname, port);
            terminal_feed_data(term, error_msg, strlen(error_msg));
        } else {
            *connected_mode = 1;
            input_area_update_mode(area, *connected_mode);
            const char *msg = "\r\n*** Connected ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));

            /* Send NAWS with current terminal size */
            int rows, cols;
            terminal_get_size(term, &rows, &cols);
            telnet_set_terminal_size(telnet, cols, rows);
        }
        return 1; /* Command processed */
    }

    /* Unknown command */
    char error_msg[512];
    snprintf(error_msg, sizeof(error_msg), "\r\n*** Unknown command: %s (type /help for available commands) ***\r\n",
             cmd);
    terminal_feed_data(term, error_msg, strlen(error_msg));
    return 1; /* Command processed (but invalid) */
}
