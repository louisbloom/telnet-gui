/* Special colon commands implementation */

#include "commands.h"
#include "lisp.h"
#include "../../telnet-lisp/include/lisp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Helper: Handle disconnection - update state and show message */
static void handle_disconnection(int *connected_mode, Terminal *term, const char *message) {
    *connected_mode = 0;
    terminal_feed_data(term, message, strlen(message));
}

/* Process special commands (starting with :) */
int process_command(const char *text, Telnet *telnet, Terminal *term, int *connected_mode, InputArea *area,
                    int *quit_requested) {
    (void)area; /* Unused parameter - reserved for future use */

    if (!text || text[0] != ':')
        return 0; /* Not a command */

    /* Skip the leading ':' */
    const char *cmd = text + 1;

    /* :help command */
    if (strcmp(cmd, "help") == 0) {
        const char *help_msg = "\r\n"
                               "Available commands:\r\n"
                               "  :help                         - Show this help message\r\n"
                               "  :connect <server> <port>      - Connect to a telnet server\r\n"
                               "  :connect <server>:<port>      - Connect to a telnet server\r\n"
                               "  :disconnect                   - Disconnect from current server\r\n"
                               "  :load <filepath>              - Load and execute a Lisp file\r\n"
                               "  :test <filepath>              - Run a Lisp test file\r\n"
                               "  :repl <code>                  - Evaluate Lisp code and show result\r\n"
                               "  :quit                         - Exit application\r\n"
                               "\r\n";
        terminal_feed_data(term, help_msg, strlen(help_msg));
        return 1; /* Command processed */
    }

    /* :disconnect command */
    if (strcmp(cmd, "disconnect") == 0) {
        if (*connected_mode) {
            telnet_disconnect(telnet);
            handle_disconnection(connected_mode, term, "\r\n*** Disconnected ***\r\n");
        } else {
            const char *msg = "\r\n*** Not connected ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
        }
        return 1; /* Command processed */
    }

    /* :quit command */
    if (strcmp(cmd, "quit") == 0) {
        const char *msg = "\r\n*** Exiting... ***\r\n";
        terminal_feed_data(term, msg, strlen(msg));
        *quit_requested = 1;
        return 1; /* Command processed */
    }

    /* :load command */
    if (strncmp(cmd, "load ", 5) == 0) {
        const char *filepath = cmd + 5; /* Skip "load " */

        /* Skip leading spaces */
        while (*filepath == ' ')
            filepath++;

        if (*filepath == '\0') {
            const char *msg = "\r\n*** Usage: :load <filepath> ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
            return 1;
        }

        /* Remove trailing whitespace */
        size_t len = strlen(filepath);
        while (len > 0 && (filepath[len - 1] == ' ' || filepath[len - 1] == '\t' || filepath[len - 1] == '\r' ||
                           filepath[len - 1] == '\n')) {
            len--;
        }

        if (len == 0) {
            const char *msg = "\r\n*** Error: Invalid filepath ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
            return 1;
        }

        /* Copy filepath (truncate if needed) */
        char load_filepath[512] = {0};
        if (len >= sizeof(load_filepath)) {
            const char *msg = "\r\n*** Error: Filepath too long ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
            return 1;
        }
        memcpy(load_filepath, filepath, len);
        load_filepath[len] = '\0';

        /* Show loading message */
        char loading_msg[1024];
        snprintf(loading_msg, sizeof(loading_msg), "\r\n*** Loading: %s ***\r\n", load_filepath);
        terminal_feed_data(term, loading_msg, strlen(loading_msg));

        /* Load and execute the file */
        int result = lisp_x_load_file(load_filepath);
        if (result < 0) {
            char error_msg[1024];
            snprintf(error_msg, sizeof(error_msg), "\r\n*** Failed to load: %s ***\r\n", load_filepath);
            terminal_feed_data(term, error_msg, strlen(error_msg));
        } else {
            const char *msg = "\r\n*** File loaded successfully ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
        }

        return 1; /* Command processed */
    }

    /* :repl <code> - Evaluate Lisp code and echo result */
    if (strncmp(cmd, "repl ", 5) == 0) {
        const char *code = cmd + 5;

        /* Skip leading spaces */
        while (*code == ' ')
            code++;

        if (*code == '\0') {
            const char *msg = "\r\n*** Usage: :repl <lisp-code> ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
            return 1;
        }

        /* Get the Lisp environment */
        Environment *env = (Environment *)lisp_x_get_environment();
        if (!env) {
            const char *msg = "\r\n*** Error: Lisp environment not initialized ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
            return 1;
        }

        /* Evaluate the code */
        LispObject *result = lisp_eval_string(code, env);

        /* Check for errors */
        if (!result) {
            const char *msg = "\r\n*** Error: Evaluation returned NULL ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
            return 1;
        }

        if (result->type == LISP_ERROR) {
            char *err_str = lisp_print(result);
            if (err_str) {
                /* Format error message */
                size_t err_len = strlen(err_str);
                size_t msg_size = err_len + 50;
                char *error_msg = malloc(msg_size);
                if (error_msg) {
                    snprintf(error_msg, msg_size, "\r\n*** Error: %s ***\r\n", err_str);
                    terminal_feed_data(term, error_msg, strlen(error_msg));
                    free(error_msg);
                }
            }
            return 1;
        }

        /* Echo result to terminal */
        char *result_str = lisp_print(result);
        if (result_str) {
            /* Format output message */
            size_t result_len = strlen(result_str);
            size_t msg_size = result_len + 10;
            char *output_msg = malloc(msg_size);
            if (output_msg) {
                snprintf(output_msg, msg_size, "\r\n> %s\r\n", result_str);
                terminal_feed_data(term, output_msg, strlen(output_msg));
                free(output_msg);
            }
        }

        return 1;
    }

    /* :connect command */
    if (strncmp(cmd, "connect ", 8) == 0) {
        const char *args = cmd + 8; /* Skip "connect " */

        /* Skip leading spaces */
        while (*args == ' ')
            args++;

        if (*args == '\0') {
            const char *msg = "\r\n*** Usage: :connect <server> <port> or :connect <server>:<port> ***\r\n";
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
                const char *msg = "\r\n*** Usage: :connect <server> <port> or :connect <server>:<port> ***\r\n";
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
            const char *msg = "\r\n*** Connected ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));

            /* Send NAWS with current terminal size */
            int rows, cols;
            terminal_get_size(term, &rows, &cols);
            telnet_set_terminal_size(telnet, cols, rows);
        }
        return 1; /* Command processed */
    }

    /* :test command */
    if (strncmp(cmd, "test ", 5) == 0) {
        const char *filepath = cmd + 5; /* Skip "test " */

        /* Skip leading spaces */
        while (*filepath == ' ')
            filepath++;

        if (*filepath == '\0') {
            const char *msg = "\r\n*** Usage: :test <filepath> ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
            return 1;
        }

        /* Remove trailing whitespace */
        size_t len = strlen(filepath);
        while (len > 0 && (filepath[len - 1] == ' ' || filepath[len - 1] == '\t' || filepath[len - 1] == '\r' ||
                           filepath[len - 1] == '\n')) {
            len--;
        }

        if (len == 0) {
            const char *msg = "\r\n*** Error: Invalid filepath ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
            return 1;
        }

        /* Copy filepath (truncate if needed) */
        char test_filepath[512] = {0};
        if (len >= sizeof(test_filepath)) {
            const char *msg = "\r\n*** Error: Filepath too long ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
            return 1;
        }
        memcpy(test_filepath, filepath, len);
        test_filepath[len] = '\0';

        /* Show loading message */
        char loading_msg[1024];
        snprintf(loading_msg, sizeof(loading_msg), "\r\n*** Running test: %s ***\r\n", test_filepath);
        terminal_feed_data(term, loading_msg, strlen(loading_msg));

        /* Load and run the test file */
        int result = lisp_x_load_file(test_filepath);
        if (result < 0) {
            char error_msg[1024];
            snprintf(error_msg, sizeof(error_msg), "\r\n*** Test failed: %s ***\r\n", test_filepath);
            terminal_feed_data(term, error_msg, strlen(error_msg));
        } else {
            const char *msg = "\r\n*** Test completed successfully ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
        }

        return 1; /* Command processed */
    }

    /* Unknown command */
    char error_msg[512];
    snprintf(error_msg, sizeof(error_msg), "\r\n*** Unknown command: %s (type :help for available commands) ***\r\n",
             cmd);
    terminal_feed_data(term, error_msg, strlen(error_msg));
    return 1; /* Command processed (but invalid) */
}
