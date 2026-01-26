/* Main entry point for Telnet GUI */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <stdbool.h>

#ifdef _WIN32
#include <winsock2.h>
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#else
#include <sys/select.h>
#include <sys/time.h>
#include <errno.h>
#endif

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

#include "telnet.h"
#include "terminal.h"
#include "window.h"
#include "renderer.h"
#include "glyph_cache.h"
#ifdef _WIN32
#include "glyph_cache_directwrite.h"
#endif
#include "input.h"
#include "dock.h"
#include "commands.h"
#include "lisp.h"
#include "ansi_sequences.h"
#include "dynamic_buffer.h"
#include "path_utils.h"
#if HAVE_RLOTTIE
#include "animation.h"
/* Forward declaration for renderer animation functions */
void renderer_set_animation(Animation *anim);
#endif
#include "../telnet-lisp/include/file_utils.h"
#include "vendor/argparse.h"
#include "display_fd.h"

/* High-resolution timing for profiling */
#ifdef _WIN32
static uint64_t get_time_ns(void) {
    static LARGE_INTEGER freq = {0};
    if (freq.QuadPart == 0) {
        QueryPerformanceFrequency(&freq);
    }
    LARGE_INTEGER count;
    QueryPerformanceCounter(&count);
    return (uint64_t)((count.QuadPart * 1000000000ULL) / freq.QuadPart);
}
#else
#include <time.h>
static uint64_t get_time_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}
#endif

/* Profiling statistics for C pipeline */
typedef struct {
    uint64_t telnet_receive_ns;
    uint64_t telnet_input_hook_ns;
    uint64_t telnet_input_filter_hook_ns;
    uint64_t terminal_feed_data_ns;
    int recv_count;
} ProfileStats;

static ProfileStats profile_stats = {0};

static void profile_stats_reset(void) {
    memset(&profile_stats, 0, sizeof(profile_stats));
}

static void profile_stats_print(void) {
    if (profile_stats.recv_count == 0) {
        printf("\n=== C Pipeline Timing (no RECV blocks) ===\n");
        return;
    }
    printf("\n=== C Pipeline Timing (%d RECV blocks) ===\n", profile_stats.recv_count);
    printf("telnet_receive:            avg %.3fms  total %.3fms\n",
           (double)profile_stats.telnet_receive_ns / profile_stats.recv_count / 1e6,
           (double)profile_stats.telnet_receive_ns / 1e6);
    printf("telnet-input-hook:         avg %.3fms  total %.3fms\n",
           (double)profile_stats.telnet_input_hook_ns / profile_stats.recv_count / 1e6,
           (double)profile_stats.telnet_input_hook_ns / 1e6);
    printf("telnet-input-filter-hook:  avg %.3fms  total %.3fms\n",
           (double)profile_stats.telnet_input_filter_hook_ns / profile_stats.recv_count / 1e6,
           (double)profile_stats.telnet_input_filter_hook_ns / 1e6);
    printf("terminal_feed_data:        avg %.3fms  total %.3fms\n",
           (double)profile_stats.terminal_feed_data_ns / profile_stats.recv_count / 1e6,
           (double)profile_stats.terminal_feed_data_ns / 1e6);
    uint64_t total_ns = profile_stats.telnet_receive_ns + profile_stats.telnet_input_hook_ns +
                        profile_stats.telnet_input_filter_hook_ns + profile_stats.terminal_feed_data_ns;
    printf("TOTAL:                     avg %.3fms  total %.3fms\n", (double)total_ns / profile_stats.recv_count / 1e6,
           (double)total_ns / 1e6);
}

/* Calculate timeout for select() based on animation state and timers */
static int calculate_timeout_ms(int animation_playing) {
    if (animation_playing)
        return 16; /* ~60 FPS for animations */

    int next_timer_ms = lisp_x_get_next_timer_timeout_ms();
    if (next_timer_ms < 0)
        return -1; /* Block indefinitely */
    if (next_timer_ms < 1)
        return 1; /* Minimum 1ms */
    return next_timer_ms;
}

/* Padding around terminal area (including input area) - must match renderer.c */
#define PADDING_X 8
#define PADDING_Y 8

static int running = 1;

/* Input area */
static Dock dock;

/* Terminal selection state */
static struct {
    int active;                /* Selection is active */
    int start_row;             /* Start row (viewport coordinate at time of selection) */
    int start_col;             /* Start column */
    int start_viewport_offset; /* Viewport offset when selection started */
    int start_scrollback_size; /* Scrollback size when selection started */
    int end_row;               /* End row (viewport coordinate at time of selection) */
    int end_col;               /* End column */
    int end_viewport_offset;   /* Viewport offset when selection ended */
    int end_scrollback_size;   /* Scrollback size when selection ended */
} terminal_selection = {0, 0, 0, 0, 0, 0, 0, 0, 0};

/* Global quit flag for :quit command */
static int quit_requested = 0;

/* Track previous input area height for resize detection */
static int prev_input_visible_rows = 1;

/* Command-line argument variables */
static const char *hostname = NULL;
static int port = 0;
static const char *lisp_files[16];
static int lisp_file_count = 0;
static const char *lisp_file_value = NULL;
static const char *font_value = NULL;
static const char *hinting_value = NULL;
static const char *antialiasing_value = NULL;
static const char *geometry_value = NULL;
static const char *line_height_value = NULL;
#ifdef _WIN32
static const char *font_backend_value = NULL;
#endif
static char font_choice =
    's';                   /* Internal font code: s=system, m=cascadia, i=inconsolata, p=plex, d=dejavu, c=courier */
static int font_size = 12; /* Default font size */
static int terminal_cols = 80;       /* Default terminal columns */
static int terminal_rows = 40;       /* Default terminal rows */
static int debug_exit = 0;           /* Exit after initialization for debug output */
static int profile_mode = 0;         /* Enable profiling (Lisp + C timing) */
static int exit_on_disconnect = 0;   /* Exit when telnet connection closes */
static float cli_line_height = 0.0f; /* CLI line height (0.0 means not set, use default) */
#ifdef _WIN32
static int use_directwrite = 1; /* Use DirectWrite font backend (Windows default) */
static int use_cleartype = 0;   /* ClearType disabled by default (enable with --cleartype) */
#else
static int use_directwrite = 0; /* DirectWrite not available on non-Windows */
static int use_cleartype = 0;   /* ClearType not available on non-Windows */
#endif
static int hinting_mode = TTF_HINTING_NONE;             /* Default: no hinting for crisp rendering */
static SDL_ScaleMode scale_mode = SDL_ScaleModeNearest; /* Default: nearest (pixel-perfect) scaling */

/* Callback for hinting mode option */
static int hinting_cb(struct argparse *self, const struct argparse_option *option) {
    (void)option; // Mark parameter as unused
    const char *value = self->optvalue;
    /* If self->optvalue is NULL, try to get value from hinting_value variable */
    if (!value) {
        value = hinting_value;
    }
    if (!value) {
        fprintf(stderr, "Error: No value provided for hinting option\n");
        return -1;
    }
    if (strcmp(value, "none") == 0) {
        hinting_mode = TTF_HINTING_NONE;
    } else if (strcmp(value, "light") == 0) {
        hinting_mode = TTF_HINTING_LIGHT;
    } else if (strcmp(value, "normal") == 0) {
        hinting_mode = TTF_HINTING_NORMAL;
    } else if (strcmp(value, "mono") == 0) {
        hinting_mode = TTF_HINTING_MONO;
    }
    return 0;
}

/* Callback for antialiasing mode option */
static int antialiasing_cb(struct argparse *self, const struct argparse_option *option) {
    (void)option; // Mark parameter as unused
    const char *value = self->optvalue;
    /* If self->optvalue is NULL, try to get value from antialiasing_value variable */
    if (!value) {
        value = antialiasing_value;
    }
    if (!value) {
        fprintf(stderr, "Error: No value provided for antialiasing option\n");
        return -1;
    }
    if (strcmp(value, "nearest") == 0) {
        scale_mode = SDL_ScaleModeNearest;
    } else if (strcmp(value, "linear") == 0) {
        scale_mode = SDL_ScaleModeLinear;
    }
    return 0;
}

/* Callback for font choice option */
static int font_cb(struct argparse *self, const struct argparse_option *option) {
    (void)option; // Mark parameter as unused
    const char *value = self->optvalue;
    /* If self->optvalue is NULL, try to get value from font_value variable */
    if (!value) {
        value = font_value;
    }
    if (!value) {
        fprintf(stderr, "Error: No value provided for font option\n");
        return -1;
    }
    if (strcmp(value, "system") == 0)
        font_choice = 's';
    else if (strcmp(value, "cascadia") == 0)
        font_choice = 'm';
    else if (strcmp(value, "inconsolata") == 0)
        font_choice = 'i';
    else if (strcmp(value, "plex") == 0)
        font_choice = 'p';
    else if (strcmp(value, "dejavu") == 0)
        font_choice = 'd';
    else if (strcmp(value, "courier") == 0)
        font_choice = 'c';
    return 0;
}

/* Callback for geometry option */
static int geometry_cb(struct argparse *self, const struct argparse_option *option) {
    (void)option; // Mark parameter as unused
    const char *value = self->optvalue;
    /* If self->optvalue is NULL, try to get value from geometry_value variable */
    if (!value) {
        value = geometry_value;
    }
    if (!value) {
        fprintf(stderr, "Error: No value provided for geometry option\n");
        return -1;
    }
    char *geom_copy = strdup(value);
    if (!geom_copy) {
        fprintf(stderr, "Error: Out of memory\n");
        return -1;
    }
    char *x_pos = strchr(geom_copy, 'x');
    if (!x_pos) {
        fprintf(stderr, "Error: Invalid geometry format '%s'. Use COLSxROWS (e.g., 80x40)\n", value);
        free(geom_copy);
        return -1;
    }
    *x_pos = '\0';
    terminal_cols = atoi(geom_copy);
    terminal_rows = atoi(x_pos + 1);
    free(geom_copy);

    if (terminal_cols <= 0 || terminal_rows <= 0) {
        fprintf(stderr, "Error: Invalid geometry dimensions. Columns and rows must be positive\n");
        return -1;
    }
    return 0;
}

/* Callback for line-height option */
static int line_height_cb(struct argparse *self, const struct argparse_option *option) {
    (void)option; // Mark parameter as unused
    const char *value = self->optvalue;
    /* If self->optvalue is NULL, try to get value from line_height_value variable */
    if (!value) {
        value = line_height_value;
    }
    if (!value) {
        fprintf(stderr, "Error: No value provided for line-height option\n");
        return -1;
    }
    cli_line_height = (float)atof(value);
    if (cli_line_height < 0.5f || cli_line_height > 3.0f) {
        fprintf(stderr, "Error: Invalid line height '%s'. Must be between 0.5 and 3.0\n", value);
        return -1;
    }
    return 0;
}

/* Callback for lisp-file option */
static int lisp_file_cb(struct argparse *self, const struct argparse_option *option) {
    (void)option; // Mark parameter as unused
    const char *value = self->optvalue;
    /* If self->optvalue is NULL, try to get value from lisp_file_value variable */
    if (!value) {
        value = lisp_file_value;
    }
    if (!value) {
        fprintf(stderr, "Error: No value provided for lisp-file option\n");
        return -1;
    }
    if (lisp_file_count < 16) {
        lisp_files[lisp_file_count++] = value;
    } else {
        fprintf(stderr, "Error: Too many -l flags (maximum 16)\n");
        return -1;
    }
    return 0;
}

#ifdef _WIN32
/* Callback for font-backend option */
static int font_backend_cb(struct argparse *self, const struct argparse_option *option) {
    (void)option; // Mark parameter as unused
    const char *value = self->optvalue;
    /* If self->optvalue is NULL, try to get value from font_backend_value variable */
    if (!value) {
        value = font_backend_value;
    }
    if (!value) {
        fprintf(stderr, "Error: No value provided for font-backend option\n");
        return -1;
    }
    if (strcmp(value, "sdl") == 0 || strcmp(value, "sdl_ttf") == 0) {
        use_directwrite = 0;
    } else if (strcmp(value, "directwrite") == 0) {
        use_directwrite = 1;
    }
    return 0;
}
#endif

/* Define argparse options */
static struct argparse_option options[] = {
    OPT_HELP(),
    OPT_GROUP("Font Options:"),
    OPT_INTEGER('s', "font-size", &font_size, "Set font size in points (default: 12)", NULL, 0, 0),
    OPT_STRING('f', "font", &font_value,
               "Select font (default: system): system, cascadia, inconsolata, plex, dejavu, courier", font_cb, 0, 0),
    OPT_STRING(0, "hinting", &hinting_value, "Set font hinting mode (default: none): none, light, normal, mono",
               hinting_cb, 0, 0),
    OPT_STRING(0, "antialiasing", &antialiasing_value, "Set anti-aliasing mode (default: nearest): nearest, linear",
               antialiasing_cb, 0, 0),
#ifdef _WIN32
    OPT_STRING(0, "font-backend", &font_backend_value,
               "Font rendering backend (default: directwrite): sdl, directwrite", font_backend_cb, 0, 0),
    OPT_BOOLEAN(0, "cleartype", &use_cleartype, "Enable ClearType subpixel rendering (default: off)", NULL, 0, 0),
    OPT_BOOLEAN(0, "no-cleartype", NULL, "Disable ClearType subpixel rendering", NULL, 0, OPT_NONEG),
#endif
    OPT_GROUP("Terminal Options:"),
    OPT_STRING('g', "geometry", &geometry_value, "Set terminal size in characters: COLSxROWS (e.g., 80x40)",
               geometry_cb, 0, 0),
    OPT_GROUP("Other Options:"),
    OPT_STRING('l', "lisp-file", &lisp_file_value,
               "Load and evaluate Lisp file on startup (can be specified multiple times)", lisp_file_cb, 0, 0),
    OPT_STRING(0, "line-height", &line_height_value, "Set line height multiplier (default: 1.0): 0.5 to 3.0",
               line_height_cb, 0, 0),
    OPT_BOOLEAN(0, "debug-exit", &debug_exit, "Exit after initialization (for debug output)", NULL, 0, 0),
    OPT_BOOLEAN(0, "profile", &profile_mode, "Enable Lisp profiler and C timing instrumentation", NULL, 0, 0),
    OPT_BOOLEAN(0, "exit-on-disconnect", &exit_on_disconnect, "Exit when telnet connection closes", NULL, 0, 0),
    OPT_END(),
};

/* Usage strings */
static const char *const usages[] = {
    "telnet-gui [OPTIONS] [hostname] [port]",
    NULL,
};

/* Clear terminal selection */
static void clear_terminal_selection(Terminal *term) {
    terminal_selection.active = 0;
    terminal_selection.start_row = 0;
    terminal_selection.start_col = 0;
    terminal_selection.start_viewport_offset = 0;
    terminal_selection.start_scrollback_size = 0;
    terminal_selection.end_row = 0;
    terminal_selection.end_col = 0;
    terminal_selection.end_viewport_offset = 0;
    terminal_selection.end_scrollback_size = 0;
    /* Request redraw to remove selection highlight from screen */
    terminal_request_redraw(term);
}

/* Start terminal selection at given viewport position */
static void start_terminal_selection(Terminal *term, int viewport_row, int col) {
    terminal_selection.active = 1;
    int viewport_offset = terminal_get_viewport_offset(term);
    int scrollback_size = terminal_get_scrollback_size(term);
    terminal_selection.start_row = viewport_row;
    terminal_selection.start_col = col;
    terminal_selection.start_viewport_offset = viewport_offset;
    terminal_selection.start_scrollback_size = scrollback_size;
    terminal_selection.end_row = viewport_row;
    terminal_selection.end_col = col;
    terminal_selection.end_viewport_offset = viewport_offset;
    terminal_selection.end_scrollback_size = scrollback_size;
}

/* Update terminal selection end position */
static void update_terminal_selection(Terminal *term, int viewport_row, int col) {
    if (terminal_selection.active) {
        int viewport_offset = terminal_get_viewport_offset(term);
        int scrollback_size = terminal_get_scrollback_size(term);
        terminal_selection.end_row = viewport_row;
        terminal_selection.end_col = col;
        terminal_selection.end_viewport_offset = viewport_offset;
        terminal_selection.end_scrollback_size = scrollback_size;
    }
}

/* Extract selected text from terminal and copy to clipboard */
static void copy_terminal_selection(Terminal *term) {
    if (!terminal_selection.active)
        return;

    /* Calculate the absolute scrollback index where the selection is located */
    int start_scrollback_index = terminal_selection.start_scrollback_size - terminal_selection.start_viewport_offset +
                                 terminal_selection.start_row;
    int end_scrollback_index =
        terminal_selection.end_scrollback_size - terminal_selection.end_viewport_offset + terminal_selection.end_row;

    int start_col = terminal_selection.start_col;
    int end_col = terminal_selection.end_col;

    /* Normalize selection (ensure start < end) using absolute indices */
    if (start_scrollback_index > end_scrollback_index ||
        (start_scrollback_index == end_scrollback_index && start_col > end_col)) {
        /* Swap start and end */
        int tmp_index = start_scrollback_index;
        int tmp_col = start_col;
        start_scrollback_index = end_scrollback_index;
        start_col = end_col;
        end_scrollback_index = tmp_index;
        end_col = tmp_col;
    }

    /* Allocate buffer for selected text (rough estimate: 4 bytes per cell for UTF-8) */
    int rows, cols;
    terminal_get_size(term, &rows, &cols);
    int estimated_size = (end_scrollback_index - start_scrollback_index + 1) * cols * 4 +
                         (end_scrollback_index - start_scrollback_index + 1) + 1;
    char *buffer = (char *)malloc(estimated_size);
    if (!buffer)
        return;

    int buf_pos = 0;

    /* Extract text row by row using absolute scrollback indices */
    for (int idx = start_scrollback_index; idx <= end_scrollback_index; idx++) {
        int col_start = (idx == start_scrollback_index) ? start_col : 0;
        int col_end = (idx == end_scrollback_index) ? end_col : cols - 1;

        /* Get text from this row */
        for (int col = col_start; col <= col_end; col++) {
            TermCell cell;
            if (terminal_get_cell_at_scrollback_index(term, idx, col, &cell)) {
                /* Skip continuation cells (right half of wide characters like emoji) */
                /* VTerm marks these with width=0 or chars[0]=0xFFFFFFFF (>0x10FFFF) */
                if (cell.width == 0 || cell.chars[0] > 0x10FFFF)
                    continue;

                /* Convert all cell codepoints to UTF-8 (up to 6 for combining chars) */
                for (int ci = 0; ci < 6 && cell.chars[ci]; ci++) {
                    char utf8[5];
                    int len = 0;
                    uint32_t codepoint = cell.chars[ci];

                    /* Encode UTF-8 */
                    if (codepoint < 0x80) {
                        utf8[len++] = (char)codepoint;
                    } else if (codepoint < 0x800) {
                        utf8[len++] = (char)(0xC0 | (codepoint >> 6));
                        utf8[len++] = (char)(0x80 | (codepoint & 0x3F));
                    } else if (codepoint < 0x10000) {
                        utf8[len++] = (char)(0xE0 | (codepoint >> 12));
                        utf8[len++] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
                        utf8[len++] = (char)(0x80 | (codepoint & 0x3F));
                    } else {
                        utf8[len++] = (char)(0xF0 | (codepoint >> 18));
                        utf8[len++] = (char)(0x80 | ((codepoint >> 12) & 0x3F));
                        utf8[len++] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
                        utf8[len++] = (char)(0x80 | (codepoint & 0x3F));
                    }

                    /* Copy to buffer */
                    for (int i = 0; i < len && buf_pos < estimated_size - 1; i++) {
                        buffer[buf_pos++] = utf8[i];
                    }
                }
            }
        }

        /* Add newline after each row except the last */
        if (idx < end_scrollback_index && buf_pos < estimated_size - 1) {
            buffer[buf_pos++] = '\n';
        }
    }

    /* Null-terminate */
    buffer[buf_pos] = '\0';

    /* Copy to clipboard */
    SDL_SetClipboardText(buffer);

    /* Free buffer */
    free(buffer);
}

static void cleanup(void) {
    lisp_x_cleanup();
    SDL_Quit();
}

/* Unified function to send data to telnet with error handling
 * Handles LF->CRLF conversion, CRLF appending, connection failure detection,
 * state updates, and UI redraws
 * Returns 0 on success, -1 on failure
 */
static int send_to_telnet(Telnet *telnet, Terminal *term, Dock *dock, int *connected_mode, const char *data, size_t len,
                          int append_crlf) {
    if (!telnet || !term || !dock || !connected_mode) {
        return -1;
    }

    DynamicBuffer *send_buffer = telnet_get_user_input_buffer(telnet);
    if (!send_buffer) {
        return -1;
    }

    if (!*connected_mode) {
        /* Not connected - echo message to terminal */
        const char *not_conn = "\r\n*** Not connected ***\r\n";
        terminal_feed_data(term, not_conn, strlen(not_conn));
        return -1;
    }

    /* Clear any accidentally buffered output before sending */
    terminal_clear_output_buffer(term);

    /* Clear and reuse send buffer */
    dynamic_buffer_clear(send_buffer);

    /* Convert LF to CRLF in the data and append to buffer */
    for (size_t i = 0; i < len; i++) {
        char c = data[i];
        if (c == '\n') {
            if (dynamic_buffer_append(send_buffer, "\r\n", 2) < 0) {
                return -1; /* Buffer allocation failed */
            }
        } else {
            if (dynamic_buffer_append(send_buffer, &c, 1) < 0) {
                return -1; /* Buffer allocation failed */
            }
        }
    }

    /* Append CRLF at end if requested */
    if (append_crlf) {
        if (dynamic_buffer_append(send_buffer, "\r\n", 2) < 0) {
            return -1; /* Buffer allocation failed */
        }
    }

    int sent = telnet_send(telnet, dynamic_buffer_data(send_buffer), dynamic_buffer_len(send_buffer));
    if (sent < 0) {
        fprintf(stderr, "Failed to send data via telnet\n");
        /* Connection lost - update telnet state and switch to unconnected mode */
        telnet_disconnect(telnet); /* Update state (cleanup function) */
        *connected_mode = 0;
        const char *msg = "\r\n*** Connection lost ***\r\n";
        terminal_feed_data(term, msg, strlen(msg));
        dock_request_redraw(dock); /* Trigger color update */
        return -1;
    }

    return 0;
}

/* Calculate terminal size (rows, cols) based on window dimensions */
static void calculate_terminal_size(int window_width, int window_height, int cell_w, int cell_h, Dock *dock, int *rows,
                                    int *cols) {
    /* Subtract padding from window dimensions before calculating terminal size */
    int available_width = window_width - 2 * PADDING_X;
    int available_height = window_height - 2 * PADDING_Y;

    /* Get line height multiplier and calculate effective cell height */
    float line_height = lisp_x_get_terminal_line_height();
    int effective_cell_h = (int)(cell_h * line_height);

    /* Calculate columns from available width */
    *cols = available_width / cell_w;
    if (*cols < 10)
        *cols = 10; /* Minimum width */

    /* Calculate number of rows that fit in available height */
    /* Subtract rows for dock area */
    int input_height_rows = dock_height_rows(dock_get_text_rows(dock));
    *rows = (available_height / effective_cell_h) - input_height_rows;
    if (*rows < 1)
        *rows = 1; /* Minimum: 1 scrolling row */
}

/* Find the best system monospace font for the current platform */
static const char *find_system_monospace_font(const char **font_name_out) {
#ifdef _WIN32
    /* Windows: Try Consolas first (best monospace), then Courier New */
    const char *fonts[] = {"C:/Windows/Fonts/consola.ttf",    /* Consolas */
                           "C:\\Windows\\Fonts\\consola.ttf", /* Consolas (backslash) */
                           "C:/Windows/Fonts/cour.ttf",       /* Courier New */
                           "C:\\Windows\\Fonts\\cour.ttf",    /* Courier New (backslash) */
                           NULL};
    const char *names[] = {"Consolas", "Consolas", "Courier New", "Courier New", NULL};

    for (int i = 0; fonts[i] != NULL; i++) {
        FILE *test = file_open(fonts[i], "rb");
        if (test) {
            fclose(test);
            if (font_name_out) {
                *font_name_out = names[i];
            }
            return fonts[i];
        }
    }
#elif defined(__APPLE__)
    /* macOS: Try Menlo first, then Monaco, then Courier New */
    const char *fonts[] = {"/Library/Fonts/Menlo.ttc",
                           "/System/Library/Fonts/Menlo.ttc",
                           "/Library/Fonts/Monaco.dfont",
                           "/System/Library/Fonts/Monaco.dfont",
                           "/Library/Fonts/Courier New.ttf",
                           "/System/Library/Fonts/Courier New.ttf",
                           NULL};
    const char *names[] = {"Menlo", "Menlo", "Monaco", "Monaco", "Courier New", "Courier New", NULL};

    for (int i = 0; fonts[i] != NULL; i++) {
        FILE *test = file_open(fonts[i], "rb");
        if (test) {
            fclose(test);
            if (font_name_out) {
                *font_name_out = names[i];
            }
            return fonts[i];
        }
    }
#else
    /* First, try to use fc-match to find a monospace font */
    {
        /* Try multiple patterns to find a monospace font */
        const char *patterns[] = {"monospace:style=Regular",
                                  "monospace",
                                  "DejaVu Sans Mono:style=Book",
                                  "Liberation Mono:style=Regular",
                                  "Source Code Pro:style=Regular",
                                  "Noto Sans Mono:style=Regular",
                                  NULL};

        for (int p = 0; patterns[p] != NULL; p++) {
            char command[256];
            snprintf(command, sizeof(command), "fc-match -f '%%{file}\n' '%s' 2>/dev/null", patterns[p]);
            FILE *fp = popen(command, "r");
            if (fp) {
                static char path[1024];
                if (fgets(path, sizeof(path), fp)) {
                    /* Remove trailing newline */
                    size_t len = strlen(path);
                    if (len > 0 && path[len - 1] == '\n') {
                        path[len - 1] = '\0';
                    }
                    pclose(fp);
                    /* Check if the file exists */
                    FILE *test = file_open(path, "rb");
                    if (test) {
                        fclose(test);
                        /* Try to get font name using fc-match */
                        char name_command[256];
                        snprintf(name_command, sizeof(name_command), "fc-match -f '%%{family}\n' '%s' 2>/dev/null",
                                 patterns[p]);
                        FILE *fp2 = popen(name_command, "r");
                        if (fp2) {
                            static char name[256];
                            if (fgets(name, sizeof(name), fp2)) {
                                len = strlen(name);
                                if (len > 0 && name[len - 1] == '\n') {
                                    name[len - 1] = '\0';
                                }
                                if (font_name_out) {
                                    *font_name_out = name;
                                }
                            }
                            pclose(fp2);
                        }
                        return path;
                    }
                }
                pclose(fp);
            }
        }
    }

    /* Fallback to hardcoded paths if fc-match fails */
    const char *fonts[] = {
        /* Fedora-specific paths */
        "/usr/share/fonts/adobe-source-code-pro-fonts/SourceCodePro-Regular.otf",
        "/usr/share/fonts/liberation-mono-fonts/LiberationMono-Regular.ttf",
        "/usr/share/fonts/adwaita-mono-fonts/AdwaitaMono-Regular.ttf",
        "/usr/share/fonts/google-noto-vf/NotoSansMono[wght].ttf",
        "/usr/share/fonts/google-noto-sans-mono-cjk-vf-fonts/NotoSansMonoCJK-VF.ttc",
        /* DejaVu fonts */
        "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", "/usr/share/fonts/TTF/DejaVuSansMono.ttf",
        "/usr/share/fonts/dejavu-sans-mono/DejaVuSansMono.ttf",
        /* Liberation fonts */
        "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf",
        "/usr/share/fonts/TTF/LiberationMono-Regular.ttf",
        "/usr/share/fonts/liberation-mono/LiberationMono-Regular.ttf",
        /* Noto fonts */
        "/usr/share/fonts/truetype/noto/NotoMono-Regular.ttf", "/usr/share/fonts/noto-mono/NotoMono-Regular.ttf",
        "/usr/share/fonts/TTF/NotoMono-Regular.ttf",
        /* Ubuntu fonts */
        "/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf", "/usr/share/fonts/ubuntu-mono/UbuntuMono-R.ttf",
        /* Courier fonts */
        "/usr/share/fonts/truetype/courier/Courier New.ttf", "/usr/share/fonts/TTF/Courier New.ttf",
        "/usr/share/fonts/courier-new/Courier New.ttf",
        /* Nimbus Mono */
        "/usr/share/fonts/truetype/nimbus-mono/NimbusMono-Regular.ttf",
        "/usr/share/fonts/urw-base35/NimbusMonoPS-Regular.otf",
        /* FreeMono */
        "/usr/share/fonts/truetype/freefont/FreeMono.ttf", "/usr/share/fonts/freefont/FreeMono.ttf",
        /* Last resort: check in X11 fonts */
        "/usr/share/fonts/X11/misc/6x13.pcf.gz", "/usr/share/fonts/X11/misc/fixed.pcf.gz", NULL};
    const char *names[] = {"Source Code Pro",
                           "Liberation Mono",
                           "Adwaita Mono",
                           "Noto Sans Mono",
                           "Noto Sans Mono CJK",
                           "DejaVu Sans Mono",
                           "DejaVu Sans Mono",
                           "DejaVu Sans Mono",
                           "Liberation Mono",
                           "Liberation Mono",
                           "Liberation Mono",
                           "Noto Mono",
                           "Noto Mono",
                           "Noto Mono",
                           "Ubuntu Mono",
                           "Ubuntu Mono",
                           "Courier New",
                           "Courier New",
                           "Courier New",
                           "Nimbus Mono",
                           "Nimbus Mono PS",
                           "FreeMono",
                           "FreeMono",
                           "Fixed",
                           "Fixed",
                           NULL};

    for (int i = 0; fonts[i] != NULL; i++) {
        FILE *test = file_open(fonts[i], "rb");
        if (test) {
            fclose(test);
            if (font_name_out) {
                *font_name_out = names[i];
            }
            return fonts[i];
        }
    }
#endif
    if (font_name_out) {
        *font_name_out = NULL;
    }
    return NULL;
}

int main(int argc, char **argv) {
    /* Initialize argparse */
    struct argparse argparse;
    argparse_init(&argparse, options, usages, 0);
    argparse_describe(&argparse,
                      "Telnet GUI client with Lisp scripting support.\n"
                      "If hostname and port are not provided, starts in unconnected mode.",
                      "\nExamples:\n"
                      "  telnet-gui\n"
                      "      Start in unconnected mode\n"
                      "  telnet-gui telnet-server 4449\n"
                      "      Connect to telnet-server on port 4449\n"
                      "  telnet-gui -s 20 telnet-server 4449\n"
                      "      Connect with 20pt font size\n"
                      "  telnet-gui -f inconsolata telnet-server 4449\n"
                      "      Connect using Inconsolata font\n"
                      "  telnet-gui -g 100x40 telnet-server 4449\n"
                      "      Connect with 100x40 terminal size\n"
                      "  telnet-gui -l completion.lisp telnet-server 4449\n"
                      "      Connect and load Lisp configuration file\n"
                      "  telnet-gui --line-height 1.5 telnet-server 4449\n"
                      "      Connect with 1.5x line height (50% more spacing)\n"
                      "  telnet-gui -l tintin.lisp -l myconfig.lisp server 4449\n"
                      "      Load multiple Lisp files in order");

    /* Parse arguments */
    int remaining = argparse_parse(&argparse, argc, (const char **)argv);
    if (remaining < 0) {
        // Error occurred during parsing
        return 1;
    }
    argc = remaining;

    /* Handle positional arguments (hostname and port) */
    if (argc > 0) {
        if (argv[0] == NULL) {
            fprintf(stderr, "Error: Invalid argument parsing\n");
            return 1;
        }
        hostname = argv[0];
        if (argc > 1) {
            if (argv[1] == NULL) {
                fprintf(stderr, "Error: Invalid argument parsing\n");
                return 1;
            }
            port = atoi(argv[1]);
            if (port <= 0 || port > 65535) {
                fprintf(stderr, "Error: Invalid port number '%s'. Must be between 1 and 65535\n", argv[1]);
                return 1;
            }
        }
    }

    /* Validate hostname/port combination */
    if (hostname != NULL && port == 0) {
        fprintf(stderr, "Error: If hostname is provided, port must also be provided\n");
        return 1;
    }
    if (hostname == NULL && port != 0) {
        fprintf(stderr, "Error: If port is provided, hostname must also be provided\n");
        return 1;
    }

    /* Connection mode: if hostname and port provided, start in connected mode */
    int connected_mode = (hostname != NULL && port != 0);

    /* Set locale for UTF-8 support */
    setlocale(LC_ALL, "");

    /* Initialize Lisp bridge (loads init.lisp) */
    if (lisp_x_init() < 0) {
        fprintf(stderr, "Failed to initialize Lisp bridge\n");
        return 1;
    }

    /* Override line height from CLI if provided (before window creation) */
    if (cli_line_height > 0.0f) {
        lisp_x_set_terminal_line_height(cli_line_height);
    }

    /* Enable high DPI awareness on Windows (must be set before SDL_Init) */
    SDL_SetHint(SDL_HINT_WINDOWS_DPI_AWARENESS, "permonitorv2");

    /* Initialize SDL2 */
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER) < 0) {
        fprintf(stderr, "SDL_Init failed: %s\n", SDL_GetError());
        return 1;
    }

    if (TTF_Init() < 0) {
        fprintf(stderr, "TTF_Init failed: %s\n", TTF_GetError());
        SDL_Quit();
        return 1;
    }

#ifdef _WIN32
    /* Initialize DirectWrite if requested */
    if (use_directwrite) {
        if (directwrite_init() < 0) {
            fprintf(stderr, "Warning: DirectWrite initialization failed, falling back to SDL_ttf\n");
            use_directwrite = 0;
        }
    }
#endif

    atexit(cleanup);

    /* Query display DPI for font rendering */
    float ddpi = 96.0f, hdpi = 96.0f, vdpi = 96.0f; /* Default to 96 DPI (Windows standard) */

    if (SDL_GetDisplayDPI(0, &ddpi, &hdpi, &vdpi) == 0) {
        fprintf(stderr, "Display DPI: %.1f diagonal, %.1f horizontal, %.1f vertical\n", ddpi, hdpi, vdpi);
    } else {
        fprintf(stderr, "Could not query display DPI: %s\n", SDL_GetError());
        fprintf(stderr, "Using default 96 DPI\n");
        hdpi = 96.0f;
        vdpi = 96.0f;
    }

    /* Determine font based on user preference */
    const char *font_filename = NULL;
    const char *font_name = NULL;
    const char *system_font_path = NULL;
    const char *system_font_name = NULL;

    /* If system font is requested, find it first */
    if (font_choice == 's') {
        system_font_path = find_system_monospace_font(&system_font_name);
        if (system_font_path) {
            fprintf(stderr, "Font resolution: Found system font: %s at %s\n", system_font_name, system_font_path);
        } else {
            fprintf(stderr, "Font resolution: No system font found, falling back to DejaVu Sans Mono\n");
            font_choice = 'd'; /* Fall back to DejaVu if system font not found */
        }
    }

    /* If not using system font, determine embedded font filename */
    if (font_choice != 's') {
        switch (font_choice) {
        case 'm':
            font_name = "Cascadia Code";
            font_filename = "CascadiaCode-Regular.ttf";
            break;
        case 'i':
            font_name = "Inconsolata";
            font_filename = "Inconsolata-Regular.ttf";
            break;
        case 'p':
            font_name = "IBM Plex Mono";
            font_filename = "IBMPlexMono-Regular.ttf";
            break;
        case 'd':
            font_name = "DejaVu Sans Mono";
            font_filename = "DejaVuSansMono.ttf";
            break;
        case 'c':
            font_name = "Courier Prime";
            font_filename = "CourierPrime-Regular.ttf";
            break;
        default:
            fprintf(stderr, "Internal error: Invalid font_choice '%c'\n", font_choice);
            return 1;
        }
        fprintf(stderr, "Font resolution: Using %s font (filename: %s)\n", font_name, font_filename);
    }

    /* Create glyph cache with selected font */
    /* Get executable base path using SDL */
    char *base_path = SDL_GetBasePath();
    char font_path[1024] = {0};
    const char *font_paths[15];
    const char *font_path_labels[15];
    int font_path_count = 0;

    fprintf(stderr, "Font resolution: Starting font search...\n");

    /* If using system font, try it first */
    if (font_choice == 's' && system_font_path) {
        font_paths[font_path_count] = system_font_path;
        font_path_labels[font_path_count++] = "system font";
        font_name = system_font_name;
        fprintf(stderr, "Font resolution: Using system font: %s\n", font_name);
    }

    /* Priority path for installed builds (POSIX-compliant, runtime-resolved) */
    if (font_choice != 's') {
        static char installed_font_path[TELNET_MAX_PATH];
        if (path_construct_installed_resource("fonts", font_filename, installed_font_path,
                                              sizeof(installed_font_path))) {
            font_paths[font_path_count] = installed_font_path;
            font_path_labels[font_path_count++] = "installed data directory (POSIX, runtime-resolved)";
            fprintf(stderr, "Font resolution: Trying installed path: %s\n", installed_font_path);
        }
    }

    /* If using embedded font, try font relative to executable first (installation path) */
    if (font_choice != 's' && base_path) {
        /* SDL_GetBasePath() should return a path with trailing separator, but be safe */
        size_t base_len = strlen(base_path);
        const char *sep =
            (base_len > 0 && (base_path[base_len - 1] == '/' || base_path[base_len - 1] == '\\')) ? "" : "/";

        /* Construct path using Unix separator internally */
        char normalized_path[1024];
        snprintf(normalized_path, sizeof(normalized_path), "%s%sfonts/%s", base_path, sep, font_filename);
        path_normalize_for_platform(normalized_path);

        strncpy(font_path, normalized_path, sizeof(font_path) - 1);
        font_path[sizeof(font_path) - 1] = '\0';

        font_paths[font_path_count] = font_path;
        font_path_labels[font_path_count] = "executable-relative (installation path)";
        font_path_count++;
        fprintf(stderr, "Font resolution: Executable base path: %s\n", base_path);
        fprintf(stderr, "Font resolution: Constructed font path: %s\n", font_path);
        SDL_free(base_path);
    } else if (font_choice != 's') {
        SDL_free(base_path);
        fprintf(stderr, "Font resolution: Warning - SDL_GetBasePath() returned NULL\n");
    } else {
        SDL_free(base_path);
    }

    /* Add fallback paths for embedded fonts */
    if (font_choice != 's') {
        static char fallback_path1[256];
        snprintf(fallback_path1, sizeof(fallback_path1), "fonts/%s", font_filename);
        font_paths[font_path_count] = fallback_path1;
        font_path_labels[font_path_count++] = "current directory relative (build/development)";

        static char fallback_path2[256];
        snprintf(fallback_path2, sizeof(fallback_path2), "../fonts/%s", font_filename);
        font_paths[font_path_count] = fallback_path2;
        font_path_labels[font_path_count++] = "parent directory relative";
    }

    /* Last resort: try system font if embedded font failed, or try DejaVu if system font failed */
    if (font_choice == 's') {
        /* If system font failed, try DejaVu as fallback */
        static char fallback_dejavu1[256];
        snprintf(fallback_dejavu1, sizeof(fallback_dejavu1), "fonts/DejaVuSansMono.ttf");
        font_paths[font_path_count] = fallback_dejavu1;
        font_path_labels[font_path_count++] = "fallback: DejaVu Sans Mono (current dir)";

        if (!font_name) {
            font_name = "DejaVu Sans Mono";
        }
    } else {
        /* If embedded font failed, try system font as fallback */
        const char *fallback_system_path = find_system_monospace_font(NULL);
        if (fallback_system_path) {
            font_paths[font_path_count] = fallback_system_path;
            font_path_labels[font_path_count++] = "fallback: system monospace font";
        }
    }

    font_paths[font_path_count] = NULL;

    /* Create a minimal hidden window to get a renderer for font loading */
    /* This allows us to calculate exact window size before creating the real window */
    SDL_Window *hidden_window =
        SDL_CreateWindow("", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 1, 1, SDL_WINDOW_HIDDEN);
    if (!hidden_window) {
        fprintf(stderr, "Failed to create hidden window for font loading: %s\n", SDL_GetError());
        SDL_Quit();
        return 1;
    }

    SDL_Renderer *temp_renderer = SDL_CreateRenderer(hidden_window, -1, SDL_RENDERER_ACCELERATED);
    if (!temp_renderer) {
        fprintf(stderr, "Failed to create renderer for font loading: %s\n", SDL_GetError());
        SDL_DestroyWindow(hidden_window);
        SDL_Quit();
        return 1;
    }

    /* Load font using temporary renderer to get actual cell metrics */
    GlyphCache *glyph_cache = NULL;
    const char *loaded_font_path = NULL;
    const char *loaded_font_label = NULL;

    for (int i = 0; font_paths[i] != NULL; i++) {
        fprintf(stderr, "Font resolution: Trying [%d] %s: %s\n", i + 1, font_path_labels[i], font_paths[i]);

        /* Check if file exists before trying to load */
        FILE *test = file_open(font_paths[i], "rb");
        if (test) {
            fclose(test);
            fprintf(stderr, "Font resolution: File exists, attempting to load...\n");
        } else {
            fprintf(stderr, "Font resolution: File does not exist, skipping...\n");
            continue;
        }

        /* Use specified font size with specified hinting and antialiasing */
        /* Pass metrics_only=1 since we only need cell dimensions here (skip loading bold/emoji/symbol fonts) */
        GlyphCacheBackendType backend = use_directwrite ? GLYPH_CACHE_BACKEND_DIRECTWRITE : GLYPH_CACHE_BACKEND_SDL_TTF;
        glyph_cache = glyph_cache_create_with_backend(backend, temp_renderer, font_paths[i], font_name, font_size,
                                                      hinting_mode, scale_mode, (int)hdpi, (int)vdpi, use_cleartype, 1);
        if (glyph_cache) {
            loaded_font_path = font_paths[i];
            loaded_font_label = font_path_labels[i];
            fprintf(stderr, "Font resolution: SUCCESS! Loaded font from [%d] %s\n", i + 1, loaded_font_label);
            fprintf(stderr, "Font resolution: Font file path: %s\n", loaded_font_path);
            break;
        } else {
            fprintf(stderr, "Font resolution: Failed to load font from: %s (error: %s)\n", font_paths[i],
                    TTF_GetError());
        }
    }

    if (!glyph_cache) {
        fprintf(stderr, "Font resolution: ERROR - Failed to create glyph cache from all attempted paths\n");
        SDL_DestroyRenderer(temp_renderer);
        SDL_DestroyWindow(hidden_window);
        SDL_Quit();
        return 1;
    }

    /* Get actual cell metrics from loaded font */
    int cell_w, cell_h;
    glyph_cache_get_cell_size(glyph_cache, &cell_w, &cell_h);

    /* Get line height multiplier and calculate effective cell height */
    float line_height = lisp_x_get_terminal_line_height();
    int effective_cell_h = (int)(cell_h * line_height);

    /* Calculate exact window size for terminal geometry using effective cell height */
    int separator_and_input_height = dock_height_rows(1) * effective_cell_h;
    int precise_width = terminal_cols * cell_w + 2 * PADDING_X;
    int precise_height = terminal_rows * effective_cell_h + separator_and_input_height + 2 * PADDING_Y;

    /* Clean up hidden window and temporary renderer */
    SDL_DestroyRenderer(temp_renderer);
    SDL_DestroyWindow(hidden_window);

    /* Create the real window with the exact calculated size */
    /* Create window */
    Window *win = window_create("Telnet GUI", precise_width, precise_height);
    if (!win) {
        fprintf(stderr, "Failed to create window\n");
        glyph_cache_destroy(glyph_cache);
        SDL_Quit();
        return 1;
    }

    SDL_Renderer *renderer = window_get_sdl_renderer(win);
    SDL_Window *sdl_window = window_get_sdl_window(win);

    /* Recreate glyph cache with the real renderer (font is already loaded, but cache needs real renderer) */
    /* We need to destroy the old cache and create a new one with the real renderer */
    glyph_cache_destroy(glyph_cache);
    glyph_cache = NULL;

    for (int i = 0; font_paths[i] != NULL; i++) {
        if (strcmp(font_paths[i], loaded_font_path) == 0) {
            GlyphCacheBackendType backend2 =
                use_directwrite ? GLYPH_CACHE_BACKEND_DIRECTWRITE : GLYPH_CACHE_BACKEND_SDL_TTF;
            glyph_cache =
                glyph_cache_create_with_backend(backend2, renderer, font_paths[i], font_name, font_size, hinting_mode,
                                                scale_mode, (int)hdpi, (int)vdpi, use_cleartype, 0);
            if (glyph_cache) {
                break;
            }
        }
    }

    if (!glyph_cache) {
        fprintf(stderr, "Font resolution: ERROR - Failed to recreate glyph cache with real renderer\n");
        window_destroy(win);
        SDL_Quit();
        return 1;
    }

    /* Get cell metrics again (should be the same, but get from new cache) */
    glyph_cache_get_cell_size(glyph_cache, &cell_w, &cell_h);

    /* Create renderer */
    Renderer *rend = renderer_create(renderer, glyph_cache, cell_w, cell_h);
    if (!rend) {
        fprintf(stderr, "Failed to create renderer\n");
        glyph_cache_destroy(glyph_cache);
        window_destroy(win);
        return 1;
    }

    /* Initialize input area (needed for terminal size calculation) */
    dock_init(&dock);

    /* Create terminal with specified geometry */
    Terminal *term = terminal_create(terminal_rows, terminal_cols);
    if (!term) {
        fprintf(stderr, "Failed to create terminal\n");
        renderer_destroy(rend);
        glyph_cache_destroy(glyph_cache);
        window_destroy(win);
        return 1;
    }

    /* Apply scrollback configuration from Lisp */
    int max_scrollback = lisp_x_get_max_scrollback_lines();
    terminal_set_max_scrollback_lines(term, max_scrollback);

    /* Register terminal with Lisp bridge for terminal-echo builtin */
    lisp_x_register_terminal(term);

    /* Create Telnet client */
    Telnet *telnet = telnet_create();
    if (!telnet) {
        fprintf(stderr, "Failed to create telnet client\n");
        terminal_destroy(term);
        renderer_destroy(rend);
        glyph_cache_destroy(glyph_cache);
        window_destroy(win);
        return 1;
    }

    /* Register telnet with Lisp bridge for telnet-send builtin */
    lisp_x_register_telnet(telnet);

    /* Register glyph cache with Lisp bridge for terminal-info builtin */
    lisp_x_register_glyph_cache(glyph_cache);

    /* Register window with Lisp bridge for terminal-info builtin */
    lisp_x_register_window(win);

    /* Register input area with Lisp bridge for input-area-redraw builtin */
    lisp_x_register_dock(&dock);

#if HAVE_RLOTTIE
    /* Register SDL renderer with Lisp for animation creation */
    lisp_x_register_renderer(renderer);
#endif

    /* Load init-post.lisp now that SDL/GUI is initialized */
    lisp_x_load_init_post();

    /* Wire telnet to terminal for output buffering */
    terminal_set_telnet(term, telnet);

    /* Get actual window size and resize terminal to match (before loading Lisp files) */
    int actual_width, actual_height;
    SDL_GetWindowSize(sdl_window, &actual_width, &actual_height);
    int initial_rows, initial_cols;
    calculate_terminal_size(actual_width, actual_height, cell_w, cell_h, &dock, &initial_rows, &initial_cols);
    int input_visible_rows = dock_get_text_rows(&dock);
    terminal_resize(term, initial_rows, initial_cols, input_visible_rows);
    telnet_set_terminal_size(telnet, initial_cols, initial_rows);

    /* Load user-provided Lisp files in order after terminal and telnet are registered */
    for (int i = 0; i < lisp_file_count; i++) {
        if (lisp_x_load_file(lisp_files[i]) < 0) {
            fprintf(stderr, "Failed to load Lisp file: %s\n", lisp_files[i]);
            /* Don't exit - just continue without user config */
        }
    }

    /* Re-apply CLI line height override after user files (CLI takes final precedence) */
    if (cli_line_height > 0.0f) {
        lisp_x_set_terminal_line_height(cli_line_height);
    }

    /* Start Lisp profiler if --profile was specified */
    if (profile_mode) {
        lisp_x_profile_start();
        profile_stats_reset();
        fprintf(stderr, "Profiling enabled: Lisp profiler + C timing instrumentation\n");
    }

    /* Connect if in connected mode */
    if (connected_mode) {
        fprintf(stderr, "Connecting to %s:%d...\n", hostname, port);
        if (telnet_connect(telnet, hostname, port) < 0) {
            fprintf(stderr, "Failed to connect to %s:%d\n", hostname, port);
            /* Don't exit - just start in unconnected mode */
            connected_mode = 0;
            const char *msg = "\r\n*** Failed to connect - starting in unconnected mode ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
        } else {
            const char *msg = "\r\n*** Connected ***\r\n";
            terminal_feed_data(term, msg, strlen(msg));
        }
    } else {
        const char *msg = "\r\n*** Starting in unconnected mode ***\r\n";
        terminal_feed_data(term, msg, strlen(msg));
    }

    /* Perform initial render to eliminate white border artifact */
    /* Clear renderer to terminal background color */
    int bg_r, bg_g, bg_b;
    lisp_x_get_terminal_bg_color(&bg_r, &bg_g, &bg_b);
    SDL_SetRenderDrawColor(renderer, bg_r, bg_g, bg_b, 255);
    SDL_RenderClear(renderer);

    /* Render input area to vterm FIRST (must happen before renderer reads vterm cells) */
    terminal_render_dock(term, &dock, initial_cols);

    /* Render initial terminal state */
    char title[256];
    snprintf(title, sizeof(title), "Telnet: %s:%d", hostname ? hostname : "", port);
    renderer_render(rend, term, title, 0, 0, 0, 0, 0, 0, 0, 0, 0, &dock, initial_cols);

    /* Present the initial frame immediately */
    SDL_RenderPresent(renderer);

    /* Main loop */
    SDL_Event event;
    int mouse_x = 0, mouse_y = 0;

    /* Get display file descriptor for blocking event loop */
    int display_fd = get_display_fd(sdl_window);
    int use_fd_wait = (display_fd >= 0);

    while (running && !quit_requested) {
        /* Check if animation is playing (affects timeout calculation) */
        int animation_playing = 0;
#if HAVE_RLOTTIE
        Animation *anim_check = lisp_x_get_active_animation();
        if (anim_check && animation_is_loaded(anim_check) && animation_is_playing(anim_check)) {
            animation_playing = 1;
        }
#endif

        /* Calculate timeout based on animations and timers */
        int timeout_ms = calculate_timeout_ms(animation_playing);

        if (use_fd_wait) {
            /* Build fd_set with display_fd and telnet socket */
            fd_set readfds;
            FD_ZERO(&readfds);
            FD_SET(display_fd, &readfds);

            int max_fd = display_fd;
            if (connected_mode) {
                int sock = telnet_get_socket(telnet);
                if (sock >= 0) {
                    FD_SET(sock, &readfds);
                    if (sock > max_fd)
                        max_fd = sock;
                }
            }

            /* Set timeout */
            struct timeval tv, *tvp = NULL;
            if (timeout_ms >= 0) {
                tv.tv_sec = timeout_ms / 1000;
                tv.tv_usec = (timeout_ms % 1000) * 1000;
                tvp = &tv;
            }

            /* Wayland: flush outgoing requests before blocking */
            wayland_pre_wait(sdl_window);

            /* Block until event or timeout */
            int select_result = select(max_fd + 1, &readfds, NULL, NULL, tvp);

            /* Wayland: dispatch any pending events after waking */
            wayland_post_wait(sdl_window);

            /* On select error (except EINTR), fall through to poll-based handling */
            if (select_result < 0 && errno != EINTR) {
                /* Error - fall back to single iteration */
            }
        } else {
            /* Fallback: polling with delay (Windows or no display fd) */
            int delay = (timeout_ms >= 0 && timeout_ms < 16) ? timeout_ms : 16;
            if (delay > 0)
                SDL_Delay(delay);
        }

        /* Pump and poll SDL events */
        SDL_PumpEvents();
        while (SDL_PollEvent(&event)) {
            switch (event.type) {
            case SDL_QUIT:
                running = 0;
                break;

            case SDL_WINDOWEVENT:
                if (event.window.event == SDL_WINDOWEVENT_RESIZED) {
                    int new_width = event.window.data1;
                    int new_height = event.window.data2;

                    /* Step 1: Calculate new columns from width */
                    int available_width = new_width - 2 * PADDING_X;
                    int new_cols = available_width / cell_w;
                    if (new_cols < 10)
                        new_cols = 10; /* Minimum width */

                    /* Step 2: Force recalculation of input area layout with new columns */
                    /* This updates visible_rows based on new width */
                    dock.needs_layout_recalc = 1;
                    dock_recalculate_layout(&dock, new_cols);

                    /* Step 3: Calculate terminal rows using updated input area height */
                    int available_height = new_height - 2 * PADDING_Y;
                    float line_height = lisp_x_get_terminal_line_height();
                    int effective_cell_h = (int)(cell_h * line_height);
                    int input_height_rows = dock_height_rows(dock_get_text_rows(&dock));
                    int new_rows = (available_height / effective_cell_h) - input_height_rows;
                    if (new_rows < 1)
                        new_rows = 1; /* Minimum: 1 scrolling row */

                    /* Step 4: Resize terminal and update */
                    int input_visible_rows = dock_get_text_rows(&dock);

                    /* Save cursor position before resize corrupts it */
                    int saved_cursor_row, saved_cursor_col, saved_cursor_visible;
                    terminal_get_cursor_info(term, &saved_cursor_row, &saved_cursor_col, &saved_cursor_visible);

                    terminal_resize(term, new_rows, new_cols, input_visible_rows);

                    /* Re-render input area immediately to update divider position */
                    terminal_render_dock(term, &dock, new_cols);

                    /* Restore cursor position using explicit CUP (avoids DECSC/DECRC nesting issue) */
                    char cursor_pos_seq[16];
                    ansi_format_cursor_pos(cursor_pos_seq, sizeof(cursor_pos_seq), saved_cursor_row + 1,
                                           saved_cursor_col + 1);
                    terminal_feed_data(term, cursor_pos_seq, strlen(cursor_pos_seq));

                    /* Send NAWS to telnet server */
                    telnet_set_terminal_size(telnet, new_cols, new_rows);

                    /* Force a full redraw to clear any artifacts from the resize */
                    /* Get actual window size to ensure we clear the entire window */
                    int actual_win_width, actual_win_height;
                    SDL_GetWindowSize(sdl_window, &actual_win_width, &actual_win_height);

                    /* Clear entire renderer to terminal background color */
                    int bg_r, bg_g, bg_b;
                    lisp_x_get_terminal_bg_color(&bg_r, &bg_g, &bg_b);
                    SDL_SetRenderDrawColor(renderer, bg_r, bg_g, bg_b, 255);
                    SDL_RenderClear(renderer);

                    /* Calculate terminal area bounds and fill any area beyond with background color */
                    int terminal_width = new_cols * cell_w + 2 * PADDING_X;
                    int terminal_height =
                        (new_rows + dock_height_rows(input_visible_rows)) * effective_cell_h + 2 * PADDING_Y;

                    /* Fill any area beyond the terminal content with background color */
                    /* This handles cases where window is larger than terminal area */
                    if (actual_win_width > terminal_width) {
                        SDL_Rect right_fill = {terminal_width, 0, actual_win_width - terminal_width, actual_win_height};
                        SDL_RenderFillRect(renderer, &right_fill);
                    }
                    if (actual_win_height > terminal_height) {
                        SDL_Rect bottom_fill = {0, terminal_height, actual_win_width,
                                                actual_win_height - terminal_height};
                        SDL_RenderFillRect(renderer, &bottom_fill);
                    }

                    /* Force terminal to redraw */
                    terminal_request_redraw(term);
                    dock_request_redraw(&dock);

                    /* Render immediately to clear artifacts */
                    char title[256];
                    snprintf(title, sizeof(title), "Telnet: %s:%d", hostname ? hostname : "", port);
                    renderer_render(rend, term, title, terminal_selection.active, terminal_selection.start_row,
                                    terminal_selection.start_col, terminal_selection.start_viewport_offset,
                                    terminal_selection.start_scrollback_size, terminal_selection.end_row,
                                    terminal_selection.end_col, terminal_selection.end_viewport_offset,
                                    terminal_selection.end_scrollback_size, &dock, new_cols);
                    terminal_mark_drawn(term);
                    dock_mark_drawn(&dock);

                    /* Present the frame */
                    SDL_RenderPresent(renderer);
                } else if (event.window.event == SDL_WINDOWEVENT_DISPLAY_CHANGED) {
                    /* Window moved to a different display - check for DPI change */
                    int display_idx = SDL_GetWindowDisplayIndex(sdl_window);
                    float new_ddpi, new_hdpi, new_vdpi;

                    if (SDL_GetDisplayDPI(display_idx, &new_ddpi, &new_hdpi, &new_vdpi) == 0) {
                        fprintf(stderr, "Display changed to %d: DPI %.1f x %.1f\n", display_idx, new_hdpi, new_vdpi);

                        /* Check if DPI actually changed */
                        if ((int)new_hdpi != (int)hdpi || (int)new_vdpi != (int)vdpi) {
                            fprintf(stderr, "DPI changed from %.1f x %.1f to %.1f x %.1f - recreating fonts\n", hdpi,
                                    vdpi, new_hdpi, new_vdpi);

                            /* Update stored DPI values */
                            hdpi = new_hdpi;
                            vdpi = new_vdpi;

                            /* Get current terminal size BEFORE recreating fonts */
                            int current_rows, current_cols;
                            terminal_get_size(term, &current_rows, &current_cols);

                            /* Get font info from existing cache before destroying it */
                            const char *current_font_path = glyph_cache_get_font_path(glyph_cache);
                            const char *current_font_name = glyph_cache_get_font_name(glyph_cache);

                            /* Make copies since the strings will be freed with the cache */
                            char *font_path_copy = strdup(current_font_path);
                            char *font_name_copy = strdup(current_font_name);

                            /* Destroy old glyph cache */
                            glyph_cache_destroy(glyph_cache);

                            /* Create new glyph cache with new DPI */
                            GlyphCacheBackendType backend3 =
                                use_directwrite ? GLYPH_CACHE_BACKEND_DIRECTWRITE : GLYPH_CACHE_BACKEND_SDL_TTF;
                            glyph_cache = glyph_cache_create_with_backend(
                                backend3, renderer, font_path_copy, font_name_copy, font_size, hinting_mode, scale_mode,
                                (int)new_hdpi, (int)new_vdpi, use_cleartype, 0);

                            free(font_path_copy);
                            free(font_name_copy);

                            if (glyph_cache) {
                                /* Update cell dimensions */
                                glyph_cache_get_cell_size(glyph_cache, &cell_w, &cell_h);
                                float line_height = lisp_x_get_terminal_line_height();
                                int effective_cell_h = (int)(cell_h * line_height);

                                /* Recreate renderer with new glyph cache */
                                renderer_destroy(rend);
                                rend = renderer_create(renderer, glyph_cache, cell_w, cell_h);

                                /* Calculate new window size to maintain same terminal dimensions */
                                int input_visible_rows = dock_get_text_rows(&dock);
                                int total_rows = current_rows + dock_height_rows(input_visible_rows);
                                int new_win_width = current_cols * cell_w + 2 * PADDING_X;
                                int new_win_height = total_rows * effective_cell_h + 2 * PADDING_Y;

                                /* Resize window to maintain terminal dimensions */
                                SDL_SetWindowSize(sdl_window, new_win_width, new_win_height);

                                /* Recalculate input area layout */
                                dock.needs_layout_recalc = 1;
                                dock_recalculate_layout(&dock, current_cols);
                                terminal_render_dock(term, &dock, current_cols);

                                /* Force full redraw */
                                terminal_request_redraw(term);
                                dock_request_redraw(&dock);

                                fprintf(stderr, "DPI change complete: cell size %dx%d, window %dx%d, terminal %dx%d\n",
                                        cell_w, cell_h, new_win_width, new_win_height, current_cols, current_rows);
                            } else {
                                fprintf(stderr, "ERROR: Failed to recreate glyph cache after DPI change\n");
                            }
                        }
                    }
                }
                break;

            case SDL_MOUSEBUTTONDOWN: {
                /* Right-click is handled in MOUSEBUTTONUP for copy */
                if (event.button.button == SDL_BUTTON_RIGHT) {
                    break;
                }
                /* Check if there was an active selection that needs clearing */
                int had_selection = terminal_selection.active;
                if (had_selection) {
                    clear_terminal_selection(term);
                }

                mouse_x = event.button.x;
                mouse_y = event.button.y;
                /* Get window size to check bounds */
                int window_width, window_height;
                window_get_size(win, &window_width, &window_height);

                /* Get line height multiplier and calculate effective cell height */
                float line_height = lisp_x_get_terminal_line_height();
                int effective_cell_h = (int)(cell_h * line_height);

                /* Handle clicks in terminal area (excluding padding) */
                if (mouse_x >= PADDING_X && mouse_x < window_width - PADDING_X && mouse_y >= PADDING_Y &&
                    mouse_y < window_height - PADDING_Y) {
                    /* Start selection only if no selection was cleared */
                    if (event.button.button == SDL_BUTTON_LEFT && !had_selection) {
                        /* Convert mouse coordinates to terminal cell coordinates, subtracting padding */
                        int term_row = (mouse_y - PADDING_Y) / effective_cell_h;
                        int term_col = (mouse_x - PADDING_X) / cell_w;
                        /* Start selection and freeze viewport */
                        start_terminal_selection(term, term_row, term_col);
                    }
                }
                break;
            }

            case SDL_KEYDOWN: {
                /* All keyboard input goes to input area, not terminal */
                SDL_Scancode scancode = event.key.keysym.scancode;
                SDL_Keymod mod = event.key.keysym.mod;

                /* Accept tab completion if active, except for TAB (cycles), ESC (cancels), and Ctrl+G (cancels) */
                if (lisp_x_is_tab_mode_active()) {
                    if (scancode != SDL_SCANCODE_TAB && scancode != SDL_SCANCODE_ESCAPE &&
                        !(scancode == SDL_SCANCODE_G && (mod & KMOD_CTRL))) {
                        lisp_x_accept_tab_completion();
                    }
                }

                /* Check for Ctrl+_ (undo) / Alt+_ (redo)
                 * Use scancode for physical key consistency across keyboard layouts.
                 * SDL_SCANCODE_MINUS targets the physical key that produces '-' and '_' on US/German layouts
                 * regardless of whether the software layout is German, US, etc.
                 * This ensures the shortcut stays in the same physical location, which is standard
                 * for gaming and professional software. */
                if (scancode == SDL_SCANCODE_MINUS) {
                    if ((mod & KMOD_CTRL) && (mod & KMOD_SHIFT)) {
                        /* Captured Ctrl + Shift + _ (physically) */
                        dock_undo(&dock);
                        break;
                    } else if ((mod & KMOD_ALT) && (mod & KMOD_SHIFT)) {
                        /* Captured Alt + Shift + _ (physically) */
                        dock_redo(&dock);
                        break;
                    }
                }

                switch (scancode) {
                case SDL_SCANCODE_RETURN:
                case SDL_SCANCODE_KP_ENTER: {
                    /* Send input area text to terminal and telnet */
                    int length = dock_get_length(&dock);
                    if (length > 0) {
                        const char *text = dock_get_text(&dock);
                        int cursor_pos = dock_get_cursor_pos(&dock);

                        /* Echo raw input FIRST for non-eval mode (eval mode has its own echo with >) */
                        if (dock_get_mode(&dock) != DOCK_MODE_EVAL && dock.echo_buf) {
                            char color_buf[32];
                            int echo_r, echo_g, echo_b;
                            lisp_x_get_user_input_echo_color(&echo_r, &echo_g, &echo_b);
                            dynamic_buffer_clear(dock.echo_buf);
                            ansi_format_fg_color_rgb(color_buf, sizeof(color_buf), echo_r, echo_g, echo_b);
                            dynamic_buffer_append_str(dock.echo_buf, color_buf);
                            if (dynamic_buffer_append_printf(dock.echo_buf, "%s\r\n", text) == 0) {
                                /* Reset color after text */
                                dynamic_buffer_append_str(dock.echo_buf, ANSI_SGR_RESET);
                                terminal_feed_data(term, dynamic_buffer_data(dock.echo_buf),
                                                   dynamic_buffer_len(dock.echo_buf));
                            }
                        }

                        /* Check if this is a special command starting with ':' */
                        if (text[0] == ':') {
                            /* Process command */
                            process_command(text, telnet, term, &connected_mode, &dock, &quit_requested);

                            /* Add to history and clear input area */
                            dock_history_add(&dock);
                            dock_clear(&dock);
                        } else if (dock_get_mode(&dock) == DOCK_MODE_EVAL) {
                            /* Eval mode - evaluate Lisp expression using shared eval logic */
                            if (dock.eval_buf && lisp_x_eval_and_echo(text, dock.eval_buf) == 0) {
                                terminal_feed_data(term, dynamic_buffer_data(dock.eval_buf),
                                                   dynamic_buffer_len(dock.eval_buf));
                            }

                            /* Add to history and clear input area */
                            dock_history_add(&dock);
                            dock_clear(&dock);
                        } else {
                            /* Normal text - call user-input-hook to transform text before sending */
                            const char *transformed_text = lisp_x_call_user_input_hook(text, cursor_pos);
                            int transformed_length = strlen(transformed_text);

                            /* Hook contract: non-string or empty string = hook handled everything */
                            /* Proper way: return nil to indicate hook handled echo/send */
                            /* If hook returns empty string, it means hook handled echo/send - don't send again */

                            if (transformed_length > 0) {
                                /* DON'T echo again - raw input was already echoed above */
                                /* Send transformed text to telnet (unified function handles LF->CRLF, CRLF appending,
                                 * and errors) */
                                send_to_telnet(telnet, term, &dock, &connected_mode, transformed_text,
                                               transformed_length, 1); /* append_crlf = 1 */
                            }
                            /* Empty string from hook - user_input_received already set by dock operations */

                            /* Add to history and clear input area */
                            dock_history_add(&dock);
                            dock_clear(&dock);
                        }
                    } else {
                        /* Even if input is empty, send CRLF for newline (unified function handles errors) */
                        send_to_telnet(telnet, term, &dock, &connected_mode, "", 0, 1); /* append_crlf = 1 */
                        /* Echo newline to terminal (vterm_feed_data will normalize LF to CRLF) */
                        terminal_feed_data(term, "\n", 1);
                    }
                    break;
                }
                case SDL_SCANCODE_BACKSPACE: {
                    dock_backspace(&dock);
                    break;
                }
                case SDL_SCANCODE_DELETE: {
                    dock_delete_char(&dock);
                    break;
                }
                case SDL_SCANCODE_LEFT: {
                    /* Start selection if Shift is pressed and no selection */
                    if ((mod & KMOD_SHIFT) && !dock_has_selection(&dock)) {
                        dock_start_selection(&dock);
                    } else if (!(mod & KMOD_SHIFT)) {
                        dock_clear_selection(&dock);
                    }

                    if (mod & KMOD_CTRL) {
                        dock_move_cursor_word_left(&dock);
                    } else {
                        dock_move_cursor_left(&dock);
                    }
                    break;
                }
                case SDL_SCANCODE_RIGHT: {
                    /* Start selection if Shift is pressed and no selection */
                    if ((mod & KMOD_SHIFT) && !dock_has_selection(&dock)) {
                        dock_start_selection(&dock);
                    } else if (!(mod & KMOD_SHIFT)) {
                        dock_clear_selection(&dock);
                    }

                    if (mod & KMOD_CTRL) {
                        dock_move_cursor_word_right(&dock);
                    } else {
                        dock_move_cursor_right(&dock);
                    }
                    break;
                }
                case SDL_SCANCODE_UP: {
                    /* Navigate lines in multi-line input, or history if at first line */
                    int rows, cols;
                    terminal_get_size(term, &rows, &cols);
                    if (!dock_is_at_first_visual_line(&dock, cols)) {
                        dock_move_cursor_up_line(&dock, cols);
                    } else {
                        dock_history_prev(&dock);
                    }
                    break;
                }
                case SDL_SCANCODE_DOWN: {
                    /* Navigate lines in multi-line input, or history if at last line */
                    int rows, cols;
                    terminal_get_size(term, &rows, &cols);
                    if (!dock_is_at_last_visual_line(&dock, cols)) {
                        dock_move_cursor_down_line(&dock, cols);
                    } else {
                        dock_history_next(&dock);
                    }
                    break;
                }
                case SDL_SCANCODE_HOME: {
                    /* Start selection if Shift is pressed and no selection */
                    if ((mod & KMOD_SHIFT) && !dock_has_selection(&dock)) {
                        dock_start_selection(&dock);
                    } else if (!(mod & KMOD_SHIFT)) {
                        dock_clear_selection(&dock);
                    }

                    dock_move_cursor_home(&dock);
                    break;
                }
                case SDL_SCANCODE_END: {
                    /* Start selection if Shift is pressed and no selection */
                    if ((mod & KMOD_SHIFT) && !dock_has_selection(&dock)) {
                        dock_start_selection(&dock);
                    } else if (!(mod & KMOD_SHIFT)) {
                        dock_clear_selection(&dock);
                    }

                    dock_move_cursor_end(&dock);
                    break;
                }
                case SDL_SCANCODE_A: {
                    if (mod & KMOD_CTRL) {
                        dock_move_cursor_beginning(&dock);
                    }
                    break;
                }
                case SDL_SCANCODE_E: {
                    if (mod & KMOD_CTRL) {
                        dock_move_cursor_end_line(&dock);
                    }
                    break;
                }
                case SDL_SCANCODE_K: {
                    if (mod & KMOD_CTRL) {
                        dock_kill_to_end(&dock);
                        /* Copy killed text to clipboard */
                        const char *killed = dock_get_kill_ring(&dock);
                        if (killed && killed[0] != '\0') {
                            SDL_SetClipboardText(killed);
                        }
                    }
                    break;
                }
                case SDL_SCANCODE_U: {
                    if (mod & KMOD_CTRL) {
                        dock_kill_from_start(&dock);
                        /* Copy killed text to clipboard */
                        const char *killed = dock_get_kill_ring(&dock);
                        if (killed && killed[0] != '\0') {
                            SDL_SetClipboardText(killed);
                        }
                    }
                    break;
                }
                case SDL_SCANCODE_W: {
                    if (mod & KMOD_CTRL) {
                        dock_kill_word(&dock);
                        /* Copy killed text to clipboard */
                        const char *killed = dock_get_kill_ring(&dock);
                        if (killed && killed[0] != '\0') {
                            SDL_SetClipboardText(killed);
                        }
                    }
                    break;
                }
                case SDL_SCANCODE_Y: {
                    if (mod & KMOD_CTRL) {
                        dock_yank(&dock);
                    }
                    break;
                }
                case SDL_SCANCODE_C: {
                    if (mod & KMOD_CTRL) {
                        /* Copy terminal selection if active */
                        if (terminal_selection.active) {
                            copy_terminal_selection(term);
                            clear_terminal_selection(term); /* Clear selection after copy */
                        }
                        /* Copy input area selection or all text to clipboard */
                        else if (dock_has_selection(&dock)) {
                            char selection_buffer[DOCK_MAX_LENGTH];
                            if (dock_copy_selection(&dock, selection_buffer, DOCK_MAX_LENGTH) > 0) {
                                SDL_SetClipboardText(selection_buffer);
                            }
                        } else {
                            const char *text = dock_copy(&dock);
                            if (text && text[0] != '\0') {
                                SDL_SetClipboardText(text);
                            }
                        }
                    }
                    break;
                }
                case SDL_SCANCODE_D: {
                    if (mod & KMOD_CTRL) {
                        /* Ctrl+D: Delete character forward (like Emacs) */
                        dock_delete_char(&dock);
                    }
                    break;
                }
                case SDL_SCANCODE_V: {
                    if (mod & KMOD_CTRL) {
                        /* Paste text from clipboard */
                        if (SDL_HasClipboardText()) {
                            char *text = SDL_GetClipboardText();
                            if (text) {
                                dock_paste(&dock, text);
                                SDL_free(text);
                            }
                        }
                    }
                    break;
                }
                case SDL_SCANCODE_G: {
                    if (mod & KMOD_CTRL) {
                        /* Ctrl+G: Cancel tab completion and revert */
                        if (lisp_x_is_tab_mode_active()) {
                            int cursor_pos = dock_get_cursor_pos(&dock);
                            int length = dock_get_length(&dock);
                            int needs_redraw = dock_needs_redraw(&dock);
                            char *buffer = dock_get_buffer(&dock);
                            lisp_x_cancel_tab_completion(buffer, DOCK_MAX_LENGTH, &cursor_pos, &length, &needs_redraw);
                            dock_sync_state(&dock);
                            dock_move_cursor(&dock, cursor_pos);
                        }
                    }
                    break;
                }
                case SDL_SCANCODE_J: {
                    if (mod & KMOD_CTRL) {
                        /* Ctrl+J: Insert newline for multi-line input */
                        dock_insert_text(&dock, "\n", 1);
                    }
                    break;
                }
                case SDL_SCANCODE_PAGEUP: {
                    /* If Ctrl is pressed, scroll viewport */
                    if (mod & KMOD_CTRL) {
                        int rows, cols;
                        terminal_get_size(term, &rows, &cols);
                        terminal_scroll_up(term, rows);
                    }
                    break;
                }
                case SDL_SCANCODE_PAGEDOWN: {
                    /* If Ctrl is pressed, scroll viewport */
                    if (mod & KMOD_CTRL) {
                        int rows, cols;
                        terminal_get_size(term, &rows, &cols);
                        terminal_scroll_down(term, rows);
                    }
                    break;
                }
                case SDL_SCANCODE_TAB: {
                    /* Shift+Tab: Toggle input mode */
                    if (mod & KMOD_SHIFT) {
                        DockMode current = dock_get_mode(&dock);
                        DockMode new_mode = (current == DOCK_MODE_NORMAL) ? DOCK_MODE_EVAL : DOCK_MODE_NORMAL;
                        dock_set_mode(&dock, new_mode);
                        break;
                    }

                    /* Tab in eval mode: do nothing */
                    if (dock_get_mode(&dock) == DOCK_MODE_EVAL) {
                        break;
                    }

                    /* Handle TAB completion via Lisp bridge */
                    /* Note: lisp_handle_tab modifies buffer directly */
                    int cursor_pos = dock_get_cursor_pos(&dock);
                    int length = dock_get_length(&dock);
                    int needs_redraw = dock_needs_redraw(&dock);
                    char *buffer = dock_get_buffer(&dock);
                    lisp_x_handle_tab(buffer, DOCK_MAX_LENGTH, &cursor_pos, &length, &needs_redraw);
                    /* Sync state after external buffer modification */
                    dock_sync_state(&dock);
                    /* Update cursor position */
                    dock_move_cursor(&dock, cursor_pos);
                    break;
                }
                case SDL_SCANCODE_ESCAPE: {
                    /* ESC: Clear terminal selection if active */
                    if (terminal_selection.active) {
                        clear_terminal_selection(term);
                    }
                    /* ESC: Cancel tab completion and revert */
                    else if (lisp_x_is_tab_mode_active()) {
                        int cursor_pos = dock_get_cursor_pos(&dock);
                        int length = dock_get_length(&dock);
                        int needs_redraw = dock_needs_redraw(&dock);
                        char *buffer = dock_get_buffer(&dock);
                        lisp_x_cancel_tab_completion(buffer, DOCK_MAX_LENGTH, &cursor_pos, &length, &needs_redraw);
                        dock_sync_state(&dock);
                        dock_move_cursor(&dock, cursor_pos);
                    }
                    break;
                }
                default:
                    /* Other keys are handled by SDL_TEXTINPUT */
                    break;
                }
                break;
            }

            case SDL_TEXTINPUT: {
                const char *text = event.text.text;
                int text_len = strlen(text);

                /* Suppress underscore input if modifiers match undo/redo shortcut
                 * This prevents underscore from being inserted when Alt+Shift+- is used for redo */
                if (text_len == 1 && text[0] == '_') {
                    SDL_Keymod current_mod = SDL_GetModState();
                    if (((current_mod & KMOD_CTRL) && (current_mod & KMOD_SHIFT)) ||
                        ((current_mod & KMOD_ALT) && (current_mod & KMOD_SHIFT))) {
                        /* Modifiers match shortcut, ignore this text input */
                        break;
                    }
                }

                /* Accept tab completion if active (any text input exits tab mode) */
                if (lisp_x_is_tab_mode_active()) {
                    lisp_x_accept_tab_completion();
                }
                /* All text input goes to input area */
                dock_insert_text(&dock, text, text_len);
                break;
            }

            case SDL_MOUSEBUTTONUP: {
                /* Right-click copies terminal selection (like Ctrl+C) */
                if (event.button.button == SDL_BUTTON_RIGHT) {
                    if (terminal_selection.active) {
                        copy_terminal_selection(term);
                        clear_terminal_selection(term);
                    }
                    break;
                }
                /* Selection remains active after mouse button up - user can copy with Ctrl+C */
                /* Only handle mouse events for terminal if not in input area or padding */
                int win_width, win_height;
                window_get_size(win, &win_width, &win_height);
                float line_height = lisp_x_get_terminal_line_height();
                int effective_cell_h = (int)(cell_h * line_height);
                int dock_height = dock_height_rows(dock_get_text_rows(&dock)) * effective_cell_h;
                /* Check if click is within terminal area (excluding padding) and not in input area */
                if (event.button.x >= PADDING_X && event.button.x < win_width - PADDING_X &&
                    event.button.y >= PADDING_Y && event.button.y < win_height - PADDING_Y &&
                    event.button.y < win_height - dock_height - PADDING_Y) {
                    input_handle_mouse(&event.button, NULL, terminal_get_vterm(term), cell_w, cell_h, 0);
                }
                break;
            }

            case SDL_MOUSEMOTION:
                /* Update selection if dragging in terminal area */
                if (terminal_selection.active && (event.motion.state & SDL_BUTTON(SDL_BUTTON_LEFT))) {
                    int motion_win_width, motion_win_height;
                    window_get_size(win, &motion_win_width, &motion_win_height);
                    float line_height = lisp_x_get_terminal_line_height();
                    int effective_cell_h = (int)(cell_h * line_height);
                    /* Check if motion is within terminal area (excluding padding) */
                    if (event.motion.x >= PADDING_X && event.motion.x < motion_win_width - PADDING_X &&
                        event.motion.y >= PADDING_Y && event.motion.y < motion_win_height - PADDING_Y) {
                        /* Convert mouse coordinates to terminal cell coordinates, subtracting padding */
                        int term_row = (event.motion.y - PADDING_Y) / effective_cell_h;
                        int term_col = (event.motion.x - PADDING_X) / cell_w;
                        /* Update selection end position */
                        update_terminal_selection(term, term_row, term_col);
                    }
                }
                /* Only handle mouse events for terminal if not in input area or padding */
                int motion_win_width, motion_win_height;
                window_get_size(win, &motion_win_width, &motion_win_height);
                float line_height = lisp_x_get_terminal_line_height();
                int effective_cell_h = (int)(cell_h * line_height);
                int dock_height = dock_height_rows(dock_get_text_rows(&dock)) * effective_cell_h;
                /* Check if motion is within terminal area (excluding padding) and not in input area */
                if (event.motion.x >= PADDING_X && event.motion.x < motion_win_width - PADDING_X &&
                    event.motion.y >= PADDING_Y && event.motion.y < motion_win_height - PADDING_Y &&
                    event.motion.y < motion_win_height - dock_height - PADDING_Y) {
                    input_handle_mouse(NULL, &event.motion, terminal_get_vterm(term), cell_w, cell_h, 0);
                }
                break;

            case SDL_MOUSEWHEEL: {
                /* Get mouse position */
                int mouse_x, mouse_y;
                SDL_GetMouseState(&mouse_x, &mouse_y);

                /* Check if mouse is over terminal area (not input area or padding) */
                int wheel_win_width, wheel_win_height;
                window_get_size(win, &wheel_win_width, &wheel_win_height);
                float line_height = lisp_x_get_terminal_line_height();
                int effective_cell_h = (int)(cell_h * line_height);
                int dock_height = dock_height_rows(dock_get_text_rows(&dock)) * effective_cell_h;
                /* Check if mouse is within terminal area (excluding padding) and not in input area */
                if (mouse_x >= PADDING_X && mouse_x < wheel_win_width - PADDING_X && mouse_y >= PADDING_Y &&
                    mouse_y < wheel_win_height - PADDING_Y && mouse_y < wheel_win_height - dock_height - PADDING_Y) {
                    /* Get scroll configuration from Lisp bridge */
                    int lines_per_click = lisp_x_get_scroll_lines_per_click();
                    int smooth_scrolling = lisp_x_get_smooth_scrolling_enabled();

                    /* Calculate scroll amount */
                    float scroll_amount = 0.0f;
                    if (smooth_scrolling && event.wheel.preciseY != 0.0f) {
                        /* Use smooth scrolling for high-resolution trackpads */
                        scroll_amount = event.wheel.preciseY * (float)lines_per_click;
                    } else {
                        /* Use discrete clicks */
                        scroll_amount = (float)event.wheel.y * (float)lines_per_click;
                    }

                    /* Only scroll if there's actual movement */
                    if (scroll_amount != 0.0f) {
                        int scroll_lines = (int)scroll_amount;
                        if (scroll_lines == 0) {
                            /* For very small smooth scroll amounts, use at least 1 line */
                            scroll_lines = scroll_amount > 0.0f ? 1 : -1;
                        }

                        /* Check if we can scroll in the requested direction */
                        int viewport_offset = terminal_get_viewport_offset(term);
                        int scrollback_size = terminal_get_scrollback_size(term);

                        if (scroll_lines > 0) {
                            /* Scroll up (view older content) */
                            if (viewport_offset < scrollback_size) {
                                terminal_scroll_up(term, scroll_lines);
                            }
                        } else {
                            /* Scroll down (view newer content) */
                            if (viewport_offset > 0) {
                                terminal_scroll_down(term, -scroll_lines);
                            }
                        }
                    }
                }
                break;
            }
            }
        }

        /* Handle scroll-to-bottom on dock content change (unified handling) */
        if (dock.user_input_received) {
            dock.user_input_received = 0;
            if (lisp_x_get_scroll_to_bottom_on_user_input()) {
                terminal_scroll_to_bottom(term);
            }
        }

        /* Run timer callbacks */
        lisp_x_run_timers();

        /* Read from socket (if connected) */
        if (connected_mode) {
            /* Use select() to check if data is available (avoid unnecessary recv() calls) */
            int sock = telnet_get_socket(telnet);
            if (sock >= 0) {
                fd_set readfds, exceptfds;
                struct timeval tv = {0, 0}; /* Non-blocking check */
                FD_ZERO(&readfds);
                FD_ZERO(&exceptfds);
#ifdef _WIN32
                FD_SET((SOCKET)sock, &readfds);
                FD_SET((SOCKET)sock, &exceptfds);
                int ready = select(0, &readfds, NULL, &exceptfds, &tv); /* First param ignored on Windows */
                int has_read = ready > 0 && FD_ISSET((SOCKET)sock, &readfds);
                int has_except = ready > 0 && FD_ISSET((SOCKET)sock, &exceptfds);
#else
                FD_SET(sock, &readfds);
                FD_SET(sock, &exceptfds);
                int ready = select(sock + 1, &readfds, NULL, &exceptfds, &tv);
                int has_read = ready > 0 && FD_ISSET(sock, &readfds);
                int has_except = ready > 0 && FD_ISSET(sock, &exceptfds);
#endif
                /* Check for socket exceptions (out-of-band data or errors) */
                if (has_except) {
                    fprintf(stderr, "select(): socket exception detected (OOB data or error)\n");
                }
                /* Call recv() when readable OR exception - exception may indicate connection closure */
                if (has_read || has_except) {
                    /* Data is available, read it */
                    char recv_buf[4096];
                    uint64_t t0 = 0, t1 = 0, t2 = 0, t3 = 0, t4 = 0;
                    if (profile_mode)
                        t0 = get_time_ns();
                    int received = telnet_receive(telnet, recv_buf, sizeof(recv_buf) - 1);
                    if (profile_mode)
                        t1 = get_time_ns();
                    if (received > 0) {
                        /* Call telnet-input-hook with received data (stripped of ANSI codes) */
                        lisp_x_call_telnet_input_hook(recv_buf, received);
                        if (profile_mode)
                            t2 = get_time_ns();
                        /* Call telnet-input-filter-hook to transform data before displaying in terminal */
                        size_t filtered_len = 0;
                        const char *filtered_data =
                            lisp_x_call_telnet_input_filter_hook(recv_buf, received, &filtered_len);
                        if (profile_mode)
                            t3 = get_time_ns();
                        /* Feed filtered data to terminal */
                        terminal_feed_data(term, filtered_data, filtered_len);
                        if (profile_mode) {
                            t4 = get_time_ns();
                            /* Accumulate timing stats */
                            profile_stats.telnet_receive_ns += (t1 - t0);
                            profile_stats.telnet_input_hook_ns += (t2 - t1);
                            profile_stats.telnet_input_filter_hook_ns += (t3 - t2);
                            profile_stats.terminal_feed_data_ns += (t4 - t3);
                            profile_stats.recv_count++;
                        }

                        /* Auto-scroll to bottom unless user has scrolled back */
                        if (!terminal_is_scroll_locked(term)) {
                            terminal_scroll_to_bottom(term);
                        }
                    } else if (received < 0) {
                        /* Connection closed or error (telnet_receive returns -1 for both) */
                        /* Note: telnet_receive() already called telnet_disconnect() internally */
                        connected_mode = 0;
                        terminal_feed_data(term, "\r\n*** Connection closed ***\r\n",
                                           strlen("\r\n*** Connection closed ***\r\n"));
                        dock_request_redraw(&dock); /* Trigger color update */
                        /* Exit if --exit-on-disconnect was specified */
                        if (exit_on_disconnect) {
                            running = 0;
                        }
                    }
                }
            }
        }

        /* Handle input area layout and sizing BEFORE rendering */
        int term_rows, term_cols;
        terminal_get_size(term, &term_rows, &term_cols);

        /* Recalculate layout if needed (text changed, newline inserted, etc.) */
        if (dock.needs_layout_recalc) {
            dock_recalculate_layout(&dock, term_cols);
        }

        /* Check if input area height changed and resize terminal if needed */
        /* This must happen BEFORE rendering so vterm has correct scrolling region */
        int current_visible_rows = dock_get_text_rows(&dock);
        if (current_visible_rows != prev_input_visible_rows) {
            prev_input_visible_rows = current_visible_rows;

            /* Recalculate terminal size with new input area height */
            int window_width, window_height;
            window_get_size(win, &window_width, &window_height);
            int new_rows, new_cols;
            calculate_terminal_size(window_width, window_height, cell_w, cell_h, &dock, &new_rows, &new_cols);

            /* Resize terminal to accommodate new input area height */
            terminal_resize(term, new_rows, new_cols, current_visible_rows);
            telnet_set_terminal_size(telnet, new_cols, new_rows);

            /* Update term_rows/cols for rendering below */
            terminal_get_size(term, &term_rows, &term_cols);
        }

#if HAVE_RLOTTIE
        /* Update animation divider mode indicator BEFORE input area render */
        /* This ensures the indicator is visible on the first frame */
        static int animation_mode_was_playing = 0; /* Track for divider mode */
        int animation_just_finished = 0;
        Animation *active_anim = lisp_x_get_active_animation();
        if (active_anim && animation_is_loaded(active_anim)) {
            int is_playing = animation_is_playing(active_anim);

            /* Auto-manage animation divider mode indicator */
            if (is_playing && !animation_mode_was_playing) {
                /* Animation started - show play button */
                lisp_x_set_divider_mode("animation", "\xE2\x96\xB6\xEF\xB8\x8F", 90); /* ▶️ U+25B6 U+FE0F */
                dock_request_redraw(&dock);                                           /* Redraw divider with new mode */
            }

            if (is_playing) {
                animation_update(active_anim, 16.0f); /* ~60fps = 16ms per frame */
            }
            /* Clear active animation when it finishes (non-looping animation stopped) */
            if (animation_mode_was_playing && !animation_is_playing(active_anim)) {
                lisp_x_clear_active_animation();
                active_anim = NULL;
                animation_just_finished = 1; /* Force redraw to clear last frame */
                /* Animation stopped - remove play button */
                lisp_x_remove_divider_mode("animation");
                dock_request_redraw(&dock); /* Redraw divider without mode */
            }
            animation_mode_was_playing = animation_is_playing(active_anim);
        } else {
            /* No active animation - ensure mode is removed */
            if (animation_mode_was_playing) {
                lisp_x_remove_divider_mode("animation");
                dock_request_redraw(&dock); /* Redraw divider without mode */
                animation_mode_was_playing = 0;
            }
        }
        /* Update renderer's animation pointer */
        renderer_set_animation(active_anim);
#endif

        /* Render input area to vterm if it needs redraw */
        /* This must happen AFTER terminal resize so cursor is positioned correctly */
        if (dock_needs_redraw(&dock)) {
            terminal_render_dock(term, &dock, term_cols);
            dock_mark_drawn(&dock);
            /* Input area updates vterm which triggers terminal_needs_redraw */
        }

        /* Render if needed */
        int window_width, window_height;
        window_get_size(win, &window_width, &window_height);
        int needs_render = 0;

#if HAVE_RLOTTIE
        /* Force redraw when animation is playing or just finished */
        Animation *anim_for_redraw = lisp_x_get_active_animation();
        int animation_needs_redraw =
            animation_just_finished ||
            (anim_for_redraw && animation_is_loaded(anim_for_redraw) && animation_is_playing(anim_for_redraw));
#else
        int animation_needs_redraw = 0;
#endif

        if (terminal_needs_redraw(term) || terminal_selection.active || animation_needs_redraw) {
            /* Clear back buffer before rendering */
            int bg_r, bg_g, bg_b;
            lisp_x_get_terminal_bg_color(&bg_r, &bg_g, &bg_b);
            SDL_SetRenderDrawColor(renderer, bg_r, bg_g, bg_b, 255);
            SDL_RenderClear(renderer);

            char title[256];
            snprintf(title, sizeof(title), "Telnet: %s:%d", hostname, port);
            int term_rows, term_cols;
            terminal_get_size(term, &term_rows, &term_cols);
            renderer_render(rend, term, title, terminal_selection.active, terminal_selection.start_row,
                            terminal_selection.start_col, terminal_selection.start_viewport_offset,
                            terminal_selection.start_scrollback_size, terminal_selection.end_row,
                            terminal_selection.end_col, terminal_selection.end_viewport_offset,
                            terminal_selection.end_scrollback_size, &dock, term_cols);
            terminal_mark_drawn(term);
            needs_render = 1;
        }

        /* Present frame if anything was rendered */
        if (needs_render) {
            SDL_Renderer *present_renderer = window_get_sdl_renderer(win);
            SDL_RenderPresent(present_renderer);

            /* Exit after first render if debug mode enabled */
            if (debug_exit) {
                fprintf(stderr, "DEBUG: First render complete, exiting\n");
                fflush(stderr);
                running = 0;
            }
        }

        /* NOTE: No SDL_Delay() here - select() handles waiting now */
    }

    /* Print profile reports if profiling was enabled */
    if (profile_mode) {
        printf("\n");
        profile_stats_print();
        printf("\n");
        lisp_x_profile_report();
        fflush(stdout);
    }

    /* Cleanup */
    /* Animation objects are GC-managed and cleaned up automatically */
    telnet_destroy(telnet);
    terminal_destroy(term);
    renderer_destroy(rend);
    glyph_cache_destroy(glyph_cache);
    window_destroy(win);

    return 0;
}
