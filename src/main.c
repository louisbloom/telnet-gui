/* Main entry point for Telnet GUI */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <stdbool.h>
#include <errno.h>

#ifdef _WIN32
#include <winsock2.h>
#include <windows.h>
#include <io.h>
#include <fcntl.h>
#else
#include <sys/select.h>
#include <sys/time.h>
#endif

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

#include "telnet.h"
#include "terminal.h"
#include "window.h"
#include "renderer.h"
#include "glyph_cache.h"
#include "input.h"
#include "input_area.h"
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
#include "../../telnet-lisp/include/lisp.h"
#include "../../telnet-lisp/include/file_utils.h"

/* Padding around terminal area (including input area) - must match renderer.c */
#define PADDING_X 8
#define PADDING_Y 8

static int running = 1;

/* Input area */
static InputArea input_area;

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
                /* Convert cell characters to UTF-8 */
                if (cell.chars[0]) {
                    char utf8[5];
                    int len = 0;
                    uint32_t codepoint = cell.chars[0];

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
static int send_to_telnet(Telnet *telnet, Terminal *term, InputArea *input_area, int *connected_mode, const char *data,
                          size_t len, int append_crlf) {
    if (!telnet || !term || !input_area || !connected_mode) {
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
        input_area_request_redraw(input_area); /* Trigger color update */
        return -1;
    }

    return 0;
}

/* Calculate terminal size (rows, cols) based on window dimensions */
static void calculate_terminal_size(int window_width, int window_height, int cell_w, int cell_h, InputArea *input_area,
                                    int *rows, int *cols) {
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
    /* Subtract rows for top divider, bottom divider, and dynamic input area height */
    int input_height_rows =
        2 + input_area_get_visible_rows(input_area); /* Top divider + bottom divider + visible input rows */
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
    /* Linux: Try common monospace fonts */
    const char *fonts[] = {"/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
                           "/usr/share/fonts/TTF/DejaVuSansMono.ttf",
                           "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf",
                           "/usr/share/fonts/TTF/LiberationMono-Regular.ttf",
                           "/usr/share/fonts/truetype/liberation-mono/LiberationMono-Regular.ttf",
                           "/usr/share/fonts/truetype/courier/Courier New.ttf",
                           "/usr/share/fonts/TTF/Courier New.ttf",
                           NULL};
    const char *names[] = {"DejaVu Sans Mono", "DejaVu Sans Mono", "Liberation Mono", "Liberation Mono",
                           "Liberation Mono",  "Courier New",      "Courier New",     NULL};

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

static void print_help(const char *program_name) {
#ifdef _WIN32
    /* Disable buffering for immediate output (console subsystem already has stdout/stderr) */
    setvbuf(stdout, NULL, _IONBF, 0);
    setvbuf(stderr, NULL, _IONBF, 0);
#endif
    printf("Usage: %s [OPTIONS] [hostname] [port]\n", program_name);
    printf("\n");
    printf("Options:\n");
    printf("  -h, --help              Show this help message and exit\n");
    printf("\n");
    printf("  Font Options:\n");
    printf("    -f, --font-size SIZE   Set font size in points (default: 12)\n");
    printf("    -F<letter>             Select font (default: s = system font):\n");
    printf("                             s = System monospace font (best for platform),\n");
    printf("                             m = Cascadia Mono, l = Cascadia Code (ligatures),\n");
    printf("                             i = Inconsolata, p = IBM Plex Mono,\n");
    printf("                             d = DejaVu Sans Mono, c = Courier Prime\n");
    printf("    --font <name>          Select font by name:\n");
    printf("                             system, cascadia, cascadiacode, inconsolata, plex,\n");
    printf("                             dejavu, courier\n");
    printf("    -H, --hinting MODE     Set font hinting mode (default: none)\n");
    printf("                            MODE can be: none, light, normal, mono\n");
    printf("    -a, --antialiasing MODE Set anti-aliasing mode (default: nearest)\n");
    printf("                            MODE can be: nearest, linear\n");
    printf("\n");
    printf("  Terminal Options:\n");
    printf("    -g, --geometry GEOM     Set terminal size in characters\n");
    printf("                            GEOM format: COLSxROWS (e.g., 80x40)\n");
    printf("                            Default: 80x40\n");
    printf("\n");
    printf("  Other Options:\n");
    printf("    -l, --lisp-file FILE   Load and evaluate Lisp file on startup\n");
    printf("                            Can be specified multiple times (loads in order)\n");
    printf("                            Used to customize completion hooks and scroll settings\n");
    printf("    -L, --line-height HEIGHT Set line height multiplier (default: 1.2)\n");
    printf("                            HEIGHT can be 0.5 to 3.0 (e.g., 1.5 for 50%% more spacing)\n");
    printf("    --debug-exit           Exit after initialization (for debug output)\n");
    printf("\n");
    printf("Arguments:\n");
    printf("  hostname                 Telnet server hostname or IP address (optional)\n");
    printf("  port                     Telnet server port number (optional)\n");
    printf("\n");
    printf("If hostname and port are not provided, starts in unconnected mode.\n");
    printf("\n");
    printf("Examples:\n");
    printf("  %s\n", program_name);
    printf("      Start in unconnected mode\n");
    printf("  %s telnet-server 4449\n", program_name);
    printf("      Connect to telnet-server on port 4449\n");
    printf("  %s -f 20 telnet-server 4449\n", program_name);
    printf("      Connect with 20pt font size\n");
    printf("  %s -Fi telnet-server 4449\n", program_name);
    printf("      Connect using Inconsolata font\n");
    printf("  %s -Fp telnet-server 4449\n", program_name);
    printf("      Connect using IBM Plex Mono font\n");
    printf("  %s --font system telnet-server 4449\n", program_name);
    printf("      Connect using system monospace font (default)\n");
    printf("  %s -g 100x40 telnet-server 4449\n", program_name);
    printf("      Connect with 100x40 terminal size\n");
    printf("  %s -l completion.lisp telnet-server 4449\n", program_name);
    printf("      Connect and load Lisp configuration file\n");
    printf("  %s --line-height 1.5 telnet-server 4449\n", program_name);
    printf("      Connect with 1.5x line height (50%% more spacing)\n");
    printf("  %s -l tintin.lisp -l myconfig.lisp server 4449\n", program_name);
    printf("      Load multiple Lisp files in order\n");
    printf("\n");
    fflush(stdout);
}

int main(int argc, char **argv) {
    /* Default settings */
    int hinting_mode = TTF_HINTING_NONE;             /* Default: no hinting for crisp rendering */
    SDL_ScaleMode scale_mode = SDL_ScaleModeNearest; /* Default: nearest (pixel-perfect) scaling */
    const char *hostname = NULL;
    int port = 0;
    const char *lisp_files[16]; /* Support up to 16 -l flags */
    int lisp_file_count = 0;
    char font_choice = 's';       /* Font selection: s=System, m=Cascadia Mono, l=Cascadia Code, i=Inconsolata, p=Plex,
                                     d=DejaVu, c=Courier */
    int font_size = 12;           /* Default font size */
    int terminal_cols = 80;       /* Default terminal columns */
    int terminal_rows = 40;       /* Default terminal rows */
    int debug_exit = 0;           /* Exit after initialization for debug output */
    float cli_line_height = 0.0f; /* CLI line height (0.0 means not set, use default) */

    /* Parse command-line arguments */
    int arg_idx = 1;
    while (arg_idx < argc) {
        if (strcmp(argv[arg_idx], "-h") == 0 || strcmp(argv[arg_idx], "--help") == 0) {
            print_help(argv[0]);
            return 0;
        } else if (strcmp(argv[arg_idx], "-H") == 0 || strcmp(argv[arg_idx], "--hinting") == 0) {
            if (arg_idx + 1 >= argc) {
                fprintf(stderr, "Error: --hinting requires a mode (none, light, normal, mono)\n");
                return 1;
            }
            arg_idx++;
            if (strcmp(argv[arg_idx], "none") == 0) {
                hinting_mode = TTF_HINTING_NONE;
            } else if (strcmp(argv[arg_idx], "light") == 0) {
                hinting_mode = TTF_HINTING_LIGHT;
            } else if (strcmp(argv[arg_idx], "normal") == 0) {
                hinting_mode = TTF_HINTING_NORMAL;
            } else if (strcmp(argv[arg_idx], "mono") == 0) {
                hinting_mode = TTF_HINTING_MONO;
            } else {
                fprintf(stderr, "Error: Invalid hinting mode '%s'. Use: none, light, normal, mono\n", argv[arg_idx]);
                return 1;
            }
        } else if (strcmp(argv[arg_idx], "-a") == 0 || strcmp(argv[arg_idx], "--antialiasing") == 0) {
            if (arg_idx + 1 >= argc) {
                fprintf(stderr, "Error: --antialiasing requires a mode (nearest, linear)\n");
                return 1;
            }
            arg_idx++;
            if (strcmp(argv[arg_idx], "nearest") == 0) {
                scale_mode = SDL_ScaleModeNearest;
            } else if (strcmp(argv[arg_idx], "linear") == 0) {
                scale_mode = SDL_ScaleModeLinear;
            } else {
                fprintf(stderr, "Error: Invalid antialiasing mode '%s'. Use: nearest, linear\n", argv[arg_idx]);
                return 1;
            }
        } else if (strcmp(argv[arg_idx], "-f") == 0 || strcmp(argv[arg_idx], "--font-size") == 0) {
            if (arg_idx + 1 >= argc) {
                fprintf(stderr, "Error: --font-size requires a size (positive integer)\n");
                return 1;
            }
            arg_idx++;
            font_size = atoi(argv[arg_idx]);
            if (font_size <= 0 || font_size > 100) {
                fprintf(stderr, "Error: Invalid font size '%s'. Must be between 1 and 100\n", argv[arg_idx]);
                return 1;
            }
        } else if (strncmp(argv[arg_idx], "-F", 2) == 0 && strlen(argv[arg_idx]) == 3) {
            font_choice = argv[arg_idx][2];
            if (font_choice != 's' && font_choice != 'm' && font_choice != 'l' && font_choice != 'i' &&
                font_choice != 'p' && font_choice != 'd' && font_choice != 'c') {
                fprintf(stderr, "Error: Invalid font flag -F%c. Use: s, m, l, i, p, d, or c\n", font_choice);
                return 1;
            }
        } else if (strcmp(argv[arg_idx], "--font") == 0) {
            if (arg_idx + 1 >= argc) {
                fprintf(stderr, "Error: --font requires a font name\n");
                return 1;
            }
            arg_idx++;
            if (strcmp(argv[arg_idx], "system") == 0)
                font_choice = 's';
            else if (strcmp(argv[arg_idx], "cascadia") == 0)
                font_choice = 'm';
            else if (strcmp(argv[arg_idx], "cascadiacode") == 0)
                font_choice = 'l';
            else if (strcmp(argv[arg_idx], "inconsolata") == 0)
                font_choice = 'i';
            else if (strcmp(argv[arg_idx], "plex") == 0)
                font_choice = 'p';
            else if (strcmp(argv[arg_idx], "dejavu") == 0)
                font_choice = 'd';
            else if (strcmp(argv[arg_idx], "courier") == 0)
                font_choice = 'c';
            else {
                fprintf(stderr, "Error: Unknown font '%s'\n", argv[arg_idx]);
                return 1;
            }
        } else if (strcmp(argv[arg_idx], "-g") == 0 || strcmp(argv[arg_idx], "--geometry") == 0) {
            if (arg_idx + 1 >= argc) {
                fprintf(stderr, "Error: --geometry requires a geometry string (COLSxROWS, e.g., 80x40)\n");
                return 1;
            }
            arg_idx++;
            /* Parse geometry string: COLSxROWS (e.g., 80x40) */
            const char *geom = argv[arg_idx];
            char *geom_copy = strdup(geom);
            if (!geom_copy) {
                fprintf(stderr, "Error: Out of memory\n");
                return 1;
            }
            char *x_pos = strchr(geom_copy, 'x');
            if (!x_pos) {
                fprintf(stderr, "Error: Invalid geometry format '%s'. Use COLSxROWS (e.g., 80x40)\n", geom);
                free(geom_copy);
                return 1;
            }
            *x_pos = '\0';
            terminal_cols = atoi(geom_copy);
            terminal_rows = atoi(x_pos + 1);
            free(geom_copy);
            if (terminal_cols <= 0 || terminal_rows <= 0) {
                fprintf(stderr, "Error: Invalid geometry dimensions. Columns and rows must be positive\n");
                return 1;
            }
        } else if (strcmp(argv[arg_idx], "-l") == 0 || strcmp(argv[arg_idx], "--lisp-file") == 0) {
            if (arg_idx + 1 >= argc) {
                fprintf(stderr, "Error: --lisp-file requires a file path\n");
                return 1;
            }
            arg_idx++;
            if (lisp_file_count < 16) {
                lisp_files[lisp_file_count++] = argv[arg_idx];
            } else {
                fprintf(stderr, "Error: Too many -l flags (maximum 16)\n");
                return 1;
            }
        } else if (strcmp(argv[arg_idx], "-L") == 0 || strcmp(argv[arg_idx], "--line-height") == 0) {
            if (arg_idx + 1 >= argc) {
                fprintf(stderr, "Error: --line-height requires a value (0.5 to 3.0)\n");
                return 1;
            }
            arg_idx++;
            cli_line_height = (float)atof(argv[arg_idx]);
            if (cli_line_height < 0.5f || cli_line_height > 3.0f) {
                fprintf(stderr, "Error: Invalid line height '%s'. Must be between 0.5 and 3.0\n", argv[arg_idx]);
                return 1;
            }
        } else if (strcmp(argv[arg_idx], "--debug-exit") == 0) {
            debug_exit = 1;
        } else {
            /* Positional arguments: hostname and port */
            if (hostname == NULL) {
                hostname = argv[arg_idx];
            } else if (port == 0) {
                port = atoi(argv[arg_idx]);
                if (port <= 0 || port > 65535) {
                    fprintf(stderr, "Error: Invalid port number '%s'. Must be between 1 and 65535\n", argv[arg_idx]);
                    return 1;
                }
            } else {
                fprintf(stderr, "Error: Unexpected argument '%s'\n", argv[arg_idx]);
                fprintf(stderr, "Use --help for usage information\n");
                return 1;
            }
        }
        arg_idx++;
    }

    /* Connection mode: if hostname and port provided, start in connected mode */
    int connected_mode = (hostname != NULL && port != 0);
    if (hostname != NULL && port == 0) {
        fprintf(stderr, "Error: If hostname is provided, port must also be provided\n");
        fprintf(stderr, "Use --help for usage information\n");
        return 1;
    }
    if (hostname == NULL && port != 0) {
        fprintf(stderr, "Error: If port is provided, hostname must also be provided\n");
        fprintf(stderr, "Use --help for usage information\n");
        return 1;
    }

    /* Set locale for UTF-8 support */
    setlocale(LC_ALL, "");

    /* Initialize Lisp bridge (loads bootstrap file) */
    if (lisp_x_init() < 0) {
        fprintf(stderr, "Failed to initialize Lisp bridge\n");
        return 1;
    }

    /* Override line height from CLI if provided (before window creation) */
    if (cli_line_height > 0.0f) {
        lisp_x_set_terminal_line_height(cli_line_height);
    }

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

#if HAVE_SDL_TTF_DPI
    fprintf(stderr, "Using TTF_OpenFontDPI for DPI-aware font rendering\n");
#else
    fprintf(stderr, "SDL_ttf DPI support not available - font sizes may not match system expectations\n");
#endif

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
            font_filename = "CascadiaMono-Regular.ttf";
            font_name = "Cascadia Mono";
            break;
        case 'l':
            font_filename = "CascadiaCode-Regular.ttf";
            font_name = "Cascadia Code";
            break;
        case 'i':
            font_filename = "Inconsolata-Regular.ttf";
            font_name = "Inconsolata";
            break;
        case 'p':
            font_filename = "IBMPlexMono-Regular.ttf";
            font_name = "IBM Plex Mono";
            break;
        case 'd':
            font_filename = "DejaVuSansMono.ttf";
            font_name = "DejaVu Sans Mono";
            break;
        case 'c':
            font_filename = "CourierPrime-Regular.ttf";
            font_name = "Courier Prime";
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
        glyph_cache = glyph_cache_create(temp_renderer, font_paths[i], font_name, font_size, hinting_mode, scale_mode,
                                         (int)hdpi, (int)vdpi);
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
    int separator_and_input_height = 3 * effective_cell_h; /* Top divider + bottom divider + input row */
    int precise_width = terminal_cols * cell_w + 2 * PADDING_X;
    int precise_height = terminal_rows * effective_cell_h + separator_and_input_height + 2 * PADDING_Y;

    /* Clean up hidden window and temporary renderer */
    SDL_DestroyRenderer(temp_renderer);
    SDL_DestroyWindow(hidden_window);

    /* Create the real window with the exact calculated size */
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
            glyph_cache = glyph_cache_create(renderer, font_paths[i], font_name, font_size, hinting_mode, scale_mode,
                                             (int)hdpi, (int)vdpi);
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
    input_area_init(&input_area);

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
    lisp_x_register_input_area(&input_area);

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
    calculate_terminal_size(actual_width, actual_height, cell_w, cell_h, &input_area, &initial_rows, &initial_cols);
    int input_visible_rows = input_area_get_visible_rows(&input_area);
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
    terminal_render_input_area(term, &input_area, initial_cols);

    /* Render initial terminal state */
    char title[256];
    snprintf(title, sizeof(title), "Telnet: %s:%d", hostname ? hostname : "", port);
    renderer_render(rend, term, title, 0, 0, 0, 0, 0, 0, 0, 0, 0, &input_area, initial_cols);

    /* Present the initial frame immediately */
    SDL_RenderPresent(renderer);

    /* Main loop */
    SDL_Event event;
    int mouse_x = 0, mouse_y = 0;

    while (running && !quit_requested) {
        /* Poll events */
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
                    input_area.needs_layout_recalc = 1;
                    input_area_recalculate_layout(&input_area, new_cols);

                    /* Step 3: Calculate terminal rows using updated input area height */
                    int available_height = new_height - 2 * PADDING_Y;
                    float line_height = lisp_x_get_terminal_line_height();
                    int effective_cell_h = (int)(cell_h * line_height);
                    int input_height_rows =
                        2 + input_area_get_visible_rows(
                                &input_area); /* Top divider + bottom divider + visible input rows */
                    int new_rows = (available_height / effective_cell_h) - input_height_rows;
                    if (new_rows < 1)
                        new_rows = 1; /* Minimum: 1 scrolling row */

                    /* Step 4: Resize terminal and update */
                    int input_visible_rows = input_area_get_visible_rows(&input_area);

                    /* Save cursor position before resize corrupts it */
                    int saved_cursor_row, saved_cursor_col, saved_cursor_visible;
                    terminal_get_cursor_info(term, &saved_cursor_row, &saved_cursor_col, &saved_cursor_visible);

                    terminal_resize(term, new_rows, new_cols, input_visible_rows);

                    /* Re-render input area immediately to update divider position */
                    terminal_render_input_area(term, &input_area, new_cols);

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
                    int terminal_height = (new_rows + 2 + input_visible_rows) * effective_cell_h + 2 * PADDING_Y;

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
                    input_area_request_redraw(&input_area);

                    /* Render immediately to clear artifacts */
                    char title[256];
                    snprintf(title, sizeof(title), "Telnet: %s:%d", hostname ? hostname : "", port);
                    renderer_render(rend, term, title, terminal_selection.active, terminal_selection.start_row,
                                    terminal_selection.start_col, terminal_selection.start_viewport_offset,
                                    terminal_selection.start_scrollback_size, terminal_selection.end_row,
                                    terminal_selection.end_col, terminal_selection.end_viewport_offset,
                                    terminal_selection.end_scrollback_size, &input_area, new_cols);
                    terminal_mark_drawn(term);
                    input_area_mark_drawn(&input_area);

                    /* Present the frame */
                    SDL_RenderPresent(renderer);
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
                /* Get window size to check if mouse is in input area */
                int window_width, window_height;
                window_get_size(win, &window_width, &window_height);

                /* Get line height multiplier and calculate effective cell height */
                float line_height = lisp_x_get_terminal_line_height();
                int effective_cell_h = (int)(cell_h * line_height);

                /* Calculate input area height: top divider + bottom divider + visible input rows */
                int input_area_height = (2 + input_area_get_visible_rows(&input_area)) * effective_cell_h;

                /* Handle clicks in terminal area (not in input area or padding) */
                /* Check if click is within terminal area (excluding padding) */
                if (mouse_x >= PADDING_X && mouse_x < window_width - PADDING_X && mouse_y >= PADDING_Y &&
                    mouse_y < window_height - PADDING_Y) {
                    /* Check if click is in terminal scrolling area (not input area) */
                    if (mouse_y < window_height - input_area_height - PADDING_Y) {
                        /* Start selection only if no selection was cleared */
                        if (event.button.button == SDL_BUTTON_LEFT && !had_selection) {
                            /* Convert mouse coordinates to terminal cell coordinates, subtracting padding */
                            int term_row = (mouse_y - PADDING_Y) / effective_cell_h;
                            int term_col = (mouse_x - PADDING_X) / cell_w;
                            /* Start selection and freeze viewport */
                            start_terminal_selection(term, term_row, term_col);
                        }
                    }
                }
                /* Mouse clicks in input area are ignored (input area is always active) */
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
                        input_area_undo(&input_area);
                        break;
                    } else if ((mod & KMOD_ALT) && (mod & KMOD_SHIFT)) {
                        /* Captured Alt + Shift + _ (physically) */
                        input_area_redo(&input_area);
                        break;
                    }
                }

                switch (scancode) {
                case SDL_SCANCODE_RETURN:
                case SDL_SCANCODE_KP_ENTER: {
                    /* Send input area text to terminal and telnet */
                    int length = input_area_get_length(&input_area);
                    if (length > 0) {
                        const char *text = input_area_get_text(&input_area);
                        int cursor_pos = input_area_get_cursor_pos(&input_area);

                        /* Echo raw input FIRST for non-eval mode (eval mode has its own echo with >) */
                        if (input_area_get_mode(&input_area) != INPUT_AREA_MODE_EVAL && input_area.echo_buf) {
                            char color_buf[32];
                            int echo_r, echo_g, echo_b;
                            lisp_x_get_user_input_echo_color(&echo_r, &echo_g, &echo_b);
                            dynamic_buffer_clear(input_area.echo_buf);
                            ansi_format_fg_color_rgb(color_buf, sizeof(color_buf), echo_r, echo_g, echo_b);
                            dynamic_buffer_append_str(input_area.echo_buf, color_buf);
                            if (dynamic_buffer_append_printf(input_area.echo_buf, "%s\r\n", text) == 0) {
                                /* Reset color after text */
                                dynamic_buffer_append_str(input_area.echo_buf, ANSI_SGR_RESET);
                                terminal_feed_data(term, dynamic_buffer_data(input_area.echo_buf),
                                                   dynamic_buffer_len(input_area.echo_buf));
                            }
                        }

                        /* Check if this is a special command starting with ':' */
                        if (text[0] == ':') {
                            /* Process command */
                            process_command(text, telnet, term, &connected_mode, &input_area, &quit_requested);

                            /* Scroll to bottom on command */
                            if (lisp_x_get_scroll_to_bottom_on_user_input()) {
                                terminal_scroll_to_bottom(term);
                            }

                            /* Add to history and clear input area */
                            input_area_history_add(&input_area);
                            input_area_clear(&input_area);
                        } else if (input_area_get_mode(&input_area) == INPUT_AREA_MODE_EVAL) {
                            /* Eval mode - evaluate Lisp expression using shared eval logic */
                            if (input_area.eval_buf && lisp_x_eval_and_echo(text, input_area.eval_buf) == 0) {
                                terminal_feed_data(term, dynamic_buffer_data(input_area.eval_buf),
                                                   dynamic_buffer_len(input_area.eval_buf));
                            }

                            /* Scroll to bottom */
                            if (lisp_x_get_scroll_to_bottom_on_user_input()) {
                                terminal_scroll_to_bottom(term);
                            }

                            /* Add to history and clear input area */
                            input_area_history_add(&input_area);
                            input_area_clear(&input_area);
                        } else {
                            /* Normal text - call user-input-hook to transform text before sending */
                            const char *transformed_text = lisp_x_call_user_input_hook(text, cursor_pos);
                            int transformed_length = strlen(transformed_text);

                            /* Hook contract: non-string or empty string = hook handled everything */
                            /* Proper way: return nil to indicate hook handled echo/send */
                            /* If hook returns empty string, it means hook handled echo/send - don't send again */

                            if (transformed_length > 0) {
                                /* DON'T echo again - raw input was already echoed above */
                                /* Scroll to bottom on user input if configured */
                                if (lisp_x_get_scroll_to_bottom_on_user_input()) {
                                    terminal_scroll_to_bottom(term);
                                }

                                /* Send transformed text to telnet (unified function handles LF->CRLF, CRLF appending,
                                 * and errors) */
                                send_to_telnet(telnet, term, &input_area, &connected_mode, transformed_text,
                                               transformed_length, 1); /* append_crlf = 1 */
                            } else {
                                /* Empty string from hook - scroll to bottom only */
                                if (lisp_x_get_scroll_to_bottom_on_user_input()) {
                                    terminal_scroll_to_bottom(term);
                                }
                            }

                            /* Add to history and clear input area */
                            input_area_history_add(&input_area);
                            input_area_clear(&input_area);
                        }
                    } else {
                        /* Even if input is empty, send CRLF for newline (unified function handles errors) */
                        send_to_telnet(telnet, term, &input_area, &connected_mode, "", 0, 1); /* append_crlf = 1 */
                        /* Echo newline to terminal (vterm_feed_data will normalize LF to CRLF) */
                        terminal_feed_data(term, "\n", 1);

                        /* Scroll to bottom on user input if configured */
                        if (lisp_x_get_scroll_to_bottom_on_user_input()) {
                            terminal_scroll_to_bottom(term);
                        }
                    }
                    break;
                }
                case SDL_SCANCODE_BACKSPACE: {
                    input_area_backspace(&input_area);
                    break;
                }
                case SDL_SCANCODE_DELETE: {
                    input_area_delete_char(&input_area);
                    break;
                }
                case SDL_SCANCODE_LEFT: {
                    /* Start selection if Shift is pressed and no selection */
                    if ((mod & KMOD_SHIFT) && !input_area_has_selection(&input_area)) {
                        input_area_start_selection(&input_area);
                    } else if (!(mod & KMOD_SHIFT)) {
                        input_area_clear_selection(&input_area);
                    }

                    if (mod & KMOD_CTRL) {
                        input_area_move_cursor_word_left(&input_area);
                    } else {
                        input_area_move_cursor_left(&input_area);
                    }
                    break;
                }
                case SDL_SCANCODE_RIGHT: {
                    /* Start selection if Shift is pressed and no selection */
                    if ((mod & KMOD_SHIFT) && !input_area_has_selection(&input_area)) {
                        input_area_start_selection(&input_area);
                    } else if (!(mod & KMOD_SHIFT)) {
                        input_area_clear_selection(&input_area);
                    }

                    if (mod & KMOD_CTRL) {
                        input_area_move_cursor_word_right(&input_area);
                    } else {
                        input_area_move_cursor_right(&input_area);
                    }
                    break;
                }
                case SDL_SCANCODE_UP: {
                    /* Navigate lines in multi-line input, or history if at first line */
                    int rows, cols;
                    terminal_get_size(term, &rows, &cols);
                    if (!input_area_is_at_first_visual_line(&input_area, cols)) {
                        input_area_move_cursor_up_line(&input_area, cols);
                    } else {
                        input_area_history_prev(&input_area);
                    }
                    break;
                }
                case SDL_SCANCODE_DOWN: {
                    /* Navigate lines in multi-line input, or history if at last line */
                    int rows, cols;
                    terminal_get_size(term, &rows, &cols);
                    if (!input_area_is_at_last_visual_line(&input_area, cols)) {
                        input_area_move_cursor_down_line(&input_area, cols);
                    } else {
                        input_area_history_next(&input_area);
                    }
                    break;
                }
                case SDL_SCANCODE_HOME: {
                    /* Start selection if Shift is pressed and no selection */
                    if ((mod & KMOD_SHIFT) && !input_area_has_selection(&input_area)) {
                        input_area_start_selection(&input_area);
                    } else if (!(mod & KMOD_SHIFT)) {
                        input_area_clear_selection(&input_area);
                    }

                    input_area_move_cursor_home(&input_area);
                    break;
                }
                case SDL_SCANCODE_END: {
                    /* Start selection if Shift is pressed and no selection */
                    if ((mod & KMOD_SHIFT) && !input_area_has_selection(&input_area)) {
                        input_area_start_selection(&input_area);
                    } else if (!(mod & KMOD_SHIFT)) {
                        input_area_clear_selection(&input_area);
                    }

                    input_area_move_cursor_end(&input_area);
                    break;
                }
                case SDL_SCANCODE_A: {
                    if (mod & KMOD_CTRL) {
                        input_area_move_cursor_beginning(&input_area);
                    }
                    break;
                }
                case SDL_SCANCODE_E: {
                    if (mod & KMOD_CTRL) {
                        input_area_move_cursor_end_line(&input_area);
                    }
                    break;
                }
                case SDL_SCANCODE_K: {
                    if (mod & KMOD_CTRL) {
                        input_area_kill_to_end(&input_area);
                        /* Copy killed text to clipboard */
                        const char *killed = input_area_get_kill_ring(&input_area);
                        if (killed && killed[0] != '\0') {
                            SDL_SetClipboardText(killed);
                        }
                    }
                    break;
                }
                case SDL_SCANCODE_U: {
                    if (mod & KMOD_CTRL) {
                        input_area_kill_from_start(&input_area);
                        /* Copy killed text to clipboard */
                        const char *killed = input_area_get_kill_ring(&input_area);
                        if (killed && killed[0] != '\0') {
                            SDL_SetClipboardText(killed);
                        }
                    }
                    break;
                }
                case SDL_SCANCODE_W: {
                    if (mod & KMOD_CTRL) {
                        input_area_kill_word(&input_area);
                        /* Copy killed text to clipboard */
                        const char *killed = input_area_get_kill_ring(&input_area);
                        if (killed && killed[0] != '\0') {
                            SDL_SetClipboardText(killed);
                        }
                    }
                    break;
                }
                case SDL_SCANCODE_Y: {
                    if (mod & KMOD_CTRL) {
                        input_area_yank(&input_area);
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
                        else if (input_area_has_selection(&input_area)) {
                            char selection_buffer[INPUT_AREA_MAX_LENGTH];
                            if (input_area_copy_selection(&input_area, selection_buffer, INPUT_AREA_MAX_LENGTH) > 0) {
                                SDL_SetClipboardText(selection_buffer);
                            }
                        } else {
                            const char *text = input_area_copy(&input_area);
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
                        input_area_delete_char(&input_area);
                    }
                    break;
                }
                case SDL_SCANCODE_V: {
                    if (mod & KMOD_CTRL) {
                        /* Paste text from clipboard */
                        if (SDL_HasClipboardText()) {
                            char *text = SDL_GetClipboardText();
                            if (text) {
                                input_area_paste(&input_area, text);
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
                            int cursor_pos = input_area_get_cursor_pos(&input_area);
                            int length = input_area_get_length(&input_area);
                            int needs_redraw = input_area_needs_redraw(&input_area);
                            char *buffer = input_area_get_buffer(&input_area);
                            lisp_x_cancel_tab_completion(buffer, INPUT_AREA_MAX_LENGTH, &cursor_pos, &length,
                                                         &needs_redraw);
                            input_area_sync_state(&input_area);
                            input_area_move_cursor(&input_area, cursor_pos);
                        }
                    }
                    break;
                }
                case SDL_SCANCODE_J: {
                    if (mod & KMOD_CTRL) {
                        /* Ctrl+J: Insert newline for multi-line input */
                        input_area_insert_text(&input_area, "\n", 1);
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
                        InputAreaMode current = input_area_get_mode(&input_area);
                        InputAreaMode new_mode =
                            (current == INPUT_AREA_MODE_NORMAL) ? INPUT_AREA_MODE_EVAL : INPUT_AREA_MODE_NORMAL;
                        input_area_set_mode(&input_area, new_mode);
                        break;
                    }

                    /* Tab in eval mode: do nothing */
                    if (input_area_get_mode(&input_area) == INPUT_AREA_MODE_EVAL) {
                        break;
                    }

                    /* Handle TAB completion via Lisp bridge */
                    /* Note: lisp_handle_tab modifies buffer directly */
                    int cursor_pos = input_area_get_cursor_pos(&input_area);
                    int length = input_area_get_length(&input_area);
                    int needs_redraw = input_area_needs_redraw(&input_area);
                    char *buffer = input_area_get_buffer(&input_area);
                    lisp_x_handle_tab(buffer, INPUT_AREA_MAX_LENGTH, &cursor_pos, &length, &needs_redraw);
                    /* Sync state after external buffer modification */
                    input_area_sync_state(&input_area);
                    /* Update cursor position */
                    input_area_move_cursor(&input_area, cursor_pos);
                    break;
                }
                case SDL_SCANCODE_ESCAPE: {
                    /* ESC: Clear terminal selection if active */
                    if (terminal_selection.active) {
                        clear_terminal_selection(term);
                    }
                    /* ESC: Cancel tab completion and revert */
                    else if (lisp_x_is_tab_mode_active()) {
                        int cursor_pos = input_area_get_cursor_pos(&input_area);
                        int length = input_area_get_length(&input_area);
                        int needs_redraw = input_area_needs_redraw(&input_area);
                        char *buffer = input_area_get_buffer(&input_area);
                        lisp_x_cancel_tab_completion(buffer, INPUT_AREA_MAX_LENGTH, &cursor_pos, &length,
                                                     &needs_redraw);
                        input_area_sync_state(&input_area);
                        input_area_move_cursor(&input_area, cursor_pos);
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
                /* Scroll to bottom when user starts typing */
                if (lisp_x_get_scroll_to_bottom_on_user_input()) {
                    terminal_scroll_to_bottom(term);
                }
                /* All text input goes to input area */
                input_area_insert_text(&input_area, text, text_len);
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
                int input_area_height = (2 + input_area_get_visible_rows(&input_area)) * effective_cell_h;
                /* Check if click is within terminal area (excluding padding) and not in input area */
                if (event.button.x >= PADDING_X && event.button.x < win_width - PADDING_X &&
                    event.button.y >= PADDING_Y && event.button.y < win_height - PADDING_Y &&
                    event.button.y < win_height - input_area_height - PADDING_Y) {
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
                    int input_area_height = (2 + input_area_get_visible_rows(&input_area)) * effective_cell_h;
                    /* Check if motion is within terminal area (excluding padding) and not in input area */
                    if (event.motion.x >= PADDING_X && event.motion.x < motion_win_width - PADDING_X &&
                        event.motion.y >= PADDING_Y && event.motion.y < motion_win_height - PADDING_Y &&
                        event.motion.y < motion_win_height - input_area_height - PADDING_Y) {
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
                int input_area_height = (2 + input_area_get_visible_rows(&input_area)) * effective_cell_h;
                /* Check if motion is within terminal area (excluding padding) and not in input area */
                if (event.motion.x >= PADDING_X && event.motion.x < motion_win_width - PADDING_X &&
                    event.motion.y >= PADDING_Y && event.motion.y < motion_win_height - PADDING_Y &&
                    event.motion.y < motion_win_height - input_area_height - PADDING_Y) {
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
                int input_area_height = (2 + input_area_get_visible_rows(&input_area)) * effective_cell_h;
                /* Check if mouse is within terminal area (excluding padding) and not in input area */
                if (mouse_x >= PADDING_X && mouse_x < wheel_win_width - PADDING_X && mouse_y >= PADDING_Y &&
                    mouse_y < wheel_win_height - PADDING_Y &&
                    mouse_y < wheel_win_height - input_area_height - PADDING_Y) {
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

        /* Run timer callbacks */
        lisp_x_run_timers();

        /* Read from socket (if connected) */
        if (connected_mode) {
            /* Use select() to check if data is available (avoid unnecessary recv() calls) */
            int sock = telnet_get_socket(telnet);
            if (sock >= 0) {
                fd_set readfds;
                struct timeval tv = {0, 0}; /* Non-blocking check */
                FD_ZERO(&readfds);
#ifdef _WIN32
                FD_SET((SOCKET)sock, &readfds);
                int ready = select(0, &readfds, NULL, NULL, &tv); /* First param ignored on Windows */
#else
                FD_SET(sock, &readfds);
                int ready = select(sock + 1, &readfds, NULL, NULL, &tv);
#endif
#ifdef _WIN32
                if (ready > 0 && FD_ISSET((SOCKET)sock, &readfds)) {
#else
                if (ready > 0 && FD_ISSET(sock, &readfds)) {
#endif
                    /* Data is available, read it */
                    char recv_buf[4096];
                    int received = telnet_receive(telnet, recv_buf, sizeof(recv_buf) - 1);
                    if (received > 0) {
                        /* Call telnet-input-hook with received data (stripped of ANSI codes) */
                        lisp_x_call_telnet_input_hook(recv_buf, received);
                        /* Call telnet-input-filter-hook to transform data before displaying in terminal */
                        size_t filtered_len = 0;
                        const char *filtered_data =
                            lisp_x_call_telnet_input_filter_hook(recv_buf, received, &filtered_len);
                        /* Feed filtered data to terminal */
                        terminal_feed_data(term, filtered_data, filtered_len);

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
                        input_area_request_redraw(&input_area); /* Trigger color update */
                    }
                }
            }
        }

        /* Handle input area layout and sizing BEFORE rendering */
        int term_rows, term_cols;
        terminal_get_size(term, &term_rows, &term_cols);

        /* Recalculate layout if needed (text changed, newline inserted, etc.) */
        if (input_area.needs_layout_recalc) {
            input_area_recalculate_layout(&input_area, term_cols);
        }

        /* Check if input area height changed and resize terminal if needed */
        /* This must happen BEFORE rendering so vterm has correct scrolling region */
        int current_visible_rows = input_area_get_visible_rows(&input_area);
        if (current_visible_rows != prev_input_visible_rows) {
            prev_input_visible_rows = current_visible_rows;

            /* Recalculate terminal size with new input area height */
            int window_width, window_height;
            window_get_size(win, &window_width, &window_height);
            int new_rows, new_cols;
            calculate_terminal_size(window_width, window_height, cell_w, cell_h, &input_area, &new_rows, &new_cols);

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
                input_area_request_redraw(&input_area);                               /* Redraw divider with new mode */
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
                input_area_request_redraw(&input_area); /* Redraw divider without mode */
            }
            animation_mode_was_playing = animation_is_playing(active_anim);
        } else {
            /* No active animation - ensure mode is removed */
            if (animation_mode_was_playing) {
                lisp_x_remove_divider_mode("animation");
                input_area_request_redraw(&input_area); /* Redraw divider without mode */
                animation_mode_was_playing = 0;
            }
        }
        /* Update renderer's animation pointer */
        renderer_set_animation(active_anim);
#endif

        /* Render input area to vterm if it needs redraw */
        /* This must happen AFTER terminal resize so cursor is positioned correctly */
        if (input_area_needs_redraw(&input_area)) {
            terminal_render_input_area(term, &input_area, term_cols);
            input_area_mark_drawn(&input_area);
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
                            terminal_selection.end_scrollback_size, &input_area, term_cols);
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

        SDL_Delay(16); /* Cap at ~60 FPS */
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
