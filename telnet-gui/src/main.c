/* Main entry point for Telnet GUI */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <stdbool.h>
#include <errno.h>

#ifdef _WIN32
#include <winsock2.h>
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

/* Calculate terminal size (rows, cols) based on window dimensions */
static void calculate_terminal_size(int window_width, int window_height, int cell_w, int cell_h, InputArea *input_area,
                                    int *rows, int *cols) {
    /* Calculate columns from window width */
    *cols = window_width / cell_w;
    if (*cols < 10)
        *cols = 10; /* Minimum width */

    /* Calculate number of rows that fit in window height */
    /* Subtract rows for top divider, bottom divider, and dynamic input area height */
    int input_height_rows =
        2 + input_area_get_visible_rows(input_area); /* Top divider + bottom divider + visible input rows */
    *rows = (window_height / cell_h) - input_height_rows;
    if (*rows < 1)
        *rows = 1; /* Minimum: 1 scrolling row */
}

static void print_help(const char *program_name) {
    printf("Usage: %s [OPTIONS] [hostname] [port]\n", program_name);
    printf("\n");
    printf("Options:\n");
    printf("  -h, --help              Show this help message and exit\n");
    printf("\n");
    printf("  Font Options:\n");
    printf("    -f, --font-size SIZE   Set font size in points (default: 12)\n");
    printf("    -F<letter>             Select font (default: d):\n");
    printf("                             m = Cascadia Mono, i = Inconsolata, p = IBM Plex Mono,\n");
    printf("                             d = DejaVu Sans Mono, c = Courier Prime\n");
    printf("    --font <name>          Select font by name:\n");
    printf("                             cascadia, inconsolata, plex, dejavu, courier\n");
    printf("    -H, --hinting MODE     Set font hinting mode (default: none)\n");
    printf("                            MODE can be: none, light, normal, mono\n");
    printf("    -a, --antialiasing MODE Set anti-aliasing mode (default: linear)\n");
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
    printf("    -t, --test FILE        Run a Lisp test file in headless mode and exit\n");
    printf("                            Returns 0 on success, non-zero on failure\n");
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
    printf("  %s --font dejavu telnet-server 4449\n", program_name);
    printf("      Connect using DejaVu Sans Mono font (long form)\n");
    printf("  %s -g 100x40 telnet-server 4449\n", program_name);
    printf("      Connect with 100x40 terminal size\n");
    printf("  %s -l completion.lisp telnet-server 4449\n", program_name);
    printf("      Connect and load Lisp configuration file\n");
    printf("  %s -l tintin.lisp -l myconfig.lisp server 4449\n", program_name);
    printf("      Load multiple Lisp files in order\n");
    printf("  %s -t tintin_test_final.lisp\n", program_name);
    printf("      Run a test file in headless mode\n");
}

int main(int argc, char **argv) {
    /* Default settings */
    int hinting_mode = TTF_HINTING_NONE;             /* Default: no hinting for crisp rendering */
    SDL_ScaleMode scale_mode = SDL_ScaleModeNearest; /* Default: nearest (pixel-perfect) scaling */
    const char *hostname = NULL;
    int port = 0;
    const char *lisp_files[16]; /* Support up to 16 -l flags */
    int lisp_file_count = 0;
    const char *test_file = NULL; /* Test file for headless mode */
    char font_choice =
        'd'; /* Font selection: d=DejaVu Sans Mono (default), m=Cascadia Mono, i=Inconsolata, p=Plex, c=Courier */
    int font_size = 12;     /* Default font size */
    int terminal_cols = 80; /* Default terminal columns */
    int terminal_rows = 40; /* Default terminal rows */
    int debug_exit = 0;     /* Exit after initialization for debug output */

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
            if (font_choice != 'm' && font_choice != 'i' && font_choice != 'p' && font_choice != 'd' &&
                font_choice != 'c') {
                fprintf(stderr, "Error: Invalid font flag -F%c. Use: m, i, p, d, or c\n", font_choice);
                return 1;
            }
        } else if (strcmp(argv[arg_idx], "--font") == 0) {
            if (arg_idx + 1 >= argc) {
                fprintf(stderr, "Error: --font requires a font name\n");
                return 1;
            }
            arg_idx++;
            if (strcmp(argv[arg_idx], "cascadia") == 0)
                font_choice = 'm';
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
        } else if (strcmp(argv[arg_idx], "-t") == 0 || strcmp(argv[arg_idx], "--test") == 0) {
            if (arg_idx + 1 >= argc) {
                fprintf(stderr, "Error: --test requires a file path\n");
                return 1;
            }
            arg_idx++;
            test_file = argv[arg_idx];
            /* Skip further argument processing when test mode is enabled */
            /* Exit the loop immediately to prevent processing the file path as a positional argument */
            break;
        } else if (strcmp(argv[arg_idx], "--debug-exit") == 0) {
            debug_exit = 1;
        } else {
            /* Positional arguments: hostname and port */
            /* Skip if test_file is already set (test mode) */
            if (test_file != NULL) {
                fprintf(stderr, "Error: Unexpected argument '%s' in test mode\n", argv[arg_idx]);
                fprintf(stderr, "Use --help for usage information\n");
                return 1;
            }
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

    /* If test mode, run test and exit (headless mode) */
    if (test_file) {
        printf("Running test: %s\n", test_file);
        int result = lisp_x_load_file(test_file);
        lisp_x_cleanup();
        if (result < 0) {
            fprintf(stderr, "Test failed: %s\n", test_file);
            return 1;
        }
        printf("Test completed successfully: %s\n", test_file);
        return 0;
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

    /* Determine font filename based on user preference */
    const char *font_filename;
    const char *font_name;
    switch (font_choice) {
    case 'm':
        font_filename = "CascadiaMono-Regular.ttf";
        font_name = "Cascadia Mono";
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

    /* Create glyph cache with selected font */
    /* Get executable base path using SDL */
    char *base_path = SDL_GetBasePath();
    char font_path[1024] = {0};
    const char *font_paths[10];
    const char *font_path_labels[10];
    int font_path_count = 0;

    fprintf(stderr, "Font resolution: Starting font search...\n");

    /* Try font relative to executable first (installation path) */
    if (base_path) {
        /* SDL_GetBasePath() should return a path with trailing separator, but be safe */
        size_t base_len = strlen(base_path);
        const char *sep =
            (base_len > 0 && (base_path[base_len - 1] == '/' || base_path[base_len - 1] == '\\')) ? "" : "/";

        /* Normalize path separators - use backslashes on Windows for consistency */
        char normalized_path[1024];
        snprintf(normalized_path, sizeof(normalized_path), "%s%sassets/fonts/%s", base_path, sep, font_filename);

/* Convert forward slashes to backslashes on Windows for SDL_ttf compatibility */
#ifdef _WIN32
        for (char *p = normalized_path; *p; p++) {
            if (*p == '/')
                *p = '\\';
        }
#endif

        strncpy(font_path, normalized_path, sizeof(font_path) - 1);
        font_path[sizeof(font_path) - 1] = '\0';

        font_paths[font_path_count] = font_path;
        font_path_labels[font_path_count] = "executable-relative (installation path)";
        font_path_count++;
        fprintf(stderr, "Font resolution: Executable base path: %s\n", base_path);
        fprintf(stderr, "Font resolution: Constructed font path: %s\n", font_path);
        SDL_free(base_path);
    } else {
        fprintf(stderr, "Font resolution: Warning - SDL_GetBasePath() returned NULL\n");
    }

    /* Add fallback paths - try source tree paths first since we know the font exists there */
    static char fallback_path1[256];
    snprintf(fallback_path1, sizeof(fallback_path1), "telnet-gui/assets/fonts/%s", font_filename);
    font_paths[font_path_count] = fallback_path1;
    font_path_labels[font_path_count++] = "source tree path (from build root)";

    static char fallback_path2[256];
    snprintf(fallback_path2, sizeof(fallback_path2), "../../telnet-gui/assets/fonts/%s", font_filename);
    font_paths[font_path_count] = fallback_path2;
    font_path_labels[font_path_count++] = "source tree path (nested build dir)";

    static char fallback_path3[256];
    snprintf(fallback_path3, sizeof(fallback_path3), "../telnet-gui/assets/fonts/%s", font_filename);
    font_paths[font_path_count] = fallback_path3;
    font_path_labels[font_path_count++] = "source tree path (parent dir)";

    static char fallback_path4[256];
    snprintf(fallback_path4, sizeof(fallback_path4), "assets/fonts/%s", font_filename);
    font_paths[font_path_count] = fallback_path4;
    font_path_labels[font_path_count++] = "current directory relative (build/development)";

    static char fallback_path5[256];
    snprintf(fallback_path5, sizeof(fallback_path5), "../assets/fonts/%s", font_filename);
    font_paths[font_path_count] = fallback_path5;
    font_path_labels[font_path_count++] = "parent directory relative";

    /* Last resort fallbacks */
    font_paths[font_path_count] = "C:/Windows/Fonts/consola.ttf";
    font_path_labels[font_path_count++] = "Windows system fallback (Consola)";
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
        FILE *test = fopen(font_paths[i], "rb");
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

    /* Calculate exact window size for terminal geometry using actual glyph metrics */
    int separator_and_input_height = 3 * cell_h; /* Top divider + bottom divider + input row */
    int precise_width = terminal_cols * cell_w;
    int precise_height = terminal_rows * cell_h + separator_and_input_height;

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

    /* Wire telnet to terminal for output buffering */
    terminal_set_telnet(term, telnet);

    /* Load user-provided Lisp files in order after terminal and telnet are registered */
    for (int i = 0; i < lisp_file_count; i++) {
        if (lisp_x_load_file(lisp_files[i]) < 0) {
            fprintf(stderr, "Failed to load Lisp file: %s\n", lisp_files[i]);
            /* Don't exit - just continue without user config */
        }
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

    /* Initialize input area */
    input_area_init(&input_area);

    /* Get actual window size after resize and resize terminal to match */
    int input_area_height = 3 * cell_h; /* Top divider + bottom divider + input row */
    int actual_width, actual_height;
    SDL_GetWindowSize(sdl_window, &actual_width, &actual_height);
    int initial_rows, initial_cols;
    calculate_terminal_size(actual_width, actual_height, cell_w, cell_h, &input_area, &initial_rows, &initial_cols);
    int input_visible_rows = input_area_get_visible_rows(&input_area);
    terminal_resize(term, initial_rows, initial_cols, input_visible_rows);
    telnet_set_terminal_size(telnet, initial_cols, initial_rows);

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

                    /* Calculate new terminal size based on window size */
                    int new_rows, new_cols;
                    calculate_terminal_size(new_width, new_height, cell_w, cell_h, &input_area, &new_rows, &new_cols);

                    /* Recalculate input area layout with new width */
                    input_area_recalculate_layout(&input_area, new_cols);

                    /* Update terminal size */
                    int input_visible_rows = input_area_get_visible_rows(&input_area);
                    terminal_resize(term, new_rows, new_cols, input_visible_rows);

                    /* Re-render input area immediately to update divider position and restore cursor */
                    terminal_render_input_area(term, &input_area, new_cols);

                    /* Send NAWS to telnet server */
                    telnet_set_terminal_size(telnet, new_cols, new_rows);
                }
                break;

            case SDL_MOUSEBUTTONDOWN: {
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

                /* Handle clicks in terminal area (not in input area) */
                if (mouse_y < window_height - input_area_height) {
                    /* Start selection only if no selection was cleared */
                    if (event.button.button == SDL_BUTTON_LEFT && !had_selection) {
                        /* Convert mouse coordinates to terminal cell coordinates */
                        int term_row = mouse_y / cell_h;
                        int term_col = mouse_x / cell_w;
                        /* Start selection and freeze viewport */
                        start_terminal_selection(term, term_row, term_col);
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

                switch (scancode) {
                case SDL_SCANCODE_RETURN:
                case SDL_SCANCODE_KP_ENTER: {
                    /* Send input area text to terminal and telnet */
                    int length = input_area_get_length(&input_area);
                    if (length > 0) {
                        const char *text = input_area_get_text(&input_area);
                        int cursor_pos = input_area_get_cursor_pos(&input_area);

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
                        } else {
                            /* Normal text - call user-input-hook to transform text before sending */
                            const char *transformed_text = lisp_x_call_user_input_hook(text, cursor_pos);
                            int transformed_length = strlen(transformed_text);

                            /* Hook contract: non-string or empty string = hook handled everything */
                            /* Proper way: return nil to indicate hook handled echo/send */
                            /* If hook returns empty string, it means hook handled echo/send - don't send again */

                            if (transformed_length > 0) {
                                /* Echo transformed text to terminal (vterm_feed_data will normalize LF to CRLF) */
                                char echo_buf[INPUT_AREA_MAX_LENGTH + 2];
                                int echo_len = transformed_length;
                                memcpy(echo_buf, transformed_text, transformed_length);
                                echo_buf[echo_len++] = '\n'; /* Add newline - vterm_feed_data will add \r if needed */
                                terminal_feed_data(term, echo_buf, echo_len);

                                /* Scroll to bottom on user input if configured */
                                if (lisp_x_get_scroll_to_bottom_on_user_input()) {
                                    terminal_scroll_to_bottom(term);
                                }

                                /* Send transformed text to telnet with CRLF (if connected) */
                                if (connected_mode) {
                                    /* Clear any accidentally buffered output before sending */
                                    /* This prevents echoed text from being sent twice */
                                    terminal_clear_output_buffer(term);

                                    char telnet_buf[INPUT_AREA_MAX_LENGTH * 2 + 2];
                                    int telnet_len = 0;
                                    for (int i = 0;
                                         i < transformed_length && telnet_len < (int)(sizeof(telnet_buf) - 2); i++) {
                                        char c = transformed_text[i];
                                        if (c == '\n') {
                                            if (telnet_len < (int)(sizeof(telnet_buf) - 2)) {
                                                telnet_buf[telnet_len++] = '\r';
                                                telnet_buf[telnet_len++] = '\n';
                                            }
                                        } else {
                                            telnet_buf[telnet_len++] = c;
                                        }
                                    }
                                    if (telnet_len < (int)(sizeof(telnet_buf) - 2)) {
                                        telnet_buf[telnet_len++] = '\r';
                                        telnet_buf[telnet_len++] = '\n';
                                        int sent = telnet_send(telnet, telnet_buf, telnet_len);
                                        if (sent < 0) {
                                            fprintf(stderr, "Failed to send data via telnet\n");
                                            /* Connection lost - switch to unconnected mode */
                                            connected_mode = 0;
                                            terminal_feed_data(term, "\r\n*** Connection lost ***\r\n",
                                                               strlen("\r\n*** Connection lost ***\r\n"));
                                        }
                                    }
                                } else {
                                    /* Not connected - echo message to terminal */
                                    const char *not_conn = "\r\n*** Not connected ***\r\n";
                                    terminal_feed_data(term, not_conn, strlen(not_conn));
                                }
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
                        /* Even if input is empty, send CRLF for newline (if connected) */
                        if (connected_mode) {
                            /* Clear any accidentally buffered output before sending */
                            terminal_clear_output_buffer(term);

                            char crlf[] = "\r\n";
                            int sent = telnet_send(telnet, crlf, 2);
                            if (sent < 0) {
                                fprintf(stderr, "Failed to send CRLF via telnet\n");
                                /* Connection lost - switch to unconnected mode */
                                connected_mode = 0;
                                terminal_feed_data(term, "\r\n*** Connection lost ***\r\n",
                                                   strlen("\r\n*** Connection lost ***\r\n"));
                            }
                        }
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
                /* Accept tab completion if active (any text input exits tab mode) */
                if (lisp_x_is_tab_mode_active()) {
                    lisp_x_accept_tab_completion();
                }
                /* All text input goes to input area */
                const char *text = event.text.text;
                int text_len = strlen(text);
                input_area_insert_text(&input_area, text, text_len);
                break;
            }

            case SDL_MOUSEBUTTONUP: {
                /* Selection remains active after mouse button up - user can copy with Ctrl+C */
                /* Only handle mouse events for terminal if not in input area */
                int win_width, win_height;
                window_get_size(win, &win_width, &win_height);
                if (event.button.y < win_height - input_area_height) {
                    input_handle_mouse(&event.button, NULL, terminal_get_vterm(term), cell_w, cell_h, 0);
                }
                break;
            }

            case SDL_MOUSEMOTION:
                /* Update selection if dragging in terminal area */
                if (terminal_selection.active && (event.motion.state & SDL_BUTTON(SDL_BUTTON_LEFT))) {
                    int motion_win_width, motion_win_height;
                    window_get_size(win, &motion_win_width, &motion_win_height);
                    if (event.motion.y < motion_win_height - input_area_height) {
                        /* Convert mouse coordinates to terminal cell coordinates */
                        int term_row = event.motion.y / cell_h;
                        int term_col = event.motion.x / cell_w;
                        /* Update selection end position */
                        update_terminal_selection(term, term_row, term_col);
                    }
                }
                /* Only handle mouse events for terminal if not in input area */
                int motion_win_width, motion_win_height;
                window_get_size(win, &motion_win_width, &motion_win_height);
                if (event.motion.y < motion_win_height - input_area_height) {
                    input_handle_mouse(NULL, &event.motion, terminal_get_vterm(term), cell_w, cell_h, 0);
                }
                break;

            case SDL_MOUSEWHEEL: {
                /* Get mouse position */
                int mouse_x, mouse_y;
                SDL_GetMouseState(&mouse_x, &mouse_y);

                /* Check if mouse is over terminal area (not input area) */
                int wheel_win_width, wheel_win_height;
                window_get_size(win, &wheel_win_width, &wheel_win_height);
                if (mouse_y < wheel_win_height - input_area_height) {
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
                        /* Clear terminal selection on new input */
                        if (terminal_selection.active) {
                            clear_terminal_selection(term);
                        }
                        /* Call telnet-input-hook with received data (stripped of ANSI codes) */
                        lisp_x_call_telnet_input_hook(recv_buf, received);
                        /* Call telnet-input-filter-hook to transform data before displaying in terminal */
                        size_t filtered_len = 0;
                        const char *filtered_data =
                            lisp_x_call_telnet_input_filter_hook(recv_buf, received, &filtered_len);
                        /* Feed filtered data to terminal */
                        terminal_feed_data(term, filtered_data, filtered_len);

                        /* Scroll to bottom on telnet input if configured */
                        if (lisp_x_get_scroll_to_bottom_on_telnet_input()) {
                            terminal_scroll_to_bottom(term);
                        }
                    } else if (received < 0) {
                        /* Connection closed or error (telnet_receive returns -1 for both) */
                        connected_mode = 0;
                        terminal_feed_data(term, "\r\n*** Connection closed ***\r\n",
                                           strlen("\r\n*** Connection closed ***\r\n"));
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

        if (terminal_needs_redraw(term) || terminal_selection.active) {
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
    telnet_destroy(telnet);
    terminal_destroy(term);
    renderer_destroy(rend);
    glyph_cache_destroy(glyph_cache);
    window_destroy(win);

    return 0;
}
