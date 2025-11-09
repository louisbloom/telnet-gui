/* Main entry point for Telnet GUI */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <stdbool.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

#include "telnet.h"
#include "terminal.h"
#include "window.h"
#include "renderer.h"
#include "glyph_cache.h"
#include "input.h"
#include "lisp_bridge.h"

static int running = 1;

/* Cached system cursors */
static SDL_Cursor *cursor_arrow = NULL;
static SDL_Cursor *cursor_sizewe = NULL;
static SDL_Cursor *cursor_sizens = NULL;
static SDL_Cursor *cursor_sizenwse = NULL;
static SDL_Cursor *cursor_sizenesw = NULL;
static SDL_Cursor *current_cursor = NULL;

/* Input area structure */
#define INPUT_AREA_MAX_LENGTH 4096
typedef struct {
    char buffer[INPUT_AREA_MAX_LENGTH];
    int cursor_pos;   /* Cursor position in buffer */
    int length;       /* Current length of text */
    int needs_redraw; /* Flag to indicate input area needs redraw */
} InputArea;

static InputArea input_area = {0};

static void cleanup(void) {
    lisp_bridge_cleanup();
    SDL_Quit();
}

/* Calculate terminal size (rows, cols) based on window dimensions */
static void calculate_terminal_size(int window_width, int window_height, int cell_w, int cell_h, int titlebar_h,
                                    int input_area_height, int *rows, int *cols) {
    /* Calculate columns from window width */
    *cols = window_width / cell_w;
    if (*cols < 1)
        *cols = 1;

    /* Calculate available height for terminal (window height minus titlebar and input area) */
    int available_height = window_height - titlebar_h - input_area_height;
    /* Calculate number of rows that fit exactly in available space */
    /* Use integer division - this ensures terminal doesn't exceed available space */
    *rows = available_height / cell_h;
    if (*rows < 1)
        *rows = 1;
}

static void print_help(const char *program_name) {
    printf("Usage: %s [OPTIONS] <hostname> <port>\n", program_name);
    printf("\n");
    printf("Options:\n");
    printf("  -h, --help              Show this help message and exit\n");
    printf("  --hinting MODE         Set font hinting mode (default: none)\n");
    printf("                           MODE can be: none, light, normal, mono\n");
    printf("  --antialiasing MODE    Set anti-aliasing mode (default: linear)\n");
    printf("                           MODE can be: nearest, linear\n");
    printf("  -l, --lisp-file FILE   Load and evaluate Lisp file on startup\n");
    printf("                           Used to customize completion hooks\n");
    printf("\n");
    printf("Arguments:\n");
    printf("  hostname               Telnet server hostname or IP address\n");
    printf("  port                   Telnet server port number\n");
    printf("\n");
    printf("Examples:\n");
    printf("  %s carrionfields.net 4449\n", program_name);
    printf("  %s --hinting light --antialiasing linear example.com 23\n", program_name);
    printf("  %s --hinting normal --antialiasing nearest localhost 8080\n", program_name);
    printf("  %s -l completion.lisp carrionfields.net 4449\n", program_name);
}

int main(int argc, char **argv) {
    /* Default settings */
    int hinting_mode = TTF_HINTING_NONE;
    SDL_ScaleMode scale_mode = SDL_ScaleModeLinear;
    const char *hostname = NULL;
    int port = 0;
    const char *lisp_file = NULL;

    /* Parse command-line arguments */
    int arg_idx = 1;
    while (arg_idx < argc) {
        if (strcmp(argv[arg_idx], "-h") == 0 || strcmp(argv[arg_idx], "--help") == 0) {
            print_help(argv[0]);
            return 0;
        } else if (strcmp(argv[arg_idx], "--hinting") == 0) {
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
        } else if (strcmp(argv[arg_idx], "--antialiasing") == 0) {
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
        } else if (strcmp(argv[arg_idx], "-l") == 0 || strcmp(argv[arg_idx], "--lisp-file") == 0) {
            if (arg_idx + 1 >= argc) {
                fprintf(stderr, "Error: --lisp-file requires a file path\n");
                return 1;
            }
            arg_idx++;
            lisp_file = argv[arg_idx];
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

    /* Validate required arguments */
    if (hostname == NULL || port == 0) {
        fprintf(stderr, "Error: Missing required arguments: hostname and port\n");
        fprintf(stderr, "Use --help for usage information\n");
        return 1;
    }

    /* Set locale for UTF-8 support */
    setlocale(LC_ALL, "");

    /* Initialize Lisp bridge (loads bootstrap file) */
    if (lisp_bridge_init() < 0) {
        fprintf(stderr, "Failed to initialize Lisp bridge\n");
        return 1;
    }

    /* Load user-provided Lisp file if provided */
    if (lisp_file) {
        if (lisp_bridge_load_file(lisp_file) < 0) {
            lisp_bridge_cleanup();
            return 1;
        }
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

    /* Initialize cursor cache */
    cursor_arrow = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_ARROW);
    cursor_sizewe = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_SIZEWE);
    cursor_sizens = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_SIZENS);
    cursor_sizenwse = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_SIZENWSE);
    cursor_sizenesw = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_SIZENESW);
    current_cursor = cursor_arrow;
    SDL_SetCursor(current_cursor);

    atexit(cleanup);

    /* Create window */
    Window *win = window_create("Telnet GUI", 800, 600);
    if (!win) {
        fprintf(stderr, "Failed to create window\n");
        return 1;
    }

    SDL_Renderer *renderer = window_get_sdl_renderer(win);
    int titlebar_h = window_get_titlebar_height();

    /* Create glyph cache with IBM Plex Mono */
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
        snprintf(normalized_path, sizeof(normalized_path), "%s%sassets/fonts/IBMPlexMono-Regular.ttf", base_path, sep);

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
    font_paths[font_path_count] = "telnet-gui/assets/fonts/IBMPlexMono-Regular.ttf";
    font_path_labels[font_path_count++] = "source tree path (from build root)";
    font_paths[font_path_count] = "../../telnet-gui/assets/fonts/IBMPlexMono-Regular.ttf";
    font_path_labels[font_path_count++] = "source tree path (nested build dir)";
    font_paths[font_path_count] = "../telnet-gui/assets/fonts/IBMPlexMono-Regular.ttf";
    font_path_labels[font_path_count++] = "source tree path (parent dir)";
    font_paths[font_path_count] = "assets/fonts/IBMPlexMono-Regular.ttf";
    font_path_labels[font_path_count++] = "current directory relative (build/development)";
    font_paths[font_path_count] = "../assets/fonts/IBMPlexMono-Regular.ttf";
    font_path_labels[font_path_count++] = "parent directory relative";
    /* Last resort fallbacks */
    font_paths[font_path_count] = "C:/Windows/Fonts/consola.ttf";
    font_path_labels[font_path_count++] = "Windows system fallback (Consola)";
    font_paths[font_path_count] = NULL;

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

        /* Use slightly smaller font size with specified hinting and antialiasing */
        glyph_cache = glyph_cache_create(renderer, font_paths[i], 16, hinting_mode, scale_mode);
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
        window_destroy(win);
        return 1;
    }

    int cell_w, cell_h;
    glyph_cache_get_cell_size(glyph_cache, &cell_w, &cell_h);

    /* Create renderer */
    Renderer *rend = renderer_create(renderer, glyph_cache, cell_w, cell_h, titlebar_h);
    if (!rend) {
        fprintf(stderr, "Failed to create renderer\n");
        glyph_cache_destroy(glyph_cache);
        window_destroy(win);
        return 1;
    }

    /* Create terminal */
    Terminal *term = terminal_create(24, 80);
    if (!term) {
        fprintf(stderr, "Failed to create terminal\n");
        renderer_destroy(rend);
        glyph_cache_destroy(glyph_cache);
        window_destroy(win);
        return 1;
    }

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

    /* Wire telnet to terminal for output buffering */
    terminal_set_telnet(term, telnet);

    /* Connect */
    fprintf(stderr, "Connecting to %s:%d...\n", hostname, port);
    if (telnet_connect(telnet, hostname, port) < 0) {
        fprintf(stderr, "Failed to connect to %s:%d\n", hostname, port);
        telnet_destroy(telnet);
        terminal_destroy(term);
        renderer_destroy(rend);
        glyph_cache_destroy(glyph_cache);
        window_destroy(win);
        return 1;
    }
    fprintf(stderr, "Connected successfully\n");

    /* Initialize input area */
    memset(&input_area, 0, sizeof(input_area));
    input_area.needs_redraw = 1;

    /* Resize terminal to match initial window size */
    int input_area_height = cell_h; /* Input area is one cell height */
    int initial_width, initial_height;
    window_get_size(win, &initial_width, &initial_height);
    int initial_rows, initial_cols;
    calculate_terminal_size(initial_width, initial_height, cell_w, cell_h, titlebar_h, input_area_height, &initial_rows,
                            &initial_cols);
    terminal_resize(term, initial_rows, initial_cols);
    telnet_set_terminal_size(telnet, initial_cols, initial_rows);

    /* Main loop */
    SDL_Event event;
    int mouse_x = 0, mouse_y = 0;

    while (running && telnet_get_state(telnet) == TELNET_STATE_CONNECTED) {
        /* Poll events */
        while (SDL_PollEvent(&event)) {
            switch (event.type) {
            case SDL_QUIT:
                running = 0;
                break;

            case SDL_WINDOWEVENT:
                if (event.window.event == SDL_WINDOWEVENT_RESIZED) {
                    /* Ignore resize events during manual resize (we handle it ourselves) */
                    if (!window_is_resizing(win)) {
                        int new_width = event.window.data1;
                        int new_height = event.window.data2;

                        /* Calculate new terminal size based on window size */
                        int new_rows, new_cols;
                        calculate_terminal_size(new_width, new_height, cell_w, cell_h, titlebar_h, input_area_height,
                                                &new_rows, &new_cols);

                        /* Update terminal size */
                        terminal_resize(term, new_rows, new_cols);

                        /* Update button positions */
                        window_update_button_positions(win);

                        /* Send NAWS to telnet server */
                        telnet_set_terminal_size(telnet, new_cols, new_rows);
                    }
                }
                break;

            case SDL_MOUSEBUTTONDOWN:
                mouse_x = event.button.x;
                mouse_y = event.button.y;
                /* Get window size to check if mouse is in input area */
                int window_width, window_height;
                window_get_size(win, &window_width, &window_height);
                /* Exclude input area from resize and titlebar checks */
                if (mouse_y < window_height - input_area_height) {
                    /* Check for resize area first */
                    int resize_mode = window_check_resize_area(win, mouse_x, mouse_y);
                    if (resize_mode != RESIZE_NONE) {
                        window_start_resize(win, resize_mode, 0,
                                            0); /* Coordinates fetched from SDL_GetGlobalMouseState */
                    } else {
                        /* Check for titlebar buttons */
                        WindowTitlebarAction action = window_check_titlebar_click(win, mouse_x, mouse_y);
                        if (action == WINDOW_TITLEBAR_ACTION_CLOSE) {
                            running = 0;
                        } else if (action == WINDOW_TITLEBAR_ACTION_MINIMIZE) {
                            SDL_MinimizeWindow(window_get_sdl_window(win));
                        }
                    }
                }
                /* Mouse clicks in input area are ignored (input area is always active) */
                break;

            case SDL_KEYDOWN: {
                /* All keyboard input goes to input area, not terminal */
                SDL_Scancode scancode = event.key.keysym.scancode;
                SDL_Keymod mod = event.key.keysym.mod;

                switch (scancode) {
                case SDL_SCANCODE_RETURN:
                case SDL_SCANCODE_KP_ENTER: {
                    /* Send input area text to terminal and telnet */
                    if (input_area.length > 0) {
                        /* Echo to terminal with CRLF (carriage return + line feed) */
                        char echo_buf[INPUT_AREA_MAX_LENGTH + 2];
                        memcpy(echo_buf, input_area.buffer, input_area.length);
                        echo_buf[input_area.length] = '\r';
                        echo_buf[input_area.length + 1] = '\n';
                        terminal_feed_data(term, echo_buf, input_area.length + 2);

                        /* Send to telnet with CRLF (telnet standard) */
                        char telnet_buf[INPUT_AREA_MAX_LENGTH + 2];
                        memcpy(telnet_buf, input_area.buffer, input_area.length);
                        telnet_buf[input_area.length] = '\r';
                        telnet_buf[input_area.length + 1] = '\n';
                        int sent = telnet_send(telnet, telnet_buf, input_area.length + 2);
                        if (sent < 0) {
                            fprintf(stderr, "Failed to send data via telnet\n");
                        }

                        /* Clear input area */
                        input_area.buffer[0] = '\0';
                        input_area.length = 0;
                        input_area.cursor_pos = 0;
                        input_area.needs_redraw = 1;
                    } else {
                        /* Even if input is empty, send CRLF for newline */
                        char crlf[] = "\r\n";
                        int sent = telnet_send(telnet, crlf, 2);
                        if (sent < 0) {
                            fprintf(stderr, "Failed to send CRLF via telnet\n");
                        }
                        /* Echo CRLF to terminal */
                        terminal_feed_data(term, "\r\n", 2);
                    }
                    break;
                }
                case SDL_SCANCODE_BACKSPACE: {
                    /* Delete character before cursor */
                    if (input_area.cursor_pos > 0) {
                        memmove(&input_area.buffer[input_area.cursor_pos - 1],
                                &input_area.buffer[input_area.cursor_pos], input_area.length - input_area.cursor_pos);
                        input_area.cursor_pos--;
                        input_area.length--;
                        /* Clear the last byte to ensure no stale data */
                        if (input_area.length < INPUT_AREA_MAX_LENGTH) {
                            input_area.buffer[input_area.length] = '\0';
                        }
                        input_area.needs_redraw = 1;
                    }
                    break;
                }
                case SDL_SCANCODE_DELETE: {
                    /* Delete character at cursor */
                    if (input_area.cursor_pos < input_area.length) {
                        memmove(&input_area.buffer[input_area.cursor_pos],
                                &input_area.buffer[input_area.cursor_pos + 1],
                                input_area.length - input_area.cursor_pos - 1);
                        input_area.length--;
                        /* Clear the last byte to ensure no stale data */
                        if (input_area.length < INPUT_AREA_MAX_LENGTH) {
                            input_area.buffer[input_area.length] = '\0';
                        }
                        input_area.needs_redraw = 1;
                    }
                    break;
                }
                case SDL_SCANCODE_LEFT: {
                    /* Move cursor left */
                    if (input_area.cursor_pos > 0) {
                        input_area.cursor_pos--;
                        input_area.needs_redraw = 1;
                    }
                    break;
                }
                case SDL_SCANCODE_RIGHT: {
                    /* Move cursor right */
                    if (input_area.cursor_pos < input_area.length) {
                        input_area.cursor_pos++;
                        input_area.needs_redraw = 1;
                    }
                    break;
                }
                case SDL_SCANCODE_HOME: {
                    /* Move cursor to beginning */
                    input_area.cursor_pos = 0;
                    input_area.needs_redraw = 1;
                    break;
                }
                case SDL_SCANCODE_END: {
                    /* Move cursor to end */
                    input_area.cursor_pos = input_area.length;
                    input_area.needs_redraw = 1;
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
                    lisp_bridge_handle_tab(input_area.buffer, INPUT_AREA_MAX_LENGTH, &input_area.cursor_pos,
                                           &input_area.length, &input_area.needs_redraw);
                    break;
                }
                default:
                    /* Other keys are handled by SDL_TEXTINPUT */
                    break;
                }
                break;
            }

            case SDL_TEXTINPUT: {
                /* All text input goes to input area */
                const char *text = event.text.text;
                int text_len = strlen(text);

                /* Insert text at cursor position */
                if (input_area.length + text_len < INPUT_AREA_MAX_LENGTH) {
                    /* Make room for new text */
                    memmove(&input_area.buffer[input_area.cursor_pos + text_len],
                            &input_area.buffer[input_area.cursor_pos], input_area.length - input_area.cursor_pos);
                    /* Insert new text */
                    memcpy(&input_area.buffer[input_area.cursor_pos], text, text_len);
                    input_area.cursor_pos += text_len;
                    input_area.length += text_len;
                    /* Null-terminate to ensure no stale data is read */
                    if (input_area.length < INPUT_AREA_MAX_LENGTH) {
                        input_area.buffer[input_area.length] = '\0';
                    }
                    input_area.needs_redraw = 1;
                }
                break;
            }

            case SDL_MOUSEBUTTONUP:
                /* End resize if active */
                if (window_is_resizing(win)) {
                    /* Update terminal size when resize ends */
                    int new_width, new_height;
                    window_get_size(win, &new_width, &new_height);
                    int new_rows, new_cols;
                    calculate_terminal_size(new_width, new_height, cell_w, cell_h, titlebar_h, input_area_height,
                                            &new_rows, &new_cols);
                    terminal_resize(term, new_rows, new_cols);
                    window_update_button_positions(win);
                    telnet_set_terminal_size(telnet, new_cols, new_rows);
                }
                window_end_resize(win);
                /* Only handle mouse events for terminal if not in input area */
                int win_width, win_height;
                window_get_size(win, &win_width, &win_height);
                if (event.button.y < win_height - input_area_height) {
                    input_handle_mouse(&event.button, NULL, terminal_get_vterm(term), cell_w, cell_h, titlebar_h);
                }
                break;

            case SDL_MOUSEMOTION:
                /* Update resize if active (mouse button down during resize) */
                if (window_is_resizing(win) && (event.motion.state & SDL_BUTTON(SDL_BUTTON_LEFT))) {
                    /* Throttle window resize updates (every ~16ms = ~60 FPS) for smooth performance */
                    /* SDL_SetWindowSize on Windows borderless windows can be slow */
                    Uint32 current_time = SDL_GetTicks();
                    static Uint32 last_window_resize = 0;
                    if (current_time - last_window_resize > 16 || last_window_resize == 0) {
                        window_update_resize(win, 0, 0); /* Coordinates fetched from SDL_GetGlobalMouseState */
                        last_window_resize = current_time;
                    }
                    /* Throttle terminal size updates during resize (every ~150ms) for smoother performance */
                    /* Terminal resize is expensive, so we only do it periodically */
                    static Uint32 last_terminal_resize = 0;
                    if (current_time - last_terminal_resize > 150 || last_terminal_resize == 0) {
                        int new_width, new_height;
                        window_get_size(win, &new_width, &new_height);
                        int new_rows, new_cols;
                        calculate_terminal_size(new_width, new_height, cell_w, cell_h, titlebar_h, input_area_height,
                                                &new_rows, &new_cols);
                        terminal_resize(term, new_rows, new_cols);
                        window_update_button_positions(win);
                        last_terminal_resize = current_time;
                    }
                } else if (!window_is_resizing(win)) {
                    /* Set cursor for resize areas (exclude input area) */
                    int win_width, win_height;
                    window_get_size(win, &win_width, &win_height);
                    ResizeMode resize_mode = RESIZE_NONE;
                    /* Only check resize areas if not in input area */
                    if (event.motion.y < win_height - input_area_height) {
                        resize_mode = window_check_resize_area(win, event.motion.x, event.motion.y);
                    }
                    SDL_Cursor *new_cursor = cursor_arrow;
                    switch (resize_mode) {
                    case RESIZE_LEFT:
                    case RESIZE_RIGHT:
                        new_cursor = cursor_sizewe;
                        break;
                    case RESIZE_BOTTOM:
                        new_cursor = cursor_sizens;
                        break;
                    case RESIZE_BOTTOMLEFT:
                        new_cursor = cursor_sizenesw;
                        break;
                    case RESIZE_BOTTOMRIGHT:
                        new_cursor = cursor_sizenwse;
                        break;
                    default:
                        new_cursor = cursor_arrow;
                        break;
                    }
                    if (new_cursor != current_cursor) {
                        SDL_SetCursor(new_cursor);
                        current_cursor = new_cursor;
                    }
                }
                /* Only handle mouse events for terminal if not in input area */
                int motion_win_width, motion_win_height;
                window_get_size(win, &motion_win_width, &motion_win_height);
                if (event.motion.y < motion_win_height - input_area_height) {
                    input_handle_mouse(NULL, &event.motion, terminal_get_vterm(term), cell_w, cell_h, titlebar_h);
                }
                break;
            }
        }

        /* Read from socket */
        char recv_buf[4096];
        int received = telnet_receive(telnet, recv_buf, sizeof(recv_buf) - 1);
        if (received > 0) {
            terminal_feed_data(term, recv_buf, received);
        }

        /* Render if needed */
        int window_width, window_height;
        window_get_size(win, &window_width, &window_height);
        int needs_render = 0;

        if (terminal_needs_redraw(term)) {
            char title[256];
            snprintf(title, sizeof(title), "Telnet: %s:%d", hostname, port);
            renderer_render(rend, term, title);
            terminal_mark_drawn(term);
            needs_render = 1;
        } else if (window_is_resizing(win)) {
            /* During resize, render at lower rate for smooth performance */
            static Uint32 last_resize_render = 0;
            Uint32 current_time = SDL_GetTicks();
            if (current_time - last_resize_render > 33) { /* ~30 FPS during resize */
                char title[256];
                snprintf(title, sizeof(title), "Telnet: %s:%d", hostname, port);
                renderer_render(rend, term, title);
                last_resize_render = current_time;
                needs_render = 1;
            }
        }

        /* Always render input area if it needs redraw or if terminal was rendered */
        if (input_area.needs_redraw || needs_render) {
            renderer_render_input_area(rend, input_area.buffer, input_area.length, input_area.cursor_pos, window_width,
                                       window_height, input_area_height);
            input_area.needs_redraw = 0;
            SDL_RenderPresent(window_get_sdl_renderer(win));
        }

        SDL_Delay(16); /* Cap at ~60 FPS */
    }

    /* Cleanup */
    telnet_destroy(telnet);
    terminal_destroy(term);
    renderer_destroy(rend);
    glyph_cache_destroy(glyph_cache);
    window_destroy(win);

    /* Free cursors */
    if (cursor_arrow)
        SDL_FreeCursor(cursor_arrow);
    if (cursor_sizewe)
        SDL_FreeCursor(cursor_sizewe);
    if (cursor_sizens)
        SDL_FreeCursor(cursor_sizens);
    if (cursor_sizenwse)
        SDL_FreeCursor(cursor_sizenwse);
    if (cursor_sizenesw)
        SDL_FreeCursor(cursor_sizenesw);

    return 0;
}
