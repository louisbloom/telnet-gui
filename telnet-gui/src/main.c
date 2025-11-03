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

static int running = 1;

static void cleanup(void) {
    SDL_Quit();
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
    printf("\n");
    printf("Arguments:\n");
    printf("  hostname               Telnet server hostname or IP address\n");
    printf("  port                   Telnet server port number\n");
    printf("\n");
    printf("Examples:\n");
    printf("  %s carrionfields.net 4449\n", program_name);
    printf("  %s --hinting light --antialiasing linear example.com 23\n", program_name);
    printf("  %s --hinting normal --antialiasing nearest localhost 8080\n", program_name);
}

int main(int argc, char **argv) {
    /* Default settings */
    int hinting_mode = TTF_HINTING_NONE;
    SDL_ScaleMode scale_mode = SDL_ScaleModeLinear;
    const char *hostname = NULL;
    int port = 0;

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

            case SDL_MOUSEBUTTONDOWN:
                mouse_x = event.button.x;
                mouse_y = event.button.y;
                WindowTitlebarAction action = window_check_titlebar_click(win, mouse_x, mouse_y);
                if (action == WINDOW_TITLEBAR_ACTION_CLOSE) {
                    running = 0;
                } else if (action == WINDOW_TITLEBAR_ACTION_MINIMIZE) {
                    SDL_MinimizeWindow(window_get_sdl_window(win));
                }
                break;

            case SDL_KEYDOWN:
                /* Don't handle keys that produce text */
                if (!(event.key.keysym.mod & KMOD_CTRL && event.key.keysym.sym == SDLK_v)) {
                    input_handle_keyboard(&event.key, term);
                }
                break;

            case SDL_TEXTINPUT:
                input_handle_text(&event.text, term);
                break;

            case SDL_MOUSEBUTTONUP:
                input_handle_mouse(&event.button, NULL, terminal_get_vterm(term), cell_w, cell_h, titlebar_h);
                break;

            case SDL_MOUSEMOTION:
                input_handle_mouse(NULL, &event.motion, terminal_get_vterm(term), cell_w, cell_h, titlebar_h);
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
        if (terminal_needs_redraw(term)) {
            char title[256];
            snprintf(title, sizeof(title), "Telnet: %s:%d", hostname, port);
            renderer_render(rend, term, title);
            terminal_mark_drawn(term);
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
