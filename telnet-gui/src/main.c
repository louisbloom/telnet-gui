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

int main(int argc, char **argv) {
    /* Parse command-line arguments */
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <hostname> <port>\n", argv[0]);
        return 1;
    }

    const char *hostname = argv[1];
    int port = atoi(argv[2]);

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

    /* Create glyph cache */
    GlyphCache *glyph_cache = glyph_cache_create(renderer, NULL, 14);
    if (!glyph_cache) {
        /* Try fallback font */
        glyph_cache = glyph_cache_create(renderer, "C:/Windows/Fonts/consola.ttf", 14);
    }
    if (!glyph_cache) {
        fprintf(stderr, "Failed to create glyph cache\n");
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

    /* We'll handle terminal output in the main loop */

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
                    input_handle_keyboard(&event.key, terminal_get_vterm(term));
                }
                break;

            case SDL_TEXTINPUT:
                input_handle_text(&event.text, terminal_get_vterm(term));
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
