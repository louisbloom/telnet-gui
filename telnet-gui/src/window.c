/* Borderless window implementation */

#include "window.h"
#include "lisp.h"
#include <stdlib.h>
#include <string.h>

#define TITLEBAR_HEIGHT 30
#define BUTTON_SIZE 16
#define BUTTON_PADDING 4
#define RESIZE_AREA_SIZE 8
#define RESIZE_BAR_HEIGHT 6

typedef struct Button {
    int x, y, w, h;
    WindowTitlebarAction action;
} Button;

struct Window {
    SDL_Window *window;
    SDL_Renderer *renderer;
    Button close_button;
    Button minimize_button;
    ResizeMode resize_mode;
    int resize_start_x;
    int resize_start_y;
    int resize_start_width;
    int resize_start_height;
    int resize_start_window_x;
    int resize_start_window_y;
    Uint32 last_resize_render_time;
};

static SDL_HitTestResult hit_test(SDL_Window *win, const SDL_Point *area, void *data) {
    Window *w = (Window *)data;
    int x = area->x, y = area->y;
    int win_w, win_h;
    SDL_GetWindowSize(win, &win_w, &win_h);

    /* Top 30px is draggable, but check buttons first */
    if (y < TITLEBAR_HEIGHT) {
        /* Check if click is on close button - return NORMAL to allow click handling */
        if (x >= w->close_button.x && x < w->close_button.x + w->close_button.w && y >= w->close_button.y &&
            y < w->close_button.y + w->close_button.h) {
            return SDL_HITTEST_NORMAL;
        }
        /* Check if click is on minimize button - return NORMAL to allow click handling */
        if (x >= w->minimize_button.x && x < w->minimize_button.x + w->minimize_button.w && y >= w->minimize_button.y &&
            y < w->minimize_button.y + w->minimize_button.h) {
            return SDL_HITTEST_NORMAL;
        }
        /* Rest of titlebar is draggable */
        return SDL_HITTEST_DRAGGABLE;
    }

    /* Resize bar at bottom - return NORMAL so we can handle resize manually */
    /* SDL's automatic resize doesn't work reliably for borderless windows on Windows */
    if (y >= win_h - RESIZE_BAR_HEIGHT) {
        return SDL_HITTEST_NORMAL;
    }

    /* Left/Right edges - return NORMAL so we can handle resize manually */
    /* But not in the resize bar area at bottom */
    if (y < win_h - RESIZE_BAR_HEIGHT && (x < RESIZE_AREA_SIZE || x > win_w - RESIZE_AREA_SIZE)) {
        return SDL_HITTEST_NORMAL;
    }

    return SDL_HITTEST_NORMAL;
}

Window *window_create(const char *title, int width, int height) {
    Window *w = (Window *)malloc(sizeof(Window));
    if (!w)
        return NULL;

    /* Set SDL hints BEFORE creating renderer for proper effect */
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1"); /* Linear scaling for smooth text */

    w->window = SDL_CreateWindow(title, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, width, height,
                                 SDL_WINDOW_BORDERLESS | SDL_WINDOW_RESIZABLE);
    if (!w->window) {
        free(w);
        return NULL;
    }

    SDL_SetWindowHitTest(w->window, hit_test, w);

    w->renderer = SDL_CreateRenderer(w->window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (!w->renderer) {
        SDL_DestroyWindow(w->window);
        free(w);
        return NULL;
    }

    /* Set blend mode for proper alpha blending */
    SDL_SetRenderDrawBlendMode(w->renderer, SDL_BLENDMODE_BLEND);

    /* Use linear filtering for best quality text rendering (0=nearest, 1=linear) */
    /* Note: SDL_HINT_RENDER_INTEGER_SCALE requires SDL 2.0.5+, not available in this version */
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1");

    /* Initialize buttons - centered vertically in titlebar */
    int button_y = (TITLEBAR_HEIGHT - BUTTON_SIZE) / 2;
    /* Horizontal spacing after buttons matches vertical spacing above buttons */
    int button_right_padding = button_y;
    w->close_button.x = width - BUTTON_SIZE - button_right_padding;
    w->close_button.y = button_y;
    w->close_button.w = BUTTON_SIZE;
    w->close_button.h = BUTTON_SIZE;
    w->close_button.action = WINDOW_TITLEBAR_ACTION_CLOSE;

    w->minimize_button.x = width - (BUTTON_SIZE * 2) - button_right_padding - BUTTON_PADDING;
    w->minimize_button.y = button_y;
    w->minimize_button.w = BUTTON_SIZE;
    w->minimize_button.h = BUTTON_SIZE;
    w->minimize_button.action = WINDOW_TITLEBAR_ACTION_MINIMIZE;
    w->resize_mode = RESIZE_NONE;
    w->last_resize_render_time = 0;

    return w;
}

SDL_Window *window_get_sdl_window(Window *w) {
    return w->window;
}

SDL_Renderer *window_get_sdl_renderer(Window *w) {
    return w->renderer;
}

void window_get_size(Window *w, int *width, int *height) {
    SDL_GetWindowSize(w->window, width, height);
}

void window_update_button_positions(Window *w) {
    if (!w)
        return;
    int width, height;
    SDL_GetWindowSize(w->window, &width, &height);

    /* Update button positions based on new window width */
    /* Horizontal spacing after buttons matches vertical spacing above buttons */
    int button_y = (TITLEBAR_HEIGHT - BUTTON_SIZE) / 2;
    int button_right_padding = button_y;
    w->close_button.x = width - BUTTON_SIZE - button_right_padding;
    w->minimize_button.x = width - (BUTTON_SIZE * 2) - button_right_padding - BUTTON_PADDING;
}

ResizeMode window_check_resize_area(Window *w, int x, int y, int input_area_height) {
    if (!w)
        return RESIZE_NONE;
    int width, height;
    SDL_GetWindowSize(w->window, &width, &height);

    /* Top area is draggable, not resize */
    if (y < TITLEBAR_HEIGHT)
        return RESIZE_NONE;

    /* Resize bar at the very bottom - only RESIZE_BAR_HEIGHT pixels */
    if (y >= height - RESIZE_BAR_HEIGHT) {
        if (x < RESIZE_AREA_SIZE)
            return RESIZE_BOTTOMLEFT;
        if (x >= width - RESIZE_AREA_SIZE)
            return RESIZE_BOTTOMRIGHT;
        return RESIZE_BOTTOM;
    }

    /* Left/Right edges - only in input area, not in terminal area */
    /* Terminal area: from TITLEBAR_HEIGHT to (height - input_area_height - RESIZE_BAR_HEIGHT) */
    /* Input area: from (height - input_area_height - RESIZE_BAR_HEIGHT) to (height - RESIZE_BAR_HEIGHT) */
    int terminal_area_bottom = height - input_area_height - RESIZE_BAR_HEIGHT;
    if (y >= terminal_area_bottom && y < height - RESIZE_BAR_HEIGHT) {
        /* In input area - allow left/right edge resize */
        if (x < RESIZE_AREA_SIZE)
            return RESIZE_LEFT;
        if (x >= width - RESIZE_AREA_SIZE)
            return RESIZE_RIGHT;
    }

    return RESIZE_NONE;
}

void window_start_resize(Window *w, ResizeMode resize_mode, int mouse_x, int mouse_y) {
    (void)mouse_x; /* Unused - using SDL_GetGlobalMouseState instead */
    (void)mouse_y; /* Unused - using SDL_GetGlobalMouseState instead */
    if (!w || resize_mode == RESIZE_NONE)
        return;
    /* Set resize mode first for immediate state */
    w->resize_mode = resize_mode;
    /* Use screen coordinates for resize calculations */
    int screen_x, screen_y;
    SDL_GetGlobalMouseState(&screen_x, &screen_y);
    w->resize_start_x = screen_x;
    w->resize_start_y = screen_y;
    /* Cache window size/position to avoid multiple SDL calls */
    SDL_GetWindowSize(w->window, &w->resize_start_width, &w->resize_start_height);
    SDL_GetWindowPosition(w->window, &w->resize_start_window_x, &w->resize_start_window_y);
}

void window_update_resize(Window *w, int mouse_x, int mouse_y) {
    (void)mouse_x; /* Unused - using SDL_GetGlobalMouseState instead */
    (void)mouse_y; /* Unused - using SDL_GetGlobalMouseState instead */
    if (!w || w->resize_mode == RESIZE_NONE)
        return;

    /* Use screen coordinates for resize calculations */
    int screen_x, screen_y;
    SDL_GetGlobalMouseState(&screen_x, &screen_y);
    int delta_x = screen_x - w->resize_start_x;
    int delta_y = screen_y - w->resize_start_y;
    int new_width = w->resize_start_width;
    int new_height = w->resize_start_height;
    int new_x = w->resize_start_window_x;
    int new_y = w->resize_start_window_y;

    switch (w->resize_mode) {
    case RESIZE_LEFT: {
        int min_width = 100;
        new_width -= delta_x;
        new_x += delta_x;
        if (new_width < min_width) {
            int excess = min_width - new_width;
            new_width = min_width;
            new_x -= excess; /* Adjust position to maintain minimum width */
        }
        break;
    }
    case RESIZE_RIGHT:
        new_width += delta_x;
        if (new_width < 100)
            new_width = 100;
        break;
    case RESIZE_BOTTOM:
        new_height += delta_y;
        if (new_height < 100)
            new_height = 100;
        break;
    case RESIZE_BOTTOMLEFT: {
        int min_width = 100;
        int min_height = 100;
        new_width -= delta_x;
        new_x += delta_x;
        new_height += delta_y;
        if (new_width < min_width) {
            int excess = min_width - new_width;
            new_width = min_width;
            new_x -= excess;
        }
        if (new_height < min_height)
            new_height = min_height;
        break;
    }
    case RESIZE_BOTTOMRIGHT:
        new_width += delta_x;
        new_height += delta_y;
        if (new_width < 100)
            new_width = 100;
        if (new_height < 100)
            new_height = 100;
        break;
    default:
        return;
    }

    /* Set window size first (for left edge resize, we need to set position too) */
    if (w->resize_mode == RESIZE_LEFT || w->resize_mode == RESIZE_BOTTOMLEFT) {
        /* For left edge resize, set position first, then size */
        SDL_SetWindowPosition(w->window, new_x, new_y);
        SDL_SetWindowSize(w->window, new_width, new_height);
    } else {
        /* For other resize modes, just set size */
        SDL_SetWindowSize(w->window, new_width, new_height);
    }

    /* Note: SDL_SetWindowSize on Windows borderless windows may not update visually */
    /* during drag. The visual update will happen on the next render frame. */
}

void window_end_resize(Window *w) {
    if (w)
        w->resize_mode = RESIZE_NONE;
}

int window_is_resizing(Window *w) {
    return w ? (w->resize_mode != RESIZE_NONE) : 0;
}

WindowTitlebarAction window_check_titlebar_click(Window *w, int mouse_x, int mouse_y) {
    /* Check close button */
    if (mouse_x >= w->close_button.x && mouse_x < w->close_button.x + w->close_button.w &&
        mouse_y >= w->close_button.y && mouse_y < w->close_button.y + w->close_button.h) {
        return WINDOW_TITLEBAR_ACTION_CLOSE;
    }

    /* Check minimize button */
    if (mouse_x >= w->minimize_button.x && mouse_x < w->minimize_button.x + w->minimize_button.w &&
        mouse_y >= w->minimize_button.y && mouse_y < w->minimize_button.y + w->minimize_button.h) {
        return WINDOW_TITLEBAR_ACTION_MINIMIZE;
    }

    return WINDOW_TITLEBAR_ACTION_NONE;
}

void window_set_title(Window *w, const char *title) {
    SDL_SetWindowTitle(w->window, title);
}

void window_render_titlebar(Window *w, SDL_Renderer *renderer, const char *text) {
    (void)text; /* Unused parameter - reserved for future use */
    int w_width, w_height;
    SDL_GetWindowSize(w->window, &w_width, &w_height);

    /* Get resize bar color from Lisp config and use for titlebar */
    int r, g, b;
    lisp_x_get_resize_bar_color(&r, &g, &b);

    /* Draw titlebar background with same color as resize bar */
    SDL_SetRenderDrawColor(renderer, r, g, b, 255);
    SDL_Rect titlebar = {0, 0, w_width, TITLEBAR_HEIGHT};
    SDL_RenderFillRect(renderer, &titlebar);

    /* Draw close button */
    SDL_SetRenderDrawColor(renderer, 200, 50, 50, 255);
    SDL_Rect close_rect = {w->close_button.x, w->close_button.y, w->close_button.w, w->close_button.h};
    SDL_RenderFillRect(renderer, &close_rect);

    /* Draw minimize button */
    SDL_SetRenderDrawColor(renderer, 100, 100, 100, 255);
    SDL_Rect min_rect = {w->minimize_button.x, w->minimize_button.y, w->minimize_button.w, w->minimize_button.h};
    SDL_RenderFillRect(renderer, &min_rect);
}

int window_get_titlebar_height(void) {
    return TITLEBAR_HEIGHT;
}

int window_get_resize_bar_height(void) {
    return RESIZE_BAR_HEIGHT;
}

void window_render_resize_bar(Window *w, SDL_Renderer *renderer) {
    if (!w || !renderer)
        return;

    int w_width, w_height;
    SDL_GetWindowSize(w->window, &w_width, &w_height);

    /* Get resize bar color from Lisp config */
    int r, g, b;
    lisp_x_get_resize_bar_color(&r, &g, &b);

    /* Draw resize bar at bottom (middle section) */
    SDL_SetRenderDrawColor(renderer, r, g, b, 255);
    SDL_Rect resize_bar = {0, w_height - RESIZE_BAR_HEIGHT, w_width, RESIZE_BAR_HEIGHT};
    SDL_RenderFillRect(renderer, &resize_bar);

    /* Draw brighter shade for left/right corners of resize bar */
    int bright_r = r + (255 - r) / 3; /* Brighten by ~33% */
    int bright_g = g + (255 - g) / 3;
    int bright_b = b + (255 - b) / 3;
    SDL_SetRenderDrawColor(renderer, bright_r, bright_g, bright_b, 255);

    /* Left corner of bottom resize bar */
    SDL_Rect left_corner = {0, w_height - RESIZE_BAR_HEIGHT, RESIZE_AREA_SIZE, RESIZE_BAR_HEIGHT};
    SDL_RenderFillRect(renderer, &left_corner);

    /* Right corner of bottom resize bar */
    SDL_Rect right_corner = {w_width - RESIZE_AREA_SIZE, w_height - RESIZE_BAR_HEIGHT, RESIZE_AREA_SIZE,
                             RESIZE_BAR_HEIGHT};
    SDL_RenderFillRect(renderer, &right_corner);
}

void window_destroy(Window *w) {
    if (!w)
        return;
    if (w->renderer)
        SDL_DestroyRenderer(w->renderer);
    if (w->window)
        SDL_DestroyWindow(w->window);
    free(w);
}
