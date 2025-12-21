/* Lisp implementation for telnet-gui */

#include "lisp.h"
#include "input_area.h"
#include "terminal.h"
#include "telnet.h"
#include "glyph_cache.h"
#include "window.h"
#include "dynamic_buffer.h"
#include "path_utils.h"
#if HAVE_RLOTTIE
#include "animation.h"
#endif
#include "../../telnet-lisp/include/lisp.h"
#include "../../telnet-lisp/include/file_utils.h"
#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Version information - fallbacks if not defined by CMake */
#ifndef PROJECT_VERSION
#define PROJECT_VERSION "unknown"
#endif

#ifndef LIBVTERM_VERSION
#define LIBVTERM_VERSION "unknown"
#endif

#ifndef SDL2_VERSION
#define SDL2_VERSION "unknown"
#endif

#ifndef SDL2_TTF_VERSION
#define SDL2_TTF_VERSION "unknown"
#endif

#ifndef GC_VERSION
#define GC_VERSION "unknown"
#endif

#ifndef PCRE2_VERSION
#define PCRE2_VERSION "unknown"
#endif

#ifndef CMAKE_VERSION
#define CMAKE_VERSION "unknown"
#endif

#ifndef SYSTEM_NAME
#define SYSTEM_NAME "unknown"
#endif

#ifndef SYSTEM_PROCESSOR
#define SYSTEM_PROCESSOR "unknown"
#endif

#ifndef TOOLCHAIN
#define TOOLCHAIN "unknown"
#endif

/* Lisp environment for completion hooks and future primitives */
static Environment *lisp_env = NULL;

/* Registered terminal pointer for terminal-echo builtin */
static Terminal *registered_terminal = NULL;
static Telnet *registered_telnet = NULL;
static GlyphCache *registered_glyph_cache = NULL;
static Window *registered_window = NULL;
static InputArea *registered_input_area = NULL;
#if HAVE_RLOTTIE
static SDL_Renderer *registered_renderer = NULL;
static Animation *active_animation = NULL; /* Animation currently being rendered */

/* Animation type tag symbol for Lisp objects */
static LispObject *sym_animation_type = NULL;

/* Helper: Create a Lisp object wrapping an Animation pointer */
static LispObject *make_animation_object(Animation *anim) {
    if (!anim)
        return NIL;
    /* Store as (animation-type . <pointer-as-integer>) */
    return lisp_make_cons(sym_animation_type, lisp_make_integer((intptr_t)anim));
}

/* Helper: Extract Animation pointer from a Lisp object */
static Animation *get_animation_object(LispObject *obj) {
    if (!obj || obj->type != LISP_CONS)
        return NULL;
    if (obj->value.cons.car != sym_animation_type)
        return NULL;
    LispObject *ptr_obj = obj->value.cons.cdr;
    if (!ptr_obj || ptr_obj->type != LISP_INTEGER)
        return NULL;
    return (Animation *)(intptr_t)ptr_obj->value.integer;
}

#endif

/* Static buffer for ANSI code stripping (pre-allocated at startup, freed on exit) */
static char *ansi_strip_buffer = NULL;
static size_t ansi_strip_buffer_size = 0;

/* Static buffer for telnet input filter result (pre-allocated at startup, freed on exit) */
static char *telnet_filter_buffer = NULL;
static size_t telnet_filter_buffer_size = 0;

/* Static buffer for telnet input filter temporary string conversion (pre-allocated, doubles on demand) */
static char *telnet_filter_temp_buffer = NULL;
static size_t telnet_filter_temp_buffer_size = 0;

/* Static buffer for user input hook result (pre-allocated, doubles on demand) */
static char *user_input_hook_buffer = NULL;
static size_t user_input_hook_buffer_size = 0;

/* Tab completion cycling state */
#define MAX_COMPLETIONS 256
#define TAB_BUFFER_SIZE 4096
static int tab_mode_active = 0;
static char *tab_completions[MAX_COMPLETIONS]; /* Array of completion strings */
static int tab_completion_count = 0;
static int tab_completion_index = 0;
static char tab_original_buffer[TAB_BUFFER_SIZE]; /* Original buffer before tab mode */
static int tab_original_cursor_pos = 0;
static int tab_original_length = 0;
static int tab_start_pos = 0;   /* Where the partial text starts */
static int tab_partial_len = 0; /* Length of the partial text */

/* Utility function to ensure a buffer is large enough, growing it by doubling if needed */
static int ensure_buffer_size(char **buffer, size_t *buffer_size, size_t required_size) {
    if (!buffer || !buffer_size) {
        return -1;
    }

    /* If buffer doesn't exist or is too small, grow it */
    if (!*buffer || *buffer_size < required_size) {
        size_t new_size = required_size;
        /* Double the size until it's large enough */
        if (*buffer_size > 0) {
            new_size = *buffer_size;
            while (new_size < required_size) {
                new_size *= 2;
            }
        } else {
            /* First allocation: use minimum size if required size is smaller */
            if (new_size < 4096) {
                new_size = 4096; /* Minimum size */
            }
        }
        char *new_buffer = realloc(*buffer, new_size);
        if (!new_buffer) {
            return -1; /* Allocation failed */
        }
        *buffer = new_buffer;
        *buffer_size = new_size;
    }
    return 0; /* Success */
}

/* Extract partial text to complete using regex pattern */
static int extract_partial_text(const char *buffer, int cursor_pos, int length, int *start_pos, int *partial_len) {
    if (!buffer || cursor_pos < 0 || cursor_pos > length || !start_pos || !partial_len) {
        return 0;
    }

    /* For now, simple implementation: look back to whitespace or beginning */
    /* TODO: Use regex pattern from *completion-pattern* for more sophisticated matching */
    int start = cursor_pos;
    while (start > 0 && buffer[start - 1] != ' ' && buffer[start - 1] != '\t' && buffer[start - 1] != '\n') {
        start--;
    }

    *start_pos = start;
    *partial_len = cursor_pos - start;
    return 1;
}

/* Replace partial text in buffer with completion */
static void replace_partial_text(char *buffer, int buffer_size, int *cursor_pos, int *length, int start_pos,
                                 int partial_len, const char *completion) {
    if (!buffer || !completion || start_pos < 0 || partial_len < 0 || !cursor_pos || !length) {
        return;
    }

    int completion_len = strlen(completion);
    int new_length = *length - partial_len + completion_len;

    if (new_length >= buffer_size) {
        return; /* Too long */
    }

    /* Make room for new text */
    if (completion_len != partial_len) {
        memmove(&buffer[start_pos + completion_len], &buffer[start_pos + partial_len],
                *length - start_pos - partial_len);
    }

    /* Insert completion */
    memcpy(&buffer[start_pos], completion, completion_len);
    *length = new_length;
    *cursor_pos = start_pos + completion_len;
    buffer[*length] = '\0';
}

/* Strip ANSI escape sequences and control codes from input */
static char *strip_ansi_codes(const char *input, size_t len, size_t *out_len) {
    if (!input || len == 0 || !out_len) {
        if (out_len)
            *out_len = 0;
        return NULL;
    }

    /* Ensure buffer is initialized and large enough */
    if (!ansi_strip_buffer || ansi_strip_buffer_size < len + 1) {
        size_t new_size = len + 1;
        if (new_size < 4096) {
            new_size = 4096; /* Minimum size */
        }
        char *new_buffer = realloc(ansi_strip_buffer, new_size);
        if (!new_buffer) {
            if (out_len)
                *out_len = 0;
            return NULL;
        }
        ansi_strip_buffer = new_buffer;
        ansi_strip_buffer_size = new_size;
    }

    size_t out_pos = 0;
    int in_escape = 0;
    int in_csi = 0; /* Control Sequence Introducer (ESC[) */

    for (size_t i = 0; i < len; i++) {
        unsigned char c = (unsigned char)input[i];

        if (in_escape) {
            if (c == '[') {
                /* Start of CSI sequence */
                in_csi = 1;
                in_escape = 0;
            } else if (c >= 0x40 && c <= 0x5F) {
                /* Single character escape sequence (e.g., ESC m) */
                in_escape = 0;
            } else if (c == 0x1B) {
                /* Another ESC - keep in escape state */
            } else {
                /* End of escape sequence */
                in_escape = 0;
            }
            continue;
        }

        if (in_csi) {
            /* Inside CSI sequence - skip until terminator */
            if ((c >= 0x40 && c <= 0x7E) || c == 0x1B) {
                /* Terminator found or new ESC */
                in_csi = 0;
                if (c == 0x1B) {
                    in_escape = 1;
                }
            }
            continue;
        }

        if (c == 0x1B) {
            /* Start of escape sequence */
            in_escape = 1;
            continue;
        }

        /* Keep printable characters and whitespace */
        if (c >= 0x20 || c == '\n' || c == '\r' || c == '\t') {
            if (out_pos < ansi_strip_buffer_size - 1) {
                ansi_strip_buffer[out_pos++] = c;
            }
        }
        /* Skip other control characters */
    }

    /* Null terminate */
    ansi_strip_buffer[out_pos] = '\0';
    *out_len = out_pos;
    return ansi_strip_buffer;
}

/* Call Lisp completion hook */
static LispObject *call_completion_hook(const char *partial_text) {
    if (!lisp_env || !partial_text) {
        return NIL;
    }

    /* Look up completion-hook */
    LispObject *hook = env_lookup(lisp_env, "completion-hook");
    if (!hook || (hook->type != LISP_LAMBDA && hook->type != LISP_BUILTIN)) {
        return NIL;
    }

    /* Create argument list with partial text */
    LispObject *arg = lisp_make_string(partial_text);
    LispObject *args = lisp_make_cons(arg, NIL);

    /* Create function call: (completion-hook "partial-text") */
    LispObject *call_expr = lisp_make_cons(hook, args);

    /* Evaluate the function call */
    LispObject *result = lisp_eval(call_expr, lisp_env);

    /* Check for errors */
    if (result && result->type == LISP_ERROR) {
        return NIL;
    }

    return result ? result : NIL;
}

/* Load bootstrap Lisp file */
static int load_bootstrap_file(void) {
    if (!lisp_env) {
        return 0;
    }

    /* Get executable base path using SDL */
    char *base_path = SDL_GetBasePath();
    char bootstrap_path[1024] = {0};
    const char *bootstrap_paths[10];
    const char *bootstrap_path_labels[10];
    int bootstrap_path_count = 0;

    fprintf(stderr, "Bootstrap file resolution: Starting search...\n");

    /* PRIORITY 1: Build/development paths (checked first for development workflow) */
    /* Try bootstrap.lisp relative to executable (build directory during development) */
    /* Skip if executable is in a bin directory (installed location) - .lisp files aren't in bin */
    if (base_path) {
        if (path_construct_exe_relative(base_path, "bootstrap.lisp", bootstrap_path, sizeof(bootstrap_path))) {
            bootstrap_paths[bootstrap_path_count] = bootstrap_path;
            bootstrap_path_labels[bootstrap_path_count] = "executable-relative (build/development)";
            bootstrap_path_count++;
            fprintf(stderr, "Bootstrap file resolution: Executable base path: %s\n", base_path);
            fprintf(stderr, "Bootstrap file resolution: Constructed bootstrap path: %s\n", bootstrap_path);
        } else {
            fprintf(stderr,
                    "Bootstrap file resolution: Executable in bin directory (installed), skipping bin path check\n");
        }
        SDL_free(base_path);
    } else {
        fprintf(stderr, "Bootstrap file resolution: Warning - SDL_GetBasePath() returned NULL\n");
    }

    /* Add source tree paths for development */
    bootstrap_paths[bootstrap_path_count] = "lisp/bootstrap.lisp";
    bootstrap_path_labels[bootstrap_path_count++] = "lisp subdirectory relative";
    bootstrap_paths[bootstrap_path_count] = "../lisp/bootstrap.lisp";
    bootstrap_path_labels[bootstrap_path_count++] = "parent lisp subdirectory";

    /* PRIORITY 2: Installed path (fallback for installed builds) */
    static char installed_bootstrap_path[TELNET_MAX_PATH];
    if (path_construct_installed_resource("lisp", "bootstrap.lisp", installed_bootstrap_path,
                                          sizeof(installed_bootstrap_path))) {
        bootstrap_paths[bootstrap_path_count] = installed_bootstrap_path;
        bootstrap_path_labels[bootstrap_path_count++] = "installed data directory";
        fprintf(stderr, "Bootstrap file resolution: Installed path available: %s\n", installed_bootstrap_path);
    }

    bootstrap_paths[bootstrap_path_count] = NULL;

    const char *loaded_bootstrap_path = NULL;
    const char *loaded_bootstrap_label = NULL;

    for (int i = 0; bootstrap_paths[i] != NULL; i++) {
        fprintf(stderr, "Bootstrap file resolution: Trying [%d] %s: %s\n", i + 1, bootstrap_path_labels[i],
                bootstrap_paths[i]);

        /* Check if file exists before trying to load */
        FILE *test = file_open(bootstrap_paths[i], "rb");
        if (test) {
            fclose(test);
            fprintf(stderr, "Bootstrap file resolution: File exists, attempting to load...\n");
        } else {
            fprintf(stderr, "Bootstrap file resolution: File does not exist, skipping...\n");
            continue;
        }

        LispObject *result = lisp_load_file(bootstrap_paths[i], lisp_env);
        if (result && result->type == LISP_ERROR) {
            char *err_str = lisp_print(result);
            fprintf(stderr, "Bootstrap file evaluation ERROR in %s:\n%s\n", bootstrap_paths[i], err_str);
            /* Continue trying other paths */
        } else {
            loaded_bootstrap_path = bootstrap_paths[i];
            loaded_bootstrap_label = bootstrap_path_labels[i];
            fprintf(stderr, "Bootstrap file resolution: SUCCESS! Loaded bootstrap file from [%d] %s\n", i + 1,
                    loaded_bootstrap_label);
            fprintf(stderr, "Bootstrap file resolution: Bootstrap file path: %s\n", loaded_bootstrap_path);
            return 1;
        }
    }

    fprintf(stderr, "Bootstrap file resolution: ERROR - Failed to load bootstrap file from all attempted paths\n");
    return 0;
}

/* Builtin function: input-area-redraw - Request input area/divider redraw */
static LispObject *builtin_input_area_redraw(LispObject *args, Environment *env) {
    (void)args;
    (void)env;

    if (registered_input_area) {
        input_area_request_redraw(registered_input_area);
    }
    return NIL;
}

/* Builtin function: terminal-echo - Echo text to terminal display */
static LispObject *builtin_terminal_echo(LispObject *args, Environment *env) {
    (void)env;

    if (args == NIL) {
        return lisp_make_error("terminal-echo requires 1 argument");
    }

    LispObject *text_obj = lisp_car(args);
    if (text_obj->type != LISP_STRING) {
        return lisp_make_error("terminal-echo requires a string argument");
    }

    const char *text = text_obj->value.string;

    if (!registered_terminal) {
        /* In headless mode (test mode), output to stdout */
        fprintf(stdout, "%s", text);
        fflush(stdout);
    } else {
        /* Normal mode: feed to terminal */
        size_t len = strlen(text);
        terminal_feed_data(registered_terminal, text, len);
    }

    return NIL;
}

static LispObject *builtin_telnet_send(LispObject *args, Environment *env) {
    (void)env;

    if (args == NIL) {
        return lisp_make_error("telnet-send requires 1 argument");
    }

    LispObject *text_obj = lisp_car(args);
    if (text_obj->type != LISP_STRING) {
        return lisp_make_error("telnet-send requires a string argument");
    }

    const char *text = text_obj->value.string;

    if (!registered_telnet) {
        /* No telnet connection registered */
        fprintf(stderr, "Warning: telnet-send called but no telnet connection registered\n");
        return NIL;
    }

    /* Send text with CRLF appended (using telnet's reusable buffer) */
    int result = telnet_send_with_crlf(registered_telnet, text, strlen(text));

    if (result < 0) {
        return lisp_make_error("telnet-send: failed to send data");
    }

    return NIL;
}

static LispObject *builtin_terminal_info(LispObject *args, Environment *env) {
    (void)args; /* No arguments expected */
    (void)env;

    /* Check if all required components are registered */
    if (!registered_terminal) {
        return lisp_make_error("terminal-info: no terminal registered");
    }
    if (!registered_glyph_cache) {
        return lisp_make_error("terminal-info: no glyph cache registered");
    }
    if (!registered_window) {
        return lisp_make_error("terminal-info: no window registered");
    }

    /* Collect terminal dimensions */
    int rows, cols;
    terminal_get_size(registered_terminal, &rows, &cols);

    /* Collect cell dimensions */
    int cell_w, cell_h;
    glyph_cache_get_cell_size(registered_glyph_cache, &cell_w, &cell_h);

    /* Get line height multiplier */
    float line_height = lisp_x_get_terminal_line_height();

    /* Collect window dimensions */
    int window_w, window_h;
    window_get_size(registered_window, &window_w, &window_h);

    /* Collect scrollback information */
    int viewport_offset = terminal_get_viewport_offset(registered_terminal);
    int scrollback_size = terminal_get_scrollback_size(registered_terminal);
    int max_scrollback = terminal_get_max_scrollback_lines(registered_terminal);

    /* Collect font information */
    const char *font_path = glyph_cache_get_font_path(registered_glyph_cache);
    const char *font_name = glyph_cache_get_font_name(registered_glyph_cache);

    /* Build association list using tail-pointer pattern for efficiency */
    LispObject *result = NIL;
    LispObject *tail = NULL;

    /* Helper macro to add key-value pairs */
#define ADD_PAIR(key_name, value_expr)                                                                                 \
    do {                                                                                                               \
        LispObject *key = lisp_make_symbol(key_name);                                                                  \
        LispObject *value = (value_expr);                                                                              \
        LispObject *pair = lisp_make_cons(key, value);                                                                 \
        LispObject *new_cons = lisp_make_cons(pair, NIL);                                                              \
        if (result == NIL) {                                                                                           \
            result = new_cons;                                                                                         \
            tail = new_cons;                                                                                           \
        } else {                                                                                                       \
            tail->value.cons.cdr = new_cons;                                                                           \
            tail = new_cons;                                                                                           \
        }                                                                                                              \
    } while (0)

    /* Add all pairs in logical order */
    ADD_PAIR("cols", lisp_make_integer(cols));
    ADD_PAIR("rows", lisp_make_integer(rows));
    ADD_PAIR("cell-width", lisp_make_integer(cell_w));
    ADD_PAIR("cell-height", lisp_make_integer(cell_h));
    ADD_PAIR("line-height", lisp_make_number((double)line_height));
    ADD_PAIR("window-width", lisp_make_integer(window_w));
    ADD_PAIR("window-height", lisp_make_integer(window_h));
    ADD_PAIR("viewport-offset", lisp_make_integer(viewport_offset));
    ADD_PAIR("scrollback-size", lisp_make_integer(scrollback_size));
    ADD_PAIR("max-scrollback", lisp_make_integer(max_scrollback));
    ADD_PAIR("font-path", lisp_make_string(font_path ? font_path : ""));
    ADD_PAIR("font-name", lisp_make_string(font_name ? font_name : ""));

#undef ADD_PAIR

    return result;
}

/* Built-in: version
 * Returns association list with version information for all dependencies and system.
 */
static LispObject *builtin_version(LispObject *args, Environment *env) {
    (void)args; /* No arguments expected */
    (void)env;  /* Environment not needed */

    /* Build association list using tail-pointer pattern */
    LispObject *result = NIL;
    LispObject *tail = NULL;

    /* Helper macro to add key-value pairs */
#define ADD_PAIR(key_name, value_expr)                                                                                 \
    do {                                                                                                               \
        LispObject *key = lisp_make_symbol(key_name);                                                                  \
        LispObject *value = (value_expr);                                                                              \
        LispObject *pair = lisp_make_cons(key, value);                                                                 \
        LispObject *new_cons = lisp_make_cons(pair, NIL);                                                              \
        if (result == NIL) {                                                                                           \
            result = new_cons;                                                                                         \
            tail = new_cons;                                                                                           \
        } else {                                                                                                       \
            tail->value.cons.cdr = new_cons;                                                                           \
            tail = new_cons;                                                                                           \
        }                                                                                                              \
    } while (0)

    /* Add version pairs in logical groups */

    /* Project version */
    ADD_PAIR("project", lisp_make_string(PROJECT_VERSION));

    /* Core dependencies */
    ADD_PAIR("libvterm", lisp_make_string(LIBVTERM_VERSION));
    ADD_PAIR("sdl2", lisp_make_string(SDL2_VERSION));
    ADD_PAIR("sdl2-ttf", lisp_make_string(SDL2_TTF_VERSION));
    ADD_PAIR("bdw-gc", lisp_make_string(GC_VERSION));
    ADD_PAIR("pcre2", lisp_make_string(PCRE2_VERSION));

    /* Build system */
    ADD_PAIR("cmake", lisp_make_string(CMAKE_VERSION));

    /* System information */
    ADD_PAIR("system", lisp_make_string(SYSTEM_NAME));
    ADD_PAIR("architecture", lisp_make_string(SYSTEM_PROCESSOR));
    ADD_PAIR("toolchain", lisp_make_string(TOOLCHAIN));

#undef ADD_PAIR

    return result;
}

/* Built-in: terminal-scroll-locked?
 * Returns t if terminal scroll is locked (user scrolled back), nil otherwise.
 */
static LispObject *builtin_terminal_scroll_locked_p(LispObject *args, Environment *env) {
    (void)args;
    (void)env;
    if (!registered_terminal) {
        return NIL; /* No terminal = not locked */
    }
    if (terminal_is_scroll_locked(registered_terminal)) {
        return LISP_TRUE;
    }
    return NIL;
}

/**
 * current-time-ms: Return current time in milliseconds
 * Returns the value of SDL_GetTicks() for timer support.
 */
static LispObject *builtin_current_time_ms(LispObject *args, Environment *env) {
    (void)args;
    (void)env;
    return lisp_make_integer((long long)SDL_GetTicks());
}

/* Animation builtin functions */
#if HAVE_RLOTTIE

static LispObject *builtin_animation_load(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("animation-load requires 1 argument (path)");
    }
    LispObject *path_obj = lisp_car(args);
    if (path_obj->type != LISP_STRING) {
        return lisp_make_error("animation-load requires a string path");
    }
    if (!registered_renderer) {
        return lisp_make_error("animation-load: no renderer registered");
    }
    /* Create a new Animation object */
    Animation *anim = animation_create(registered_renderer);
    if (!anim) {
        return lisp_make_error("animation-load: failed to create animation");
    }
    /* Load the animation file */
    int result = animation_load(anim, path_obj->value.string);
    if (result != 0) {
        return lisp_make_error("animation-load: failed to load file");
    }
    /* Return the Animation as a Lisp object */
    return make_animation_object(anim);
}

static LispObject *builtin_animation_unload(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("animation-unload requires 1 argument (animation)");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-unload: first argument must be an animation object");
    }
    animation_unload(anim);
    return NIL;
}

static LispObject *builtin_animation_play(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("animation-play requires 1 argument (animation)");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-play: first argument must be an animation object");
    }
    /* Automatically set as active animation when playing */
    active_animation = anim;
    animation_play(anim);
    return NIL;
}

static LispObject *builtin_animation_pause(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("animation-pause requires 1 argument (animation)");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-pause: first argument must be an animation object");
    }
    animation_pause(anim);
    return NIL;
}

static LispObject *builtin_animation_stop(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("animation-stop requires 1 argument (animation)");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-stop: first argument must be an animation object");
    }
    animation_stop(anim);
    return NIL;
}

static LispObject *builtin_animation_set_speed(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("animation-set-speed requires 2 arguments (animation speed)");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-set-speed: first argument must be an animation object");
    }
    LispObject *speed_obj = lisp_car(lisp_cdr(args));
    float speed = 1.0f;
    if (speed_obj->type == LISP_NUMBER) {
        speed = (float)speed_obj->value.number;
    } else if (speed_obj->type == LISP_INTEGER) {
        speed = (float)speed_obj->value.integer;
    } else {
        return lisp_make_error("animation-set-speed: second argument must be a number");
    }
    animation_set_speed(anim, speed);
    return NIL;
}

static LispObject *builtin_animation_set_loop(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("animation-set-loop requires 1-2 arguments (animation [loop])");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-set-loop: first argument must be an animation object");
    }
    int loop = 1; /* Default to true */
    LispObject *rest = lisp_cdr(args);
    if (rest != NIL) {
        LispObject *loop_obj = lisp_car(rest);
        loop = lisp_is_truthy(loop_obj) ? 1 : 0;
    }
    animation_set_loop(anim, loop);
    return NIL;
}

static LispObject *builtin_animation_seek(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL || lisp_cdr(args) == NIL) {
        return lisp_make_error("animation-seek requires 2 arguments (animation position)");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-seek: first argument must be an animation object");
    }
    LispObject *pos_obj = lisp_car(lisp_cdr(args));
    float pos = 0.0f;
    if (pos_obj->type == LISP_NUMBER) {
        pos = (float)pos_obj->value.number;
    } else if (pos_obj->type == LISP_INTEGER) {
        pos = (float)pos_obj->value.integer;
    } else {
        return lisp_make_error("animation-seek: second argument must be a number (0.0-1.0)");
    }
    animation_seek(anim, pos);
    return NIL;
}

static LispObject *builtin_animation_playing_p(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("animation-playing? requires 1 argument (animation)");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-playing?: first argument must be an animation object");
    }
    if (animation_is_playing(anim)) {
        return LISP_TRUE;
    }
    return NIL;
}

static LispObject *builtin_animation_loaded_p(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("animation-loaded? requires 1 argument (animation)");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-loaded?: first argument must be an animation object");
    }
    if (animation_is_loaded(anim)) {
        return LISP_TRUE;
    }
    return NIL;
}

static LispObject *builtin_animation_position(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("animation-position requires 1 argument (animation)");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-position: first argument must be an animation object");
    }
    return lisp_make_number((double)animation_get_position(anim));
}

static LispObject *builtin_animation_duration(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("animation-duration requires 1 argument (animation)");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-duration: first argument must be an animation object");
    }
    return lisp_make_number((double)animation_get_duration(anim));
}

static LispObject *builtin_animation_set_dim_mode(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("animation-set-dim-mode requires 1-2 arguments (animation [alpha])");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-set-dim-mode: first argument must be an animation object");
    }
    float alpha = 0.7f; /* Default: 70% overlay opacity */
    LispObject *rest = lisp_cdr(args);
    if (rest != NIL) {
        LispObject *alpha_obj = lisp_car(rest);
        if (alpha_obj->type == LISP_NUMBER) {
            alpha = (float)alpha_obj->value.number;
        } else if (alpha_obj->type == LISP_INTEGER) {
            alpha = (float)alpha_obj->value.integer;
        }
    }
    animation_set_visibility_mode(anim, ANIMATION_VISIBILITY_DIM);
    animation_set_dim_alpha(anim, alpha);
    return NIL;
}

static LispObject *builtin_animation_set_transparent_mode(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        return lisp_make_error("animation-set-transparent-mode requires 1-2 arguments (animation [alpha])");
    }
    Animation *anim = get_animation_object(lisp_car(args));
    if (!anim) {
        return lisp_make_error("animation-set-transparent-mode: first argument must be an animation object");
    }
    float alpha = 0.85f; /* Default: 85% terminal bg opacity */
    LispObject *rest = lisp_cdr(args);
    if (rest != NIL) {
        LispObject *alpha_obj = lisp_car(rest);
        if (alpha_obj->type == LISP_NUMBER) {
            alpha = (float)alpha_obj->value.number;
        } else if (alpha_obj->type == LISP_INTEGER) {
            alpha = (float)alpha_obj->value.integer;
        }
    }
    animation_set_visibility_mode(anim, ANIMATION_VISIBILITY_TRANSPARENT);
    animation_set_terminal_alpha(anim, alpha);
    return NIL;
}

/* Set the active animation for rendering */
static LispObject *builtin_animation_set_active(LispObject *args, Environment *env) {
    (void)env;
    if (args == NIL) {
        /* Clear active animation */
        active_animation = NULL;
        return NIL;
    }
    /* Check for nil argument to clear active animation */
    LispObject *arg = lisp_car(args);
    if (arg == NIL) {
        active_animation = NULL;
        return NIL;
    }
    Animation *anim = get_animation_object(arg);
    if (!anim) {
        return lisp_make_error("animation-set-active: argument must be an animation object or nil");
    }
    active_animation = anim;
    return arg; /* Return the animation object */
}

#endif /* HAVE_RLOTTIE */

int lisp_x_init(void) {
    /* Initialize Lisp interpreter */
    if (lisp_init() < 0) {
        fprintf(stderr, "Failed to initialize Lisp interpreter\n");
        return -1;
    }
    lisp_env = env_create_global();
    if (!lisp_env) {
        fprintf(stderr, "Failed to create Lisp environment\n");
        lisp_cleanup();
        return -1;
    }

    /* Allocate static buffer for ANSI stripping */
    ansi_strip_buffer_size = 4096; /* Initial size */
    ansi_strip_buffer = malloc(ansi_strip_buffer_size);
    if (!ansi_strip_buffer) {
        fprintf(stderr, "Failed to allocate ANSI strip buffer\n");
    }

    /* Allocate static buffer for telnet input filter */
    telnet_filter_buffer_size = 4096; /* Initial size */
    telnet_filter_buffer = malloc(telnet_filter_buffer_size);
    if (!telnet_filter_buffer) {
        fprintf(stderr, "Failed to allocate telnet filter buffer\n");
        free(ansi_strip_buffer);
        ansi_strip_buffer = NULL;
        env_free(lisp_env);
        lisp_env = NULL;
        lisp_cleanup();
        return -1;
    }

    /* Allocate static buffer for telnet input filter temporary string conversion */
    telnet_filter_temp_buffer_size = 4096; /* Initial size */
    telnet_filter_temp_buffer = malloc(telnet_filter_temp_buffer_size);
    if (!telnet_filter_temp_buffer) {
        fprintf(stderr, "Failed to allocate telnet filter temp buffer\n");
        free(telnet_filter_buffer);
        telnet_filter_buffer = NULL;
        free(ansi_strip_buffer);
        ansi_strip_buffer = NULL;
        env_free(lisp_env);
        lisp_env = NULL;
        lisp_cleanup();
        return -1;
    }

    /* Allocate static buffer for user input hook result */
    user_input_hook_buffer_size = 4096; /* Initial size */
    user_input_hook_buffer = malloc(user_input_hook_buffer_size);
    if (!user_input_hook_buffer) {
        fprintf(stderr, "Failed to allocate user input hook buffer\n");
        free(telnet_filter_temp_buffer);
        telnet_filter_temp_buffer = NULL;
        free(telnet_filter_buffer);
        telnet_filter_buffer = NULL;
        free(ansi_strip_buffer);
        ansi_strip_buffer = NULL;
        env_free(lisp_env);
        lisp_env = NULL;
        lisp_cleanup();
        return -1;
    }

/* Helper macro to register a builtin and set its symbol's docstring */
#define REGISTER_BUILTIN(name, func, doc)                                                                              \
    do {                                                                                                               \
        LispObject *sym = lisp_intern(name);                                                                           \
        if ((doc) != NULL)                                                                                             \
            sym->value.symbol->docstring = (char *)(doc);                                                              \
        env_define(lisp_env, name, lisp_make_builtin(func, name));                                                     \
    } while (0)

/* Helper macro to register a hook (Lisp lambda) with docstring */
#define REGISTER_HOOK(name, lambda_code, doc)                                                                          \
    do {                                                                                                               \
        LispObject *sym = lisp_intern(name);                                                                           \
        if ((doc) != NULL)                                                                                             \
            sym->value.symbol->docstring = (char *)(doc);                                                              \
        LispObject *hook = lisp_eval_string(lambda_code, lisp_env);                                                    \
        if (hook && hook->type != LISP_ERROR)                                                                          \
            env_define(lisp_env, name, hook);                                                                          \
    } while (0)

    /* Register terminal-echo builtin */
    const char *terminal_echo_doc = "Output text to terminal display (local echo).\n"
                                    "\n"
                                    "## Parameters\n"
                                    "- `text` - String to display in terminal\n"
                                    "\n"
                                    "## Returns\n"
                                    "`nil`\n"
                                    "\n"
                                    "## Description\n"
                                    "Echoes text directly to the terminal display without sending to server.\n"
                                    "Used for local feedback, command echoing, and status messages.\n"
                                    "\n"
                                    "**Behavior:**\n"
                                    "- **GUI mode**: Feeds text to libvterm terminal emulator\n"
                                    "- **Headless mode** (tests): Outputs to stdout\n"
                                    "\n"
                                    "## Examples\n"
                                    "```lisp\n"
                                    "(terminal-echo \"Command sent\\r\\n\")\n"
                                    "; => nil  (displays \"Command sent\" in terminal)\n"
                                    "\n"
                                    "(terminal-echo \"\\033[1;31mError:\\033[0m Operation failed\\r\\n\")\n"
                                    "; => nil  (displays colored error message)\n"
                                    "```\n"
                                    "\n"
                                    "## Notes\n"
                                    "- Use `\\r\\n` for proper line breaks (terminal expects CRLF)\n"
                                    "- Supports ANSI escape codes for colors and formatting\n"
                                    "- Does NOT send to telnet server (local display only)\n"
                                    "- Used by TinTin++ for command echoing\n"
                                    "\n"
                                    "## See Also\n"
                                    "- `telnet-send` - Send text to server\n"
                                    "- `terminal-info` - Get terminal dimensions and info";
    REGISTER_BUILTIN("terminal-echo", builtin_terminal_echo, terminal_echo_doc);

    /* Register input-area-redraw builtin */
    const char *input_area_redraw_doc = "Request redraw of input area and divider.\n"
                                        "\n"
                                        "## Returns\n"
                                        "`nil`\n"
                                        "\n"
                                        "## Description\n"
                                        "Requests a redraw of the input area, including the divider line\n"
                                        "and mode indicators. Call this after modifying `*divider-modes*`\n"
                                        "to ensure the display is updated.\n"
                                        "\n"
                                        "## See Also\n"
                                        "- `divider-mode-set` - Set a divider mode indicator\n"
                                        "- `divider-mode-remove` - Remove a divider mode indicator";
    REGISTER_BUILTIN("input-area-redraw", builtin_input_area_redraw, input_area_redraw_doc);

    /* Register telnet-send builtin */
    const char *telnet_send_doc = "Send text to telnet server.\n"
                                  "\n"
                                  "## Parameters\n"
                                  "- `text` - String to send (CRLF automatically appended)\n"
                                  "\n"
                                  "## Returns\n"
                                  "`nil` on success, error object on failure\n"
                                  "\n"
                                  "## Description\n"
                                  "Sends text to the telnet server with automatic CRLF (\\\\r\\\\n) appended.\n"
                                  "Used to transmit commands, responses, and data to the remote server.\n"
                                  "\n"
                                  "**Automatic CRLF:**\n"
                                  "- Input: `\\\"look\\\"` → Sent: `\\\"look\\\\r\\\\n\\\"`\n"
                                  "- Telnet protocol requires CRLF line endings\n"
                                  "- Do NOT manually append CRLF (will result in double line breaks)\n"
                                  "\n"
                                  "## Examples\n"
                                  "```lisp\n"
                                  "; Send single command\n"
                                  "(telnet-send \"look\")\n"
                                  "; => nil  (sends \"look\\\\r\\\\n\" to server)\n"
                                  "\n"
                                  "; Send with arguments\n"
                                  "(telnet-send \"say Hello world\")\n"
                                  "; => nil  (sends \"say Hello world\\\\r\\\\n\")\n"
                                  "\n"
                                  "; Multiple commands require multiple calls\n"
                                  "(telnet-send \"north\")\n"
                                  "(telnet-send \"south\")\n"
                                  "; => Sends two separate commands\n"
                                  "```\n"
                                  "\n"
                                  "## Notes\n"
                                  "- CRLF automatically appended (do NOT include in text)\n"
                                  "- Requires active telnet connection (registered via `lisp_x_register_telnet`)\n"
                                  "- Does NOT echo to terminal (use `terminal-echo` for local display)\n"
                                  "- Used by TinTin++ and user-input-hook for command transmission\n"
                                  "- Returns error if no telnet connection registered\n"
                                  "\n"
                                  "## See Also\n"
                                  "- `terminal-echo` - Echo text locally\n"
                                  "- `user-input-hook` - Hook that uses this for command sending";
    REGISTER_BUILTIN("telnet-send", builtin_telnet_send, telnet_send_doc);

    /* Register terminal-info builtin */
    const char *terminal_info_doc =
        "Get terminal dimensions and configuration information.\n"
        "\n"
        "## Parameters\n"
        "None\n"
        "\n"
        "## Returns\n"
        "Association list with terminal properties, or error if components not registered.\n"
        "\n"
        "## Description\n"
        "Returns detailed information about terminal dimensions, font metrics, scrollback,\n"
        "and libvterm version. Used by table formatting, layout calculations, and diagnostics.\n"
        "\n"
        "**Returned Properties (association list):**\n"
        "\n"
        "**Terminal Dimensions:**\n"
        "- `cols` - Terminal width in characters (integer)\n"
        "- `rows` - Terminal height in characters (integer)\n"
        "\n"
        "**Cell Metrics:**\n"
        "- `cell-width` - Character cell width in pixels (integer)\n"
        "- `cell-height` - Character cell height in pixels (integer)\n"
        "- `line-height` - Line height multiplier (number, default: 1.0)\n"
        "\n"
        "**Window Dimensions:**\n"
        "- `window-width` - Window width in pixels (integer)\n"
        "- `window-height` - Window height in pixels (integer)\n"
        "\n"
        "**Scrollback:**\n"
        "- `viewport-offset` - Current scroll position (0 = bottom) (integer)\n"
        "- `scrollback-size` - Lines currently in scrollback buffer (integer)\n"
        "- `max-scrollback` - Maximum scrollback lines (0 = unbounded) (integer)\n"
        "\n"
        "**Font:**\n"
        "- `font-path` - Path to font file (string)\n"
        "- `font-name` - Font family name (string)\n"
        "\n"
        "**Version:**\n"
        "- `libvterm-version` - libvterm library version (string)\n"
        "\n"
        "## Examples\n"
        "```lisp\n"
        "; Get terminal info\n"
        "(define info (terminal-info))\n"
        "; => ((cols . 80) (rows . 24) (cell-width . 10) ...)\n"
        "\n"
        "; Extract specific values\n"
        "(define cols (cdr (assoc 'cols info)))\n"
        "; => 80\n"
        "\n"
        "(define rows (cdr (assoc 'rows info)))\n"
        "; => 24\n"
        "\n"
        "; Get line height multiplier\n"
        "(define line-height (cdr (assoc 'line-height info)))\n"
        "; => 1.0  (normal spacing, or 1.5 if set to 1.5x)\n"
        "\n"
        "; Check scrollback position\n"
        "(define offset (cdr (assoc 'viewport-offset info)))\n"
        "; => 0  (at bottom)\n"
        "\n"
        "; Get font info\n"
        "(define font (cdr (assoc 'font-name info)))\n"
        "; => \"DejaVu Sans Mono\"\n"
        "```\n"
        "\n"
        "## Notes\n"
        "- Used by `tintin-print-table` for automatic width calculations\n"
        "- Terminal dimensions update on window resize\n"
        "- Cell metrics depend on font size and DPI settings\n"
        "- Scrollback values change as buffer fills and user scrolls\n"
        "- Returns error if terminal, glyph cache, or window not registered\n"
        "\n"
        "## See Also\n"
        "- `tintin-calculate-optimal-widths` - Uses terminal width for table layout\n"
        "- `tintin-print-table` - Uses this for responsive table formatting";
    REGISTER_BUILTIN("terminal-info", builtin_terminal_info, terminal_info_doc);

    /* version: Return version information for all dependencies */
    const char *version_doc =
        "Return version information for all dependencies and system.\n"
        "\n"
        "Returns an association list with the following keys:\n"
        "  - project: TelnetLisp version\n"
        "  - libvterm: libvterm version (with feature flags)\n"
        "  - sdl2: SDL2 version\n"
        "  - sdl2-ttf: SDL2_ttf version\n"
        "  - bdw-gc: Boehm GC version\n"
        "  - pcre2: PCRE2 version\n"
        "  - cmake: CMake version used for build\n"
        "  - system: Operating system name\n"
        "  - architecture: System processor architecture\n"
        "  - toolchain: Full toolchain details (e.g., \"UCRT64 (GCC 13.2.0, x86_64-w64-mingw32-ucrt)\")\n"
        "\n"
        "## Returns\n"
        "Association list: ((key . \"version\") ...)\n"
        "\n"
        "## Examples\n"
        "```lisp\n"
        "(version)\n"
        "; => ((project . \"1.0.0\") (libvterm . \"0.3.3+pushline4\") ...)\n"
        "\n"
        "; Get specific version:\n"
        "(cdr (assoc 'libvterm (version)))\n"
        "; => \"0.3.3+pushline4\"\n"
        "```\n";
    REGISTER_BUILTIN("version", builtin_version, version_doc);

    /* terminal-scroll-locked?: Check if terminal scroll is locked */
    const char *terminal_scroll_locked_doc = "Check if terminal scroll is locked (user scrolled back from bottom).\n"
                                             "\n"
                                             "## Returns\n"
                                             "`t` if scroll locked, `nil` if at bottom\n"
                                             "\n"
                                             "## Description\n"
                                             "Returns true when the user has scrolled up from the current position,\n"
                                             "preventing auto-scroll. Returns nil when viewing the live terminal\n"
                                             "output at the bottom of the scrollback.\n"
                                             "\n"
                                             "## Examples\n"
                                             "```lisp\n"
                                             "(if (terminal-scroll-locked?)\n"
                                             "    (terminal-echo \"[Scroll locked]\\r\\n\")\n"
                                             "    (terminal-echo \"[At live output]\\r\\n\"))\n"
                                             "```\n";
    REGISTER_BUILTIN("terminal-scroll-locked?", builtin_terminal_scroll_locked_p, terminal_scroll_locked_doc);

    /* current-time-ms: Return current time in milliseconds for timer support */
    const char *current_time_ms_doc = "Return current time in milliseconds.\n"
                                      "\n"
                                      "## Returns\n"
                                      "Integer - milliseconds since application start (from SDL_GetTicks)\n"
                                      "\n"
                                      "## Description\n"
                                      "Returns the number of milliseconds since the SDL library was initialized.\n"
                                      "Used by the timer system to track when timers should fire.\n"
                                      "\n"
                                      "## Examples\n"
                                      "```lisp\n"
                                      "(current-time-ms)\n"
                                      "; => 45230  (45.23 seconds since start)\n"
                                      "```\n"
                                      "\n"
                                      "## See Also\n"
                                      "- `run-at-time` - Schedule a timer\n"
                                      "- `run-timers` - Execute due timers";
    REGISTER_BUILTIN("current-time-ms", builtin_current_time_ms, current_time_ms_doc);

#if HAVE_RLOTTIE
    /* Initialize animation type symbol for Lisp object wrapping */
    sym_animation_type = lisp_make_symbol("animation-type");

    /* Register animation builtins */
    REGISTER_BUILTIN("animation-load", builtin_animation_load,
                     "Load a Lottie animation file.\n\n(animation-load path) => animation object");
    REGISTER_BUILTIN("animation-unload", builtin_animation_unload,
                     "Unload an animation.\n\n(animation-unload anim) => nil");
    REGISTER_BUILTIN("animation-play", builtin_animation_play,
                     "Start playing an animation and set it as active.\n\n(animation-play anim) => nil\n\n"
                     "Automatically sets the animation as the active background animation.");
    REGISTER_BUILTIN("animation-pause", builtin_animation_pause,
                     "Pause an animation.\n\n(animation-pause anim) => nil");
    REGISTER_BUILTIN("animation-stop", builtin_animation_stop,
                     "Stop an animation and reset to beginning.\n\n(animation-stop anim) => nil");
    REGISTER_BUILTIN("animation-set-speed", builtin_animation_set_speed,
                     "Set animation playback speed.\n\n(animation-set-speed anim multiplier) => nil\n\n1.0 "
                     "= normal, 0.5 = half speed, 2.0 = double speed");
    REGISTER_BUILTIN("animation-set-loop", builtin_animation_set_loop,
                     "Enable or disable looping.\n\n(animation-set-loop anim [enabled]) => nil\n\nIf "
                     "enabled is truthy (default t), animation loops.");
    REGISTER_BUILTIN("animation-seek", builtin_animation_seek,
                     "Seek to a position in the animation.\n\n(animation-seek anim position) => "
                     "nil\n\nPosition is 0.0 to 1.0 (start to end).");
    REGISTER_BUILTIN("animation-playing?", builtin_animation_playing_p,
                     "Check if animation is currently playing.\n\n(animation-playing? anim) => t or nil");
    REGISTER_BUILTIN("animation-loaded?", builtin_animation_loaded_p,
                     "Check if an animation file is loaded.\n\n(animation-loaded? anim) => t or nil");
    REGISTER_BUILTIN("animation-position", builtin_animation_position,
                     "Get current playback position.\n\n(animation-position anim) => number (0.0 to 1.0)");
    REGISTER_BUILTIN("animation-duration", builtin_animation_duration,
                     "Get animation duration in seconds.\n\n(animation-duration anim) => number");
    REGISTER_BUILTIN("animation-set-dim-mode", builtin_animation_set_dim_mode,
                     "Set visibility to dim mode (overlay on animation).\n\n(animation-set-dim-mode anim [alpha]) "
                     "=> nil\n\nAlpha is overlay opacity (0.0-1.0, default 0.7). Higher = more dim.");
    REGISTER_BUILTIN("animation-set-transparent-mode", builtin_animation_set_transparent_mode,
                     "Set visibility to transparent mode (see-through terminal).\n\n(animation-set-transparent-mode "
                     "anim [alpha]) => nil\n\nAlpha is terminal background opacity (0.0-1.0, default 0.85).");
    REGISTER_BUILTIN("animation-set-active", builtin_animation_set_active,
                     "Set the active animation for rendering.\n\n(animation-set-active anim) => "
                     "anim\n(animation-set-active nil) => nil\n\nThe active animation is rendered behind terminal "
                     "text.");
#endif

    /* Note: divider-mode-set and divider-mode-remove are defined in bootstrap.lisp */

#undef REGISTER_BUILTIN

    /* Define hook stubs - no-op implementations overridden by bootstrap.lisp
     * These establish the C contract: telnet-gui calls these hooks at specific points.
     * bootstrap.lisp redefines them with actual implementations using run-hook. */
    const char *telnet_input_hook_doc = "Process telnet server output through registered hooks.\n"
                                        "\n"
                                        "## Parameters\n"
                                        "- `text` - Plain text from server (ANSI codes already stripped)\n"
                                        "\n"
                                        "## Returns\n"
                                        "`nil` (side-effect only hook)\n"
                                        "\n"
                                        "## Description\n"
                                        "Called by C code when text is received from the telnet server. Dispatches\n"
                                        "to all functions registered with `(add-hook 'telnet-input-hook ...)`.\n"
                                        "\n"
                                        "This hook is for side effects only (logging, word collection, etc.).\n"
                                        "To transform the data before display, use `telnet-input-filter-hook`.\n"
                                        "\n"
                                        "## Default Handlers\n"
                                        "- Word collection for tab completion\n"
                                        "- Scroll-lock notification animation\n"
                                        "\n"
                                        "## See Also\n"
                                        "- `add-hook` - Register a handler\n"
                                        "- `telnet-input-filter-hook` - Transform data before display";
    REGISTER_HOOK("telnet-input-hook", "(lambda (text) nil)", telnet_input_hook_doc);

    const char *user_input_hook_doc = "Transform user input before sending to telnet server.\n"
                                      "\n"
                                      "## Parameters\n"
                                      "- `text` - The text user typed in input area\n"
                                      "- `cursor-pos` - Cursor position in input area (integer)\n"
                                      "\n"
                                      "## Returns\n"
                                      "- String: C code echoes and sends the returned text\n"
                                      "- `nil` or empty string: Hook handled everything (no further action)\n"
                                      "\n"
                                      "## Description\n"
                                      "Called by C code when user presses Enter. Dispatches to all functions\n"
                                      "registered with `(add-hook 'user-input-hook ...)`.\n"
                                      "\n"
                                      "Handlers can set `*user-input-handled*` to `#t` and `*user-input-result*`\n"
                                      "to control the return value. First handler to set these wins.\n"
                                      "\n"
                                      "## See Also\n"
                                      "- `add-hook` - Register a handler\n"
                                      "- `*user-input-handled*` - Flag to indicate input was handled\n"
                                      "- `*user-input-result*` - Result when handled";
    REGISTER_HOOK("user-input-hook", "(lambda (text cursor-pos) text)", user_input_hook_doc);

    const char *telnet_input_filter_hook_doc =
        "Transform telnet server output before displaying in terminal.\n"
        "\n"
        "## Parameters\n"
        "- `text` - Raw telnet data from server (includes ANSI escape codes)\n"
        "\n"
        "## Returns\n"
        "String to display in terminal. If non-string returned, original text is used.\n"
        "\n"
        "## Description\n"
        "Called BEFORE displaying text in the terminal, allowing transformation,\n"
        "filtering, or replacement of telnet server output. The returned string is\n"
        "what actually gets displayed.\n"
        "\n"
        "Unlike `telnet-input-hook`, this hook TRANSFORMS the data flow.\n"
        "\n"
        "## See Also\n"
        "- `telnet-input-hook` - Side-effect hook (word collection, logging)";
    REGISTER_HOOK("telnet-input-filter-hook", "(lambda (text) text)", telnet_input_filter_hook_doc);

    const char *completion_hook_doc = "Provide tab completion candidates for partial text.\n"
                                      "\n"
                                      "## Parameters\n"
                                      "- `text` - Partial text to complete (matched by `*completion-pattern*`)\n"
                                      "\n"
                                      "## Returns\n"
                                      "List of completion candidate strings, or empty list `()` if no matches.\n"
                                      "\n"
                                      "## Description\n"
                                      "Called by C code when user presses TAB. Searches the word store for\n"
                                      "words matching the given prefix (case-insensitive). Results returned\n"
                                      "in newest-first order.\n"
                                      "\n"
                                      "## See Also\n"
                                      "- `*completion-pattern*` - Regex for matching partial text\n"
                                      "- `get-completions-from-store` - Internal search function";
    REGISTER_HOOK("completion-hook", "(lambda (text) ())", completion_hook_doc);

#undef REGISTER_HOOK

    /* Define configuration variables accessed from C (bootstrap.lisp documents with defvar) */
    env_define(lisp_env, "*scroll-lines-per-click*", lisp_make_integer(3));
    env_define(lisp_env, "*smooth-scrolling-enabled*", lisp_make_boolean(1));
    env_define(lisp_env, "*max-scrollback-lines*", lisp_make_integer(0));
    env_define(lisp_env, "*scroll-to-bottom-on-user-input*", lisp_make_boolean(1));
    env_define(lisp_env, "*input-history-size*", lisp_make_integer(100));
    env_define(lisp_env, "*terminal-line-height*", lisp_make_number(1.2));
    env_define(lisp_env, "*divider-modes*", NIL);

    /* Load bootstrap file - redefines hooks with actual implementations.
     * Hooks and variables are already defined above, so app works even if bootstrap fails. */
    if (!load_bootstrap_file()) {
        fprintf(stderr, "Warning: Failed to load bootstrap file, continuing with defaults\n");
    }

    return 0;
}

/* file_exists() is now in path_utils.h */

int lisp_x_load_file(const char *filepath) {
    if (!lisp_env || !filepath) {
        return -1;
    }

    /* Check if filepath is absolute or contains directory separators */
    /* If so, use it as-is without searching */
    int has_path_separator = 0;
#ifdef _WIN32
    if (strchr(filepath, '\\') || strchr(filepath, '/') || strchr(filepath, ':')) {
        has_path_separator = 1;
    }
#else
    if (strchr(filepath, '/')) {
        has_path_separator = 1;
    }
#endif

    if (has_path_separator) {
        /* Path contains directory separator - use as-is */
        LispObject *result = lisp_load_file(filepath, lisp_env);
        if (result && result->type == LISP_ERROR) {
            char *err_str = lisp_print(result);
            fprintf(stderr, "Error loading Lisp file '%s': %s\n", filepath, err_str);
            return -1;
        }
        fprintf(stderr, "Loaded Lisp file: %s\n", filepath);

        /* Echo loaded filename to terminal */
        if (registered_terminal) {
            char message[1024];
            snprintf(message, sizeof(message), "Loaded: %s\r\n", filepath);
            terminal_feed_data(registered_terminal, message, strlen(message));
        }

        return 0;
    }

    /* Filename only - search multiple locations */
    fprintf(stderr, "Lisp file resolution: Starting search for %s...\n", filepath);

    /* Get executable base path */
    char *base_path = SDL_GetBasePath();

    /* Build search paths array */
    const char *search_paths[10];
    const char *search_labels[10];
    int search_count = 0;

    /* PRIORITY 1: Build/development paths (checked first for development workflow) */
    /* Path 1: Executable-relative (build directory during development) */
    /* Skip if executable is in a bin directory (installed location) - .lisp files aren't in bin */
    if (base_path) {
        static char exe_rel_path[1024];
        if (path_construct_exe_relative(base_path, filepath, exe_rel_path, sizeof(exe_rel_path))) {
            search_paths[search_count] = exe_rel_path;
            search_labels[search_count] = "executable-relative (build/development)";
            search_count++;
            fprintf(stderr, "Lisp file resolution: Executable base path: %s\n", base_path);
            fprintf(stderr, "Lisp file resolution: Constructed path: %s\n", exe_rel_path);
        } else {
            fprintf(stderr, "Lisp file resolution: Executable in bin directory (installed), skipping bin path check\n");
        }
        SDL_free(base_path);
    } else {
        fprintf(stderr, "Lisp file resolution: Warning - SDL_GetBasePath() returned NULL\n");
    }

    /* Path 2: lisp subdirectory (consistent with bootstrap) */
    static char lisp_subdir_path[256];
    snprintf(lisp_subdir_path, sizeof(lisp_subdir_path), "lisp/%s", filepath);
    search_paths[search_count] = lisp_subdir_path;
    search_labels[search_count] = "lisp subdirectory relative";
    search_count++;

    /* Path 3: Parent's lisp subdirectory (for nested builds) */
    static char parent_lisp_path[256];
    snprintf(parent_lisp_path, sizeof(parent_lisp_path), "../lisp/%s", filepath);
    search_paths[search_count] = parent_lisp_path;
    search_labels[search_count] = "parent lisp subdirectory";
    search_count++;

    /* Path 4: Current working directory (for custom user scripts) */
    search_paths[search_count] = filepath;
    search_labels[search_count] = "current working directory";
    search_count++;

    /* PRIORITY 2: Installed path (fallback for installed builds) */
    static char installed_lisp_path[TELNET_MAX_PATH];
    if (path_construct_installed_resource("lisp", filepath, installed_lisp_path, sizeof(installed_lisp_path))) {
        search_paths[search_count] = installed_lisp_path;
        search_labels[search_count] = "installed data directory";
        search_count++;
        fprintf(stderr, "Lisp file resolution: Installed path available: %s\n", installed_lisp_path);
    }

    /* Try each path in order */
    for (int i = 0; i < search_count; i++) {
        fprintf(stderr, "Lisp file resolution: Trying [%d] %s: %s\n", i + 1, search_labels[i], search_paths[i]);

        if (file_exists(search_paths[i])) {
            fprintf(stderr, "Lisp file resolution: File exists, attempting to load...\n");

            LispObject *result = lisp_load_file(search_paths[i], lisp_env);
            if (result && result->type == LISP_ERROR) {
                char *err_str = lisp_print(result);
                fprintf(stderr, "Lisp file resolution: Failed to load: %s\n", err_str);
                continue; /* Try next path */
            }

            fprintf(stderr, "Lisp file resolution: SUCCESS! Loaded from [%d] %s\n", i + 1, search_labels[i]);
            fprintf(stderr, "Lisp file resolution: File path: %s\n", search_paths[i]);

            /* Echo loaded filename to terminal */
            if (registered_terminal) {
                char message[1024];
                snprintf(message, sizeof(message), "Loaded: %s\r\n", filepath);
                terminal_feed_data(registered_terminal, message, strlen(message));
            }

            return 0;
        } else {
            fprintf(stderr, "Lisp file resolution: File does not exist, skipping...\n");
        }
    }

    /* All paths exhausted */
    fprintf(stderr, "Lisp file resolution: ERROR - Failed to find file in any search path\n");
    fprintf(stderr, "Error loading Lisp file '%s': File not found in search paths\n", filepath);
    return -1;
}

void lisp_x_cleanup(void) {
    if (lisp_env) {
        env_free(lisp_env);
        lisp_env = NULL;
    }
    if (ansi_strip_buffer) {
        free(ansi_strip_buffer);
        ansi_strip_buffer = NULL;
        ansi_strip_buffer_size = 0;
    }
    if (telnet_filter_buffer) {
        free(telnet_filter_buffer);
        telnet_filter_buffer = NULL;
        telnet_filter_buffer_size = 0;
    }
    if (telnet_filter_temp_buffer) {
        free(telnet_filter_temp_buffer);
        telnet_filter_temp_buffer = NULL;
        telnet_filter_temp_buffer_size = 0;
    }
    if (user_input_hook_buffer) {
        free(user_input_hook_buffer);
        user_input_hook_buffer = NULL;
        user_input_hook_buffer_size = 0;
    }
    lisp_cleanup();
}

/* Free all tab completion strings */
static void free_tab_completions(void) {
    for (int i = 0; i < tab_completion_count; i++) {
        if (tab_completions[i]) {
            free(tab_completions[i]);
            tab_completions[i] = NULL;
        }
    }
    tab_completion_count = 0;
}

/* Exit tab mode and cleanup */
static void exit_tab_mode(void) {
    free_tab_completions();
    tab_mode_active = 0;
    tab_completion_index = 0;
    tab_start_pos = 0;
    tab_partial_len = 0;
}

/* Enter tab mode with completions from Lisp list */
static int enter_tab_mode(LispObject *completions, const char *buffer, int cursor_pos, int length, int start_pos,
                          int partial_len) {
    if (!completions || completions == NIL || completions->type != LISP_CONS || !buffer) {
        return 0;
    }

    /* Free any existing completions */
    free_tab_completions();

    /* Save original buffer state */
    int save_len = length < TAB_BUFFER_SIZE ? length : TAB_BUFFER_SIZE - 1;
    memcpy(tab_original_buffer, buffer, save_len);
    tab_original_buffer[save_len] = '\0';
    tab_original_cursor_pos = cursor_pos;
    tab_original_length = length;
    tab_start_pos = start_pos;
    tab_partial_len = partial_len;

    /* Convert Lisp list to array of strings */
    LispObject *current = completions;
    while (current != NIL && current->type == LISP_CONS && tab_completion_count < MAX_COMPLETIONS) {
        LispObject *car = current->value.cons.car;
        if (car && car->type == LISP_STRING) {
            tab_completions[tab_completion_count] = strdup(car->value.string);
            if (tab_completions[tab_completion_count]) {
                tab_completion_count++;
            }
        }
        current = current->value.cons.cdr;
    }

    if (tab_completion_count == 0) {
        return 0; /* No valid completions */
    }

    tab_mode_active = 1;
    tab_completion_index = 0;
    return 1;
}

/* Apply current completion to buffer */
static void apply_current_completion(char *buffer, int buffer_size, int *cursor_pos, int *length) {
    if (!tab_mode_active || tab_completion_index >= tab_completion_count || !buffer || !cursor_pos || !length) {
        return;
    }

    const char *completion = tab_completions[tab_completion_index];
    if (!completion) {
        return;
    }

    /* Replace partial text with current completion */
    replace_partial_text(buffer, buffer_size, cursor_pos, length, tab_start_pos, tab_partial_len, completion);

    /* Update partial_len to match the completion we just inserted, so next cycle replaces the whole completion */
    tab_partial_len = strlen(completion);
}

/* Cycle to next completion */
static void cycle_next_completion(char *buffer, int buffer_size, int *cursor_pos, int *length, int *needs_redraw) {
    if (!tab_mode_active || tab_completion_count == 0) {
        return;
    }

    tab_completion_index = (tab_completion_index + 1) % tab_completion_count;
    apply_current_completion(buffer, buffer_size, cursor_pos, length);
    if (needs_redraw) {
        *needs_redraw = 1;
    }
}

void lisp_x_handle_tab(char *buffer, int buffer_size, int *cursor_pos, int *length, int *needs_redraw) {
    if (!buffer || !cursor_pos || !length || *cursor_pos < 0 || *cursor_pos > *length) {
        return;
    }

    /* If already in tab mode, cycle to next completion */
    if (tab_mode_active) {
        cycle_next_completion(buffer, buffer_size, cursor_pos, length, needs_redraw);
        return;
    }

    /* Not in tab mode - extract partial text and get completions */
    int start_pos, partial_len;
    if (!extract_partial_text(buffer, *cursor_pos, *length, &start_pos, &partial_len)) {
        return;
    }

    /* Extract partial text using stack buffer */
    char partial_buf[1024];
    int copy_len = partial_len;
    if ((size_t)copy_len >= sizeof(partial_buf)) {
        copy_len = (int)(sizeof(partial_buf) - 1);
    }
    memcpy(partial_buf, &buffer[start_pos], copy_len);
    partial_buf[copy_len] = '\0';

    /* Call Lisp completion hook */
    LispObject *completions = call_completion_hook(partial_buf);
    if (!completions || completions == NIL || completions->type != LISP_CONS) {
        return; /* No completions */
    }

    /* Count completions */
    int count = 0;
    LispObject *current = completions;
    while (current != NIL && current->type == LISP_CONS && count < MAX_COMPLETIONS) {
        LispObject *car = current->value.cons.car;
        if (car && car->type == LISP_STRING) {
            count++;
        }
        current = current->value.cons.cdr;
    }

    if (count == 0) {
        return; /* No valid completions */
    } else if (count == 1) {
        /* Single completion - apply it directly without entering tab mode */
        LispObject *first = completions->value.cons.car;
        if (first && first->type == LISP_STRING) {
            replace_partial_text(buffer, buffer_size, cursor_pos, length, start_pos, partial_len, first->value.string);
            if (needs_redraw) {
                *needs_redraw = 1;
            }
        }
    } else {
        /* Multiple completions - enter tab mode and apply first */
        if (enter_tab_mode(completions, buffer, *cursor_pos, *length, start_pos, partial_len)) {
            apply_current_completion(buffer, buffer_size, cursor_pos, length);
            if (needs_redraw) {
                *needs_redraw = 1;
            }
        }
    }
}

/* Check if tab mode is active */
int lisp_x_is_tab_mode_active(void) {
    return tab_mode_active;
}

/* Accept current completion and exit tab mode */
void lisp_x_accept_tab_completion(void) {
    if (tab_mode_active) {
        exit_tab_mode();
    }
}

/* Cancel tab mode and revert to original buffer */
void lisp_x_cancel_tab_completion(char *buffer, int buffer_size, int *cursor_pos, int *length, int *needs_redraw) {
    if (!tab_mode_active || !buffer || !cursor_pos || !length) {
        return;
    }

    /* Restore original buffer state */
    int restore_len = tab_original_length < buffer_size ? tab_original_length : buffer_size - 1;
    memcpy(buffer, tab_original_buffer, restore_len);
    buffer[restore_len] = '\0';
    *length = tab_original_length;
    *cursor_pos = tab_original_cursor_pos;
    if (needs_redraw) {
        *needs_redraw = 1;
    }

    /* Exit tab mode */
    exit_tab_mode();
}

int lisp_x_get_scroll_lines_per_click(void) {
    if (!lisp_env) {
        return 3; /* Default */
    }

    LispObject *value = env_lookup(lisp_env, "*scroll-lines-per-click*");
    if (!value || value == NIL) {
        return 3; /* Default */
    }

    /* Check if it's an integer */
    if (value->type == LISP_INTEGER) {
        int lines = (int)value->value.integer;
        /* Clamp to reasonable range (1-100) */
        if (lines < 1)
            lines = 1;
        if (lines > 100)
            lines = 100;
        return lines;
    }

    /* If it's a number, convert to integer */
    if (value->type == LISP_NUMBER) {
        int lines = (int)value->value.number;
        if (lines < 1)
            lines = 1;
        if (lines > 100)
            lines = 100;
        return lines;
    }

    /* Invalid type, return default */
    return 3;
}

int lisp_x_get_smooth_scrolling_enabled(void) {
    if (!lisp_env) {
        return 1; /* Default: enabled */
    }

    LispObject *value = env_lookup(lisp_env, "*smooth-scrolling-enabled*");
    if (!value || value == NIL) {
        return 1; /* Default: enabled */
    }

    /* Check if it's a boolean */
    if (value->type == LISP_BOOLEAN) {
        return value->value.boolean ? 1 : 0;
    }

    /* Use truthy check for other types */
    return lisp_is_truthy(value) ? 1 : 0;
}

int lisp_x_get_max_scrollback_lines(void) {
    if (!lisp_env) {
        return 0; /* Default: unbounded */
    }

    LispObject *value = env_lookup(lisp_env, "*max-scrollback-lines*");
    if (!value || value == NIL) {
        return 0; /* Default: unbounded */
    }

    /* Check if it's an integer */
    if (value->type == LISP_INTEGER) {
        int lines = (int)value->value.integer;
        /* Ensure non-negative */
        if (lines < 0)
            lines = 0;
        return lines;
    }

    /* Check if it's a number (convert to integer) */
    if (value->type == LISP_NUMBER) {
        int lines = (int)value->value.number;
        /* Ensure non-negative */
        if (lines < 0)
            lines = 0;
        return lines;
    }

    /* Invalid type, return default */
    return 0;
}

int lisp_x_get_scroll_to_bottom_on_user_input(void) {
    if (!lisp_env) {
        return 1; /* Default: enabled */
    }

    LispObject *value = env_lookup(lisp_env, "*scroll-to-bottom-on-user-input*");
    if (!value || value == NIL) {
        return 1; /* Default: enabled */
    }

    /* Check if it's a boolean */
    if (value->type == LISP_BOOLEAN) {
        return value->value.boolean ? 1 : 0;
    }

    /* Use truthy check for other types */
    return lisp_is_truthy(value) ? 1 : 0;
}

int lisp_x_get_input_history_size(void) {
    if (!lisp_env) {
        return 100; /* Default: 100 entries */
    }

    LispObject *value = env_lookup(lisp_env, "*input-history-size*");
    if (!value || value == NIL) {
        return 100; /* Default: 100 entries */
    }

    /* Check if it's an integer */
    if (value->type == LISP_INTEGER) {
        int size = (int)value->value.integer;
        /* Clamp to reasonable range (10-1000) */
        if (size < 10)
            size = 10;
        if (size > 1000)
            size = 1000;
        return size;
    }

    /* Check if it's a number (convert to integer) */
    if (value->type == LISP_NUMBER) {
        int size = (int)value->value.number;
        /* Clamp to reasonable range (10-1000) */
        if (size < 10)
            size = 10;
        if (size > 1000)
            size = 1000;
        return size;
    }

    /* Invalid type, return default */
    return 100;
}

float lisp_x_get_terminal_line_height(void) {
    if (!lisp_env) {
        return 1.2f; /* Default: 1.2 (20% more spacing for readability) */
    }

    LispObject *value = env_lookup(lisp_env, "*terminal-line-height*");
    if (!value || value == NIL) {
        return 1.2f; /* Default: 1.2 (20% more spacing for readability) */
    }

    /* Check if it's an integer */
    if (value->type == LISP_INTEGER) {
        float height = (float)value->value.integer;
        /* Clamp to reasonable range (0.5 to 3.0) */
        if (height < 0.5f)
            height = 0.5f;
        if (height > 3.0f)
            height = 3.0f;
        return height;
    }

    /* Check if it's a number (float) */
    if (value->type == LISP_NUMBER) {
        float height = (float)value->value.number;
        /* Clamp to reasonable range (0.5 to 3.0) */
        if (height < 0.5f)
            height = 0.5f;
        if (height > 3.0f)
            height = 3.0f;
        return height;
    }

    /* Invalid type, return default */
    return 1.2f;
}

void lisp_x_set_terminal_line_height(float line_height) {
    if (!lisp_env) {
        return;
    }
    /* Clamp to reasonable range */
    if (line_height < 0.5f)
        line_height = 0.5f;
    if (line_height > 3.0f)
        line_height = 3.0f;
    /* Set the variable using env_define (creates or updates) */
    LispObject *line_height_obj = lisp_make_number((double)line_height);
    env_define(lisp_env, "*terminal-line-height*", line_height_obj);
}

void lisp_x_call_telnet_input_hook(const char *text, size_t len) {
    if (!lisp_env || !text || len == 0) {
        return;
    }

    /* Strip ANSI codes from input text */
    size_t stripped_len = 0;
    char *stripped_text = strip_ansi_codes(text, len, &stripped_len);
    if (!stripped_text || stripped_len == 0) {
        return; /* Nothing to process after stripping */
    }

    /* Look up telnet-input-hook function */
    LispObject *hook = env_lookup(lisp_env, "telnet-input-hook");
    if (!hook || (hook->type != LISP_LAMBDA && hook->type != LISP_BUILTIN)) {
        return; /* Hook not found */
    }

    /* Create Lisp string from stripped text */
    LispObject *text_arg = lisp_make_string(stripped_text);
    if (!text_arg || text_arg->type == LISP_ERROR) {
        return; /* Failed to create string */
    }

    /* Create argument list: (text) */
    LispObject *args = lisp_make_cons(text_arg, NIL);

    /* Create function call: (telnet-input-hook "stripped-text") */
    LispObject *call_expr = lisp_make_cons(hook, args);

    /* Evaluate the function call */
    LispObject *result = lisp_eval(call_expr, lisp_env);

    /* Check for errors - log but continue */
    if (result && result->type == LISP_ERROR) {
        char *err_str = lisp_print(result);
        if (err_str) {
            fprintf(stderr, "Error in telnet-input-hook: %s\n", err_str);
        }
    }
    /* Ignore return value - hook is side-effect only */
}

/* Run all due timers - calls (run-timers) in Lisp */
void lisp_x_run_timers(void) {
    if (!lisp_env)
        return;

    /* Look up run-timers function */
    LispObject *fn = env_lookup(lisp_env, "run-timers");
    if (!fn || (fn->type != LISP_LAMBDA && fn->type != LISP_BUILTIN))
        return;

    /* Call (run-timers) */
    LispObject *call = lisp_make_cons(fn, NIL);
    LispObject *result = lisp_eval(call, lisp_env);

    /* Log errors but don't crash */
    if (result && result->type == LISP_ERROR) {
        char *err_str = lisp_print(result);
        if (err_str) {
            fprintf(stderr, "Error in run-timers: %s\n", err_str);
        }
    }
}

const char *lisp_x_call_user_input_hook(const char *text, int cursor_pos) {
    if (!lisp_env || !text) {
        return text; /* Return original if no environment or text */
    }

    /* Look up user-input-hook */
    LispObject *hook = env_lookup(lisp_env, "user-input-hook");
    if (!hook || (hook->type != LISP_LAMBDA && hook->type != LISP_BUILTIN)) {
        return text; /* Hook not found - return original */
    }

    /* Create Lisp string from input text */
    LispObject *text_arg = lisp_make_string(text);
    if (!text_arg || text_arg->type == LISP_ERROR) {
        return text; /* Failed to create string - return original */
    }

    /* Create Lisp integer for cursor position */
    LispObject *cursor_arg = lisp_make_integer(cursor_pos);
    if (!cursor_arg || cursor_arg->type == LISP_ERROR) {
        return text; /* Failed to create integer - return original */
    }

    /* Create argument list: (text cursor-pos) */
    LispObject *args = lisp_make_cons(text_arg, lisp_make_cons(cursor_arg, NIL));

    /* Create function call: (user-input-hook "text" cursor-pos) */
    LispObject *call_expr = lisp_make_cons(hook, args);

    /* Evaluate the function call */
    LispObject *result = lisp_eval(call_expr, lisp_env);

    /* Check for errors */
    if (!result || result->type == LISP_ERROR) {
        char *err_str = lisp_print(result);
        if (err_str) {
            fprintf(stderr, "Error in user-input-hook: %s\n", err_str);

            /* Echo error to terminal if available, replacing \n with \r\n for proper line breaks */
            if (registered_terminal) {
                char error_msg[1024];
                int out_pos = 0;
                const char *prefix = "Error: ";

                /* Add prefix */
                for (const char *p = prefix; *p && out_pos < (int)sizeof(error_msg) - 3; p++) {
                    error_msg[out_pos++] = *p;
                }

                /* Copy error string, replacing \n with \r\n */
                for (const char *p = err_str; *p && out_pos < (int)sizeof(error_msg) - 3; p++) {
                    if (*p == '\n') {
                        error_msg[out_pos++] = '\r';
                        error_msg[out_pos++] = '\n';
                    } else {
                        error_msg[out_pos++] = *p;
                    }
                }

                /* Add final \r\n if not already present */
                if (out_pos < 2 || error_msg[out_pos - 2] != '\r' || error_msg[out_pos - 1] != '\n') {
                    if (out_pos < (int)sizeof(error_msg) - 2) {
                        error_msg[out_pos++] = '\r';
                        error_msg[out_pos++] = '\n';
                    }
                }
                error_msg[out_pos] = '\0';

                terminal_feed_data(registered_terminal, error_msg, out_pos);
            }
        }
        /* If error occurred in TinTin++ command (starts with #), suppress sending to telnet */
        if (text && text[0] == '#') {
            return ""; /* TinTin++ command error - don't send to telnet */
        }
        return text; /* Error - return original for non-TinTin++ commands */
    }

    /* Hook contract: non-string (nil) or empty string = hook handled everything */
    if (result->type != LISP_STRING) {
        return ""; /* Non-string (nil) - hook handled echo/send */
    }

    /* Get the transformed string from Lisp result */
    const char *transformed = result->value.string;
    size_t transformed_len = strlen(transformed);

    /* Ensure hook buffer is large enough (double on demand) */
    if (ensure_buffer_size(&user_input_hook_buffer, &user_input_hook_buffer_size, transformed_len + 1) < 0) {
        return text; /* Allocation failed - return original */
    }

    /* Copy transformed string to hook buffer */
    memcpy(user_input_hook_buffer, transformed, transformed_len);
    user_input_hook_buffer[transformed_len] = '\0';

    /* Return the transformed result */
    return user_input_hook_buffer;
}

const char *lisp_x_call_telnet_input_filter_hook(const char *text, size_t len, size_t *out_len) {
    if (!lisp_env || !text || len == 0 || !out_len) {
        if (out_len)
            *out_len = len;
        return text; /* Return original if no environment or text */
    }

    /* Look up telnet-input-filter-hook */
    LispObject *hook = env_lookup(lisp_env, "telnet-input-filter-hook");
    if (!hook || (hook->type != LISP_LAMBDA && hook->type != LISP_BUILTIN)) {
        *out_len = len;
        return text; /* Hook not found - return original */
    }

    /* Create null-terminated string from input text for Lisp (telnet data should be text-like) */
    /* Ensure temp buffer is large enough (double on demand) */
    if (ensure_buffer_size(&telnet_filter_temp_buffer, &telnet_filter_temp_buffer_size, len + 1) < 0) {
        *out_len = len;
        return text; /* Allocation failed - return original */
    }

    /* Copy input text to temp buffer and null-terminate */
    memcpy(telnet_filter_temp_buffer, text, len);
    telnet_filter_temp_buffer[len] = '\0';

    /* Create Lisp string from input text */
    LispObject *arg = lisp_make_string(telnet_filter_temp_buffer);
    if (!arg || arg->type == LISP_ERROR) {
        *out_len = len;
        return text; /* Failed to create string - return original */
    }

    /* Create argument list */
    LispObject *args = lisp_make_cons(arg, NIL);

    /* Create function call: (telnet-input-filter-hook "text") */
    LispObject *call_expr = lisp_make_cons(hook, args);

    /* Evaluate the function call */
    LispObject *result = lisp_eval(call_expr, lisp_env);

    /* Check for errors */
    if (!result || result->type == LISP_ERROR) {
        char *err_str = lisp_print(result);
        if (err_str) {
            fprintf(stderr, "Error in telnet-input-filter-hook: %s\n", err_str);
        }
        *out_len = len;
        return text; /* Error - return original */
    }

    /* Check if result is a string */
    if (result->type != LISP_STRING) {
        fprintf(stderr, "Warning: telnet-input-filter-hook returned non-string, using original text\n");
        *out_len = len;
        return text; /* Not a string - return original */
    }

    /* Get the transformed string from Lisp result */
    const char *transformed = result->value.string;
    size_t transformed_len = strlen(transformed);

    /* Ensure filter buffer is large enough (double on demand) */
    if (ensure_buffer_size(&telnet_filter_buffer, &telnet_filter_buffer_size, transformed_len + 1) < 0) {
        *out_len = len;
        return text; /* Allocation failed - return original */
    }

    /* Copy transformed string to filter buffer */
    memcpy(telnet_filter_buffer, transformed, transformed_len);
    telnet_filter_buffer[transformed_len] = '\0';
    *out_len = transformed_len;

    /* Return the filtered result */
    return telnet_filter_buffer;
}

/* Helper: Extract RGB from Lisp list (r g b) or return default */
static void get_color_from_lisp(const char *var_name, int *r, int *g, int *b, int default_r, int default_g,
                                int default_b) {
    /* Set defaults */
    *r = default_r;
    *g = default_g;
    *b = default_b;

    if (!lisp_env) {
        return;
    }

    LispObject *value = env_lookup(lisp_env, var_name);
    if (!value || value == NIL || value->type != LISP_CONS) {
        return; /* Not found or not a list */
    }

    /* Extract (r g b) from list */
    LispObject *r_obj = lisp_car(value);
    LispObject *rest = lisp_cdr(value);
    if (!rest || rest->type != LISP_CONS)
        return;

    LispObject *g_obj = lisp_car(rest);
    rest = lisp_cdr(rest);
    if (!rest || rest->type != LISP_CONS)
        return;

    LispObject *b_obj = lisp_car(rest);

    /* Convert to integers */
    if (r_obj && (r_obj->type == LISP_INTEGER || r_obj->type == LISP_NUMBER)) {
        int val = (r_obj->type == LISP_INTEGER) ? (int)r_obj->value.integer : (int)r_obj->value.number;
        if (val >= 0 && val <= 255)
            *r = val;
    }

    if (g_obj && (g_obj->type == LISP_INTEGER || g_obj->type == LISP_NUMBER)) {
        int val = (g_obj->type == LISP_INTEGER) ? (int)g_obj->value.integer : (int)g_obj->value.number;
        if (val >= 0 && val <= 255)
            *g = val;
    }

    if (b_obj && (b_obj->type == LISP_INTEGER || b_obj->type == LISP_NUMBER)) {
        int val = (b_obj->type == LISP_INTEGER) ? (int)b_obj->value.integer : (int)b_obj->value.number;
        if (val >= 0 && val <= 255)
            *b = val;
    }
}

void lisp_x_get_selection_fg_color(int *r, int *g, int *b) {
    get_color_from_lisp("*selection-fg-color*", r, g, b, 0, 0, 0); /* Black */
}

void lisp_x_get_selection_bg_color(int *r, int *g, int *b) {
    get_color_from_lisp("*selection-bg-color*", r, g, b, 0, 180, 180); /* Cyan */
}

void lisp_x_get_cursor_color(int *r, int *g, int *b) {
    get_color_from_lisp("*terminal-cursor-color*", r, g, b, 140, 120, 150); /* Muted purple */
}

/* Terminal default colors */
void lisp_x_get_terminal_fg_color(int *r, int *g, int *b) {
    get_color_from_lisp("*terminal-fg-color*", r, g, b, 255, 255, 255); /* White */
}

void lisp_x_get_terminal_bg_color(int *r, int *g, int *b) {
    get_color_from_lisp("*terminal-bg-color*", r, g, b, 0, 0, 0); /* Black */
}

/* Divider colors */
void lisp_x_get_divider_connected_color(int *r, int *g, int *b) {
    get_color_from_lisp("*divider-connected-color*", r, g, b, 128, 150, 150); /* Gray with green/blue tint */
}

void lisp_x_get_divider_disconnected_color(int *r, int *g, int *b) {
    get_color_from_lisp("*divider-disconnected-color*", r, g, b, 128, 128, 128); /* Gray */
}

/* Get divider modes alist for rendering */
LispObject *lisp_x_get_divider_modes(void) {
    LispObject *modes = env_lookup(lisp_env, "*divider-modes*");
    return (modes != NULL) ? modes : NIL;
}

/* C helper: Set a divider mode (calls Lisp function) */
void lisp_x_set_divider_mode(const char *symbol_name, const char *display, int priority) {
    if (!lisp_env || !symbol_name || !display)
        return;

    /* Look up divider-mode-set function */
    LispObject *func = env_lookup(lisp_env, "divider-mode-set");
    if (!func || (func->type != LISP_LAMBDA && func->type != LISP_BUILTIN))
        return;

    /* Build argument list: ('symbol display priority) */
    /* The symbol must be quoted so it evaluates to itself, not looked up as a variable */
    LispObject *sym_arg = lisp_make_symbol(symbol_name);
    LispObject *quoted_sym = lisp_make_cons(sym_quote, lisp_make_cons(sym_arg, NIL));
    LispObject *display_arg = lisp_make_string(display);
    LispObject *priority_arg = lisp_make_integer(priority);
    LispObject *args = lisp_make_cons(quoted_sym, lisp_make_cons(display_arg, lisp_make_cons(priority_arg, NIL)));

    /* Call: (divider-mode-set 'symbol "display" priority) */
    LispObject *call_expr = lisp_make_cons(func, args);
    lisp_eval(call_expr, lisp_env);
}

/* C helper: Remove a divider mode (calls Lisp function) */
void lisp_x_remove_divider_mode(const char *symbol_name) {
    if (!lisp_env || !symbol_name)
        return;

    /* Look up divider-mode-remove function */
    LispObject *func = env_lookup(lisp_env, "divider-mode-remove");
    if (!func || (func->type != LISP_LAMBDA && func->type != LISP_BUILTIN))
        return;

    /* Build argument list: ('symbol) */
    /* The symbol must be quoted so it evaluates to itself, not looked up as a variable */
    LispObject *sym_arg = lisp_make_symbol(symbol_name);
    LispObject *quoted_sym = lisp_make_cons(sym_quote, lisp_make_cons(sym_arg, NIL));
    LispObject *args = lisp_make_cons(quoted_sym, NIL);

    /* Call: (divider-mode-remove 'symbol) */
    LispObject *call_expr = lisp_make_cons(func, args);
    lisp_eval(call_expr, lisp_env);
}

/* User input echo color */
void lisp_x_get_user_input_echo_color(int *r, int *g, int *b) {
    get_color_from_lisp("*user-input-echo-color*", r, g, b, 255, 215, 0); /* Gold */
}

/* Register terminal pointer for terminal-echo builtin */
void lisp_x_register_terminal(Terminal *term) {
    registered_terminal = term;
}

void lisp_x_register_telnet(Telnet *t) {
    registered_telnet = t;
}

void lisp_x_register_glyph_cache(GlyphCache *cache) {
    registered_glyph_cache = cache;
}

void lisp_x_register_window(Window *w) {
    registered_window = w;
}

void lisp_x_register_input_area(InputArea *area) {
    registered_input_area = area;
}

#if HAVE_RLOTTIE
void lisp_x_register_renderer(SDL_Renderer *renderer) {
    registered_renderer = renderer;
}

Animation *lisp_x_get_active_animation(void) {
    return active_animation;
}

void lisp_x_clear_active_animation(void) {
    active_animation = NULL;
}
#endif

/* Evaluate Lisp code and build echo buffer (eval-mode style)
 * Uses preallocated DynamicBuffer
 * Output format: "> code\r\n" + (result or "; Error: ...\r\n")
 * Returns: 0 on success, -1 on failure
 */
int lisp_x_eval_and_echo(const char *code, DynamicBuffer *buf) {
    if (!code || !buf || !lisp_env) {
        return -1;
    }

    /* Clear buffer for new output */
    dynamic_buffer_clear(buf);

    /* Echo the code with > prefix */
    if (dynamic_buffer_append_printf(buf, "> %s\r\n", code) < 0) {
        return -1;
    }

    /* Evaluate the code */
    LispObject *result = lisp_eval_string(code, lisp_env);

    /* Append result or error to buffer */
    if (result && result->type == LISP_ERROR) {
        if (dynamic_buffer_append_printf(buf, "; Error: %s\r\n", result->value.error_with_stack.message) < 0) {
            return -1;
        }
    } else if (result) {
        char *result_str = lisp_print(result);
        if (result_str) {
            if (dynamic_buffer_append_printf(buf, "%s\r\n", result_str) < 0) {
                return -1;
            }
        }
    } else {
        if (dynamic_buffer_append_str(buf, "; Error: Evaluation returned NULL\r\n") < 0) {
            return -1;
        }
    }

    return 0;
}

/* Get lisp environment (for accessing Lisp variables from C) */
void *lisp_x_get_environment(void) {
    return lisp_env;
}
