/*
 * animation.c - Lottie animation support using rlottie
 *
 * Renders Lottie JSON animations as background layer in terminal.
 */

#include "animation.h"
#include "../../telnet-lisp/include/file_utils.h"

#if HAVE_RLOTTIE
#include <rlottie_capi.h>
#endif

#include <gc.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Forward declaration for finalizer */
static void animation_gc_finalizer(void *obj, void *client_data);

/* Helper: check if file exists using file_open */
static int file_exists(const char *path) {
    FILE *f = file_open(path, "r");
    if (f) {
        fclose(f);
        return 1;
    }
    return 0;
}

/* Helper: read entire file into malloc'd buffer, returns NULL on failure */
static char *read_file_contents(const char *path, size_t *out_size) {
    FILE *f = file_open(path, "rb");
    if (!f)
        return NULL;

    /* Get file size */
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    if (size <= 0) {
        fclose(f);
        return NULL;
    }

    /* Allocate buffer with null terminator */
    char *buffer = malloc(size + 1);
    if (!buffer) {
        fclose(f);
        return NULL;
    }

    /* Read file */
    size_t read = fread(buffer, 1, size, f);
    fclose(f);

    if ((long)read != size) {
        free(buffer);
        return NULL;
    }

    buffer[size] = '\0';
    if (out_size)
        *out_size = (size_t)size;
    return buffer;
}

struct Animation {
    SDL_Renderer *renderer;

#if HAVE_RLOTTIE
    Lottie_Animation *lottie;
    uint32_t *frame_buffer; /* ARGB pixel buffer for rlottie rendering */
    size_t frame_buffer_size;
    SDL_Texture *frame_texture; /* SDL texture from frame_buffer */
    int texture_width;          /* Current texture dimensions */
    int texture_height;
#endif

    /* Animation properties (from file) */
    int native_width;
    int native_height;
    size_t frame_count;
    double frame_rate;
    double duration_seconds;

    /* Playback state */
    int playing;
    int loop;
    float speed;
    double current_time; /* Current position in seconds */

    /* Visibility settings */
    AnimationVisibilityMode visibility_mode;
    float dim_alpha;      /* For DIM mode: overlay opacity (0=no dim, 1=fully black) */
    float terminal_alpha; /* For TRANSPARENT mode: terminal bg alpha */

    /* Cached frame info */
    size_t last_rendered_frame;
};

Animation *animation_create(SDL_Renderer *renderer) {
    if (!renderer)
        return NULL;

    /* Use GC allocation - animation will be automatically collected */
    Animation *anim = GC_MALLOC(sizeof(Animation));
    if (!anim)
        return NULL;

    /* GC_MALLOC returns zeroed memory, but set fields explicitly for clarity */
    memset(anim, 0, sizeof(Animation));

    anim->renderer = renderer;
    anim->playing = 0;
    anim->loop = 1;
    anim->speed = 1.0f;
    anim->current_time = 0.0;
    anim->visibility_mode = ANIMATION_VISIBILITY_DIM;
    anim->dim_alpha = 0.7f;       /* 70% overlay = 30% animation visible */
    anim->terminal_alpha = 0.85f; /* 85% terminal bg opacity */
    anim->last_rendered_frame = (size_t)-1;

    /* Register finalizer for SDL/rlottie resource cleanup */
    GC_register_finalizer(anim, animation_gc_finalizer, NULL, NULL, NULL);

    return anim;
}

void animation_destroy(Animation *anim) {
    if (!anim)
        return;

#if HAVE_RLOTTIE
    /* Clean up SDL and rlottie resources (non-GC managed) */
    if (anim->frame_texture) {
        SDL_DestroyTexture(anim->frame_texture);
        anim->frame_texture = NULL;
    }
    /* frame_buffer is GC-managed, no need to free */
    anim->frame_buffer = NULL;
    if (anim->lottie) {
        lottie_animation_destroy(anim->lottie);
        anim->lottie = NULL;
    }
#endif

    /* Animation struct itself is GC-managed, no need to free */
    /* Unregister finalizer since we cleaned up manually */
    GC_register_finalizer(anim, NULL, NULL, NULL, NULL);
}

int animation_load(Animation *anim, const char *path) {
    if (!anim || !path)
        return -1;

#if HAVE_RLOTTIE
    /* Unload any existing animation */
    animation_unload(anim);

    /* Try multiple paths to find the animation file */
    const char *try_paths[4];
    int path_count = 0;
    char exe_relative_path[1024] = {0};

    /* Path 1: Executable-relative path (for build directory) */
    char *base_path = SDL_GetBasePath();
    if (base_path) {
        size_t base_len = strlen(base_path);
        const char *sep =
            (base_len > 0 && (base_path[base_len - 1] == '/' || base_path[base_len - 1] == '\\')) ? "" : "/";
        snprintf(exe_relative_path, sizeof(exe_relative_path), "%s%s%s", base_path, sep, path);
        try_paths[path_count++] = exe_relative_path;
        SDL_free(base_path);
    }

    /* Path 2: Raw path as provided (current directory relative or absolute) */
    try_paths[path_count++] = path;

    /* Find the first path that exists */
    const char *found_path = NULL;
    for (int i = 0; i < path_count; i++) {
        if (file_exists(try_paths[i])) {
            found_path = try_paths[i];
            break;
        }
    }

    if (!found_path) {
        fprintf(stderr, "animation_load: file not found '%s' (tried %d paths)\n", path, path_count);
        return -1;
    }

    /* Read file contents */
    size_t json_size;
    char *json_data = read_file_contents(found_path, &json_size);
    if (!json_data) {
        fprintf(stderr, "animation_load: failed to read '%s'\n", found_path);
        return -1;
    }

    /* Create animation from JSON data */
    anim->lottie = lottie_animation_from_data(json_data, found_path, "");
    free(json_data);

    if (!anim->lottie) {
        fprintf(stderr, "animation_load: failed to parse JSON from '%s'\n", found_path);
        return -1;
    }

    /* Get animation properties */
    size_t width, height;
    lottie_animation_get_size(anim->lottie, &width, &height);
    anim->native_width = (int)width;
    anim->native_height = (int)height;
    anim->frame_count = lottie_animation_get_totalframe(anim->lottie);
    anim->frame_rate = lottie_animation_get_framerate(anim->lottie);
    anim->duration_seconds = lottie_animation_get_duration(anim->lottie);

    /* Allocate frame buffer at renderer output size for crisp rendering */
    int buf_width, buf_height;
    if (SDL_GetRendererOutputSize(anim->renderer, &buf_width, &buf_height) != 0) {
        /* Fallback to reasonable defaults if we can't get renderer size */
        buf_width = anim->native_width > 0 ? anim->native_width : 800;
        buf_height = anim->native_height > 0 ? anim->native_height : 600;
    }
    /* Clamp to reasonable maximum to avoid excessive memory usage */
    if (buf_width > 3840)
        buf_width = 3840;
    if (buf_height > 2160)
        buf_height = 2160;

    anim->frame_buffer_size = buf_width * buf_height * sizeof(uint32_t);
    /* Use GC_MALLOC_ATOMIC for pixel buffer (no pointers to scan) */
    anim->frame_buffer = GC_MALLOC_ATOMIC(anim->frame_buffer_size);
    if (!anim->frame_buffer) {
        lottie_animation_destroy(anim->lottie);
        anim->lottie = NULL;
        return -1;
    }

    /* Create SDL texture for rendering
     * rlottie outputs ARGB32 format (0xAARRGGBB) which maps to SDL_PIXELFORMAT_ARGB8888 */
    anim->frame_texture =
        SDL_CreateTexture(anim->renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, buf_width, buf_height);
    if (!anim->frame_texture) {
        /* frame_buffer is GC-managed, just NULL it */
        anim->frame_buffer = NULL;
        lottie_animation_destroy(anim->lottie);
        anim->lottie = NULL;
        return -1;
    }

    /* Enable alpha blending on texture */
    SDL_SetTextureBlendMode(anim->frame_texture, SDL_BLENDMODE_BLEND);

    /* Store texture dimensions for dynamic resize detection */
    anim->texture_width = buf_width;
    anim->texture_height = buf_height;

    /* Reset playback state */
    anim->current_time = 0.0;
    anim->last_rendered_frame = (size_t)-1;

    fprintf(stderr, "animation_load: success - native %dx%d, render %dx%d, %zu frames, %.1f fps, %.2f sec\n",
            anim->native_width, anim->native_height, buf_width, buf_height, anim->frame_count, anim->frame_rate,
            anim->duration_seconds);

    return 0;
#else
    (void)path;
    return -1; /* rlottie not available */
#endif
}

void animation_unload(Animation *anim) {
    if (!anim)
        return;

#if HAVE_RLOTTIE
    if (anim->frame_texture) {
        SDL_DestroyTexture(anim->frame_texture);
        anim->frame_texture = NULL;
    }
    /* frame_buffer is GC-managed, just NULL it */
    anim->frame_buffer = NULL;
    anim->frame_buffer_size = 0;
    if (anim->lottie) {
        lottie_animation_destroy(anim->lottie);
        anim->lottie = NULL;
    }
#endif

    anim->playing = 0;
    anim->current_time = 0.0;
    anim->frame_count = 0;
    anim->duration_seconds = 0.0;
    anim->last_rendered_frame = (size_t)-1;
}

void animation_play(Animation *anim) {
    if (!anim)
        return;
    anim->playing = 1;
}

void animation_pause(Animation *anim) {
    if (!anim)
        return;
    anim->playing = 0;
}

void animation_stop(Animation *anim) {
    if (!anim)
        return;
    anim->playing = 0;
    anim->current_time = 0.0;
    anim->last_rendered_frame = (size_t)-1;
}

void animation_set_speed(Animation *anim, float speed) {
    if (!anim)
        return;
    if (speed < 0.0f)
        speed = 0.0f;
    if (speed > 10.0f)
        speed = 10.0f;
    anim->speed = speed;
}

void animation_set_loop(Animation *anim, int loop) {
    if (!anim)
        return;
    anim->loop = loop ? 1 : 0;
}

void animation_seek(Animation *anim, float position) {
    if (!anim)
        return;
    if (position < 0.0f)
        position = 0.0f;
    if (position > 1.0f)
        position = 1.0f;
    anim->current_time = position * anim->duration_seconds;
    anim->last_rendered_frame = (size_t)-1; /* Force re-render */
}

int animation_is_playing(Animation *anim) {
    return anim ? anim->playing : 0;
}

int animation_is_loaded(Animation *anim) {
#if HAVE_RLOTTIE
    return anim && anim->lottie != NULL;
#else
    (void)anim;
    return 0;
#endif
}

float animation_get_position(Animation *anim) {
    if (!anim || anim->duration_seconds <= 0.0)
        return 0.0f;
    return (float)(anim->current_time / anim->duration_seconds);
}

float animation_get_duration(Animation *anim) {
    return anim ? (float)anim->duration_seconds : 0.0f;
}

int animation_get_frame_count(Animation *anim) {
    return anim ? (int)anim->frame_count : 0;
}

void animation_set_visibility_mode(Animation *anim, AnimationVisibilityMode mode) {
    if (!anim)
        return;
    anim->visibility_mode = mode;
}

AnimationVisibilityMode animation_get_visibility_mode(Animation *anim) {
    return anim ? anim->visibility_mode : ANIMATION_VISIBILITY_DIM;
}

void animation_set_dim_alpha(Animation *anim, float alpha) {
    if (!anim)
        return;
    if (alpha < 0.0f)
        alpha = 0.0f;
    if (alpha > 1.0f)
        alpha = 1.0f;
    anim->dim_alpha = alpha;
}

void animation_set_terminal_alpha(Animation *anim, float alpha) {
    if (!anim)
        return;
    if (alpha < 0.0f)
        alpha = 0.0f;
    if (alpha > 1.0f)
        alpha = 1.0f;
    anim->terminal_alpha = alpha;
}

float animation_get_terminal_bg_alpha(Animation *anim) {
    return anim ? anim->terminal_alpha : 1.0f;
}

void animation_update(Animation *anim, float delta_time_ms) {
    if (!anim || !anim->playing)
        return;

#if HAVE_RLOTTIE
    if (!anim->lottie)
        return;

    /* Advance time */
    anim->current_time += (delta_time_ms / 1000.0) * anim->speed;

    /* Handle looping */
    if (anim->current_time >= anim->duration_seconds) {
        if (anim->loop) {
            anim->current_time = fmod(anim->current_time, anim->duration_seconds);
        } else {
            anim->current_time = anim->duration_seconds;
            anim->playing = 0;
        }
    }
#else
    (void)delta_time_ms;
#endif
}

void animation_render(Animation *anim, int x, int y, int width, int height) {
    if (!anim)
        return;

#if HAVE_RLOTTIE
    if (!anim->lottie || !anim->frame_texture || !anim->frame_buffer)
        return;

    /* Check if we need to resize the texture to match destination size */
    /* Only resize if destination differs significantly (avoid constant resizing) */
    int target_width = width;
    int target_height = height;
    /* Clamp to reasonable maximum to avoid excessive memory usage */
    if (target_width > 3840)
        target_width = 3840;
    if (target_height > 2160)
        target_height = 2160;

    if (target_width != anim->texture_width || target_height != anim->texture_height) {
        /* Recreate frame buffer at new size using GC allocation */
        size_t new_buffer_size = target_width * target_height * sizeof(uint32_t);
        uint32_t *new_buffer = GC_MALLOC_ATOMIC(new_buffer_size);
        if (new_buffer) {
            /* Create new texture */
            SDL_Texture *new_texture = SDL_CreateTexture(anim->renderer, SDL_PIXELFORMAT_ARGB8888,
                                                         SDL_TEXTUREACCESS_STREAMING, target_width, target_height);
            if (new_texture) {
                /* Success - swap in new resources */
                SDL_SetTextureBlendMode(new_texture, SDL_BLENDMODE_BLEND);
                /* Old frame_buffer is GC-managed, just reassign (old will be collected) */
                SDL_DestroyTexture(anim->frame_texture);
                anim->frame_buffer = new_buffer;
                anim->frame_buffer_size = new_buffer_size;
                anim->frame_texture = new_texture;
                anim->texture_width = target_width;
                anim->texture_height = target_height;
                anim->last_rendered_frame = (size_t)-1; /* Force re-render */
            }
            /* If texture creation failed, new_buffer will be collected by GC */
        }
    }

    /* Calculate current frame */
    size_t frame = (size_t)(anim->current_time * anim->frame_rate);
    if (frame >= anim->frame_count && anim->frame_count > 0) {
        frame = anim->frame_count - 1;
    }

    /* Only re-render if frame changed */
    if (frame != anim->last_rendered_frame) {
        /* Get texture dimensions */
        int tex_width, tex_height;
        SDL_QueryTexture(anim->frame_texture, NULL, NULL, &tex_width, &tex_height);

        /* Render frame to buffer */
        lottie_animation_render(anim->lottie, frame, anim->frame_buffer, (size_t)tex_width, (size_t)tex_height,
                                (size_t)(tex_width * sizeof(uint32_t)));

        /* Update SDL texture */
        SDL_UpdateTexture(anim->frame_texture, NULL, anim->frame_buffer, tex_width * sizeof(uint32_t));

        anim->last_rendered_frame = frame;
    }

    /* Render to screen, scaling to fit destination */
    SDL_Rect dst = {x, y, width, height};
    SDL_RenderCopy(anim->renderer, anim->frame_texture, NULL, &dst);
#else
    (void)x;
    (void)y;
    (void)width;
    (void)height;
#endif
}

void animation_render_dim_overlay(Animation *anim, int x, int y, int width, int height) {
    if (!anim || anim->visibility_mode != ANIMATION_VISIBILITY_DIM)
        return;

    /* Draw semi-transparent black overlay */
    Uint8 alpha = (Uint8)(anim->dim_alpha * 255);
    SDL_SetRenderDrawBlendMode(anim->renderer, SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(anim->renderer, 0, 0, 0, alpha);
    SDL_Rect rect = {x, y, width, height};
    SDL_RenderFillRect(anim->renderer, &rect);
}

/* GC finalizer - called when Animation object is garbage collected */
static void animation_gc_finalizer(void *obj, void *client_data) {
    (void)client_data;
    Animation *anim = (Animation *)obj;
    if (!anim)
        return;

#if HAVE_RLOTTIE
    /* Clean up SDL texture (non-GC managed) */
    if (anim->frame_texture) {
        SDL_DestroyTexture(anim->frame_texture);
        anim->frame_texture = NULL;
    }
    /* Clean up rlottie animation (non-GC managed) */
    if (anim->lottie) {
        lottie_animation_destroy(anim->lottie);
        anim->lottie = NULL;
    }
    /* frame_buffer is GC-managed, will be collected separately */
#endif
}
