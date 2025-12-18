/*
 * animation.h - Lottie animation support for telnet-gui
 *
 * Provides background animation rendering using rlottie library.
 * Animations render behind terminal text with configurable visibility.
 */

#ifndef ANIMATION_H
#define ANIMATION_H

#include <SDL2/SDL.h>

/* Animation state opaque type */
typedef struct Animation Animation;

/* Animation visibility mode */
typedef enum {
    ANIMATION_VISIBILITY_DIM,        /* Dim/darken animation with overlay */
    ANIMATION_VISIBILITY_TRANSPARENT /* Semi-transparent terminal backgrounds */
} AnimationVisibilityMode;

/* Lifecycle */
Animation *animation_create(SDL_Renderer *renderer);
void animation_destroy(Animation *anim);

/* Loading */
int animation_load(Animation *anim, const char *path);
void animation_unload(Animation *anim);

/* Playback control */
void animation_play(Animation *anim);
void animation_pause(Animation *anim);
void animation_stop(Animation *anim);
void animation_set_speed(Animation *anim, float speed);
void animation_set_loop(Animation *anim, int loop);
void animation_seek(Animation *anim, float position); /* 0.0 to 1.0 */

/* State queries */
int animation_is_playing(Animation *anim);
int animation_is_loaded(Animation *anim);
float animation_get_position(Animation *anim);
float animation_get_duration(Animation *anim); /* in seconds */
int animation_get_frame_count(Animation *anim);

/* Visibility configuration */
void animation_set_visibility_mode(Animation *anim, AnimationVisibilityMode mode);
AnimationVisibilityMode animation_get_visibility_mode(Animation *anim);
void animation_set_dim_alpha(Animation *anim, float alpha);   /* 0.0-1.0, overlay opacity for DIM mode */
void animation_set_terminal_alpha(Animation *anim, float alpha); /* 0.0-1.0, terminal bg alpha for TRANSPARENT mode */
float animation_get_terminal_bg_alpha(Animation *anim);

/* Rendering */
void animation_update(Animation *anim, float delta_time_ms); /* Advance animation */
void animation_render(Animation *anim, int x, int y, int width, int height); /* Render current frame */
void animation_render_dim_overlay(Animation *anim, int x, int y, int width, int height); /* Render overlay for DIM mode */

#endif /* ANIMATION_H */
