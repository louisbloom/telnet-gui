/* Input handling for terminal */

#ifndef INPUT_H
#define INPUT_H

#include <SDL2/SDL.h>
#include <vterm.h>

/* Handle SDL keyboard event */
void input_handle_keyboard(SDL_KeyboardEvent *event, VTerm *vterm);

/* Handle SDL text input event */
void input_handle_text(SDL_TextInputEvent *event, VTerm *vterm);

/* Handle SDL mouse event */
void input_handle_mouse(SDL_MouseButtonEvent *event, SDL_MouseMotionEvent *motion, VTerm *vterm, int cell_width,
                        int cell_height, int titlebar_height);

#endif /* INPUT_H */
