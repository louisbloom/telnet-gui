/* Input handling for terminal */

#ifndef INPUT_H
#define INPUT_H

#include <SDL2/SDL.h>
#include <vterm.h>

/* Forward declaration - Terminal is defined in terminal.h */
struct Terminal;

/* Handle SDL keyboard event */
void input_handle_keyboard(SDL_KeyboardEvent *event, struct Terminal *term);

/* Handle SDL text input event */
void input_handle_text(SDL_TextInputEvent *event, struct Terminal *term);

/* Handle SDL mouse event */
void input_handle_mouse(SDL_MouseButtonEvent *event, SDL_MouseMotionEvent *motion, VTerm *vterm, int cell_width,
                        int cell_height, int titlebar_height);

#endif /* INPUT_H */
