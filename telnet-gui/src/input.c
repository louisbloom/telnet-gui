/* Input handling implementation */

#include "input.h"
#include "terminal.h"
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

static VTermModifier get_vterm_modifier(void) {
    VTermModifier mod = 0;
    const Uint8 *state = SDL_GetKeyboardState(NULL);

    if (state[SDL_SCANCODE_LSHIFT] || state[SDL_SCANCODE_RSHIFT]) {
        mod |= VTERM_MOD_SHIFT;
    }
    if (state[SDL_SCANCODE_LCTRL] || state[SDL_SCANCODE_RCTRL]) {
        mod |= VTERM_MOD_CTRL;
    }
    if (state[SDL_SCANCODE_LALT] || state[SDL_SCANCODE_RALT]) {
        mod |= VTERM_MOD_ALT;
    }

    return mod;
}

void input_handle_keyboard(SDL_KeyboardEvent *event, struct Terminal *term) {
    if (!event || !term)
        return;

    VTerm *vterm = terminal_get_vterm(term);
    if (!vterm)
        return;

    VTermModifier mod = get_vterm_modifier();
    SDL_Scancode scancode = event->keysym.scancode;

    /* Map SDL scancodes to vterm keys */
    VTermKey key = VTERM_KEY_NONE;

    switch (scancode) {
    case SDL_SCANCODE_UP:
        key = VTERM_KEY_UP;
        break;
    case SDL_SCANCODE_DOWN:
        key = VTERM_KEY_DOWN;
        break;
    case SDL_SCANCODE_LEFT:
        key = VTERM_KEY_LEFT;
        break;
    case SDL_SCANCODE_RIGHT:
        key = VTERM_KEY_RIGHT;
        break;
    case SDL_SCANCODE_INSERT:
        key = VTERM_KEY_INS;
        break;
    case SDL_SCANCODE_HOME:
        key = VTERM_KEY_HOME;
        break;
    case SDL_SCANCODE_END:
        key = VTERM_KEY_END;
        break;
    case SDL_SCANCODE_PAGEUP:
        key = VTERM_KEY_PAGEUP;
        break;
    case SDL_SCANCODE_PAGEDOWN:
        key = VTERM_KEY_PAGEDOWN;
        break;
    case SDL_SCANCODE_DELETE:
        key = VTERM_KEY_DEL;
        break;
    case SDL_SCANCODE_RETURN:
        key = VTERM_KEY_ENTER;
        /* Send buffered output when Enter is pressed */
        vterm_keyboard_key(vterm, key, mod);
        terminal_send_buffer(term);
        return;
    case SDL_SCANCODE_ESCAPE:
        key = VTERM_KEY_ESCAPE;
        break;
    case SDL_SCANCODE_TAB:
        key = VTERM_KEY_TAB;
        break;
    case SDL_SCANCODE_BACKSPACE:
        key = VTERM_KEY_BACKSPACE;
        break;
    case SDL_SCANCODE_F1:
        key = VTERM_KEY_FUNCTION(1);
        break;
    case SDL_SCANCODE_F2:
        key = VTERM_KEY_FUNCTION(2);
        break;
    case SDL_SCANCODE_F3:
        key = VTERM_KEY_FUNCTION(3);
        break;
    case SDL_SCANCODE_F4:
        key = VTERM_KEY_FUNCTION(4);
        break;
    case SDL_SCANCODE_F5:
        key = VTERM_KEY_FUNCTION(5);
        break;
    case SDL_SCANCODE_F6:
        key = VTERM_KEY_FUNCTION(6);
        break;
    case SDL_SCANCODE_F7:
        key = VTERM_KEY_FUNCTION(7);
        break;
    case SDL_SCANCODE_F8:
        key = VTERM_KEY_FUNCTION(8);
        break;
    case SDL_SCANCODE_F9:
        key = VTERM_KEY_FUNCTION(9);
        break;
    case SDL_SCANCODE_F10:
        key = VTERM_KEY_FUNCTION(10);
        break;
    case SDL_SCANCODE_F11:
        key = VTERM_KEY_FUNCTION(11);
        break;
    case SDL_SCANCODE_F12:
        key = VTERM_KEY_FUNCTION(12);
        break;
    default:
        break;
    }

    if (key != VTERM_KEY_NONE) {
        vterm_keyboard_key(vterm, key, mod);
    }
}

void input_handle_text(SDL_TextInputEvent *event, struct Terminal *term) {
    if (!event || !term)
        return;

    VTerm *vterm = terminal_get_vterm(term);
    if (!vterm)
        return;

    /* Convert UTF-8 to UCS-32 codepoint */
    const char *text = event->text;
    uint32_t codepoint = 0;

    if ((unsigned char)text[0] < 0x80) {
        codepoint = text[0];
    } else if ((unsigned char)text[0] < 0xE0) {
        codepoint = ((text[0] & 0x1F) << 6) | (text[1] & 0x3F);
    } else if ((unsigned char)text[0] < 0xF0) {
        codepoint = ((text[0] & 0x0F) << 12) | ((text[1] & 0x3F) << 6) | (text[2] & 0x3F);
    } else {
        codepoint = ((text[0] & 0x07) << 18) | ((text[1] & 0x3F) << 12) | ((text[2] & 0x3F) << 6) | (text[3] & 0x3F);
    }

    /* Process keyboard input - this calls output callback synchronously to buffer the bytes */
    vterm_keyboard_unichar(vterm, codepoint, get_vterm_modifier());

    /* Echo buffered input to screen for immediate feedback */
    terminal_echo_local(term);
}

void input_handle_mouse(SDL_MouseButtonEvent *event, SDL_MouseMotionEvent *motion, VTerm *vterm, int cell_width,
                        int cell_height, int titlebar_height) {
    if (!vterm || !event)
        return;

    if (event->type == SDL_MOUSEBUTTONDOWN) {
        int col = event->x / cell_width;
        int row = (event->y - titlebar_height) / cell_height;

        int button = 0;
        if (event->button == SDL_BUTTON_LEFT) {
            button = 0; /* LEFT */
        } else if (event->button == SDL_BUTTON_MIDDLE) {
            button = 1; /* MIDDLE */
        } else if (event->button == SDL_BUTTON_RIGHT) {
            button = 2; /* RIGHT */
        } else {
            return;
        }

        vterm_mouse_move(vterm, row, col, VTERM_MOD_NONE);
        vterm_mouse_button(vterm, button, true, VTERM_MOD_NONE);
    } else if (event->type == SDL_MOUSEBUTTONUP) {
        int button = 0;
        if (event->button == SDL_BUTTON_LEFT) {
            button = 0; /* LEFT */
        } else if (event->button == SDL_BUTTON_MIDDLE) {
            button = 1; /* MIDDLE */
        } else if (event->button == SDL_BUTTON_RIGHT) {
            button = 2; /* RIGHT */
        } else {
            return;
        }

        vterm_mouse_button(vterm, button, false, VTERM_MOD_NONE);
    }

    if (motion) {
        int col = motion->x / cell_width;
        int row = (motion->y - titlebar_height) / cell_height;
        vterm_mouse_move(vterm, row, col, VTERM_MOD_NONE);
    }
}
