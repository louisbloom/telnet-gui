/* Special slash commands for input area */

#ifndef COMMANDS_H
#define COMMANDS_H

#include "telnet.h"
#include "terminal.h"
#include "input_area.h"

/* Process special commands starting with '/'
 * Returns 1 if command was processed, 0 if not a command
 *
 * Parameters:
 *   text          - Input text to process
 *   telnet        - Telnet connection instance
 *   term          - Terminal instance
 *   connected_mode - Pointer to connection state (updated by commands)
 *   area          - Input area instance
 */
int process_command(const char *text, Telnet *telnet, Terminal *term, int *connected_mode, InputArea *area);

#endif /* COMMANDS_H */
