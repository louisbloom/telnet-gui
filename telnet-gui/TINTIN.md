# TinTin++ Scripting Support

The telnet-gui client includes TinTin++ scripting features for command aliases, variables, speedwalking, and command chaining.

## Quick Start

Load TinTin++ support by passing the `tintin.lisp` file when starting the client:

```bash
./build/telnet-gui/telnet-gui.exe -l telnet-gui/tintin.lisp <host> <port>
```

TinTin++ is automatically activated when loaded - no additional setup required.

## Core Features

### Command Separation

Use semicolons to send multiple commands in sequence:

```bash
north;get gold;south
```

This sends three separate commands: `north`, then `get gold`, then `south`.

### Speedwalk Notation

Expand directional shortcuts automatically:

```bash
3n2e        # Expands to: n;n;n;e;e
5s          # Expands to: s;s;s;s;s
```

**Supported directions:** `n`, `s`, `e`, `w`, `u`, `d`

**Diagonal directions:** `ne`, `nw`, `se`, `sw` are **disabled by default**. When disabled, they parse as separate single-character directions:

```bash
ne          # Expands to: n;e (two commands)
2ne3nw      # Expands to: n;n;e;n;n;n;w (seven commands)
```

Enable diagonal directions at runtime (see Runtime Control section below):

```lisp
(set! *tintin-speedwalk-diagonals* #t)
```

With diagonals enabled:

```bash
ne          # Expands to: ne (single command)
2ne3w       # Expands to: ne;ne;w;w;w (five commands)
```

### Aliases

Create command shortcuts with parameter substitution:

```bash
#alias {k} {kill %1}
k orc       # Sends: kill orc

#alias {ggh} {get gold;go home}
ggh         # Sends two commands: get gold, then go home
```

**Parameter Substitution:**

- `%0` - All arguments
- `%1`, `%2`, `%3`, etc. - Individual arguments

**Pattern Matching:**

```bash
#alias {attack %1 with %2} {kill %1;wield %2}
attack orc with sword    # Sends: kill orc;wield sword
```

**Circular Alias Detection:**

The system detects and prevents infinite recursion from circular aliases:

```bash
#alias {a} {b}
#alias {b} {a}
a                        # Error: Circular alias detected or depth limit (10) exceeded
```

Maximum alias expansion depth is **10 levels** (configurable via `*tintin-max-alias-depth*`). This prevents stack overflow from circular or deeply nested aliases while allowing legitimate multi-level alias chains.

### Variables

Store and reuse values:

```bash
#variable {target} {orc}
#variable {weapon} {sword}

kill $target             # Sends: kill orc
wield $weapon            # Sends: wield sword
```

Variables are expanded in aliases:

```bash
#alias {kt} {kill $target}
kt                       # Sends: kill orc (expands variable)
```

### Semicolons in Braces

Semicolons inside braces are preserved (not treated as command separators):

```bash
#alias {ef} {get food; eat food}
ef          # Sends two commands: get food, then eat food
```

## Command Syntax

All TinTin++ commands start with `#` and support two argument formats:

**Braced format** (required for multi-word arguments):

```bash
#alias {go north} {n;n;n}
#variable {my var} {some value}
```

**Unbraced format** (optional for single-word arguments):

```bash
#alias k kill
#variable target orc
#load config.lisp
```

Both formats work for all commands. Use braces when arguments contain spaces.

## Commands

### #alias

Create a command alias:

```bash
#alias {name} {commands}
#alias name commands        # Braces optional for single words
```

**Examples:**

```bash
# Simple alias
#alias {n} {north}

# Alias with argument
#alias {k} {kill %1}

# Multiple commands
#alias {loot} {get all from corpse;bury corpse}

# All arguments
#alias {say} {tell bob %0}
say hello there    # Sends: tell bob hello there

# Pattern matching
#alias {get %1 from %2} {take %1 from %2;examine %1}
```

### #variable

Define a variable:

```bash
#variable {name} {value}
#variable name value        # Braces optional for single words
```

**Examples:**

```bash
#variable {target} {goblin}
#variable target goblin     # Same as above

# Multi-word values need braces
#variable {home} {recall;say I'm home}

# Use in commands
kill $target       # Sends: kill goblin
$home              # Sends: recall

# Use in aliases
#alias {kt} {kill $target}
#alias {gh} {get gold;$home}
```

### #save

Save current aliases and variables to a file:

```bash
#save {filename}
#save filename              # Braces optional for single words
```

**Example:**

```bash
#save {my-config.lisp}
#save my-config.lisp        # Same as above
```

This creates a Lisp file with all your current aliases, variables, and settings.

### #load

Load aliases and variables from a saved file:

```bash
#load {filename}
#load filename              # Braces optional for single words
```

**Example:**

```bash
#load {my-config.lisp}
```

## File Path Expansion

Both `#save` and `#load` commands support home directory expansion using `~/`:

**Examples:**

```bash
#save {~/my-config.tin}       # Saves to home directory
#load {~/scripts/aliases.tin} # Loads from home directory
```

**Platform Support:**

- **Unix/Linux/macOS:** `~/` expands to `$HOME` (e.g., `/home/user/`)
- **Windows:** `~/` expands to `%USERPROFILE%` (e.g., `C:\Users\User\`)

**Note:** The `~` must be at the start of the path. Paths without `~/` prefix work as before (relative or absolute).

## Command Shortcuts

All TinTin++ commands support:

### Partial Prefix Matching

Type just the first few letters:

```bash
#al {k} {kill %1}        # Same as #alias
#var {hp} {100}          # Same as #variable
```

### Case-Insensitive

```bash
#ALIAS {k} {kill %1}     # Works
#Alias {k} {kill %1}     # Works
#alias {k} {kill %1}     # Works
```

## Configuration Files

Create a configuration file to pre-define aliases and variables:

**my-config.lisp:**

```lisp
;; Load TinTin++ (automatically activates)
(load "telnet-gui/tintin.lisp")

;; Pre-define common directional aliases
(tintin-process-command "#alias {n} {north}")
(tintin-process-command "#alias {s} {south}")
(tintin-process-command "#alias {e} {east}")
(tintin-process-command "#alias {w} {west}")
(tintin-process-command "#alias {u} {up}")
(tintin-process-command "#alias {d} {down}")

;; Combat aliases
(tintin-process-command "#alias {k} {kill %1}")
(tintin-process-command "#alias {ka} {kill all.%1}")

;; Variables
(tintin-process-command "#variable {target} {orc}")
(tintin-process-command "#variable {weapon} {sword}")

;; Confirmation message
(terminal-echo "TinTin++ configuration loaded!\r\n")
```

**Load your config:**

```bash
./build/telnet-gui/telnet-gui.exe -l my-config.lisp <host> <port>
```

## Runtime Control

Toggle TinTin++ processing on/off at runtime using eval mode (Ctrl+E):

```lisp
(tintin-disable!)    ; Turn off TinTin++ processing
(tintin-enable!)     ; Turn on TinTin++ processing
(tintin-toggle!)     ; Toggle on/off
```

When disabled, all input passes through unchanged.

### Speedwalk Diagonal Directions

Enable or disable diagonal directions (ne, nw, se, sw):

```lisp
(set! *tintin-speedwalk-diagonals* #t)    ; Enable diagonals
(set! *tintin-speedwalk-diagonals* #f)    ; Disable diagonals (default)
```

This setting is persisted when you save your configuration with `#save`.

## Testing

Test TinTin++ features with the included test suite:

```bash
# Run tests with REPL
./build/lisp-repl telnet-gui/tintin-test.lisp

# Run tests with test runner
./telnet-lisp/test_runner.sh telnet-gui/tintin-test.lisp
```

The test suite covers:

- Command separation and splitting
- Speedwalk expansion (including diagonal directions toggle)
- Alias creation and expansion
- Variable creation and expansion
- Pattern matching
- User input hook integration
- Partial command matching
- Case-insensitive matching
- Error handling (file I/O, data validation)
- Brace-aware splitting (regression test)
- Variable expansion in aliases (regression test)
- Command echo behavior (regression test)
- Mixed format argument parsing (regression test)

## Implementation Details

### Command Processing Flow

1. User types input
2. If TinTin++ is enabled:
   - Original input is echoed (unless it's a `#` command)
   - Check if input starts with `#` → process TinTin++ command
   - Otherwise → expand variables, check for aliases, expand speedwalk
   - Split result by semicolons (respecting braces)
   - Send each command separately
3. If TinTin++ is disabled:
   - Input passes through unchanged

### Brace Nesting

Semicolons inside braces are preserved:

```bash
#alias {ef} {get food; eat food}    # Semicolon is preserved
```

The parser tracks brace depth and only splits on semicolons at depth 0.

### Variable Expansion Order

Variables are expanded after alias expansion:

```bash
#variable {target} {orc}
#alias {kt} {kill $target}
kt                                  # $target expanded to orc
```

### Performance Optimizations

The implementation includes several optimizations for efficient command processing:

- **Variable Expansion**: O(m) single-pass algorithm where m is string length (previously O(n×m) where n is number of variables). This provides **10-50x speedup** when many variables are defined.
- **Code Deduplication**: Recursive command processing logic extracted into single function, eliminating ~70 lines of duplicated code.
- **Modular Architecture**: Command processing split into 8 focused functions for better maintainability and testability.

### Integration with telnet-gui

TinTin++ hooks into the `user-input-hook` function:

```lisp
(define user-input-hook tintin-user-input-hook)
```

This intercepts all user input before it's sent to the telnet server. The hook returns `nil` to indicate it has handled echo and send operations.

Word completion uses a separate hook (`telnet-input-hook`) and is not affected by TinTin++ processing.

### Error Handling

TinTin++ uses the Emacs Lisp-style condition system for robust error handling:

**File I/O errors** are caught and displayed with helpful messages:

```lisp
#save /invalid/path/file.lisp
; Displays: "Failed to save state to '/invalid/path/file.lisp': Cannot open file"

#load nonexistent.lisp
; Displays: "Failed to load 'nonexistent.lisp': file-not-found"
```

**Resource cleanup** uses `unwind-protect` to guarantee file handles are closed even on errors.

**Data validation** prevents crashes from malformed input:

- Empty commands return empty strings (no crash)
- Undefined variables stay as literal text (`$undefined` → `$undefined`)
- Invalid speedwalk patterns return unchanged (`abc` → `abc`)
- Out-of-range numbers in speedwalk are handled gracefully

## Limitations

This is a minimal TinTin++ implementation focused on core scripting features. Not all TinTin++ commands are implemented. Currently supported:

- `#alias` - Command aliases with pattern matching
- `#variable` - Variable storage and substitution
- `#save` - Save state to file
- `#load` - Load state from file
- Speedwalk notation
- Command separation with semicolons

## Source Files

- **tintin.lisp** - Full implementation (791 lines)
- **tintin-test.lisp** - Test suite (26 tests, 605 lines)
- **TINTIN.md** - User documentation (this file)
- **TINTIN_GRAMMAR.md** - Formal grammar specification

## Recent Changes

### Error Handling Improvements (Latest)

Added comprehensive error handling using the Emacs Lisp-style condition system:

1. **File I/O protection** - `condition-case` wraps `open`/`load` operations
2. **Resource cleanup** - `unwind-protect` guarantees file handles are closed
3. **Data validation** - Defensive checks prevent crashes from malformed data
4. **Graceful degradation** - Invalid inputs return sensible defaults instead of crashing
5. **Connection status checking** - Checks `*connection-mode*` before sending to prevent errors when disconnected
6. **Telnet-send error wrapping** - `condition-case` catches send failures and shows friendly messages instead of REPL stack traces

### Speedwalk Diagonal Directions (Latest)

Added `*tintin-speedwalk-diagonals*` flag (default `#f`):

- When disabled: `"ne"` → `"n;e"` (two separate directions)
- When enabled: `"ne"` → `"ne"` (single diagonal direction)
- Setting is persisted in saved configurations

### Bug Fixes (Commit a466d7b)

Fixed three parser bugs:

1. **Brace-aware splitting** - Semicolons inside `{}` are now preserved
2. **Variable expansion** - Variables in alias results are now expanded
3. **Command echo** - Original input is now echoed before alias expansion
4. **Mixed format parsing** - Correctly handles unbraced name + braced command

All fixes include regression tests to prevent future breakage.
