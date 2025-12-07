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

Create a command alias or list all aliases:

```bash
#alias                      # List all defined aliases
#alias {name} {commands}
#alias name commands        # Braces optional for single words
```

**Examples:**

```bash
# List all aliases
#alias
Aliases (3):
  k → kill %1 (priority: 5)
  n → north (priority: 5)
  ggh → get gold;go home (priority: 5)

# Simple alias
#alias {n} {north}
Alias 'n' created: n → north (priority: 5)

# Alias with argument
#alias {k} {kill %1}
Alias 'k' created: k → kill %1 (priority: 5)

# Multiple commands
#alias {loot} {get all from corpse;bury corpse}
Alias 'loot' created: loot → get all from corpse;bury corpse (priority: 5)

# All arguments
#alias {say} {tell bob %0}
say hello there    # Sends: tell bob hello there

# Pattern matching
#alias {get %1 from %2} {take %1 from %2;examine %1}
```

### #unalias

Remove an alias:

```bash
#unalias {name}
#unalias name              # Braces optional for single words
```

**Examples:**

```bash
#unalias {k}
Alias 'k' removed

#unalias nonexistent
Alias 'nonexistent' not found
```

### #variable

Define a variable or list all variables:

```bash
#variable                   # List all defined variables
#variable {name} {value}
#variable name value        # Braces optional for single words
```

**Examples:**

```bash
# List all variables
#variable
Variables (2):
  target = goblin
  weapon = sword

# Define variable
#variable {target} {goblin}
Variable 'target' set to 'goblin'

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

### Highlights

Automatically colorize text patterns in incoming server output using ANSI escape codes. Highlights use pattern matching with wildcards and support full TinTin++ color specifications.

#### Pattern Matching

Highlights support TinTin++ wildcard patterns:

- `%*` or `%1`-`%99` - Match any text (wildcard)
- `^` at start - Match beginning of line (anchor)
- Literal text - Match exact text

**Examples:**

```bash
#highlight {Valgar} {<fff>}                    # Highlight exact name
#highlight {You hit %*} {<0f0>}                # Wildcard match
#highlight {^Health:} {bold red}               # Line anchor
#highlight {%* damage} {yellow}                # Trailing wildcard
```

#### Color Specifications

Highlights support multiple color formats:

**RGB Colors:**

```bash
#highlight {critical} {<fff>}                  # 3-char RGB (white)
#highlight {warning} {<F00>}                   # 3-char RGB (bright red)
#highlight {info} {<F0080FF>}                  # 7-char RGB with flags
```

**Named Colors:**

- Standard ANSI: `black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `white`
- Bright variants: `light red`, `light blue`, `light green`, etc.
- Tertiary colors: `azure`, `jade`, `violet`, `lime`, `orange`, `pink`, `silver`, `tan`

```bash
#highlight {error} {red}
#highlight {success} {light green}
#highlight {warning} {orange}
```

**Foreground and Background:**

Use `:` to separate foreground and background colors:

```bash
#highlight {Valgar} {<fff>:<000>}              # White on black
#highlight {You died} {red:yellow}             # Red on yellow
#highlight {Level up!} {<0f0>:blue}            # Green RGB on blue
```

**Text Attributes:**

Combine attributes with colors using spaces:

- `bold` - Bold/bright text
- `dim` - Dimmed text
- `italic` - Italic text
- `underscore` / `underline` - Underlined text
- `blink` - Blinking text
- `reverse` - Reverse video
- `strikethrough` - Strike through text

```bash
#highlight {CRITICAL} {bold red}
#highlight {Note:} {underscore light blue}
#highlight {Important} {bold <fff>:<f00>}      # Bold white on red
```

#### Priority System

When multiple highlight patterns match the same line, the highest priority highlight is applied. Default priority is 5 (lower number = lower priority).

**Current Implementation:**

Priority is managed via direct hash table manipulation:

```lisp
;; Set custom priority via Lisp
(hash-set! *tintin-highlights* "pattern" (list "color" nil 10))
```

**Note:** Command-line priority specification will be added in a future update. For now, use the default priority of 5 via `#highlight {pattern} {color}`.

### #highlight

Create a highlight rule or list all highlights:

```bash
#highlight                     # List all defined highlights
#highlight {pattern} {color}
#highlight pattern color       # Braces optional for single words
```

**Examples:**

```bash
# List all highlights
#highlight
Highlights (3):
  Valgar → <fff> (priority: 5)
  You hit %* → <0f0> (priority: 5)
  ^Health → bold red (priority: 5)

# Create highlight
#highlight {Valgar} {<fff>}
Highlight 'Valgar' created: Valgar → <fff> (priority: 5)

# Wildcard pattern
#highlight {You hit %*} {<0f0>}
Highlight 'You hit %*' created: You hit %* → <0f0> (priority: 5)

# Foreground and background
#highlight {error} {red:yellow}
Highlight 'error' created: error → red:yellow (priority: 5)

# Attributes with colors
#highlight {CRITICAL} {bold red}
Highlight 'CRITICAL' created: CRITICAL → bold red (priority: 5)
```

### #unhighlight

Remove a highlight rule:

```bash
#unhighlight {pattern}
#unhighlight pattern          # Braces optional for single words
```

**Examples:**

```bash
#unhighlight {Valgar}
Highlight 'Valgar' removed

#unhighlight {You hit %*}
Highlight 'You hit %*' removed
```

### #save

Save current aliases, variables, and highlights to a file:

```bash
#save {filename}
#save filename              # Braces optional for single words
```

**Example:**

```bash
#save {my-config.lisp}
#save my-config.lisp        # Same as above
```

This creates a Lisp file with all your current aliases, variables, highlights, and settings.

### #load

Load aliases, variables, and highlights from a saved file:

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
- Highlight pattern matching and colorization
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

TinTin++ hooks into multiple telnet-gui extension points:

**User input processing** via `user-input-hook`:

```lisp
(define user-input-hook tintin-user-input-hook)
```

This intercepts all user input before it's sent to the telnet server. The hook returns `nil` to indicate it has handled echo and send operations.

**Highlight colorization** via `telnet-input-filter-hook`:

```lisp
(define telnet-input-filter-hook tintin-telnet-input-filter)
```

This transforms incoming telnet data before display, applying ANSI color codes to matching patterns. The hook receives raw telnet data (with ANSI codes preserved) and returns transformed text.

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

- `#alias` / `#unalias` - Command aliases with pattern matching
- `#variable` - Variable storage and substitution
- `#highlight` / `#unhighlight` - Pattern-based text colorization with full TinTin++ color specs
- `#save` - Save state to file (aliases, variables, highlights)
- `#load` - Load state from file
- Speedwalk notation with optional diagonal directions
- Command separation with semicolons

## Source Files

- **tintin.lisp** - Full implementation (1849 lines)
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
