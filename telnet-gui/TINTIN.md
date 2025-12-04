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
2ne3w       # Expands to: ne;ne;w;w;w
```

**Supported directions:** `n`, `s`, `e`, `w`, `u`, `d`, `ne`, `nw`, `se`, `sw`

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
- Speedwalk expansion
- Alias creation and expansion
- Variable creation and expansion
- Pattern matching
- User input hook integration
- Partial command matching
- Case-insensitive matching
- Brace-aware splitting (regression test)
- Variable expansion in aliases (regression test)
- Command echo behavior (regression test)

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

### Integration with telnet-gui

TinTin++ hooks into the `user-input-hook` function:

```lisp
(define user-input-hook tintin-user-input-hook)
```

This intercepts all user input before it's sent to the telnet server. The hook returns `nil` to indicate it has handled echo and send operations.

Word completion uses a separate hook (`telnet-input-hook`) and is not affected by TinTin++ processing.

## Limitations

This is a minimal TinTin++ implementation focused on core scripting features. Not all TinTin++ commands are implemented. Currently supported:

- `#alias` - Command aliases with pattern matching
- `#variable` - Variable storage and substitution
- `#save` - Save state to file
- `#load` - Load state from file
- Speedwalk notation
- Command separation with semicolons

## Source Files

- **tintin.lisp** - Full implementation (lines 1-690)
- **tintin-test.lisp** - Test suite (20 tests, all passing)
- **TINTIN.md** - This documentation file

## Recent Fixes

Three bugs were fixed in commit `a466d7b`:

1. **Brace-aware splitting** - Semicolons inside `{}` are now preserved
2. **Variable expansion** - Variables in alias results are now expanded
3. **Command echo** - Original input is now echoed before alias expansion

All fixes include regression tests to prevent future breakage.
