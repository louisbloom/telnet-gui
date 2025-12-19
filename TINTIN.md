# TinTin++ Language Emulator

telnet-gui includes a **mini TinTin++ language emulator** that implements a subset of TinTin++ scripting features. For complete TinTin++ language documentation, see the [official TinTin++ documentation](https://tintin.mudhalla.net/).

## What's Emulated

This implementation emulates the following TinTin++ features:

### Commands

- **`#alias` / `#unalias`** - Command aliases with pattern matching and parameter substitution (`%0`, `%1`, etc.)
- **`#variable`** - Variable storage and substitution (`$variable`)
- **`#action` / `#unaction`** - Triggers that execute commands when server output matches patterns
- **`#highlight` / `#unhighlight`** - Pattern-based text colorization with TinTin++ color specifications
- **`#save` / `#load`** - Save and load state (aliases, variables, highlights, actions, settings)

### Language Features

- **Command separation** - Semicolons split commands (`n;s;e;w`)
- **Speedwalk notation** - Numeric prefixes expand directions (`3n2e` → `n;n;n;e;e`)
- **Variable expansion** - Variables expanded in aliases and commands (`$target`)
- **Pattern matching** - Wildcards (`%*`, `%1`-`%99`) and anchors (`^`) in aliases, actions, and highlights
- **Partial command matching** - Commands can be abbreviated (`#al` → `#alias`)
- **Case-insensitive commands** - All commands work in any case (`#ALIAS`, `#Alias`, `#alias`)

### Command Syntax

- **Braced format**: `#alias {name} {commands}` (required for multi-word arguments)
- **Unbraced format**: `#alias name commands` (optional for single words)

## Quick Start

Load the TinTin++ emulator:

```bash
./telnet-gui -l lisp/tintin.lisp <host> <port>
```

The emulator automatically activates when loaded.

## Speedwalk

Speedwalk notation is supported. Diagonal directions (`ne`, `nw`, `se`, `sw`) are **disabled by default** and parse as separate directions.

Enable diagonals:

```lisp
(set! *tintin-speedwalk-diagonals* #t)
```

## File Paths

`#save` and `#load` support home directory expansion:

- `#save {~/config.lisp}` - Saves to home directory
- `#load {~/config.lisp}` - Loads from home directory

## Configuration Files

Create a configuration file to pre-define aliases and variables:

**my-config.lisp:**

```lisp
(load "lisp/tintin.lisp")
(tintin-process-command "#alias {k} {kill %1}")
(tintin-process-command "#variable {target} {orc}")
```

Load it:

```bash
./telnet-gui -l my-config.lisp <host> <port>
```

## Runtime Control

Toggle TinTin++ processing (use eval mode, Ctrl+E):

```lisp
(tintin-disable!)    ; Turn off
(tintin-enable!)     ; Turn on
(tintin-toggle!)     ; Toggle
```

## Differences from Standard TinTin++

- **Maximum alias expansion depth**: 10 levels (configurable via `*tintin-max-alias-depth*`)
- **Diagonal speedwalk directions**: Disabled by default (standard TinTin++ enables them)
- **Highlight priority**: Default priority is 5 (managed via Lisp, not command-line)
- **Not implemented**: Many TinTin++ commands are not supported (see Limitations below)

## Limitations

This is a **minimal implementation** focused on core scripting features. Many TinTin++ commands and features are **not emulated**, including but not limited to:

- Most TinTin++ commands beyond the core set listed above
- Complex trigger systems beyond basic actions
- Scripting language features beyond basic variable/alias expansion
- Many advanced TinTin++ configuration options

For full TinTin++ functionality, use the official TinTin++ client.

## Testing

Run the test suite:

```bash
./build/lisp-repl lisp/tintin-test.lisp
```

## Implementation Details

### Integration

The emulator hooks into telnet-gui via:

- `user-input-hook` - Processes user input (aliases, variables, speedwalk)
- `telnet-input-filter-hook` - Applies highlights to server output
- `telnet-input-hook` - Triggers actions on server output

## Source Files

- `lisp/tintin.lisp` - Implementation
- `lisp/tintin-test.lisp` - Test suite
- `TINTIN.md` - This file
- `TINTIN_GRAMMAR.md` - Formal grammar specification
