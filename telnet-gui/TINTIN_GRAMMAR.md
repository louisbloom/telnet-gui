# TinTin++ Mini-Language Grammar

This document specifies the formal grammar for our mini TinTin++ implementation.

## EBNF Grammar

```ebnf
(* Input Line *)
input_line      = command_list ;
command_list    = command { ";" command } ;
command         = tintin_cmd | regular_cmd | speedwalk | "" ;

(* TinTin++ Commands *)
tintin_cmd      = "#" cmd_name arguments ;
cmd_name        = identifier ;

(* Mixed Format: each argument can be braced or unbraced independently *)
arguments       = { ws argument } ;
argument        = braced_arg | unbraced_arg ;
braced_arg      = "{" braced_text "}" ;
braced_text     = { char | "{" braced_text "}" | variable | placeholder } ;
unbraced_arg    = token ;
token           = identifier | variable ;

(* Regular Commands (sent to server) *)
regular_cmd     = expandable { ws expandable } ;
expandable      = token | variable ;

(* Speedwalk Notation *)
speedwalk       = { digit } direction { { digit } direction } ;
direction       = "n" | "s" | "e" | "w" | "u" | "d" | "ne" | "nw" | "se" | "sw" ;

(* Variable and Placeholder Expansion *)
variable        = "$" identifier ;
placeholder     = "%" ( "0" | digit { digit } ) ;

(* Lexical Elements *)
identifier      = ( letter | "_" ) { letter | digit | "_" | "-" } ;
digit           = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
letter          = "a".."z" | "A".."Z" ;
ws              = { " " | "\t" } ;
char            = ? any character except { } ? ;
```

## Parsing Rules

### 1. Command Name Extraction

- Start after '#'
- Skip whitespace
- Extract until space or '{' (first non-identifier char)
- Case-insensitive, prefix-matching allowed
- Examples:
  - `#alias` → "alias"
  - `#al` → "alias" (prefix match)
  - `#VARIABLE` → "variable" (case-insensitive)

### 2. Mixed Format Arguments

After extracting the command name, repeatedly:
1. Skip whitespace
2. If next char is '{': extract braced argument
3. Otherwise: extract unbraced token
4. Continue until N arguments collected

**Key feature:** Arguments can mix braced and unbraced formats independently.

Examples:
- `#alias bag {kill %1}` - arg[0]="bag" (unbraced), arg[1]="{kill %1}" (braced)
- `#variable {my var} value` - arg[0]="{my var}" (braced), arg[1]="value" (unbraced)

### 3. Braced Arguments

- Enclosed in `{}`
- Braces can be nested (track depth)
- Semicolons inside braces are literal (not command separators)
- Extract from opening '{' to matching '}'
- **IMPORTANT:** Preserve braces in the extracted value

Examples:
- `{simple text}` → extract as `"{simple text}"`
- `{nested {braces}}` → extract as `"{nested {braces}}"`
- `{cmd1;cmd2}` → semicolon is literal, extract as `"{cmd1;cmd2}"`

### 4. Unbraced Arguments

- Space-delimited token
- No multi-word values (single token)
- Extract until whitespace or end of line

Examples:
- `bag` → extract as `"bag"`
- `#var` → extract as `"#var"` (# is allowed in argument)

### 5. Variable Expansion

- Syntax: `$varname`
- Lookup in `*tintin-variables*` hash table
- Expanded **before** alias matching
- Applied recursively in alias results

Example:
```
#variable {target} {orc}
kill $target  → expands to "kill orc"
```

### 6. Placeholder Substitution

- `%0` → all arguments
- `%1`, `%2`, etc. → individual arguments (1-indexed)
- Applied **after** alias matching

Example:
```
#alias {k} {kill %1}
k orc  → expands to "kill orc"
```

## Examples with Parse Trees

### Example 1: Mixed Format
```
Input: #alias bag {#var bag %1}

Parse Tree:
tintin_cmd
├─ cmd_name: "alias"
└─ arguments (mixed format)
   ├─ arg[0]: "bag"              (unbraced token)
   └─ arg[1]: "{#var bag %1}"    (braced text, braces preserved)

Result: Creates alias "bag" with command "{#var bag %1}"
```

### Example 2: All Braced
```
Input: #variable {my var} {some value}

Parse Tree:
tintin_cmd
├─ cmd_name: "variable"
└─ arguments (all braced)
   ├─ arg[0]: "{my var}"         (braced, preserves spaces)
   └─ arg[1]: "{some value}"     (braced, preserves spaces)

Result: Creates variable "my var" with value "some value"
```

### Example 3: All Unbraced
```
Input: #alias k kill

Parse Tree:
tintin_cmd
├─ cmd_name: "alias"
└─ arguments (all unbraced)
   ├─ arg[0]: "k"
   └─ arg[1]: "kill"

Result: Creates alias "k" with command "kill"
```

### Example 4: Nested Braces
```
Input: #alias {ef} {get {food}; eat {food}}

Parse Tree:
tintin_cmd
├─ cmd_name: "alias"
└─ arguments (all braced)
   ├─ arg[0]: "{ef}"                          (braced)
   └─ arg[1]: "{get {food}; eat {food}}"      (braced, nested braces preserved)

Result: Creates alias "ef" with nested braces in command
```

## Edge Cases

### Empty Arguments
```
#alias {} {}  → Creates alias with empty name and empty command
```

### Special Characters in Arguments
```
#alias {test!} {echo @#$%}  → All characters allowed inside braces
```

### Command Name Prefix Matching
```
#al {k} {kill}    → "al" matches "alias"
#var {x} {10}     → "var" matches "variable"
#ALIAS {k} {kill} → Case-insensitive matching
```

### Semicolons
```
Regular: n;s;e      → Three separate commands
Braced: {n;s;e}     → Single argument with semicolons
```

## Implementation Notes

1. **Parser must skip past command name** before extracting arguments
2. **Braces must be preserved** in braced arguments
3. **Mixed format** is supported - each argument is independently braced or unbraced
4. **Nested braces** require depth tracking
5. **Variable expansion** happens before alias matching
6. **Placeholder substitution** happens after alias matching
