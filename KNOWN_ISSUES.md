# Known Issues

## Windows: White Bar Artifact on First Window Resize

**Platform:** Windows (MSYS2 UCRT64)
**Status:** Known Issue - Platform Limitation
**Severity:** Minor Visual Artifact

### Description

On Windows, when resizing the telnet-gui window for the first time in each direction (wider or taller), a ~10px white bar may briefly appear along the edge being resized. This artifact:

- Only occurs on the **first** resize in each direction
- Does not appear on subsequent resizes in the same direction
- Does not appear if the window is resized smaller, then larger again
- Disappears immediately when the resize operation completes

### Root Cause

This is a Windows-specific SDL2 behavior where:

1. The Windows window manager expands the window during drag operations
2. SDL2's renderer back buffer may not be immediately updated to match the new window size
3. The newly exposed window area shows the default white background before our resize event handler can clear it
4. On the first resize, SDL2's internal renderer state may not be fully synchronized with the window size

### Attempted Fixes

Multiple approaches were attempted to resolve this issue:

1. **Immediate clear on resize event** - Clearing the renderer at the start of `SDL_WINDOWEVENT_RESIZED` handler
2. **Separate SIZE_CHANGED handling** - Handling `SDL_WINDOWEVENT_SIZE_CHANGED` events during drag
3. **Main loop size checking** - Proactively checking window size in the main loop before event polling
4. **Event watch callbacks** - Using `SDL_AddEventWatch` to catch resize events earlier
5. **Continuous presentation during resize** - Presenting frames continuously while resizing

None of these approaches fully resolved the issue, as the white bar appears before any of our code can execute.

### Workaround

The artifact is cosmetic only and does not affect functionality. Users can:

- Ignore the brief white bar (it disappears immediately)
- Resize the window smaller then larger again to avoid the artifact
- The artifact only appears once per direction per session

### Future Considerations

Potential solutions that were not attempted:

- Using a different SDL2 renderer backend (software vs. accelerated)
- Setting a custom window background color via Windows API
- Using SDL2's window flags to control resize behavior
- Investigating SDL2 version-specific behavior

This issue may be resolved in future SDL2 versions or with different renderer configurations.

## Emoji with Variation Selectors Need Extra Spacing

**Platform:** All
**Status:** Known Limitation
**Severity:** Minor Visual Issue

### Description

Some emoji that use variation selectors (U+FE0F) to request emoji presentation are reported as 1-cell width by libvterm, but visually render as 2-cell width. This causes the emoji to overlap with the following character.

Affected emoji include:

- 🗡️ (U+1F5E1 + U+FE0F) - Dagger
- ⚔️ (U+2694 + U+FE0F) - Crossed Swords
- ▶️ (U+25B6 + U+FE0F) - Play Button

Unaffected emoji (inherently 2-cell width):

- ✨ (U+2728) - Sparkles
- 🎮 (U+1F3AE) - Game Controller
- 🏰 (U+1F3F0) - Castle
- 🐉 (U+1F409) - Dragon
- 🔮 (U+1F52E) - Crystal Ball

### Root Cause

The base characters (U+1F5E1, U+2694, U+25B6) are classified as narrow or ambiguous width in Unicode's East Asian Width property. The variation selector U+FE0F requests emoji presentation but doesn't change the width classification used by terminal emulators.

### Workaround

Add an extra space after affected emoji:

```lisp
;; Incorrect - will overlap
(tintin-echo "🗡️⚔️Text")

;; Correct - add space after each affected emoji
(tintin-echo "🗡️ ⚔️ Text")
```

### Technical Details

The renderer detects variation selectors and renders affected emoji at 2-cell width for proper appearance, but cannot change the terminal's text layout which already allocated only 1 cell.
