/* Glyph cache for terminal rendering */

#ifndef GLYPH_CACHE_H
#define GLYPH_CACHE_H

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

typedef struct GlyphCache GlyphCache;

/* Create a new glyph cache with the given font */
/* hinting_mode: TTF_HINTING_NONE, TTF_HINTING_LIGHT, TTF_HINTING_NORMAL, or TTF_HINTING_MONO */
/* scale_mode: SDL_ScaleModeNearest or SDL_ScaleModeLinear */
/* hdpi, vdpi: Horizontal and vertical DPI for font rendering (e.g., 96, 96 for Windows default) */
GlyphCache *glyph_cache_create(SDL_Renderer *renderer, const char *font_path, const char *font_name,
                                int font_size, int hinting_mode, SDL_ScaleMode scale_mode, int hdpi, int vdpi);

/* Get a cached texture for a glyph */
/* is_emoji: if true and emoji font available, use it even if main font has the glyph */
SDL_Texture *glyph_cache_get(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color, int bold,
                             int italic, int is_emoji);

/* Get cell dimensions (width and height) */
void glyph_cache_get_cell_size(GlyphCache *cache, int *cell_w, int *cell_h);

/* Get font path */
const char *glyph_cache_get_font_path(GlyphCache *cache);

/* Get font name */
const char *glyph_cache_get_font_name(GlyphCache *cache);

/* Get the actual rendered width of a glyph (returns cached width if available, or renders and caches it) */
int glyph_cache_get_glyph_width(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color);

/* Get the left bearing (minx) of a glyph for precise positioning */
int glyph_cache_get_minx(GlyphCache *cache, uint32_t codepoint, SDL_Color fg_color, SDL_Color bg_color);

/* Clean up and destroy the cache */
void glyph_cache_destroy(GlyphCache *cache);

/* Check if codepoint is in a symbol range that needs emoji/symbol font fallback. */
static inline int is_symbol_range(uint32_t codepoint) {
    /* Geometric Shapes (U+25A0-U+25FF) - includes ◆ ◇ diamonds */
    if (codepoint >= 0x25A0 && codepoint <= 0x25FF)
        return 1;
    /* Miscellaneous Symbols (U+2600-U+26FF) - includes ⚔ ☆ ★ */
    if (codepoint >= 0x2600 && codepoint <= 0x26FF)
        return 1;
    /* Dingbats (U+2700-U+27BF) - includes ✦ ✧ ✂ ✈ */
    if (codepoint >= 0x2700 && codepoint <= 0x27BF)
        return 1;
    return 0;
}

/* Check if codepoint has Unicode Emoji_Presentation=Yes (defaults to emoji style, true 2-cell).
 * Based on Unicode 15.0 emoji-data.txt */
static inline int is_emoji_presentation(uint32_t cp) {
    if (cp == 0x231A || cp == 0x231B) return 1; /* ⌚⌛ watch, hourglass */
    if (cp >= 0x23E9 && cp <= 0x23F3) return 1; /* ⏩-⏳ media controls */
    if (cp >= 0x23F8 && cp <= 0x23FA) return 1; /* ⏸⏹⏺ */
    if (cp == 0x25FD || cp == 0x25FE) return 1; /* ◽◾ squares */
    if (cp == 0x2614 || cp == 0x2615) return 1; /* ☔☕ */
    if (cp >= 0x2648 && cp <= 0x2653) return 1; /* ♈-♓ zodiac */
    if (cp == 0x267F) return 1; /* ♿ wheelchair */
    if (cp == 0x2693) return 1; /* ⚓ anchor */
    if (cp == 0x26A1) return 1; /* ⚡ high voltage */
    if (cp == 0x26AA || cp == 0x26AB) return 1; /* ⚪⚫ circles */
    if (cp == 0x26BD || cp == 0x26BE) return 1; /* ⚽⚾ sports */
    if (cp == 0x26C4 || cp == 0x26C5) return 1; /* ⛄⛅ weather */
    if (cp == 0x26CE) return 1; /* ⛎ ophiuchus */
    if (cp == 0x26D4) return 1; /* ⛔ no entry */
    if (cp == 0x26EA) return 1; /* ⛪ church */
    if (cp == 0x26F2 || cp == 0x26F3) return 1; /* ⛲⛳ fountain, golf */
    if (cp == 0x26F5) return 1; /* ⛵ sailboat */
    if (cp == 0x26FA) return 1; /* ⛺ tent */
    if (cp == 0x26FD) return 1; /* ⛽ fuel pump */
    if (cp == 0x2702) return 1; /* ✂ scissors */
    if (cp == 0x2705) return 1; /* ✅ check */
    if (cp >= 0x2708 && cp <= 0x270D) return 1; /* ✈✉✊✋✌✍ */
    if (cp == 0x270F) return 1; /* ✏ pencil */
    if (cp == 0x2712) return 1; /* ✒ pen */
    if (cp == 0x2714) return 1; /* ✔ check */
    if (cp == 0x2716) return 1; /* ✖ multiply */
    if (cp == 0x271D) return 1; /* ✝ cross */
    if (cp == 0x2721) return 1; /* ✡ star of david */
    if (cp == 0x2728) return 1; /* ✨ sparkles */
    if (cp == 0x2733 || cp == 0x2734) return 1; /* ✳✴ */
    if (cp == 0x2744) return 1; /* ❄ snowflake */
    if (cp == 0x2747) return 1; /* ❇ sparkle */
    if (cp == 0x274C || cp == 0x274E) return 1; /* ❌❎ */
    if (cp >= 0x2753 && cp <= 0x2755) return 1; /* ❓❔❕ */
    if (cp == 0x2757) return 1; /* ❗ */
    if (cp == 0x2763 || cp == 0x2764) return 1; /* ❣❤ hearts */
    if (cp >= 0x2795 && cp <= 0x2797) return 1; /* ➕➖➗ */
    if (cp == 0x27A1) return 1; /* ➡ arrow */
    if (cp == 0x27B0 || cp == 0x27BF) return 1; /* ➰➿ loops */
    if (cp >= 0x2934 && cp <= 0x2935) return 1; /* ⤴⤵ arrows */
    if (cp >= 0x2B05 && cp <= 0x2B07) return 1; /* ⬅⬆⬇ arrows */
    if (cp == 0x2B1B || cp == 0x2B1C) return 1; /* ⬛⬜ squares */
    if (cp == 0x2B50) return 1; /* ⭐ star */
    if (cp == 0x2B55) return 1; /* ⭕ circle */
    if (cp == 0x3030) return 1; /* 〰 wavy dash */
    if (cp == 0x303D) return 1; /* 〽 */
    if (cp == 0x3297 || cp == 0x3299) return 1; /* ㊗㊙ */
    return 0;
}

/* Check if codepoint has Unicode Emoji=Yes property (render large but may occupy 1 cell).
 * Based on Unicode 15.0 emoji-data.txt */
static inline int is_emoji_property(uint32_t cp) {
    /* Common emoji symbols in Misc Symbols block (U+2600-U+26FF) */
    if (cp >= 0x2600 && cp <= 0x2605) return 1; /* ☀☁☂☃☄★ - but ★ is not emoji */
    if (cp == 0x2600 || cp == 0x2601 || cp == 0x2602 || cp == 0x2603 || cp == 0x2604) return 1;
    if (cp == 0x260E) return 1; /* ☎ telephone */
    if (cp == 0x2611) return 1; /* ☑ ballot box */
    if (cp == 0x2614 || cp == 0x2615) return 1; /* ☔☕ */
    if (cp == 0x2618) return 1; /* ☘ shamrock */
    if (cp == 0x261D) return 1; /* ☝ pointing up */
    if (cp == 0x2620) return 1; /* ☠ skull */
    if (cp == 0x2622 || cp == 0x2623) return 1; /* ☢☣ radioactive, biohazard */
    if (cp == 0x2626) return 1; /* ☦ orthodox cross */
    if (cp == 0x262A) return 1; /* ☪ star and crescent */
    if (cp == 0x262E || cp == 0x262F) return 1; /* ☮☯ peace, yin yang */
    if (cp >= 0x2638 && cp <= 0x263A) return 1; /* ☸☹☺ */
    if (cp == 0x2640 || cp == 0x2642) return 1; /* ♀♂ gender symbols */
    if (cp >= 0x2648 && cp <= 0x2653) return 1; /* ♈-♓ zodiac */
    if (cp == 0x265F) return 1; /* ♟ chess pawn */
    /* ♠♣♥♦ (U+2660, U+2663, U+2665, U+2666) have Emoji=Yes but Emoji_Presentation=No, so 1-cell */
    if (cp == 0x2668) return 1; /* ♨ hot springs */
    if (cp == 0x267B) return 1; /* ♻ recycling */
    if (cp == 0x267E || cp == 0x267F) return 1; /* ♾♿ infinity, wheelchair */
    if (cp >= 0x2692 && cp <= 0x2697) return 1; /* ⚒⚓⚔⚕⚖⚗ tools */
    if (cp == 0x2699) return 1; /* ⚙ gear */
    if (cp == 0x269B || cp == 0x269C) return 1; /* ⚛⚜ atom, fleur-de-lis */
    if (cp == 0x26A0 || cp == 0x26A1) return 1; /* ⚠⚡ warning, lightning */
    if (cp == 0x26A7) return 1; /* ⚧ transgender */
    if (cp == 0x26AA || cp == 0x26AB) return 1; /* ⚪⚫ circles */
    if (cp == 0x26B0 || cp == 0x26B1) return 1; /* ⚰⚱ coffin, urn */
    if (cp == 0x26BD || cp == 0x26BE) return 1; /* ⚽⚾ soccer, baseball */
    if (cp == 0x26C4 || cp == 0x26C5) return 1; /* ⛄⛅ snowman, sun behind cloud */
    if (cp == 0x26C8) return 1; /* ⛈ thunder cloud */
    if (cp == 0x26CE || cp == 0x26CF) return 1; /* ⛎⛏ ophiuchus, pick */
    if (cp == 0x26D1) return 1; /* ⛑ helmet */
    if (cp == 0x26D3 || cp == 0x26D4) return 1; /* ⛓⛔ chains, no entry */
    if (cp == 0x26E9 || cp == 0x26EA) return 1; /* ⛩⛪ shrine, church */
    if (cp >= 0x26F0 && cp <= 0x26F5) return 1; /* ⛰⛱⛲⛳⛴⛵ */
    if (cp >= 0x26F7 && cp <= 0x26FA) return 1; /* ⛷⛸⛹⛺ */
    if (cp == 0x26FD) return 1; /* ⛽ fuel pump */
    /* Dingbats block (U+2700-U+27BF) with Emoji=Yes */
    if (cp == 0x2702) return 1; /* ✂ scissors */
    if (cp == 0x2705) return 1; /* ✅ check mark */
    if (cp >= 0x2708 && cp <= 0x270D) return 1; /* ✈✉✊✋✌✍ */
    if (cp == 0x270F) return 1; /* ✏ pencil */
    if (cp == 0x2712) return 1; /* ✒ pen */
    if (cp == 0x2714) return 1; /* ✔ check mark */
    if (cp == 0x2716) return 1; /* ✖ multiplication */
    if (cp == 0x271D) return 1; /* ✝ cross */
    if (cp == 0x2721) return 1; /* ✡ star of david */
    if (cp == 0x2728) return 1; /* ✨ sparkles */
    if (cp == 0x2733 || cp == 0x2734) return 1; /* ✳✴ asterisks */
    if (cp == 0x2744) return 1; /* ❄ snowflake */
    if (cp == 0x2747) return 1; /* ❇ sparkle */
    if (cp == 0x274C) return 1; /* ❌ cross mark */
    if (cp == 0x274E) return 1; /* ❎ cross mark button */
    if (cp >= 0x2753 && cp <= 0x2755) return 1; /* ❓❔❕ */
    if (cp == 0x2757) return 1; /* ❗ exclamation */
    if (cp == 0x2763 || cp == 0x2764) return 1; /* ❣❤ hearts */
    if (cp >= 0x2795 && cp <= 0x2797) return 1; /* ➕➖➗ math */
    if (cp == 0x27A1) return 1; /* ➡ right arrow */
    if (cp == 0x27B0) return 1; /* ➰ curly loop */
    if (cp == 0x27BF) return 1; /* ➿ double curly loop */
    /* Other blocks with Emoji=Yes */
    if (cp == 0x203C) return 1; /* ‼ double exclamation */
    if (cp == 0x2049) return 1; /* ⁉ exclamation question */
    if (cp >= 0x2194 && cp <= 0x2199) return 1; /* ↔↕↖↗↘↙ arrows */
    if (cp == 0x21A9 || cp == 0x21AA) return 1; /* ↩↪ arrows */
    if (cp == 0x231A || cp == 0x231B) return 1; /* ⌚⌛ watch, hourglass */
    if (cp == 0x2328) return 1; /* ⌨ keyboard */
    if (cp == 0x23CF) return 1; /* ⏏ eject */
    if (cp >= 0x23E9 && cp <= 0x23F3) return 1; /* ⏩-⏳ media controls */
    if (cp >= 0x23F8 && cp <= 0x23FA) return 1; /* ⏸⏹⏺ media controls */
    if (cp == 0x24C2) return 1; /* Ⓜ circled M */
    if (cp == 0x25AA || cp == 0x25AB) return 1; /* ▪▫ squares */
    if (cp == 0x25B6) return 1; /* ▶ play */
    if (cp == 0x25C0) return 1; /* ◀ reverse */
    if (cp >= 0x25FB && cp <= 0x25FE) return 1; /* ◻◼◽◾ squares */
    if (cp >= 0x2934 && cp <= 0x2935) return 1; /* ⤴⤵ arrows */
    if (cp >= 0x2B05 && cp <= 0x2B07) return 1; /* ⬅⬆⬇ arrows */
    if (cp == 0x2B1B || cp == 0x2B1C) return 1; /* ⬛⬜ squares */
    if (cp == 0x2B50) return 1; /* ⭐ star */
    if (cp == 0x2B55) return 1; /* ⭕ circle */
    if (cp == 0x3030) return 1; /* 〰 wavy dash */
    if (cp == 0x303D) return 1; /* 〽 part alternation mark */
    if (cp == 0x3297) return 1; /* ㊗ congratulations */
    if (cp == 0x3299) return 1; /* ㊙ secret */
    return 0;
}

/* Check if codepoint renders at 2-cell width (true emoji or emoji-style that overflows).
 * This is used to detect emoji that visually occupy 2 cells even if vterm treats them as 1. */
static inline int is_emoji_2wide(uint32_t cp, uint32_t second_char) {
    return (second_char == 0xFE0F) ||           /* Variation selector forces emoji presentation */
           is_emoji_presentation(cp) ||          /* Emoji_Presentation=Yes */
           is_emoji_property(cp) ||              /* Emoji=Yes property */
           (cp >= 0x1F300 && cp <= 0x1F5FF) ||   /* Misc Symbols and Pictographs */
           (cp >= 0x1F600 && cp <= 0x1F64F) ||   /* Emoticons */
           (cp >= 0x1F680 && cp <= 0x1F6FF) ||   /* Transport/Map */
           (cp >= 0x1F900 && cp <= 0x1F9FF) ||   /* Supplemental Symbols */
           (cp >= 0x1FA00 && cp <= 0x1FAFF);     /* Chess, Extended-A symbols */
}

/* Check if codepoint is a block element (U+2580-U+259F).
 * These are 1-cell wide but need to fill the entire cell. */
static inline int is_block_element(uint32_t codepoint) {
    return (codepoint >= 0x2580 && codepoint <= 0x259F);
}

#endif /* GLYPH_CACHE_H */
