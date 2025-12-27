#ifndef UTF8_H
#define UTF8_H

#include <stddef.h>
#include <stdint.h>

/* ============================================================================
 * Unicode Character Property Helpers
 * ============================================================================
 * These functions detect Unicode character properties based on codepoint ranges.
 * Based on Unicode 15.0 emoji-data.txt and character block definitions.
 */

/* Check if codepoint is in a symbol range that typically needs special font fallback.
 * Includes: Geometric Shapes, Miscellaneous Symbols, Dingbats */
static inline int utf8_is_symbol_range(uint32_t codepoint) {
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
static inline int utf8_is_emoji_presentation(uint32_t cp) {
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
static inline int utf8_is_emoji_property(uint32_t cp) {
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
 * This is used to detect emoji that visually occupy 2 cells even if vterm treats them as 1.
 * second_char: the next codepoint (for variation selector check) or 0 */
static inline int utf8_is_emoji_2wide(uint32_t cp, uint32_t second_char) {
    return (second_char == 0xFE0F) ||           /* Variation selector forces emoji presentation */
           utf8_is_emoji_presentation(cp) ||    /* Emoji_Presentation=Yes */
           utf8_is_emoji_property(cp) ||        /* Emoji=Yes property */
           (cp >= 0x1F300 && cp <= 0x1F5FF) ||  /* Misc Symbols and Pictographs */
           (cp >= 0x1F600 && cp <= 0x1F64F) ||  /* Emoticons */
           (cp >= 0x1F680 && cp <= 0x1F6FF) ||  /* Transport/Map */
           (cp >= 0x1F900 && cp <= 0x1F9FF) ||  /* Supplemental Symbols */
           (cp >= 0x1FA00 && cp <= 0x1FAFF);    /* Chess, Extended-A symbols */
}

/* Check if codepoint is a block element (U+2580-U+259F).
 * These are 1-cell wide but need to fill the entire cell. */
static inline int utf8_is_block_element(uint32_t codepoint) {
    return (codepoint >= 0x2580 && codepoint <= 0x259F);
}

/* ============================================================================
 * UTF-8 String Functions
 * ============================================================================ */

/* UTF-8 character counting - returns number of characters (not bytes) */
size_t utf8_strlen(const char *str);

/* Get UTF-8 character at index (character index, not byte index) */
/* Returns NULL if index is out of bounds */
const char *utf8_char_at(const char *str, size_t char_index);

/* Advance pointer to next UTF-8 character */
/* Returns pointer to next character, or NULL if end of string */
const char *utf8_next_char(const char *ptr);

/* Move pointer to previous UTF-8 character */
/* Returns pointer to previous character, or str if at beginning */
const char *utf8_prev_char(const char *str, const char *ptr);

/* Validate UTF-8 sequence */
/* Returns 1 if valid, 0 if invalid */
int utf8_validate(const char *str);

/* Get byte length of UTF-8 string up to character index */
/* Returns byte count from start of string to start of character at index */
size_t utf8_byte_offset(const char *str, size_t char_index);

/* Count bytes in a UTF-8 character starting at ptr */
/* Returns number of bytes (1-4) or 0 if invalid */
int utf8_char_bytes(const char *ptr);

/* Get next character as Unicode codepoint */
/* Returns codepoint or -1 if invalid */
int utf8_get_codepoint(const char *ptr);

/* Encode Unicode codepoint as UTF-8 into buffer */
/* Returns number of bytes written (1-4) or 0 if invalid */
/* Buffer must have space for at least 5 bytes (4 UTF-8 bytes + null terminator) */
int utf8_put_codepoint(unsigned int codepoint, char *buf);

/* Get display width of a single Unicode codepoint */
/* Returns 0 for control/combining chars, 2 for wide chars (CJK/emoji), 1 otherwise */
int utf8_codepoint_width(int codepoint);

/* Calculate display width of UTF-8 string in terminal columns */
/* Returns number of columns the string would occupy */
int utf8_display_width(const char *str);

#endif /* UTF8_H */
