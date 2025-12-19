#include "../include/utf8.h"
#include <stdlib.h>

/* Count UTF-8 characters in string (not bytes) */
size_t utf8_strlen(const char *str) {
    if (str == NULL)
        return 0;

    size_t count = 0;
    const char *ptr = str;
    while (*ptr) {
        /* Check if this is start of UTF-8 character */
        if ((*ptr & 0x80) == 0) {
            /* ASCII character */
            count++;
            ptr++;
        } else if ((*ptr & 0xE0) == 0xC0) {
            /* 2-byte character */
            count++;
            ptr += 2;
        } else if ((*ptr & 0xF0) == 0xE0) {
            /* 3-byte character */
            count++;
            ptr += 3;
        } else if ((*ptr & 0xF8) == 0xF0) {
            /* 4-byte character */
            count++;
            ptr += 4;
        } else {
            /* Invalid UTF-8 sequence */
            ptr++;
        }
    }
    return count;
}

/* Get character at character index (not byte index) */
const char *utf8_char_at(const char *str, size_t char_index) {
    if (str == NULL)
        return NULL;

    size_t count = 0;
    const char *ptr = str;
    while (*ptr && count < char_index) {
        /* Advance to next character */
        if ((*ptr & 0x80) == 0) {
            ptr++;
        } else if ((*ptr & 0xE0) == 0xC0) {
            ptr += 2;
        } else if ((*ptr & 0xF0) == 0xE0) {
            ptr += 3;
        } else if ((*ptr & 0xF8) == 0xF0) {
            ptr += 4;
        } else {
            ptr++;
        }
        count++;
    }

    return (count == char_index) ? ptr : NULL;
}

/* Advance pointer to next UTF-8 character */
const char *utf8_next_char(const char *ptr) {
    if (ptr == NULL || *ptr == '\0')
        return NULL;

    if ((*ptr & 0x80) == 0) {
        return ptr + 1; /* ASCII */
    } else if ((*ptr & 0xE0) == 0xC0) {
        return ptr + 2; /* 2-byte */
    } else if ((*ptr & 0xF0) == 0xE0) {
        return ptr + 3; /* 3-byte */
    } else if ((*ptr & 0xF8) == 0xF0) {
        return ptr + 4; /* 4-byte */
    }
    return ptr + 1; /* Invalid, advance by 1 */
}

/* Validate UTF-8 sequence */
int utf8_validate(const char *str) {
    if (str == NULL)
        return 1;

    const char *ptr = str;
    while (*ptr) {
        if ((*ptr & 0x80) == 0) {
            ptr++;
        } else if ((*ptr & 0xE0) == 0xC0) {
            if ((*(ptr + 1) & 0xC0) != 0x80)
                return 0;
            ptr += 2;
        } else if ((*ptr & 0xF0) == 0xE0) {
            if ((*(ptr + 1) & 0xC0) != 0x80 || (*(ptr + 2) & 0xC0) != 0x80)
                return 0;
            ptr += 3;
        } else if ((*ptr & 0xF8) == 0xF0) {
            if ((*(ptr + 1) & 0xC0) != 0x80 || (*(ptr + 2) & 0xC0) != 0x80 || (*(ptr + 3) & 0xC0) != 0x80)
                return 0;
            ptr += 4;
        } else {
            return 0;
        }
    }
    return 1;
}

/* Get byte offset to character at char_index */
size_t utf8_byte_offset(const char *str, size_t char_index) {
    if (str == NULL)
        return 0;

    size_t count = 0;
    size_t byte_offset = 0;
    const char *ptr = str;

    while (*ptr && count < char_index) {
        int bytes = utf8_char_bytes(ptr);
        byte_offset += bytes;
        ptr += bytes;
        count++;
    }

    return byte_offset;
}

/* Count bytes in UTF-8 character at ptr */
int utf8_char_bytes(const char *ptr) {
    if (ptr == NULL || *ptr == '\0')
        return 0;

    if ((*ptr & 0x80) == 0) {
        return 1; /* ASCII */
    } else if ((*ptr & 0xE0) == 0xC0) {
        return 2; /* 2-byte */
    } else if ((*ptr & 0xF0) == 0xE0) {
        return 3; /* 3-byte */
    } else if ((*ptr & 0xF8) == 0xF0) {
        return 4; /* 4-byte */
    }
    return 1; /* Invalid, treat as 1 byte */
}

/* Get Unicode codepoint from UTF-8 character */
int utf8_get_codepoint(const char *ptr) {
    if (ptr == NULL || *ptr == '\0')
        return -1;

    if ((*ptr & 0x80) == 0) {
        return (unsigned char)*ptr; /* ASCII */
    } else if ((*ptr & 0xE0) == 0xC0) {
        return ((ptr[0] & 0x1F) << 6) | (ptr[1] & 0x3F);
    } else if ((*ptr & 0xF0) == 0xE0) {
        return ((ptr[0] & 0x0F) << 12) | ((ptr[1] & 0x3F) << 6) | (ptr[2] & 0x3F);
    } else if ((*ptr & 0xF8) == 0xF0) {
        return ((ptr[0] & 0x07) << 18) | ((ptr[1] & 0x3F) << 12) | ((ptr[2] & 0x3F) << 6) | (ptr[3] & 0x3F);
    }
    return -1; /* Invalid */
}

/* Encode Unicode codepoint as UTF-8 into buffer */
/* Returns number of bytes written (1-4) or 0 if invalid */
/* Buffer must have space for at least 5 bytes (4 UTF-8 bytes + null terminator) */
int utf8_put_codepoint(unsigned int codepoint, char *buf) {
    if (buf == NULL)
        return 0;

    if (codepoint < 0x80) {
        /* 1-byte sequence (ASCII) */
        buf[0] = (char)codepoint;
        buf[1] = '\0';
        return 1;
    } else if (codepoint < 0x800) {
        /* 2-byte sequence */
        buf[0] = (char)(0xC0 | (codepoint >> 6));
        buf[1] = (char)(0x80 | (codepoint & 0x3F));
        buf[2] = '\0';
        return 2;
    } else if (codepoint < 0x10000) {
        /* 3-byte sequence */
        buf[0] = (char)(0xE0 | (codepoint >> 12));
        buf[1] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
        buf[2] = (char)(0x80 | (codepoint & 0x3F));
        buf[3] = '\0';
        return 3;
    } else if (codepoint < 0x110000) {
        /* 4-byte sequence */
        buf[0] = (char)(0xF0 | (codepoint >> 18));
        buf[1] = (char)(0x80 | ((codepoint >> 12) & 0x3F));
        buf[2] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
        buf[3] = (char)(0x80 | (codepoint & 0x3F));
        buf[4] = '\0';
        return 4;
    }
    /* Invalid codepoint */
    buf[0] = '\0';
    return 0;
}

/* Get display width of a single codepoint */
static int codepoint_width(int cp) {
    if (cp < 0)
        return 0;

    /* Control characters */
    if (cp < 0x20 || (cp >= 0x7F && cp < 0xA0))
        return 0;

    /* Combining characters (zero width) */
    if ((cp >= 0x0300 && cp <= 0x036F) || /* Combining Diacritical Marks */
        (cp >= 0x1AB0 && cp <= 0x1AFF) || /* Combining Diacritical Marks Extended */
        (cp >= 0x1DC0 && cp <= 0x1DFF) || /* Combining Diacritical Marks Supplement */
        (cp >= 0x20D0 && cp <= 0x20FF) || /* Combining Diacritical Marks for Symbols */
        (cp >= 0xFE20 && cp <= 0xFE2F))   /* Combining Half Marks */
        return 0;

    /* Variation selectors (zero width) */
    if ((cp >= 0xFE00 && cp <= 0xFE0F) || /* Variation Selectors */
        (cp >= 0xE0100 && cp <= 0xE01EF)) /* Variation Selectors Supplement */
        return 0;

    /* Wide characters (width 2) */
    /* CJK ranges */
    if ((cp >= 0x1100 && cp <= 0x115F) ||   /* Hangul Jamo */
        (cp >= 0x2E80 && cp <= 0x9FFF) ||   /* CJK blocks */
        (cp >= 0xAC00 && cp <= 0xD7A3) ||   /* Hangul Syllables */
        (cp >= 0xF900 && cp <= 0xFAFF) ||   /* CJK Compatibility Ideographs */
        (cp >= 0xFE10 && cp <= 0xFE1F) ||   /* Vertical Forms */
        (cp >= 0xFE30 && cp <= 0xFE6F) ||   /* CJK Compatibility Forms */
        (cp >= 0xFF00 && cp <= 0xFF60) ||   /* Fullwidth Forms */
        (cp >= 0xFFE0 && cp <= 0xFFE6) ||   /* Fullwidth Forms */
        (cp >= 0x20000 && cp <= 0x2FFFF) || /* CJK Extension B+ */
        (cp >= 0x30000 && cp <= 0x3FFFF))   /* CJK Extension G+ */
        return 2;

    /* Emoji (width 2) */
    if ((cp >= 0x1F300 && cp <= 0x1F9FF) || /* Misc Symbols, Emoticons, etc */
        (cp >= 0x1FA00 && cp <= 0x1FAFF) || /* Chess, Extended-A */
        (cp >= 0x2600 && cp <= 0x26FF) ||   /* Misc Symbols (includes ⚡) */
        (cp >= 0x2700 && cp <= 0x27BF) ||   /* Dingbats */
        (cp >= 0x25A0 && cp <= 0x25FF))     /* Geometric Shapes (includes ▶) */
        return 2;

    /* Default width 1 */
    return 1;
}

/* Calculate display width of UTF-8 string in terminal columns */
int utf8_display_width(const char *str) {
    if (str == NULL)
        return 0;

    int width = 0;
    const char *ptr = str;

    while (*ptr) {
        int cp = utf8_get_codepoint(ptr);
        width += codepoint_width(cp);
        ptr = utf8_next_char(ptr);
        if (ptr == NULL)
            break;
    }

    return width;
}
