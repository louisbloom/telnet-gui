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
