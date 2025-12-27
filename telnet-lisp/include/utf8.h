#ifndef UTF8_H
#define UTF8_H

#include <stddef.h>

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
