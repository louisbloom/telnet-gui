/* Dynamic buffer implementation */

#include "dynamic_buffer.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>

#define MIN_BUFFER_SIZE 1024

/* Create a new dynamic buffer with initial size */
DynamicBuffer *dynamic_buffer_create(size_t initial_size) {
    DynamicBuffer *buf = (DynamicBuffer *)malloc(sizeof(DynamicBuffer));
    if (!buf)
        return NULL;

    if (initial_size < MIN_BUFFER_SIZE)
        initial_size = MIN_BUFFER_SIZE;

    buf->data = (char *)malloc(initial_size);
    if (!buf->data) {
        free(buf);
        return NULL;
    }

    buf->size = initial_size;
    buf->len = 0;
    buf->data[0] = '\0';

    return buf;
}

/* Destroy a dynamic buffer and free memory */
void dynamic_buffer_destroy(DynamicBuffer *buf) {
    if (!buf)
        return;
    if (buf->data)
        free(buf->data);
    free(buf);
}

/* Ensure buffer has at least 'required_size' bytes allocated (grows if needed) */
int dynamic_buffer_ensure_size(DynamicBuffer *buf, size_t required_size) {
    if (!buf)
        return -1;

    /* Already big enough */
    if (buf->size >= required_size)
        return 0;

    /* Calculate new size by doubling until large enough */
    size_t new_size = buf->size;
    if (new_size == 0)
        new_size = MIN_BUFFER_SIZE;

    while (new_size < required_size)
        new_size *= 2;

    /* Reallocate */
    char *new_data = (char *)realloc(buf->data, new_size);
    if (!new_data)
        return -1;

    buf->data = new_data;
    buf->size = new_size;

    return 0;
}

/* Append data to buffer (grows if needed), returns 0 on success, -1 on failure */
int dynamic_buffer_append(DynamicBuffer *buf, const char *data, size_t data_len) {
    if (!buf || !data)
        return -1;

    /* Ensure we have space for data + null terminator */
    size_t required = buf->len + data_len + 1;
    if (dynamic_buffer_ensure_size(buf, required) < 0)
        return -1;

    /* Append data */
    memcpy(buf->data + buf->len, data, data_len);
    buf->len += data_len;
    buf->data[buf->len] = '\0';

    return 0;
}

/* Append a null-terminated string to buffer (grows if needed) */
int dynamic_buffer_append_str(DynamicBuffer *buf, const char *str) {
    if (!buf || !str)
        return -1;
    return dynamic_buffer_append(buf, str, strlen(str));
}

/* Append formatted string to buffer (like snprintf, grows if needed) */
int dynamic_buffer_append_printf(DynamicBuffer *buf, const char *fmt, ...) {
    if (!buf || !fmt)
        return -1;

    /* Try with current available space */
    size_t available = buf->size - buf->len;

    va_list args;
    va_start(args, fmt);
    int written = vsnprintf(buf->data + buf->len, available, fmt, args);
    va_end(args);

    if (written < 0)
        return -1;

    /* If it fit, update length and return success */
    if ((size_t)written < available) {
        buf->len += written;
        return 0;
    }

    /* Didn't fit - grow buffer and try again */
    size_t required = buf->len + written + 1;
    if (dynamic_buffer_ensure_size(buf, required) < 0)
        return -1;

    /* Try again with larger buffer */
    available = buf->size - buf->len;
    va_start(args, fmt);
    written = vsnprintf(buf->data + buf->len, available, fmt, args);
    va_end(args);

    if (written < 0 || (size_t)written >= available)
        return -1;

    buf->len += written;
    return 0;
}

/* Get pointer to buffer data */
const char *dynamic_buffer_data(const DynamicBuffer *buf) {
    return buf ? buf->data : NULL;
}

/* Get current data length */
size_t dynamic_buffer_len(const DynamicBuffer *buf) {
    return buf ? buf->len : 0;
}

/* Get allocated buffer size */
size_t dynamic_buffer_size(const DynamicBuffer *buf) {
    return buf ? buf->size : 0;
}

/* Clear buffer (resets length to 0 without freeing memory) */
void dynamic_buffer_clear(DynamicBuffer *buf) {
    if (!buf)
        return;
    buf->len = 0;
    if (buf->data)
        buf->data[0] = '\0';
}

/* Reset buffer to initial state and free excess memory */
void dynamic_buffer_reset(DynamicBuffer *buf, size_t initial_size) {
    if (!buf)
        return;

    if (initial_size < MIN_BUFFER_SIZE)
        initial_size = MIN_BUFFER_SIZE;

    /* If current buffer is larger than initial_size, shrink it */
    if (buf->size > initial_size) {
        char *new_data = (char *)realloc(buf->data, initial_size);
        if (new_data) {
            buf->data = new_data;
            buf->size = initial_size;
        }
    }

    buf->len = 0;
    if (buf->data)
        buf->data[0] = '\0';
}
