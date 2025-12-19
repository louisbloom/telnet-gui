/* Dynamic buffer - auto-growing buffer for building strings/data */

#ifndef DYNAMIC_BUFFER_H
#define DYNAMIC_BUFFER_H

#include <stddef.h>

/* Dynamic buffer structure */
typedef struct {
    char *data;          /* Buffer data */
    size_t size;         /* Current buffer size (allocated) */
    size_t len;          /* Current data length (used) */
} DynamicBuffer;

/* Create a new dynamic buffer with initial size */
DynamicBuffer *dynamic_buffer_create(size_t initial_size);

/* Destroy a dynamic buffer and free memory */
void dynamic_buffer_destroy(DynamicBuffer *buf);

/* Ensure buffer has at least 'required_size' bytes allocated (grows if needed) */
int dynamic_buffer_ensure_size(DynamicBuffer *buf, size_t required_size);

/* Append data to buffer (grows if needed), returns 0 on success, -1 on failure */
int dynamic_buffer_append(DynamicBuffer *buf, const char *data, size_t data_len);

/* Append a null-terminated string to buffer (grows if needed) */
int dynamic_buffer_append_str(DynamicBuffer *buf, const char *str);

/* Append formatted string to buffer (like snprintf, grows if needed) */
int dynamic_buffer_append_printf(DynamicBuffer *buf, const char *fmt, ...) __attribute__((format(printf, 2, 3)));

/* Get pointer to buffer data */
const char *dynamic_buffer_data(const DynamicBuffer *buf);

/* Get current data length */
size_t dynamic_buffer_len(const DynamicBuffer *buf);

/* Get allocated buffer size */
size_t dynamic_buffer_size(const DynamicBuffer *buf);

/* Clear buffer (resets length to 0 without freeing memory) */
void dynamic_buffer_clear(DynamicBuffer *buf);

/* Reset buffer to initial state and free excess memory */
void dynamic_buffer_reset(DynamicBuffer *buf, size_t initial_size);

#endif /* DYNAMIC_BUFFER_H */
