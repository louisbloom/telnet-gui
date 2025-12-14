/* Cross-platform file utilities with UTF-8 support */

#ifndef FILE_UTILS_H
#define FILE_UTILS_H

#include <stdio.h>

/* Cross-platform file open with UTF-8 path support.
 * On Windows: converts UTF-8 path to UTF-16 and uses _wfopen()
 * On Unix/macOS: calls fopen() directly
 *
 * Parameters:
 *   utf8_path - File path in UTF-8 encoding
 *   mode - Standard fopen() mode string ("r", "w", "rb", etc.)
 *
 * Returns:
 *   FILE* on success, NULL on failure
 */
FILE *file_open(const char *utf8_path, const char *mode);

/* Cross-platform file removal with UTF-8 path support.
 * On Windows: converts UTF-8 path to UTF-16 and uses _wremove()
 * On Unix/macOS: calls remove() directly
 *
 * Parameters:
 *   utf8_path - File path in UTF-8 encoding
 *
 * Returns:
 *   0 on success, -1 on failure
 */
int file_remove(const char *utf8_path);

/* Cross-platform directory creation with UTF-8 path support.
 * On Windows: converts UTF-8 path to UTF-16 and uses _wmkdir()
 * On Unix/macOS: calls mkdir() directly with 0755 permissions
 *
 * Parameters:
 *   utf8_path - Directory path in UTF-8 encoding
 *
 * Returns:
 *   0 on success, -1 on failure
 */
int file_mkdir(const char *utf8_path);

#endif /* FILE_UTILS_H */
