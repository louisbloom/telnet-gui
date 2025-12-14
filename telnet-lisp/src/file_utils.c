/* Cross-platform file utilities with UTF-8 support */

#include "../include/file_utils.h"
#include <stdlib.h>
#include <sys/stat.h>

#ifdef _WIN32
#include <windows.h>
#include <wchar.h>

/* Convert UTF-8 string to UTF-16 (wide char) for Windows APIs.
 * Caller must free the returned buffer with free().
 *
 * Returns: Wide char string on success, NULL on failure
 */
static wchar_t *utf8_to_utf16(const char *utf8_str) {
    if (!utf8_str)
        return NULL;

    /* Get required buffer size */
    int wchar_count = MultiByteToWideChar(CP_UTF8, 0, utf8_str, -1, NULL, 0);
    if (wchar_count == 0)
        return NULL;

    /* Allocate buffer */
    wchar_t *wchar_str = (wchar_t *)malloc(wchar_count * sizeof(wchar_t));
    if (!wchar_str)
        return NULL;

    /* Convert UTF-8 to UTF-16 */
    if (MultiByteToWideChar(CP_UTF8, 0, utf8_str, -1, wchar_str, wchar_count) == 0) {
        free(wchar_str);
        return NULL;
    }

    return wchar_str;
}
#endif

FILE *file_open(const char *utf8_path, const char *mode) {
    if (!utf8_path || !mode)
        return NULL;

#ifdef _WIN32
    /* Convert UTF-8 path and mode to UTF-16 */
    wchar_t *wpath = utf8_to_utf16(utf8_path);
    if (!wpath)
        return NULL;

    wchar_t *wmode = utf8_to_utf16(mode);
    if (!wmode) {
        free(wpath);
        return NULL;
    }

    /* Open file with wide char API */
    FILE *file = _wfopen(wpath, wmode);

    /* Clean up */
    free(wpath);
    free(wmode);

    return file;
#else
    /* On Unix/macOS, fopen() already handles UTF-8 */
    return fopen(utf8_path, mode);
#endif
}

int file_remove(const char *utf8_path) {
    if (!utf8_path)
        return -1;

#ifdef _WIN32
    /* Convert UTF-8 path to UTF-16 */
    wchar_t *wpath = utf8_to_utf16(utf8_path);
    if (!wpath)
        return -1;

    /* Remove file with wide char API */
    int result = _wremove(wpath);

    /* Clean up */
    free(wpath);

    return result;
#else
    /* On Unix/macOS, remove() already handles UTF-8 */
    return remove(utf8_path);
#endif
}

int file_mkdir(const char *utf8_path) {
    if (!utf8_path)
        return -1;

#ifdef _WIN32
    /* Convert UTF-8 path to UTF-16 */
    wchar_t *wpath = utf8_to_utf16(utf8_path);
    if (!wpath)
        return -1;

    /* Create directory with wide char API */
    int result = _wmkdir(wpath);

    /* Clean up */
    free(wpath);

    return result;
#else
    /* On Unix/macOS, mkdir() already handles UTF-8 */
    return mkdir(utf8_path, 0755);
#endif
}
