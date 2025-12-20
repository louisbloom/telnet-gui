/* Path utilities for cross-platform path handling */

#include "path_utils.h"
#include "../../telnet-lisp/include/file_utils.h"
#include <SDL2/SDL.h>
#include <string.h>
#include <stdio.h>

/* Convert Unix path to platform-native path (in-place).
 * On Windows: converts / to \
 * On Unix: no-op
 */
void path_normalize_for_platform(char *path) {
    if (!path)
        return;

#ifdef _WIN32
    for (char *p = path; *p; p++) {
        if (*p == '/')
            *p = '\\';
    }
#endif
    // Unix: no-op, already using /
}

/* Check if path ends with /bin/ or \bin\ - indicates installed location */
int path_is_installed_bin_directory(const char *path) {
    if (!path)
        return 0;
    size_t len = strlen(path);
    if (len < 4)
        return 0;

    const char *end = path + len;
    /* Check if ends with /bin/ or \bin\ */
    if (len >= 5 && (end[-1] == '/' || end[-1] == '\\')) {
        if ((end[-5] == '/' || end[-5] == '\\') && end[-4] == 'b' && end[-3] == 'i' && end[-2] == 'n') {
            return 1;
        }
    }
    /* Check if ends with /bin or \bin (no trailing separator) */
    if ((end[-4] == '/' || end[-4] == '\\') && end[-3] == 'b' && end[-2] == 'i' && end[-1] == 'n') {
        return 1;
    }
    return 0;
}

/* Construct POSIX-compliant data directory from executable path.
 * If exe is in .../bin/, returns .../share/telnet-gui/
 * Returns 1 if successful and fills out_path, 0 otherwise.
 */
int path_construct_data_directory(const char *base_path, char *out_path, size_t out_path_size) {
    if (!base_path || !path_is_installed_bin_directory(base_path)) {
        return 0;
    }

    // Copy base path and strip trailing separator
    size_t len = strlen(base_path);
    char temp_path[1024]; // Reasonable size for path manipulation
    strncpy(temp_path, base_path, sizeof(temp_path) - 1);
    temp_path[sizeof(temp_path) - 1] = '\0';

    // Remove trailing separator if present
    if (len > 0 && (temp_path[len - 1] == '/' || temp_path[len - 1] == '\\')) {
        temp_path[len - 1] = '\0';
        len--;
    }

    // Find and remove the "bin" part (should be last 3 chars)
    if (len >= 3 && temp_path[len - 3] == 'b' && temp_path[len - 2] == 'i' && temp_path[len - 1] == 'n') {
        // Remove "bin"
        temp_path[len - 3] = '\0';
        // Remove separator before "bin" if present
        len -= 3;
        if (len > 0 && (temp_path[len - 1] == '/' || temp_path[len - 1] == '\\')) {
            temp_path[len - 1] = '\0';
        }
    } else {
        return 0; // Unexpected format
    }

    // Append share/telnet-gui (using Unix separator internally)
    snprintf(out_path, out_path_size, "%s/share/telnet-gui", temp_path);

    // Normalize for platform at the end
    path_normalize_for_platform(out_path);

    return 1;
}

/* Construct executable-relative path for a file.
 * Returns 1 if path should be used (not in bin directory), 0 otherwise.
 * If returning 1, fills out_path with the constructed path.
 */
int path_construct_exe_relative(const char *base_path, const char *filename, char *out_path, size_t out_path_size) {
    if (!base_path || path_is_installed_bin_directory(base_path)) {
        return 0;
    }

    size_t base_len = strlen(base_path);
    const char *sep = (base_len > 0 && (base_path[base_len - 1] == '/' || base_path[base_len - 1] == '\\')) ? "" : "/";

    // Construct path using Unix separator internally
    snprintf(out_path, out_path_size, "%s%s%s", base_path, sep, filename);

    // Normalize for platform at the end
    path_normalize_for_platform(out_path);

    return 1;
}

/* Check if a file exists at the given path.
 * Returns: 1 if exists, 0 if not
 */
int file_exists(const char *path) {
    FILE *f = file_open(path, "r");
    if (f) {
        fclose(f);
        return 1;
    }
    return 0;
}

/* Construct installed resource path: ${DATA_DIR}/${subdir}/${filename}
 * Handles SDL_GetBasePath(), data directory construction, and normalization.
 * Returns: 1 if path constructed (exe is in bin/), 0 if not installed
 */
int path_construct_installed_resource(const char *subdir, const char *filename, char *out_path, size_t out_path_size) {
    if (!subdir || !filename || !out_path || out_path_size == 0) {
        return 0;
    }

    char *base_path = SDL_GetBasePath();
    if (!base_path) {
        return 0;
    }

    char data_dir[TELNET_MAX_PATH];
    if (!path_construct_data_directory(base_path, data_dir, sizeof(data_dir))) {
        SDL_free(base_path);
        return 0;
    }
    SDL_free(base_path);

    snprintf(out_path, out_path_size, "%s/%s/%s", data_dir, subdir, filename);
    path_normalize_for_platform(out_path);

    return 1;
}
