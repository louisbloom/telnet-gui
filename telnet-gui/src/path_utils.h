/* Path utilities for cross-platform path handling */

#ifndef PATH_UTILS_H
#define PATH_UTILS_H

#include <stddef.h>
#include <limits.h>

/* Maximum path length for all path buffers */
#ifdef _WIN32
#define TELNET_MAX_PATH 32768  /* Windows: UNICODE_STRING max (32767 chars + null) */
#else
#define TELNET_MAX_PATH PATH_MAX  /* Unix/Linux/macOS system maximum */
#endif

/* Convert Unix path to platform-native path (in-place).
 * On Windows: converts / to \
 * On Unix: no-op
 */
void path_normalize_for_platform(char *path);

/* Check if path ends with /bin/ or \bin\ - indicates installed location */
int path_is_installed_bin_directory(const char *path);

/* Construct POSIX-compliant data directory from executable path.
 * If exe is in .../bin/, returns .../share/telnet-gui/
 * Returns 1 if successful and fills out_path, 0 otherwise.
 */
int path_construct_data_directory(const char *base_path, char *out_path, size_t out_path_size);

/* Construct executable-relative path for a file.
 * Returns 1 if path should be used (not in bin directory), 0 otherwise.
 * If returning 1, fills out_path with the constructed path.
 */
int path_construct_exe_relative(const char *base_path, const char *filename, char *out_path, size_t out_path_size);

#endif /* PATH_UTILS_H */
