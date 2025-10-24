#include "../include/lisp.h"
#include <string.h>
#include <stdlib.h>

#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>

/* Regex helper functions */

/* Compile a PCRE2 regex pattern */
pcre2_code* compile_regex_pattern(const char* pattern, char** error_msg) {
    int errornumber;
    PCRE2_SIZE erroroffset;

    pcre2_code* re = pcre2_compile(
        (PCRE2_SPTR)pattern,
        PCRE2_ZERO_TERMINATED,
        0,  /* options */
        &errornumber,
        &erroroffset,
        NULL
    );

    if (re == NULL) {
        /* Get error message */
        PCRE2_UCHAR buffer[256];
        pcre2_get_error_message(errornumber, buffer, sizeof(buffer));
        *error_msg = GC_strdup((char*)buffer);
        return NULL;
    }

    return re;
}

/* Execute regex match and return match data */
pcre2_match_data* execute_regex(pcre2_code* re, const char* subject) {
    pcre2_match_data* match_data = pcre2_match_data_create_from_pattern(re, NULL);

    int rc = pcre2_match(
        re,
        (PCRE2_SPTR)subject,
        strlen(subject),
        0,  /* start offset */
        0,  /* options */
        match_data,
        NULL
    );

    if (rc < 0) {
        pcre2_match_data_free(match_data);
        return NULL;
    }

    return match_data;
}

/* Extract a specific capture group */
char* extract_capture(pcre2_match_data* match_data, const char* subject, int capture_num) {
    PCRE2_SIZE* ovector = pcre2_get_ovector_pointer(match_data);
    int start = ovector[2 * capture_num];
    int end = ovector[2 * capture_num + 1];

    if (start < 0 || end < 0) {
        return NULL;
    }

    int length = end - start;
    char* result = GC_malloc(length + 1);
    strncpy(result, subject + start, length);
    result[length] = '\0';

    return result;
}

/* Get number of captures */
int get_capture_count(pcre2_code* re) {
    uint32_t count;
    pcre2_pattern_info(re, PCRE2_INFO_CAPTURECOUNT, &count);
    return (int)count;
}

/* Free regex resources */
void free_regex_resources(pcre2_code* re, pcre2_match_data* match_data) {
    if (match_data) {
        pcre2_match_data_free(match_data);
    }
    if (re) {
        pcre2_code_free(re);
    }
}
