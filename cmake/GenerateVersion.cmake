# GenerateVersion.cmake - Generate version header at build time
#
# This script is run at build time (not configure time) to generate
# a version header that includes git commit information.
#
# Input variables (passed via -D):
#   SOURCE_DIR  - Path to source directory containing src/version.h
#   OUTPUT_FILE - Path to output header file (in build directory)
#   APP_NAME    - "TELNET_GUI" or "TELNET_LISP"
#   VERSION_HEADER - Path to the version.h file to read base version from
#
# Version formats:
#   At tagged release:     0.4
#   Commits after tag:     0.4.5-abc1234
#   No git available:      0.4-UNKNOWN

# Read base version from version.h
if(NOT EXISTS "${VERSION_HEADER}")
    message(FATAL_ERROR "Version header not found: ${VERSION_HEADER}")
endif()

file(READ "${VERSION_HEADER}" VERSION_CONTENT)

# Extract major and minor version
# Pattern matches "APP_NAME_VERSION_MAJOR 0" and captures the number
string(REGEX MATCH "${APP_NAME}_VERSION_MAJOR[ \t]+([0-9]+)" _ "${VERSION_CONTENT}")
set(VERSION_MAJOR "${CMAKE_MATCH_1}")

string(REGEX MATCH "${APP_NAME}_VERSION_MINOR[ \t]+([0-9]+)" _ "${VERSION_CONTENT}")
set(VERSION_MINOR "${CMAKE_MATCH_1}")

# Check if version was extracted (use string comparison since 0 is a valid version)
if("${VERSION_MAJOR}" STREQUAL "" OR "${VERSION_MINOR}" STREQUAL "")
    message(FATAL_ERROR "Could not extract version from ${VERSION_HEADER}")
endif()

set(BASE_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}")

# Check for git
find_program(GIT_EXECUTABLE git)

if(GIT_EXECUTABLE AND EXISTS "${SOURCE_DIR}/.git")
    # We're in a git repository
    set(TAG_NAME "v${BASE_VERSION}")

    # Check if tag exists
    execute_process(
        COMMAND ${GIT_EXECUTABLE} tag -l "${TAG_NAME}"
        WORKING_DIRECTORY "${SOURCE_DIR}"
        OUTPUT_VARIABLE TAG_EXISTS
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )

    if(TAG_EXISTS)
        # Tag exists, count commits since tag
        execute_process(
            COMMAND ${GIT_EXECUTABLE} rev-list "${TAG_NAME}..HEAD" --count
            WORKING_DIRECTORY "${SOURCE_DIR}"
            OUTPUT_VARIABLE COMMITS_SINCE_TAG
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
            RESULT_VARIABLE GIT_RESULT
        )

        if(GIT_RESULT EQUAL 0 AND COMMITS_SINCE_TAG GREATER 0)
            # Get short commit hash
            execute_process(
                COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
                WORKING_DIRECTORY "${SOURCE_DIR}"
                OUTPUT_VARIABLE COMMIT_HASH
                OUTPUT_STRIP_TRAILING_WHITESPACE
                ERROR_QUIET
            )
            set(FULL_VERSION "${BASE_VERSION}.${COMMITS_SINCE_TAG}-${COMMIT_HASH}")
        else()
            # At the tag exactly
            set(FULL_VERSION "${BASE_VERSION}")
        endif()
    else()
        # Tag doesn't exist yet, get commit info anyway
        execute_process(
            COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
            WORKING_DIRECTORY "${SOURCE_DIR}"
            OUTPUT_VARIABLE COMMIT_HASH
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
            RESULT_VARIABLE GIT_RESULT
        )

        if(GIT_RESULT EQUAL 0)
            # Count all commits (no tag to compare against)
            execute_process(
                COMMAND ${GIT_EXECUTABLE} rev-list HEAD --count
                WORKING_DIRECTORY "${SOURCE_DIR}"
                OUTPUT_VARIABLE TOTAL_COMMITS
                OUTPUT_STRIP_TRAILING_WHITESPACE
                ERROR_QUIET
            )
            set(FULL_VERSION "${BASE_VERSION}.${TOTAL_COMMITS}-${COMMIT_HASH}")
        else()
            set(FULL_VERSION "${BASE_VERSION}-UNKNOWN")
        endif()
    endif()
else()
    # No git available
    set(FULL_VERSION "${BASE_VERSION}-UNKNOWN")
endif()

# Generate the header file
file(WRITE "${OUTPUT_FILE}"
"/* Auto-generated at build time - do not edit */
#ifndef ${APP_NAME}_GENERATED_VERSION_H
#define ${APP_NAME}_GENERATED_VERSION_H

#define ${APP_NAME}_VERSION \"${FULL_VERSION}\"
#define ${APP_NAME}_VERSION_MAJOR ${VERSION_MAJOR}
#define ${APP_NAME}_VERSION_MINOR ${VERSION_MINOR}

#endif /* ${APP_NAME}_GENERATED_VERSION_H */
")

message(STATUS "${APP_NAME} version: ${FULL_VERSION}")
