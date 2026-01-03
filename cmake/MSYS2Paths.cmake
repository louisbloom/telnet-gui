# cmake/MSYS2Paths.cmake
# MSYS2/UCRT64 path detection module for Windows builds
#
# This module provides functions to detect MSYS2 installations and search
# for files in standard MSYS2 locations. It supports various installation
# methods including Scoop, standard installer, and GitHub Actions.
#
# Functions:
#   detect_msys2_root()                    - Detect MSYS2 installation, set MSYS2_ROOT/UCRT64_ROOT
#   get_msys2_search_paths(OUTPUT SUFFIX)  - Get prioritized list of search paths
#   find_in_msys2_paths(OUTPUT FILE ...)   - Find a file in MSYS2 paths
#   normalize_path(OUTPUT PATH)            - Normalize path separators

# Helper function to normalize paths (convert backslashes to forward slashes)
function(normalize_path OUTPUT_VAR INPUT_PATH)
    string(REPLACE "\\" "/" NORMALIZED "${INPUT_PATH}")
    set(${OUTPUT_VAR} "${NORMALIZED}" PARENT_SCOPE)
endfunction()

# Detect MSYS2 installation and set MSYS2_ROOT and UCRT64_ROOT variables
# After calling, these variables are set in parent scope:
#   MSYS2_ROOT  - Path to MSYS2 root (e.g., C:/msys64)
#   UCRT64_ROOT - Path to UCRT64 environment (e.g., C:/msys64/ucrt64)
function(detect_msys2_root)
    # Skip if already detected
    if(DEFINED MSYS2_ROOT AND DEFINED UCRT64_ROOT)
        return()
    endif()

    set(_MSYS2_ROOT "")
    set(_UCRT64_ROOT "")

    # 1. Check MSYS2_ROOT environment variable first
    if(DEFINED ENV{MSYS2_ROOT})
        if(EXISTS "$ENV{MSYS2_ROOT}")
            set(_MSYS2_ROOT "$ENV{MSYS2_ROOT}")
        endif()
    endif()

    # 2. Search standard locations if not found
    if(NOT _MSYS2_ROOT)
        set(PROGRAM_FILES_X86 "$ENV{ProgramFiles\(x86\)}")

        # Build search list: Scoop first (user's intentional override), then standard paths
        set(MSYS2_SEARCH_PATHS "")

        # Scoop: Add both current symlink and versioned directories
        if(EXISTS "$ENV{USERPROFILE}/scoop/apps/msys2")
            list(APPEND MSYS2_SEARCH_PATHS "$ENV{USERPROFILE}/scoop/apps/msys2/current")
            file(GLOB SCOOP_VERSIONS "$ENV{USERPROFILE}/scoop/apps/msys2/*")
            list(APPEND MSYS2_SEARCH_PATHS ${SCOOP_VERSIONS})
        endif()

        # Standard installation paths
        list(APPEND MSYS2_SEARCH_PATHS
            "C:/msys64"
            "C:/tools/msys64"
            "$ENV{ProgramFiles}/msys64"
            "${PROGRAM_FILES_X86}/msys64"
            "D:/a/_temp/msys64"  # GitHub Actions
        )

        # Find first existing installation
        foreach(PATH IN LISTS MSYS2_SEARCH_PATHS)
            if(EXISTS "${PATH}")
                set(_MSYS2_ROOT "${PATH}")
                break()
            endif()
        endforeach()
    endif()

    # Set UCRT64_ROOT based on MSYS2_ROOT
    if(_MSYS2_ROOT)
        set(_UCRT64_ROOT "${_MSYS2_ROOT}/ucrt64")
    endif()

    # Export to parent scope
    set(MSYS2_ROOT "${_MSYS2_ROOT}" PARENT_SCOPE)
    set(UCRT64_ROOT "${_UCRT64_ROOT}" PARENT_SCOPE)

    if(_MSYS2_ROOT)
        message(STATUS "MSYS2 detected at: ${_MSYS2_ROOT}")
    endif()
endfunction()

# Get a prioritized list of paths to search for MSYS2 resources
# Arguments:
#   OUTPUT_VAR - Variable name to store the result list
#   SUFFIX     - Optional suffix to append to each path (e.g., "lib/pkgconfig")
#   PRIORITY   - Optional: "usrlocal" (default), "ucrt64", or "both"
#                usrlocal: Search /usr/local first (source builds)
#                ucrt64: Search /ucrt64 first (distribution packages)
#                both: Include both with /usr/local priority
function(get_msys2_search_paths OUTPUT_VAR)
    cmake_parse_arguments(ARG "" "SUFFIX;PRIORITY" "" ${ARGN})

    if(NOT ARG_PRIORITY)
        set(ARG_PRIORITY "both")
    endif()

    # Ensure MSYS2_ROOT is detected
    if(NOT DEFINED MSYS2_ROOT)
        detect_msys2_root()
    endif()

    set(RESULT_PATHS "")
    set(PROGRAM_FILES_X86 "$ENV{ProgramFiles\(x86\)}")

    # Build base MSYS2 root paths
    set(MSYS2_ROOTS "")

    # Add detected MSYS2_ROOT first if available
    if(MSYS2_ROOT)
        list(APPEND MSYS2_ROOTS "${MSYS2_ROOT}")
    endif()

    # Add Scoop paths
    if(EXISTS "$ENV{USERPROFILE}/scoop/apps/msys2")
        list(APPEND MSYS2_ROOTS "$ENV{USERPROFILE}/scoop/apps/msys2/current")
        file(GLOB SCOOP_VERSIONS "$ENV{USERPROFILE}/scoop/apps/msys2/*")
        list(APPEND MSYS2_ROOTS ${SCOOP_VERSIONS})
    endif()

    # Add standard paths
    list(APPEND MSYS2_ROOTS
        "C:/msys64"
        "C:/tools/msys64"
        "$ENV{ProgramFiles}/msys64"
        "${PROGRAM_FILES_X86}/msys64"
        "D:/a/_temp/msys64"
    )

    # Remove duplicates while preserving order
    list(REMOVE_DUPLICATES MSYS2_ROOTS)

    # Build final paths based on priority
    if(ARG_PRIORITY STREQUAL "usrlocal" OR ARG_PRIORITY STREQUAL "both")
        # /usr/local paths (source builds)
        foreach(ROOT IN LISTS MSYS2_ROOTS)
            if(ARG_SUFFIX)
                list(APPEND RESULT_PATHS "${ROOT}/usr/local/${ARG_SUFFIX}")
            else()
                list(APPEND RESULT_PATHS "${ROOT}/usr/local")
            endif()
        endforeach()
    endif()

    if(ARG_PRIORITY STREQUAL "ucrt64" OR ARG_PRIORITY STREQUAL "both")
        # UCRT64 paths (distribution packages)
        foreach(ROOT IN LISTS MSYS2_ROOTS)
            if(ARG_SUFFIX)
                list(APPEND RESULT_PATHS "${ROOT}/ucrt64/${ARG_SUFFIX}")
            else()
                list(APPEND RESULT_PATHS "${ROOT}/ucrt64")
            endif()
        endforeach()
    endif()

    set(${OUTPUT_VAR} "${RESULT_PATHS}" PARENT_SCOPE)
endfunction()

# Find a file in MSYS2 search paths
# Arguments:
#   RESULT_VAR - Variable name to store the directory containing the file (empty if not found)
#   FILENAME   - The file to search for
#   SUFFIX     - Optional suffix for search paths (e.g., "lib" or "lib/pkgconfig")
#   PRIORITY   - Optional: "usrlocal" (default), "ucrt64", or "both"
function(find_in_msys2_paths RESULT_VAR FILENAME)
    cmake_parse_arguments(ARG "" "SUFFIX;PRIORITY" "" ${ARGN})

    set(SEARCH_ARGS "")
    if(ARG_SUFFIX)
        list(APPEND SEARCH_ARGS SUFFIX "${ARG_SUFFIX}")
    endif()
    if(ARG_PRIORITY)
        list(APPEND SEARCH_ARGS PRIORITY "${ARG_PRIORITY}")
    endif()

    get_msys2_search_paths(SEARCH_PATHS ${SEARCH_ARGS})

    foreach(PATH IN LISTS SEARCH_PATHS)
        if(EXISTS "${PATH}/${FILENAME}")
            # Normalize path before returning (convert backslashes to forward slashes)
            string(REPLACE "\\" "/" NORMALIZED_PATH "${PATH}")
            set(${RESULT_VAR} "${NORMALIZED_PATH}" PARENT_SCOPE)
            return()
        endif()
    endforeach()

    # Not found
    set(${RESULT_VAR} "" PARENT_SCOPE)
endfunction()
