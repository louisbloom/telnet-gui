# cmake/GitVersion.cmake
# Git-based version extraction module
#
# This module extracts version from git tags in the format vMAJOR.MINOR
# Patch version is the number of commits since the last tag.
# Full version: MAJOR.MINOR.PATCH (e.g., 0.1.5)
#
# After calling get_git_version(), these variables are set in parent scope:
#   GIT_VERSION_MAJOR - Major version number
#   GIT_VERSION_MINOR - Minor version number
#   GIT_VERSION_PATCH - Patch version (commits since tag)
#   GIT_VERSION_FULL  - Full version string "MAJOR.MINOR.PATCH"
#   GIT_VERSION_TAG   - The git tag (e.g., "v0.1")

function(get_git_version)
    # Find git executable
    find_package(Git QUIET)

    if(NOT GIT_FOUND)
        message(WARNING "Git not found - using fallback version 0.0.0")
        set(GIT_VERSION_MAJOR 0 PARENT_SCOPE)
        set(GIT_VERSION_MINOR 0 PARENT_SCOPE)
        set(GIT_VERSION_PATCH 0 PARENT_SCOPE)
        set(GIT_VERSION_FULL "0.0.0" PARENT_SCOPE)
        set(GIT_VERSION_TAG "v0.0" PARENT_SCOPE)
        return()
    endif()

    # Check if we're in a git repo
    execute_process(
        COMMAND ${GIT_EXECUTABLE} rev-parse --is-inside-work-tree
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE IN_GIT_REPO
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE GIT_RESULT
    )

    if(NOT GIT_RESULT EQUAL 0)
        message(WARNING "Not in a git repository - using fallback version 0.0.0")
        set(GIT_VERSION_MAJOR 0 PARENT_SCOPE)
        set(GIT_VERSION_MINOR 0 PARENT_SCOPE)
        set(GIT_VERSION_PATCH 0 PARENT_SCOPE)
        set(GIT_VERSION_FULL "0.0.0" PARENT_SCOPE)
        set(GIT_VERSION_TAG "v0.0" PARENT_SCOPE)
        return()
    endif()

    # Get most recent tag matching vMAJOR.MINOR pattern
    execute_process(
        COMMAND ${GIT_EXECUTABLE} describe --tags --match "v[0-9]*.[0-9]*" --abbrev=0
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE GIT_TAG
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE GIT_TAG_RESULT
    )

    if(NOT GIT_TAG_RESULT EQUAL 0 OR NOT GIT_TAG)
        message(WARNING "No version tag found (expected format: vMAJOR.MINOR) - using fallback 0.0.0")
        set(GIT_VERSION_MAJOR 0 PARENT_SCOPE)
        set(GIT_VERSION_MINOR 0 PARENT_SCOPE)
        set(GIT_VERSION_PATCH 0 PARENT_SCOPE)
        set(GIT_VERSION_FULL "0.0.0" PARENT_SCOPE)
        set(GIT_VERSION_TAG "v0.0" PARENT_SCOPE)
        return()
    endif()

    # Parse MAJOR.MINOR from tag (format: vMAJOR.MINOR)
    string(REGEX MATCH "^v([0-9]+)\\.([0-9]+)$" TAG_MATCH "${GIT_TAG}")
    if(NOT TAG_MATCH)
        message(WARNING "Invalid tag format: ${GIT_TAG} (expected vMAJOR.MINOR) - using fallback 0.0.0")
        set(GIT_VERSION_MAJOR 0 PARENT_SCOPE)
        set(GIT_VERSION_MINOR 0 PARENT_SCOPE)
        set(GIT_VERSION_PATCH 0 PARENT_SCOPE)
        set(GIT_VERSION_FULL "0.0.0" PARENT_SCOPE)
        set(GIT_VERSION_TAG "v0.0" PARENT_SCOPE)
        return()
    endif()

    set(VERSION_MAJOR ${CMAKE_MATCH_1})
    set(VERSION_MINOR ${CMAKE_MATCH_2})

    # Count commits since tag for patch version
    execute_process(
        COMMAND ${GIT_EXECUTABLE} rev-list ${GIT_TAG}..HEAD --count
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE COMMITS_SINCE_TAG
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
        RESULT_VARIABLE COMMIT_COUNT_RESULT
    )

    if(NOT COMMIT_COUNT_RESULT EQUAL 0 OR NOT COMMITS_SINCE_TAG)
        set(COMMITS_SINCE_TAG 0)
    endif()

    set(VERSION_PATCH ${COMMITS_SINCE_TAG})
    set(VERSION_FULL "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}")

    # Export to parent scope
    set(GIT_VERSION_MAJOR ${VERSION_MAJOR} PARENT_SCOPE)
    set(GIT_VERSION_MINOR ${VERSION_MINOR} PARENT_SCOPE)
    set(GIT_VERSION_PATCH ${VERSION_PATCH} PARENT_SCOPE)
    set(GIT_VERSION_FULL ${VERSION_FULL} PARENT_SCOPE)
    set(GIT_VERSION_TAG ${GIT_TAG} PARENT_SCOPE)

    message(STATUS "Git version: ${VERSION_FULL} (tag: ${GIT_TAG}, commits since: ${COMMITS_SINCE_TAG})")
endfunction()
