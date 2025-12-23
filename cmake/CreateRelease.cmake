# cmake/CreateRelease.cmake
# Script to create annotated git release tags
#
# Called with:
#   -DSOURCE_DIR=<path>
#   -DRELEASE_VERSION=<major.minor>

if(NOT DEFINED RELEASE_VERSION OR RELEASE_VERSION STREQUAL "")
    message(FATAL_ERROR
        "Release version not specified!\n"
        "Usage: cmake -DRELEASE_VERSION=X.Y --build build --target release\n"
        "Example: cmake -DRELEASE_VERSION=0.2 --build build --target release"
    )
endif()

# Validate version format (MAJOR.MINOR)
string(REGEX MATCH "^([0-9]+)\\.([0-9]+)$" VERSION_MATCH "${RELEASE_VERSION}")
if(NOT VERSION_MATCH)
    message(FATAL_ERROR
        "Invalid version format: ${RELEASE_VERSION}\n"
        "Expected format: MAJOR.MINOR (e.g., 0.2)"
    )
endif()

set(TAG_NAME "v${RELEASE_VERSION}")

# Find git
find_package(Git QUIET REQUIRED)

# Check working directory is clean
execute_process(
    COMMAND ${GIT_EXECUTABLE} status --porcelain
    WORKING_DIRECTORY ${SOURCE_DIR}
    OUTPUT_VARIABLE GIT_STATUS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

if(GIT_STATUS)
    message(FATAL_ERROR
        "Working directory is not clean!\n"
        "Please commit or stash your changes before creating a release.\n"
        "Uncommitted changes:\n${GIT_STATUS}"
    )
endif()

# Check if tag already exists
execute_process(
    COMMAND ${GIT_EXECUTABLE} tag -l ${TAG_NAME}
    WORKING_DIRECTORY ${SOURCE_DIR}
    OUTPUT_VARIABLE EXISTING_TAG
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

if(EXISTING_TAG)
    message(FATAL_ERROR "Tag ${TAG_NAME} already exists!")
endif()

# Create annotated tag
message(STATUS "Creating annotated tag: ${TAG_NAME}")
execute_process(
    COMMAND ${GIT_EXECUTABLE} tag -a ${TAG_NAME} -m "Release ${RELEASE_VERSION}"
    WORKING_DIRECTORY ${SOURCE_DIR}
    RESULT_VARIABLE TAG_RESULT
)

if(NOT TAG_RESULT EQUAL 0)
    message(FATAL_ERROR "Failed to create tag ${TAG_NAME}")
endif()

message(STATUS "")
message(STATUS "========================================")
message(STATUS "Release tag ${TAG_NAME} created!")
message(STATUS "========================================")
message(STATUS "")
message(STATUS "Next steps:")
message(STATUS "  1. Push the tag: git push origin ${TAG_NAME}")
message(STATUS "  2. Reconfigure CMake to pick up new version:")
message(STATUS "     cmake -B build")
message(STATUS "  3. Build: cmake --build build")
message(STATUS "")
