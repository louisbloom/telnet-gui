# cmake/Release.cmake
# Release target for creating git tags
#
# Usage: cmake -DRELEASE_VERSION=0.2 --build build --target release

add_custom_target(release
    COMMAND ${CMAKE_COMMAND}
        -DSOURCE_DIR=${CMAKE_SOURCE_DIR}
        -DRELEASE_VERSION=${RELEASE_VERSION}
        -P ${CMAKE_SOURCE_DIR}/cmake/CreateRelease.cmake
    COMMENT "Creating release tag..."
    USES_TERMINAL
)
