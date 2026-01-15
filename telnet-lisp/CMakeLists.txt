# Telnet Lisp Library CMakeLists.txt
# Self-contained library build configuration

cmake_minimum_required(VERSION 3.10)

# Check if this is being included as a subdirectory or standalone
if(NOT DEFINED PROJECT_NAME)
    # Standalone build - extract version from git
    list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake")
    include(GitVersion)
    get_git_version()

    project(TelnetLisp VERSION ${GIT_VERSION_FULL} LANGUAGES C)

    # Set version variables for standalone build
    set(TELNET_LISP_VERSION "${GIT_VERSION_FULL}")
    set(TELNET_LISP_VERSION_MAJOR ${GIT_VERSION_MAJOR})
    set(TELNET_LISP_VERSION_MINOR ${GIT_VERSION_MINOR})
    set(TELNET_LISP_VERSION_PATCH ${GIT_VERSION_PATCH})

    # Set C standard
    set(CMAKE_C_STANDARD 99)
    set(CMAKE_C_STANDARD_REQUIRED ON)

    # Compiler flags
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -pedantic")
    set(CMAKE_C_FLAGS_DEBUG "-g -O0")
    set(CMAKE_C_FLAGS_RELEASE "-O2 -DNDEBUG")

    # Export compile commands for clangd
    set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
endif()

# Library sources
set(LIB_SOURCES
    src/lisp.c
    src/reader.c
    src/eval.c
    src/env.c
    src/builtins.c
    src/print.c
    src/regex.c
    src/hash_table.c
    src/utf8.c
    src/file_utils.c
)

# Create static library
add_library(lisp STATIC ${LIB_SOURCES})

# Find required packages
find_package(PkgConfig REQUIRED)
pkg_check_modules(GC REQUIRED bdw-gc)
pkg_check_modules(PCRE2 REQUIRED libpcre2-8)

# Include directories for library
target_include_directories(lisp PUBLIC include)

# Add include directories from pkg-config
target_include_directories(lisp PRIVATE ${GC_INCLUDE_DIRS} ${PCRE2_INCLUDE_DIRS})

# Add library directories from pkg-config so linker can find libraries
target_link_directories(lisp PRIVATE ${GC_LIBRARY_DIRS} ${PCRE2_LIBRARY_DIRS})

# Link libraries
target_link_libraries(lisp ${GC_LIBRARIES} ${PCRE2_LIBRARIES} m)

# Add UCRT64 system include paths for clangd on Windows
if(WIN32 AND MINGW)
    # For MSYS2/MINGW environments, add system include paths
    # These are typically set by the environment automatically
    # Additional system paths can be specified via CMAKE_SYSROOT
endif()

# Installation rules for standalone builds
# When used as subdirectory, parent handles installation
install(TARGETS lisp
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(FILES include/lisp.h include/utf8.h include/file_utils.h
    DESTINATION include
)

# REPL executable (includes lineedit for interactive line editing)
add_executable(lisp-repl repl/main.c repl/lineedit.c)
target_include_directories(lisp-repl PRIVATE include src)
target_include_directories(lisp-repl PRIVATE ${GC_INCLUDE_DIRS} ${PCRE2_INCLUDE_DIRS})
target_link_directories(lisp-repl PRIVATE ${GC_LIBRARY_DIRS} ${PCRE2_LIBRARY_DIRS})
target_link_libraries(lisp-repl lisp ${GC_LIBRARIES} ${PCRE2_LIBRARIES} m)

# ============================================================================
# Build-time Version Generation
# ============================================================================
# Generate version header at build time (not configure time) so new commits
# are reflected without needing to reconfigure. The generated header includes
# commit count and hash when building from a commit after a release tag.
set(LISP_VERSION_HEADER_DIR "${CMAKE_CURRENT_BINARY_DIR}/generated")
set(LISP_VERSION_HEADER "${LISP_VERSION_HEADER_DIR}/telnet_lisp_version.h")

add_custom_command(
    OUTPUT ${LISP_VERSION_HEADER}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${LISP_VERSION_HEADER_DIR}
    COMMAND ${CMAKE_COMMAND}
        -DSOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/..
        -DOUTPUT_FILE=${LISP_VERSION_HEADER}
        -DAPP_NAME=TELNET_LISP
        -DVERSION_HEADER=${CMAKE_CURRENT_SOURCE_DIR}/src/version.h
        -P ${CMAKE_CURRENT_SOURCE_DIR}/../cmake/GenerateVersion.cmake
    DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/src/version.h
        ${CMAKE_CURRENT_SOURCE_DIR}/../cmake/GenerateVersion.cmake
    COMMENT "Generating telnet-lisp version header"
    VERBATIM
)

add_custom_target(generate_lisp_version DEPENDS ${LISP_VERSION_HEADER})

# Make library and REPL depend on version generation
add_dependencies(lisp generate_lisp_version)
add_dependencies(lisp-repl generate_lisp_version)

# Add generated header directory to include paths
target_include_directories(lisp PRIVATE ${LISP_VERSION_HEADER_DIR})
target_include_directories(lisp-repl PRIVATE ${LISP_VERSION_HEADER_DIR})

# Install REPL
install(TARGETS lisp-repl
    RUNTIME DESTINATION bin
)
