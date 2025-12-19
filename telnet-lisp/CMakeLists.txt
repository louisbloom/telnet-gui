# Telnet Lisp Library CMakeLists.txt
# Self-contained library build configuration

cmake_minimum_required(VERSION 3.10)

# Check if this is being included as a subdirectory or standalone
if(NOT DEFINED PROJECT_NAME)
    project(TelnetLisp VERSION 1.0.0 LANGUAGES C)

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

# Link libraries
target_link_libraries(lisp ${GC_LIBRARIES} ${PCRE2_LIBRARIES} m)

# Include directories for library
target_include_directories(lisp PUBLIC include)

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

# REPL executable
add_executable(lisp-repl repl/main.c)
target_link_libraries(lisp-repl lisp ${GC_LIBRARIES} ${PCRE2_LIBRARIES} m)
target_include_directories(lisp-repl PRIVATE include)

# Install REPL
install(TARGETS lisp-repl
    RUNTIME DESTINATION bin
)
