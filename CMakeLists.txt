cmake_minimum_required(VERSION 3.10)
project(TelnetLisp VERSION 1.0.0 LANGUAGES C)

# Set C standard
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Compiler flags
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -pedantic")

# Debug/Release flags
set(CMAKE_C_FLAGS_DEBUG "-g -O0")
set(CMAKE_C_FLAGS_RELEASE "-O2 -DNDEBUG")

# Find required packages
find_package(PkgConfig REQUIRED)
pkg_check_modules(GC REQUIRED libgc)
pkg_check_modules(PCRE2 REQUIRED libpcre2-8)

# Include directories
include_directories(include)

# Library sources
set(LIB_SOURCES
    src/lisp.c
    src/reader.c
    src/eval.c
    src/env.c
    src/builtins.c
    src/print.c
    src/regex.c
)

# Create static library
add_library(liblisp STATIC ${LIB_SOURCES})

# Link libraries
target_link_libraries(liblisp ${GC_LIBRARIES} ${PCRE2_LIBRARIES} m)

# Include directories for library
target_include_directories(liblisp PUBLIC include)

# REPL executable
add_executable(lisp-repl repl/main.c)
target_link_libraries(lisp-repl liblisp ${GC_LIBRARIES} ${PCRE2_LIBRARIES} m)

# Installation
install(TARGETS lisp-repl liblisp
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(FILES include/lisp.h
    DESTINATION include
)

# Testing
enable_testing()

# Simple test
add_test(NAME basic_test
    COMMAND lisp-repl
    INPUT_FILE ${CMAKE_SOURCE_DIR}/examples/arithmetic.lisp
)

# Package configuration
set(CPACK_PACKAGE_NAME "TelnetLisp")
set(CPACK_PACKAGE_VERSION "1.0.0")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Telnet LISP Interpreter")
set(CPACK_PACKAGE_VENDOR "Telnet LISP Project")

include(CPack)
