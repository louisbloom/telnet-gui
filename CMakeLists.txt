cmake_minimum_required(VERSION 3.10)
project(TelnetLisp VERSION 1.0.0 LANGUAGES C)

# Export compile commands for clangd
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Set C standard
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Compiler flags
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -pedantic")

# Debug/Release flags
set(CMAKE_C_FLAGS_DEBUG "-g -O0")
set(CMAKE_C_FLAGS_RELEASE "-O2 -DNDEBUG")

# For Windows UCRT64, prefer static linking to avoid msys-2.0.dll
if(WIN32)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libgcc")
    # Add UCRT64 system include paths for clangd
    set(UCRT64_ROOT "C:/Users/tchristensen/scoop/apps/msys2/current/ucrt64")
    include_directories(SYSTEM "${UCRT64_ROOT}/include")
    include_directories(SYSTEM "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/15.2.0/include")
    include_directories(SYSTEM "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/15.2.0/include-fixed")
endif()

# Find required packages
find_package(PkgConfig REQUIRED)
pkg_check_modules(GC REQUIRED bdw-gc)
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
    src/hash_table.c
)

# Create static library
add_library(liblisp STATIC ${LIB_SOURCES})

# Link libraries
target_link_libraries(liblisp ${GC_LIBRARIES} ${PCRE2_LIBRARIES} m)

# Include directories for library
target_include_directories(liblisp PUBLIC include)

# Add UCRT64 system include paths for clangd on Windows
if(WIN32)
    set(UCRT64_ROOT "C:/Users/tchristensen/scoop/apps/msys2/current/ucrt64")
    target_include_directories(liblisp SYSTEM PUBLIC
        "${UCRT64_ROOT}/include"
        "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/15.2.0/include"
        "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/15.2.0/include-fixed"
    )
endif()

# REPL executable
add_executable(lisp-repl repl/main.c)
target_link_libraries(lisp-repl liblisp ${GC_LIBRARIES} ${PCRE2_LIBRARIES} m)

# Add UCRT64 system include paths for REPL executable on Windows
if(WIN32)
    target_include_directories(lisp-repl SYSTEM PRIVATE
        "${UCRT64_ROOT}/include"
        "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/15.2.0/include"
        "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/15.2.0/include-fixed"
    )
endif()

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

# REPL test suite
add_test(NAME repl_basic_test
    COMMAND lisp-repl
    INPUT_FILE ${CMAKE_SOURCE_DIR}/examples/arithmetic.lisp
)

add_test(NAME repl_regression_test
    COMMAND lisp-repl
    INPUT_FILE ${CMAKE_SOURCE_DIR}/tests/test_regression.lisp
)

add_test(NAME repl_multiline_test
    COMMAND lisp-repl
    INPUT_FILE ${CMAKE_SOURCE_DIR}/tests/test_multiline.lisp
)

# Package configuration
set(CPACK_PACKAGE_NAME "TelnetLisp")
set(CPACK_PACKAGE_VERSION "1.0.0")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Telnet LISP Interpreter")
set(CPACK_PACKAGE_VENDOR "Telnet LISP Project")

include(CPack)
