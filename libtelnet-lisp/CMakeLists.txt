# Telnet Lisp Library CMakeLists.txt
# Self-contained library build configuration

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
)

# Create static library
add_library(liblisp STATIC ${LIB_SOURCES})

# Find required packages
find_package(PkgConfig REQUIRED)
pkg_check_modules(GC REQUIRED bdw-gc)
pkg_check_modules(PCRE2 REQUIRED libpcre2-8)

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
