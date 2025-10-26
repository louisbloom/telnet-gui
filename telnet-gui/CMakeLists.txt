# Telnet GUI CMakeLists.txt

# Application sources
set(GUI_SOURCES
    src/main.c
    src/telnet.c
    src/terminal.c
    src/window.c
    src/renderer.c
    src/glyph_cache.c
    src/input.c
)

# Create telnet-gui executable
add_executable(telnet-gui ${GUI_SOURCES})

# Find required packages
find_package(PkgConfig REQUIRED)
pkg_check_modules(SDL2 REQUIRED sdl2)
pkg_check_modules(SDL2_TTF REQUIRED SDL2_ttf)
pkg_check_modules(VTERM REQUIRED vterm)
pkg_check_modules(GC REQUIRED bdw-gc)
pkg_check_modules(PCRE2 REQUIRED libpcre2-8)

# Link directories first
target_link_directories(telnet-gui PRIVATE ${SDL2_LIBDIR} ${SDL2_TTF_LIBDIR} ${VTERM_LIBDIR})

# Link libraries - only link SDL2_ttf since it includes SDL2
# VTERM already includes pthread
# Need ws2_32 for Windows sockets
target_link_libraries(telnet-gui PRIVATE
    ${SDL2_TTF_LIBRARIES}
    ${VTERM_LIBRARIES}
    ws2_32
)

# Include directories
target_include_directories(telnet-gui PRIVATE
    src
    ../libtelnet-lisp/include
)

# Add UCRT64 system include paths on Windows
# Use UCRT64_ROOT from parent CMakeLists.txt or detect it here
if(WIN32)
    if(NOT DEFINED UCRT64_ROOT)
        # Detect MSYS2/UCRT64 installation in common locations
        if(DEFINED ENV{MSYS2_ROOT})
            set(UCRT64_ROOT "$ENV{MSYS2_ROOT}/ucrt64")
        else()
            # Try common installation paths
            set(PROGRAM_FILES_X86 "$ENV{ProgramFiles\(x86\)}")
            file(GLOB MSYS2_INSTALLS
                "C:/msys64"
                "C:/tools/msys64"
                "$ENV{USERPROFILE}/scoop/apps/msys2/current"
                "$ENV{ProgramFiles}/msys64"
                "${PROGRAM_FILES_X86}/msys64"
            )

            # Find the first existing MSYS2 installation
            foreach(path IN LISTS MSYS2_INSTALLS)
                if(EXISTS "${path}")
                    set(UCRT64_ROOT "${path}/ucrt64")
                    break()
                endif()
            endforeach()
        endif()
    endif()

    if(EXISTS "${UCRT64_ROOT}/include")
        target_include_directories(telnet-gui SYSTEM PRIVATE "${UCRT64_ROOT}/include")

        # Add GCC include paths if available
        file(GLOB GCC_VERSIONS "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/*")
        if(GCC_VERSIONS)
            list(SORT GCC_VERSIONS)
            list(REVERSE GCC_VERSIONS)
            get_filename_component(GCC_VERSION "${GCC_VERSIONS}" NAME)
            target_include_directories(telnet-gui SYSTEM PRIVATE
                "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/${GCC_VERSION}/include")
            if(EXISTS "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/${GCC_VERSION}/include-fixed")
                target_include_directories(telnet-gui SYSTEM PRIVATE
                    "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/${GCC_VERSION}/include-fixed")
            endif()
        endif()
    endif()
endif()
