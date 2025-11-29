# Telnet GUI CMakeLists.txt

# Application sources
set(GUI_SOURCES
    src/main.c
    src/commands.c
    src/telnet.c
    src/terminal.c
    src/window.c
    src/renderer.c
    src/glyph_cache.c
    src/input.c
    src/input_area.c
    src/lisp_bridge.c
)

# Create telnet-gui executable
add_executable(telnet-gui ${GUI_SOURCES})

# Copy font files to build directory
set(FONT_PLEX_SRC "${CMAKE_CURRENT_SOURCE_DIR}/assets/fonts/IBMPlexMono-Regular.ttf")
set(FONT_PLEX_DST "${CMAKE_CURRENT_BINARY_DIR}/assets/fonts/IBMPlexMono-Regular.ttf")
set(FONT_MONACO_SRC "${CMAKE_CURRENT_SOURCE_DIR}/assets/fonts/Monaco-Regular.ttf")
set(FONT_MONACO_DST "${CMAKE_CURRENT_BINARY_DIR}/assets/fonts/Monaco-Regular.ttf")

# Copy bootstrap Lisp file to build directory
set(BOOTSTRAP_SRC "${CMAKE_CURRENT_SOURCE_DIR}/bootstrap.lisp")
set(BOOTSTRAP_DST "${CMAKE_CURRENT_BINARY_DIR}/bootstrap.lisp")

# Copy test-related Lisp files to build directory
set(TINTIN_SRC "${CMAKE_CURRENT_SOURCE_DIR}/tintin.lisp")
set(TINTIN_DST "${CMAKE_CURRENT_BINARY_DIR}/tintin.lisp")
set(TINTIN_TEST_SRC "${CMAKE_CURRENT_SOURCE_DIR}/tintin-test.lisp")
set(TINTIN_TEST_DST "${CMAKE_CURRENT_BINARY_DIR}/tintin-test.lisp")
set(COMPLETION_TEST_SRC "${CMAKE_CURRENT_SOURCE_DIR}/completion-test.lisp")
set(COMPLETION_TEST_DST "${CMAKE_CURRENT_BINARY_DIR}/completion-test.lisp")

# Create assets/fonts directory in build directory
add_custom_command(TARGET telnet-gui POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_BINARY_DIR}/assets/fonts"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${FONT_PLEX_SRC}" "${FONT_PLEX_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${FONT_MONACO_SRC}" "${FONT_MONACO_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${BOOTSTRAP_SRC}" "${BOOTSTRAP_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${TINTIN_SRC}" "${TINTIN_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${TINTIN_TEST_SRC}" "${TINTIN_TEST_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${COMPLETION_TEST_SRC}" "${COMPLETION_TEST_DST}"
    COMMENT "Copying fonts, bootstrap.lisp, and test files to build directory"
)

# Install font files with executable (in bin directory so it's relative to the executable)
install(FILES "${FONT_PLEX_SRC}"
    DESTINATION "bin/assets/fonts"
    COMPONENT Runtime
)
install(FILES "${FONT_MONACO_SRC}"
    DESTINATION "bin/assets/fonts"
    COMPONENT Runtime
)

# Install bootstrap Lisp file with executable (in bin directory so it's relative to the executable)
# Always installs from source directory, so changes are picked up on next install
install(FILES "${BOOTSTRAP_SRC}"
    DESTINATION "bin"
    COMPONENT Runtime
)

# Find required packages
find_package(PkgConfig REQUIRED)
pkg_check_modules(SDL2 REQUIRED sdl2)
pkg_check_modules(SDL2_TTF REQUIRED SDL2_ttf)
# Try pkg-config with /usr/local/lib/pkgconfig in path
# Find the pkg-config file location first
set(USR_LOCAL_PKG_CONFIG "")
if(WIN32)
    if(DEFINED ENV{MSYS2_ROOT})
        if(EXISTS "$ENV{MSYS2_ROOT}/usr/local/lib/pkgconfig/vterm.pc")
            set(USR_LOCAL_PKG_CONFIG "$ENV{MSYS2_ROOT}/usr/local/lib/pkgconfig")
        endif()
    endif()

    if(NOT USR_LOCAL_PKG_CONFIG)
        set(PKG_SEARCH_PATHS
            "C:/msys64/usr/local/lib/pkgconfig"
            "C:/tools/msys64/usr/local/lib/pkgconfig"
            "$ENV{USERPROFILE}/scoop/apps/msys2/current/usr/local/lib/pkgconfig"
            "$ENV{ProgramFiles}/msys64/usr/local/lib/pkgconfig"
        )
        if(EXISTS "$ENV{USERPROFILE}/scoop/apps/msys2")
            file(GLOB SCOOP_PKG_VERSIONS "$ENV{USERPROFILE}/scoop/apps/msys2/*/usr/local/lib/pkgconfig")
            list(APPEND PKG_SEARCH_PATHS ${SCOOP_PKG_VERSIONS})
        endif()

        foreach(PKG_PATH IN LISTS PKG_SEARCH_PATHS)
            if(EXISTS "${PKG_PATH}/vterm.pc")
                set(USR_LOCAL_PKG_CONFIG "${PKG_PATH}")
                break()
            endif()
        endforeach()
    endif()
endif()

# Try pkg-config - CMake's pkg_check_modules caches results and may ignore ENV changes
# So we manually call pkg-config via execute_process
set(VTERM_FOUND FALSE)
if(USR_LOCAL_PKG_CONFIG OR EXISTS "/usr/local/lib/pkgconfig/vterm.pc")
    if(NOT USR_LOCAL_PKG_CONFIG AND EXISTS "/usr/local/lib/pkgconfig/vterm.pc")
        set(USR_LOCAL_PKG_CONFIG "/usr/local/lib/pkgconfig")
    endif()

    # Use pkg-config executable from find_package
    # PKG_CONFIG_EXECUTABLE is set by find_package(PkgConfig) above
    if(PKG_CONFIG_EXECUTABLE)
        # Check if VS Code or user already set PKG_CONFIG_PATH
        # If so, make sure our path is included
        set(PKG_CONFIG_UNIX_PATH "${USR_LOCAL_PKG_CONFIG}")
        string(REPLACE "\\" "/" PKG_CONFIG_UNIX_PATH "${PKG_CONFIG_UNIX_PATH}")

        # Check if vterm.pc is already in PKG_CONFIG_PATH
        set(PKG_PATH_NEEDS_UPDATE TRUE)
        if(DEFINED ENV{PKG_CONFIG_PATH})
            string(FIND "$ENV{PKG_CONFIG_PATH}" "${PKG_CONFIG_UNIX_PATH}" PKG_PATH_FOUND)
            if(NOT PKG_PATH_FOUND EQUAL -1)
                set(PKG_PATH_NEEDS_UPDATE FALSE)
                # Path already included (e.g., by VS Code settings) - no message needed
            endif()
        endif()

        # Prepare PKG_CONFIG_PATH environment variable
        # Priority: Use ONLY our path first (it works standalone), then try with system paths
        # pkg-config in MSYS2 uses colon (:) separator and forward slashes even on Windows

        # Try with just our path first (simpler, more reliable)
        set(OLD_PKG_CONFIG_PATH_ENV "$ENV{PKG_CONFIG_PATH}")
        set(ENV{PKG_CONFIG_PATH} "${PKG_CONFIG_UNIX_PATH}")

        message(STATUS "Testing pkg-config with PKG_CONFIG_PATH=${PKG_CONFIG_UNIX_PATH}")

        # Test if pkg-config can find it with just our path
        execute_process(
            COMMAND ${PKG_CONFIG_EXECUTABLE} --exists vterm
            RESULT_VARIABLE PKG_RESULT
            OUTPUT_VARIABLE PKG_OUTPUT
            ERROR_VARIABLE PKG_ERROR
        )

        # If that didn't work and we have existing PKG_CONFIG_PATH, try combining
        if(NOT PKG_RESULT EQUAL 0 AND DEFINED ENV{PKG_CONFIG_PATH} AND PKG_PATH_NEEDS_UPDATE)
            # Convert existing PKG_CONFIG_PATH: semicolons to colons, backslashes to forward slashes
            string(REPLACE ";" ":" EXISTING_PKG_PATH "${OLD_PKG_CONFIG_PATH_ENV}")
            string(REPLACE "\\" "/" EXISTING_PKG_PATH "${EXISTING_PKG_PATH}")
            set(NEW_PKG_CONFIG_PATH "${PKG_CONFIG_UNIX_PATH}:${EXISTING_PKG_PATH}")
            set(ENV{PKG_CONFIG_PATH} "${NEW_PKG_CONFIG_PATH}")

            message(STATUS "Retrying with combined PKG_CONFIG_PATH: ${NEW_PKG_CONFIG_PATH}")

            execute_process(
                COMMAND ${PKG_CONFIG_EXECUTABLE} --exists vterm
                RESULT_VARIABLE PKG_RESULT
                OUTPUT_VARIABLE PKG_OUTPUT
                ERROR_VARIABLE PKG_ERROR
            )
        else()
            set(NEW_PKG_CONFIG_PATH "${PKG_CONFIG_UNIX_PATH}")
        endif()

        if(NOT PKG_RESULT EQUAL 0)
            message(STATUS "pkg-config test result: ${PKG_RESULT}, output: '${PKG_OUTPUT}', error: '${PKG_ERROR}'")
        endif()

        if(PKG_RESULT EQUAL 0)
            message(STATUS "Found vterm package via pkg-config!")

            # pkg-config found it, get the values manually
            # Keep PKG_CONFIG_PATH set for these calls
            execute_process(
                COMMAND ${PKG_CONFIG_EXECUTABLE} --modversion vterm
                OUTPUT_VARIABLE VTERM_VERSION
                OUTPUT_STRIP_TRAILING_WHITESPACE
                ERROR_QUIET
            )
            execute_process(
                COMMAND ${PKG_CONFIG_EXECUTABLE} --cflags vterm
                OUTPUT_VARIABLE VTERM_CFLAGS
                OUTPUT_STRIP_TRAILING_WHITESPACE
                ERROR_QUIET
            )
            execute_process(
                COMMAND ${PKG_CONFIG_EXECUTABLE} --libs vterm
                OUTPUT_VARIABLE VTERM_LIBS
                OUTPUT_STRIP_TRAILING_WHITESPACE
                ERROR_QUIET
            )

            # Parse and set variables (simple parsing - assumes -I and -L flags)
            string(REPLACE " " ";" VTERM_CFLAGS_LIST ${VTERM_CFLAGS})
            string(REPLACE " " ";" VTERM_LIBS_LIST ${VTERM_LIBS})

            set(VTERM_FOUND TRUE)
            set(VTERM_VERSION ${VTERM_VERSION})
            set(VTERM_LIBRARIES ${VTERM_LIBS_LIST})
            set(VTERM_CFLAGS_OTHER ${VTERM_CFLAGS_LIST})
            message(STATUS "Found vterm via pkg-config: ${VTERM_VERSION}")
        else()
            message(STATUS "pkg-config lookup failed (result: ${PKG_RESULT})")
        endif()

        # Restore original PKG_CONFIG_PATH after all pkg-config calls
        set(ENV{PKG_CONFIG_PATH} "${OLD_PKG_CONFIG_PATH_ENV}")
    endif()
endif()

# Fallback if pkg-config didn't work
if(NOT VTERM_FOUND)
    set(VTERM_FOUND TRUE)
    set(VTERM_LIBRARIES vterm)
    message(STATUS "Using libvterm from /usr/local (pkg-config not available or package not found)")
endif()
pkg_check_modules(GC REQUIRED bdw-gc)
pkg_check_modules(PCRE2 REQUIRED libpcre2-8)

# Link directories first
target_link_directories(telnet-gui PRIVATE ${SDL2_LIBDIR} ${SDL2_TTF_LIBDIR} ${VTERM_LIBDIR})

# Add /usr/local/lib for libvterm if not found via pkg-config
if(WIN32 AND NOT VTERM_LIBDIR)
    # Find MSYS2 root and add /usr/local/lib
    set(USR_LOCAL_LIB "")
    if(DEFINED ENV{MSYS2_ROOT})
        if(EXISTS "$ENV{MSYS2_ROOT}/usr/local/lib/libvterm.a" OR EXISTS "$ENV{MSYS2_ROOT}/usr/local/lib/libvterm.dll.a")
            set(USR_LOCAL_LIB "$ENV{MSYS2_ROOT}/usr/local/lib")
        endif()
    endif()

    if(NOT USR_LOCAL_LIB)
        set(MSYS2_LIB_PATHS
            "C:/msys64"
            "C:/tools/msys64"
            "$ENV{USERPROFILE}/scoop/apps/msys2/current"
            "$ENV{ProgramFiles}/msys64"
            "${PROGRAM_FILES_X86}/msys64"
        )
        if(EXISTS "$ENV{USERPROFILE}/scoop/apps/msys2")
            file(GLOB SCOOP_LIB_VERSIONS "$ENV{USERPROFILE}/scoop/apps/msys2/*")
            list(APPEND MSYS2_LIB_PATHS ${SCOOP_LIB_VERSIONS})
        endif()

        foreach(ROOT IN LISTS MSYS2_LIB_PATHS)
            if(EXISTS "${ROOT}/usr/local/lib/libvterm.a" OR EXISTS "${ROOT}/usr/local/lib/libvterm.dll.a")
                set(USR_LOCAL_LIB "${ROOT}/usr/local/lib")
                message(STATUS "Found libvterm library at: ${USR_LOCAL_LIB}")
                break()
            endif()
        endforeach()
    endif()

    if(USR_LOCAL_LIB)
        target_link_directories(telnet-gui PRIVATE "${USR_LOCAL_LIB}")
    else()
        target_link_directories(telnet-gui PRIVATE "/usr/local/lib")
    endif()
else()
    target_link_directories(telnet-gui PRIVATE "/usr/local/lib")
endif()

# Link libraries - only link SDL2_ttf since it includes SDL2
# VTERM already includes pthread
# Need ws2_32 for Windows sockets
# Link against telnet-lisp for completion hooks
# Also need GC and PCRE2 since liblisp is static
target_link_libraries(telnet-gui PRIVATE
    ${SDL2_TTF_LIBRARIES}
    ${VTERM_LIBRARIES}
    liblisp
    ${GC_LIBRARIES}
    ${PCRE2_LIBRARIES}
    ws2_32
    m
)

# Include directories
target_include_directories(telnet-gui PRIVATE
    src
    ../telnet-lisp/include
)

# Add /usr/local/include for libvterm
# On MSYS2 Windows, we need to handle path conversion
if(WIN32)
    set(USR_LOCAL_INCLUDE "")
    if(NOT DEFINED PROGRAM_FILES_X86)
        set(PROGRAM_FILES_X86 "$ENV{ProgramFiles\(x86\)}")
    endif()

    # Try environment variable first
    if(DEFINED ENV{MSYS2_ROOT})
        if(EXISTS "$ENV{MSYS2_ROOT}/usr/local/include/vterm.h")
            set(USR_LOCAL_INCLUDE "$ENV{MSYS2_ROOT}/usr/local/include")
        endif()
    endif()

    # Try common MSYS2 locations if not found
    if(NOT USR_LOCAL_INCLUDE)
        set(MSYS2_PATHS
            "C:/msys64"
            "C:/tools/msys64"
            "$ENV{USERPROFILE}/scoop/apps/msys2/current"
            "$ENV{ProgramFiles}/msys64"
            "${PROGRAM_FILES_X86}/msys64"
        )

        # Also check for versioned Scoop installations
        if(EXISTS "$ENV{USERPROFILE}/scoop/apps/msys2")
            file(GLOB SCOOP_VERSIONS "$ENV{USERPROFILE}/scoop/apps/msys2/*")
            list(APPEND MSYS2_PATHS ${SCOOP_VERSIONS})
        endif()

        foreach(ROOT IN LISTS MSYS2_PATHS)
            if(EXISTS "${ROOT}/usr/local/include/vterm.h")
                set(USR_LOCAL_INCLUDE "${ROOT}/usr/local/include")
                message(STATUS "Found libvterm headers at: ${USR_LOCAL_INCLUDE}")
                break()
            endif()
        endforeach()
    endif()

    # Fallback: try /usr/local/include directly (works in MSYS2 shell)
    if(NOT USR_LOCAL_INCLUDE)
        if(EXISTS "/usr/local/include/vterm.h")
            set(USR_LOCAL_INCLUDE "/usr/local/include")
            message(STATUS "Found libvterm headers at: /usr/local/include")
        endif()
    endif()

    if(USR_LOCAL_INCLUDE)
        target_include_directories(telnet-gui SYSTEM PRIVATE "${USR_LOCAL_INCLUDE}")
    else()
        message(WARNING "libvterm headers not found. Please ensure libvterm is installed in /usr/local")
        target_include_directories(telnet-gui SYSTEM PRIVATE "/usr/local/include")
    endif()
else()
    target_include_directories(telnet-gui SYSTEM PRIVATE "/usr/local/include")
endif()

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

# Note: Tests are registered in the root CMakeLists.txt to ensure
# they are discovered after enable_testing() is called
