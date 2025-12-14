# Telnet GUI CMakeLists.txt

# Application sources
set(GUI_SOURCES
    src/main.c
    src/commands.c
    src/telnet.c
    src/terminal.c
    src/terminal_backend_vterm.c
    src/window.c
    src/renderer.c
    src/renderer_backend_sdl.c
    src/glyph_cache.c
    src/box_drawing.c
    src/input.c
    src/input_area.c
    src/lisp.c
    src/path_utils.c
    src/dynamic_buffer.c
    src/ansi_sequences.c
)

# Add Windows resource file for icon
if(WIN32)
    list(APPEND GUI_SOURCES telnet-gui.rc)
endif()

# Create telnet-gui executable
add_executable(telnet-gui ${GUI_SOURCES})

# Copy font files to build directory
set(FONT_INCONSOLATA_SRC "${CMAKE_CURRENT_SOURCE_DIR}/fonts/Inconsolata-Regular.ttf")
set(FONT_INCONSOLATA_DST "${CMAKE_CURRENT_BINARY_DIR}/fonts/Inconsolata-Regular.ttf")
set(FONT_PLEX_SRC "${CMAKE_CURRENT_SOURCE_DIR}/fonts/IBMPlexMono-Regular.ttf")
set(FONT_PLEX_DST "${CMAKE_CURRENT_BINARY_DIR}/fonts/IBMPlexMono-Regular.ttf")
set(FONT_DEJAVU_SRC "${CMAKE_CURRENT_SOURCE_DIR}/fonts/DejaVuSansMono.ttf")
set(FONT_DEJAVU_DST "${CMAKE_CURRENT_BINARY_DIR}/fonts/DejaVuSansMono.ttf")
set(FONT_DEJAVU_BOLD_SRC "${CMAKE_CURRENT_SOURCE_DIR}/fonts/DejaVuSansMono-Bold.ttf")
set(FONT_DEJAVU_BOLD_DST "${CMAKE_CURRENT_BINARY_DIR}/fonts/DejaVuSansMono-Bold.ttf")
set(FONT_COURIER_SRC "${CMAKE_CURRENT_SOURCE_DIR}/fonts/CourierPrime-Regular.ttf")
set(FONT_COURIER_DST "${CMAKE_CURRENT_BINARY_DIR}/fonts/CourierPrime-Regular.ttf")
set(FONT_CASCADIA_SRC "${CMAKE_CURRENT_SOURCE_DIR}/fonts/CascadiaMono-Regular.ttf")
set(FONT_CASCADIA_DST "${CMAKE_CURRENT_BINARY_DIR}/fonts/CascadiaMono-Regular.ttf")

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
set(BOOTSTRAP_TEST_SRC "${CMAKE_CURRENT_SOURCE_DIR}/bootstrap-test.lisp")
set(BOOTSTRAP_TEST_DST "${CMAKE_CURRENT_BINARY_DIR}/bootstrap-test.lisp")
set(TEST_HELPERS_SRC "${CMAKE_CURRENT_SOURCE_DIR}/test-helpers.lisp")
set(TEST_HELPERS_DST "${CMAKE_CURRENT_BINARY_DIR}/test-helpers.lisp")

# Copy telnet-gui-log2html.py script to build directory
set(LOG2HTML_SRC "${CMAKE_CURRENT_SOURCE_DIR}/scripts/telnet-gui-log2html.py")
set(LOG2HTML_DST "${CMAKE_CURRENT_BINARY_DIR}/telnet-gui-log2html.py")

# Create fonts directory in build directory
add_custom_command(TARGET telnet-gui POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_BINARY_DIR}/fonts"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${FONT_INCONSOLATA_SRC}" "${FONT_INCONSOLATA_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${FONT_PLEX_SRC}" "${FONT_PLEX_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${FONT_DEJAVU_SRC}" "${FONT_DEJAVU_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${FONT_DEJAVU_BOLD_SRC}" "${FONT_DEJAVU_BOLD_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${FONT_COURIER_SRC}" "${FONT_COURIER_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${FONT_CASCADIA_SRC}" "${FONT_CASCADIA_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${BOOTSTRAP_SRC}" "${BOOTSTRAP_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${TINTIN_SRC}" "${TINTIN_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${TINTIN_TEST_SRC}" "${TINTIN_TEST_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${COMPLETION_TEST_SRC}" "${COMPLETION_TEST_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${BOOTSTRAP_TEST_SRC}" "${BOOTSTRAP_TEST_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${TEST_HELPERS_SRC}" "${TEST_HELPERS_DST}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${LOG2HTML_SRC}" "${LOG2HTML_DST}"
    COMMENT "Copying resources to build directory"
)

# On Windows, copy required UCRT64 runtime DLLs to telnet-gui build directory
if(WIN32 AND DEFINED UCRT64_ROOT)
    set(REQUIRED_DLLS
        "${UCRT64_ROOT}/bin/libpcre2-8-0.dll"
        "${UCRT64_ROOT}/bin/libwinpthread-1.dll"
    )
    foreach(DLL_FILE ${REQUIRED_DLLS})
        if(EXISTS "${DLL_FILE}")
            get_filename_component(DLL_NAME "${DLL_FILE}" NAME)
            add_custom_command(TARGET telnet-gui POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    "${DLL_FILE}"
                    "${CMAKE_CURRENT_BINARY_DIR}/${DLL_NAME}"
                COMMENT "Copying ${DLL_NAME} to telnet-gui build directory"
            )
        endif()
    endforeach()
endif()

# Install font files to share directory (POSIX-compliant)
install(FILES
    "${FONT_INCONSOLATA_SRC}"
    "${FONT_PLEX_SRC}"
    "${FONT_DEJAVU_SRC}"
    "${FONT_DEJAVU_BOLD_SRC}"
    "${FONT_COURIER_SRC}"
    "${FONT_CASCADIA_SRC}"
    DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/telnet-gui/fonts"
    COMPONENT Runtime
)

# Install icon files to share directory
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/icons/"
    DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/telnet-gui/icons"
    COMPONENT Runtime
    FILES_MATCHING
    PATTERN "*.png"
    PATTERN "*.ico"
)

# Install Lisp files to share directory (POSIX-compliant)
# Always installs from source directory, so changes are picked up on next install
install(FILES
    "${BOOTSTRAP_SRC}"
    "${TINTIN_SRC}"
    DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/telnet-gui/lisp"
    COMPONENT Runtime
)

# Install telnet-gui-log2html.py as telnet-gui-log2html (without .py extension)
install(PROGRAMS "${LOG2HTML_SRC}"
    DESTINATION "${CMAKE_INSTALL_BINDIR}"
    RENAME "telnet-gui-log2html"
    COMPONENT Runtime
)

# Find required packages
find_package(PkgConfig REQUIRED)
pkg_check_modules(SDL2 REQUIRED sdl2)
pkg_check_modules(SDL2_TTF REQUIRED SDL2_ttf)

# On Windows, prefer static linking for SDL libraries to avoid DLL dependencies
if(WIN32)
    message(STATUS "Windows detected: attempting static linking for SDL libraries")

    # Get library directory from pkg-config
    execute_process(
        COMMAND ${PKG_CONFIG_EXECUTABLE} --variable=libdir SDL2_ttf
        OUTPUT_VARIABLE SDL2_TTF_LIBDIR_RAW
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )

    # Normalize path (convert backslashes to forward slashes for consistency)
    string(REPLACE "\\" "/" SDL2_TTF_LIBDIR "${SDL2_TTF_LIBDIR_RAW}")

    # Check if static libraries exist and use them explicitly
    set(SDL2_STATIC_LIB "${SDL2_TTF_LIBDIR}/libSDL2.a")
    set(SDL2_TTF_STATIC_LIB "${SDL2_TTF_LIBDIR}/libSDL2_ttf.a")

    # Also try to find static libraries in common UCRT64 locations as fallback
    set(SDL2_STATIC_LIB_FOUND "")
    set(SDL2_TTF_STATIC_LIB_FOUND "")

    # Helper function to normalize paths (convert backslashes to forward slashes)
    # This prevents CMake from interpreting backslashes as escape sequences
    function(normalize_path VAR PATH)
        string(REPLACE "\\" "/" NORMALIZED "${PATH}")
        set(${VAR} "${NORMALIZED}" PARENT_SCOPE)
    endfunction()

    # First try the pkg-config libdir
    if(SDL2_TTF_LIBDIR AND EXISTS "${SDL2_STATIC_LIB}" AND EXISTS "${SDL2_TTF_STATIC_LIB}")
        normalize_path(SDL2_STATIC_LIB_FOUND "${SDL2_STATIC_LIB}")
        normalize_path(SDL2_TTF_STATIC_LIB_FOUND "${SDL2_TTF_STATIC_LIB}")
    else()
        # Try to find UCRT64_ROOT and check there
        if(DEFINED UCRT64_ROOT)
            set(UCRT64_LIBDIR "${UCRT64_ROOT}/lib")
            set(UCRT64_SDL2_STATIC "${UCRT64_LIBDIR}/libSDL2.a")
            set(UCRT64_SDL2_TTF_STATIC "${UCRT64_LIBDIR}/libSDL2_ttf.a")
            if(EXISTS "${UCRT64_SDL2_STATIC}" AND EXISTS "${UCRT64_SDL2_TTF_STATIC}")
                normalize_path(SDL2_STATIC_LIB_FOUND "${UCRT64_SDL2_STATIC}")
                normalize_path(SDL2_TTF_STATIC_LIB_FOUND "${UCRT64_SDL2_TTF_STATIC}")
                normalize_path(SDL2_TTF_LIBDIR "${UCRT64_LIBDIR}")
            endif()
        endif()

        # If still not found, try common MSYS2 locations
        if(NOT SDL2_STATIC_LIB_FOUND)
            set(MSYS2_PATHS
                "C:/msys64/ucrt64/lib"
                "C:/tools/msys64/ucrt64/lib"
                "$ENV{USERPROFILE}/scoop/apps/msys2/current/ucrt64/lib"
                "$ENV{ProgramFiles}/msys64/ucrt64/lib"
            )
            # Also check for versioned Scoop installations
            if(EXISTS "$ENV{USERPROFILE}/scoop/apps/msys2")
                file(GLOB SCOOP_UCRT64_LIBS "$ENV{USERPROFILE}/scoop/apps/msys2/*/ucrt64/lib")
                list(APPEND MSYS2_PATHS ${SCOOP_UCRT64_LIBS})
            endif()

            foreach(LIBDIR IN LISTS MSYS2_PATHS)
                set(TEST_SDL2_STATIC "${LIBDIR}/libSDL2.a")
                set(TEST_SDL2_TTF_STATIC "${LIBDIR}/libSDL2_ttf.a")
                if(EXISTS "${TEST_SDL2_STATIC}" AND EXISTS "${TEST_SDL2_TTF_STATIC}")
                    normalize_path(SDL2_STATIC_LIB_FOUND "${TEST_SDL2_STATIC}")
                    normalize_path(SDL2_TTF_STATIC_LIB_FOUND "${TEST_SDL2_TTF_STATIC}")
                    normalize_path(SDL2_TTF_LIBDIR "${LIBDIR}")
                    break()
                endif()
            endforeach()
        endif()
    endif()

    if(SDL2_STATIC_LIB_FOUND AND SDL2_TTF_STATIC_LIB_FOUND)
        # Paths are already normalized by the normalize_path function
        set(SDL2_STATIC_LIB "${SDL2_STATIC_LIB_FOUND}")
        set(SDL2_TTF_STATIC_LIB "${SDL2_TTF_STATIC_LIB_FOUND}")
        message(STATUS "Found static SDL libraries: ${SDL2_STATIC_LIB}, ${SDL2_TTF_STATIC_LIB}")

        # Get dependency flags from pkg-config (but not the SDL libraries themselves)
        execute_process(
            COMMAND ${PKG_CONFIG_EXECUTABLE} --libs --static SDL2_ttf
            OUTPUT_VARIABLE SDL2_TTF_STATIC_LIBS_RAW
            OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_QUIET
        )

        if(SDL2_TTF_STATIC_LIBS_RAW)
            # Parse the pkg-config output into a list first
            string(REPLACE " " ";" SDL2_TTF_LIBS_LIST ${SDL2_TTF_STATIC_LIBS_RAW})

            # List of libraries that should be statically linked (replace -l flags with .a files)
            # These are SDL_ttf dependencies that have both .a and .dll.a versions
            set(STATIC_LIB_NAMES
                freetype bz2 png16 z harfbuzz
                glib-2.0 intl pcre2-8 graphite2
                brotlidec brotlicommon
            )

            # Build a new list with static libraries instead of -l flags
            set(SDL2_TTF_LIBRARIES "")
            foreach(LIB IN LISTS SDL2_TTF_LIBS_LIST)
                # Skip -lSDL2_ttf and -lSDL2, we'll add the static libraries explicitly
                if(LIB STREQUAL "-lSDL2_ttf" OR LIB STREQUAL "-lSDL2")
                    continue()
                endif()

                # Check if this is a library we should statically link
                set(REPLACED FALSE)
                if(LIB MATCHES "^-l(.+)$")
                    set(LIB_NAME "${CMAKE_MATCH_1}")
                    foreach(STATIC_NAME IN LISTS STATIC_LIB_NAMES)
                        if(LIB_NAME STREQUAL STATIC_NAME)
                            # Try to find the static library
                            set(STATIC_LIB_PATH "${SDL2_TTF_LIBDIR}/lib${LIB_NAME}.a")
                            if(EXISTS "${STATIC_LIB_PATH}")
                                normalize_path(STATIC_LIB_PATH_NORM "${STATIC_LIB_PATH}")
                                list(APPEND SDL2_TTF_LIBRARIES "${STATIC_LIB_PATH_NORM}")
                                set(REPLACED TRUE)
                                break()
                            endif()
                        endif()
                    endforeach()
                endif()

                # If not replaced, keep the original flag/library
                if(NOT REPLACED)
                    list(APPEND SDL2_TTF_LIBRARIES ${LIB})
                endif()
            endforeach()

            # Insert static libraries in the correct order: SDL2_ttf depends on SDL2
            # Insert SDL2_ttf first (it will be linked first, then SDL2)
            list(INSERT SDL2_TTF_LIBRARIES 0 "${SDL2_TTF_STATIC_LIB}")
            # Find where -lSDL2main is (if present) and insert SDL2 before it
            # Otherwise, insert SDL2 right after SDL2_ttf
            list(FIND SDL2_TTF_LIBRARIES "-lSDL2main" SDL2MAIN_INDEX)
            if(SDL2MAIN_INDEX GREATER_EQUAL 0)
                list(INSERT SDL2_TTF_LIBRARIES ${SDL2MAIN_INDEX} "${SDL2_STATIC_LIB}")
            else()
                list(INSERT SDL2_TTF_LIBRARIES 1 "${SDL2_STATIC_LIB}")
            endif()

            # Add C++ standard library for harfbuzz and graphite2 (they contain C++ code)
            # Find the static libstdc++.a to ensure static linking
            execute_process(
                COMMAND ${CMAKE_C_COMPILER} -print-file-name=libstdc++.a
                OUTPUT_VARIABLE LIBSTDCPP_STATIC
                OUTPUT_STRIP_TRAILING_WHITESPACE
            )
            if(EXISTS "${LIBSTDCPP_STATIC}")
                normalize_path(LIBSTDCPP_STATIC_NORM "${LIBSTDCPP_STATIC}")
                list(APPEND SDL2_TTF_LIBRARIES "${LIBSTDCPP_STATIC_NORM}")
                message(STATUS "Using explicit static libstdc++: ${LIBSTDCPP_STATIC_NORM}")
            else()
                # Fallback to -lstdc++ and rely on -static-libstdc++ flag
                list(APPEND SDL2_TTF_LIBRARIES -lstdc++)
                message(WARNING "Could not find libstdc++.a, using -lstdc++ (may link dynamically)")
            endif()

            message(STATUS "Using explicit static SDL libraries with dependencies from pkg-config")
            message(STATUS "  Statically linking SDL_ttf dependencies: freetype, bz2, png16, z, harfbuzz, glib-2.0, intl, pcre2-8, graphite2, brotlidec, brotlicommon")
        else()
            # Fallback: use explicit paths with static libraries for dependencies
            # Build list of static dependency libraries
            set(STATIC_DEPS "")
            set(STATIC_DEP_NAMES freetype bz2 png16 z harfbuzz glib-2.0 intl pcre2-8 graphite2 brotlidec brotlicommon)
            foreach(DEP_NAME IN LISTS STATIC_DEP_NAMES)
                set(DEP_STATIC "${SDL2_TTF_LIBDIR}/lib${DEP_NAME}.a")
                if(EXISTS "${DEP_STATIC}")
                    normalize_path(DEP_STATIC_NORM "${DEP_STATIC}")
                    list(APPEND STATIC_DEPS "${DEP_STATIC_NORM}")
                else()
                    # Fallback to -l flag if static library not found
                    list(APPEND STATIC_DEPS "-l${DEP_NAME}")
                endif()
            endforeach()

            # Find static libstdc++.a for fallback path too
            execute_process(
                COMMAND ${CMAKE_C_COMPILER} -print-file-name=libstdc++.a
                OUTPUT_VARIABLE LIBSTDCPP_STATIC
                OUTPUT_STRIP_TRAILING_WHITESPACE
            )
            if(EXISTS "${LIBSTDCPP_STATIC}")
                normalize_path(LIBSTDCPP_STATIC_NORM "${LIBSTDCPP_STATIC}")
                set(STDCPP_LIB "${LIBSTDCPP_STATIC_NORM}")
                message(STATUS "Using explicit static libstdc++: ${LIBSTDCPP_STATIC_NORM}")
            else()
                set(STDCPP_LIB "-lstdc++")
                message(WARNING "Could not find libstdc++.a, using -lstdc++ (may link dynamically)")
            endif()

            # Build base library list without -mwindows (we'll add it conditionally based on build type)
            # -lSDL2main is always needed for SDL2 entry point
            set(SDL2_TTF_LIBRARIES
                -lmingw32 -lSDL2main
                "${SDL2_STATIC_LIB}"
                "${SDL2_TTF_STATIC_LIB}"
                -lm -lkernel32 -luser32 -lgdi32 -lwinmm -limm32 -lole32 -loleaut32 -lversion -luuid
                -ladvapi32 -lsetupapi -lshell32 -ldinput8
                ${STATIC_DEPS}
                -lusp10 -ldwrite
                -lws2_32 -lshlwapi -latomic
                ${STDCPP_LIB}
            )
            message(STATUS "Using explicit static SDL libraries (with fallback dependencies)")
        endif()
    else()
        message(WARNING "Static SDL libraries not found")
        message(WARNING "  Checked: ${SDL2_STATIC_LIB}")
        message(WARNING "  Checked: ${SDL2_TTF_STATIC_LIB}")
        if(SDL2_TTF_LIBDIR)
            message(WARNING "  pkg-config libdir: ${SDL2_TTF_LIBDIR}")
        endif()
        message(WARNING "Falling back to dynamic linking - DLLs will be required at runtime")
        # Don't override SDL2_TTF_LIBRARIES here - let pkg_check_modules set it
    endif()

    # Remove -mwindows from SDL2_TTF_LIBRARIES if present (we'll add it conditionally based on build type)
    # Keep -lSDL2main as it's needed for SDL2 entry point, but only use -mwindows for Release builds
    # This ensures non-Release builds stay attached to console
    list(REMOVE_ITEM SDL2_TTF_LIBRARIES "-mwindows")
endif()
# Try pkg-config with /ucrt64/lib/pkgconfig (MSYS2 package) or /usr/local/lib/pkgconfig (source build)
# Find the pkg-config file location first
set(VTERM_PKG_CONFIG "")
if(WIN32)
    # First check /usr/local by deriving MSYS2 root from UCRT64_ROOT (PRIORITY)
    if(DEFINED UCRT64_ROOT)
        # UCRT64_ROOT is like C:/msys64/ucrt64, so parent is C:/msys64
        get_filename_component(MSYS2_ROOT_FROM_UCRT64 "${UCRT64_ROOT}" DIRECTORY)
        if(EXISTS "${MSYS2_ROOT_FROM_UCRT64}/usr/local/lib/pkgconfig/vterm.pc")
            set(VTERM_PKG_CONFIG "${MSYS2_ROOT_FROM_UCRT64}/usr/local/lib/pkgconfig")
            message(STATUS "Found vterm.pc in /usr/local: ${VTERM_PKG_CONFIG}")
        endif()
    endif()

    # If not found, check /usr/local via MSYS2_ROOT env var (fallback)
    if(NOT VTERM_PKG_CONFIG AND DEFINED ENV{MSYS2_ROOT})
        if(EXISTS "$ENV{MSYS2_ROOT}/usr/local/lib/pkgconfig/vterm.pc")
            set(VTERM_PKG_CONFIG "$ENV{MSYS2_ROOT}/usr/local/lib/pkgconfig")
            message(STATUS "Found vterm.pc in /usr/local: ${VTERM_PKG_CONFIG}")
        endif()
    endif()

    # If not found in /usr/local, check UCRT64 (distribution package - FALLBACK)
    if(NOT VTERM_PKG_CONFIG AND DEFINED UCRT64_ROOT)
        if(EXISTS "${UCRT64_ROOT}/lib/pkgconfig/vterm.pc")
            set(VTERM_PKG_CONFIG "${UCRT64_ROOT}/lib/pkgconfig")
            message(STATUS "Found vterm.pc in UCRT64: ${VTERM_PKG_CONFIG}")
        endif()
    endif()

    # Search common MSYS2 locations for both UCRT64 and /usr/local
    if(NOT VTERM_PKG_CONFIG)
        set(PKG_SEARCH_PATHS
            # /usr/local source build locations (PRIORITY)
            "C:/msys64/usr/local/lib/pkgconfig"
            "C:/tools/msys64/usr/local/lib/pkgconfig"
            "$ENV{USERPROFILE}/scoop/apps/msys2/current/usr/local/lib/pkgconfig"
            "$ENV{ProgramFiles}/msys64/usr/local/lib/pkgconfig"
            # UCRT64 package locations (FALLBACK)
            "C:/msys64/ucrt64/lib/pkgconfig"
            "C:/tools/msys64/ucrt64/lib/pkgconfig"
            "$ENV{USERPROFILE}/scoop/apps/msys2/current/ucrt64/lib/pkgconfig"
            "$ENV{ProgramFiles}/msys64/ucrt64/lib/pkgconfig"
        )
        # Add Scoop versioned directories - /usr/local first
        if(EXISTS "$ENV{USERPROFILE}/scoop/apps/msys2")
            file(GLOB SCOOP_PKG_USRLOCAL "$ENV{USERPROFILE}/scoop/apps/msys2/*/usr/local/lib/pkgconfig")
            file(GLOB SCOOP_PKG_UCRT64 "$ENV{USERPROFILE}/scoop/apps/msys2/*/ucrt64/lib/pkgconfig")
            list(APPEND PKG_SEARCH_PATHS ${SCOOP_PKG_USRLOCAL} ${SCOOP_PKG_UCRT64})
        endif()

        foreach(PKG_PATH IN LISTS PKG_SEARCH_PATHS)
            if(EXISTS "${PKG_PATH}/vterm.pc")
                set(VTERM_PKG_CONFIG "${PKG_PATH}")
                message(STATUS "Found vterm.pc at: ${VTERM_PKG_CONFIG}")
                break()
            endif()
        endforeach()
    endif()
endif()

# Try pkg-config - CMake's pkg_check_modules caches results and may ignore ENV changes
# So we manually call pkg-config via execute_process
set(VTERM_FOUND FALSE)
if(VTERM_PKG_CONFIG OR EXISTS "/usr/local/lib/pkgconfig/vterm.pc" OR EXISTS "/ucrt64/lib/pkgconfig/vterm.pc")
    if(NOT VTERM_PKG_CONFIG)
        # Try Unix-style paths (for MSYS2 shell environments)
        if(EXISTS "/usr/local/lib/pkgconfig/vterm.pc")
            set(VTERM_PKG_CONFIG "/usr/local/lib/pkgconfig")
        elseif(EXISTS "/ucrt64/lib/pkgconfig/vterm.pc")
            set(VTERM_PKG_CONFIG "/ucrt64/lib/pkgconfig")
        endif()
    endif()

    # Use pkg-config executable from find_package
    # PKG_CONFIG_EXECUTABLE is set by find_package(PkgConfig) above
    if(PKG_CONFIG_EXECUTABLE)
        # Check if VS Code or user already set PKG_CONFIG_PATH
        # If so, make sure our path is included
        set(PKG_CONFIG_UNIX_PATH "${VTERM_PKG_CONFIG}")
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
    message(STATUS "Using libvterm from system paths (pkg-config not available or package not found)")
    message(STATUS "  Will search: /ucrt64 (MSYS2 package) and /usr/local (source build)")
endif()

# Test if libvterm has sb_pushline4 support (unreleased feature)
include(CheckCSourceCompiles)
# Derive include directory from VTERM_PKG_CONFIG path
# If vterm.pc is in /path/lib/pkgconfig, headers are in /path/include
set(VTERM_TEST_INCLUDES "")
if(VTERM_PKG_CONFIG)
    string(REGEX REPLACE "/lib/pkgconfig$" "/include" VTERM_TEST_INCLUDE_DIR "${VTERM_PKG_CONFIG}")
    list(APPEND VTERM_TEST_INCLUDES "${VTERM_TEST_INCLUDE_DIR}")
endif()

# Add UCRT64 system includes when running on Windows with forced UCRT64 toolchain
# This ensures check_c_source_compiles can find system headers
if(WIN32 AND DEFINED UCRT64_ROOT)
    list(APPEND VTERM_TEST_INCLUDES "${UCRT64_ROOT}/include")

    # Add GCC include paths if available
    file(GLOB GCC_VERSIONS "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/*")
    if(GCC_VERSIONS)
        list(SORT GCC_VERSIONS)
        list(REVERSE GCC_VERSIONS)
        list(GET GCC_VERSIONS 0 GCC_FIRST)
        get_filename_component(GCC_VERSION "${GCC_FIRST}" NAME)
        list(APPEND VTERM_TEST_INCLUDES
            "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/${GCC_VERSION}/include")
        if(EXISTS "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/${GCC_VERSION}/include-fixed")
            list(APPEND VTERM_TEST_INCLUDES
                "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/${GCC_VERSION}/include-fixed")
        endif()
    endif()
endif()

set(CMAKE_REQUIRED_INCLUDES ${VTERM_TEST_INCLUDES})
set(CMAKE_REQUIRED_LIBRARIES ${VTERM_LIBRARIES})

check_c_source_compiles("
    #include <vterm.h>
    int main() {
        VTermScreenCallbacks cb;
        cb.sb_pushline4 = NULL;
        return 0;
    }
" HAVE_VTERM_PUSHLINE4)

if(HAVE_VTERM_PUSHLINE4)
    message(STATUS "libvterm has pushline4 support - enabling text reflow")
    target_compile_definitions(telnet-gui PRIVATE HAVE_VTERM_PUSHLINE4=1)
else()
    message(STATUS "libvterm does not have pushline4 - text reflow disabled")
    target_compile_definitions(telnet-gui PRIVATE HAVE_VTERM_PUSHLINE4=0)

    # Check if using distribution libvterm 0.3.3 and recommend upstream
    if(VTERM_VERSION AND VTERM_VERSION VERSION_LESS_EQUAL "0.3.3")
        message(STATUS "")

        # Determine if from source build or distribution package
        string(FIND "${VTERM_PKG_CONFIG}" "/usr/local/" FOUND_USR_LOCAL)
        if(FOUND_USR_LOCAL EQUAL -1)
            # Not in /usr/local, likely distribution package
            message(STATUS "NOTE: libvterm ${VTERM_VERSION} detected from distribution package")
            message(STATUS "For full text reflow support, consider building from upstream source:")
            message(STATUS "  git clone https://github.com/neovim/libvterm.git -b nvim")
            message(STATUS "  cd libvterm && make && make install")
            message(STATUS "This will install to /usr/local which is checked first by CMake")
        else()
            # In /usr/local but still lacks pushline4
            message(STATUS "NOTE: libvterm ${VTERM_VERSION} detected in /usr/local (lacks pushline4)")
            message(STATUS "For full text reflow support, update from nvim branch:")
            message(STATUS "  cd <libvterm-source-dir>")
            message(STATUS "  git checkout nvim && git pull")
            message(STATUS "  make clean && make && make install")
        endif()
        message(STATUS "")
    endif()
endif()

# Pass libvterm version to C code with pushline4 indicator
if(VTERM_VERSION)
    if(HAVE_VTERM_PUSHLINE4)
        set(VTERM_VERSION_WITH_FEATURES "${VTERM_VERSION}+pushline4")
    else()
        set(VTERM_VERSION_WITH_FEATURES "${VTERM_VERSION}")
    endif()
    target_compile_definitions(telnet-gui PRIVATE LIBVTERM_VERSION="${VTERM_VERSION_WITH_FEATURES}")
else()
    target_compile_definitions(telnet-gui PRIVATE LIBVTERM_VERSION="unknown")
endif()

# Collect dependency versions using pkg-config
execute_process(
    COMMAND ${PKG_CONFIG_EXECUTABLE} --modversion sdl2
    OUTPUT_VARIABLE SDL2_PKG_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
)

execute_process(
    COMMAND ${PKG_CONFIG_EXECUTABLE} --modversion SDL2_ttf
    OUTPUT_VARIABLE SDL2_TTF_PKG_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
)

execute_process(
    COMMAND ${PKG_CONFIG_EXECUTABLE} --modversion bdw-gc
    OUTPUT_VARIABLE GC_PKG_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
)

execute_process(
    COMMAND ${PKG_CONFIG_EXECUTABLE} --modversion libpcre2-8
    OUTPUT_VARIABLE PCRE2_PKG_VERSION
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
)

# Pass all versions to C code as macros
if(SDL2_PKG_VERSION)
    target_compile_definitions(telnet-gui PRIVATE SDL2_VERSION="${SDL2_PKG_VERSION}")
else()
    target_compile_definitions(telnet-gui PRIVATE SDL2_VERSION="unknown")
endif()

if(SDL2_TTF_PKG_VERSION)
    target_compile_definitions(telnet-gui PRIVATE SDL2_TTF_VERSION="${SDL2_TTF_PKG_VERSION}")
else()
    target_compile_definitions(telnet-gui PRIVATE SDL2_TTF_VERSION="unknown")
endif()

if(GC_PKG_VERSION)
    target_compile_definitions(telnet-gui PRIVATE GC_VERSION="${GC_PKG_VERSION}")
else()
    target_compile_definitions(telnet-gui PRIVATE GC_VERSION="unknown")
endif()

if(PCRE2_PKG_VERSION)
    target_compile_definitions(telnet-gui PRIVATE PCRE2_VERSION="${PCRE2_PKG_VERSION}")
else()
    target_compile_definitions(telnet-gui PRIVATE PCRE2_VERSION="unknown")
endif()

# Project version from root CMakeLists.txt
target_compile_definitions(telnet-gui PRIVATE PROJECT_VERSION="${CMAKE_PROJECT_VERSION}")

# System information
target_compile_definitions(telnet-gui PRIVATE
    CMAKE_VERSION="${CMAKE_VERSION}"
    SYSTEM_NAME="${CMAKE_SYSTEM_NAME}"
    SYSTEM_PROCESSOR="${CMAKE_SYSTEM_PROCESSOR}"
)

# Toolchain detection - detailed information about compiler and environment
# Get compiler target triple (e.g., x86_64-w64-mingw32-ucrt)
execute_process(
    COMMAND ${CMAKE_C_COMPILER} -dumpmachine
    OUTPUT_VARIABLE COMPILER_TARGET_TRIPLE
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
)

# On Windows, detect MSYS2 environment from compiler path
if(WIN32)
    set(MSYS2_ENV "")
    string(TOLOWER "${CMAKE_C_COMPILER}" COMPILER_PATH_LOWER)
    if(COMPILER_PATH_LOWER MATCHES "/ucrt64/")
        set(MSYS2_ENV "UCRT64")
    elseif(COMPILER_PATH_LOWER MATCHES "/mingw64/")
        set(MSYS2_ENV "MINGW64")
    elseif(COMPILER_PATH_LOWER MATCHES "/mingw32/")
        set(MSYS2_ENV "MINGW32")
    elseif(COMPILER_PATH_LOWER MATCHES "/clang64/")
        set(MSYS2_ENV "CLANG64")
    elseif(COMPILER_PATH_LOWER MATCHES "/clang32/")
        set(MSYS2_ENV "CLANG32")
    elseif(COMPILER_PATH_LOWER MATCHES "/clangarm64/")
        set(MSYS2_ENV "CLANGARM64")
    elseif(COMPILER_PATH_LOWER MATCHES "/msys/")
        set(MSYS2_ENV "MSYS")
    endif()

    # Build detailed toolchain string
    if(MSYS2_ENV)
        # MSYS2: "UCRT64 (GCC 13.2.0, x86_64-w64-mingw32-ucrt)"
        set(TOOLCHAIN_STRING "${MSYS2_ENV} (${CMAKE_C_COMPILER_ID} ${CMAKE_C_COMPILER_VERSION}, ${COMPILER_TARGET_TRIPLE})")
    else()
        # Native Windows: "MSVC 19.30 (x86_64-pc-windows-msvc)" or similar
        set(TOOLCHAIN_STRING "${CMAKE_C_COMPILER_ID} ${CMAKE_C_COMPILER_VERSION} (${COMPILER_TARGET_TRIPLE})")
    endif()
else()
    # Unix/Linux/macOS: "GCC 11.4.0 (x86_64-linux-gnu)" or "AppleClang 14.0 (arm64-apple-darwin22)"
    set(TOOLCHAIN_STRING "${CMAKE_C_COMPILER_ID} ${CMAKE_C_COMPILER_VERSION} (${COMPILER_TARGET_TRIPLE})")
endif()

target_compile_definitions(telnet-gui PRIVATE TOOLCHAIN="${TOOLCHAIN_STRING}")

# Test for vterm_screen_enable_reflow
check_c_source_compiles("
    #include <vterm.h>
    int main() {
        VTerm *vt = NULL;
        VTermScreen *screen = NULL;
        vterm_screen_enable_reflow(screen, 1);
        return 0;
    }
" HAVE_VTERM_REFLOW)

if(HAVE_VTERM_REFLOW)
    target_compile_definitions(telnet-gui PRIVATE HAVE_VTERM_REFLOW=1)
else()
    target_compile_definitions(telnet-gui PRIVATE HAVE_VTERM_REFLOW=0)
endif()

# Check for SDL_ttf DPI support (SDL_ttf 2.20.0+)
# Set up required includes and libraries for the compile test
set(SDL_TTF_TEST_INCLUDES "")
list(APPEND SDL_TTF_TEST_INCLUDES ${SDL2_TTF_INCLUDE_DIRS})
list(APPEND SDL_TTF_TEST_INCLUDES ${SDL2_INCLUDE_DIRS})

# Add UCRT64 system includes when running on Windows
if(WIN32 AND DEFINED UCRT64_ROOT)
    list(APPEND SDL_TTF_TEST_INCLUDES "${UCRT64_ROOT}/include")
endif()

# Save current values and set up for test
set(CMAKE_REQUIRED_INCLUDES_SAVE ${CMAKE_REQUIRED_INCLUDES})
set(CMAKE_REQUIRED_LIBRARIES_SAVE ${CMAKE_REQUIRED_LIBRARIES})
set(CMAKE_REQUIRED_INCLUDES ${SDL_TTF_TEST_INCLUDES})
set(CMAKE_REQUIRED_LIBRARIES ${SDL2_TTF_LIBRARIES})

check_c_source_compiles("
    #include <SDL_ttf.h>
    int main(int argc, char *argv[]) {
        TTF_Font *font = TTF_OpenFontDPI(\"test.ttf\", 12, 96, 96);
        (void)font;
        (void)argc;
        (void)argv;
        return 0;
    }
" HAVE_SDL_TTF_DPI)

# Restore previous values
set(CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES_SAVE})
set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES_SAVE})

if(HAVE_SDL_TTF_DPI)
    message(STATUS "SDL_ttf DPI support: available (SDL_ttf 2.20.0+)")
    target_compile_definitions(telnet-gui PRIVATE HAVE_SDL_TTF_DPI=1)
else()
    message(STATUS "SDL_ttf DPI support: not available (SDL_ttf < 2.20.0)")
    target_compile_definitions(telnet-gui PRIVATE HAVE_SDL_TTF_DPI=0)
endif()

# GC and PCRE2 configured by parent CMakeLists.txt with static linking support

# Link directories first
target_link_directories(telnet-gui PRIVATE ${SDL2_LIBDIR} ${SDL2_TTF_LIBDIR} ${VTERM_LIBDIR})

# Add libvterm library directories if not found via pkg-config
# Search both /ucrt64/lib (MSYS2 package) and /usr/local/lib (source build)
if(WIN32 AND NOT VTERM_LIBDIR)
    # Find MSYS2 root and search for libvterm in both UCRT64 and /usr/local
    set(VTERM_LIB_DIR "")

    # First check /usr/local by deriving MSYS2 root from UCRT64_ROOT (PRIORITY)
    if(DEFINED UCRT64_ROOT)
        get_filename_component(MSYS2_ROOT_FROM_UCRT64 "${UCRT64_ROOT}" DIRECTORY)
        if(EXISTS "${MSYS2_ROOT_FROM_UCRT64}/usr/local/lib/libvterm.a" OR EXISTS "${MSYS2_ROOT_FROM_UCRT64}/usr/local/lib/libvterm.dll.a")
            set(VTERM_LIB_DIR "${MSYS2_ROOT_FROM_UCRT64}/usr/local/lib")
            message(STATUS "Found libvterm library in /usr/local: ${VTERM_LIB_DIR}")
        endif()
    endif()

    # Then check /usr/local via MSYS2_ROOT env var (fallback)
    if(NOT VTERM_LIB_DIR AND DEFINED ENV{MSYS2_ROOT})
        if(EXISTS "$ENV{MSYS2_ROOT}/usr/local/lib/libvterm.a" OR EXISTS "$ENV{MSYS2_ROOT}/usr/local/lib/libvterm.dll.a")
            set(VTERM_LIB_DIR "$ENV{MSYS2_ROOT}/usr/local/lib")
            message(STATUS "Found libvterm library in /usr/local: ${VTERM_LIB_DIR}")
        endif()
    endif()

    # Then check UCRT64_ROOT if defined (FALLBACK)
    if(NOT VTERM_LIB_DIR AND DEFINED UCRT64_ROOT)
        if(EXISTS "${UCRT64_ROOT}/lib/libvterm.a" OR EXISTS "${UCRT64_ROOT}/lib/libvterm.dll.a")
            set(VTERM_LIB_DIR "${UCRT64_ROOT}/lib")
            message(STATUS "Found libvterm library in UCRT64: ${VTERM_LIB_DIR}")
        endif()
    endif()

    # Search common MSYS2 locations for both UCRT64 and /usr/local
    if(NOT VTERM_LIB_DIR)
        set(MSYS2_LIB_PATHS
            # /usr/local source build locations (PRIORITY)
            "C:/msys64"
            "C:/tools/msys64"
            "$ENV{USERPROFILE}/scoop/apps/msys2/current"
            "$ENV{ProgramFiles}/msys64"
            "${PROGRAM_FILES_X86}/msys64"
            # UCRT64 package locations (FALLBACK)
            "C:/msys64/ucrt64"
            "C:/tools/msys64/ucrt64"
            "$ENV{USERPROFILE}/scoop/apps/msys2/current/ucrt64"
            "$ENV{ProgramFiles}/msys64/ucrt64"
        )
        if(EXISTS "$ENV{USERPROFILE}/scoop/apps/msys2")
            file(GLOB SCOOP_ROOT_VERSIONS "$ENV{USERPROFILE}/scoop/apps/msys2/*")
            file(GLOB SCOOP_UCRT64_VERSIONS "$ENV{USERPROFILE}/scoop/apps/msys2/*/ucrt64")
            list(APPEND MSYS2_LIB_PATHS ${SCOOP_ROOT_VERSIONS} ${SCOOP_UCRT64_VERSIONS})
        endif()

        foreach(ROOT IN LISTS MSYS2_LIB_PATHS)
            # Check /usr/local/lib first
            if(EXISTS "${ROOT}/usr/local/lib/libvterm.a" OR EXISTS "${ROOT}/usr/local/lib/libvterm.dll.a")
                set(VTERM_LIB_DIR "${ROOT}/usr/local/lib")
                message(STATUS "Found libvterm library at: ${VTERM_LIB_DIR}")
                break()
            endif()
            # Then check UCRT64 lib location
            if(EXISTS "${ROOT}/lib/libvterm.a" OR EXISTS "${ROOT}/lib/libvterm.dll.a")
                set(VTERM_LIB_DIR "${ROOT}/lib")
                message(STATUS "Found libvterm library at: ${VTERM_LIB_DIR}")
                break()
            endif()
        endforeach()
    endif()

    if(VTERM_LIB_DIR)
        target_link_directories(telnet-gui PRIVATE "${VTERM_LIB_DIR}")
    else()
        # Fallback: add both possible locations
        target_link_directories(telnet-gui PRIVATE "/ucrt64/lib" "/usr/local/lib")
    endif()
else()
    # Unix/Linux: try both standard locations
    target_link_directories(telnet-gui PRIVATE "/usr/local/lib")
endif()

# Link libraries - only link SDL2_ttf since it includes SDL2
# VTERM already includes pthread
# Need ws2_32 for Windows sockets
# Link against telnet-lisp for completion hooks
# Also need GC and PCRE2 since lisp is static
target_link_libraries(telnet-gui PRIVATE
    ${SDL2_TTF_LIBRARIES}
    ${VTERM_LIBRARIES}
    lisp
    ${GC_LIBRARIES}
    ${PCRE2_LIBRARIES}
    ws2_32
    m
)

# On Windows, keep console subsystem (no -mwindows) for proper console I/O with -t and -h flags.
# The console window is hidden programmatically in main.c for normal GUI operation using ShowWindow().
# This approach works better than -mwindows + AttachConsole() which has issues with PowerShell redirection.
# Note: -lSDL2main is still linked to provide the proper SDL2 entry point.

# Include directories
target_include_directories(telnet-gui PRIVATE
    src
    ../telnet-lisp/include
)

# Add libvterm include directories
# Search both /ucrt64/include (MSYS2 package) and /usr/local/include (source build)
if(WIN32)
    set(VTERM_INCLUDE_DIR "")
    if(NOT DEFINED PROGRAM_FILES_X86)
        set(PROGRAM_FILES_X86 "$ENV{ProgramFiles\(x86\)}")
    endif()

    # First check /usr/local by deriving MSYS2 root from UCRT64_ROOT (PRIORITY)
    if(DEFINED UCRT64_ROOT)
        get_filename_component(MSYS2_ROOT_FROM_UCRT64 "${UCRT64_ROOT}" DIRECTORY)
        if(EXISTS "${MSYS2_ROOT_FROM_UCRT64}/usr/local/include/vterm.h")
            set(VTERM_INCLUDE_DIR "${MSYS2_ROOT_FROM_UCRT64}/usr/local/include")
            message(STATUS "Found libvterm headers in /usr/local: ${VTERM_INCLUDE_DIR}")
        endif()
    endif()

    # Then check /usr/local via MSYS2_ROOT env var (fallback)
    if(NOT VTERM_INCLUDE_DIR AND DEFINED ENV{MSYS2_ROOT})
        if(EXISTS "$ENV{MSYS2_ROOT}/usr/local/include/vterm.h")
            set(VTERM_INCLUDE_DIR "$ENV{MSYS2_ROOT}/usr/local/include")
            message(STATUS "Found libvterm headers in /usr/local: ${VTERM_INCLUDE_DIR}")
        endif()
    endif()

    # Then check UCRT64_ROOT if defined (FALLBACK)
    if(NOT VTERM_INCLUDE_DIR AND DEFINED UCRT64_ROOT)
        if(EXISTS "${UCRT64_ROOT}/include/vterm.h")
            set(VTERM_INCLUDE_DIR "${UCRT64_ROOT}/include")
            message(STATUS "Found libvterm headers in UCRT64: ${VTERM_INCLUDE_DIR}")
        endif()
    endif()

    # Search common MSYS2 locations for both UCRT64 and /usr/local
    if(NOT VTERM_INCLUDE_DIR)
        set(MSYS2_PATHS
            # /usr/local source build locations (PRIORITY)
            "C:/msys64"
            "C:/tools/msys64"
            "$ENV{USERPROFILE}/scoop/apps/msys2/current"
            "$ENV{ProgramFiles}/msys64"
            "${PROGRAM_FILES_X86}/msys64"
            # UCRT64 package locations (FALLBACK)
            "C:/msys64/ucrt64"
            "C:/tools/msys64/ucrt64"
            "$ENV{USERPROFILE}/scoop/apps/msys2/current/ucrt64"
            "$ENV{ProgramFiles}/msys64/ucrt64"
        )

        # Also check for versioned Scoop installations
        if(EXISTS "$ENV{USERPROFILE}/scoop/apps/msys2")
            file(GLOB SCOOP_ROOT_VERSIONS "$ENV{USERPROFILE}/scoop/apps/msys2/*")
            file(GLOB SCOOP_UCRT64_VERSIONS "$ENV{USERPROFILE}/scoop/apps/msys2/*/ucrt64")
            list(APPEND MSYS2_PATHS ${SCOOP_ROOT_VERSIONS} ${SCOOP_UCRT64_VERSIONS})
        endif()

        foreach(ROOT IN LISTS MSYS2_PATHS)
            # Check /usr/local/include first
            if(EXISTS "${ROOT}/usr/local/include/vterm.h")
                set(VTERM_INCLUDE_DIR "${ROOT}/usr/local/include")
                message(STATUS "Found libvterm headers at: ${VTERM_INCLUDE_DIR}")
                break()
            endif()
            # Then check UCRT64 include location
            if(EXISTS "${ROOT}/include/vterm.h")
                set(VTERM_INCLUDE_DIR "${ROOT}/include")
                message(STATUS "Found libvterm headers at: ${VTERM_INCLUDE_DIR}")
                break()
            endif()
        endforeach()
    endif()

    # Fallback: try Unix-style paths (works in MSYS2 shell)
    if(NOT VTERM_INCLUDE_DIR)
        if(EXISTS "/usr/local/include/vterm.h")
            set(VTERM_INCLUDE_DIR "/usr/local/include")
            message(STATUS "Found libvterm headers at: /usr/local/include")
        elseif(EXISTS "/ucrt64/include/vterm.h")
            set(VTERM_INCLUDE_DIR "/ucrt64/include")
            message(STATUS "Found libvterm headers at: /ucrt64/include")
        endif()
    endif()

    if(VTERM_INCLUDE_DIR)
        target_include_directories(telnet-gui SYSTEM PRIVATE "${VTERM_INCLUDE_DIR}")
    else()
        message(WARNING "libvterm headers not found. Please install libvterm:")
        message(WARNING "  MSYS2 package: pacman -S mingw-w64-ucrt-x86_64-libvterm")
        message(WARNING "  Source build: https://github.com/neovim/libvterm → make install")
        # Fallback: add both possible locations
        target_include_directories(telnet-gui SYSTEM PRIVATE "/ucrt64/include" "/usr/local/include")
    endif()
else()
    # Unix/Linux: standard location
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
