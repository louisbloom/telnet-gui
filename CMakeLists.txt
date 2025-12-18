cmake_minimum_required(VERSION 3.10)

# IMPORTANT: Detect and set UCRT64 compiler BEFORE project() command
# This ensures CMake uses the correct compiler from the start
if(WIN32 AND NOT CMAKE_C_COMPILER)
    # Detect MSYS2/UCRT64 installation in common locations
    if(DEFINED ENV{MSYS2_ROOT})
        set(UCRT64_ROOT "$ENV{MSYS2_ROOT}/ucrt64")
    else()
        # Try common installation paths
        set(PROGRAM_FILES_X86 "$ENV{ProgramFiles\(x86\)}")

        # Start with Scoop installations (user's intentional override)
        set(MSYS2_INSTALLS)

        # Scoop: Add both current symlink and versioned directories
        if(EXISTS "$ENV{USERPROFILE}/scoop/apps/msys2")
            list(APPEND MSYS2_INSTALLS "$ENV{USERPROFILE}/scoop/apps/msys2/current")
            file(GLOB SCOOP_MSYS2_VERSIONS "$ENV{USERPROFILE}/scoop/apps/msys2/*")
            list(APPEND MSYS2_INSTALLS ${SCOOP_MSYS2_VERSIONS})
        endif()

        # Then check official installations
        list(APPEND MSYS2_INSTALLS
            "C:/msys64"
            "C:/tools/msys64"
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

    if(UCRT64_ROOT)
        message(STATUS "Found MSYS2 UCRT64 at: ${UCRT64_ROOT}")
        message(STATUS "Configuring UCRT64 toolchain before project initialization")

        # Set UCRT64 compiler BEFORE project() command
        set(CMAKE_C_COMPILER "${UCRT64_ROOT}/bin/gcc.exe" CACHE FILEPATH "C compiler" FORCE)
        set(CMAKE_CXX_COMPILER "${UCRT64_ROOT}/bin/g++.exe" CACHE FILEPATH "C++ compiler" FORCE)
        set(CMAKE_RC_COMPILER "${UCRT64_ROOT}/bin/windres.exe" CACHE FILEPATH "RC compiler" FORCE)

        # Force pkg-config to use UCRT64 packages
        set(UCRT64_PKG_CONFIG "${UCRT64_ROOT}/bin/pkg-config.exe")
        if(EXISTS "${UCRT64_PKG_CONFIG}")
            set(PKG_CONFIG_EXECUTABLE "${UCRT64_PKG_CONFIG}" CACHE FILEPATH "pkg-config" FORCE)
        endif()

        # Prepend UCRT64 pkg-config paths
        set(UCRT64_PKG_CONFIG_PATH "${UCRT64_ROOT}/lib/pkgconfig")
        if(DEFINED ENV{PKG_CONFIG_PATH})
            set(ENV{PKG_CONFIG_PATH} "${UCRT64_PKG_CONFIG_PATH}:$ENV{PKG_CONFIG_PATH}")
        else()
            set(ENV{PKG_CONFIG_PATH} "${UCRT64_PKG_CONFIG_PATH}")
        endif()

        message(STATUS "  Using UCRT64 compiler: ${CMAKE_C_COMPILER}")
        message(STATUS "  Using UCRT64 pkg-config: ${PKG_CONFIG_EXECUTABLE}")
    endif()
endif()

project(TelnetLisp VERSION 1.0.0 LANGUAGES C)

# Use GNUInstallDirs for POSIX-compliant installation paths
include(GNUInstallDirs)

# Default to user install directory (no admin privileges required)
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    if(WIN32)
        set(CMAKE_INSTALL_PREFIX "$ENV{USERPROFILE}/telnet-lisp" CACHE PATH "Install path prefix" FORCE)
    else()
        set(CMAKE_INSTALL_PREFIX "$ENV{HOME}/telnet-lisp" CACHE PATH "Install path prefix" FORCE)
    endif()
endif()

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
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libgcc -static-libstdc++")

    # UCRT64_ROOT was already set before project() if needed
    # This section handles cases where we're already in UCRT64 environment
    if(NOT DEFINED UCRT64_ROOT)
        # Detect MSYS2/UCRT64 installation in common locations
        if(DEFINED ENV{MSYS2_ROOT})
            set(UCRT64_ROOT "$ENV{MSYS2_ROOT}/ucrt64")
        else()
            # Try common installation paths
            set(PROGRAM_FILES_X86 "$ENV{ProgramFiles\(x86\)}")

            # Start with Scoop installations (user's intentional override)
            set(MSYS2_INSTALLS)

            # Scoop: Add both current symlink and versioned directories
            if(EXISTS "$ENV{USERPROFILE}/scoop/apps/msys2")
                list(APPEND MSYS2_INSTALLS "$ENV{USERPROFILE}/scoop/apps/msys2/current")
                file(GLOB SCOOP_MSYS2_VERSIONS "$ENV{USERPROFILE}/scoop/apps/msys2/*")
                list(APPEND MSYS2_INSTALLS ${SCOOP_MSYS2_VERSIONS})
            endif()

            # Then check official installations
            list(APPEND MSYS2_INSTALLS
                "C:/msys64"
                "C:/tools/msys64"
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

        if(UCRT64_ROOT)
            message(STATUS "Found MSYS2 UCRT64 at: ${UCRT64_ROOT}")
        else()
            message(WARNING "MSYS2 UCRT64 not found - some features may not be available")
            message(WARNING "Install MSYS2 UCRT64 packages: pacman -S mingw-w64-ucrt-x86_64-{gcc,cmake,bdw-gc,pcre2}")
        endif()
    endif()

    # Add UCRT64 system include paths for clangd if found
    if(EXISTS "${UCRT64_ROOT}/include")
        include_directories(SYSTEM "${UCRT64_ROOT}/include")
        # Add GCC include paths if they exist
        file(GLOB GCC_VERSIONS "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/*")
        if(GCC_VERSIONS)
            list(SORT GCC_VERSIONS)
            list(REVERSE GCC_VERSIONS)
            get_filename_component(GCC_VERSION "${GCC_VERSIONS}" NAME)
            include_directories(SYSTEM "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/${GCC_VERSION}/include")
            if(EXISTS "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/${GCC_VERSION}/include-fixed")
                include_directories(SYSTEM "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/${GCC_VERSION}/include-fixed")
            endif()
        endif()
    endif()
endif()

# Find required packages
find_package(PkgConfig REQUIRED)
pkg_check_modules(GC REQUIRED bdw-gc)
pkg_check_modules(PCRE2 REQUIRED libpcre2-8)

# On Windows, prefer static linking for GC and PCRE2 to avoid DLL dependencies
if(WIN32)
    message(STATUS "Windows detected: attempting static linking for libgc and libpcre2")

    # Get library directory from pkg-config
    execute_process(
        COMMAND ${PKG_CONFIG_EXECUTABLE} --variable=libdir bdw-gc
        OUTPUT_VARIABLE GC_LIBDIR_RAW
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )

    # Normalize path (convert backslashes to forward slashes)
    string(REPLACE "\\" "/" GC_LIBDIR "${GC_LIBDIR_RAW}")

    # Check if static libraries exist
    set(GC_STATIC_LIB "${GC_LIBDIR}/libgc.a")
    set(PCRE2_STATIC_LIB "${GC_LIBDIR}/libpcre2-8.a")

    # Search for static libraries
    set(GC_STATIC_LIB_FOUND "")
    set(PCRE2_STATIC_LIB_FOUND "")

    # Helper function to normalize paths (prevents CMake from interpreting backslashes as escapes)
    function(normalize_path VAR PATH)
        string(REPLACE "\\" "/" NORMALIZED "${PATH}")
        set(${VAR} "${NORMALIZED}" PARENT_SCOPE)
    endfunction()

    # First try the pkg-config libdir
    if(GC_LIBDIR AND EXISTS "${GC_STATIC_LIB}" AND EXISTS "${PCRE2_STATIC_LIB}")
        normalize_path(GC_STATIC_LIB_FOUND "${GC_STATIC_LIB}")
        normalize_path(PCRE2_STATIC_LIB_FOUND "${PCRE2_STATIC_LIB}")
    else()
        # Try to find UCRT64_ROOT and check there
        if(DEFINED UCRT64_ROOT)
            set(UCRT64_LIBDIR "${UCRT64_ROOT}/lib")
            set(UCRT64_GC_STATIC "${UCRT64_LIBDIR}/libgc.a")
            set(UCRT64_PCRE2_STATIC "${UCRT64_LIBDIR}/libpcre2-8.a")
            if(EXISTS "${UCRT64_GC_STATIC}" AND EXISTS "${UCRT64_PCRE2_STATIC}")
                normalize_path(GC_STATIC_LIB_FOUND "${UCRT64_GC_STATIC}")
                normalize_path(PCRE2_STATIC_LIB_FOUND "${UCRT64_PCRE2_STATIC}")
            endif()
        endif()

        # If still not found, try common MSYS2 locations
        if(NOT GC_STATIC_LIB_FOUND)
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
                set(TEST_GC_STATIC "${LIBDIR}/libgc.a")
                set(TEST_PCRE2_STATIC "${LIBDIR}/libpcre2-8.a")
                if(EXISTS "${TEST_GC_STATIC}" AND EXISTS "${TEST_PCRE2_STATIC}")
                    normalize_path(GC_STATIC_LIB_FOUND "${TEST_GC_STATIC}")
                    normalize_path(PCRE2_STATIC_LIB_FOUND "${TEST_PCRE2_STATIC}")
                    break()
                endif()
            endforeach()
        endif()
    endif()

    if(GC_STATIC_LIB_FOUND)
        message(STATUS "Found static GC library:")
        message(STATUS "  GC: ${GC_STATIC_LIB_FOUND}")

        # Override pkg-config variables with explicit static library path
        # Note: GC needs -lpthread (from bdw-gc.pc Libs field)
        set(GC_LIBRARIES "${GC_STATIC_LIB_FOUND}" -lpthread)

        message(STATUS "Using explicit static GC library (avoiding .dll.a import library)")

        # Note: Explicit path to libpcre2-8.a causes "ld returned 5 exit status" (access violation)
        # This appears to be a compatibility issue with PCRE2's static library on Windows
        # Keeping PCRE2 dynamically linked
        message(STATUS "Keeping PCRE2 dynamically linked (libpcre2-8.a causes linker errors)")
        # PCRE2_LIBRARIES already set by pkg_check_modules, no override needed
    else()
        message(WARNING "Static GC library not found")
        message(WARNING "  Checked GC: ${GC_STATIC_LIB}")
        if(GC_LIBDIR)
            message(WARNING "  pkg-config libdir: ${GC_LIBDIR}")
        endif()
        message(WARNING "Falling back to dynamic linking - DLLs will be required at runtime")
    endif()
endif()

# Add library subdirectory
add_subdirectory(telnet-lisp)

# Telnet GUI executable
add_subdirectory(telnet-gui)

# REPL executable (default application)
add_executable(lisp-repl telnet-lisp/repl/main.c)
target_link_libraries(lisp-repl lisp ${GC_LIBRARIES} ${PCRE2_LIBRARIES} m)

# On Windows, copy required UCRT64 runtime DLLs to build directory and include in distribution
if(WIN32 AND UCRT64_ROOT)
    set(REQUIRED_DLLS
        "${UCRT64_ROOT}/bin/libpcre2-8-0.dll"
        "${UCRT64_ROOT}/bin/libwinpthread-1.dll"
    )

    # Copy DLLs to build directory for testing
    foreach(DLL_FILE ${REQUIRED_DLLS})
        if(EXISTS "${DLL_FILE}")
            get_filename_component(DLL_NAME "${DLL_FILE}" NAME)
            # Copy to root build directory for lisp-repl.exe
            add_custom_command(TARGET lisp-repl POST_BUILD
                COMMAND ${CMAKE_COMMAND} -E copy_if_different
                    "${DLL_FILE}"
                    "${CMAKE_BINARY_DIR}/${DLL_NAME}"
                COMMENT "Copying ${DLL_NAME} to build directory"
            )
            message(STATUS "Will bundle runtime DLL: ${DLL_NAME}")
        endif()
    endforeach()

    # Install DLLs alongside executables
    # Normalize paths for install() command to avoid escape character issues
    set(NORMALIZED_DLLS "")
    foreach(DLL_FILE ${REQUIRED_DLLS})
        if(EXISTS "${DLL_FILE}")
            string(REPLACE "\\" "/" NORMALIZED_DLL "${DLL_FILE}")
            list(APPEND NORMALIZED_DLLS "${NORMALIZED_DLL}")
        endif()
    endforeach()

    install(FILES ${NORMALIZED_DLLS}
        DESTINATION bin
        COMPONENT Runtime
    )
endif()

# Add include directories for REPL
target_include_directories(lisp-repl PRIVATE telnet-lisp/include)

# Add UCRT64 system include paths for REPL executable on Windows
if(WIN32 AND EXISTS "${UCRT64_ROOT}/include")
    target_include_directories(lisp-repl SYSTEM PRIVATE "${UCRT64_ROOT}/include")

    # Add GCC include paths if available
    file(GLOB GCC_VERSIONS "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/*")
    if(GCC_VERSIONS)
        list(SORT GCC_VERSIONS)
        list(REVERSE GCC_VERSIONS)
        get_filename_component(GCC_VERSION "${GCC_VERSIONS}" NAME)
        target_include_directories(lisp-repl SYSTEM PRIVATE
            "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/${GCC_VERSION}/include")
        if(EXISTS "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/${GCC_VERSION}/include-fixed")
            target_include_directories(lisp-repl SYSTEM PRIVATE
                "${UCRT64_ROOT}/lib/gcc/x86_64-w64-mingw32/${GCC_VERSION}/include-fixed")
        endif()
    endif()
endif()

# Installation
install(TARGETS lisp-repl lisp
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

# Install telnet-gui with Runtime component (includes bootstrap.lisp and assets)
install(TARGETS telnet-gui
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    COMPONENT Runtime
)

install(FILES telnet-lisp/include/lisp.h
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

# Install telnet-gui-clean-small-logs.sh script
install(PROGRAMS telnet-gui/scripts/telnet-gui-clean-small-logs.sh
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    RENAME "telnet-gui-clean-small-logs.sh"
)

# Testing
enable_testing()

# Find all test files and register them for assertion-based testing
file(GLOB_RECURSE TEST_FILES "telnet-lisp/tests/**/*.lisp")

foreach(TEST_FILE ${TEST_FILES})
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    # Generate a unique test name with category prefix
    get_filename_component(TEST_DIR ${TEST_FILE} DIRECTORY)
    get_filename_component(CATEGORY ${TEST_DIR} NAME)

    # All tests now use assertion-based format with test-helpers.lisp
    add_test(NAME ${CATEGORY}_${TEST_NAME}
        COMMAND $<TARGET_FILE:lisp-repl> ${TEST_FILE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/telnet-lisp
    )
endforeach()

# Register telnet-gui test files for direct execution (no test_runner.sh)
# Tests use assert-* macros that abort on failure via (error ...)
add_test(NAME gui_tintin-test
    COMMAND $<TARGET_FILE:lisp-repl> tintin-test.lisp
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/telnet-gui
)

add_test(NAME gui_completion-test
    COMMAND $<TARGET_FILE:lisp-repl> completion-test.lisp
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/telnet-gui
)

add_test(NAME gui_bootstrap-test
    COMMAND $<TARGET_FILE:telnet-gui> -t bootstrap-test.lisp
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/telnet-gui
)

# Package configuration
set(CPACK_PACKAGE_NAME "TelnetLisp")
set(CPACK_PACKAGE_VERSION "1.0.0")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Telnet Lisp Interpreter")
set(CPACK_PACKAGE_VENDOR "Telnet Lisp Project")

include(CPack)

# Format target for clang-format and shell scripts

# Find nvm/fnm node bin directories dynamically (for prettier)
set(NODE_VERSION_MANAGERS_BIN_PATHS "")
if(DEFINED ENV{HOME})
    # Find nvm node versions
    file(GLOB NVM_NODE_BINS "$ENV{HOME}/.nvm/versions/node/*/bin")
    list(APPEND NODE_VERSION_MANAGERS_BIN_PATHS ${NVM_NODE_BINS})
    # Find fnm node versions (stable location, not the volatile multishell paths)
    file(GLOB FNM_NODE_BINS "$ENV{HOME}/.local/share/fnm/node-versions/*/installation/bin")
    list(APPEND NODE_VERSION_MANAGERS_BIN_PATHS ${FNM_NODE_BINS})
    file(GLOB FNM_NODE_BINS_ALT "$ENV{HOME}/.fnm/node-versions/*/installation/bin")
    list(APPEND NODE_VERSION_MANAGERS_BIN_PATHS ${FNM_NODE_BINS_ALT})
endif()

find_program(CLANG_FORMAT
    NAMES clang-format
    PATHS
        ${UCRT64_ROOT}/bin
        /ucrt64/bin
    NO_DEFAULT_PATH
)
find_program(CLANG_FORMAT NAMES clang-format)

find_program(SHFMT
    NAMES shfmt
    PATHS
        ${UCRT64_ROOT}/bin
        /ucrt64/bin
    NO_DEFAULT_PATH
)
find_program(SHFMT NAMES shfmt)

find_program(EMACS
    NAMES emacs
    PATHS
        ${UCRT64_ROOT}/bin
        /usr/bin
        /usr/local/bin
        C:/Program Files/Emacs/emacs-*/bin
    NO_DEFAULT_PATH
)
find_program(EMACS NAMES emacs)

find_program(PRETTIER
    NAMES prettier
    PATHS
        ${UCRT64_ROOT}/bin
        /usr/bin
        /usr/local/bin
        /usr/local/share/npm/bin
        $ENV{APPDATA}/npm
        # Common GNU/Linux npm installation paths
        $ENV{HOME}/.npm-global/bin
        $ENV{HOME}/.local/bin
        $ENV{HOME}/node_modules/.bin
        $ENV{HOME}/.yarn/bin
        /usr/local/lib/node_modules/.bin
        # nvm/fnm node versions (dynamically discovered)
        ${NODE_VERSION_MANAGERS_BIN_PATHS}
    NO_DEFAULT_PATH
)

find_program(BLACK
    NAMES black
    PATHS
        ${UCRT64_ROOT}/bin
        /usr/bin
        /usr/local/bin
        $ENV{APPDATA}/Python*/Scripts
        $ENV{LOCALAPPDATA}/Programs/Python/Python*/Scripts
    NO_DEFAULT_PATH
)
find_program(BLACK NAMES black)

if(CLANG_FORMAT)
    # Collect all C source files
    file(GLOB_RECURSE LIB_SOURCES "telnet-lisp/src/*.c")
    file(GLOB_RECURSE REPL_SOURCES "telnet-lisp/repl/*.c")
    file(GLOB_RECURSE GUI_SOURCES "telnet-gui/src/*.c")

    # Collect all shell scripts
    file(GLOB SHELL_SCRIPTS
        "*.sh"
        "telnet-lisp/*.sh"
    )

    # Collect all Lisp source files (excluding build directory)
    file(GLOB_RECURSE LISP_SOURCES
        "telnet-gui/*.lisp"
        "telnet-lisp/*.lisp"
        "telnet-lisp/tests/**/*.lisp"
    )
    list(FILTER LISP_SOURCES EXCLUDE REGEX ".*/build/.*")

    # Collect all Markdown files (excluding build directory and node_modules)
    file(GLOB_RECURSE MARKDOWN_FILES
        "*.md"
        "telnet-gui/*.md"
        "telnet-lisp/*.md"
    )
    list(FILTER MARKDOWN_FILES EXCLUDE REGEX ".*/build/.*")
    list(FILTER MARKDOWN_FILES EXCLUDE REGEX ".*/node_modules/.*")

    # Collect all Python files (excluding build directory)
    file(GLOB_RECURSE PYTHON_FILES
        "*.py"
        "telnet-gui/*.py"
        "telnet-lisp/*.py"
    )
    list(FILTER PYTHON_FILES EXCLUDE REGEX ".*/build/.*")

    # Set up Black command if available
    if(BLACK)
        set(BLACK_COMMAND
            COMMAND ${CMAKE_COMMAND} -E echo ""
            COMMAND ${CMAKE_COMMAND} -E echo "Formatting Python files..."
            COMMAND ${BLACK} ${PYTHON_FILES}
        )
        set(BLACK_STATUS_MSG "black found: ${BLACK}")
    else()
        set(BLACK_COMMAND "")
        set(BLACK_STATUS_MSG "black not found - Python files will not be formatted")
    endif()

    # Format target for C sources
    if(SHFMT)
        if(EMACS)
            # Format C, shell scripts, and Lisp files
            # Write list of Lisp files to a temp file
            set(LISP_FILES_LIST "${CMAKE_BINARY_DIR}/lisp_files.txt")
            file(WRITE "${LISP_FILES_LIST}" "")
            foreach(LISP_FILE ${LISP_SOURCES})
                file(RELATIVE_PATH LISP_FILE_REL "${CMAKE_SOURCE_DIR}" "${LISP_FILE}")
                file(APPEND "${LISP_FILES_LIST}" "${LISP_FILE_REL}\n")
            endforeach()
            # Format all files in a single Emacs batch invocation (much faster than per-file)
            # Strip Windows line endings (\r) before xargs
            # Use relative path for bash compatibility (MSYS2/bash doesn't handle Windows paths well)
            file(RELATIVE_PATH LISP_FILES_LIST_REL "${CMAKE_SOURCE_DIR}" "${LISP_FILES_LIST}")
            set(LISP_FORMAT_COMMANDS
                COMMAND bash tools/format-all-lisp-files.sh ${LISP_SOURCES}
            )

            if(PRETTIER)
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${SHFMT} -w -i 2 ${SHELL_SCRIPTS}
                    COMMAND ${CMAKE_COMMAND} -E echo ""
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting Lisp files..."
                    ${LISP_FORMAT_COMMANDS}
                    COMMAND ${PRETTIER} --write ${MARKDOWN_FILES}
                    ${BLACK_COMMAND}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C, shell, Lisp, Markdown, and Python files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                    USES_TERMINAL
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "shfmt found: ${SHFMT}")
                message(STATUS "emacs found: ${EMACS}")
                message(STATUS "prettier found: ${PRETTIER}")
                message(STATUS "${BLACK_STATUS_MSG}")
            else()
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${SHFMT} -w -i 2 ${SHELL_SCRIPTS}
                    COMMAND ${CMAKE_COMMAND} -E echo ""
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting Lisp files..."
                    ${LISP_FORMAT_COMMANDS}
                    ${BLACK_COMMAND}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C, shell, Lisp, and Python files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                    USES_TERMINAL
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "shfmt found: ${SHFMT}")
                message(STATUS "emacs found: ${EMACS}")
                message(STATUS "${BLACK_STATUS_MSG}")
                message(WARNING "prettier not found - Markdown files will not be formatted")
            endif()
        else()
            # Format C and shell scripts only (no Emacs)
            if(PRETTIER)
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${SHFMT} -w -i 2 ${SHELL_SCRIPTS}
                    COMMAND ${PRETTIER} --write ${MARKDOWN_FILES}
                    ${BLACK_COMMAND}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C, shell, Markdown, and Python files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                    USES_TERMINAL
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "shfmt found: ${SHFMT}")
                message(STATUS "prettier found: ${PRETTIER}")
                message(STATUS "${BLACK_STATUS_MSG}")
                message(WARNING "emacs not found - Lisp files will not be formatted")
            else()
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${SHFMT} -w -i 2 ${SHELL_SCRIPTS}
                    ${BLACK_COMMAND}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C, shell, and Python files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                    USES_TERMINAL
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "shfmt found: ${SHFMT}")
                message(STATUS "${BLACK_STATUS_MSG}")
                message(WARNING "emacs not found - Lisp files will not be formatted")
                message(WARNING "prettier not found - Markdown files will not be formatted")
            endif()
        endif()
    else()
        if(EMACS)
            # Format C and Lisp files (no shfmt)
            # Write list of Lisp files to a temp file
            set(LISP_FILES_LIST "${CMAKE_BINARY_DIR}/lisp_files.txt")
            file(WRITE "${LISP_FILES_LIST}" "")
            foreach(LISP_FILE ${LISP_SOURCES})
                file(RELATIVE_PATH LISP_FILE_REL "${CMAKE_SOURCE_DIR}" "${LISP_FILE}")
                file(APPEND "${LISP_FILES_LIST}" "${LISP_FILE_REL}\n")
            endforeach()
            # Format all files in a single Emacs batch invocation (much faster than per-file)
            # Strip Windows line endings (\r) before xargs
            # Use relative path for bash compatibility (MSYS2/bash doesn't handle Windows paths well)
            file(RELATIVE_PATH LISP_FILES_LIST_REL "${CMAKE_SOURCE_DIR}" "${LISP_FILES_LIST}")
            set(LISP_FORMAT_COMMANDS
                COMMAND bash tools/format-all-lisp-files.sh ${LISP_SOURCES}
            )

            if(PRETTIER)
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${CMAKE_COMMAND} -E echo ""
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting Lisp files..."
                    ${LISP_FORMAT_COMMANDS}
                    COMMAND ${PRETTIER} --write ${MARKDOWN_FILES}
                    ${BLACK_COMMAND}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C, Lisp, Markdown, and Python files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                    USES_TERMINAL
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "emacs found: ${EMACS}")
                message(STATUS "prettier found: ${PRETTIER}")
                message(STATUS "${BLACK_STATUS_MSG}")
                message(WARNING "shfmt not found - shell scripts will not be formatted")
            else()
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${CMAKE_COMMAND} -E echo ""
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting Lisp files..."
                    ${LISP_FORMAT_COMMANDS}
                    ${BLACK_COMMAND}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C, Lisp, and Python files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                    USES_TERMINAL
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "emacs found: ${EMACS}")
                message(STATUS "${BLACK_STATUS_MSG}")
                message(WARNING "shfmt not found - shell scripts will not be formatted")
                message(WARNING "prettier not found - Markdown files will not be formatted")
            endif()
        else()
            # Format C sources only (no shfmt, no emacs)
            if(PRETTIER)
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${PRETTIER} --write ${MARKDOWN_FILES}
                    ${BLACK_COMMAND}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C, Markdown, and Python files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                    USES_TERMINAL
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "prettier found: ${PRETTIER}")
                message(STATUS "${BLACK_STATUS_MSG}")
                message(WARNING "shfmt not found - shell scripts will not be formatted")
                message(WARNING "emacs not found - Lisp files will not be formatted")
            else()
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    ${BLACK_COMMAND}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C and Python files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                    USES_TERMINAL
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "${BLACK_STATUS_MSG}")
                message(WARNING "shfmt not found - shell scripts will not be formatted")
                message(WARNING "emacs not found - Lisp files will not be formatted")
                message(WARNING "prettier not found - Markdown files will not be formatted")
            endif()
        endif()
    endif()

    message(STATUS "Use 'cmake --build . --target format' to format sources")
else()
    message(STATUS "clang-format not found - skipping format target")
    message(STATUS "Install clang-tools-extra package for formatting support")
endif()
