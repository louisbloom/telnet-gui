cmake_minimum_required(VERSION 3.10)
project(TelnetLisp VERSION 1.0.0 LANGUAGES C)

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
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libgcc")

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

    # Show UCRT64 detection result
    if(UCRT64_ROOT)
        message(STATUS "Found MSYS2 UCRT64 at: ${UCRT64_ROOT}")

        # Warn if running from wrong MSYS2 environment
        if(DEFINED ENV{MSYSTEM})
            if(NOT "$ENV{MSYSTEM}" STREQUAL "UCRT64")
                message(WARNING "")
                message(WARNING "You are running CMake from MSYS2 $ENV{MSYSTEM} environment!")
                message(WARNING "This project requires UCRT64 environment.")
                message(WARNING "")
                message(WARNING "Solution 1: Run from PowerShell/CMD instead (recommended)")
                message(WARNING "Solution 2: Open MSYS2 UCRT64 shell: C:\\msys64\\msys2_shell.cmd -ucrt64")
                message(WARNING "")
            endif()
        endif()
    else()
        message(WARNING "MSYS2 UCRT64 not found - some features may not be available")
        message(WARNING "Install MSYS2 UCRT64 packages: pacman -S mingw-w64-ucrt-x86_64-{gcc,cmake,bdw-gc,pcre2}")
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

# Add library subdirectory
add_subdirectory(telnet-lisp)

# Telnet GUI executable
add_subdirectory(telnet-gui)

# REPL executable (default application)
add_executable(lisp-repl telnet-lisp/repl/main.c)
target_link_libraries(lisp-repl lisp ${GC_LIBRARIES} ${PCRE2_LIBRARIES} m)

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
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

# Install telnet-gui with Runtime component (includes bootstrap.lisp and assets)
install(TARGETS telnet-gui
    RUNTIME DESTINATION bin
    COMPONENT Runtime
)

install(FILES telnet-lisp/include/lisp.h
    DESTINATION include
)

# Testing
enable_testing()

# Find all test files and add them with validation
file(GLOB_RECURSE TEST_FILES "telnet-lisp/tests/**/*.lisp")

foreach(TEST_FILE ${TEST_FILES})
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    # Generate a unique test name with category prefix
    get_filename_component(TEST_DIR ${TEST_FILE} DIRECTORY)
    get_filename_component(CATEGORY ${TEST_DIR} NAME)

    add_test(NAME ${CATEGORY}_${TEST_NAME}
        COMMAND bash ${CMAKE_SOURCE_DIR}/telnet-lisp/test_runner.sh ${TEST_FILE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/telnet-lisp
    )
endforeach()

# Find all telnet-gui test files matching *-test.lisp pattern
file(GLOB GUI_TEST_FILES "telnet-gui/*-test.lisp")

foreach(TEST_FILE ${GUI_TEST_FILES})
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    # Pass just the test filename - file is copied to build dir by POST_BUILD
    get_filename_component(TEST_FILE_NAME ${TEST_FILE} NAME)
    # Use telnet-lisp test_runner.sh with lisp-repl (found as ../lisp-repl.exe from telnet-gui build dir)
    add_test(NAME gui_${TEST_NAME}
        COMMAND bash ${CMAKE_SOURCE_DIR}/telnet-lisp/test_runner.sh ${TEST_FILE_NAME}
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/telnet-gui
    )
endforeach()

# Package configuration
set(CPACK_PACKAGE_NAME "TelnetLisp")
set(CPACK_PACKAGE_VERSION "1.0.0")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Telnet Lisp Interpreter")
set(CPACK_PACKAGE_VENDOR "Telnet Lisp Project")

include(CPack)

# Format target for clang-format and shell scripts
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
    NO_DEFAULT_PATH
)
find_program(PRETTIER NAMES prettier)

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

    # Format target for C sources
    if(SHFMT)
        if(EMACS)
            # Format C, shell scripts, and Lisp files
            # Build format commands for each Lisp file using wrapper script
            set(LISP_FORMAT_COMMANDS "")
            foreach(LISP_FILE ${LISP_SOURCES})
                list(APPEND LISP_FORMAT_COMMANDS
                    COMMAND bash ${CMAKE_SOURCE_DIR}/tools/format-lisp-file.sh ${LISP_FILE}
                )
            endforeach()

            if(PRETTIER)
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${SHFMT} -w -i 2 ${SHELL_SCRIPTS}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting Lisp files..."
                    ${LISP_FORMAT_COMMANDS}
                    COMMAND ${PRETTIER} --write ${MARKDOWN_FILES}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C, shell, Lisp, and Markdown files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "shfmt found: ${SHFMT}")
                message(STATUS "emacs found: ${EMACS}")
                message(STATUS "prettier found: ${PRETTIER}")
            else()
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${SHFMT} -w -i 2 ${SHELL_SCRIPTS}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting Lisp files..."
                    ${LISP_FORMAT_COMMANDS}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C, shell, and Lisp files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "shfmt found: ${SHFMT}")
                message(STATUS "emacs found: ${EMACS}")
                message(WARNING "prettier not found - Markdown files will not be formatted")
            endif()
        else()
            # Format C and shell scripts only (no Emacs)
            if(PRETTIER)
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${SHFMT} -w -i 2 ${SHELL_SCRIPTS}
                    COMMAND ${PRETTIER} --write ${MARKDOWN_FILES}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C, shell, and Markdown files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "shfmt found: ${SHFMT}")
                message(STATUS "prettier found: ${PRETTIER}")
                message(WARNING "emacs not found - Lisp files will not be formatted")
            else()
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${SHFMT} -w -i 2 ${SHELL_SCRIPTS}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C source files and shell scripts"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "shfmt found: ${SHFMT}")
                message(WARNING "emacs not found - Lisp files will not be formatted")
                message(WARNING "prettier not found - Markdown files will not be formatted")
            endif()
        endif()
    else()
        if(EMACS)
            # Format C and Lisp files (no shfmt)
            # Build format commands for each Lisp file using wrapper script
            set(LISP_FORMAT_COMMANDS "")
            foreach(LISP_FILE ${LISP_SOURCES})
                list(APPEND LISP_FORMAT_COMMANDS
                    COMMAND bash ${CMAKE_SOURCE_DIR}/tools/format-lisp-file.sh ${LISP_FILE}
                )
            endforeach()

            if(PRETTIER)
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting Lisp files..."
                    ${LISP_FORMAT_COMMANDS}
                    COMMAND ${PRETTIER} --write ${MARKDOWN_FILES}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C, Lisp, and Markdown files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "emacs found: ${EMACS}")
                message(STATUS "prettier found: ${PRETTIER}")
                message(WARNING "shfmt not found - shell scripts will not be formatted")
            else()
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting Lisp files..."
                    ${LISP_FORMAT_COMMANDS}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C and Lisp files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "emacs found: ${EMACS}")
                message(WARNING "shfmt not found - shell scripts will not be formatted")
                message(WARNING "prettier not found - Markdown files will not be formatted")
            endif()
        else()
            # Format C sources only (no shfmt, no emacs)
            if(PRETTIER)
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${PRETTIER} --write ${MARKDOWN_FILES}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C and Markdown files"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
                message(STATUS "prettier found: ${PRETTIER}")
                message(WARNING "shfmt not found - shell scripts will not be formatted")
                message(WARNING "emacs not found - Lisp files will not be formatted")
            else()
                add_custom_target(format
                    COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
                    COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
                    COMMENT "Formatting all C source files with clang-format"
                    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                )
                message(STATUS "clang-format found: ${CLANG_FORMAT}")
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
