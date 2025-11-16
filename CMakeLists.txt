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
target_link_libraries(lisp-repl liblisp ${GC_LIBRARIES} ${PCRE2_LIBRARIES} m)

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
install(TARGETS lisp-repl liblisp
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
        /ucrt64/bin
        C:/msys64/ucrt64/bin
        C:/tools/msys64/ucrt64/bin
    NO_DEFAULT_PATH
)
find_program(CLANG_FORMAT NAMES clang-format)

find_program(SHFMT NAMES shfmt)

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

    # Format target for C sources
    if(SHFMT)
        # Format both C and shell scripts
        add_custom_target(format
            COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
            COMMAND ${SHFMT} -w -i 2 ${SHELL_SCRIPTS}
            COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete!"
            COMMENT "Formatting all C source files and shell scripts"
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        )
        message(STATUS "clang-format found: ${CLANG_FORMAT}")
        message(STATUS "shfmt found: ${SHFMT}")
    else()
        # Format C sources only
        add_custom_target(format
            COMMAND ${CLANG_FORMAT} -i ${LIB_SOURCES} ${REPL_SOURCES} ${GUI_SOURCES}
            COMMAND ${CMAKE_COMMAND} -E echo "Formatting complete! (shell scripts skipped - shfmt not found)"
            COMMENT "Formatting all C source files with clang-format"
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        )
        message(STATUS "clang-format found: ${CLANG_FORMAT}")
        message(STATUS "shfmt not found - shell scripts will not be formatted")
    endif()

    message(STATUS "Use 'cmake --build . --target format' to format sources")
else()
    message(STATUS "clang-format not found - skipping format target")
    message(STATUS "Install clang-tools-extra package for formatting support")
endif()
