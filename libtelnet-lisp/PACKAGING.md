# Packaging Telnet Lisp for Use in Other Projects

This library can be used independently in other projects. Here's how to package and integrate it.

## Option 1: Copy into Your Project

The simplest approach is to copy the `libtelnet-lisp/` directory into your project:

```bash
# From your project root
cp -r /path/to/telnet-lisp/libtelnet-lisp ./libs/
```

Then in your CMakeLists.txt:

```cmake
cmake_minimum_required(VERSION 3.10)
project(YourProject LANGUAGES C)

# Add libtelnet-lisp
add_subdirectory(libs/libtelnet-lisp)

# Your application
add_executable(myapp main.c)
target_link_libraries(myapp liblisp)

# That's it! The library automatically configures dependencies
```

## Option 2: Use as External Project

```cmake
cmake_minimum_required(VERSION 3.10)
project(YourProject LANGUAGES C)

include(ExternalProject)

ExternalProject_Add(libtelnet_lisp
    SOURCE_DIR /path/to/telnet-lisp/libtelnet-lisp
    BINARY_DIR ${CMAKE_BINARY_DIR}/external/libtelnet_lisp
    CMAKE_ARGS
        -DCMAKE_BUILD_TYPE=Release
    INSTALL_COMMAND ""
)

# Use the library
add_executable(myapp main.c)
target_link_libraries(myapp ${CMAKE_BINARY_DIR}/external/libtelnet_lisp/liblisp.a)
```

## Option 3: Standalone Build and Install

Build and install the library system-wide:

```bash
# In libtelnet-lisp directory
mkdir build && cd build
cmake ..
cmake --build . -j
sudo cmake --install .
```

Then in your project:

```cmake
find_package(TelnetLisp REQUIRED)
target_link_libraries(myapp liblisp)
```

## Dependencies

The library requires:

- **libgc** (Boehm GC) - Memory management
- **libpcre2-8** - Regex support

### Installing Dependencies

**Linux:**

```bash
sudo apt-get install libgc-dev libpcre2-8-dev  # Debian/Ubuntu
sudo dnf install gc-devel pcre2-devel          # Fedora
```

**macOS:**

```bash
brew install bdw-gc pcre2
```

**Windows (MSYS2 UCRT64):**

```bash
pacman -S mingw-w64-ucrt-x86_64-gc mingw-w64-ucrt-x86_64-pcre2
```

## Using the Library in C Code

```c
#include "lisp.h"  // If embedded, or
// #include <lisp.h>  // If installed system-wide

int main() {
    // Initialize the interpreter (includes GC)
    lisp_init();

    // Create environment
    Environment *env = env_create_global();

    // Evaluate Lisp code
    LispObject *result = lisp_eval_string("(+ 1 2 3)", env);
    char *output = lisp_print(result);
    printf("Result: %s\n", output);

    // Cleanup (GC handles most memory)
    env_free(env);
    lisp_cleanup();

    return 0;
}
```

## Library API

See `README.md` for complete API documentation.

Key functions:

- `lisp_init()` - Initialize interpreter
- `lisp_eval_string(const char* code, Environment* env)` - Evaluate Lisp code
- `lisp_load_file(const char* filename, Environment* env)` - Load and evaluate file
- `env_create_global()` - Create environment with built-ins
- `lisp_cleanup()` - Clean up resources

## Building as Standalone

To build the library and REPL independently:

```bash
# In libtelnet-lisp directory
mkdir build && cd build
cmake ..
cmake --build .
./lisp-repl  # Test the REPL
```

## License

See the main project README for license information.
