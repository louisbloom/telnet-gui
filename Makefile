# Telnet LISP Interpreter Makefile
# Cross-platform build system

# Detect OS
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    OS := Linux
    LDFLAGS := -lm -lgc
    EXE_EXT :=
endif
ifeq ($(UNAME_S),Darwin)
    OS := macOS
    LDFLAGS := -lm -lgc
    EXE_EXT :=
endif
ifeq ($(OS),Windows_NT)
    OS := Windows
    LDFLAGS := -lm -lgc
    EXE_EXT := .exe
endif
ifeq ($(UNAME_S),MINGW32_NT-6.1)
    OS := Windows
    LDFLAGS := -lm -lgc
    EXE_EXT := .exe
endif
ifeq ($(UNAME_S),MINGW32_NT-6.2)
    OS := Windows
    LDFLAGS := -lm -lgc
    EXE_EXT := .exe
endif
ifeq ($(UNAME_S),MINGW32_NT-6.3)
    OS := Windows
    LDFLAGS := -lm -lgc
    EXE_EXT := .exe
endif
ifeq ($(UNAME_S),MINGW32_NT-10.0)
    OS := Windows
    LDFLAGS := -lm -lgc
    EXE_EXT := .exe
endif
ifeq ($(UNAME_S),CYGWIN_NT-6.1)
    OS := Windows
    LDFLAGS := -lm -lgc
    EXE_EXT := .exe
endif
ifeq ($(UNAME_S),CYGWIN_NT-6.2)
    OS := Windows
    LDFLAGS := -lm -lgc
    EXE_EXT := .exe
endif
ifeq ($(UNAME_S),CYGWIN_NT-6.3)
    OS := Windows
    LDFLAGS := -lm -lgc
    EXE_EXT := .exe
endif
ifeq ($(UNAME_S),CYGWIN_NT-10.0)
    OS := Windows
    LDFLAGS := -lm -lgc
    EXE_EXT := .exe
endif

# Default to Linux if not detected
ifeq ($(OS),)
    OS := Linux
    LDFLAGS := -lm -lgc
    EXE_EXT :=
endif

CC = gcc
AR = ar
CFLAGS = -Wall -Wextra -std=c99 -pedantic -O2 -Iinclude

# Directories
SRC_DIR = src
REPL_DIR = repl
INCLUDE_DIR = include
BUILD_DIR = build

# Library sources and objects
LIB_SOURCES = $(SRC_DIR)/lisp.c $(SRC_DIR)/reader.c $(SRC_DIR)/eval.c \
              $(SRC_DIR)/env.c $(SRC_DIR)/builtins.c $(SRC_DIR)/print.c
LIB_OBJECTS = $(BUILD_DIR)/lisp.o $(BUILD_DIR)/reader.o $(BUILD_DIR)/eval.o \
              $(BUILD_DIR)/env.o $(BUILD_DIR)/builtins.o $(BUILD_DIR)/print.o

# REPL sources and objects
REPL_SOURCES = $(REPL_DIR)/main.c
REPL_OBJECTS = $(BUILD_DIR)/main.o

# Targets
LIBRARY = $(BUILD_DIR)/liblisp.a
REPL_EXEC = lisp-repl$(EXE_EXT)

.PHONY: all clean test info

all: $(BUILD_DIR) $(LIBRARY) $(REPL_EXEC)
	@echo "Build complete for $(OS)!"
	@echo "Executable: $(REPL_EXEC)"
	@echo "Library: $(LIBRARY)"

# Create build directory
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Build library
$(LIBRARY): $(LIB_OBJECTS)
	$(AR) rcs $@ $^

# Compile library sources
$(BUILD_DIR)/lisp.o: $(SRC_DIR)/lisp.c $(INCLUDE_DIR)/lisp.h
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/reader.o: $(SRC_DIR)/reader.c $(INCLUDE_DIR)/lisp.h
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/eval.o: $(SRC_DIR)/eval.c $(INCLUDE_DIR)/lisp.h
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/env.o: $(SRC_DIR)/env.c $(INCLUDE_DIR)/lisp.h
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/builtins.o: $(SRC_DIR)/builtins.c $(INCLUDE_DIR)/lisp.h
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/print.o: $(SRC_DIR)/print.c $(INCLUDE_DIR)/lisp.h
	$(CC) $(CFLAGS) -c $< -o $@

# Compile REPL
$(BUILD_DIR)/main.o: $(REPL_DIR)/main.c $(INCLUDE_DIR)/lisp.h
	$(CC) $(CFLAGS) -c $< -o $@

# Link REPL executable
$(REPL_EXEC): $(REPL_OBJECTS) $(LIBRARY)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

# Clean build artifacts
clean:
	rm -rf $(BUILD_DIR) lisp-repl$(EXE_EXT)

# Show platform information
info:
	@echo "Platform: $(OS)"
	@echo "Compiler: $(CC)"
	@echo "Flags: $(CFLAGS)"
	@echo "Linker flags: $(LDFLAGS)"
	@echo "Executable extension: $(EXE_EXT)"

# Test target (run some basic tests)
test: $(REPL_EXEC)
	@echo "Running basic tests..."
	@echo "(+ 1 2 3)" | ./$(REPL_EXEC)
	@echo "(define x 42)" | ./$(REPL_EXEC)
	@echo "Tests complete!"

# Install target (copy to system directories)
install: $(REPL_EXEC) $(LIBRARY)
	@echo "Installing for $(OS)..."
	@mkdir -p /usr/local/bin
	@mkdir -p /usr/local/lib
	@mkdir -p /usr/local/include
	@cp $(REPL_EXEC) /usr/local/bin/
	@cp $(LIBRARY) /usr/local/lib/
	@cp $(INCLUDE_DIR)/lisp.h /usr/local/include/
	@echo "Installation complete!"

# Uninstall target
uninstall:
	@echo "Uninstalling..."
	@rm -f /usr/local/bin/lisp-repl$(EXE_EXT)
	@rm -f /usr/local/lib/liblisp.a
	@rm -f /usr/local/include/lisp.h
	@echo "Uninstallation complete!"
