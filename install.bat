@echo off
REM install.bat - Windows installation script for Telnet Lisp

echo Telnet Lisp Interpreter - Installation Script
echo ============================================

REM Detect if we're in MSYS2
if "%MSYSTEM%"=="UCRT64" (
    echo Detected OS: Windows MSYS2 UCRT64
    set OS=Windows
) else if "%MSYSTEM%"=="MINGW64" (
    echo Detected OS: Windows MSYS2 MINGW64
    set OS=Windows
) else (
    echo Detected OS: Windows
    set OS=Windows
)

echo Checking dependencies...

REM Check for GCC
where gcc >nul 2>&1
if %errorlevel% neq 0 (
    echo Error: GCC compiler not found. Please install GCC.
    exit /b 1
)

REM Check for Make
where make >nul 2>&1
if %errorlevel% neq 0 (
    echo Error: Make not found. Please install Make.
    exit /b 1
)

REM Check for Boehm GC
pkg-config --exists libgc >nul 2>&1
if %errorlevel% neq 0 (
    echo Warning: Boehm GC not found via pkg-config.
    echo Please install Boehm GC:
    echo   MSYS2: pacman -S mingw-w64-ucrt-x86_64-gc
    exit /b 1
)

echo Dependencies OK!

REM Build
echo Building Telnet Lisp...
make clean
make

if %errorlevel% equ 0 (
    echo Build successful!
) else (
    echo Build failed!
    exit /b 1
)

REM Test
echo Testing...
echo (+ 1 2 3) | lisp-repl.exe

if %errorlevel% equ 0 (
    echo.
    echo Installation complete!
    echo You can now run 'lisp-repl.exe' from this directory.
    echo.
    echo Test it with:
    echo   lisp-repl.exe
    echo   echo ^(+ 1 2 3^) ^| lisp-repl.exe
) else (
    echo Test failed!
    exit /b 1
)
