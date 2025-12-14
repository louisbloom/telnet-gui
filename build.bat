@echo off
REM Build script for CMD users
REM Invokes UCRT64 shell to ensure correct environment

setlocal

set TARGET=%1
if "%TARGET%"=="" set TARGET=all

REM Detect MSYS2 location
if exist "%USERPROFILE%\scoop\apps\msys2\current" (
    set MSYS2_PATH=%USERPROFILE%\scoop\apps\msys2\current
) else (
    set MSYS2_PATH=C:\msys64
)

set SHELL_CMD=%MSYS2_PATH%\msys2_shell.cmd

if not exist build (
    echo Configuring with CMake...
    "%SHELL_CMD%" -defterm -here -no-start -ucrt64 -c "cmake -B build -G Ninja"
    if errorlevel 1 exit /b 1
)

echo Building target: %TARGET%
"%SHELL_CMD%" -defterm -here -no-start -ucrt64 -c "cmake --build build --target %TARGET%"
if errorlevel 1 exit /b 1

endlocal
