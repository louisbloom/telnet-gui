@echo off
REM Format Lisp file using Emacs in batch mode (Windows)
REM Reads from stdin, writes to stdout

set TEMP_FILE=%TEMP%\lisp-format-%RANDOM%.lisp

REM Read from stdin and write to temp file
more > "%TEMP_FILE%"

REM Get full path using PowerShell to expand short names
for /f "delims=" %%F in ('powershell -Command "[System.IO.Path]::GetFullPath('%TEMP_FILE%')"') do set TEMP_FILE_FULL=%%F

REM If PowerShell failed, try for loop expansion
if "%TEMP_FILE_FULL%"=="" (
    for %%F in ("%TEMP_FILE%") do set TEMP_FILE_FULL=%%~fF
)

REM Fallback to original if still empty
if "%TEMP_FILE_FULL%"=="" set TEMP_FILE_FULL=%TEMP_FILE%

REM Convert Windows path to forward slashes for Emacs
REM Keep drive letter as-is (C:/Users/...)
set TEMP_FILE_FORWARD=%TEMP_FILE_FULL:\=/%

REM Verify file exists before calling Emacs
if not exist "%TEMP_FILE%" (
    echo Error: Temporary file not created >&2
    exit /b 1
)

REM Use Emacs in batch mode to indent the entire buffer
REM --quick: Skip loading user config (faster, avoids hooks)
REM find-file-literally: Avoid auto-mode hooks and processing
REM Disable hooks that might cause hangs
emacs --batch --quick ^
    --eval "(setq inhibit-startup-message t)" ^
    --eval "(setq inhibit-startup-echo-area-message t)" ^
    --eval "(setq enable-local-variables nil)" ^
    --eval "(setq enable-local-eval nil)" ^
    --eval "(setq indent-tabs-mode nil)" ^
    --eval "(setq lisp-indent-offset 2)" ^
    --eval "(setq lisp-body-indent 2)" ^
    --eval "(find-file-literally \"%TEMP_FILE_FORWARD%\")" ^
    --eval "(lisp-mode)" ^
    --eval "(untabify (point-min) (point-max))" ^
    --eval "(indent-region (point-min) (point-max))" ^
    --eval "(save-buffer)" ^
    --eval "(kill-buffer)" ^
    --eval "(kill-emacs)" 2>nul

REM Output the formatted file
type "%TEMP_FILE%"

REM Clean up
del "%TEMP_FILE%" 2>nul
