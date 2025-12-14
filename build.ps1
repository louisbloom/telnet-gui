# Build script for PowerShell users
# Invokes UCRT64 shell to ensure correct environment

param(
    [string]$Target = "all",
    [switch]$Clean,
    [switch]$Install,
    [string]$Prefix = "$env:USERPROFILE\.local"
)

$msys2_path = if (Test-Path "$env:USERPROFILE\scoop\apps\msys2\current") {
    "$env:USERPROFILE\scoop\apps\msys2\current"
} else {
    "C:\msys64"
}

$shell_cmd = "$msys2_path\msys2_shell.cmd"

if ($Clean) {
    Write-Host "Cleaning build directory..."
    Remove-Item -Recurse -Force build -ErrorAction SilentlyContinue
}

if (-not (Test-Path "build")) {
    Write-Host "Configuring with CMake..."
    & $shell_cmd -defterm -here -no-start -ucrt64 -c "cmake -B build -G Ninja"
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}

Write-Host "Building target: $Target"
& $shell_cmd -defterm -here -no-start -ucrt64 -c "cmake --build build --target $Target"
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

if ($Install) {
    Write-Host "Installing to: $Prefix"
    & $shell_cmd -defterm -here -no-start -ucrt64 -c "cmake --install build --prefix $Prefix"
}
