# Wrapper so that typing `yazi` in PowerShell can move the shell to the folder yazi
# was last in. A child process cannot change its parent's directory, so yazi writes the
# folder to a temp file (--cwd-file) and this script, running in the shell's own
# session, does the Set-Location. Which keys write the file is set in keymap.toml.
$exe = (Get-Command yazi.exe -CommandType Application -ErrorAction Stop | Select-Object -First 1).Source
$tmp = (New-TemporaryFile).FullName
try {
    & $exe @args --cwd-file="$tmp"
    $cwd = Get-Content -Path $tmp -Encoding UTF8 -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($cwd -and ($cwd -ne $PWD.Path) -and (Test-Path -LiteralPath $cwd -PathType Container)) {
        Set-Location -LiteralPath $cwd
    }
}
finally {
    Remove-Item -LiteralPath $tmp -ErrorAction SilentlyContinue
}
