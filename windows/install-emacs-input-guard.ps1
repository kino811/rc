# install-emacs-input-guard.ps1
#
# Registers emacs-input-guard.ps1 (same folder) to start hidden at logon, and starts it now.
# Run once per Windows machine that uses terminal Emacs (emacs -nw) in WezTerm + psmux.
# Re-running is safe: the shortcut is overwritten and only one guard runs at a time.
#
# Usage:
#   powershell -NoProfile -ExecutionPolicy Bypass -File install-emacs-input-guard.ps1
#   powershell -NoProfile -ExecutionPolicy Bypass -File install-emacs-input-guard.ps1 -Uninstall

param([switch]$Uninstall)

$guard = Join-Path $PSScriptRoot "emacs-input-guard.ps1"
$lnk = Join-Path ([Environment]::GetFolderPath('Startup')) "emacs-input-guard.lnk"

if ($Uninstall) {
    if (Test-Path $lnk) { Remove-Item $lnk; Write-Host "Removed $lnk" }
    else { Write-Host "Not installed." }
    exit 0
}

if (-not (Test-Path $guard)) { Write-Error "Not found: $guard"; exit 1 }

$guardArgs = '-NoProfile -WindowStyle Hidden -ExecutionPolicy Bypass -File "{0}"' -f $guard
$shell = New-Object -ComObject WScript.Shell
$s = $shell.CreateShortcut($lnk)
$s.TargetPath = (Get-Command powershell.exe).Source
$s.Arguments = $guardArgs
$s.WindowStyle = 7   # minimized
$s.WorkingDirectory = $env:USERPROFILE
$s.Save()
Write-Host "Installed startup shortcut: $lnk"

# Start now; if a guard is already running it exits at once (named mutex).
Start-Process -FilePath (Get-Command powershell.exe).Source -ArgumentList $guardArgs -WindowStyle Hidden
Write-Host "Guard started (log: $(Join-Path $env:USERPROFILE '.emacs-input-guard.log'))."
