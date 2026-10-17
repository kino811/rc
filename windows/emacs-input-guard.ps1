# emacs-input-guard.ps1
#
# Keeps terminal Emacs (emacs -nw) on Windows from freezing.
#
# Problem: in a WezTerm + psmux pane, a few seconds after Emacs starts, the pane's console
# input mode gets ENABLE_VIRTUAL_TERMINAL_INPUT (0x200) added (0xB8 -> 0x2B8).  Emacs then
# stops processing keys until the flag is cleared again.  The cause is not known yet
# (root cause still under investigation).
#
# This script clears that one flag on every console-attached emacs.exe, as soon as it shows
# up.  Terminal Emacs never needs it, and all other mode bits are left alone.  GUI Emacs has
# no console and is skipped.
#
# Usage:
#   powershell -NoProfile -File emacs-input-guard.ps1            # keep watching (default 1 s)
#   powershell -NoProfile -File emacs-input-guard.ps1 -Once      # fix once now and exit
#   powershell -NoProfile -File emacs-input-guard.ps1 -IntervalMs 500
#
# Only one watching instance runs at a time (named mutex).  Fixes are logged to
# ~/.emacs-input-guard.log; nothing is logged while everything is healthy.

param(
    [int]$IntervalMs = 1000,
    [switch]$Once,
    [string]$LogPath = (Join-Path $env:USERPROFILE ".emacs-input-guard.log")
)

Add-Type -TypeDefinition @"
using System;
using System.Runtime.InteropServices;
public static class GuardCon {
  [DllImport("kernel32.dll", SetLastError=true)] public static extern bool FreeConsole();
  [DllImport("kernel32.dll", SetLastError=true)] public static extern bool AttachConsole(uint pid);
  [DllImport("kernel32.dll", SetLastError=true, CharSet=CharSet.Unicode)] public static extern IntPtr CreateFile(string n, uint a, uint s, IntPtr sa, uint d, uint f, IntPtr t);
  [DllImport("kernel32.dll", SetLastError=true)] public static extern bool GetConsoleMode(IntPtr h, out uint m);
  [DllImport("kernel32.dll", SetLastError=true)] public static extern bool SetConsoleMode(IntPtr h, uint m);
  [DllImport("kernel32.dll")] public static extern bool CloseHandle(IntPtr h);
}
"@

$VtInput = [uint32]0x200
$GenericReadWrite = [uint32]3221225472   # GENERIC_READ | GENERIC_WRITE
$ShareReadWrite = [uint32]3
$OpenExisting = [uint32]3

# Returns the old mode when the flag was set and has been cleared, otherwise $null.
function Repair-EmacsConsole([int]$TargetPid) {
    [GuardCon]::FreeConsole() | Out-Null
    if (-not [GuardCon]::AttachConsole([uint32]$TargetPid)) { return $null }   # GUI Emacs
    $fixed = $null
    $h = [GuardCon]::CreateFile("CONIN$", $GenericReadWrite, $ShareReadWrite, [IntPtr]::Zero, $OpenExisting, [uint32]0, [IntPtr]::Zero)
    if ($h -ne [IntPtr](-1)) {
        [uint32]$mode = 0
        if ([GuardCon]::GetConsoleMode($h, [ref]$mode) -and ($mode -band $VtInput)) {
            if ([GuardCon]::SetConsoleMode($h, ($mode -band (-bnot $VtInput)))) { $fixed = $mode }
        }
        [GuardCon]::CloseHandle($h) | Out-Null
    }
    [GuardCon]::FreeConsole() | Out-Null
    return $fixed
}

function Write-Log([string]$Text) {
    Add-Content -Path $LogPath -Value ("{0:yyyy-MM-dd HH:mm:ss} {1}" -f (Get-Date), $Text)
}

$messages = @()
if (-not $Once) {
    $created = $false
    $mutex = New-Object System.Threading.Mutex($true, "Local\emacs-input-guard", [ref]$created)
    if (-not $created) { exit 0 }   # another guard is already running
    Write-Log "guard started (interval $IntervalMs ms, pid $PID)"
}

do {
    foreach ($p in @(Get-Process emacs -ErrorAction SilentlyContinue)) {
        $old = Repair-EmacsConsole $p.Id
        if ($null -ne $old) {
            $text = "emacs pid={0} input mode 0x{1:X} -> 0x{2:X} (cleared VT input flag)" -f $p.Id, $old, ($old -band (-bnot $VtInput))
            Write-Log $text
            $messages += $text
        }
    }
    if (-not $Once) { Start-Sleep -Milliseconds $IntervalMs }
} while (-not $Once)

if ($Once) {
    # Repair-EmacsConsole detached this process from its console; reattach to the parent
    # console so the result can be printed.
    [GuardCon]::AttachConsole([uint32]4294967295) | Out-Null
    if ($messages.Count -eq 0) { Write-Host "No frozen terminal Emacs found." }
    else { $messages | ForEach-Object { Write-Host $_ } }
}
