# emacs-input-guard.ps1
#
# Keeps terminal Emacs (emacs -nw) on Windows from freezing.
#
# Problem: in a WezTerm + psmux pane, a few seconds after Emacs starts, the pane's console
# input mode gets ENABLE_VIRTUAL_TERMINAL_INPUT (0x200) added (0xB8 -> 0x2B8).  Emacs then
# stops processing keys until the flag is cleared again.  The cause is not known yet.
# Ruled out (2026-11-20 tests): child processes of Emacs (git, rg, cmd, pwsh, MSYS2 tools),
# psmux CLI commands run inside the pane, and psmux input injection (send-keys,
# paste-buffer, resize).  psmux itself turns VT input on for its own client console and
# re-enables it when cleared, so psmux is the main suspect.
#
# This script clears that one flag on console-attached emacs.exe, as soon as it shows up.
# Terminal Emacs never needs it, and all other mode bits are left alone.  GUI Emacs has no
# console and is skipped.  Each fix is logged with diagnostics (processes attached to the
# console, Emacs uptime) to help find the cause.  Other input mode changes are only logged.
#
# Usage:
#   powershell -NoProfile -File emacs-input-guard.ps1 -TargetPid <emacs pid>
#       Watch one Emacs and exit when it exits.  myInit.org starts this for every
#       Windows terminal Emacs, so nothing has to be registered to run at logon.
#   powershell -NoProfile -File emacs-input-guard.ps1            # watch every emacs.exe
#   powershell -NoProfile -File emacs-input-guard.ps1 -Once      # fix once now and exit
#   powershell -NoProfile -File emacs-input-guard.ps1 -IntervalMs 500
#
# Only one watching instance runs per target (named mutex).  Log: ~/.emacs-input-guard.log.

param(
    [int]$IntervalMs = 1000,
    [int]$TargetPid = 0,
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
  [DllImport("kernel32.dll")] public static extern uint GetConsoleProcessList(uint[] list, uint count);
}
"@

$VtInput = [uint32]0x200
$GenericReadWrite = [uint32]3221225472   # GENERIC_READ | GENERIC_WRITE
$ShareReadWrite = [uint32]3
$OpenExisting = [uint32]3

# Reads (and repairs) the input mode of the console that $EmacsPid is attached to.
# Returns $null for GUI Emacs (no console), otherwise an object with the mode seen, whether
# the VT input flag was cleared, and the ids of the processes attached to that console.
function Repair-EmacsConsole([int]$EmacsPid) {
    [GuardCon]::FreeConsole() | Out-Null
    if (-not [GuardCon]::AttachConsole([uint32]$EmacsPid)) { return $null }
    $result = $null
    $h = [GuardCon]::CreateFile("CONIN$", $GenericReadWrite, $ShareReadWrite, [IntPtr]::Zero, $OpenExisting, [uint32]0, [IntPtr]::Zero)
    if ($h -ne [IntPtr](-1)) {
        [uint32]$mode = 0
        if ([GuardCon]::GetConsoleMode($h, [ref]$mode)) {
            $fixed = $false
            if ($mode -band $VtInput) {
                $fixed = [GuardCon]::SetConsoleMode($h, ($mode -band (-bnot $VtInput)))
            }
            $list = New-Object uint32[] 64
            $n = [GuardCon]::GetConsoleProcessList($list, 64)
            $ids = @()
            if ($n -gt 0 -and $n -le 64) { $ids = @($list[0..($n - 1)] | Where-Object { $_ -ne $PID }) }
            # Names are taken right away: the process that changed the mode may be gone soon.
            $names = @{}
            foreach ($id in $ids) {
                $proc = Get-Process -Id $id -ErrorAction SilentlyContinue
                $names[[uint32]$id] = if ($proc) { $proc.ProcessName } else { '?' }
            }
            $result = [pscustomobject]@{ Mode = $mode; Fixed = $fixed; ConsolePids = $ids; Names = $names }
        }
        [GuardCon]::CloseHandle($h) | Out-Null
    }
    [GuardCon]::FreeConsole() | Out-Null
    return $result
}

function Get-ProcessSummary($Result) {
    $parts = foreach ($id in $Result.ConsolePids) {
        $w = Get-CimInstance Win32_Process -Filter "ProcessId=$id" -ErrorAction SilentlyContinue
        if ($w) {
            $cmd = "$($w.CommandLine)"
            if ($cmd.Length -gt 120) { $cmd = $cmd.Substring(0, 120) + '...' }
            "{0}({1}, parent {2}) {3}" -f $w.Name, $id, $w.ParentProcessId, $cmd
        } else { "{0}({1}, exited)" -f $Result.Names[[uint32]$id], $id }
    }
    return ($parts -join ' | ')
}

function Get-Uptime([int]$Id) {
    $p = Get-Process -Id $Id -ErrorAction SilentlyContinue
    if ($p) { return "{0:N0}s" -f ((Get-Date) - $p.StartTime).TotalSeconds }
    return "?"
}

function Write-Log([string]$Text) {
    Add-Content -Path $LogPath -Value ("{0:yyyy-MM-dd HH:mm:ss.fff} {1}" -f (Get-Date), $Text)
}

$messages = @()
$lastMode = @{}
if (-not $Once) {
    $created = $false
    $name = if ($TargetPid -gt 0) { "Local\emacs-input-guard-$TargetPid" } else { "Local\emacs-input-guard" }
    $mutex = New-Object System.Threading.Mutex($true, $name, [ref]$created)
    if (-not $created) { exit 0 }   # a guard for the same target is already running
    if ($TargetPid -gt 0) { Write-Log "guard started for emacs pid=$TargetPid (interval $IntervalMs ms, pid $PID)" }
    else { Write-Log "guard started for all emacs (interval $IntervalMs ms, pid $PID)" }
}

do {
    if ($TargetPid -gt 0) {
        $targets = @(Get-Process -Id $TargetPid -ErrorAction SilentlyContinue)
        if ($targets.Count -eq 0) {
            if (-not $Once) { Write-Log "emacs pid=$TargetPid exited; guard stops" }
            break
        }
    } else {
        $targets = @(Get-Process emacs -ErrorAction SilentlyContinue)
    }
    foreach ($p in $targets) {
        $r = Repair-EmacsConsole $p.Id
        if ($null -eq $r) { continue }   # GUI Emacs
        $prev = $lastMode[$p.Id]
        if ($r.Fixed) {
            $text = "emacs pid={0} input mode 0x{1:X} -> 0x{2:X} (cleared VT input flag; uptime {3}; console: {4})" -f `
                $p.Id, $r.Mode, ($r.Mode -band (-bnot $VtInput)), (Get-Uptime $p.Id), (Get-ProcessSummary $r)
            Write-Log $text
            $messages += $text
            $lastMode[$p.Id] = $r.Mode -band (-bnot $VtInput)
        } elseif ($null -eq $prev) {
            $lastMode[$p.Id] = $r.Mode
            if ($TargetPid -gt 0 -and -not $Once) { Write-Log ("emacs pid={0} initial input mode 0x{1:X}" -f $p.Id, $r.Mode) }
        } elseif ($prev -ne $r.Mode) {
            # Not repaired: only record it (e.g. line input turned on by another process).
            Write-Log ("emacs pid={0} input mode changed 0x{1:X} -> 0x{2:X} (logged only; uptime {3}; console: {4})" -f `
                $p.Id, $prev, $r.Mode, (Get-Uptime $p.Id), (Get-ProcessSummary $r))
            $lastMode[$p.Id] = $r.Mode
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
