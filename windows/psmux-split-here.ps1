# psmux-split-here.ps1 - split a psmux pane in the working directory of what runs in it.
#
# Bound to prefix % / " in .tmux.conf (run-shell, without -b):
#   -PaneId   #{pane_id}            pane to split
#   -PanePid  #{pane_pid}           the pane's shell process
#   -Fallback #{pane_current_path}  directory psmux reports (used when nothing better is found)
#   -Dir      h | v                 split-window -h (side by side) or -v (stacked)
#
# Directory choice:
#   1. Claude Code runs in the pane (deepest claude.exe under the pane shell):
#      - plain `claude`: its own session file ~/.claude/sessions/<pid>.json (follows /cd).
#      - `claude agents` (no session file of its own): it does not expose which session it
#        shows, so use the session the last prompt was sent to
#        (~/.claude/noty/state/<sessionId>.prompt, written by the noty plugin on every prompt)
#        and take that session's cwd.
#   2. Otherwise #{pane_current_path} (psmux keeps it in sync with PowerShell cd).
param(
    [Parameter(Mandatory)] [string] $PaneId,
    [Parameter(Mandatory)] [int] $PanePid,
    [string] $Fallback = '',
    [ValidateSet('h', 'v')] [string] $Dir = 'h',
    [switch] $PrintOnly   # print the chosen directory instead of splitting (for testing)
)

$claudeDir = Join-Path $HOME '.claude'

function Get-SessionCwd([string] $SessionId) {
    foreach ($f in Get-ChildItem (Join-Path $claudeDir 'sessions') -Filter *.json -ErrorAction SilentlyContinue) {
        try { $s = Get-Content -LiteralPath $f.FullName -Raw | ConvertFrom-Json } catch { continue }
        if ($s.sessionId -eq $SessionId -and $s.cwd) { return $s.cwd }
    }
    $state = Join-Path $claudeDir ('jobs/{0}/state.json' -f $SessionId.Substring(0, 8))
    if (Test-Path -LiteralPath $state) {
        try { return (Get-Content -LiteralPath $state -Raw | ConvertFrom-Json).cwd } catch {}
    }
    return $null
}

function Get-ClaudeCwd {
    # Get-Process + .Parent only: reading CommandLine (WMI) would add ~0.7 s per split.
    $best = $null
    foreach ($c in Get-Process -Name claude -ErrorAction SilentlyContinue) {
        $p = $c
        for ($depth = 0; $p -and $depth -lt 32; $depth++) {
            if ($p.Id -eq $PanePid) {
                if (-not $best -or $depth -gt $best.Depth) { $best = @{ Id = $c.Id; Depth = $depth } }
                break
            }
            $p = $p.Parent
        }
    }
    if (-not $best) { return $null }

    # A plain `claude` session has its own session file; a `claude agents` client has none.
    $own = Join-Path $claudeDir ('sessions/{0}.json' -f $best.Id)
    if (Test-Path -LiteralPath $own) {
        try { return (Get-Content -LiteralPath $own -Raw | ConvertFrom-Json).cwd } catch {}
        return $null
    }
    $last = Get-ChildItem (Join-Path $claudeDir 'noty/state') -Filter *.prompt -ErrorAction SilentlyContinue |
        Sort-Object LastWriteTime -Descending | Select-Object -First 1
    if ($last) { return Get-SessionCwd $last.BaseName }
    return $null
}

$cwd = $null
try { $cwd = Get-ClaudeCwd } catch {}
if (-not $cwd -or -not (Test-Path -LiteralPath $cwd -PathType Container)) { $cwd = $Fallback }
if ($PrintOnly) { return $cwd }

$splitArgs = @('split-window', "-$Dir", '-t', $PaneId)
if ($cwd) { $splitArgs += @('-c', $cwd) }
& psmux @splitArgs
