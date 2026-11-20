# install-yazi.ps1 - set up the yazi config from this repo on a Windows machine.
# Run once per machine (safe to run again):
#   pwsh -File windows\install-yazi.ps1
#
# What it does:
#   1. Links %APPDATA%\yazi\config to .config\yazi in this repo (a directory junction,
#      which needs no admin rights). An existing config folder is renamed to config.bak-<time>.
#   2. Writes small forwarder scripts that call the real scripts in windows\bin:
#        <WrapperDir>\yazi.ps1        `yazi` wrapper; q/Q in yazi move the shell (cwd-file).
#                                     WrapperDir must come before the yazi.exe folder in PATH.
#        <BinDir>\yazi-pane-open.cmd  used by the yazi "o" keys (emacs-q in a new psmux pane).
#   3. Installs the plugins listed in .config\yazi\package.toml (`ya pkg install`).
#   4. Reports missing tools and PATH problems. It does not change PATH and does not
#      register anything to run at logon.
param(
    [string]$WrapperDir = 'C:\usr\bin',
    [string]$BinDir = (Join-Path $HOME 'bin')
)
$ErrorActionPreference = 'Stop'

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$configSrc = Join-Path $repo '.config\yazi'
$yaziHome = Join-Path $env:APPDATA 'yazi'
$configLink = Join-Path $yaziHome 'config'
$stamp = Get-Date -Format 'yyyyMMdd-HHmmss'

function Write-Step([string]$msg) { Write-Host "==> $msg" -ForegroundColor Cyan }
function Write-Warn([string]$msg) { Write-Host "    WARNING: $msg" -ForegroundColor Yellow }

# 1. Config folder link.
Write-Step "Link $configLink -> $configSrc"
New-Item -ItemType Directory -Force -Path $yaziHome, (Join-Path $yaziHome 'state') | Out-Null
$item = Get-Item -LiteralPath $configLink -Force -ErrorAction SilentlyContinue
if ($item -and $item.LinkType -and ($item.Target -contains $configSrc)) {
    Write-Host '    already linked'
} else {
    if ($item) {
        $backup = "$configLink.bak-$stamp"
        if ($item.LinkType) {
            Write-Host "    removing old link (target: $($item.Target))"
            $item.Delete()
        } else {
            Write-Host "    moving existing config to $backup"
            Move-Item -LiteralPath $configLink -Destination $backup
        }
    }
    New-Item -ItemType Junction -Path $configLink -Target $configSrc | Out-Null
    Write-Host '    linked'
}

# 2. Forwarder scripts.
function Set-Forwarder([string]$path, [string[]]$lines) {
    New-Item -ItemType Directory -Force -Path (Split-Path $path) | Out-Null
    $text = ($lines -join "`r`n") + "`r`n"
    if (Test-Path -LiteralPath $path) {
        if ((Get-Content -LiteralPath $path -Raw) -eq $text) { Write-Host "    up to date: $path"; return }
        Copy-Item -LiteralPath $path -Destination "$path.bak-$stamp"
        Write-Host "    backed up old file to $path.bak-$stamp"
    }
    [System.IO.File]::WriteAllText($path, $text)
    Write-Host "    wrote $path"
}

Write-Step 'Forwarder scripts'
Set-Forwarder (Join-Path $WrapperDir 'yazi.ps1') @(
    '# Forwarder: the real script lives in the rc repo (windows/bin/yazi.ps1).',
    '# It must stay in a PATH folder that comes before the folder of yazi.exe.',
    "& `"$repo\windows\bin\yazi.ps1`" @args"
)
Set-Forwarder (Join-Path $BinDir 'yazi-pane-open.cmd') @(
    '@echo off',
    'rem Forwarder: the real script lives in the rc repo.',
    "call `"$repo\windows\bin\yazi-pane-open.cmd`" %*"
)

# 3. Plugins.
Write-Step 'Plugins (ya pkg install)'
if (Get-Command ya.exe -ErrorAction SilentlyContinue) {
    & ya.exe pkg install
} else {
    Write-Warn 'ya.exe not found; run `ya pkg install` after installing yazi.'
}

# 4. Checks.
Write-Step 'Checks'
foreach ($tool in 'yazi.exe', 'ya.exe', 'fzf', 'emacs', 'tmux', 'pwsh') {
    if (-not (Get-Command $tool -ErrorAction SilentlyContinue)) { Write-Warn "$tool not found in PATH" }
}
$pathDirs = @($env:Path -split ';' | Where-Object { $_ } | ForEach-Object { $_.TrimEnd('\') })
function Get-PathIndex([string]$dir) {
    for ($i = 0; $i -lt $pathDirs.Count; $i++) { if ($pathDirs[$i] -ieq $dir.TrimEnd('\')) { return $i } }
    return -1
}
$wrapperIdx = Get-PathIndex $WrapperDir
$binIdx = Get-PathIndex $BinDir
if ($wrapperIdx -lt 0) { Write-Warn "$WrapperDir is not in PATH (needed for the yazi wrapper)" }
if ($binIdx -lt 0) { Write-Warn "$BinDir is not in PATH (needed for yazi-pane-open and emacs-q)" }
$yaziExe = Get-Command yazi.exe -CommandType Application -ErrorAction SilentlyContinue | Select-Object -First 1
if ($yaziExe -and $wrapperIdx -ge 0) {
    $exeIdx = Get-PathIndex (Split-Path $yaziExe.Source)
    if ($exeIdx -ge 0 -and $exeIdx -lt $wrapperIdx) {
        Write-Warn "$WrapperDir comes after $(Split-Path $yaziExe.Source) in PATH, so 'yazi' runs yazi.exe directly (q/Q will not move the shell)"
    }
}
Write-Step 'Done'
