# install.ps1 - set up the terminal and Emacs environment from this repo on a new Windows machine.
# Run once per machine (safe to run again; existing files are kept as <name>.bak-<time>):
#   pwsh -File windows\install.ps1            # do it
#   pwsh -File windows\install.ps1 -WhatIf    # only print what would change
#
# What it does:
#   1. Checks required tools (warns only, installs nothing). JetBrains Mono ships with WezTerm.
#   2. PATH: <BinDir> (default ~/bin) must be the FIRST entry of the system PATH, because the
#      user PATH always comes after the system PATH (so after chocolatey\bin). If it is not,
#      shows the change and asks before writing it with gsudo. -SkipPath skips this step.
#   3. Emacs: ~/.emacs.d/init.el forwarder that loads .emacs.d/init.el of this repo;
#      junctions ~/.emacs.d/lisp and ~/.emacs.d/site-lisp/p4-review to this repo.
#   4. site-lisp: clones project-x and eat-pty-win over HTTPS when missing (push URL uses the
#      SSH alias git@github.com-kino811) and runs `npm ci` in eat-pty-win/node-bridge.
#      EAF is not handled; see the install comments in the EAF section of myInit.org.
#   5. Terminal: ~/.tmux.conf forwarder (`source-file` this repo's .tmux.conf, no admin needed);
#      ~/.config/wezterm and ~/.config/herdr junctions; user env HERDR_CONFIG_PATH points herdr
#      at ~/.config/herdr/config.toml (herdr reads %APPDATA%\herdr\config.toml otherwise).
#   6. ~/bin forwarders for windows/bin: ec, ec.cmd, emacs-q, emacs-q.cmd, claude-editor.cmd,
#      chrome-cdp.cmd, yazi.cmd. claude-editor.cmd is Claude Code's EDITOR (its settings "env").
#   7. yazi: runs install-yazi.ps1 with the same BinDir.
# It never registers anything to run at logon.
[CmdletBinding(SupportsShouldProcess)]
param(
    [string]$HomeDir = $HOME,
    [string]$BinDir,
    [switch]$SkipPath,
    [switch]$SkipSiteLisp,
    [switch]$SkipYazi
)
$ErrorActionPreference = 'Stop'
if (-not $BinDir) { $BinDir = Join-Path $HomeDir 'bin' }

$repo = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$repoFwd = $repo -replace '\\', '/'
$stamp = Get-Date -Format 'yyyyMMdd-HHmmss'
$dry = [bool]$WhatIfPreference
$warnings = [System.Collections.Generic.List[string]]::new()

function Write-Step([string]$msg) { Write-Host "==> $msg" -ForegroundColor Cyan }
function Write-Info([string]$msg) { Write-Host "    $msg" }
function Write-Would([string]$msg) { Write-Host "    [WhatIf] $msg" -ForegroundColor DarkGray }
function Write-Warn([string]$msg) { Write-Host "    WARNING: $msg" -ForegroundColor Yellow; $warnings.Add($msg) }

# Move an existing file/folder aside (or just remove a link, never touching its target).
function Clear-Target([string]$path) {
    $item = Get-Item -LiteralPath $path -Force -ErrorAction SilentlyContinue
    if (-not $item) { return }
    if ($item.LinkType) {
        Write-Info "removing old $($item.LinkType) $path (target: $($item.Target))"
        $item.Delete()
    } else {
        Write-Info "moving existing $path to $path.bak-$stamp"
        Move-Item -LiteralPath $path -Destination "$path.bak-$stamp"
    }
}

function Set-Junction([string]$link, [string]$target) {
    $item = Get-Item -LiteralPath $link -Force -ErrorAction SilentlyContinue
    if ($item -and $item.LinkType) {
        # Link targets may be relative (e.g. ..\rc\.emacs.d\lisp); compare full paths.
        $current = @($item.Target | ForEach-Object { [System.IO.Path]::GetFullPath($_, (Split-Path $link)).TrimEnd('\') })
        if ($current -contains $target.TrimEnd('\')) { Write-Info "already linked: $link"; return }
    }
    if ($dry) { Write-Would "link $link -> $target$(if ($item) { ' (existing one moved aside)' })"; return }
    New-Item -ItemType Directory -Force -Path (Split-Path $link) | Out-Null
    Clear-Target $link
    New-Item -ItemType Junction -Path $link -Target $target | Out-Null
    Write-Info "linked $link -> $target"
}

# Write a small text file. -Lf for sh scripts and tmux config; CRLF otherwise.
function Set-Forwarder([string]$path, [string[]]$lines, [switch]$Lf) {
    $nl = if ($Lf) { "`n" } else { "`r`n" }
    $text = ($lines -join $nl) + $nl
    $item = Get-Item -LiteralPath $path -Force -ErrorAction SilentlyContinue
    if ($item -and -not $item.LinkType -and ([System.IO.File]::ReadAllText($path) -eq $text)) { Write-Info "up to date: $path"; return }
    if ($dry) { Write-Would "write $path$(if ($item) { ' (existing one moved aside)' })"; return }
    New-Item -ItemType Directory -Force -Path (Split-Path $path) | Out-Null
    Clear-Target $path   # a symlink is removed first so its target (e.g. this repo's file) is never overwritten
    [System.IO.File]::WriteAllText($path, $text)
    Write-Info "wrote $path"
}

# 1. Checks ------------------------------------------------------------------------------
Write-Step 'Required tools'
$tools = [ordered]@{
    'git' = 'Git for Windows'; 'pwsh' = 'PowerShell 7'; 'emacs' = 'Emacs'; 'wezterm' = 'WezTerm'
    'tmux' = 'psmux (choco install psmux)'; 'yazi' = 'yazi'; 'ya' = 'yazi (ya)'; 'fzf' = 'fzf'
    'rg' = 'ripgrep'; 'fd' = 'fd'; 'node' = 'Node.js'; 'npm' = 'Node.js (npm)'; 'gsudo' = 'gsudo'
    'herdr' = 'herdr (choco install herdr)'
}
foreach ($t in $tools.Keys) {
    $cmd = Get-Command $t -CommandType Application -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($cmd) { Write-Info ("{0,-8} {1}" -f $t, $cmd.Source) } else { Write-Warn "$t not found ($($tools[$t]))" }
}

# 2. PATH --------------------------------------------------------------------------------
Write-Step "PATH: $BinDir first in the system PATH"
if ($SkipPath) {
    Write-Info 'skipped (-SkipPath)'
} else {
    $envKey = 'HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager\Environment'
    $rawPath = (Get-Item $envKey).GetValue('Path', '', 'DoNotExpandEnvironmentNames')
    $entries = @($rawPath -split ';' | Where-Object { $_ })
    $norm = { param($p) [Environment]::ExpandEnvironmentVariables($p).TrimEnd('\') }
    $binNorm = & $norm $BinDir
    if ($entries.Count -gt 0 -and ((& $norm $entries[0]) -ieq $binNorm)) {
        Write-Info 'ok'
    } else {
        $rest = @($entries | Where-Object { (& $norm $_) -ine $binNorm })
        $newPath = (@($BinDir) + $rest) -join ';'
        Write-Info "current first entries: $(($entries | Select-Object -First 3) -join ';') ..."
        Write-Info "new first entries:     $((@($BinDir) + ($rest | Select-Object -First 2)) -join ';') ..."
        if ($dry) {
            Write-Would 'prepend it to the system PATH with gsudo (asks first)'
        } elseif (-not (Get-Command gsudo -ErrorAction SilentlyContinue)) {
            Write-Warn "gsudo not found; add $BinDir to the front of the system PATH yourself"
        } else {
            $answer = Read-Host "    Prepend $BinDir to the system PATH with gsudo (admin)? [y/N]"
            if ($answer -match '^(y|yes)$') {
                $escaped = $newPath.Replace("'", "''")
                $script = "Set-ItemProperty -Path '$envKey' -Name Path -Type ExpandString -Value '$escaped'"
                $encoded = [Convert]::ToBase64String([Text.Encoding]::Unicode.GetBytes($script))
                & gsudo pwsh -NoProfile -EncodedCommand $encoded
                if ($LASTEXITCODE -eq 0) { Write-Info 'system PATH updated; open a new terminal (or sign out) to pick it up' }
                else { Write-Warn "gsudo failed (exit $LASTEXITCODE); system PATH not changed" }
            } else {
                Write-Warn "system PATH not changed; $BinDir must come before chocolatey\bin for the yazi wrapper"
            }
        }
    }
}

# 3. Emacs ---------------------------------------------------------------------------------
Write-Step 'Emacs'
$emacsDir = Join-Path $HomeDir '.emacs.d'
Set-Forwarder (Join-Path $emacsDir 'init.el') @(
    ';;; init.el --- Forwarder: the real init lives in the rc repo -*- lexical-binding: t -*-',
    "(load `"$repoFwd/.emacs.d/init.el`" nil t)"
)
Set-Junction (Join-Path $emacsDir 'lisp') (Join-Path $repo '.emacs.d\lisp')
Set-Junction (Join-Path $emacsDir 'site-lisp\p4-review') (Join-Path $repo '.emacs.d\site-lisp\p4-review')

# 4. site-lisp packages from GitHub --------------------------------------------------------
Write-Step 'site-lisp packages (GitHub)'
if ($SkipSiteLisp) {
    Write-Info 'skipped (-SkipSiteLisp)'
} else {
    foreach ($name in 'project-x', 'eat-pty-win') {
        $dir = Join-Path $emacsDir "site-lisp\$name"
        if (Test-Path -LiteralPath $dir) { Write-Info "exists: $dir" }
        elseif ($dry) { Write-Would "git clone https://github.com/kino811/$name.git $dir (push URL: SSH alias)" }
        else {
            & git clone "https://github.com/kino811/$name.git" $dir
            if ($LASTEXITCODE -ne 0) { Write-Warn "git clone $name failed"; continue }
            & git -C $dir remote set-url --push origin "git@github.com-kino811:kino811/$name.git"
            Write-Info "cloned $name (push URL: git@github.com-kino811:kino811/$name.git)"
        }
    }
    $bridge = Join-Path $emacsDir 'site-lisp\eat-pty-win\node-bridge'
    if (Test-Path -LiteralPath (Join-Path $bridge 'node_modules')) { Write-Info 'eat-pty-win node-bridge: node_modules present' }
    elseif ($dry) { Write-Would "npm ci in $bridge" }
    elseif (-not (Test-Path -LiteralPath $bridge)) { Write-Warn 'eat-pty-win/node-bridge not found; skipped npm' }
    elseif (-not (Get-Command npm -ErrorAction SilentlyContinue)) { Write-Warn 'npm not found; run `npm ci` in eat-pty-win/node-bridge later' }
    else {
        Push-Location $bridge
        try { & npm ci; if ($LASTEXITCODE -ne 0) { Write-Warn 'npm ci failed in eat-pty-win/node-bridge' } } finally { Pop-Location }
    }
}

# 5. Terminal ------------------------------------------------------------------------------
Write-Step 'Terminal (tmux, wezterm, herdr)'
Set-Forwarder (Join-Path $HomeDir '.tmux.conf') @(
    '# Forwarder: the real config lives in the rc repo.',
    "source-file `"$repoFwd/.tmux.conf`""
) -Lf
Set-Junction (Join-Path $HomeDir '.config\wezterm') (Join-Path $repo '.config\wezterm')
Set-Junction (Join-Path $HomeDir '.config\herdr') (Join-Path $repo '.config\herdr')
# herdr has no home-folder lookup on Windows: HERDR_CONFIG_PATH (or XDG_CONFIG_HOME, which
# other tools read too) is the only way off %APPDATA%\herdr. A running herdr server keeps
# its old environment, so restart it to pick this up.
$herdrConfig = Join-Path $HomeDir '.config\herdr\config.toml'
if ([Environment]::GetEnvironmentVariable('HERDR_CONFIG_PATH', 'User') -eq $herdrConfig) {
    Write-Info "already set: HERDR_CONFIG_PATH=$herdrConfig"
} elseif ($dry) {
    Write-Would "set user env HERDR_CONFIG_PATH=$herdrConfig"
} else {
    [Environment]::SetEnvironmentVariable('HERDR_CONFIG_PATH', $herdrConfig, 'User')
    Write-Info "set user env HERDR_CONFIG_PATH=$herdrConfig (restart the herdr server to use it)"
}

# 6. ~/bin forwarders ----------------------------------------------------------------------
Write-Step "Forwarders in $BinDir"
foreach ($name in 'ec', 'emacs-q') {
    Set-Forwarder (Join-Path $BinDir $name) @(
        '#!/bin/sh',
        '# Forwarder: the real script lives in the rc repo.',
        "exec `"$repoFwd/windows/bin/$name`" `"`$@`""
    ) -Lf
}
foreach ($name in 'ec.cmd', 'emacs-q.cmd', 'claude-editor.cmd', 'chrome-cdp.cmd', 'yazi.cmd') {
    $extra = if ($name -eq 'yazi.cmd') { @('rem It must stay in a PATH folder that comes before the folder of yazi.exe.') } else { @() }
    Set-Forwarder (Join-Path $BinDir $name) (@(
        '@echo off',
        "rem Forwarder: the real script lives in the rc repo (windows/bin/$name)."
    ) + $extra + @("call `"$repo\windows\bin\$name`" %*"))
}

# 7. yazi ----------------------------------------------------------------------------------
Write-Step 'yazi (install-yazi.ps1)'
if ($SkipYazi) { Write-Info 'skipped (-SkipYazi)' }
elseif ($dry) { Write-Would "pwsh -File $repo\windows\install-yazi.ps1 -WrapperDir $BinDir -BinDir $BinDir" }
else { & (Join-Path $PSScriptRoot 'install-yazi.ps1') -WrapperDir $BinDir -BinDir $BinDir }

# Summary ----------------------------------------------------------------------------------
Write-Step 'Done'
if ($warnings.Count) {
    Write-Host "    $($warnings.Count) warning(s):" -ForegroundColor Yellow
    $warnings | ForEach-Object { Write-Host "      - $_" -ForegroundColor Yellow }
}
Write-Info 'Next: start Emacs once (use-package installs the packages), and see the EAF section of'
Write-Info '      .emacs.d/myInit.org if you want EAF.'
