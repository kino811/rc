# rc
my resource files

- configure files
  - vim
  - emacs (`.emacs.d`: `init.el` -> `myInit.org`, Customize settings in `emacs-custom.el`)
  - tmux / psmux (`.tmux.conf`), wezterm (`.config/wezterm`)
  - yazi (`.config/yazi`)

## New Windows machine

1. Install the tools (e.g. with chocolatey): Git, PowerShell 7, Emacs, WezTerm, psmux, yazi, fzf,
   ripgrep, fd, Node.js, gsudo (WezTerm already bundles the JetBrains Mono font).
2. Clone this repo to `~/rc` and run once (safe to run again):
   ```
   pwsh -File ~/rc/windows/install.ps1 -WhatIf   # see what it would change
   pwsh -File ~/rc/windows/install.ps1
   ```
   - Puts `~/bin` first in the **system** PATH (asks, then uses gsudo). The user PATH always comes
     after the system PATH, so `~/bin` there would lose to `chocolatey\bin` (e.g. `yazi.exe`).
     Putting it first before installing chocolatey packages is fine; they are appended.
   - Emacs: `~/.emacs.d/init.el` forwarder, junctions `~/.emacs.d/lisp` and
     `~/.emacs.d/site-lisp/p4-review`; clones project-x and eat-pty-win into `~/.emacs.d/site-lisp`
     over HTTPS (push URL: SSH alias `github.com-kino811`) and runs `npm ci` for eat-pty-win.
   - `~/.tmux.conf` forwarder (`source-file`), `~/.config/wezterm` junction.
   - `~/bin` forwarders for `windows/bin` scripts, then `install-yazi.ps1`. The yazi wrapper
     forwarders (`yazi.cmd`, `yazi.ps1`) go to `-WrapperDir` (default: the same `~/bin`).
   - Existing files are kept as `<name>.bak-<time>`. Nothing is registered to run at logon.
   - Options: `-BinDir`, `-WrapperDir`, `-SkipPath`, `-SkipSiteLisp`, `-SkipYazi`, `-HomeDir`
     (for testing).
   - A machine that keeps another folder ahead of `chocolatey\bin` for the yazi wrappers (e.g.
     `C:\usr\bin` in the system PATH) runs `install.ps1 -WrapperDir C:\usr\bin`.
3. Start Emacs once; `use-package` installs the packages.
4. EAF is optional and not installed by the script; see the install comments in the EAF section of
   `.emacs.d/myInit.org`.

## windows

- `windows/bin` (reached through forwarders in `~/bin`)
  - `emacs-q`, `emacs-q.cmd`: quick terminal Emacs with `.emacs.d/quick-init.el`.
  - `ec`, `ec.cmd`: `emacsclient`.
  - `chrome-cdp.cmd`: Chrome with CDP on port 9223 and a dedicated profile.
  - `yazi.ps1`, `yazi.cmd`: `yazi` wrappers; q/Q move the shell to yazi's last folder. Their
    forwarders (in `-WrapperDir`) must come before the yazi.exe folder in PATH.
  - `claude-editor.cmd` (+ `console-enter.ps1`): Claude Code's `EDITOR` for Ctrl+G (emacs-q after
    one Enter that ends Claude Code's pending console read).
  - `yazi-pane-open.cmd`: used by the yazi `o` keys.
- yazi
  - `install-yazi.ps1` (run by `install.ps1`) links `%APPDATA%\yazi\config` to `.config/yazi`
    (directory junction), writes the yazi forwarders (default `~/bin`), and runs `ya pkg install`.
  - Keys: Enter opens in emacs-q in the same pane; `o h/j/k/l` opens in emacs-q in a new psmux pane
    on that side, `o o` picks the side automatically.
  - Bookmarks (yamb) are kept in `%APPDATA%\yazi\state\bookmark`, outside the repo.
- `emacs-input-guard.ps1`: keeps terminal Emacs (`emacs -nw`) from freezing in WezTerm + psmux
  (clears the console flag ENABLE_VIRTUAL_TERMINAL_INPUT, 0x200, that sometimes gets set on its console).
  Terminal Emacs starts it for its own process (`w32-tty.el`), so no logon entry is needed.
  `install-emacs-input-guard.ps1` (optional) registers it in the Startup folder; `-Uninstall` removes it.
- `psmux-split-here.ps1`: psmux `prefix %` / `prefix "` (`.tmux.conf`) split in the working
  directory of what runs in the pane: PowerShell `cd`, a `claude` session's cwd (follows `/cd`),
  or for `claude agents` the session that got the last prompt (read from the noty plugin state).
- Windows terminal Emacs also relies on the Windows-only key block in `.config/wezterm/wezterm.lua`
  (sends Ctrl+Shift+Fn "carrier" keys that `.emacs.d/myInit.org` translates back) and on
  `C-S-F7` in `.tmux.conf`.
