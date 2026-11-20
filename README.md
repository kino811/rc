# rc
my resource files

- configure files
  - vim
  - emacs
  - yazi (`.config/yazi`)

- yazi on Windows
  - Run `pwsh -File windows\install-yazi.ps1` once per machine. It links `%APPDATA%\yazi\config` to
    `.config/yazi` (directory junction), writes forwarders for `windows/bin/yazi.ps1` (`yazi` wrapper,
    q/Q move the shell; default `C:\usr\bin`, must come before the yazi.exe folder in PATH) and
    `windows/bin/yazi-pane-open.cmd` (default `~/bin`), and runs `ya pkg install`.
  - Keys: Enter opens in emacs-q in the same pane; `o h/j/k/l` opens in emacs-q in a new psmux pane
    on that side, `o o` picks the side automatically.
  - Bookmarks (yamb) are kept in `%APPDATA%\yazi\state\bookmark`, outside the repo.

- windows
  - `emacs-input-guard.ps1`: keeps terminal Emacs (`emacs -nw`) from freezing in WezTerm + psmux
    (clears the console flag ENABLE_VIRTUAL_TERMINAL_INPUT, 0x200, that sometimes gets set on its console).
  - `install-emacs-input-guard.ps1`: run once per Windows machine that uses terminal Emacs; registers the
    guard in the Startup folder and starts it. `-Uninstall` removes it.
    Just double-click `windows\install-emacs-input-guard.bat` (or run it from any shell).
- Windows terminal Emacs also relies on the Windows-only key block in `.config/wezterm/wezterm.lua`
  (sends Ctrl+Shift+Fn "carrier" keys that `.emacs.d/myInit.org` translates back) and on
  `C-S-F7` in `.tmux.conf`.
