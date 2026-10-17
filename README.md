# rc
my resource files

- configure files
  - vim
  - emacs

- windows
  - `emacs-input-guard.ps1`: keeps terminal Emacs (`emacs -nw`) from freezing in WezTerm + psmux
    (clears the console flag ENABLE_VIRTUAL_TERMINAL_INPUT, 0x200, that sometimes gets set on its console).
  - `install-emacs-input-guard.ps1`: run once per Windows machine that uses terminal Emacs; registers the
    guard in the Startup folder and starts it. `-Uninstall` removes it.
    `powershell -NoProfile -ExecutionPolicy Bypass -File windows\install-emacs-input-guard.ps1`
- Windows terminal Emacs also relies on the Windows-only key block in `.config/wezterm/wezterm.lua`
  (sends Ctrl+Shift+Fn "carrier" keys that `.emacs.d/myInit.org` translates back) and on
  `C-S-F7` in `.tmux.conf`.
