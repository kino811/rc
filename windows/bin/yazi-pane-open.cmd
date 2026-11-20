@echo off
rem yazi-pane-open - open files in emacs-q in a new psmux pane next to the current pane.
rem Usage: yazi-pane-open left|right|up|down|auto file...
rem Used by the yazi "o" keys (see %APPDATA%\yazi\config\keymap.toml).
rem   left/right/up/down: side of the current pane where the new pane goes.
rem   auto: split along the longer side. A terminal cell is about twice as tall as it is
rem         wide, so width >= 2 * height -> right, otherwise -> down.
rem psmux notes:
rem - It joins separate command arguments without keeping their quotes, so the whole
rem   command is passed as one string with the inner quotes escaped (\").
rem - "split-window -b" is ignored, so left/up split right/down first and then swap the
rem   new pane (active) with the previous one ("swap-pane -U"); focus stays on the new pane.
setlocal DisableDelayedExpansion
set "WHERE=%~1"
set "ARGS=%*"
if /i "%WHERE%"=="auto" goto auto
if /i "%WHERE%"=="left" (set "DIR=-h" & set "SWAP=1" & goto split)
if /i "%WHERE%"=="right" (set "DIR=-h" & set "SWAP=" & goto split)
if /i "%WHERE%"=="up" (set "DIR=-v" & set "SWAP=1" & goto split)
if /i "%WHERE%"=="down" (set "DIR=-v" & set "SWAP=" & goto split)
echo yazi-pane-open: first argument must be left, right, up, down or auto 1>&2
exit /b 1

:auto
set "DIR=-h"
set "SWAP="
set "W="
set "H="
for /f "tokens=1,2" %%a in ('tmux display-message -p -t %TMUX_PANE% "#{pane_width} #{pane_height}"') do (
  set "W=%%a"
  set "H=%%b"
)
if not defined W goto split
if not defined H goto split
set /a H2=H*2
if %W% LSS %H2% set "DIR=-v"

:split
setlocal EnableDelayedExpansion
rem Drop the direction word from the file list.
set "ARGS=!ARGS:*%WHERE%=!"
if defined ARGS set "ARGS=!ARGS:"=\"!"
tmux split-window %DIR% -t %TMUX_PANE% "emacs-q !ARGS!"
if defined SWAP tmux swap-pane -U
