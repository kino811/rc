@echo off
rem claude-editor - $EDITOR for Claude Code on Windows (Ctrl+G): emacs-q after one Enter.
rem Claude Code leaves a line-mode console read pending while the editor runs, and it
rem takes every key until an Enter arrives; console-enter.ps1 sends that Enter first.
rem Set it only as Claude Code's EDITOR (settings.json "env"); elsewhere the Enter would
rem become a blank line in the editor. See console-enter.ps1.
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0console-enter.ps1"
call "%~dp0emacs-q.cmd" %*
