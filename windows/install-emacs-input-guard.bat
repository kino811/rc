@echo off
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0install-emacs-input-guard.ps1" %*
pause
