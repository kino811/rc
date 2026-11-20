@echo off
rem emacs-q - quick terminal Emacs with a minimal config (Windows console).
rem Usage: emacs-q [file ...]
rem Loads .emacs.d/quick-init.el from this repo instead of the full init.
emacs -nw -Q -l "%~dp0..\..\.emacs.d\quick-init.el" %*
