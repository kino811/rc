@echo off
rem Wrapper for cmd.exe: after yazi quits with Q, move this cmd.exe session to the folder
rem yazi was last in. A .cmd run from cmd.exe executes in the same process, so cd persists.
rem PowerShell uses yazi.ps1 instead (see C:\usr\bin\yazi.ps1).
set "YAZI_CWD_FILE=%TEMP%\yazi-cwd-%RANDOM%.txt"
yazi.exe %* --cwd-file="%YAZI_CWD_FILE%"
set "YAZI_CWD="
if exist "%YAZI_CWD_FILE%" set /p YAZI_CWD=<"%YAZI_CWD_FILE%"
if defined YAZI_CWD if /i not "%YAZI_CWD%"=="%CD%" cd /d "%YAZI_CWD%"
del "%YAZI_CWD_FILE%" 2>nul
set "YAZI_CWD_FILE="
set "YAZI_CWD="
