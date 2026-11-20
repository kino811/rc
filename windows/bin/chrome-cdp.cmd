@echo off
rem Launch Chrome with CDP (remote debugging) on port 9223 using a dedicated profile.
start "" "C:\Program Files\Google\Chrome\Application\chrome.exe" --remote-debugging-port=9223 --user-data-dir="%USERPROFILE%\chrome-cdp" --disable-renderer-backgrounding --disable-backgrounding-occluded-windows %*
