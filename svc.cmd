@ECHO off

reg add HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Run /v UserActivityCounter /d "%~dp0\UserActivityCounter.exe"

pause