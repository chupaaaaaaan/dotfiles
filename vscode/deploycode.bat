@echo off

pushd %0\..
cls

mklink settings.json %APPDATA%\Code\User\settings.json
mklink keybindings.json %APPDATA%\Code\User\keybindings.json 

for /f %%a in (extensions) do (
    code --install-extension %%a
)

pause
exit
