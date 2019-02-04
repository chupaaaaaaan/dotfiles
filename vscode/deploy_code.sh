#!/bin/bash

cd $(dirname $0)

##################### vscode settings #####################

## deploy setting files
SETTINGDIR=""
if [ "`uname -s`" = "Linux" ]; then
    SETTINGDIR="${HOME}/.config/Code/User"

elif [ "`uname -s`" = "Darwin" ]; then
    SETTINGDIR="${HOME}/Library/Application\ Support/Code/User"

else
    echo "vscode setting is not installed."
    exit 0
fi

ln -n -f -s `pwd`/settings.json ${SETTINGDIR}/settings.json
ln -n -f -s `pwd`/keybindings.json ${SETTINGDIR}/keybindigns.json


## install plugins
cat extensions | xargs -n1 -I@ code --install-extension @
