#!/bin/bash

cd $(dirname $0)

VERSION="$1"
shift

### install peco ###
curl -L -o peco.zip https://github.com/peco/peco/releases/download/${VERSION}/peco_windows_amd64.zip &&
    unzip -o -j -d ~/.local/bin peco.zip "**/peco.exe" &&
    chmod +x ~/.local/bin/peco.exe &&
    rm -f peco.zip
