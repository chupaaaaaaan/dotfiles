#!/bin/bash

cd $(dirname $0)

VERSION="$1"
shift

### install peco ###
curl -L -o peco.zip https://github.com/peco/peco/releases/download/${VERSION}/peco_darwin_amd64.zip &&
    unzip -o -j -d ~/.local/bin peco.zip "**/peco" &&
    chmod +x ~/.local/bin/peco &&
    rm -f peco.zip
