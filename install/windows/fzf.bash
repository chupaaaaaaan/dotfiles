#!/bin/bash

cd $(dirname $0)

VERSION="$1"
shift

### install fzf ###
curl -L -o fzf.zip https://github.com/junegunn/fzf/releases/download/${VERSION}/fzf-${VERSION}-windows_amd64.zip &&
    unzip -o -j -d ~/.local/bin fzf.zip "**/fzf.exe" &&
    chmod +x ~/.local/bin/fzf.exe &&
    rm -f fzf.zip
