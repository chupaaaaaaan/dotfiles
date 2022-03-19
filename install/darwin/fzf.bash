#!/bin/bash

cd $(dirname $0)

VERSION="$1"
shift

### install fzf ###
curl -L -o fzf.zip https://github.com/junegunn/fzf/releases/download/${VERSION}/fzf-${VERSION}-darwin_amd64.zip &&
    unzip -o -j -d ~/.local/bin fzf.zip "**/fzf" &&
    chmod +x ~/.local/bin/fzf &&
    rm -f fzf.zip
