#!/bin/bash

cd $(dirname $0)

VERSION="$1"
shift

### install ghq ###
curl -L -o ghq.zip https://github.com/x-motemen/ghq/releases/download/${VERSION}/ghq_windows_amd64.zip &&
    unzip -o -j -d ~/.local/bin ghq.zip "**/ghq.exe" &&
    chmod +x ~/.local/bin/ghq.exe &&
    rm -f ghq.zip
