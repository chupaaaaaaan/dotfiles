#!/bin/bash

cd $(dirname $0)

VERSION="$1"
shift

### install delta ###
curl -L -o delta.zip https://github.com/dandavison/delta/releases/download/${VERSION}/delta-${VERSION}-x86_64-pc-windows-msvc.zip &&
    unzip -o -j -d ~/.local/bin delta.zip "**/delta.exe" &&
    chmod +x ~/.local/bin/delta.exe &&
    rm -f delta.zip
