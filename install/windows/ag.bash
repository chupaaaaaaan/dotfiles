#!/bin/bash

cd $(dirname $0)

VERSION="$1"
shift

VERSION_ENCODED=$(echo ${VERSION} | sed 's@_@%2F@')

### install fzf ###
curl -L -o ag.zip https://github.com/k-takata/the_silver_searcher-win32/releases/download/${VERSION_ENCODED}/ag-${VERSION}-x64.zip &&
    unzip -o -j -d ~/.local/bin ag.zip "ag.exe" &&
    chmod +x ~/.local/bin/ag.exe &&
    rm -f ag.zip
