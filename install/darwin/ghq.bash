#!/bin/bash

cd $(dirname $0)

VERSION="$1"
shift

### install ghq ###
curl -L -o ghq.zip https://github.com/x-motemen/ghq/releases/download/${VERSION}/ghq_darwin_amd64.zip &&
    unzip -o -j -d ~/.local/bin ghq.zip "**/ghq" &&
    chmod +x ~/.local/bin/ghq &&
    rm -f ghq.zip
