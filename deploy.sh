#!/bin/bash

cd $(dirname $0)
for file in .??*
do
    [ "${file}" = ".gitignore" ] && continue
    [ "${file}" = ".git" ]       && continue

    ln -n -f -s `pwd`/${file} ~/${file}
done



##################### peco settings #####################
ARCHIVE=""
EXTENDER=""
if [ "`uname -s`" = "Linux" -a "`uname -m`" = "x86_64" ]; then
    ARCHIVE="peco_linux_amd64.tar.gz"
    EXTENDER="tar xzvf"
fi

if [ "`uname -s`" = "Darwin" -a "`uname -m`" = "x86_64" ]; then
    ARCHIVE="peco_darwin_amd64.zip"
    EXTENDER="unzip"
fi

if [ -z ${ARCHIVE} ]; then
    echo "peco is not installed."
    exit 0
fi


## peco download
if [ ! -e ${ARCHIVE} ]; then
    curl -L -O https://github.com/peco/peco/releases/download/v0.5.3/${ARCHIVE} 
fi

## peco install
BINDIR="$(pwd)/$(echo ${ARCHIVE} | cut -d'.' -f1)"

if [ ! -x ${BINDIR}/peco ]; then
    rm -rf ${BINDIR}
    ${EXTENDER} ${ARCHIVE}
fi

if [ ! -h /usr/local/bin/peco ]; then
    rm -f /usr/local/bin/peco
    ln -s ${BINDIR}/peco /usr/local/bin/peco
fi
