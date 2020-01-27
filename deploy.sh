#!/bin/bash

cd $(dirname $0)
for file in .??*
do
    [ "${file}" = ".gitignore" ] && continue
    [ "${file}" = ".git" ]       && continue

    ln -n -f -s `pwd`/${file} ~/${file}
done



##################### peco settings #####################
if [ "`uname -s`" = "Linux" -a "`uname -m`" = "x86_64" ]; then
    ARCHIVE="peco_linux_amd64.tar.gz"
    EXTENDER="tar xzvf"

elif [ "`uname -s`" = "Darwin" -a "`uname -m`" = "x86_64" ]; then
    ARCHIVE="peco_darwin_amd64.zip"
    EXTENDER="unzip"

else
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

if [ ! -h $HOME/bin/peco ]; then
    rm -f $HOME/bin/peco
    ln -s ${BINDIR}/peco $HOME/bin/peco
fi

############################ps1 settings#####################################3

# git clone https://github.com/jonmosco/kube-ps1.git
curl -O https://raw.githubusercontent.com/jonmosco/kube-ps1/master/kube-ps1.sh

# git clone https://github.com/git/git.git
curl -O https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
curl -O https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
