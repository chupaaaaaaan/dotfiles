#!/bin/bash

cd $(dirname $0)

### create directories ###
mkdir -p ~/.local/bin
mkdir -p ~/.local/share/tools


### check architecture ###
if [ "$(uname -m)" != "x86_64" ]; then
    echo "Incompatible machine type: $(uname -m)" 1>&2
    exit 1
fi


### install tools ###
case "$(uname -s)" in
    Linux)
        ;;
    Darwin)
        ./install/darwin/ghq.bash v1.2.1
        ./install/darwin/fzf.bash 0.29.0
        ./install/darwin/peco.bash v0.5.10
        ;;
    MINGW64_NT-*)
        ./install/windows/ghq.bash v1.2.1
        ./install/windows/fzf.bash 0.29.0
        ./install/windows/peco.bash v0.5.10
        ;;
    *)
        echo "Incompatible OS type: $(uname -s)" 1>&2
        exit 1
        ;;
esac


./install/share/kube-ps1.sh.bash
./install/share/git.bash




for file in .??*
do
    [ "${file}" = ".gitignore" ] && continue
    [ "${file}" = ".git" ]       && continue

    ln -n -f -s $(pwd)/${file} ~/${file}
    
done

