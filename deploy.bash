#!/bin/bash

cd $(dirname $0)

### create directories ###
mkdir -p ~/.bash_profile.d
mkdir -p ~/.bashrc.d
mkdir -p ~/.local/bin
mkdir -p ~/.local/share/tools
mkdir -p ~/.local/share/applications
mkdir -p ~/bin
mkdir -p ~/.elisp/local


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
        bash install/darwin/gnutools.bash
        bash install/darwin/ghq.bash v1.2.1
        bash install/darwin/fzf.bash 0.29.0
        bash install/darwin/peco.bash v0.5.10
        bash install/darwin/ag.bash
        bash install/darwin/delta.bash
        ;;
    MINGW64_NT-*)
        bash install/windows/ghq.bash v1.2.1
        bash install/windows/fzf.bash 0.29.0
        bash install/windows/peco.bash v0.5.10
        bash install/windows/ag.bash 2020-07-05_2.2.0-58-g5a1c8d8
        ;;
    *)
        echo "Incompatible OS type: $(uname -s)" 1>&2
        exit 1
        ;;
esac


bash install/share/kube-ps1.sh.bash
bash install/share/git.bash
bash install/share/local-elisp.bash



### deploy dotfiles ###
for file in .??*
do
    [ "${file}" = ".gitignore" ] && continue
    [ "${file}" = ".git" ]       && continue

    ln -n -f -s $(pwd)/${file} ~/${file}
    
done

