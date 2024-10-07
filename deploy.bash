#!/bin/bash

cd $(dirname $0)

### create directories ###
mkdir -p ~/.bash_profile.d
mkdir -p ~/.bashrc.d
mkdir -p ~/.local/bin
mkdir -p ~/.local/share/tools
mkdir -p ~/.local/share/applications
mkdir -p ~/.local/share/bash-completion/completions
mkdir -p ~/.local/share/fonts
mkdir -p ~/bin


bash install/share/kube-ps1.sh.bash
bash install/share/git.bash


### deploy dotfiles ###
for file in .??*
do
    [ "${file}" = ".gitignore" ] && continue
    [ "${file}" = ".git" ]       && continue

    ln -n -f -s $(pwd)/${file} ~/${file}
    
done

