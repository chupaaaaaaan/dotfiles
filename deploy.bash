#!/bin/bash

cd $(dirname $0)

mkdir -p ~/.bash_profile.d
mkdir -p ~/.bashrc.d
mkdir -p ~/.local/bin
mkdir -p ~/.local/share/tools
mkdir -p ~/.local/share/applications
mkdir -p ~/.local/share/bash-completion/completions
mkdir -p ~/.local/share/fonts
mkdir -p ~/bin
sudo mkdir -p /usr/local/share/applications

for file in .??*
do
    [[ "${file}" == ".gitignore" ]] && continue
    [[ "${file}" == ".git" ]]       && continue

    ln -n -f -s $(pwd)/${file} ~/${file}
done

for file in desktop-launcher/*
do
    [[ "${file}" =~ ^.+\.wrapper$ ]] && sudo ln -n -f -s $(pwd)/${file} /usr/local/bin/${file##*/}
    [[ "${file}" =~ ^.+\.desktop$ ]] && sudo ln -n -f -s $(pwd)/${file} /usr/local/share/applications/${file##*/}
done
