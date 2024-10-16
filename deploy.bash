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

# git clone https://github.com/git/git.git
wget -q -O ~/.local/share/bash-completion/completions/git.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
wget -q -O ~/.local/share/tools/git-prompt.sh                  https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh

# git clone https://github.com/jonmosco/kube-ps1.git
wget -q -O ~/.local/share/tools/kube-ps1.sh                    https://raw.githubusercontent.com/jonmosco/kube-ps1/master/kube-ps1.sh

### deploy dotfiles ###
for file in .??*
do
    [ "${file}" = ".gitignore" ] && continue
    [ "${file}" = ".git" ]       && continue

    ln -n -f -s $(pwd)/${file} ~/${file}
    
done

