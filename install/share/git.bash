#!/bin/bash

cd $(dirname $0)

# git clone https://github.com/git/git.git
wget -q -O ~/.local/share/bash-completion/completions/git.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
wget -q -O ~/.local/share/tools/git-prompt.sh                  https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
