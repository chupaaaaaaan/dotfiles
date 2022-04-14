#!/bin/bash

cd $(dirname $0)

# git clone https://github.com/git/git.git
curl -o ~/.local/share/tools/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
curl -o ~/.local/share/tools/git-prompt.sh       https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh



