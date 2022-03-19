#!/bin/bash

cd $(dirname $0)

# git clone https://github.com/git/git.git
curl --output-dir ~/.local/share/tools -O https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
curl --output-dir ~/.local/share/tools -O https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh



