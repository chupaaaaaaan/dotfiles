# GNU coreutils' commands
if [ "$(uname)" = "Darwin" ]; then
    . ~/.bash_aliases_macOS
else
    . ~/.bash_aliases_Linux
fi

# emacs
alias em='emacsclient -t -a ""'
alias killem='emacsclient -e "(kill-emacs)"'

# haskell
alias ghc='stack ghc'
alias ghci='stack ghci'
alias runghc='stack runghc'
alias rghc='eval $(stack path --compiler-exe)'
alias rghci='eval $(stack path --compiler-exe) --interactive'
alias ghci_atcoder='stack --resolver=lts-6.35 ghci'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
# alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
