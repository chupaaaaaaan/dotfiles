# colorful alias
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# VERY VERY DENGAROUS COMMANDS!!!!!!!!!!!!!!
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

# some more ls
alias l='ls -CF'
alias la='ls -A'
alias ll='ls -alF'

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
