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

# docker
alias d='docker'
alias dc='docker-compose'

dpsa() {
    docker ps -a --format "table {{.ID}}\t{{.Names}}\t{{.Image}}\t{{.Ports}}\t{{.Status}}"|peco|tr -s ' '|cut -d' ' -f1
}

dimg() {
    docker images --format "table {{.ID}}\t{{.Repository}}:{{.Tag}}\t{{.Size}}"|peco|tr -s ' '|cut -d' ' -f1
}

alias dstop='dpsa|xargs -r -I@ docker stop @'
alias drmc='dpsa|xargs -r -I@ docker rm @'
alias drmi='dimg|xargs -r -I@ docker rmi @'
alias drmi_none='d images -q -f "dangling=true"|xargs -r -I@ docker rmi @'
alias drun='docker run -it $(dimg) /bin/bash --login'

alias dcsh='dc exec $(docker-compose ps --services|peco) /bin/bash --login'






# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
# alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
