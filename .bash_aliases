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
# alias ghc='stack ghc'
# alias ghci='stack ghci'
# alias runghc='stack runghc'

# docker
alias d='docker'
alias dc='docker-compose'

dpsa() {
    docker ps -a --format "table {{.ID}}\t{{.Names}}\t{{.Image}}\t{{.Ports}}\t{{.Status}}"|peco|tr -s ' '|cut -d' ' -f1
}

dimg() {
    docker images --format "table {{.ID}}\t{{.Repository}}:{{.Tag}}\t{{.Size}}"|peco|tr -s ' '|cut -d' ' -f2
}

drun() {
    docker run --rm -d $1 "$(dimg)"
}

alias dstop='dpsa|xargs -r -I@ docker stop @'
alias drmc='dpsa|xargs -r -I@ docker rm @'
alias drmi='dimg|xargs -r -I@ docker rmi @'
alias drmi_none='d images -q -f "dangling=true"|xargs -r -I@ docker rmi @'
alias dexe='docker exec -it $(dpsa) /bin/bash'

# AWS EC2 Instances
ec2ips() {
    aws ec2 describe-instances --query 'Reservations[*].Instances[].[PublicIpAddress, InstanceId, Tags[?Key==`Name`] | [0].Value]' --output text|grep -v "^None"|peco|cut -f1
}

# kubernetes
alias k='kubectl'
alias kx='kubectx'
alias kn='kubens'


# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
# alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
