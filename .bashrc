# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Command history
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000

# Window size
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi


# docker
dpsa() {
    docker ps -a --format "table {{.ID}}\t{{.Names}}\t{{.Image}}\t{{.Networks}}\t{{.Ports}}\t{{.Status}}"|peco|tr -s ' '|cut -d' ' -f1
}

dimg() {
    docker images --format "table {{.ID}}\t{{.Repository}}\t{{.Tag}}\t{{.Size}}"|peco|tr -s ' '|cut -d' ' -f1
}

dimg_name() {
    docker images --format "table {{.ID}}\t{{.Repository}}\t{{.Tag}}\t{{.Size}}"|peco|tr -s ' '|cut -d' ' -f2,3|tr ' ' ':'
}

drun() {
    docker run --rm -d "$@" "$(dimg)"
}

# AWS EC2 Instances
ec2ips() {
    aws ec2 describe-instances --query 'Reservations[*].Instances[].[PublicIpAddress, InstanceId, Tags[?Key==`Name`] | [0].Value]' --output text|grep -v "^None"|peco|cut -f1
}


# Prompt settings
## ANSI escape sequence (character theme)
RESET="\e[m"
BOLD="\e[1m"
RED="\e[31m"
GREEN="\e[32m"
YELLOW="\e[33m"
BLUE="\e[34m"
MAGENTA="\e[35m"
CYAN="\e[36m"
WHITE="\e[37m"
SSH_COLOR="${BLUE}"

[ -n "${SSH_CONNECTION}" ] && SSH_COLOR="${RED}"

## git
. ~/.local/share/tools/git-prompt.sh

___git_ps1_toggled () {
    if [ "${__GIT_PS1_TOGGLE}" -eq "1" ]; then
        __git_ps1 " (${MAGENTA}%s${RESET})"
    else
        :
    fi
}

giton () {
    __GIT_PS1_TOGGLE=1
}

gitoff () {
    __GIT_PS1_TOGGLE=0
}

gr () {
    if git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
        cd `pwd`/`git rev-parse --show-cdup`
    fi
}

GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUPSTREAM=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWSTASHSTATE=1
__GIT_PS1_CMD="\$(___git_ps1_toggled)"
__GIT_PS1_TOGGLE=1

giton

## ghq setting
sd () {
    if [ -n "$*" ]; then
        local srcdir=$(ghq list|peco --select-1 --query "$*")
    else
        local srcdir=$(ghq list|peco --select-1)
    fi
    [ -n "$srcdir" ] && echo "$(ghq list --full-path --exact $srcdir)"
}

cs () {
    cd $(sd "$*")
}

ccl () {
    local target="${1}"
    ghq create "localproject/${target}"
}

## PSn setting
if [ `id -u` -eq 0 ]; then
    PS1="${BOLD}${GREEN}\D{%F} ${YELLOW}\t${RESET}|${BOLD}${RED}\u${WHITE}@${SSH_COLOR}\h${RESET}${__KUBE_PS1_CMD}${__GIT_PS1_CMD}${RESET}| ${CYAN}\w${RESET}"$'\n# '
else
    PS1="${BOLD}${GREEN}\D{%F} ${YELLOW}\t${RESET}|${BOLD}${BLUE}\u${WHITE}@${SSH_COLOR}\h${RESET}${__KUBE_PS1_CMD}${__GIT_PS1_CMD}${RESET}| ${CYAN}\w${RESET}"$'\n$ '
fi    
PS2='| '


# Include local settings if they exist
for f in ~/.bashrc.d/*; do
    [ -f "$f" ] && . "$f"
done


# Alias definitions
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi
