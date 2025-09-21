# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# Command history ##########################################################
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000

# Window size ##########################################################
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Completions ##########################################################
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Emacs ##########################################################
[ -s ~/.local/share/tools/emacs-vterm-bash.sh ] && . ~/.local/share/tools/emacs-vterm-bash.sh

# Node ##########################################################
export NVM_DIR=$HOME/.nvm
[[ -s "$NVM_DIR/nvm.sh" ]]          && . $NVM_DIR/nvm.sh
[[ -s "$NVM_DIR/bash_completion" ]] && . $NVM_DIR/bash_completion
[[ "none" = $(nvm current) ]]       && nvm install node

# Haskell ##########################################################
command -v stack > /dev/null 2>&1 && eval "$(stack --bash-completion-script stack)"


# Sdkman ##########################################################
export SDKMAN_DIR="${HOME}/.sdkman"
[[ -s "${SDKMAN_DIR}/bin/sdkman-init.sh" ]] && . "${SDKMAN_DIR}/bin/sdkman-init.sh"

# Dropbox ##########################################################
command -v dropbox.py > /dev/null 2>&1 && dropbox.py status | grep -q "Dropbox isn't running\!" && dropbox.py start > /dev/null 2>&1

# Docker ##########################################################
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

alias d='docker'
alias dc='docker compose'
alias dstop='dpsa|xargs -r -I@ docker stop @'
alias drmc='dpsa|xargs -r -I@ docker rm @'
alias drmi='dimg_name|xargs -r -I@ docker rmi @'
alias drmi_none='docker images -q -f "dangling=true"|xargs -r -I@ docker rmi @'
alias dbash='docker exec -it $(dpsa) /bin/bash'
alias dsh='docker exec -it $(dpsa) /bin/sh'

# Kubernetes ##########################################################
alias k='kubectl'
alias kx='kubectx'
alias kn='kubens'

# Git ##########################################################
[[ -s ~/.local/share/tools/git-prompt.sh ]] && . ~/.local/share/tools/git-prompt.sh

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

# Ghq ##########################################################
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

# Prompt ##########################################################
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
[[ -n "${SSH_CONNECTION}" ]] && SSH_COLOR="${RED}"

USER_COLOR="${BLUE}"
TERM_CHAR="$"
[[ $(id -u) == 0 ]] && {
    USER_COLOR="${RED}"
    TERM_CHAR="#"
}

## PS1/PS2
PS1="${BOLD}${GREEN}\D{%F} ${YELLOW}\t${RESET}|${BOLD}${USER_COLOR}\u${WHITE}@${SSH_COLOR}\h${RESET}${__KUBE_PS1_CMD}${__GIT_PS1_CMD}${RESET}| ${CYAN}\w${RESET}"$'\n${TERM_CHAR} '
PS2='| '

# Local settings ##########################################################
for f in ~/.bashrc.d/*; do
    [ -f "$f" ] && . "$f"
done

# Some aliases ##########################################################
## colorful alias
test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

## VERY VERY DENGEROUS COMMANDS!!!!!!!!!!!!!!
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

## some more ls
alias l='ls -CF'
alias la='ls -A'
alias ll='ls -alF'

# Source profile ##########################################################
. ~/.env.sh
