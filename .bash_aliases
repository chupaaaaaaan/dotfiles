# colorful alias
test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# VERY VERY DENGEROUS COMMANDS!!!!!!!!!!!!!!
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'

# some more ls
alias l='ls -CF'
alias la='ls -A'
alias ll='ls -alF'

# docker
alias d='docker'
alias dc='docker-compose'
alias dstop='dpsa|xargs -r -I@ docker stop @'
alias drmc='dpsa|xargs -r -I@ docker rm @'
alias drmi='dimg_name|xargs -r -I@ docker rmi @'
alias drmi_none='d images -q -f "dangling=true"|xargs -r -I@ docker rmi @'
alias dexe='docker exec -it $(dpsa) /bin/bash'

# kubernetes
alias k='kubectl'
alias kx='kubectx'
alias kn='kubens'

