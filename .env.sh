path_prepend() {
    case ":$PATH:" in
        *":$1:"*) ;;
        *) PATH="$1:$PATH" ;;
    esac
}

# User's executables
[[ -d ~/bin ]] && path_prepend ~/bin

# Executables installed by user
[[ -d ~/.local/bin ]] && path_prepend ~/.local/bin

# Go
export GOBIN=~/.local/bin
[[ -d /usr/local/go/bin ]] && path_prepend /usr/local/go/bin

# Haskell (GHCup)
[[ -f ~/.ghcup/env ]] && . ~/.ghcup/env

# Python3
export PYPATH=~/.local/python
[[ -d $PYPATH/bin ]] && path_prepend $PYPATH/bin

# Vagrant
export VAGRANT_WSL_ENABLE_WINDOWS_ACCESS="1"
[[ -d "/mnt/c/Program Files/Oracle/VirtualBox" ]] && path_prepend "/mnt/c/Program Files/Oracle/VirtualBox"

# Graphviz
export GRAPHVIZ_DOT=$(which dot)

# Input Method
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx


export PATH
