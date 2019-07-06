umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# create ~/.local/bin if not exist
if [ ! -d "$HOME/.local/bin" ]; then
    mkdir -p "$HOME/.local/bin"
fi

# create ~/bin if not exist
if [ ! -d "$HOME/bin" ]; then
    mkdir -p "$HOME/bin"
fi

# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"


# set local profiles if they exist
for f in $HOME/.bash_profile.d/*; do
    [ -f "$f" ] && . "$f"
done
