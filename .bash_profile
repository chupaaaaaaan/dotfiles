umask 022

# set local profiles if they exist
for f in $HOME/.bash_profile.d/*; do
    [ -f "$f" ] && . "$f"
done

[[ -f ~/.bashrc  ]] && . ~/.bashrc
