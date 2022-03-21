#!/bin/bash

### see https://qiita.com/eumesy/items/3bb39fc783c8d4863c5f

cd $(dirname $0)


### install gnu tools ###
brew install coreutils diffutils ed findutils gawk gnu-sed gnu-tar grep gzip

### configure path of gnu tools ###
: > ~/.bash_profile.d/gnutools
# coreutils
echo 'PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH'       >> ~/.bash_profile.d/gnutools
echo 'MANPATH=/usr/local/opt/coreutils/libexec/gnuman:$MANPATH' >> ~/.bash_profile.d/gnutools
# ed
echo 'PATH=/usr/local/opt/ed/libexec/gnubin:$PATH'              >> ~/.bash_profile.d/gnutools
echo 'MANPATH=/usr/local/opt/ed/libexec/gnuman:$MANPATH'        >> ~/.bash_profile.d/gnutools
# findutils
echo 'PATH=/usr/local/opt/findutils/libexec/gnubin:$PATH'       >> ~/.bash_profile.d/gnutools
echo 'MANPATH=/usr/local/opt/findutils/libexec/gnuman:$MANPATH' >> ~/.bash_profile.d/gnutools
# sed
echo 'PATH=/usr/local/opt/gnu-sed/libexec/gnubin:$PATH'         >> ~/.bash_profile.d/gnutools
echo 'MANPATH=/usr/local/opt/gnu-sed/libexec/gnuman:$MANPATH'   >> ~/.bash_profile.d/gnutools
# tar
echo 'PATH=/usr/local/opt/gnu-tar/libexec/gnubin:$PATH'         >> ~/.bash_profile.d/gnutools
echo 'MANPATH=/usr/local/opt/gnu-tar/libexec/gnuman:$MANPATH'   >> ~/.bash_profile.d/gnutools
# grep
echo 'PATH=/usr/local/opt/grep/libexec/gnubin:$PATH'            >> ~/.bash_profile.d/gnutools
echo 'MANPATH=/usr/local/opt/grep/libexec/gnuman:$MANPATH'      >> ~/.bash_profile.d/gnutools
