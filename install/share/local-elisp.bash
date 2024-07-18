#!/bin/bash

cd $(dirname $0)

find ${HOME}/.emacs.d/local-elisp/ -type f | xargs -I@ cp -n @ ~/.elisp/local
