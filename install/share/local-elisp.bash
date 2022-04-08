#!/bin/bash

cd $(dirname $0)

find ../../.emacs.d/local/ -type f | xargs -I@ cp -n @ ~/.elisp/local
