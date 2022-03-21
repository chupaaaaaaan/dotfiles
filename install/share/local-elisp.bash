#!/bin/bash

cd $(dirname $0)

find ../../.emacs.d/local_conf -type f ! -name "_*" | xargs -I@ cp -n @ ~/.elisp/local
