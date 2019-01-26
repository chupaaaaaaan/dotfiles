#!/bin/bash

cd $(dirname $0)
for file in .??*
do
    [ "${file}" = ".gitignore" ] && continue
    [ "${file}" = ".git" ]       && continue

    ln -n -f -s `pwd`/${file} ~/${file}
done

