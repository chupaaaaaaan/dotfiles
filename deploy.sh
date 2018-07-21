#!/bin/bash

for file in .??*
do
    [ "${file}" = ".gitignore" ] && continue
    [ "${file}" = ".git" ]       && continue

    ln -f -s `pwd`/${file} ~/${file}
done

