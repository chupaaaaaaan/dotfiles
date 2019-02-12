#!/bin/bash

cd $(dirname $0)

while read line
do
    pkgname=$(basename "${line}" .git)
    if [ ! -d "${pkgname}" ]; then
        git clone "${line}"
    fi
done < <(cat package_list.txt | grep -v -e "^#" -e "^[[:space:]]*$")
