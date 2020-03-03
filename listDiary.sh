#!/bin/bash

cd ${HOME}
grep -H "<..:..>" Dropbox/org/diary/*.org | sed -e "s/.org:  - </@/g" -e "s/\(:..\)> /\1@/g" | cut -c19- | awk 'BEGIN{FS="@";OFS=" | "}{print $1,$2,$3}'

