#!/bin/bash

cd ${HOME}
grep -H "^\*\* .*$" Dropbox/org/diary/*.org |
    sed "s/.org:\*\* /@/g" |
    cut -c19- |
    awk 'BEGIN{FS="@";OFS=" | ";a=1}{printf("% 5d | %s | %s\n",a,$1,$2); a=a+1}'
