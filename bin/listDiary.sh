#!/bin/bash

cd ${HOME}
grep -H "<..:..>" Dropbox/org/diary/*.org |
    sed -e "s/.org:  - </@/g" -e "s/\(:..\)> /\1@/g" |
    cut -c19- |
    awk 'BEGIN{
           FS="@";
           OFS=" | ";
           a="x"
         }
         {
           if(a!="x"$1){
             print "\n--< "$1" >""-------------------------------------------------"
           };
           print $2,$3;
           a="x"$1
         }'

