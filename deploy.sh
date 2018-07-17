#!/bin/bash

for file in .??*
do
    ln -f -s `pwd`/${file} ~/${file}
done

