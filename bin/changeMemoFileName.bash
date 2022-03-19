#!/bin/bash

cd $(dirname $0)

cd ../../org/memo || {
    echo "incorrect directory."
    exit 1
}


for file in 202*.org
do
    DATE=${file:0:10}
    # sed -i -e "1s/^/#+DATE: ${DATE}\n/" ${file}
    sed -e "1s/^/#+DATE: ${DATE}\n/" ${file} > ${file}-e

    FNAME=${file:11}
    mv ${file}-e ${FNAME}

    rm ${file}
done
