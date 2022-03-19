#!/bin/bash

cd $(dirname $0)

# git clone https://github.com/jonmosco/kube-ps1.git
curl --output-dir ~/.local/share/tools -O https://raw.githubusercontent.com/jonmosco/kube-ps1/master/kube-ps1.sh

