#!/bin/bash

cd $(dirname $0)

# git clone https://github.com/jonmosco/kube-ps1.git
curl -o ~/.local/share/tools/kube-ps1.sh https://raw.githubusercontent.com/jonmosco/kube-ps1/master/kube-ps1.sh

