#/bin/sh

EMACSD="../../.emacs.d"

apt-get -y install emacs25-nox

# githubリポジトリのファイルをダウンロードする
cd ${EMACSD}/public_repos
cat ../conf/package.list | grep -v -e '^[[:space:]]*$' -e '^[[:space:]]*#' | xargs -n1 -I@ curl -O @
cd -

