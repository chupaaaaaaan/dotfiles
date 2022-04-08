#!/bin/bash

cd $(dirname $0)

find ../../.emacs.d/local-elisp/ -type f | xargs -I@ cp -n @ ~/.elisp/local

# プロキシ設定
# HTTP_PROXY環境変数が存在している場合のみ、「scheme://」を削った文字列を追加
# HTTP_PROXYおよびHTTPS_PROXYは同じになる想定
PROXY_HOST="${HTTP_PROXY:+${HTTP_PROXY##*://}}"

[ -n "${PROXY_HOST}" ] && cat <<EOF > ~/.elisp/local/local-proxy-conf.el 
;;; local-proxy-conf --- Summary

;;; Commentary:

;;; Code:

(require 'url-vars)

(customize-set-variable 'url-proxy-services '(("http" . "${PROXY_HOST}")
                                              ("https" . "${PROXY_HOST}")))

(provide 'local-proxy-conf)
;;; local-proxy-conf.el ends here
EOF
