;;; local-proxy-conf --- Summary

;;; Commentary:

;;; Code:

(require 'url-vars)

(customize-set-variable 'url-proxy-services '(("http" . "proxyhost:port")
                                              ("https" . "proxyhost:port")))

(provide 'local-proxy-conf)
;;; local-proxy-conf.el ends here
