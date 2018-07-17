;;; helm
(when (require 'helm-config nil t)
  (helm-mode 1)
  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key global-map (kbd "C-x b") 'helm-mini)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files))
