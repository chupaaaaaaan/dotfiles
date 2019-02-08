;;; helm
(when (require 'helm-config nil t)
  (helm-mode 1)
  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "M-y") 'helm-show-kill-ring)
  (define-key global-map (kbd "C-x b") 'helm-mini)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-c g") 'helm-google-suggest)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)

  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t))
