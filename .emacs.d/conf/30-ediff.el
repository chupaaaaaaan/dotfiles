;;; ediff
(when (executable-find "diff")
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (define-key global-map (kbd "C-c d") 'ediff-files))
