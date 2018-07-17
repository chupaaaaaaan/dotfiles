(require 'ucs-normalize nil t)
(set-file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;; invisiblize menu-bar
(unless (eq window-system 'ns)
  (menu-bar-mode nil))
