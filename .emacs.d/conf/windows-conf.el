(when (eq window-system 'w32)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932)
  (setq default-directory (concat (getenv "HOMEPATH") "/"))
  (setq command-line-default-directory (concat (getenv "HOMEPATH") "/")))
