(when (eq window-system 'w32)
  ;; file name and message encoding
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932)

  ;; default path setting
  (setq default-directory (concat (getenv "HOMEPATH") "/"))
  (setq command-line-default-directory (concat (getenv "HOMEPATH") "/"))

  ;; font (face) setting
  (set-face-attribute 'default nil
                      :family "Ricty Diminished"
                      :height 150))
