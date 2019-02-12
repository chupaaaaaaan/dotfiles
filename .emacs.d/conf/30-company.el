;;; company
(when (require 'auto-complete-config nil t)
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
)
