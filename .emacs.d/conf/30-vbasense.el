;;; vbasence
(when (require 'vbasense nit t)
  (setq vbasense-popup-help-key "C-:")
  (setq vbasense-jump-to-definition-key "C->")
  ;; (customize-group "vbasense")
  (vbasense-config-default))
