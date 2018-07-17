;;; backup
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,temporary-file-directory t)))

(setq auto-save-timeout 15)
(setq auto-save-interval 60)

;; (setq make-backup-files nil)
;; (setq auto-save-default nil)
;; (setq auto-save-list-file-name nil)
;; (setq auto-save-list-file-prefix nil)
