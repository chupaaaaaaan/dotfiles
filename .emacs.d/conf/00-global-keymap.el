;;; global-map
;;; Settings that do not depend on some major modes or minor modes
(defun toggle-scroll-lock ()
  (interactive)
  (scroll-lock-mode
   (if scroll-lock-mode -1 1))
  (message "Scroll lock %s"
           (if scroll-lock-mode "enabled" "disabled")))
(define-key global-map (kbd "C-c m") 'toggle-scroll-lock)
(define-key global-map (kbd "C-h")   'delete-backward-char)
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
(define-key global-map (kbd "C-t")   'other-window)
