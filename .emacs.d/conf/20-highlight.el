;; user-defined hlface
(defface my-hl-line-face
  '((((class color) (background dark))
     (:background "blue"))
    (((class color) (background light))
     (:background "dark slate gray"))
    (t (:bold t)))
  "*Face used by hl-line.")

;; highlight on the current line
(global-hl-line-mode t)
(setq hl-line-face 'my-hl-line-face)

;; highlight on the region
(setq transient-mark-mode t)

;; highlight between two corresponding parentheses
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; volatile-highlights
(when (require 'volatile-highlights nil t)
  (volatile-highlights-mode t))
