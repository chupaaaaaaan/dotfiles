;;; mode-line --- Summary

;;; Commentary:

;;; Code:

(display-time-mode t)
(setq display-time-interval 60)
(setq display-time-format "%m/%d %H:%M")
;; (setq display-time-format "%F %T")
;; (display-battery-mode t)

(line-number-mode t)
(column-number-mode t)

(defvar sml/no-confirm-load-theme t)
(defvar sml/theme 'light)
(defvar sml/shorten-directory -1)
(sml/setup)

;; (when (require 'powerline nil t)
;;   )



;; Minor Mode名を定義 (必要に応じて)
(when (require 'diminish nil t)
  (eval-after-load "company"             '(diminish 'company-mode "Comp")) ;; 自分でMinor Mode名を定義
  ;; 非表示
  (eval-after-load "undo-tree"           '(diminish 'undo-tree-mode))
  (eval-after-load "volatile-highlights" '(diminish 'volatile-highlights-mode))
  (eval-after-load "helm-mode"           '(diminish 'helm-mode))
  (eval-after-load "helm-mode"           '(diminish 'helm--minor-mode))
  )


;;; 複数のディレクトリで同じファイル名のファイルを開いたときのバッファ名を調整する
;; (when (require 'uniquify nil t)
;;   (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;;   ;; (setq uniquify-ignore-buffers-re "[^*]+")
;;   (setq uniquify-min-dir-content 4)
;;   )

;; bufferの行数の表示 ('\n'の数)
(when (require 'total-lines nil t)
  (global-total-lines-mode t)
  (defun my-set-line-numbers ()
    (setq-default mode-line-front-space
                  (append mode-line-front-space
                          '((:eval (format " (%d)" (- total-lines 1)))))))
  (add-hook 'after-init-hook 'my-set-line-numbers))


(when (require 'stopwatch nil t))





(provide '20-modeline)
;;; 20-modeline.el ends here
