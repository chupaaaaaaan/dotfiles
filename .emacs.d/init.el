;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load-path setting
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; add directories under "~/.emacs.d/" to load-path
(add-to-load-path "elisp" "conf" "public_repos")

;; load local configures
(dolist (lcnf (directory-files (concat user-emacs-directory "local_conf") t "\\.el$"))
  (load-file lcnf))

;; package.el
(require 'package nil t)
(package-initialize)

;; add repositories
(add-to-list 'package-archives '("org"           . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable"  . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa"         . "https://melpa.org/packages/"))

;; package list with repository
(setq package-pinned-packages
      '(
        ;; (auto-complete        . "melpa-stable")
        (company              . "melpa-stable")
        (company-ghc          . "melpa-stable")
        (diminish             . "melpa-stable")
        (docker-compose-mode  . "melpa-stable")
        (dockerfile-mode      . "melpa-stable")
        ;; (egg                  . "melpa-stable")
        (elm-mode             . "melpa-stable")
        (flycheck             . "melpa-stable")
        (flycheck-haskell     . "melpa-stable")
        (ghc                  . "melpa-stable")
        (haskell-mode         . "melpa-stable")
        (helm                 . "melpa-stable")
        (init-loader          . "melpa-stable")
        (lsp-haskell          . "melpa")
        (lsp-mode             . "melpa-stable")
        (lsp-ui               . "melpa-stable")
        (markdown-mode        . "melpa-stable")
        ;; (powerline            . "melpa-stable")
        (smart-mode-line      . "melpa-stable")
        (total-lines          . "melpa-stable")
        (undo-tree            . "gnu")
        (vbasense             . "melpa-stable")
        (volatile-highlights  . "melpa-stable")
        (yaml-mode            . "melpa-stable")
        ))

(unless package-archive-contents (package-refresh-contents))

(dolist (pkg (mapcar 'car package-pinned-packages))
  (unless (package-installed-p pkg)
        (package-install pkg)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; locale and environment
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; face
;; キャラクタ端末では、バックグラウンドを"dark"に設定する
(when (eq window-system nil)
  (setq frame-background-mode 'dark))

;; others
;; ツールバー・スクロールバー非表示設定
(when window-system
  (tool-bar-mode nil)
  (scroll-bar-mode nil))

;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;; splash screenを無効にする
(setq inhibit-splash-screen t)

;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;; C-u C-SPC C-SPC ... でどんどん過去のマークを遡る
;; (setq set-mark-command-repeat-pop t)

;; インデントにTABを使わないようにする
(setq-default indent-tabs-mode nil)

;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode t)

;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;; ログの記録行数を増やす
(setq message-log-max 10000)

;; 履歴をたくさん保存する
(setq history-length 1000)

;; マウスによるyankを許可
(setq mouse-yank-at-point t)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theme
;; (load-theme 'wheatgrass t)
(load-theme 'manoj-dark t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlights
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user-defined hlface
(defface my-hl-line-face
  '((((class color) (background dark))
     (:background "blue"))
    (((class color) (background light))
     (:background "dark slate gray"))
    (t (:bold t)))
  "*Face used by hl-line.")

;; highlight on the current line
(require 'hl-line nil t)
(global-hl-line-mode t)
;; (setq hl-line-face 'my-hl-line-face)

;; highlight on the region
(setq transient-mark-mode t)


;; PARENTHESES
;; highlight between two corresponding parentheses
(require 'paren nil t)
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match nil)
(set-face-underline 'show-paren-match "yellow")

;; volatile-highlights
(require 'volatile-highlights nil t)
(volatile-highlights-mode t)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'time nil t)
(display-time-mode t)
(setq display-time-interval 60)
(setq display-time-format "%m/%d %H:%M")

(require 'simple nil t)
(line-number-mode t)
(column-number-mode t)


;; (require 'smart-mode-line nil t)
(defvar sml/no-confirm-load-theme t)
(defvar sml/theme 'light)
(defvar sml/shorten-directory -1)
(sml/setup)

;; (require 'powerline nil t)



;; diminish: Minor-mode name definition
(require 'company nil t)
(eval-after-load "company"             '(diminish 'company-mode "Comp"))
;; Hidden
(eval-after-load "undo-tree"           '(diminish 'undo-tree-mode))
(eval-after-load "volatile-highlights" '(diminish 'volatile-highlights-mode))
(eval-after-load "helm-mode"           '(diminish 'helm-mode))
(eval-after-load "helm-mode"           '(diminish 'helm--minor-mode))


;; 複数のディレクトリで同じファイル名のファイルを開いたときのバッファ名を調整する
;; (when (require 'uniquify nil t)
;;   (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;;   ;; (setq uniquify-ignore-buffers-re "[^*]+")
;;   (setq uniquify-min-dir-content 4)
;;   )

;; total-line: show number of lines
(require 'total-lines nil t)
(global-total-lines-mode t)
(defun my-set-line-numbers ()
  (setq-default mode-line-front-space
                (append mode-line-front-space
                        '((:eval (format " (%d)" (- total-lines 1)))))))
(add-hook 'after-init-hook 'my-set-line-numbers)

;; stopwatch-mode
(require 'stopwatch nil t)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq backup-directory-alist         `((".*" . ,temporary-file-directory)))
;; (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq auto-save-timeout 15)
(setq auto-save-interval 60)

;; (setq make-backup-files nil)
;; (setq auto-save-default nil)
;; (setq auto-save-list-file-name nil)
;; (setq auto-save-list-file-prefix nil)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saveplace
(require 'saveplace nil t)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))


;; company
(require 'company nil t)
(global-company-mode t)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)

(set-face-attribute 'company-tooltip                  nil :background "lightgrey" :foreground "black")
(set-face-attribute 'company-tooltip-common           nil :background "lightgrey" :foreground "black")
(set-face-attribute 'company-tooltip-common-selection nil :background "steelblue" :foreground "white")
(set-face-attribute 'company-tooltip-selection        nil :background "steelblue" :foreground "black")
(set-face-attribute 'company-preview-common           nil :background nil         :foreground "lightgrey" :underline t)
(set-face-attribute 'company-scrollbar-fg             nil :background "orange")
(set-face-attribute 'company-scrollbar-bg             nil :background "gray40")

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
(define-key company-active-map (kbd "C-i") 'company-complete-selection)

(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-h") nil)

(add-to-list 'company-backends 'company-elm)
(add-to-list 'company-backends 'company-ghc)
(add-to-list 'company-backends 'company-lsp)



;; flycheck
(require 'flycheck nil t)
(add-hook 'after-init-hook #'global-flycheck-mode)


;; ediff
(when (executable-find "diff")
  (require 'ediff nil t)
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-split-window-function 'split-window-horizontally)
  (global-set-key (kbd "C-c d") 'ediff-files)
  )


;; helm
(require 'helm nil t)
(require 'helm-config nil t)
(helm-mode t)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c g") 'helm-google-suggest)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(require 'helm-buffers nil t)
(setq helm-buffers-fuzzy-matching t)
(require 'helm-for-files nil t)
(setq helm-recentf-fuzzy-match t)


;; undo-tree
(require 'undo-tree nil t)
(global-undo-tree-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp
(require 'lsp-mode)
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; vbasence
(require 'vbasense nil t)
(setq vbasense-popup-help-key "C-:")
(setq vbasense-jump-to-definition-key "C->")
;; (customize-group "vbasense")
(vbasense-config-default)


;; elm
(require 'elm-mode nil t)
;; (add-to-list 'auto-mode-alist '("\\.elm$" . elm-mode))


;; haskell
(require 'haskell-mode nil t)
;; (require 'haskell nil t)
(require 'ghc nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)


;; (setq haskell-program-name "stack ghci")
;; (add-hook 'haskell-mode-hook 'inf-haskell-mode)

;; (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
;; (add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'font-lock-mode)
;; (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
;; (add-hook 'haskell-mode-hook 'flycheck-mode)










;; markdown
(require 'markdown-mode nil t)
;; (add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode) )
;; (add-to-list 'auto-mode-alist '("\\.md"       . markdown-mode) )
;; (add-to-list 'auto-mode-alist '("\\.md"       . gfm-mode) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings that do not depend on some major modes or minor modes
(require 'scroll-lock nil t)
(defun toggle-scroll-lock ()
  "Toggle scroll lock."
  (interactive)
  (scroll-lock-mode
   (if scroll-lock-mode -1 1))
  (message "Scroll lock %s"
           (if scroll-lock-mode "enabled" "disabled")))

(global-set-key (kbd "C-c m") 'toggle-scroll-lock)
(global-set-key (kbd "C-h")   'delete-backward-char)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
;; (global-set-key (kbd "C-t")   'other-window)









;; init-loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lsp-haskell company-lsp lsp-mode lsp-ui flymake-shell powerline total-lines diminish smart-mode-line ac-haskell-process flycheck-haskell flycheck undo-tree yaml-mode volatile-highlights vbasense helm haskell-mode init-loader egg auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
