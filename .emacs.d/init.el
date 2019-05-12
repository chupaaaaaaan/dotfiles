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
;; `package-pinned-packages' is customized below
;; (auto-complete        . "melpa-stable")
;; (company-ghc          . "melpa-stable")
;; (diminish             . "melpa-stable")
;; (ghc                  . "melpa-stable")
;; (htmlize              . "melpa-stable")
;; (smart-mode-line      . "melpa-stable")

(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

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


;; Doom-themes
;; https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))
  

;; Font
(set-face-attribute 'default nil :family "Ricty Diminished" :height 180)

;; Highlights
;; highlight on the current line
(use-package hl-line
  :ensure t
  :config
  (global-hl-line-mode t))

;; highlight between two corresponding parentheses
(use-package paren
  :ensure t
  :custom
  (show-paren-delay 0)
  (show-paren-style 'expression)
  :config
  (show-paren-mode t)
  (set-face-background 'show-paren-match nil)
  (set-face-underline 'show-paren-match "yellow"))

;; highlight on the region
(setq transient-mark-mode t)

;; volatile-highlights
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

;; Modeline (Doom)
;; https://github.com/seagle0128/doom-modeline
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1))

(use-package doom-modeline
  :ensure t
  :config
  (line-number-mode t)
  (column-number-mode t)
  (doom-modeline-mode 1)
  (setq doom-modeline-height 20)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-minor-modes nil))

;; datetime format
(use-package time
  :ensure t
  :custom
  (display-time-interval 60)
  (display-time-format "%m/%d %H:%M")
  :config
  (display-time-mode t)
)

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
(use-package saveplace
  :ensure t
  :custom
  (save-place-file (concat user-emacs-directory "places"))
  :config
  (save-place-mode))

;; scroll-lock
(use-package scroll-lock
  :ensure t
  :init
  (defun toggle-scroll-lock ()
    "Toggle scroll lock."
    (interactive)
    (scroll-lock-mode (if scroll-lock-mode -1 1))
    (message "Scroll lock %s" (if scroll-lock-mode "enabled" "disabled")))

  :bind(("C-c m" . toggle-scroll-lock)))

;; ediff
;; (when (executable-find "diff")
;;   (require 'ediff nil t)
;;   (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
;;   (setq-default ediff-split-window-function 'split-window-horizontally)
;;   (global-set-key (kbd "C-c d") 'ediff-files)
;;   )

;; helm
(use-package helm
  :ensure t
  :custom
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  :bind(("C-c h" . helm-command-prefix)
        ("M-x" . helm-M-x)
        ("M-y" . helm-show-kill-ring)
        ("C-x b" . helm-mini)
        ("C-x C-f" . helm-find-files)
        ("C-c g" . helm-google-suggest)
        ("C-x c" . nil)
        :map helm-map
        ("C-h" . delete-backward-char)
        ("<tab>" . helm-execute-persistent-action)
        ("C-i" . helm-execute-persistent-action)
        ("C-z" . helm-select-action))
  :config
  (require 'helm-config)
  (require 'helm-buffers)
  (require 'helm-for-files)
  (helm-mode t))


;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure t
  :custom
  (org-directory "~/Dropbox/org/")
  (org-agenda-files '("~/Dropbox/org/"))
  (org-default-notes-file (concat org-directory "notes.org"))
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-todo-keywords '(
                       ; (sequence "TODO(t)" "WIP(w)" "PENDING(p)" "|" "DONE(d)" "CANCELED(c@)")
                       (sequence "TODO(t)" "WIP(w)" "PENDING(p)" "|" "DONE(d)" "CANCELED(c)")
                       (sequence "MEMO(m)" "|" "REFLECTION(r)" "KNOWLEDGE(k)" "FORGETTABLE(f)")
                       ))
  (org-capture-templates
   '(
     ("t" " Todo"     entry (file+headline org-default-notes-file "Inbox")    "* TODO %?\n  %U\n** Reference\n  %i\n\n")
     ("c" " Calender" entry (file+headline org-default-notes-file "Schedule") "* TODO %?\n  %U\n\n")
     ("m" " Memo"     entry (file+headline org-default-notes-file "Journals") "* MEMO %?\n  %U\n** Reference\n  %i\n\n")
     ))

  :bind(("C-c c" . org-capture)
        ("C-c a" . org-agenda))

  :hook (org-mode . org-bullets-mode)

  :config
  (require 'org-capture)
  (require 'org-agenda)

  (use-package org-bullets
    :ensure t
    :custom
    (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" "")))

  (defun show-org-buffer (file)
    "Show an org-file FILE on the current buffer."
    (interactive)
    (if (get-buffer file) (let ((buffer (get-buffer file)))
                            (switch-to-buffer buffer)
                            (message "%s" file))
      (find-file (concat org-directory file)))))

;; (global-set-key (kbd "C-M-^") '(lambda () (interactive) (show-org-buffer "notes.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Develop Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
)

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package helm-c-yasnippet
  ;:disabled
  :ensure t
  :after yasnippet
  :custom
  (helm-yas-space-match-any-greedy t)
  :bind(("C-c y" . helm-yas-complete)))


;; company
(use-package company
  :ensure t
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)

  :hook (after-init . global-company-mode)

  :bind(:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-s" . company-filter-candidates)
        ;;("<tab>" . company-complete-common-or-cycle)
        ("<tab>" . company-complete)
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-h" . nil)
        :map company-search-map
        ("<tab>" . company-complete)
        ;("<tab>" . company--insert-candidate)
        ("C-s" . company-select-next)
        ("C-r" . company-select-previous))
  
  :config
  (require 'company-yasnippet)
  ;; (defvar company-mode/enable-yas t
  ;;   "Enable yasnippet for all backends.")
  ;; (defun company-mode/backend-with-yas (backend)
  ;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
  ;;       backend
  ;;     (append (if (consp backend)
  ;;                 backend
  ;;               (list backend))
  ;;             '(:with company-yasnippet))))
  ;; (defun set-yas-as-company-backend ()
  ;;   (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
  ;; (add-hook 'company-mode-hook 'set-yas-as-company-backend)
)




  
(use-package company-posframe
  :disabled
  :ensure t
  :after company
  :custom
  (company-posframe-mode t))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode))


;; flycheck
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package flycheck-posframe
  :ensure t
  :hook (flycheck-mode . flycheck-posframe-mode))

;; lsp
(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-prefer-flymake nil)
  (lsp-document-sync-method 'incremental)
  (lsp-enable-snippet t)
  ;; (lsp-lens-hide)

  :config
  (require 'lsp-clients))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  
  :custom-face
  (lsp-ui-doc-background ((nil (:background "black"))))
  
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  ;; (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-position 'bottom)
  ;; (lsp-ui-doc-border "white")
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-list-position 'bottom)
  (lsp-ui-flycheck-live-reporting t)
  (lsp-ui-sideline-enable nil)
  ;;    (lsp-ui
  :preface
  (defun ladicle/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))
  
  :bind(:map lsp-ui-mode-map
             ("C-c C-r" . lsp-ui-peek-find-references)
             ("C-c C-j" . lsp-ui-peek-find-definitions)
             ("C-c i"   . lsp-ui-peek-find-implementation)
             ("C-c d"   . ladicle/toggle-lsp-ui-doc)))

(use-package company-lsp
    ;:disabled
    :ensure t
    :after (company lsp-mode)
    :config
    (add-to-list 'company-backends 'company-lsp)
    ;;(add-to-list 'company-backends 'company-yasnippet)
    :custom
    (company-lsp-cache-candidates t)
    (company-lsp-async t)
    (company-lsp-enable-snippet t)
    (company-lsp-enable-recompletion t)
    (company-lsp-match-candidate-predicate 'company-lsp-match-candidate-flex))



;; haskell
;; for ghc-8.0.2 and later
(use-package lsp-haskell
  :ensure t
  :hook ((haskell-mode . lsp)
  ;       (haskell-mode . company-yasnippet)
         )
  :custom
  (lsp-haskell-process-path-hie "hie-wrapper"))


;; for ghc-7.10.3
;; (require 'haskell-mode nil t)
;; (require 'ghc nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  ;(add-to-list 'company-backends 'company-ghc t)


;; elm
(use-package elm-mode
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-elm))
;; (add-to-list 'auto-mode-alist '("\\.elm$" . elm-mode))

;; markdown
(use-package markdown-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keymap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-unset-key (kbd "C-x C-c"))
(defalias 'exit 'save-buffers-kill-emacs)

;; Settings that do not depend on some major modes or minor modes
(global-set-key (kbd "C-h")   'delete-backward-char)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
(global-set-key (kbd "C-t")   'other-window)


;; init-loader
(use-package init-loader
  :ensure t
  :config
  (init-loader-load "~/.emacs.d/conf"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(package-selected-packages
   (quote
    (flycheck-posframe company-posframe yasnippet-snippets volatile-highlights use-package undo-tree org-bullets nyan-mode lsp-ui lsp-haskell init-loader helm-c-yasnippet flycheck elm-mode doom-themes doom-modeline company-quickhelp company-lsp company-box))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((nil (:background "black")))))
