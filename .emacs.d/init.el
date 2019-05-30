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
(add-to-load-path "conf" "public_repos")

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
;; (company-ghc          . "melpa-stable")
;; (diminish             . "melpa-stable")
;; (ghc                  . "melpa-stable")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  ;; (package-install 'diminish)
  ;; (package-install 'bind-key)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package use-package
  :custom
  (use-package-compute-statistics t)
  ;; (use-package-always-defer nil)

  ;; customize general settings
  (inhibit-startup-screen t)
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (indent-tabs-mode nil)
  (transient-mark-mode t)
  (savehist-mode t)
  (history-delete-duplicates t)
  (history-length 1000)
  (message-log-max 10000)
  (gc-cons-threshold (* 10 gc-cons-threshold))
  (mouse-yank-at-point t)
  )

;;TODO: 雑多な設定を整理する
;; locale and environment
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; maximize frame
(toggle-frame-maximized)

;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;; Doom-themes
;; https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :ensure t
  :demand t
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
  :demand t
  :config
  (global-hl-line-mode t))

;; highlight between two corresponding parentheses
(use-package paren
  :ensure t
  :demand t
  :custom
  (show-paren-mode t)
  (show-paren-delay 0)
  ;; (show-paren-style 'expression)
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  ;; :custom-face
  ;; (show-paren-match ((nil (:underline "#ff5555"))))
  )

;; volatile-highlights
(use-package volatile-highlights
  :ensure t
  :demand t
  :diminish t
  :after undo-tree
  :config
  (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

;; Modeline (Doom)
;; https://github.com/seagle0128/doom-modeline
(use-package nyan-mode
  :ensure t
  :demand t
  :config
  (nyan-mode 1))

(use-package doom-modeline
  :ensure t
  :demand t
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
  :demand t
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
  :demand t
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
  :custom
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  :config
  (helm-mode t)
  (require 'helm-config)
  (require 'helm-buffers)
  (require 'helm-for-files)
  )

;; undo-tree
(use-package undo-tree
  :ensure t
  :demand t
  :diminish t
  :config
  (global-undo-tree-mode t))

(use-package google-this
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: 便利に使う設定を考える
(use-package org
  :ensure t
  :custom
  (org-directory "~/Dropbox/org/")
  (org-agenda-files '("~/Dropbox/org/"))
  (org-default-notes-file (concat org-directory "notes.org"))
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-todo-keywords '(
                       ;; (sequence "TODO(t)" "WIP(w)" "PENDING(p)" "|" "DONE(d)" "CANCELED(c@)")
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

  :config
  (require 'org-capture)
  (require 'org-agenda)

  (use-package org-bullets
    :ensure t
    :hook
    (org-mode . org-bullets-mode)

    :custom
    (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" "")))

  (defun show-org-buffer (file)
    "Show an org-file FILE on the current buffer."
    (interactive)
    (if (get-buffer file) (let ((buffer (get-buffer file)))
                            (switch-to-buffer buffer)
                            (message "%s" file))
      (find-file (concat org-directory file))))
  )

;; (global-set-key (kbd "C-M-^") '(lambda () (interactive) (show-org-buffer "notes.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Develop Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
(use-package yasnippet
  ;; :disabled
  :ensure t
  :hook (after-init . yas-global-mode)

  :config
  (use-package yasnippet-snippets :ensure t)

  (use-package helm-c-yasnippet
    ;; :disabled
    :ensure t
    :after yasnippet
    :bind(("C-c y" . helm-yas-complete)))
  :custom
  (helm-yas-space-match-any-greedy t)
  )

;; company
;; TODO: company-backendsをバッファローカルに定義する方法を考える
(use-package company
  :ensure t
  :custom
  (company-global-mode t)
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)

  :hook
  (after-init . global-company-mode)
  (elm-mode     . (lambda () (set (make-local-variable 'company-backends) '((company-yasnippet company-elm company-files)))))
  (haskell-mode . (lambda () (set (make-local-variable 'company-backends)
                                  '((
                                     company-yasnippet
                                     company-lsp
                                     company-files
                                     )))))
  
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-s" . company-filter-candidates)
              ("<tab>" . company-complete-common-or-cycle)
              ;; ("<tab>" . company-complete)
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-h" . nil)
              :map company-search-map
              ("<tab>" . company-complete)
              ;; ("<tab>" . company--insert-candidate)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  ;; ("C-s" . company-select-next)
  ;; ("C-r" . company-select-previous))
  
  :config
  (use-package company-box
    :ensure t
    :diminish
    :hook (company-mode . company-box-mode)
    :custom
    (company-box-icons-alist 'company-box-icons-all-the-icons)
    (company-box-show-single-candidate nil))

  (use-package company-quickhelp
    :ensure t
    :hook (company-mode . company-quickhelp-mode))
  )

;; projectile
(use-package projectile
  :ensure t
  )

;; flycheck
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)

  :config
  (use-package flycheck-posframe
    :disabled
    :ensure t
    :hook (flycheck-mode . flycheck-posframe-mode))
  )

;; lsp
(use-package lsp-mode
  ;; :disabled
  :ensure t
  :commands lsp

  :custom
  (lsp-prefer-flymake nil)
  (lsp-document-sync-method 'incremental)
  (lsp-enable-snippet t)
  ;; (lsp-enable-snippet nil)
  (lsp-print-io t)

  :config
  (use-package lsp-ui
    :ensure t
    :hook (lsp-mode . lsp-ui-mode)
    
    :custom-face
    (lsp-ui-doc-background ((nil (:background "black"))))
    
    :custom
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-position 'bottom)
    (lsp-ui-doc-max-width 150)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit t)

    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand)

    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)

    (lsp-ui-flycheck-enable t)
    (lsp-ui-flycheck-list-position 'bottom)
    (lsp-ui-flycheck-live-reporting t)

    (lsp-ui-sideline-enable nil)
    
    :preface
    ;; https://ladicle.com/post/config/#lsp
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
    :ensure t
    :custom
    (company-lsp-cache-candidates nil)
    (company-lsp-async t)
    (company-lsp-enable-snippet t)
    (company-lsp-enable-recompletion t)
    (company-lsp-match-candidate-predicate 'company-lsp-match-candidate-flex)
    )
  )

;; Haskell
;; for ghc-8.0.2 and later
(use-package lsp-haskell
  :ensure t
  ;; :init
  ;; (add-to-list 'company-backends 'company-lsp)
  :hook
  (haskell-mode . lsp)
  :custom
  (lsp-haskell-process-path-hie "hie-wrapper"))


;; for ghc-7.10.3
;; (require 'haskell-mode nil t)
;; (require 'ghc nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
                                        ;(add-to-list 'company-backends 'company-ghc t)

(use-package dockerfile-mode
  :ensure t
  )

(use-package docker-compose-mode
  :ensure t
  )

;; elm
(use-package elm-mode
  :ensure t
  )

;; markdown
(use-package markdown-mode
  :ensure t
  )

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
