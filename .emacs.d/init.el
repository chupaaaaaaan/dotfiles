;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load-path setting
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((elisp-dir
             (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path elisp-dir)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; add directories under "~/.emacs.d/" to load-path
(add-to-load-path "public_repos")

;; separate customize file
(setq custom-file "~/.emacs.d/customize.el")

;; load local configures
(dolist (lcnf (directory-files (concat user-emacs-directory "local_conf") t "^[^_].+\\.el$"))
  (load-file lcnf))

;; package.el
(require 'package nil t)
(package-initialize)

;; add repositories
(add-to-list 'package-archives '("org"           . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable"  . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa"         . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package use-package
  :custom
  (use-package-compute-statistics t)
  ;; (use-package-always-defer nil)

  ;; customize general settings
  (inhibit-startup-screen -1)
  (menu-bar-mode nil)
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (indent-tabs-mode nil)
  (transient-mark-mode t)
  (savehist-mode t)
  (history-delete-duplicates t)
  (history-length 1000)
  (message-log-max 10000)
  ;(gc-cons-threshold (* 10 gc-cons-threshold))
  (mouse-yank-at-point t))

;;TODO: 雑多な設定を整理する
;; maximize frame
(toggle-frame-maximized)

;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;; Font
;; ;; all-the-icons
;; (use-package all-the-icons-dired
;;   :ensure t
;;   :hook
;;   (dired-mode . all-the-icons-dired-mode))

;; (use-package all-the-icons-ivy
;;   :ensure t
;;   :after ivy
;;   :config
;;   (all-the-icons-ivy-setup))

(defun chupaaaaaaan:font-setting ()
  "Initialize fonts on window-system"
  (interactive)

  (cond
   ((or (eq window-system 'x)
        (eq window-system 'w32)
        (eq window-system 'ns))
    (let* ((size my:font-size)
           (family my:font-family)
           (h (round (* size 10))))
      (set-face-attribute 'default nil :family family :height h)
      (set-fontset-font nil 'unicode           (font-spec :family family) nil 'append)
      (set-fontset-font nil 'japanese-jisx0208 (font-spec :family family) nil 'append)
      (set-fontset-font nil 'japanese-jisx0212 (font-spec :family family) nil 'append)
      (set-fontset-font nil 'katakana-jisx0201 (font-spec :family family) nil 'append)
      (add-to-list 'face-font-rescale-alist (cons family 1.0))
      (when (featurep 'all-the-icons)
        (set-fontset-font nil 'unicode (font-spec :family "all-the-icons")   nil 'append)
        (set-fontset-font nil 'unicode (font-spec :family "Material Icons")  nil 'append)
        (set-fontset-font nil 'unicode (font-spec :family "FontAwesome")     nil 'append)
        (set-fontset-font nil 'unicode (font-spec :family "file-icons")      nil 'append)
        (set-fontset-font nil 'unicode (font-spec :family "github-octicons") nil 'append)
        (set-fontset-font nil 'unicode (font-spec :family "Weather Icons")   nil 'append)
        (add-to-list 'face-font-rescale-alist (cons "FontAwesome" 0.85))
        (add-to-list 'face-font-rescale-alist (cons "file-icons" 0.85))
        (add-to-list 'face-font-rescale-alist (cons "github-octicons" 0.85)))
      (message (format "Setup for %s with %f" family size))))
   (t
    (message "Not have window-system"))))

(setq use-default-font-for-symbols nil)
(setq inhibit-compacting-font-caches t)

(add-hook 'after-init-hook #'chupaaaaaaan:font-setting)


;; locale and environment
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; OS
;; Mac
(when (eq system-type 'darwin)
  (require 'ucs-normalize nil t)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)
  (use-package exec-path-from-shell
    :ensure t
    :init
    (exec-path-from-shell-initialize)))

;; GNU/Linux
(when (eq system-type 'gnu/linux))

;; Windows
(when (eq system-type 'windows-nt)
  (setq file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932)

  ;; default path
  (setq default-directory (concat (getenv "HOMEPATH") "/"))
  (setq command-line-default-directory (concat (getenv "HOMEPATH") "/")))


;; Keymap
(use-package keyfreq
  :ensure t
  :demand t
  :custom
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (keyfreq-buffer "*KeyFrequency*"))

;; which-key
(use-package which-key
  :ensure t
  :demand t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;; define prefix-key
(define-prefix-command 'ladicle-window-map)
(define-key global-map (kbd "M-i") 'ladicle-window-map)

(define-prefix-command 'ladicle-toggle-map)
(define-key global-map (kbd "M-t") 'ladicle-toggle-map)

(define-prefix-command 'ladicle-link-map)
(define-key global-map (kbd "M-i l") 'ladicle-link-map)


(global-unset-key (kbd "C-x C-c"))
(defalias 'exit 'save-buffers-kill-emacs)

;; Settings that do not depend on some major modes or minor modes
(global-set-key (kbd "C-h")   'delete-backward-char)
(global-set-key (kbd "M-t l") 'toggle-truncate-lines)
(global-set-key (kbd "C-t")   'other-window)
(global-set-key [f6] (lambda () (interactive) (counsel-M-x "^counsel ")))


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

(use-package doom-modeline
  :ensure t
  :demand t
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  ;; (doom-modeline-display-default-persp-name t)
  ;; (doom-modeline-mode t)
  ;; (doom-modeline-icon t)
  ;; (doom-modeline-major-mode-icon t)
  ;; (doom-modeline-major-mode-color-icon t)
  ;; (doom-modeline-minor-modes nil)
  :config
  (set-cursor-color "cyan")
  (line-number-mode 1)
  (column-number-mode 1))

(use-package hide-mode-line
  :ensure t
  :demand t
  :disabled
  :hook
  (treemacs-mode . hide-mode-line-mode))

(use-package dashboard
  :ensure t
  :diminish
  (dashboard-mode page-break-lines-mode)
  :custom
  (dashboard-center-content nil)
  (dashboard-set-file-icons t)
  ;; (dashboard-org-agenda-categories t)
  (dashboard-set-heading-icons t)
  ;; (dashboard-set-navigator t)
  ;; (dashboard-startup-banner 4)
  (dashboard-items '((recents . 15)
                     (agenda . 10)
                     (projects . 5)
                     (bookmarks . 5)))
  :custom-face
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :hook
  (after-init . dashboard-setup-startup-hook))

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
  :custom-face
  ;; (show-paren-match ((nil (:underline "#ff5555"))))
  (show-paren-match ((nil (:background "#44475a" :foreground "#f1fa8c"))))
  :custom
  (show-paren-mode t)
  (show-paren-delay 0)
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package rainbow-delimiters
  :ensure t
  :demand t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :ensure t
  :demand t
  :diminish
  :preface
  (defun toggle-highlight-indent-guides ()
    "Toggle highlight indent guides."
    (interactive)
    (highlight-indent-guides-mode (if highlight-indent-guides-mode -1 1))
    (message "Highlight indent guides %s" (if highlight-indent-guides-mode "enabled" "disabled")))
  :bind
  ("M-t i" . toggle-highlight-indent-guides)
  ;; :hook
  ;; ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-character 124)
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character))

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
  :custom
  (nyan-cat-face-number 4)
  (nyan-animate-nyancat t)
  :config
  (nyan-mode 1))

;; datetime format
(use-package time
  :ensure t
  :demand t
  :custom
  (display-time-interval 60)
  (display-time-format " %m/%d %H:%M ")
  :config
  (display-time-mode t))

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

;; Recent files
(use-package recentf
  :ensure nil
  :defer t
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 20000000)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     "cache"
                     "recentf"
                     "COMMIT_EDITMSG\\'"))
  :preface
  (defun ladicle/recentf-save-list-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if (fboundp 'shut-up)
          (shut-up (recentf-save-list))
        (recentf-save-list)))
    (message ""))
  (defun ladicle/recentf-cleanup-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if shutup-p
          (shut-up (recentf-cleanup))
        (recentf-cleanup)))
    (message ""))
  :hook
  (focus-out-hook . (ladicle/recentf-save-list-silence ladicle/recentf-cleanup-silence)))


;; scroll-lock
(use-package scroll-lock
  :ensure t
  :defer t
  :disabled
  :init
  (defun toggle-scroll-lock ()
    "Toggle scroll lock."
    (interactive)
    (scroll-lock-mode (if scroll-lock-mode -1 1))
    (message "Scroll lock %s" (if scroll-lock-mode "enabled" "disabled")))

  :bind
  ("M-t m" . toggle-scroll-lock))

;; ediff
;; (when (executable-find "diff")
;;   (require 'ediff nil t)
;;   (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
;;   (setq-default ediff-split-window-function 'split-window-horizontally)
;;   (global-set-key (kbd "C-c d") 'ediff-files)
;;   )

;; undo-tree
(use-package undo-tree
  :ensure t
  :demand t
  :diminish t
  :config
  (global-undo-tree-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search / Replace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hydra
(use-package hydra
  :ensure t
  :defer t)

(use-package ivy-rich
  :ensure t
  :defer t
  :after ivy
  :defines (all-the-icons-dir-icon-alist bookmark-alist)
  :functions (all-the-icons-icon-family
              all-the-icons-match-to-alist
              all-the-icons-auto-mode-match?
              all-the-icons-octicon
              all-the-icons-dir-is-submodule)
  :preface
  (defun ivy-rich-bookmark-name (candidate)
    (car (assoc candidate bookmark-alist)))

  (defun ivy-rich-repo-icon (candidate)
    "Display repo icons in `ivy-rich`."
    (all-the-icons-octicon "repo" :height .9))

  (defun ivy-rich-org-capture-icon (candidate)
    "Display repo icons in `ivy-rich`."
    (pcase (car (last (split-string (car (split-string candidate)) "-")))
      ("emacs" (all-the-icons-fileicon "emacs" :height .68 :v-adjust .001))
      ("schedule" (all-the-icons-faicon "calendar" :height .68 :v-adjust .005))
      ("tweet" (all-the-icons-faicon "commenting" :height .7 :v-adjust .01))
      ("link" (all-the-icons-faicon "link" :height .68 :v-adjust .01))
      ("memo" (all-the-icons-faicon "pencil" :height .7 :v-adjust .01))
      (_       (all-the-icons-octicon "inbox" :height .68 :v-adjust .01))
      ))

  (defun ivy-rich-org-capture-title (candidate)
    (let* ((octl (split-string candidate))
           (title (pop octl))
           (desc (mapconcat 'identity octl " ")))
      (format "%-25s %s"
              title
              (propertize desc 'face `(:inherit font-lock-doc-face)))))

  (defun ivy-rich-buffer-icon (candidate)
    "Display buffer icons in `ivy-rich'."
    (when (display-graphic-p)
      (when-let* ((buffer (get-buffer candidate))
                  (major-mode (buffer-local-value 'major-mode buffer))
                  (icon (if (and (buffer-file-name buffer)
                                 (all-the-icons-auto-mode-match? candidate))
                            (all-the-icons-icon-for-file candidate)
                          (all-the-icons-icon-for-mode major-mode))))
        (if (symbolp icon)
            (setq icon (all-the-icons-icon-for-mode 'fundamental-mode)))
        (unless (symbolp icon)
          (propertize icon 'face `(:height 1.1 :family ,(all-the-icons-icon-family icon)))))))

  (defun ivy-rich-file-icon (candidate)
    "Display file icons in `ivy-rich'."
    (when (display-graphic-p)
      (let ((icon (if (file-directory-p candidate)
                      (cond
                       ((and (fboundp 'tramp-tramp-file-p)
                             (tramp-tramp-file-p default-directory))
                        (all-the-icons-octicon "file-directory"))
                       ((file-symlink-p candidate)
                        (all-the-icons-octicon "file-symlink-directory"))
                       ((all-the-icons-dir-is-submodule candidate)
                        (all-the-icons-octicon "file-submodule"))
                       ((file-exists-p (format "%s/.git" candidate))
                        (all-the-icons-octicon "repo"))
                       (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
                            (apply (car matcher) (list (cadr matcher))))))
                    (all-the-icons-icon-for-file candidate))))
        (unless (symbolp icon)
          (propertize icon
                      'face `(
                              :height 1.1
                              :family ,(all-the-icons-icon-family icon)
                              ))))))
  :hook
  (ivy-mode . ivy-rich-mode)
  (ivy-rich-mode . (lambda ()
                     (setq ivy-virtual-abbreviate
                           (or (and ivy-rich-mode 'abbreviate) 'name))))
  :init
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          ivy-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 45))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 45))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-find-file
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-file-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-dired-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-git
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-recentf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate (:width 110))))
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type)
            (ivy-rich-bookmark-name (:width 30))
            (ivy-rich-bookmark-info (:width 80))))
          counsel-projectile-switch-project
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-fzf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          ivy-ghq-open
          (:columns
           ((ivy-rich-repo-icon)
            (ivy-rich-candidate)))
          ivy-ghq-open-and-fzf
          (:columns
           ((ivy-rich-repo-icon)
            (ivy-rich-candidate)))
          counsel-projectile-find-file
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-org-capture
          (:columns
           ((ivy-rich-org-capture-icon)
            (ivy-rich-org-capture-title)
            ))
          counsel-projectile-find-dir
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-dir-transformer)))))
  :custom
  (ivy-rich-parse-remote-buffer nil)
  :config
  (ivy-rich-mode 1))

(use-package ivy-hydra
  :ensure t
  :defer t
  :after ivy hydra
  :custom
  (ivy-read-action-function (function ivy-hydra-read-action)))

(use-package counsel
  :ensure t
  :defer t
  :diminish ivy-mode counsel-mode
  :preface
  (defun ivy-format-function-pretty (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat
        (all-the-icons-faicon "hand-o-right" :height .85 :v-adjust .05 :face 'font-lock-constant-face)
        (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat "  " str))
     cands
     "\n"))
  :custom
  (ivy-truncate-lines nil)
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  (ivy-on-del-error-function nil)
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (swiper-action-recenter t)
  (counsel-yank-pop-height 15)
  :hook
  (after-init . ivy-mode)
  (ivy-mode . counsel-mode)
  :bind
  ("C-s" . swiper)
  ("M-s M-s" . swiper-thing-at-point)
  ("M-x" . counsel-M-x)
  ("M-y" . counsel-yank-pop)
  ("C-M-z" . counsel-fzf)
  ("C-M-r" . counsel-recentf)
  ("C-M-f" . counsel-ag)
  ("C-x C-b" . counsel-ibuffer)
  (:map ivy-minibuffer-map
        ("C-w" . ivy-backward-kill-word)
        ("C-k" . ivy-kill-line)
        ("C-j" . ivy-immediate-done)
        ("RET" . ivy-alt-done)
        ("C-h" . ivy-backward-delete-char)
        ("<escape>" . minibuffer-keyboard-quit))
  :config
  (setq ivy-format-function 'ivy-format-function-pretty)
  (setq counsel-yank-pop-separator
        (propertize "\n────────────────────────────────────────────────────────\n"
                    'face `(:foreground "#6272a4")))
  )

;; anzu
(use-package anzu
  :ensure t
  :defer t
  :diminish
  :bind
  ("C-r" . anzu-query-replace-regexp)
  ;; ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode)
  :custom
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000))

(use-package google-this
  :ensure t
  :defer t
  :bind
  ("M-i G" . google-this))

(use-package google-translate
  :ensure t
  :defer t
  :bind
  ("M-i t" . google-translate-at-point)
  ("M-i T" . google-translate-at-point-reverse)

  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure t
  :defer t
  :custom
  (org-directory "~/Dropbox/org/")
  ;; agenda-files
  (inbox-file (concat org-directory "inbox.org"))
  (schedule-file (concat org-directory "schedule.org"))
  (org-agenda-files (list inbox-file schedule-file))

  (org-default-notes-file (concat org-directory "notes.org"))
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-clocktable-default-properties '(:maxlevel 2 :scope agenda :fileskip0 t :link t :block today :match ""))
  (org-clock-clocked-in-display 'mode-line) ;; 'frame-title

  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "PENDING(p)" "|" "DONE(d)" "CANCELED(c)")))
  (org-capture-templates my:org-capture-templates)

  :bind
  ("C-c c" . counsel-org-capture)
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("M-i l i" . (lambda () (interactive) (ladicle/open-org-file inbox-file)))
  ("M-i l s" . (lambda () (interactive) (ladicle/open-org-file schedule-file)))
  ("M-i l l" . (lambda () (interactive) (ladicle/open-org-file mylist-file)))
  ("M-i l o" . (lambda () (interactive) (ladicle/open-org-file objective-file)))
  ("M-i l y" . (lambda () (interactive) (ladicle/open-org-file (ladicle/get-yesterday-diary))))
  ("M-i l p" . (lambda () (interactive) (ladicle/open-org-file (ladicle/get-diary-from-cal))))
  ("M-i l t" . (lambda () (interactive) (ladicle/open-org-file (ladicle/get-today-diary))))
  (:map org-mode-map
        ;; ("C-c i" . org-clock-in)
        ;; ("C-c o" . org-clock-out)
        ;; ("C-c u" . org-dblock-update)
        ("C-c n" . org-narrow-to-subtree)
        ("C-c b" . org-narrow-to-block)
        ("C-c w" . widen)
        ("C-c e" . org-set-effort))

  :hook
  (kill-emacs . ladicle/org-clock-out-and-save-when-exit)
  (org-clock-in . (lambda ()
                    (setq org-mode-line-string (ladicle/task-clocked-time))
                    (run-at-time 0 60 '(lambda ()
                                         (setq org-mode-line-string (ladicle/task-clocked-time))
                                         (force-mode-line-update)))
                    (force-mode-line-update)))
  (org-mode . (lambda ()
                (dolist (key '("C-'" "C-," "C-."))
                  (unbind-key key org-mode-map))))
  (org-agenda-mode . (lambda ()
                       (setq org-agenda-weekday-num (string-to-number (format-time-string "%u" (current-time))))
                       (custom-set-variables '(org-agenda-start-on-weekday org-agenda-weekday-num))))


  :preface
  (defun ladicle/get-today-diary ()
    (concat org-directory (format-time-string "diary/%Y-%m-%d.org" (current-time))))
  (defun ladicle/get-yesterday-diary ()
    (concat org-directory (format-time-string "diary/%Y-%m-%d.org" (time-add (current-time) (* -24 3600)))))
  (defun ladicle/get-diary-from-cal ()
    (concat org-directory (format-time-string "diary/%Y-%m-%d.org" (apply 'encode-time (parse-time-string (concat (org-read-date) " 00:00"))))))
  (defun ladicle/open-org-file (fname)
    (switch-to-buffer (find-file-noselect fname)))
  (defun ladicle/org-get-time ()
    (format-time-string "<%H:%M>" (current-time)))
  (defun ladicle/task-clocked-time ()
    "Return a string with the clocked time and effort, if any"
    (interactive)
    (let* ((clocked-time (org-clock-get-clocked-time))
           (h (truncate clocked-time 60))
           (m (mod clocked-time 60))
           (work-done-str (format "%d:%02d" h m)))
      (if org-clock-effort
          (let* ((effort-in-minutes
                  (org-duration-to-minutes org-clock-effort))
                 (effort-h (truncate effort-in-minutes 60))
                 (effort-m (truncate (mod effort-in-minutes 60)))
                 (effort-str (format "%d:%02d" effort-h effort-m)))
            (format " %s/%s" work-done-str effort-str))
        (format " %s" work-done-str))))

  (defun ladicle/org-clock-out-and-save-when-exit ()
    "Save buffers and stop clocking when kill emacs."
    (ignore-errors (org-clock-out) t)
    (save-some-buffers t)))

(use-package org-bullets
  :ensure t
  :defer t
  :after org
  :hook
  (org-mode . org-bullets-mode)

  :custom
  (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" "")))

;; Pomodoro (from @ladicle)
(use-package org-pomodoro
  :ensure t
  :defer t
  :after org-agenda
  :custom
  (org-pomodoro-ask-upon-killing t)
  (org-pomodoro-keep-killed-pomodoro-time t)
  (org-pomodoro-format "%s") ;;  
  (org-pomodoro-short-break-format "%s")
  (org-pomodoro-long-break-format  "%s")

  :custom-face
  (org-pomodoro-mode-line ((t (:foreground "#ff5555"))))
  (org-pomodoro-mode-line-break   ((t (:foreground "#50fa7b"))))

  ;; :bind
  ;; (:map org-mode-map
  ;;       ("C-c p" . org-pomodoro))
  :bind
  (:map org-agenda-mode-map
        ("p" . org-pomodoro))

  ;; :preface
  ;; (defun chupaaaaaaan:pomodoro-notify (title body)
  ;;   "Save buffers and stop clocking when kill emacs."
  ;;   (cond
  ;;    ((eq system-type 'darwin)
  ;;     (notifications-notify :title title :body body))
  ;;    ((eq system-type 'gnu/linux)
  ;;     (notifications-notify :title title :body body))
  ;;    ((eq system-type 'windows-nt)
  ;;     (w32-notification-notify :title title :body body))))

  ;; :hook
  ;; (org-pomodoro-started  . (lambda () (chupaaaaaaan:pomodoro-notify "org-pomodoro" "Let's focus for 25 minutes!")))
  ;; (org-pomodoro-finished  . (lambda () (chupaaaaaaan:pomodoro-notify "org-pomodoro" "Well done! Take a break.")))
  ;; (org-pomodoro-started  . (lambda () (notify :title "org-pomodoro" :body "Let's focus for 25 minutes!")))
  ;; (org-pomodoro-finished . (lambda () (notify :title "org-pomodoro" :body "Well done! Take a break.")))

  ;; :config
  ;; (when (eq system-type 'darwin)
  ;;   (setq alert-default-style 'osx-notifier))
  ;; (require 'alert)
  )

(use-package org-mobile-sync
  :ensure t
  :defer t
  :after org
  :custom
  (org-mobile-directory "~/Dropbox/Apps/MobileOrg/")
  (org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org")
  :config
  (org-mobile-sync-mode t))

(use-package org-re-reveal
  :ensure t
  :defer t
  :after org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Develop Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git
(use-package git-timemachine
  :ensure t
  :defer t
  :bind
  ("M-g t" . git-timemachine-toggle))

(use-package diffview
  :ensure t
  :defer t
  :commands (diffview-region diffview-current)
  :preface
  (defun ladicle/diffview-dwim ()
    (interactive)
    (if (region-active-p)
        (diffview-region)
      (diffview-current)))
  :bind
  ("M-g v" . ladicle/diffview-dwim))

(use-package magit
  :ensure t
  :defer t
  :custom
  (magit-auto-revert-mode nil)
  (magit-completing-read-function 'ivy-completing-read)
  :bind
  ("M-g s" . magit-status))

(use-package gitattributes-mode
  :ensure t
  :defer t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package git-gutter
  :ensure t
  :defer t
  :hook
  (after-init . global-git-gutter-mode)
  :custom
  (git-gutter:modified-sign "=")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  :custom-face
  ;; (git-gutter:modified ((t (:foreground "#f1fa8c"))))
  ;; (git-gutter:added    ((t (:foreground "#50fa7b"))))
  ;; (git-gutter:deleted  ((t (:foreground "#ff79c6"))))
  (git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
  (git-gutter:added    ((t (:foreground "#50fa7b" :background "#50fa7b"))))
  (git-gutter:deleted  ((t (:foreground "#ff79c6" :background "#ff79c6")))))

(use-package browse-at-remote
  :ensure t
  :defer t
  :bind
  ("M-g r" . browse-at-remote))

(use-package github-pullrequest
  :ensure t
  :defer t
  :disabled)


;; yasnippet
(use-package yasnippet
  ;; :disabled
  :ensure t
  :defer t
  :hook
  (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets :ensure t))

;; company
(use-package company
  :ensure t
  :defer t
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)

  :hook
  (after-init . global-company-mode)
  
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-s" . company-filter-candidates)
        ("<tab>" . company-complete-common-or-cycle)
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-h" . nil))
  (:map company-search-map
        ("<tab>" . company-complete-common-or-cycle)
        ("C-s" . company-select-next)
        ("C-r" . company-select-previous)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)))

(use-package company-box
  :ensure t
  :defer t
  :after company
  :diminish
  :hook
  (company-mode . company-box-mode)
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-show-single-candidate nil))

(use-package company-quickhelp
  :ensure t
  :defer t
  :after company
  :hook
  (global-company-mode . company-quickhelp-mode))

;; projectile
(use-package projectile
  :ensure t
  :defer t
  :bind
  ("C-c p" . projectile-command-map)
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode 1))

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :bind
  ("M-0"       . treemacs-select-window)
  ("C-x t 1"   . treemacs-delete-other-windows)
  ("C-x t t"   . treemacs)
  ("C-x t B"   . treemacs-bookmark)
  ("C-x t C-t" . treemacs-find-file)
  ("C-x t M-t" . treemacs-find-tag)

  :custom
  (treemacs-is-never-other-window t)
  (treemacs-no-delete-other-windows nil)

  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'simple))

(use-package counsel-projectile
  :ensure t
  :after counsel projectile
  :custom
  (projectile-completion-system 'ivy)
  (counsel-projectile-sort-files t)
  (counsel-projectile-sort-projects t)
  :config
  (counsel-projectile-mode 1))

(use-package treemacs-icons-dired
  :ensure t
  :defer t
  :after treemacs dired
  :config
  (treemacs-icons-dired-mode))


;; flycheck
(use-package flycheck
  :ensure t
  :defer t
  ;; :disabled
  :hook
  ;; (emacs-lisp-mode . flycheck-mode)
  ;; (haskell-mode . flycheck-mode)
  ;; (java-mode . flycheck-mode)
  ;; (elm-mode . flycheck-mode)
  (after-init . global-flycheck-mode))


(use-package flycheck-posframe
  :ensure t
  :defer t
  :after flycheck
  :hook
  (flycheck-mode . flycheck-posframe-mode))


;; lsp
(use-package lsp-mode
  :ensure t
  :defer t
  :commands lsp

  :custom
  (lsp-prefer-flymake nil)
  (lsp-document-sync-method 'incremental)
  (lsp-enable-snippet t)
  (lsp-print-io t)

  :hook
  (haskell-mode . lsp)
  ;; (java-mode . lsp)

  :bind
  (:map lsp-mode-map
        ("C-c C-l" . lsp-lens-mode)))


(use-package lsp-ui
  :ensure t
  :defer t
  :after lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  
  ;; :custom-face
  ;; (lsp-ui-doc-background ((nil (:background "black"))))
  
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)

  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand)

  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)

  (lsp-ui-flycheck-enable t)
  ;; (lsp-ui-flycheck-list-position 'right)
  (lsp-ui-flycheck-list-position 'bottom)
  (lsp-ui-flycheck-live-reporting t)

  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions nil)
  
  :preface
  ;; https://ladicle.com/post/config/#lsp
  (defun ladicle/toggle-lsp-ui-doc ()
    "Toggle Lsp-Ui-Doc mode."
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1))
    (message "Lsp-Ui-Doc mode %s in current buffer" (if lsp-ui-doc-mode "enabled" "disabled")))
  
  :bind
  (:map lsp-mode-map
        ("C-c C-r" . lsp-ui-peek-find-references)
        ("C-c C-d" . lsp-ui-peek-find-definitions)
        ("C-c C-i" . lsp-ui-peek-find-implementation)
        ("C-c m"   . lsp-ui-imenu)
        ("C-c s"   . lsp-ui-sideline-mode)
        ("C-c d"   . ladicle/toggle-lsp-ui-doc)))


(use-package company-lsp
  :ensure t
  :defer t
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates nil)
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  (company-lsp-enable-recompletion t)
  (company-lsp-match-candidate-predicate 'company-lsp-match-candidate-flex)
  :config
  (add-to-list 'company-backends 'company-lsp))


(use-package lsp-treemacs
  :ensure t
  :defer t
  :after lsp-mode treemacs
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :ensure t
  :defer t
  :config
  (dap-mode t)
  (dap-ui-mode t))

;; Java (STS4)
(use-package lsp-java-boot
  :ensure lsp-java
  :disabled
  :defer t
  ;; :init
  ;; (setq chupaaaaaaan:lombok-path (expand-file-name (concat user-emacs-directory "eclipse.jdt.ls/server/boot-server/lombok.jar")))
  ;; :custom
  ;; (lsp-java-vmargs (list "-noverify"
  ;;                        "-Xmx1G"
  ;;                        "-XX:+UseG1GC"
  ;;                        "-XX:+UseStringDeduplication"
  ;;                        (concat "-javaagent:" chupaaaaaaan:lombok-path)
  ;;                        (concat "-Xbootclasspath/a:" chupaaaaaaan:lombok-path)))

  :hook
  (java-mode . lsp-java-boot-lens-mode))

(use-package dap-java
  :disabled
  :defer t)


(use-package meghanada
  :ensure t
  :defer t
  :hook
  (java-mode . (lambda ()
                 (meghanada-mode t)
                 ;; (meghanada-telemetry-enable t)
                 (setq c-basic-offset 2)
                 ;; use code format
                 (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
  :bind
  (:map meghanada-mode-map
        ("C-c C-c C-m" . meghanada-exec-main))
  :config
  (cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn"))))

;; Haskell
;; for ghc-8.0.2 and later
(use-package lsp-haskell
  :ensure t
  :defer t
  :after lsp-mode haskell-mode
  :custom
  (lsp-haskell-process-path-hie "hie-wrapper"))

(use-package flycheck-haskell
  :ensure t
  :defer t
  :disabled
  :hook
  (flycheck-mode . flycheck-haskell-setup))

(use-package terraform-mode
  :ensure t
  :defer t
  :custom
  (terraform-indent-level 4)
  :config
  (add-to-list 'company-backends 'company-terraform))

(use-package elm-mode
  :ensure t
  :defer t
  :custom
  (elm-package-json "elm.json")
  :config
  (add-to-list 'company-backends 'company-elm))

(use-package flycheck-elm
  :ensure t
  :defer t
  ;; :bind
  ;; (:map elm-mode-map
  ;;       ("C-c C-f" . elm-format-buffer))
  :hook
  (flycheck-mode . flycheck-elm-setup))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package docker-compose-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package rjsx-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode)))

(use-package lsp-python-ms
  :ensure t
  :defer t
  :if (eq system-type 'windows-nt)
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-python-ms)
                   (lsp))))

;; load customize file
(load custom-file t)
