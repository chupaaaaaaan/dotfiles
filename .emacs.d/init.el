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
  (gc-cons-threshold (* 10 gc-cons-threshold))
  (mouse-yank-at-point t)
  )

;;TODO: 雑多な設定を整理する
;; maximize frame
(toggle-frame-maximized)

;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;; Font
;; Japanese font settings
(defun set-japanese-font (family)
  "Set japanese font by FAMILY."
  (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family family))
  (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0212 (font-spec :family family))
  (set-fontset-font (frame-parameter nil 'font) 'katakana-jisx0201 (font-spec :family family)))

;; Overwrite latin and greek char's font
(defun set-latin-and-greek-font (family)
  "Set default font by FAMILY."
  (set-fontset-font (frame-parameter nil 'font) '(#x0250 . #x02AF) (font-spec :family family)) ; IPA extensions
  (set-fontset-font (frame-parameter nil 'font) '(#x00A0 . #x00FF) (font-spec :family family)) ; latin-1
  (set-fontset-font (frame-parameter nil 'font) '(#x0100 . #x017F) (font-spec :family family)) ; latin extended-A
  (set-fontset-font (frame-parameter nil 'font) '(#x0180 . #x024F) (font-spec :family family)) ; latin extended-B
  (set-fontset-font (frame-parameter nil 'font) '(#x2018 . #x2019) (font-spec :family family)) ; end quote
  (set-fontset-font (frame-parameter nil 'font) '(#x2588 . #x2588) (font-spec :family family)) ; █
  (set-fontset-font (frame-parameter nil 'font) '(#x2500 . #x2500) (font-spec :family family)) ; ─
  (set-fontset-font (frame-parameter nil 'font) '(#x2504 . #x257F) (font-spec :family family)) ; box character
  (set-fontset-font (frame-parameter nil 'font) '(#x0370 . #x03FF) (font-spec :family family)))

(setq use-default-font-for-symbols nil)
(setq inhibit-compacting-font-caches t)
(setq jp-font-family "Ricty Diminished")
(setq default-font-family "all-the-icons")

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family jp-font-family :height 180))
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :family jp-font-family :height 150))
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :family jp-font-family :height 150))
(set-japanese-font jp-font-family)
(set-latin-and-greek-font default-font-family)
(add-to-list 'face-font-rescale-alist (cons default-font-family 0.86))
(add-to-list 'face-font-rescale-alist (cons jp-font-family 1.0))


;; locale and environment
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; OS
;; Mac
(when (eq system-type 'darwin)
  (require 'ucs-normalize nil t)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)
;;  (setq alert-default-style 'osx-notifier)
  )

;; GNU/Linux
(when (eq system-type 'gnu/linux)
  )

;; Windows
(when (eq system-type 'windows-nt)
  (setq file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932)

  ;; default path
  (setq default-directory (concat (getenv "HOMEPATH") "/"))
  (setq command-line-default-directory (concat (getenv "HOMEPATH") "/"))
  )


;; Keymap
(use-package keyfreq
  :ensure t
  :custom
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (keyfreq-buffer "*KeyFrequency*"))

(use-package which-key
  :ensure t
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

  :config
  (set-cursor-color "cyan")
  (line-number-mode 1)
  (column-number-mode 1)
  (doom-modeline-mode 1)
  (setq doom-modeline-buffer-file-name-style 'truncate-all)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-minor-modes nil)
  ;; (setq doom-modeline-height 20)
  ;; (setq doom-modeline-bar-width 3)
  ;; (setq doom-modeline-major-mode-color-icon t)

  ;; (doom-modeline-def-modeline 'main
  ;;   '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
  ;;   '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))
  )

(use-package dashboard
  :ensure t
  :diminish
  (dashboard-mode page-break-lines-mode))

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

;; scroll-lock
(use-package scroll-lock
  :ensure t
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
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)

  :bind
  (:map ivy-minibuffer-map
        ("<escape>" . minibuffer-keyboard-quit )))

(use-package ivy-hydra
  :ensure t
  :after (ivy hydra)
  :custom
  (ivy-read-action-function (function ivy-hydra-read-action)))

;; swiper (isearch)
(use-package swiper
  :diminish
  :ensure t
  :after ivy
  :bind
  ("C-s" . swiper)
  ("M-s M-s" . swiper-thing-at-point))

(use-package counsel
  :ensure t
  :after swiper
  :bind
  ("M-x" . counsel-M-x)
  ("M-y" . counsel-yank-pop)
  ("C-M-z" . counsel-fzf)
  ("C-M-r" . counsel-recentf)
  ("C-x C-b" . counsel-ibuffer)
  ("C-M-f" . counsel-ag)
  :config
  (counsel-mode 1))

;; anzu
(use-package anzu
  :diminish
  :ensure t
  :bind
  ("C-r"   . anzu-query-replace-regexp)
  ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (after-init . global-anzu-mode)
  :custom
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000)
  )
  
(use-package google-this
  :ensure t
  :bind
  ("M-i G" . google-this))

(use-package google-translate
  :ensure t
  :bind
  ("M-i t" . google-translate-at-point)
  ("M-i T" . google-translate-at-point-reverse)

  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "ja")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure t
  :custom
  ;; agenda-files
  (org-directory "~/Dropbox/org/")
  (inbox-file (concat org-directory "inbox.org"))
  (work-file (concat org-directory "work.org"))
  (review-file (concat org-directory "review.org"))
  (mtg-file (concat org-directory "mtg.org"))
  (schedule-file (concat org-directory "schedule.org"))
  (mylist-file (concat org-directory "mylist.org"))

  (org-default-notes-file (concat org-directory "notes.org"))
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-clocktable-default-properties '(:maxlevel 2 :scope agenda-with-archives :fileskip0 t :link t :block today :match ""))
  (org-clock-clocked-in-display 'frame-title)

  ;; (org-agenda-files '("~/Dropbox/org/inbox.org"
  ;;                     "~/Dropbox/org/work.org"
  ;;                     "~/Dropbox/org/review.org"
  ;;                     "~/Dropbox/org/mtg.org"
  ;;                     "~/Dropbox/org/schedule.org"
  ;;                     "~/Dropbox/org/mylist.org"))
  (org-agenda-files (list inbox-file
                          work-file
                          review-file
                          mtg-file
                          schedule-file
                          mylist-file))

  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "PENDING(p)" "|" "DONE(d)" "CANCELED(c)")))
  (org-capture-templates
   '(("t" "Write down the thoughts of this moment with a timestamp."      item  (file+headline ladicle/get-today-diary "Log")  "%(ladicle/org-get-time) %?\n"            :prepend 1)
     ("m" "Memorize something in the memo section of today's diary."      entry (file+headline ladicle/get-today-diary "Memo") "* %?\n"                                  :empty-lines 1 :jump-to-captured 1 :unnarrowed 1)
     ("i" "Create a general task to the inbox and jump to the task file." entry (file+headline inbox-file "Inbox")             "* TODO %?\n  %U\n%i\n"                   :empty-lines 1 :jump-to-captured 1)
     ("p" "Create an interrupt task to the inbox and start clocking."     entry (file+headline inbox-file "Inbox")             "* TODO %?\n  %U\n%i\n"                   :empty-lines 1 :clock-in 1 :clock-resume 1)
     ("s" "Add an event to the calendar."                                 entry (file+headline schedule-file "Schedule")       "* %?\n  SCHEDULED: <%(org-read-date)>\n" :empty-lines 1)
     ("h" "Collect hacking Emacs ideas!"                                  item  (file+headline inbox-file "Hacking Emacs")     "[ ] %?"                                  :prepend 1)
     ("w" "Wish list for my life!"                                        entry (file+headline mylist-file "My Wishes")        "* TODO %?"                               :prepend 1)
     ("l" "Store the link of the current position in the clocking task."  item  (clock)                                        "%A\n"                                    :immediate-finish 1 :prepend 1)))

  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("M-i l i" . (lambda () (interactive) (ladicle/open-org-file inbox-file)))
  ("M-i l s" . (lambda () (interactive) (ladicle/open-org-file schedule-file)))
  ("M-i l l" . (lambda () (interactive) (ladicle/open-org-file mylist-file)))
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
            (format "%s/%s" work-done-str effort-str))
        (format "%s" work-done-str))))

  (defun ladicle/org-clock-out-and-save-when-exit ()
    "Save buffers and stop clocking when kill emacs."
    (ignore-errors (org-clock-out) t)
    (save-some-buffers t)))

(use-package org-bullets
  :ensure t
  :after org
  :hook
  (org-mode . org-bullets-mode)

  ;; :custom
  ;; (org-bullets-bullet-list '("" "" "" "" "" "" "" "" "" ""))
  )


;; Pomodoro (from @ladicle)
(use-package org-pomodoro
  :ensure t
  :after org
  :custom
  (org-pomodoro-ask-upon-killing t)
  (org-pomodoro-keep-killed-pomodoro-time t)
  (org-pomodoro-format "%s") ;;     
  (org-pomodoro-short-break-format "%s")
  (org-pomodoro-long-break-format  "%s")

  :custom-face
  (org-pomodoro-mode-line ((t (:foreground "#ff5555"))))
  (org-pomodoro-mode-line-break   ((t (:foreground "#50fa7b"))))

  ;; :bind
  ;; (:map org-mode-map
  ;;       ("C-c p" . org-pomodoro))
  :bind
  ("C-c p" . org-pomodoro)

  :hook
  (org-pomodoro-started  . (lambda () (notifications-notify
                                       :title "org-pomodoro"
                                       :body "Let's focus for 25 minutes!")))
  (org-pomodoro-finished . (lambda () (notifications-notify
                                       :title "org-pomodoro"
                                       :body "Well done! Take a break.")))
  :config
  (when (eq system-type 'darwin)
    (setq alert-default-style 'osx-notifier))
  (require 'alert)
  )

(use-package org-mobile-sync
  :ensure t
  :after org
  :custom
  (org-mobile-directory "~/Dropbox/Apps/MobileOrg/")
  (org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org")
  :config
  (org-mobile-sync-mode t)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Develop Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git
(use-package git-timemachine
  :ensure t
  :bind
  ("M-g t" . git-timemachine-toggle))

(use-package diffview
  :ensure t
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
  :custom
  (magit-auto-revert-mode nil)
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
  :custom
  (git-gutter:modified-sign "=")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  :custom-face
  (git-gutter:modified ((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
  (git-gutter:added    ((t (:foreground "#50fa7b" :background "#50fa7b"))))
  (git-gutter:deleted  ((t (:foreground "#ff79c6" :background "#ff79c6"))))
  ;; (git-gutter:modified ((t (:foreground "#f1fa8c"))))
  ;; (git-gutter:added    ((t (:foreground "#50fa7b"))))
  ;; (git-gutter:deleted  ((t (:foreground "#ff79c6"))))
  :config
  (global-git-gutter-mode 1))

(use-package browse-at-remote
  :ensure t
  :bind
  ("M-g r" . browse-at-remote))

(use-package github-pullrequest
  :ensure t)


;; yasnippet
(use-package yasnippet
  ;; :disabled
  :ensure t
  :hook (after-init . yas-global-mode)

  :config
  (use-package yasnippet-snippets :ensure t)

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
  ;; (elm-mode . (lambda () (set (make-local-variable 'company-backends) '((company-yasnippet company-elm company-files)))))
  ;; using company by lsp-completion
  ((haskell-mode
    java-mode
    ;; elm-mode
    ) . (lambda () (set (make-local-variable 'company-backends) '((company-yasnippet company-lsp company-files)))))
  
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-s" . company-filter-candidates)
        ("<tab>" . company-complete)
        ;; ("<tab>" . company-complete)
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-h" . nil)
        :map company-search-map
        ("<tab>" . company-complete)
        ;; ("<tab>" . company--insert-candidate)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ;; ("C-s" . company-select-next)
        ;; ("C-r" . company-select-previous)
        )
  
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
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c f" . projectile-command-map))
  :config
  (projectile-mode 1)
  )

;; treemacs
(use-package treemacs
  :ensure t
  :bind
  ("M-0"       . treemacs-select-window)
  ("C-x t 1"   . treemacs-delete-other-windows)
  ("C-x t t"   . treemacs)
  ("C-x t B"   . treemacs-bookmark)
  ;; ("C-x t C-t" . treemacs-find-file)
  ("C-x t M-t" . treemacs-find-tag)

  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'simple)

  (use-package treemacs-icons-dired
    :after treemacs dired
    :ensure t
    :config
    (treemacs-icons-dired-mode))
  )


;; flycheck
(use-package flycheck
  :ensure t
  ;; :disabled
  :hook ((
          emacs-lisp-mode
          ) . flycheck-mode))


(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode))


;; lsp
(use-package lsp-mode
  :ensure t
  :commands lsp

  :custom
  (lsp-prefer-flymake nil)
  (lsp-document-sync-method 'incremental)
  (lsp-enable-snippet t)
  (lsp-print-io t)

  :bind
  (:map lsp-mode-map
        ("C-c l" . lsp-lens-mode)))


(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  
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
        ("C-c C-j" . lsp-ui-peek-find-definitions)
        ("C-c C-i" . lsp-ui-peek-find-implementation)
        ("C-c m"   . lsp-ui-imenu)
        ("C-c s"   . lsp-ui-sideline-mode)
        ("C-c d"   . ladicle/toggle-lsp-ui-doc)))


(use-package company-lsp
  :ensure t
  :after company lsp-mode
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates nil)
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  (company-lsp-enable-recompletion t)
  (company-lsp-match-candidate-predicate 'company-lsp-match-candidate-flex)
  ;; :config
  ;; (add-to-list 'company-backends 'company-lsp)
  )

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode treemacs
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :ensure t
  :config
  (dap-mode t)
  (dap-ui-mode t))

;; Java (STS4)
;; (setq chupaaaaaaan:lombok-path (expand-file-name (concat user-emacs-directory "eclipse.jdt.ls/server/boot-server/lombok.jar")))
(use-package lsp-java-boot
  :ensure lsp-java
  :defer t
  ;; :custom
  ;; (lsp-java-vmargs (list "-noverify"
  ;;                        "-Xmx1G"
  ;;                        "-XX:+UseG1GC"
  ;;                        "-XX:+UseStringDeduplication"
  ;;                        (concat "-javaagent:" chupaaaaaaan:lombok-path)
  ;;                        (concat "-Xbootclasspath/a:" chupaaaaaaan:lombok-path)))

  :hook
  (java-mode . lsp)
  (java-mode . lsp-java-boot-lens-mode)
  (java-mode . flycheck-mode))

(use-package dap-java
  :after lsp-java)

;; Haskell
;; for ghc-8.0.2 and later
(use-package lsp-haskell
  :ensure t
  :defer t
  ;; :init
  ;; (add-to-list 'company-backends 'company-lsp)
  :hook
  (haskell-mode . lsp)
  (haskell-mode . flycheck-mode)

  :custom
  (lsp-haskell-process-path-hie "hie-wrapper"))

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
  :hook
  (elm-mode . flycheck-mode)
  :config
  (add-to-list 'company-backends 'company-elm))

(use-package flycheck-elm
  :ensure t
  :after flycheck elm
  ;; :bind
  ;; (:map elm-mode-map
  ;;       ("C-c C-f" . elm-format-buffer))
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

(use-package dockerfile-mode :ensure t)

(use-package docker-compose-mode :ensure t)

(use-package markdown-mode :ensure t)



;; load customize file
(load custom-file t)
