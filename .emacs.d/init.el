;;; package --- Summary

;;; Commentary:

;; chupaaaaaaan's Emacs settings.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-and-compile
  ;; directory
  (defconst chpn/dir-cache "~/.cache/emacs/")
  (unless (file-directory-p chpn/dir-cache)
    (make-directory chpn/dir-cache t))

  (defconst chpn/dir-jars "~/.cache/jars/")
  (unless (file-directory-p chpn/dir-jars)
    (make-directory chpn/dir-jars t))

  (defconst chpn/dir-pkg-elpa "~/.elisp/elpa/")
  (defconst chpn/dir-pkg-elget "~/.elisp/el-get/")
  (defconst chpn/dir-pkg-local "~/.elisp/local/")
  (unless (file-directory-p chpn/dir-pkg-local)
    (make-directory chpn/dir-pkg-local t)))

(eval-and-compile
  (add-to-list 'load-path chpn/dir-pkg-local)
  (require 'local-proxy-conf nil t)

  (customize-set-variable
   'package-archives '(("org"          . "https://orgmode.org/elpa/")
                       ("melpa"        . "https://melpa.org/packages/")
                       ("melpa-stable" . "https://stable.melpa.org/packages/")
                       ("gnu"          . "https://elpa.gnu.org/packages/")))
  (customize-set-variable 'package-user-dir chpn/dir-pkg-elpa)
  (customize-set-variable 'package-gnupghome-dir (concat chpn/dir-pkg-elpa "gnupg/"))
  (customize-set-variable 'gnutls-algorithm-priority  "normal:-vers-tls1.3")

  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get
      :ensure t
      :custom
      `((el-get-dir . ,chpn/dir-pkg-elget)))
    (leaf blackout :ensure t)
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree :ensure t))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf transient-dwim
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch))
  :custom
  `((transient-history-file . ,(concat chpn/dir-cache "transient-history.el"))
    (transient-levels-file . ,(concat chpn/dir-cache "transient-levels.el"))
    (transient-values-file . ,(concat chpn/dir-cache "transient-values.el"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; define prefix-key
(defvar chpn-function-map)
(define-prefix-command 'chpn-function-map)
(define-key global-map (kbd "M-i") 'chpn-function-map)

(defvar chpn-toggle-map)
(define-prefix-command 'chpn-toggle-map)
(define-key global-map (kbd "M-t") 'chpn-toggle-map)

(defvar chpn-org-map)
(define-prefix-command 'chpn-org-map)
(define-key global-map (kbd "M-q") 'chpn-org-map)

(global-unset-key (kbd "C-x C-c"))
(defalias 'exit 'save-buffers-kill-emacs)

;; Settings that do not depend on some major modes or minor modes
(global-set-key (kbd "C-h")   'delete-backward-char)
(global-set-key [f7] (lambda () (interactive) (chpn/open-file (concat user-emacs-directory "init.el"))))
(global-set-key [f8] (lambda () (interactive) (switch-to-buffer "*scratch*")))

(leaf cus-start
  :custom
  `((menu-bar-mode . nil)
    (tool-bar-mode . nil)
    (indent-tabs-mode . nil)
    (transient-mark-mode . t)
    (scroll-conservatively . 101)
    (scroll-margin . 10)
    (history-delete-duplicates . t)
    (history-length . 1000)
    (message-log-max . 10000)
    (enable-recursive-minibuffers . t)
    (auto-save-timeout . 20)
    (auto-save-interval . 60)
    (bidi-paragraph-direction . 'left-to-right)
    (gc-cons-threshold . ,(* 10 gc-cons-threshold))))

(leaf cus-edit
  :custom
  `((custom-file . ,(concat chpn/dir-cache "customize.el"))))

(leaf startup
  :custom
  `((auto-save-list-file-prefix . ,(concat chpn/dir-cache "auto-saves-"))
    (inhibit-startup-screen . t)))

(leaf scroll-bar
  :custom
  (scroll-bar-mode . nil))

(leaf savehist
  :custom
  `((savehist-mode . t)
    (savehist-file . ,(concat chpn/dir-cache "history"))))

(leaf mouse
  :custom
  (mouse-yank-at-point . t))

(leaf files
  :custom
  (backup-directory-alist . `((".*" . ,chpn/dir-cache)))
  (make-backup-files . t)
  (auto-save-default . t))

(leaf autorevert
  :custom
  (global-auto-revert-mode . t)
  (auto-revert-interval . 0.5))

(leaf simple
  :require t
  :bind
  (chpn-toggle-map
   ("l" . toggle-truncate-lines))
  :config
  (line-number-mode 1)
  (column-number-mode 1))

(leaf bookmark
  :custom
  `((bookmark-default-file . ,(concat chpn/dir-cache "bookmarks"))))

(leaf mb-depth
  :custom
  (minibuffer-depth-indicate-mode . t))

(leaf url-cookie
  :custom
  `((url-cookie-file . ,(concat chpn/dir-cache "cookie"))))

(leaf elec-pair
  :bind
  (chpn-toggle-map
   ("e" . electric-pair-local-mode))
  :custom
  (electric-pair-mode . nil))

(leaf hungry-delete
  :ensure t
  :blackout t
  :bind
  (chpn-toggle-map
   ("h" . hungry-delete-mode))
  :custom
  (global-hungry-delete-mode . t)
  (hungry-delete-join-reluctantly . t))

(leaf shut-up :ensure t)

(leaf uniquify
  :require t
  :custom
  ((uniquify-buffer-name-style . 'post-forward)
   (uniquify-separator . "|")))

(leaf window
  :bind
  (("M-[" . previous-buffer)
   ("M-]" . next-buffer)))

(leaf centaur-tabs
  :ensure t
  :disabled t
  :leaf-defer nil
  :defun (centaur-tabs-headline-match centaur-tabs-get-group-name)
  :bind
  (("M-[" . centaur-tabs-backward)
   ("M-]" . centaur-tabs-forward)
   ("C-c t b" . centaur-tabs-switch-group)
   ("C-c t p" . centaur-tabs-group-by-projectile-project)
   ("C-c t g" . centaur-tabs-group-buffer-groups))
  :hook
  ((term-mode-hook . centaur-tabs-local-mode)
   (calendar-mode-hook . centaur-tabs-local-mode)
   (helpful-mode-hook . centaur-tabs-local-mode))
  :custom
  ((centaur-tabs-style . "bar")
   (centaur-tabs-height . 20)
   (centaur-tabs-set-icons . t)
   (centaur-tabs-set-bar . 'over)
   (centaur-tabs-cycle-scope . 'tabs)
   ;; (centaur-tabs-set-modified-marker . t)
   (centaur-tabs-set-close-button . nil))
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  ;; (centaur-tabs-enable-buffer-reordering)
  (defun centaur-tabs-buffer-groups ()
    "Control buffers' group rules.
This function overwrite default function in centaur-tabs-functions.el.
Original function is from
https://github.com/ema2159/centaur-tabs#my-personal-configuration"
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode)))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer)))))))

;;TODO: 雑多な設定を整理する
;; maximize frame
(toggle-frame-maximized)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;; Increase the amount of data which Emacs reads from the process.
;; Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024))

;; Font
;; ;; all-the-icons
;; (use-package all-the-icons-dired
;;   :ensure t
;;   :hook
;;   (dired-mode . all-the-icons-dired-mode))

(leaf all-the-icons
  :ensure t
  :when (display-graphic-p)
  :require (font-setting)
  :defvar (my:font-size my:font-family)
  :if (or (eq window-system 'x) (eq window-system 'w32) (eq window-system 'ns))
  :config
  (let* ((size my:font-size)
         (family my:font-family)
         (h (round (* size 10))))
    (set-face-attribute 'default nil :family family :height h)
    (set-fontset-font nil 'unicode           (font-spec :family family) nil 'append)
    (set-fontset-font nil 'japanese-jisx0208 (font-spec :family family) nil 'append)
    (set-fontset-font nil 'japanese-jisx0212 (font-spec :family family) nil 'append)
    (set-fontset-font nil 'katakana-jisx0201 (font-spec :family family) nil 'append)
    (add-to-list 'face-font-rescale-alist (cons family 1.0))
    (set-fontset-font nil 'unicode (font-spec :family "all-the-icons")   nil 'append)
    (set-fontset-font nil 'unicode (font-spec :family "Material Icons")  nil 'append)
    (set-fontset-font nil 'unicode (font-spec :family "FontAwesome")     nil 'append)
    (set-fontset-font nil 'unicode (font-spec :family "file-icons")      nil 'append)
    (set-fontset-font nil 'unicode (font-spec :family "github-octicons") nil 'append)
    (set-fontset-font nil 'unicode (font-spec :family "Weather Icons")   nil 'append)
    (add-to-list 'face-font-rescale-alist (cons "FontAwesome" 0.85))
    (add-to-list 'face-font-rescale-alist (cons "file-icons" 0.85))
    (add-to-list 'face-font-rescale-alist (cons "github-octicons" 0.85))
    (message (format "Setup for %s with %f" family size))))

(setq use-default-font-for-symbols nil)
(setq inhibit-compacting-font-caches t)

;; Functions
(defun chpn/open-file (fname)
  "Open FNAME and switch to the buffer non-interactively."
  (switch-to-buffer (find-file-noselect fname)))

;; locale and environment
(leaf *language-environment
  :leaf-defer nil
  :custom
  ((default-input-method . "japanese-mozc")
   (current-language-environment . "Japanese"))
  :config
  ;; coding system
  (prefer-coding-system 'utf-8-unix)
  (cond ((eq system-type 'darwin)
         (set-file-name-coding-system 'utf-8-hfs)
         (setq locale-coding-system 'utf-8-hfs))
        ;; ((eq system-type 'windows-nt)
        ;;  (set-file-name-coding-system 'cp932)
        ;;  (setq locale-coding-system 'cp932))
        (t
         (set-file-name-coding-system 'utf-8)
         (setq locale-coding-system 'utf-8)))
  ;; input method
  (leaf mozc
    :ensure t
    :if (eq system-type 'gnu/linux)
    :bind
    ((minibuffer-local-map
      ("<henkan>" . (lambda () (interactive) (unless current-input-method (toggle-input-method))))
      ("<muhenkan>" . (lambda () (interactive) (when current-input-method (toggle-input-method))))))
    :bind*
    (("<henkan>" . (lambda () (interactive) (unless current-input-method (toggle-input-method))))
     ("<muhenkan>" . (lambda () (interactive) (when current-input-method (toggle-input-method)))))
    :config
    (leaf mozc-posframe
      ;; :straight (mozc-posframe :type git :host github :repo "derui/mozc-posframe")
      :el-get (mozc-posframe
               :url "https://raw.githubusercontent.com/derui/mozc-posframe/master/mozc-posframe.el"
               :features mozc-posframe)
      :defun (mozc-posframe-register)
      :custom
      ((mozc-candidate-style . 'posframe))
      :config
      (mozc-posframe-register))))

;; keybinds including back slashes
(when (eq system-type 'darwin)
  (define-key local-function-key-map [?\C-¥] [?\C-\\])
  (define-key local-function-key-map [?\M-¥] [?\M-\\])
  (define-key local-function-key-map [?\C-\M-¥] [?\C-\M-\\]))

(leaf ucs-normalize
  :if (eq system-type 'darwin)
  :require t)

(leaf exec-path-from-shell
  :ensure t
  :if (or (eq system-type 'darwin)
          (eq system-type 'gnu/linux))
  :init
  (exec-path-from-shell-initialize))

(leaf keyfreq
  :ensure t
  :custom
  ((keyfreq-mode . 1)
   (keyfreq-autosave-mode . 1)
   (keyfreq-buffer . "*KeyFrequency*")))

(leaf which-key
  :ensure t
  :blackout which-key-mode
  :hook
  ((emacs-startup-hook . which-key-mode)))

(leaf golden-ratio
  :ensure t
  :leaf-defer nil
  :blackout t
  :bind
  (chpn-toggle-map
   ("g" . golden-ratio-mode))
  :custom
  ((golden-ratio-mode . t)
   (golden-ratio-extra-commands . '(ace-window
                                    projectile-vc
                                    persp-list-buffers
                                    quit-window
                                    xref-goto-xref
                                    undo-tree-visualizer-quit
                                    magit-mode-bury-buffer))
   (golden-ratio-exclude-modes . '(treemacs-mode
                                   imenu-list-major-mode))))

(leaf ace-window
  :ensure t
  :leaf-defer nil
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-dispatch-always . t))

;; (leaf perspective
;;   :ensure t
;;   :leaf-defer nil
;;   :custom
;;   `((persp-state-default-file . ,(concat chpn/dir-cache "persp-state-file"))
;;     (persp-modestring-short . t))
;;   :bind
;;   ;; ("C-x b"   . persp-switch-to-buffer*)
;;   ("C-x k"   . persp-kill-buffer*)
;;   ("C-x C-b" . persp-bs-show)
;;   :hook
;;   (kill-emacs-hook . persp-state-save)
;;   :config
;;   (persp-mode))

(leaf nyan-mode
  :ensure t
  :custom
  (nyan-cat-face-number . 4)
  (nyan-animate-nyancat . t)
  (nyan-mode . t))

(leaf time
  :custom
  (display-time-interval . 60)
  (display-time-format . " %F %R ")
  (display-time-mode . t))

(leaf moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(leaf hide-mode-line
  :ensure t
  :hook
  (treemacs-mode-hook . hide-mode-line-mode))

;; Commented out due to bug.
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2012-11/msg00862.html
;; (leaf hl-line
;;   :custom
;;   (global-hl-line-mode . t))

(leaf paren
  :leaf-defer nil
  :custom-face
  ;; (show-paren-match ((nil (:underline "#ff5555"))))
  (show-paren-match . '((nil (:background "#44475a" :foreground "#f1fa8c"))))
  :custom
  (show-paren-mode . t)
  (show-paren-delay . 0.3)
  (show-paren-style . 'mixed)
  (show-paren-when-point-inside-paren . t)
  (show-paren-when-point-in-periphery . t)
  :bind
  (chpn-toggle-map
   ("p" . toggle-show-paren))
  :preface
  (defun toggle-show-paren ()
    "Toggle show paren."
    (interactive)
    (show-paren-mode (if show-paren-mode -1 1))
    (message "Show paren %s" (if show-paren-mode "enabled" "disabled"))))

(leaf rainbow-delimiters
  :ensure t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(leaf highlight-indent-guides
  :ensure t
  :blackout t
  :defvar highlight-indent-guides-mode
  :bind
  (chpn-toggle-map
   ("i" . toggle-highlight-indent-guides))
  :hook
  ((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-character . 124)
  (highlight-indent-guides-auto-enabled . t)
  (highlight-indent-guides-responsive . t)
  (highlight-indent-guides-method . 'fill) ;; or 'column, 'character, 'bitmap
  :preface
  (defun toggle-highlight-indent-guides ()
    "Toggle highlight indent guides."
    (interactive)
    (highlight-indent-guides-mode (if highlight-indent-guides-mode -1 1))
    (message "Highlight indent guides %s" (if highlight-indent-guides-mode "enabled" "disabled"))))

;; volatile-highlights
(leaf volatile-highlights
  :ensure t
  :blackout t
  :defun (vhl/define-extension vhl/install-extension)
  :custom
  (volatile-highlights-mode . t)
  :config
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))


(leaf modus-themes
  :ensure t
  :leaf-defer nil
  :custom
  ((modus-themes-italic-constructs . t)
   (modus-themes-bold-constructs . nil)
   (modus-themes-hl-line . '(underline accented))
   (modus-themes-region . '(bg-only no-extend)))
  :bind
  ("<f5>" . modus-themes-toggle)
  :config
  (load-theme 'modus-vivendi-tinted t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf saveplace
  :custom
  `((save-place-mode . t)
    (save-place-file . ,(concat chpn/dir-cache "places"))))

;; Recent files
(leaf recentf
  :custom
  `((recentf-mode . t)
    (recentf-save-file . ,(concat chpn/dir-cache "recentf"))
    (recentf-max-saved-items . 20000000)
    (recentf-auto-cleanup . 'never))
  ;; :hook
  ;; (focus-out-hook . (ladicle/recentf-save-list-silence ladicle/recentf-cleanup-silence))
  ;; :preface
  ;; (defun ladicle/recentf-save-list-silence ()
  ;;   (interactive)
  ;;   (let ((message-log-max nil))
  ;;     (if (fboundp 'shut-up)
  ;;         (shut-up (recentf-save-list))
  ;;       (recentf-save-list)))
  ;;   (message ""))
  ;; (defun ladicle/recentf-cleanup-silence ()
  ;;   (interactive)
  ;;   (let ((message-log-max nil))
  ;;     (if (fboundp 'shut-up)
  ;;         (shut-up (recentf-cleanup))
  ;;       (recentf-cleanup)))
  ;;   (message ""))
  )

;; scroll-lock
(leaf scroll-lock
  :require t
  :bind
  (chpn-toggle-map
   ("m" . scroll-lock-mode)))

;; ediff
;; (when (executable-find "diff")
;;   (require 'ediff nil t)
;;   (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
;;   (setq-default ediff-split-window-function 'split-window-horizontally)
;;   (global-set-key (kbd "C-c d") 'ediff-files)
;;   )

(leaf undo-tree
  :ensure t
  :blackout t
  :custom
  (global-undo-tree-mode . t)
  (undo-tree-history-directory-alist . `((".*" . ,chpn/dir-cache))))

(leaf amx
  :ensure t
  :custom
  `((amx-save-file . ,(concat chpn/dir-cache "amx-items"))))

(leaf vertico
  :ensure t
  :custom
  ((vertico-mode . t)
   (read-extended-command-predicate . #'command-completion-default-include-p)))

(leaf consult
  :ensure t
  :bind
  (("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c k" . consult-kmacro)
   ("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)
   ("C-M-#" . consult-register)
   ("C-M-r" . consult-recent-file)
   ("M-y" . consult-yank-pop)
   ("M-g e" . consult-compile-error)
   ;; ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ("M-s e" . consult-isearch-history)
   (minibuffer-local-map
    ("M-s" . consult-history)
    ("M-r" . consult-history)))
  :hook
  (completion-list-mode-hook . consult-preview-at-point-mode))

(leaf isearch
  :bind
  (isearch-mode-map
   ("M-e"   . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("C-h"   . isearch-delete-char)))

(leaf orderless
  :ensure t
  :custom
  ((completion-styles . '(orderless basic))
   (completion-category-overrides . '((file (styles basic partial-completion))))))

(leaf marginalia
  :ensure t
  :custom
  ((marginalia-mode . t))
  :bind (("M-A" . marginalia-cycle)
         (minibuffer-local-map
          ("M-A" . marginalia-cycle))))

(leaf ag :ensure t)

(leaf anzu
  :ensure t
  :blackout t
  :bind
  ("C-r" . anzu-query-replace-regexp)
  :custom
  ((global-anzu-mode . t)
   (anzu-deactivate-region . t)
   (anzu-search-threshold . 1000)))

(leaf go-translate
  :ensure t
  :require t
  :defvar (gts-default-translator gts-prompt-picker-keymap)
  :defun (gts-translator gts-prompt-picker gts-google-engine gts-buffer-render gts-prompt-picker-next-path)
  :custom
  (gts-translate-list . '(("en" "ja")))
  :bind
  (chpn-function-map
   ("t" . gts-do-translate))
  :config
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (gts-google-engine)
         :render (gts-buffer-render)))
  (setq gts-prompt-picker-keymap
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map minibuffer-local-map)
          (define-key map "\C-g" #'top-level)
          (define-key map "\C-n" #'next-line-or-history-element)
          (define-key map "\C-p" #'previous-line-or-history-element)
          (define-key map "\M-n" #'gts-prompt-picker-next-path)
          (define-key map "\M-p" (lambda () (interactive) (gts-prompt-picker-next-path t)))
          (define-key map "\C-l" #'delete-minibuffer-contents)
          map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure t
  :defer t
  :custom
  ;; files and directories
  (org-directory "~/org/")
  (org-default-notes-file (concat org-directory "notes.org"))
  (org-agenda-files `(,(concat org-directory agenda-dir) ,org-default-notes-file))

  ;; view style
  (org-startup-indented t)
  (org-indent-indentation-per-level 2)
  (org-startup-with-inline-images t)
  (org-startup-folded 'content)

  ;; agenda
  (org-agenda-span 'day)
  (org-agenda-include-diary nil)
  (org-agenda-dim-blocked-tasks t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-log-mode-items '(clock))
  (org-agenda-tags-todo-honor-ignore-options t)
  (org-agenda-clockreport-parameter-plist '(:maxlevel 5 :fileskip0 t :link t))
  (org-agenda-start-on-weekday 2)
  (org-agenda-custom-commands
   `(("i" "Agenda: 予定表"
      ((agenda "" ((org-agenda-span 'day)))
       (tags-todo "-INBOX+HABIT" ((org-agenda-overriding-header "Habit")
                                  (org-agenda-sorting-strategy '(category-keep)))) nil))

     ("p" "Tasks: タスク"
      ((agenda "" ((org-agenda-span 'week)))
       (tags-todo "+INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   ;; (org-agenda-todo-ignore-scheduled nil)
                   (org-tags-match-list-sublevels nil)))
       (tags-todo "-INBOX-HABIT/-REFR-SOME-DONE-CANCELED"
                  ((org-agenda-overriding-header "Tasks")
                   (org-tags-match-list-sublevels 'indented)
                   (org-agenda-todo-ignore-scheduled 'all)
                   (org-agenda-sorting-strategy '(priority-down scheduled-up)))) nil))))

  ;; refile
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-targets '((org-agenda-files . (:todo . "TODO"))
                        (org-agenda-files . (:todo . "NEXT"))
                        (org-agenda-files . (:todo . "WAIT"))
                        (org-agenda-files . (:todo . "REFR"))
                        (org-agenda-files . (:todo . "SOME"))))

  ;; log
  (org-log-done 'time)
  (org-log-done-with-time t)
  (org-log-into-drawer t)
  (org-log-redeadline 'time)
  (org-log-reschedule 'time)
  
  ;; clock/timer
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-clocktable-default-properties '(:maxlevel 2 :scope agenda :wstart 2 :fileskip0 t :link nil :tags t :block today))
  (org-clock-clocked-in-display 'mode-line) ;; 'frame-title
  (org-timer-default-timer 30)

  ;; todo
  (org-todo-keywords '((sequence "TODO(t)" "WAIT(w@)" "REFR(r)" "SOME(s)" "|" "DONE(d)" "CANCELED(c@)")))
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-track-ordered-property-with-tag t)
  (org-priority-highest 1)
  (org-priority-lowest 10)
  (org-priority-default 10)

  ;; capture
  (org-capture-templates
   `(("d" "diary: 日々の記録" entry (file+headline ladicle/get-today-diary "Diary")
      "* %?\n"
      :empty-lines 1 :jump-to-captured 1 :unnarrowed nil)
     ("i" "inbox: 新規タスク" entry (file ,(concat org-directory agenda-dir "inbox.org"))
      "* TODO [/] %?\n:PROPERTIES:\n:COOKIE_DATA: checkbox\n:END:\n%U"
      :empty-lines 1 :jump-to-captured nil)
     ("s" "schedule: スケジュール" entry (file ,(concat org-directory agenda-dir "inbox.org"))
      "* TODO %?\nSCHEDULED: <%(org-read-date t)>\n%U"
      :empty-lines 1)
     ("m" "memo: 新規文書" plain (file chpn/today-memo-string-with-mkdir)
      "#+TITLE: %?\n#+DATE: %(chpn/insert-today-string)\n#+OPTIONS: ^:{}\n#+OPTIONS: \\n:t\n#+OPTIONS: toc:nil\n#+OPTIONS: H:3\n\n"
      :empty-lines 1 :jump-to-captured 1 :unnarrowed nil)
     ("l" "link: リンクを追加" item (clock)
      "%A\n"
      :immediate-finish 1 :prepend nil)))

  ;; tags
  (org-tag-alist '((:startgroup . nil) ("requirement" . ?r) ("design" . ?d) ("implement" . ?i) ("test" . ?t) (:endgroup . nil)
                   (:startgroup . nil) ("comment" . ?c) (:endgroup . nil)))

  ;; property
  (org-global-properties '(("Effort_ALL" . "0:05 0:15 0:30 1:00 1:30 2:00 3:00 4:00")))

  ;; columns
  ;; (org-columns-default-format "%40ITEM %TAGS %TODO %BLOCKED %PRIORITY %SCHEDULED %DEADLINE %EFFORT{:} %CLOCKSUM %CLOCKSUM_T")
  (org-columns-default-format "%40ITEM %TODO %PRIORITY %SCHEDULED %DEADLINE %EFFORT %CLOCKSUM %CLOCKSUM_T")

  ;; archive
  (org-archive-location (concat org-directory "agenda/archive/archive_%s::"))

  ;; source code
  (org-src-tab-acts-natively t)
  (org-src-lang-modes '(("C" . c)
                        ("C++" . c++)
                        ("asymptote" . asy)
                        ("bash" . sh)
                        ("beamer" . latex)
                        ("calc" . fundamental)
                        ("cpp" . c++)
                        ("ditaa" . artist)
                        ("dot" . fundamental)
                        ("elisp" . emacs-lisp)
                        ("ocaml" . tuareg)
                        ("plantuml" . plantuml)
                        ("screen" . shell-script)
                        ("shell" . sh)
                        ("sqlite" . sql)))

  ;; plantuml
  (org-plantuml-jar-path (expand-file-name "plantuml.jar" chpn/dir-jars))
  (org-babel-load-languages '((plantuml . t)))

  :bind
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-+"   . (lambda () (interactive) (insert (chpn/insert-today-string))))
  ("C-*"   . (lambda () (interactive) (insert (chpn/insert-timestamp-string))))
  ("C-c t c" . org-table-create)
  ("C-c t -" . org-table-insert-row)
  ("C-c t |" . org-table-insert-column)
  ("C-c t =" . org-table-insert-hline)
  (:map chpn-org-map
        ("i" . agenda-inbox)
        ("p" . agenda-task)
        ("t" . diary-today)
        ("y" . diary-yesterday)
        ("c" . diary-from-cal)
        ("m" . open-memo))
  (:map org-mode-map
        ;; ("C-c i" . org-clock-in)
        ;; ("C-c o" . org-clock-out)
        ;; ("C-c u" . org-dblock-update)
        ("C-c n" . org-narrow-to-subtree)
        ("C-c b" . org-narrow-to-block)
        ("C-c w" . widen)
        ("C-c e" . org-set-effort))

  :hook
  (emacs-startup . (lambda () (org-agenda nil "i")))
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
  (auto-save . org-save-all-org-buffers)

  ;; agenda-viewのweekly viewで、週の始まりを今日（の曜日）にする
  ;; (org-agenda-mode . (lambda ()
  ;;                      (setq org-agenda-weekday-num (string-to-number (format-time-string "%u" (current-time))))
  ;;                      (custom-set-variables '(org-agenda-start-on-weekday org-agenda-weekday-num))))


  :preface
  (setq memo-dir "memo/")
  (setq issue-dir "issue/")
  (setq diary-dir "diary/")
  (setq agenda-dir "agenda/")
  (defun chpn/deploy-templates-if-not-exist (from-base to-base dirlist)
    (mapc (lambda (dir) (unless (file-directory-p (concat to-base dir))
                          (copy-directory (concat from-base dir) to-base nil t))) dirlist))
  (defun agenda-inbox    () (interactive) (org-agenda nil "i"))
  (defun agenda-task     () (interactive) (org-agenda nil "p"))
  (defun diary-today     () (interactive) (chpn/open-file (ladicle/get-today-diary)))
  (defun diary-yesterday () (interactive) (chpn/open-file (ladicle/get-yesterday-diary)))
  (defun diary-from-cal  () (interactive) (chpn/open-file (ladicle/get-diary-from-cal)))
  (defun open-memo       () (interactive) (chpn/open-file (consult-find (concat org-directory memo-dir) "..#")))

  (defun chpn/insert-today-string     () (format-time-string "%F"    (current-time)))
  (defun chpn/insert-timestamp-string () (format-time-string "%F %T" (current-time)))
  (defun chpn/today-memo-string-with-mkdir ()
    (let* ((title (read-string "memo title: "))
           (dn (concat org-directory memo-dir (format-time-string "%F_" (current-time)) title)))
      (unless (file-directory-p dn)
        (make-directory dn))
      (concat dn "/" title ".org")))
  (defun chpn/today-issue-string      () (concat org-directory issue-dir (format-time-string "%F_" (current-time)) (read-string "issue title: ") ".org"))
  (defun ladicle/get-today-diary      () (concat org-directory diary-dir (format-time-string "%F.org" (current-time))))
  (defun ladicle/get-yesterday-diary  () (concat org-directory diary-dir (format-time-string "%F.org" (time-add (current-time) (* -24 3600)))))
  (defun ladicle/get-diary-from-cal   () (concat org-directory diary-dir (format-time-string "%F.org" (apply 'encode-time (parse-time-string (concat (org-read-date) " 00:00"))))))
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
    (save-some-buffers t))

  :init
  (chpn/deploy-templates-if-not-exist (concat user-emacs-directory "org-dir-template/") "~/org/" `(,agenda-dir))
  :config
  (defun org-ascii--box-string (s info)
    "Return string S with a partial box to its left.
INFO is a plist used as a communication channel."
    (let ((utf8p (eq (plist-get info :ascii-charset) 'utf-8)))
      (format (if utf8p "─────\n%s\n─────" "-----\n%s\n-----")
	      (replace-regexp-in-string
	       "^" (if utf8p " " " ")
	       ;; Remove last newline character.
	       (replace-regexp-in-string "\n[ \t]*\\'" "" s))))))

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
  (org-pomodoro-manual-break t)
  (org-pomodoro-long-break-frequency 4)
  (org-pomodoro-format "%s") ;;  
  (org-pomodoro-short-break-format "%s")
  (org-pomodoro-long-break-format  "%s")
  (org-pomodoro-overtime-format "%s")
  (org-pomodoro-length 25)
  (org-pomodoro-short-break-length 5)
  (org-pomodoro-long-break-length 15)

  :custom-face
  (org-pomodoro-mode-line ((t (:foreground "#ff5555"))))
  (org-pomodoro-mode-line-break   ((t (:foreground "#50fa7b"))))

  :bind
  (:map org-agenda-mode-map
        ("C" . org-agenda-columns)
        ("w" . org-agenda-refile)
        ("d" . org-agenda-set-property)
        ("P" . org-pomodoro)
        ("W" . org-agenda-week-view)
        ("D" . org-agenda-day-view))

  :preface
  ;; from https://gist.github.com/ayman/bb72a25e16af9e6f30bf
  (defun terminal-notifier-notify (title message)
    "Show a message with `terminal-notifier-command`."
    (start-process "terminal-notifier"
                   "*terminal-notifier*"
                   (executable-find "terminal-notifier")
                   "-title" title
                   "-message" message))

  (defun chpn/pomodoro-notify (title body)
    "Save buffers and stop clocking when kill emacs."
    (cond
     ((eq system-type 'darwin)
      (terminal-notifier-notify title body))
     ((eq system-type 'gnu/linux)
      (notifications-notify :title title :body body))
     ((eq system-type 'windows-nt)
      (w32-notification-notify :title title :body body))))

  :hook
  (org-pomodoro-started              . (lambda () (chpn/pomodoro-notify "Org Pomodoro" "スタート！25分間がんばろう")))
  (org-pomodoro-overtime             . (lambda () (chpn/pomodoro-notify "Org Pomodoro" "25分間お疲れ様！まだがんばる？")))
  (org-pomodoro-finished             . (lambda () (chpn/pomodoro-notify "Org Pomodoro" "お疲れ様！休憩にしましょう")))
  (org-pomodoro-short-break-finished . (lambda () (chpn/pomodoro-notify "Org Pomodoro" "小休憩終わり！またがんばりましょう")))
  (org-pomodoro-long-break-finished  . (lambda () (chpn/pomodoro-notify "Org Pomodoro" "Pomodoroを一周したよ！またよろしくね")))
  (org-pomodoro-killed               . (lambda () (chpn/pomodoro-notify "Org Pomodoro" "Pomodoroをkillしたよ！またよろしくね"))))

(use-package org-re-reveal
  :ensure t
  :defer t
  :after org)

(leaf company-org-block :ensure t :after company org)

  ;; latex
(leaf ox-latex
  :after org
  :custom
  (org-latex-default-class . "bxjsarticle")
  (org-latex-pdf-process . '("latexmk -e '$latex=q/uplatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -e '$dvipdf=q/dvipdfmx -o %D %S/' -norc -gg -pdfdvi %f"))
  ;; (org-latex-pdf-process . '("latexmk -e '$lualatex=q/lualatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdflua %f"))
  ;; (org-export-in-background . t)
  (org-file-apps . '(("pdf" . "evince %s")))
  (org-latex-classes . '(("article" "\\documentclass[11pt]{article}"
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                          ("\\paragraph{%s}" . "\\paragraph*{%s}")
                          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                         ("report" "\\documentclass[11pt]{report}"
                          ("\\part{%s}" . "\\part*{%s}")
                          ("\\chapter{%s}" . "\\chapter*{%s}")
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                         ("book" "\\documentclass[11pt]{book}"
                          ("\\part{%s}" . "\\part*{%s}")
                          ("\\chapter{%s}" . "\\chapter*{%s}")
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                         ("bxjsarticle" "\\documentclass[autodetect-engine,dvi=dvipdfmx,11pt,a4paper,ja=standard]{bxjsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\ifdefined\\kanjiskip
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=true}
\\else
  \\ifdefined\\XeTeXversion
      \\hypersetup{colorlinks=true}
  \\else
    \\ifdefined\\directlua
      \\hypersetup{pdfencoding=auto,colorlinks=true}
    \\else
      \\hypersetup{unicode,colorlinks=true}
    \\fi
  \\fi
\\fi"
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                          ("\\paragraph{%s}" . "\\paragraph*{%s}")
                          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

                         ("jlreq" "\\documentclass[11pt,paper=a4]{jlreq}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\ifdefined\\kanjiskip
  \\usepackage[dvipdfmx]{graphicx}
  \\usepackage[dvipdfmx]{hyperref}
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=true}
\\else
  \\usepackage{graphicx}
  \\usepackage{hyperref}
  \\hypersetup{pdfencoding=auto,colorlinks=true}
\\fi"
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                          ("\\paragraph{%s}" . "\\paragraph*{%s}")
                          ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

                         ("jlreq-tate" "\\documentclass[tate,11pt,paper=a4]{jlreq}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\ifdefined\\kanjiskip
  \\usepackage[dvipdfmx]{graphicx}
  \\usepackage[dvipdfmx]{hyperref}
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=true}
\\else
  \\usepackage{graphicx}
  \\usepackage{hyperref}
  \\hypersetup{pdfencoding=auto,colorlinks=true}
\\fi"
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                          ("\\paragraph{%s}" . "\\paragraph*{%s}")
                          ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-S-l" . mc/skip-to-next-like-this)
   ("C-S-k" . mc/skip-to-previous-like-this)
   ("C-M->" . mc/unmark-next-like-this)
   ("C-M-<" . mc/unmark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this))
  :custom
  `((mc/list-file . ,(concat chpn/dir-cache ".mc-lists.el"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Develop Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf *git
  :config
  (leaf git-modes :ensure t)
  (leaf git-timemachine
    :ensure t
    :bind
    ("M-g t" . git-timemachine-toggle))
  (leaf magit
    :ensure t
    :custom
    ((magit-auto-revert-mode . nil))
    :bind
    ("M-g s" . magit-status))
  (leaf git-gutter
    :ensure t
    :blackout t
    :custom
    ((global-git-gutter-mode . t)
     (git-gutter:modified-sign . "=")
     (git-gutter:added-sign . "+")
     (git-gutter:deleted-sign . "-"))
    :custom-face
    ((git-gutter:modified . '((t (:foreground "#f1fa8c" :background "#f1fa8c"))))
     (git-gutter:added . '((t (:foreground "#50fa7b" :background "#50fa7b"))))
     (git-gutter:deleted . '((t (:foreground "#ff79c6" :background "#ff79c6")))))))

(leaf yasnippet
  :ensure t
  :blackout yas-minor-mode
  :custom
  (yas-global-mode . t)
  :config
  (leaf yasnippet-snippets :ensure t)
  (leaf haskell-snippets :ensure t)
  (leaf yasnippet-capf :ensure t)
  (leaf awk-yasnippets :ensure t)
  (leaf elm-yasnippets :ensure t))

(leaf company
  :ensure t
  :blackout t
  :hook
  (emacs-startup-hook . global-company-mode)
  :custom
  ((company-idle-delay . 0)
   (company-echo-delay . 0)
   (company-minimum-prefix-length . 2)
   (company-selection-wrap-around . t))
  :bind
  ("C-c y" . company-yasnippet)
  (company-active-map
   ("<tab>" . company-complete)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-s" . company-filter-candidates)
   ("C-h" . nil)
   ("M-n" . nil)
   ("M-p" . nil))
  (company-search-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-s" . company-search-repeat-forward)
   ("C-r" . company-search-repeat-backward)
   ("C-h" . company-search-delete-char)
   ("M-n" . nil)
   ("M-p" . nil))
  :config
  (leaf company-box
    :ensure t
    :blackout t
    :hook
    (global-company-mode-hook . company-box-mode)
    :custom
    (company-box-icons-alist . 'company-box-icons-all-the-icons)
    (company-box-show-single-candidate . nil)))

;; projectile
(leaf projectile
  :ensure t
  :blackout t
  :bind
  (projectile-mode-map
   ("C-c p" . projectile-command-map))
  :custom
  `((projectile-known-projects-file . ,(concat chpn/dir-cache "projectile-bookmarks.eld"))
    (projectile-cache-file . ,(concat chpn/dir-cache "projectile.cache"))
    (projectile-enable-caching . t)
    (projectile-require-project-root . t)
    (projectile-dirconfig-comment-prefix . "#")
    (projectile-mode . t))
  :config
  (leaf consult-projectile :ensure t))

(leaf treemacs
  :ensure t
  :bind
  (("M-1" . treemacs-select-window)
   (treemacs-mode-map
    ("M-1" . other-window)))
  :custom
  `((treemacs-is-never-other-window . t)
    (treemacs-no-delete-other-windows . t)
    (treemacs-persist-file . ,(concat chpn/dir-cache "treemacs-persist"))
    (treemacs-last-error-persist-file . ,(concat chpn/dir-cache "treemacs-persist-at-last-error"))
    ;; (treemacs-width 20)
    (treemacs-follow-mode . t)
    (treemacs-filewatch-mode . t)
    (treemacs-fringe-indicator-mode . t)
    (treemacs-git-mode . 'simple))
  :config
  (leaf treemacs-projectile
    :ensure t
    :require t
    :after projectile)
  (leaf treemacs-icons-dired
    :ensure t
    :config
    (treemacs-icons-dired-mode))
  (leaf treemacs-magit
    :ensure t
    :after magit))

(leaf flycheck
  :ensure t
  :blackout t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom
  `((flycheck-temp-prefix . ,(concat chpn/dir-cache "flycheck"))
    (global-flycheck-mode . t))
  :config
  (leaf flycheck-posframe
    :ensure t
    :custom
    ((flycheck-posframe-position . 'window-bottom-right-corner))
    :hook
    (flycheck-mode-hook . flycheck-posframe-mode)))

(leaf imenu-list
  :ensure t
  :bind (("<f10>" . imenu-list-smart-toggle))
  :custom
  (;;(imenu-list-size . 30)
   (imenu-list-auto-resize . nil)
   (imenu-list-focus-after-activation . t)
   (imenu-list-position . 'right)))

;; lsp
(leaf lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  ((lsp-diagnostics-provider . :auto)
   (lsp-completion-provider . :capf)
   (lsp-lens-enable . t)
   (lsp-semantic-tokens-enable . t)
   (lsp-semantic-tokens-honor-refresh-requests . t)
   (lsp-enable-links . t)
   ;; (lsp-log-io t)
   ;; (lsp-document-sync-method 'lsp--sync-incremental)
   (lsp-keymap-prefix . "M-l"))
  :hook
  ((lsp-mode-hook . lsp-enable-which-key-integration)
   (lsp-mode-hook . lsp-ui-mode)
   ;; (elm-mode-hook  . lsp)
   (java-mode-hook       . lsp-deferred)
   (haskell-mode-hook    . lsp-deferred)
   (js-mode-hook         . lsp-deferred)
   (typescript-mode-hook . lsp-deferred)
   (terraform-mode-hook  . lsp-deferred)
   (sh-mode-hook         . lsp-deferred)
   (python-mode-hook     . lsp-deferred))
  :config
  (leaf lsp-ui
    :ensure t
    ;; :custom-face
    ;; (lsp-ui-doc-background ((nil (:background "black"))))
    :custom
    ((lsp-ui-doc-enable . nil)
     (lsp-ui-doc-header . t)
     (lsp-ui-doc-include-signature . t)
     (lsp-ui-doc-position . 'top)
     (lsp-ui-doc-max-width . 150)
     (lsp-ui-doc-max-height . 30)
     (lsp-ui-doc-show-with-mouse . t)
     (lsp-ui-doc-show-with-cursor . t)
     (lsp-ui-doc-use-childframe . t)
     (lsp-ui-doc-use-webkit . nil)
     (lsp-ui-flycheck-list-position . 'right)
     (lsp-ui-imenu-enable . nil)
     ;; (lsp-ui-imenu-auto-refresh . t)
     ;; (lsp-ui-imenu-kind-position . 'top)
     ;; (lsp-ui-imenu-window-width . 0)
     (lsp-ui-peek-enable . nil)
     (lsp-ui-peek-peek-height . 50)
     (lsp-ui-peek-list-width . 50)
     (lsp-ui-peek-fontify . 'on-demand)
     (lsp-ui-peek-show-directory . t)
     (lsp-ui-sideline-enable . nil)
     (lsp-ui-sideline-show-symbol . t)
     (lsp-ui-sideline-show-hover . t)
     (lsp-ui-sideline-show-diagnostics . nil)
     (lsp-ui-sideline-show-code-actions . t)))
  (leaf lsp-treemacs
    :ensure t
    :after treemacs
    :custom
    (lsp-treemacs-sync-mode . t))
  (leaf dap-mode
    :ensure t
    :config
    (dap-auto-configure-mode)
    (leaf dap-chrome :require t)))

(leaf lsp-java
  :ensure t
  :after lsp-mode dap-mode
  :custom
  (lsp-java-vmargs . `("-XX:+UseParallelGC"
                       "-XX:GCTimeRatio=4"
                       "-XX:AdaptiveSizePolicyWeight=90"
                       "-Dsun.zip.disableMemoryMapping=true"
                       ,(concat "-javaagent:" (expand-file-name "lombok.jar" chpn/dir-jars))
                       ,(concat "-Xbootclasspath/a:" (expand-file-name "lombok.jar" chpn/dir-jars))
                       ;; "-noverify"
                       ;; "-XX:+UseG1GC"
                       ;; "-XX:+UseStringDeduplication"
                       "-Xmx1G"
                       "-Xms100m"))
  (lsp-java-configuration-maven-user-settings . "~/.m2/settings.xml")
  (lsp-java-import-maven-enabled . t)
  (lsp-java-maven-download-sources . t)
  (lsp-java-maven-update-snapshots . t)
  :config
  (leaf dap-java :require t))

(leaf lsp-haskell
  :ensure t
  :after lsp-mode
  :custom
  (lsp-haskell-server-args . '("-d"))
  (lsp-haskell-formatting-provider . "fourmolu"))

(leaf haskell-mode
  :ensure t
  :custom
  (haskell-indentation-layout-offset . 4)
  (haskell-indentation-left-offset . 4)
  (haskell-indentation-starter-offset . 4)
  (haskell-indentation-where-post-offset . 4)
  (haskell-indentation-where-pre-offset . 4)
  :bind
  (haskell-mode-map
   ("C-c C-h" . haskell-compile)
   ("C-c ?" . hoogle)))

(leaf restclient
  :ensure t
  :config
  (leaf ob-restclient :ensure t))

(leaf urlenc :ensure t)

(leaf lsp-terraform
  :after lsp-mode
  :custom
  ((lsp-terraform-ls-enable-show-reference . t)
   )
  :bind
  (terraform-mode-map
   ("C-c C-i" . lsp-terraform-ls-init)
   ("C-c C-v" . lsp-terraform-ls-validate)))

(leaf terraform-mode
  :ensure t
  :custom
  (terraform-indent-level . 2)
  :config
  (leaf company-terraform
    :ensure t
    :config
    (company-terraform-init)))

(leaf yaml-mode :ensure t)

(leaf dockerfile-mode :ensure t)

(leaf docker-compose-mode
  :ensure t
  :after yaml-mode)

(leaf markdown-mode :ensure t)

(leaf js
  :custom
  ((js-indent-level . 2)
   (js-jsx-indent-level . 2)))

;; from https://github.com/emacs-typescript/typescript.el/issues/4#issuecomment-873485004
(leaf typescript-mode
  :ensure t
  :hook
  (typescript-mode-hook . subword-mode)
  :mode
  (".*\\.tsx\\'" . typescript-tsx-mode)
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TypeScript[TSX]")
  :custom
  (typescript-indent-level . 2)
  :config
  (leaf ob-typescript
    :ensure t
    :after org))

(leaf tree-sitter
  :ensure t
  :hook
  ((typescript-mode typescript-tsx-mode) . tree-sitter-hl-mode)
  :config
  (leaf tree-sitter-langs
    :ensure t
    :defvar (tree-sitter-major-mode-language-alist)
    :config
    (tree-sitter-require 'tsx)
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))))

(leaf elm-mode
  :ensure t
  :custom
  (elm-package-json . "elm.json")
  :hook
  (elm-mode-hook . elm-format-on-save-mode))

(leaf lsp-pyright
  :ensure t
  :after lsp-mode
  :custom
  (lsp-pyright-python-executable-cmd . "python3"))

(leaf python
  :custom
  ((python-shell-interpreter . "python3")
   (python-indent-guess-indent-offset-verbose . nil)))

(leaf apache-mode :ensure t)

(leaf nginx-mode
  :ensure t
  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/")
  :config
  (leaf company-nginx
    :ensure t
    :after company
    :hook
    (nginx-mode-hook . company-nginx-keywords)))

(leaf plantuml-mode
  :ensure t
  :mode ("\\.puml\\'")
  :custom
  `((plantuml-default-exec-mode . 'jar)
    (plantuml-jar-path . ,(expand-file-name "plantuml.jar" chpn/dir-jars))
    (plantuml-output-type . "png")))

(leaf nxml-mode
  :mode ("\.xml$" "\.xsl$" "\.xhtml$" "\.page$")
  :custom
  ((nxml-child-indent . 2)
   (nxml-attribute-indent . 2)
   (nxml-slash-auto-complete-flag . t)))

(leaf sqlformat
  :ensure t
  :custom
  (sqlformat-command . 'pgformatter)
  (sqlformat-args . '("-s2" "-L"))
  :bind
  (sql-mode-map
   ("<tab>" . sqlformat-buffer)))

(leaf vterm
  :ensure t
  :bind
  (vterm-mode-map
   ("C-h" . vterm-send-C-h)
   ("C-g" . vterm-send-C-g)))

(leaf web-mode
  :ensure t
  :mode ("\.html$")
  :custom
  (web-mode-markup-indent-offset . 4)
  (web-mode-css-indent-offset . 4)
  (web-mode-code-indent-offset . 4)
  (web-mode-enable-auto-pairing . t)
  (web-mode-enable-auto-closing . t)
  (web-mode-auto-close-style . 2)
  :custom-face
  (web-mode-doctype-face . '((nil (:foreground "Pink3"))))
  (web-mode-html-tag-face . '((nil (:foreground "Green"))))
  (web-mode-html-attr-value-face . '((nil (:foreground "Yellow"))))
  (web-mode-html-attr-name-face . '((nil (:foreground "#0FF")))))


(provide 'init)
;;; init.el ends here
