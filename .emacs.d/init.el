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

  (defconst chpn/dir-pkg-elpa "~/.elisp/elpa/")
  (defconst chpn/dir-pkg-elget "~/.elisp/el-get/")
  (defconst chpn/dir-pkg-local "~/.elisp/local/")
  (unless (file-directory-p chpn/dir-pkg-local)
    (make-directory chpn/dir-pkg-local t)))

;; load local configures
(mapc (lambda (lc) (when (file-regular-p lc) (load-file lc))) (directory-files chpn/dir-pkg-local t))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"          . "https://orgmode.org/elpa/")
                       ("melpa"        . "https://melpa.org/packages/")
                       ("melpa-stable" . "https://stable.melpa.org/packages/")
                       ("gnu"          . "https://elpa.gnu.org/packages/")))
  (customize-set-variable 'package-user-dir chpn/dir-pkg-elpa)
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
  (leaf leaf-tree
    :ensure t
    :custom
    ((imenu-list-size . 30)
     (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf transient-dwim
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf cus-start
  :custom
  `((menu-bar-mode . nil)
    (tool-bar-mode . nil)
    (indent-tabs-mode . nil)
    (transient-mark-mode . t)
    (history-delete-duplicates . t)
    (history-length . 1000)
    (message-log-max . 10000)
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
  :config
  (line-number-mode 1)
  (column-number-mode 1))

(leaf bookmark
  :custom
  `((bookmark-default-file . ,(concat chpn/dir-cache "bookmarks"))))

(leaf elec-pair
  :bind
  ("<f5> p" . electric-pair-local-mode)
  :custom
  (electric-pair-mode . t))

(leaf hungry-delete
  :ensure t
  :bind
  ("<f5> d" . hungry-delete-mode)
  :custom
  (global-hungry-delete-mode . t))

(leaf shut-up :ensure t)

(leaf uniquify
  :require t
  :custom
  ((uniquify-buffer-name-style . 'post-forward)
   (uniquify-separator . "|")))

(leaf centaur-tabs
  :ensure t
  :leaf-defer nil
  :bind
  (("M-[" . centaur-tabs-backward)
   ("M-]" . centaur-tabs-forward)
   ("C-c t b" . centaur-tabs-counsel-switch-group)
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
   ;; (centaur-tabs-set-modified-marker . t)
   (centaur-tabs-set-close-button . nil))
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name.
Original function is from `https://github.com/ema2159/centaur-tabs#my-personal-configuration'."
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

;; (use-package all-the-icons-ivy
;;   :ensure t
;;   :after ivy
;;   :config
;;   (all-the-icons-ivy-setup))

(leaf all-the-icons
  :ensure t
  :when (display-graphic-p)
  :if (or (eq window-system 'x) (eq window-system 'w32) (eq window-system 'ns))
  :config
  (let* ((size my:font-size) (family my:font-family) (h (round (* size 10))))
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

;; (add-hook 'emacs-startup-hook #'chpn/font-setting)

;; locale and environment
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Functions
(defun chpn/open-file (fname)
  "Open FNAME and switch to the buffer non-interactively."
  (switch-to-buffer (find-file-noselect fname)))

;; OS
;; Mac
(when (eq system-type 'darwin)
  (require 'ucs-normalize nil t)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs)
  (use-package exec-path-from-shell
    :ensure t
    :init
    (exec-path-from-shell-initialize))
  ;; バックスラッシュを含むキーバインドの設定
  (define-key local-function-key-map [?\C-¥] [?\C-\\])
  (define-key local-function-key-map [?\M-¥] [?\M-\\])
  (define-key local-function-key-map [?\C-\M-¥] [?\C-\M-\\]))

;; GNU/Linux
(when (eq system-type 'gnu/linux)
  (use-package exec-path-from-shell
    :ensure t
    :init
    (exec-path-from-shell-initialize)))

;; Windows
(when (eq system-type 'windows-nt)
  (setq file-name-coding-system 'cp932)
  (setq locale-coding-system 'cp932))


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
  :hook
  (emacs-startup . which-key-mode))

;; define prefix-key
(define-prefix-command 'ladicle-window-map)
(define-key global-map (kbd "M-i") 'ladicle-window-map)

(define-prefix-command 'ladicle-toggle-map)
(define-key global-map (kbd "M-t") 'ladicle-toggle-map)

(define-prefix-command 'chpn-org-map)
(define-key global-map (kbd "M-q") 'chpn-org-map)


(global-unset-key (kbd "C-x C-c"))
(defalias 'exit 'save-buffers-kill-emacs)

;; Settings that do not depend on some major modes or minor modes
(global-set-key (kbd "C-h")   'delete-backward-char)
(global-set-key (kbd "M-t l") 'toggle-truncate-lines)
;; (global-set-key (kbd "C-t")   'other-window)
(global-set-key [f6] (lambda () (interactive) (counsel-M-x "^counsel ")))
(global-set-key [f7] (lambda () (interactive) (chpn/open-file (concat user-emacs-directory "init.el"))))
(global-set-key [f8] (lambda () (interactive) (switch-to-buffer "*scratch*")))

(leaf golden-ratio
  :ensure t
  :leaf-defer nil
  :bind
  ("M-t g" . golden-ratio-mode)
  :custom
  ((golden-ratio-mode . t)
   (golden-ratio-extra-commands . '(ace-window
                                    projectile-vc
                                    persp-list-buffers
                                    quit-window
                                    undo-tree-visualizer-quit
                                    magit-mode-bury-buffer))
   (golden-ratio-exclude-modes . '(treemacs-mode
                                   lsp-ui-imenu-mode))))

(leaf ace-window
  :ensure t
  :leaf-defer nil
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-dispatch-always . t))

(leaf perspective
  :ensure t
  :leaf-defer nil
  :custom
  `((persp-state-default-file . ,(concat chpn/dir-cache "persp-state-file"))
    (persp-modestring-short . t))
  :bind
  ;; ("C-x b"   . persp-ivy-switch-buffer)
  ;; ("C-x b"   . persp-switch-to-buffer*)
  ("C-x b"   . persp-counsel-switch-buffer)
  ("C-x k"   . persp-kill-buffer*)
  ("C-x C-b" . persp-bs-show)
  :hook
  (kill-emacs-hook . persp-state-save)
  :config
  (persp-mode))

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

(leaf doom-modeline
  :ensure t
  :after all-the-icons
  ;; :after all-the-icons shrink-path
  :custom
  (doom-modeline-mode . t)
  (doom-modeline-buffer-file-name-style . 'auto)
  (doom-modeline-display-default-persp-name . t))

(use-package hide-mode-line
  :ensure t
  :demand t
  :disabled
  :hook
  (treemacs-mode . hide-mode-line-mode))

(leaf hl-line
  :custom
  (global-hl-line-mode . t))

(leaf paren
  :leaf-defer nil
  :custom-face
  ;; (show-paren-match ((nil (:underline "#ff5555"))))
  (show-paren-match . '((nil (:background "#44475a" :foreground "#f1fa8c"))))
  :custom
  (show-paren-mode . t)
  (show-paren-delay . 0)
  (show-paren-style . 'mixed)
  (show-paren-when-point-inside-paren . t)
  (show-paren-when-point-in-periphery . t)
  :bind
  ("M-t p" . toggle-show-paren)
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
  ("M-t i" . toggle-highlight-indent-guides)
  :hook
  ((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-character . 124)
  (highlight-indent-guides-auto-enabled . t)
  (highlight-indent-guides-responsive . t)
  (highlight-indent-guides-method . 'column) ;; or 'fill, 'character, 'bitmap
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
  :custom
  (volatile-highlights-mode . t)
  :config
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))


(leaf doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold . t)
  (doom-themes-enable-italic . t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  (leaf frame
    :require t
    :config
    (set-cursor-color "cyan")))




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
  :defvar scroll-lock-mode
  :preface
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

(leaf undo-tree
  :ensure t
  :blackout t
  :custom
  (global-undo-tree-mode . t)
  (undo-tree-history-directory-alist . `((".*" . ,chpn/dir-cache))))

(leaf amx
  :ensure t
  :custom
  `((amx-save-file . ,(concat chpn/dir-cache "emacs-amx-items"))))

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
  (emacs-startup . ivy-mode)
  (ivy-mode . counsel-mode)
  :bind
  ("C-s" . swiper)
  ("M-s M-s" . swiper-thing-at-point)
  ("M-x" . counsel-M-x)
  ("M-y" . counsel-yank-pop)
  ("C-M-z" . counsel-fzf)
  ("C-M-r" . counsel-recentf)
  ("C-M-f" . counsel-ag)
  ;; ("C-x b" . counsel-switch-buffer)
  ;; ("C-x C-b" . counsel-ibuffer)
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
        (propertize "\n----------------------------------------------------------------------\n"
                    'face `(:foreground "#6272a4"))))

(use-package counsel-tramp
  :ensure t
  :disabled
  :bind
  ("C-c C-f" . counsel-tramp))

;; anzu
(use-package anzu
  :ensure t
  :defer t
  :diminish
  :bind
  ("C-r" . anzu-query-replace-regexp)
  ;; ("C-M-r" . anzu-query-replace-at-cursor-thing)
  :hook
  (emacs-startup . global-anzu-mode)
  :custom
  (anzu-deactivate-region t)
  (anzu-search-threshold 1000))

(leaf go-translate
  :ensure t
  :custom
  (gts-translate-list . '(("en" "ja")))
  :bind
  (("M-i t" . gts-do-translate))
  :config
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines `(,(gts-google-engine) ,(gts-google-rpc-engine))
         :render (gts-buffer-render))))

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

  ;; agenda
  (org-agenda-span 'day)
  (org-agenda-include-diary nil)
  (org-agenda-dim-blocked-tasks t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-log-mode-items '(clock))
  (org-agenda-custom-commands
   `(("h" "Habits: 習慣タスク"
      tags-todo "+HABIT" ((org-agenda-overriding-header "Habit")
                          (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))

     ("i" "Agenda: 予定表"
      ((agenda "" nil)
       (tags-todo "-INBOX-HABIT/+NEXT"
                  ((org-agenda-overriding-header "Next Actions")
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-sorting-strategy '(priority-down scheduled-up effort-up)))) nil))

     ("p" "Tasks: タスク"
      ((tags-todo "+INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
       (tags-todo "-INBOX-HABIT/-REFR-SOME-DONE-CANCELED"
                  ((org-agenda-overriding-header "Tasks")
                   (org-tags-match-list-sublevels 'indented)
                   (org-agenda-sorting-strategy '(priority-down scheduled-up)))) nil))

     ("w" "Waiting for: 待ち状態"
      ((tags-todo "-INBOX-HABIT/+WAIT"
                  ((org-agenda-overriding-header "Waiting for")
                   (org-tags-match-list-sublevels 'indented)
                   (org-agenda-sorting-strategy '(category-keep)))) nil))

     ("r" "Reference: 参考資料など"
      ((tags-todo "-INBOX-HABIT/+REFR"
                  ((org-agenda-overriding-header "Reference")
                   (org-tags-match-list-sublevels nil))) nil))

     ("y" "Someday: いつかやる/多分やる"
      ((tags-todo "-INBOX-HABIT/+SOME"
                  ((org-agenda-overriding-header "Someday")
                   (org-agenda-sorting-strategy '(category-keep))
                   (org-tags-match-list-sublevels nil))) nil))

     ("d" "Done: 完了"
      ((tags "+CLOSED<\"<tomorrow>\"+CLOSED>=\"<today>\""
             ((org-agenda-overriding-header "Today's Done")
              (org-tags-match-list-sublevels nil)))
       (tags "+CLOSED<\"<today>\"+CLOSED>=\"<yesterday>\""
             ((org-agenda-overriding-header "Yesterday's Done")
              (org-tags-match-list-sublevels nil)))
       (tags "+CLOSED<\"<yesterday>\"+CLOSED>=\"<-1w>\""
             ((org-agenda-overriding-header "Weekly Done")
              (org-tags-match-list-sublevels nil))) nil))))

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
  (org-clock-clocktable-default-properties '(:maxlevel 2 :scope agenda :fileskip0 t :link t :block today :match ""))
  (org-clock-clocked-in-display 'mode-line) ;; 'frame-title
  (org-timer-default-timer 30)

  ;; todo
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@)" "REFR(r)" "SOME(s)" "|" "DONE(d)" "CANCELED(c@)")))
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-track-ordered-property-with-tag t)

  ;; capture
  (org-capture-templates
   `(("diary" "日記" entry
      (file+headline ladicle/get-today-diary "Diary")
      "* %?\n"
      :empty-lines 1 :jump-to-captured 1 :unnarrowed nil)
     ("tweet" "一言メモ" item
      (file+headline ladicle/get-today-diary "Log")
      "%(ladicle/org-get-time) %?\n"
      :prepend nil)
     ("inbox" "新規プロジェクト" entry
      (file ,(concat org-directory agenda-dir "inbox.org"))
      ,(concat "%[" org-directory capture-template-dir "inbox.org" "]")
      :empty-lines 1 :jump-to-captured nil)
     ("interrupt" "突発作業" entry
      (file ,(concat org-directory agenda-dir "inbox.org"))
      ,(concat "%[" org-directory capture-template-dir "interrupt.org" "]")
      :empty-lines 1 :clock-in 1 :clock-resume 1)
     ("schedule" "予定作業" entry
      (file ,(concat org-directory agenda-dir "schedule.org"))
      ,(concat "%[" org-directory capture-template-dir "schedule.org" "]")
      :empty-lines 1)
     ("memo" "メモ・記録" plain
      (file chpn/today-memo-string)
      ,(concat "%[" org-directory capture-template-dir "memo.org" "]")
      :empty-lines 1 :jump-to-captured 1 :unnarrowed nil)
     ("break" "休憩" entry
      (file ,(concat org-directory agenda-dir "inbox.org"))
      "* DONE 休憩（%?）  :break:\n  %U\n"
      :empty-lines 1 :clock-in 1 :clock-resume 1)
     ("link" "リンクを追加" item
      (clock)
      "%A\n"
      :immediate-finish 1 :prepend nil)))

  ;; tags
  (org-tag-alist '((:startgroup . nil) ("design" . ?s) ("develop" . ?d) ("meeting" . ?m) (:endgroup . nil)
                   (:startgroup . nil) ("work"   . ?w) ("qanda"   . ?q) ("break"   . ?b) (:endgroup . nil)))

  ;; property
  (org-global-properties '(("Effort_ALL" . "0:05 0:15 0:30 1:00 1:30 2:00 2:30 3:00")))

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
  (org-plantuml-jar-path (concat user-emacs-directory "plantuml.jar"))
  (org-babel-load-languages '((plantuml . t)))

  :bind
  ("C-c c" . counsel-org-capture)
  ("C-c a" . org-agenda)
  ("C-c l" . org-store-link)
  ("C-+"   . (lambda () (interactive) (insert (chpn/insert-today-string))))
  ("C-*"   . (lambda () (interactive) (insert (chpn/insert-timestamp-string))))
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
  (emacs-startup . (lambda () (interactive) (org-agenda nil "i")))
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
  (setq agenda-dir "agenda/")
  (setq capture-template-dir "capture_templates/")
  (defun chpn/deploy-templates-if-not-exist (from-base to-base dirlist)
    (mapc (lambda (dir) (unless (file-directory-p (concat to-base dir))
                          (copy-directory (concat from-base dir) to-base nil t))) dirlist))
  (defun agenda-inbox    () (interactive) (org-agenda nil "i"))
  (defun agenda-task     () (interactive) (org-agenda nil "p"))
  (defun diary-today     () (interactive) (chpn/open-file (ladicle/get-today-diary)))
  (defun diary-yesterday () (interactive) (chpn/open-file (ladicle/get-yesterday-diary)))
  (defun diary-from-cal  () (interactive) (chpn/open-file (ladicle/get-diary-from-cal)))
  (defun open-memo       () (interactive) (chpn/open-file (counsel-find-file "~/org/memo/")))

  (defun chpn/insert-today-string     () (format-time-string "%F"    (current-time)))
  (defun chpn/insert-timestamp-string () (format-time-string "%F %T" (current-time)))
  (defun ladicle/org-get-time         () (format-time-string "<%R>"  (current-time)))
  (defun chpn/today-memo-string       () (concat org-directory "memo/" (read-string "memo title: ") ".org"))
  (defun ladicle/get-today-diary      () (concat org-directory (format-time-string "diary/%F.org" (current-time))))
  (defun ladicle/get-yesterday-diary  () (concat org-directory (format-time-string "diary/%F.org" (time-add (current-time) (* -24 3600)))))
  (defun ladicle/get-diary-from-cal   () (concat org-directory (format-time-string "diary/%F.org" (apply 'encode-time (parse-time-string (concat (org-read-date) " 00:00"))))))
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
  (chpn/deploy-templates-if-not-exist (concat user-emacs-directory "org-dir-template/") "~/org/" `(,agenda-dir ,capture-template-dir)))

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
        ("P" . org-pomodoro))

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

;; (use-package org-mobile-sync
;;   :ensure t
;;   :defer t
;;   :after org
;;   :custom
;;   (org-mobile-directory "~/Dropbox/Apps/MobileOrg/")
;;   (org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org")
;;   :config
;;   (org-mobile-sync-mode t))

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
  :ensure git-modes
  :defer t)

(use-package gitconfig-mode
  :ensure git-modes
  :defer t)

(use-package gitignore-mode
  :ensure git-modes
  :defer t)

(use-package git-gutter
  :ensure t
  :defer t
  :hook
  (emacs-startup . global-git-gutter-mode)
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
  (emacs-startup . yas-global-mode)
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
  (emacs-startup . global-company-mode)

  :bind
  (:map company-active-map
        ("<tab>" . company-complete)
        ("C-n"   . company-select-next)
        ("C-p"   . company-select-previous)
        ("C-s"   . company-filter-candidates)
        ("C-h"   . nil)
        ("M-n"   . nil)
        ("M-p"   . nil))
  (:map company-search-map
        ("C-n"   . company-select-next)
        ("C-p"   . company-select-previous)
        ("C-s"   . company-search-repeat-forward)
        ("C-r"   . company-search-repeat-backward)
        ("C-h"   . company-search-delete-char)
        ("M-n"   . nil)
        ("M-p"   . nil)))

(use-package company-box
  :ensure t
  :defer t
  :after company
  :diminish
  :hook
  (global-company-mode . company-box-mode)
  :custom
  (company-box-icons-alist 'company-box-icons-all-the-icons)
  (company-box-show-single-candidate nil))

(use-package company-quickhelp
  :ensure t
  :disabled
  :defer t
  :after company
  :hook
  (global-company-mode . company-quickhelp-mode))

;; projectile
(use-package projectile
  :ensure t
  :bind
  ("C-c p" . projectile-command-map)
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :custom
  (projectile-known-projects-file (concat chpn/dir-cache "emacs-projectile-bookmarks.eld"))
  (projectile-cache-file (concat chpn/dir-cache "emacs-projectile.cache"))
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-require-project-root t)
  (projectile-dirconfig-comment-prefix "#")
  :hook
  (emacs-startup . projectile-mode))

;; treemacs
(use-package treemacs
  :ensure t
  ;; :defer t
  :bind
  ("M-1" . treemacs-select-window)
  ("M-0" . treemacs)
  (:map treemacs-mode-map
        ("M-1" . other-window))
  ;; ("C-x t 1"   . treemacs-delete-other-windows)
  ;; ("C-x t B"   . treemacs-bookmark)
  ;; ("C-x t C-t" . treemacs-find-file)
  ;; ("C-x t M-t" . treemacs-find-tag)
  :custom
  (treemacs-is-never-other-window t)
  (treemacs-no-delete-other-windows t)
  ;; (treemacs-width 20)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'simple))

(use-package counsel-projectile
  :ensure t
  :after counsel projectile
  :custom
  (counsel-projectile-sort-files t)
  (counsel-projectile-sort-projects t)
  :config
  (counsel-projectile-mode 1))

(use-package treemacs-projectile
  :ensure t
  :after treemacs projectile)

(use-package treemacs-magit
  :ensure t
  :after treemacs magit)

(use-package treemacs-icons-dired
  :ensure t
  :defer t
  :after treemacs dired
  :config
  (treemacs-icons-dired-mode))

(leaf flycheck
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom
  `((flycheck-temp-prefix . ,(concat chpn/dir-cache "flycheck"))
    (global-flycheck-mode . t))
  :config
  (leaf flycheck-posframe
    :ensure t
    :hook
    (flycheck-mode-hook . flycheck-posframe-mode)))

;; lsp
(leaf lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  ((lsp-diagnostics-provider . :auto)
   (lsp-completion-provider . :capf)
   (lsp-lens-enable . t)
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
   (python-mode-hook     . lsp-deferred))
  :bind
  (lsp-mode-map
   ("C-c m" . lsp-ui-imenu)
   ("C-c C-c" . lsp-ivy-workspace-symbol)
   ("C-u C-c C-c" . lsp-ivy-global-workspace-symbol))
  :config
  (leaf lsp-ui
    :ensure t
    ;; :custom-face
    ;; (lsp-ui-doc-background ((nil (:background "black"))))
    :custom
    ((lsp-ui-doc-enable . nil)
     (lsp-ui-doc-header . t)
     (lsp-ui-doc-include-signature . t)
     (lsp-ui-doc-position . 'bottom)
     (lsp-ui-doc-max-width . 150)
     (lsp-ui-doc-max-height . 30)
     (lsp-ui-doc-show-with-mouse . t)
     (lsp-ui-doc-show-with-cursor . t)
     (lsp-ui-doc-use-childframe . t)
     (lsp-ui-doc-use-webkit . nil)
     (lsp-ui-flycheck-list-position . 'right)
     (lsp-ui-imenu-enable . t)
     (lsp-ui-imenu-auto-refresh . t)
     (lsp-ui-imenu-kind-position . 'top)
     (lsp-ui-imenu-window-width . 0)
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
  (leaf lsp-ivy :ensure t :after ivy)
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
                       ,(concat "-javaagent:" (expand-file-name "lombok.jar" user-emacs-directory))
                       ,(concat "-Xbootclasspath/a:" (expand-file-name "lombok.jar" user-emacs-directory))
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
  (lsp-haskell-server-path . "haskell-language-server-wrapper")
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
  (elm-mode . elm-format-on-save-mode))

(leaf yaml-mode :ensure t)

(leaf docker-compose-mode
  :ensure t
  :after yaml-mode)

(use-package jenkinsfile-mode
  :ensure t
  :defer t)

(use-package markdown-mode :ensure t)

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
    :config
    (tree-sitter-require 'tsx)
    (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))))

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
    (plantuml-jar-path . ,(concat user-emacs-directory "plantuml.jar"))
    (plantuml-output-type . "png")))

(leaf nxml-mode
  :mode ("\.xml$" "\.xsl$" "\.xhtml$" "\.page$")
  :custom
  ((nxml-child-indent . 2)
   (nxml-attribute-indent . 2)
   (nxml-slash-auto-complete-flag . t)))

(leaf sql-indent :ensure t)

;; (use-package vterm
;;   :ensure t
;;   :custom
;;   (vterm-environment (list "LANG=ja_JP.UTF-8"))
;;   :bind
;;   (:map vterm-mode-map
;;         ("C-h" . vterm-send-C-h)
;;         ("C-g" . vterm-send-C-g)))

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
