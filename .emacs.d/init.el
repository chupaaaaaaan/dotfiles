;;; load-path
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


;;; package.el
(require 'package nil t)

;; add repositories
(add-to-list 'package-archives '("org"           . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable"  . "https://stable.melpa.org/packages/"))
(package-initialize)

;; package list with repository
(setq package-pinned-packages
      '(
        (auto-complete        . "melpa-stable")
        (company              . "melpa-stable")
        (egg                  . "melpa-stable")
        (elm-mode             . "melpa-stable")
        (flycheck             . "melpa-stable")
        (flycheck-haskell     . "melpa-stable")
        (haskell-mode         . "melpa-stable")
        (helm                 . "melpa-stable")
        (init-loader          . "melpa-stable")
        (markdown-mode        . "melpa-stable")
        (undo-tree            . "gnu")
        (vbasense             . "melpa-stable")
        (volatile-highlights  . "melpa-stable")
        (yaml-mode            . "melpa-stable")
        ))

(unless package-archive-contents (package-refresh-contents))

(dolist (pkg (mapcar 'car package-pinned-packages))
  (unless (package-installed-p pkg)
        (package-install pkg)))




;;; locale and environment
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)


;;; init-loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/conf")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ac-haskell-process flycheck-haskell flycheck undo-tree yaml-mode volatile-highlights vbasense helm haskell-mode init-loader egg auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
