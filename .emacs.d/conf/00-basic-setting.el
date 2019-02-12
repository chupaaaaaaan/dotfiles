;;; locale and environment
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;;; face
;; キャラクタ端末では、バックグラウンドを"dark"に設定する
(when (eq window-system nil)
  (setq frame-background-mode 'dark))

;;; others
;; ツールバー・スクロールバー非表示設定
(when window-system
  (tool-bar-mode nil)
  (scroll-bar-mode nil))

;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;;; splash screenを無効にする
(setq inhibit-splash-screen t)

;;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;;; C-u C-SPC C-SPC ... でどんどん過去のマークを遡る
;; (setq set-mark-command-repeat-pop t)

;;; ファイルを開いた位置を保存する
(when (require 'saveplace nil t)
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))


;;; インデントにTABを使わないようにする
(setq-default indent-tabs-mode nil)

;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode t)

;;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;; ログの記録行数を増やす
(setq message-log-max 10000)

;;; 履歴をたくさん保存する
(setq history-length 1000)

(setq mouse-yank-at-point t)

