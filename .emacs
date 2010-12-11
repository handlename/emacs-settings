;; .emacs written by NAGATA Hiroaki (handlename.net)

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/auto-install")

;; INSTALL
;; (install-elisp "http://svn.coderepos.org/share/lang/elisp/init-loader/init-loader.el")

(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")

;; 00 一般設定
;; 10 起動前実行系
;; 20 関数定義
;; 30 追加機能系
;; 40 マイナーモード
;; 50 メジャーモード
;; 90 起動後実行系
