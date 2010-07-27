;; .emacs written by NAGATA Hiroaki (handlename.net)

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")

;; 00 一般設定
;; 10 起動前実行系
;; 20 関数定義
;; 30 追加機能系
;; 40 メジャーモード
;; 50 マイナーモード
;; 90 起動後実行系
