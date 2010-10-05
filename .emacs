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
;; 40 メジャーモード
;; 50 マイナーモード
;; 90 起動後実行系
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(org-agenda-files (quote ("/Users/handle/Dropbox/memo/note.org" "/Users/handle/Dropbox/memo/kayac/tech.org" "/Users/handle/Dropbox/memo/kayac/sv-adm.org" "/Users/handle/Dropbox/memo/kayac/sonpo.org" "/Users/handle/Dropbox/memo/kayac/shokama-hs.org" "/Users/handle/Dropbox/memo/kayac/minpo.org" "/Users/handle/Dropbox/memo/kayac/mikke.org" "/Users/handle/Dropbox/memo/kayac/google-tw.org" "/Users/handle/Dropbox/memo/kayac/google-pl.org_archive" "/Users/handle/Dropbox/memo/kayac/google-pl.org" "/Users/handle/Dropbox/memo/kayac/cozy.org" "/Users/handle/Dropbox/memo/kayac/aji-mixi.org")))
 '(safe-local-variable-values (quote ((clmemo-mode . t)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
