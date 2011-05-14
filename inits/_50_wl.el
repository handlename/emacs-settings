;; http://d.hatena.ne.jp/stanaka/20071025/1193286440
;; http://d.hatena.ne.jp/yamdan/20090430/1241105044

;(add-to-list 'load-path "~/.emacs.d/site-lisp/apel")
;(add-to-list 'load-path "~/.emacs.d/site-lisp/flim")
;(add-to-list 'load-path "~/.emacs.d/site-lisp/semi")
;(add-to-list 'load-path "~/.emacs.d/site-lisp/wanderlust/elmo")
;(add-to-list 'load-path "~/.emacs.d/site-lisp/wanderlust/utils")
;(add-to-list 'load-path "~/.emacs.d/site-lisp/wanderlust/wl")

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(setq wl-icon-directory "~/.emacs.d/site-lisp/wanderlust/etc")

;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "handlename.net@gmail.com")
(setq elmo-imap4-default-authenticate-type 'clear)
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)
(setq elmo-imap4-use-modified-utf7 t)

;; SMTP
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port '587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "handlename_net")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")

;; デフォルトのフォルダ
(setq wl-default-folder "%inbox")

;; フォルダ名補完時に使用するデフォルトのスペック
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAPの仕様に合わせて
(setq wl-trash-folder "%[Gmail]/Trash")

(setq wl-folder-check-async t) ; 非同期でチェックするように

(setq wl-dispose-folder-alist
      (cons '("^%inbox" . remove) wl-dispose-folder-alist))


(setq ssl-program-name "openssl")
(setq ssl-certificate-directory (expand-file-name "~/.w3/certs"))