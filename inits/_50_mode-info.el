;; http://www.namazu.org/~tsuchiya/elisp/mode-info.html
;; http://d.hatena.ne.jp/higepon/20080828/1219932411

;(add-to-list 'load-path "~/.emacs.d/site-lisp/mode-info/")

(require 'mi-config)
(setq mode-info-index-directory "~/info/index")
(add-to-list 'Info-directory-list (expand-file-name "~/info"))
(add-to-list 'Info-directory-list (expand-file-name "~/info/glibc-2.3.2"))
(add-to-list 'Info-directory-list (expand-file-name "~/info/Gauche-0.8.13"))

(define-key global-map "\C-chf" 'mode-info-describe-function)
(define-key global-map "\C-chv" 'mode-info-describe-variable)
(define-key global-map "\M-." 'mode-info-find-tag)
(require 'mi-fontify)

(setq mode-info-class-alist
      '((elisp  emacs-lisp-mode lisp-interaction-mode)
        (libc   c-mode c++-mode)
        (make   makefile-mode)
        (perl   perl-mode cperl-mode eperl-mode)
        (ruby   ruby-mode)
        (gauche scheme-mode scheme-interaction-mode inferior-scheme-mode)))