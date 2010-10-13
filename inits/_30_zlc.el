;; http://d.hatena.ne.jp/mooz/20101003/p1
;; INSTALL
;; (install-elisp "http://github.com/mooz/emacs-zlc/raw/master//zlc.el")

(require 'zlc)
(let ((map minibuffer-local-map))
  ;;; like menu select
  (define-key map (kbd "C-S-n") 'zlc-select-next-vertical)
  (define-key map (kbd "C-S-p") 'zlc-select-previous-vertical)
  (define-key map (kbd "C-n")   'zlc-select-next)
  (define-key map (kbd "C-p")   'zlc-select-previous)
  )
