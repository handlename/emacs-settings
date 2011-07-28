;; http://www.morishima.net/~naoto/software/elscreen/

;(add-to-list 'load-path "~/.emacs.d/site-lisp/apel")

(load "elscreen" "ElScreen" t)
(setq elscreen-display-tab nil) ; タブを非表示
(define-key global-map (kbd "M-t") 'elscreen-next)
