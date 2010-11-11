;; INSTALL
;; (install-elisp "http://tweedle-dee.org/svn/emacs.d/site-lisp/yaml-mode.el")

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
