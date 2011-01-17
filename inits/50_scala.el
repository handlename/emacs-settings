(add-to-list 'load-path "~/.emacs.d/site-lisp/scala-mode")
(require 'scala-mode-auto)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
