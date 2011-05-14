;; http://code.google.com/p/yasnippet/

(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory "~/.emacs.d/site-lisp/yasnippet/snippets")
(yas/load-directory yas/root-directory)

(set-face-background 'yas/field-highlight-face "#404040")

(global-set-key (kbd "C-c s") 'yas/insert-snippet)