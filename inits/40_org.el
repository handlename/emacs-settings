(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp")

(require 'org-install)
(add-hook 'org-mode-hook
          '(lambda ()
             (setq org-log-done t)
             (setq org-tags-column 60)
             (setq org-hide-leading-stars t)))

(add-hook 'org-mode-hook
          '(lambda ()
             (set-face-foreground 'org-hide "#282828")))
