(require 'org-install)
(add-hook 'org-mode-hook
          '(lambda ()
             (setq org-log-done t)
             (setq org-tags-column 60)
             (setq org-hide-leading-stars t)))

(add-hook 'org-mode-hook
          '(lambda ()
             (set-face-foreground 'org-hide "#282828")))
