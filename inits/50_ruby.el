;; ruby-mode

(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq tab-width 4)
             (setq ruby-indent-level tab-width)))
