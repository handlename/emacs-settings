(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c e") 'eval-region)
             (setq indent-tabs-mode nil)))
