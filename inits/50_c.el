(add-hook 'c-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c c") 'compile)
             (local-set-key (kbd "C-c g") 'gdb)))
