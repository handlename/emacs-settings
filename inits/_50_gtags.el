(autoload 'gtags-mode "gtags" "" t)
(setq 'c-mode-common-hook
      '(lambda ()
         (gtags-mode 1)
         ))
(setq 'php-mode-common-hook
      '(lambda ()
         (gtags-mode 1)
         ))
