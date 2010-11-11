;; text-mode
;;______________________________________________________________________

(add-hook 'text-mode-hook
          '(lambda ()
             (setq tab-width 4)
             (setq c-basic-offset 4)))
