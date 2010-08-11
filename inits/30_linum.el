;; linum
(global-linum-mode t)
(setq linum-format "%4d ")
(add-hook 'linum-mode-hook
          '(lambda ()
             (set-face-foreground 'linum "#000000")
             (set-face-background 'linum "#FFFFFF")))
