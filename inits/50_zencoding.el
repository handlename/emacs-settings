;; INSTALL
;; (install-elisp "http://github.com/chrisdone/zencoding/raw/master/zencoding-mode.el")

(require 'zencoding-mode)
(add-hook 'html-helper-mode-hook 'zencoding-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(safe-local-variable-values (quote ((clmemo-mode . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'zencoding-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-z C-z") 'zencoding-expand-line)
             ))
