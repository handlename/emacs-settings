;; INSTALL
;; (install-elisp "http://www.gentei.org/~yuuji/software/revive.el")

(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe emacs" t)
(define-key global-map (kbd "C-x S") 'save-current-configuration)
(define-key global-map (kbd "C-x F") 'resume)
(add-hook 'kill-emacs-hook 'save-current-configuration)
