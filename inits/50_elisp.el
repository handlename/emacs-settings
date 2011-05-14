(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c e") 'eval-region)
             (setq indent-tabs-mode nil)))

;; lispxmp.el
;; INSTALL
;; (install-elisp-from-emacswiki "lispxmp.el")
(require 'lispxmp)
