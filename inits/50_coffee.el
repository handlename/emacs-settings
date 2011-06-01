;; INSTALL
;; (install-elisp "https://github.com/defunkt/coffee-mode/raw/master/coffee-mode.el")

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(add-hook 'coffee-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-c") 'coffee-compile-file)))

(add-hook 'after-save-hook
          (lambda ()
            (if (eq major-mode 'coffee-mode)
                (coffee-compile-file))))