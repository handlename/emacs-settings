;; INSTALL
;; (install-elisp "https://github.com/imakado/emacs-smartchr/raw/master/smartchr.el")

(require 'smartchr)

(defun my-smartchr-setting ()
  (local-set-key (kbd ">") (smartchr '(">" "->" ">>")))

  (local-set-key (kbd "(") (smartchr '("(" "(`!!')")))
  (local-set-key (kbd "{") (smartchr '("{" "{ `!!' }" "{\n`!!'\n}")))
  (local-set-key (kbd "[") (smartchr '("[" "[`!!']")))
  )

(add-hook 'php-mode-hook 'my-smartchr-setting)
(add-hook 'javascript-mode-hook 'my-smartchr-setting)
(add-hook 'ruby-mode-hook 'my-smartchr-setting)
(add-hook 'cperl-mode-hook 'my-smartchr-setting)
