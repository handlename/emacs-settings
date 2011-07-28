;; INSTALL
;; (install-elisp "https://raw.github.com/kiwanami/emacs-calfw/master/calfw.el")

(require 'calfw)
(add-hook 'calendar-load-hook
          (lambda ()
            (require 'japanese-holidays)
            (setq calendar-holidays '(japanese-holidays))
            (require 'calfw-org)
            (cfw:install-org-schedules)))

;; for org-mode
;; (install-elisp "https://raw.github.com/kiwanami/emacs-calfw/master/calfw-org.el")
(require 'calfw-org)
(cfw:install-org-schedules)
