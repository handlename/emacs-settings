;; INSTALL
;; (install-elisp "http://github.com/mooz/emacs-zlc/raw/master//zlc.el")

(require 'zlc)
(setq zlc-select-completion-immediately t)
(let ((map minibuffer-local-map))
  ;;; like menu select
  (define-key map (kbd "C-n") 'zlc-select-next)
  (define-key map (kbd "C-p")  'zlc-select-previous)

  ;;; reset selection
  (define-key map (kbd "C-c") 'zlc-reset)
  )