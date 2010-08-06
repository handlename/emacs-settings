;; auto-save-buffers
(require 'auto-save-buffers)
(run-with-idle-timer 5.0 t 'auto-save-buffers)


;; Install elisp
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/")


;; sense-region
;; http://taiyaki.org/elisp/sense-region/
;; (autoload 'sense-region-on
;;   "sense-region"
;;   "System to toggle region and rectangle." t nil)
;; (sense-region-on)


;; uniq
(load "uniq")


;; linum
(global-linum-mode t)