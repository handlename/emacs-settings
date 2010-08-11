;; auto-save-buffers
(require 'auto-save-buffers)
(run-with-idle-timer 5.0 t 'auto-save-buffers)


;; Install elisp
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/site-lisp/")


;; sense-region
;; http://taiyaki.org/elisp/sense-region/
;; (autoload 'sense-region-on
;;   "sense-region"
;;   "System to toggle region and rectangle." t nil)
;; (sense-region-on)


;; uniq
(load "uniq")

;; 終了前に確認する
(defadvice save-buffers-kill-emacs
  (before safe-save-buffers-kill-emacs activate)
  "safe-save-buffers-kill-emacs"
  (unless (y-or-n-p "Really exit emacs? ")
    (keyboard-quit)))
