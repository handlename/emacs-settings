;; auto-save-buffers-enhanced
;; INSTALL
;; (install-elisp "http://svn.coderepos.org/share/lang/elisp/auto-save-buffers-enhanced/trunk/auto-save-buffers-enhanced.el")

(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 5.0)
(auto-save-buffers-enhanced t)
(global-set-key "\C-xas" 'auto-save-buffers-enhanced-toggle-activity)


;; uniq
;; (install-elisp "http://www.arttaylor.com/~reeses/software/uniq.el")
(load "uniq")

;; 終了前に確認する
(defadvice save-buffers-kill-emacs
  (before safe-save-buffers-kill-emacs activate)
  "safe-save-buffers-kill-emacs"
  (unless (y-or-n-p "Really exit emacs? ")
    (keyboard-quit)))


;; auto-compile
;; INSTALL
;; (install-elisp-from-emacswiki "auto-async-byte-compile.el")
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)


;; sequential-command
;; (auto-install-batch "sequential-command")
(require 'sequential-command-config)
(sequential-command-setup-keys)


;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")
