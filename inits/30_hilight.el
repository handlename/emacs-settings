;; highlight current line
;; INSTALL
;; (install-elisp-from-emacswiki "highlight-current-line.el")
;; (install-elisp "http://homepage3.nifty.com/satomii/software/jaspace.el")

(require 'highlight-current-line)
(highlight-current-line-on t)
(set-face-background 'highlight-current-line-face "#000000")


;; INSTALL
;; (auto-install-batch "crosshairs")

;; (require 'crosshairs)


;; hilight paren
(show-paren-mode 1)


;; highlight reagion
(setq transient-mark-mode t)


;; highlight edit characters
(require 'jaspace)
(setq jaspace-highlight-tabs t)
(add-hook 'mmm-mode-hook 'jaspace-mmm-mode-hook)


;; highlight current buffer
;; http://ksugita.blog62.fc2.com/blog-entry-8.html
;; (load-file "~/.emacs.d/site-lisp/hiwin.el")
;; (setq hiwin-color "#496B22")
;; (hiwin-mode)
