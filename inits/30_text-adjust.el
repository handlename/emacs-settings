;; http://d.hatena.ne.jp/rubikitch/20090220/text_adjust
;; INSTALL
;; (install-elisp "http://taiyaki.org/elisp/text-adjust/src/text-adjust.el")
;; (install-elisp "http://taiyaki.org/elisp/mell/src/mell.el")

(require 'text-adjust)
;; (defun text-adjust-space-before-save-if-needed ()
;;   (when (memq major-mode
;;               '(org-mode text-mode mew-draft-mode myhatena-mode))
;;     (text-adjust-space-buffer)))
;;(add-hook 'before-save-hook 'text-adjust-space-before-save-if-needed)

(global-set-key (kbd "C-c C-b s") 'text-adjust-space-buffer)
