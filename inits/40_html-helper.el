;; INSTALL
;; (install-elisp "http://tuvalu.santafe.edu/~nelson/tools/tempo.el")
;; (install-elisp "http://tuvalu.santafe.edu/~nelson/tools/html-helper-mode.el")
;; (install-elisp "http://www.nbi.dk/TOOLS/emacs/lisp/html-font.el")

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq html-helper-basic-offset 0)
(setq html-helper-item-continue-indent 0)
(defvar html-helper-new-buffer-template
  '("<!DOCTYPE html>\n"
    "<html>\n"
    "\n"
    "<head>\n"
    "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\">\n"
    "<title></title>\n"
    "</head>\n"
    "\n"
    "<body>\n"
    "\n"
    "\n"
    "\n"
    "</body>\n"
    "</html>\n")
  "*Template for new buffers, inserted by html-helper-insert-new-buffer-strings if html-helper-build-new-buffer is set to t")
(require 'sgml-mode)
(add-hook 'html-helper-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-e") 'sgml-close-tag)
             (require 'html-font)
             (font-lock-mode t)))
