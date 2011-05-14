;; http://www.nongnu.org/baol-hth/
;; https://gist.github.com/672655

;(add-to-list 'load-path "~/.emacs.d/site-lisp/html-helper-mode")

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq html-helper-basic-offset 0)
(setq html-helper-item-continue-indent 0)
(setq html-helper-never-indent t)
(setq html-helper-verbose nil)
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
             (set-face-bold-p 'html-tag-face nil)))

;; html-tt
;; http://clouder.jp/src/elisp/html-tt-1.13.tar.gz
;; http://www.kzfmix.com/blosxom_archive/Computer/Linux/emacstt060307.html
(add-to-list 'auto-mode-alist '("\\.tt$" . html-helper-mode))
(add-to-list 'auto-mode-alist '("\\.tx$" . html-helper-mode))
(require 'html-tt)
(add-hook 'html-helper-mode-hook 'html-tt-load-hook)

(make-face 'my-html-tt-face)
(set-face-foreground 'my-html-tt-face "orange")
(set-face-bold-p 'my-html-tt-face nil)
(setq html-tt-sequence-face 'my-html-tt-face)
