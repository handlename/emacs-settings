;; yatex-mode

;(add-to-list 'load-path "~/.emacs.d/site-lisp/yatex/")
(setq auto-mode-alist (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq tex-command "/opt/local/bin/platex")
(setq dvi2-command "/opt/local/bin/xdvi")
(setq YaTeX-kanji-code 4) ;; utf-8
(add-hook 'yatex-mode-hook'(lambda ()(setq auto-fill-function nil)))
