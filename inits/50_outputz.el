(require 'outputz)
(load-file "~/.emacs.d/outputz-pass.el")
(setq outputz-key outputz-pass)      ;; 復活の呪文
(setq outputz-uri "http://emacs.curly.local/%s") ;; 適当なURL。%sにmajor-modeの名前が入るので、major-modeごとのURLで投稿できます。
(global-outputz-mode t)
