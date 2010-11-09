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


;; ;; auto-compile
;; ;; INSTALL
;; ;; (install-elisp-from-emacswiki "auto-async-byte-compile.el")
;; (require 'auto-async-byte-compile)
;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)


;; sequential-command
;; (auto-install-batch "sequential-command")
(require 'sequential-command-config)
(sequential-command-setup-keys)


;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;; ;; 関数名を常に表示する
;; (which-func-mode t)
;; (setq which-func-modes t)
;; (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
;; (setq-default header-line-format '(which-func-mode ("function " which-func-format)))
;; (set-face-foreground 'which-func "#CCCCCC")


;; ポイント位置が空行なら C-k してもキルリングに追加しない - わからん。
;; http://d.hatena.ne.jp/kitokitoki/20100904/p2
(defun my-kill-or-delete-line ()
  "ポイントが空行ならキルリングに追加しない"
  (interactive)
  (if (and (bolp) (eolp)) ;お気に入り
      (my-delete-line)
    (my-kill-line)))

(defun my-kill-line ()
  "C-u C-k でキルリングに入れない"
  (interactive)
  (if current-prefix-arg
      (delete-region (point) 
                     (save-excursion (end-of-line) (point)))
    (kill-line)))

;; kill-line から置換。もっと縮められそう。
(defun my-delete-line (&optional arg)
  (interactive "P")
  (delete-region (point)
                 (progn
                   (if arg
                       (forward-visible-line (prefix-numeric-value arg))
                     (if (eobp)
                         (signal 'end-of-buffer nil))
                     (let ((end
                            (save-excursion
                              (end-of-visible-line) (point))))
                       (if (or (save-excursion
                                 (unless show-trailing-whitespace
                                   (skip-chars-forward " \t" end))
                                 (= (point) end))
                               (and kill-whole-line (bolp)))
                           (forward-visible-line 1)
                         (goto-char end))))
                   (point))))

(global-set-key (kbd "C-k") 'my-kill-or-delete-line)


;; multiverse.el
;; INSTALL
;; (install-elisp "http://www.emacswiki.org/emacs/download/multiverse.el")
(require 'multiverse)


;; 小菊
;; fild-fileでmigemoを使う
(require 'kogiku)


;; sudo-ext
;; (install-elisp-from-emacswiki "sudo-ext.el")
(require 'sudo-ext)