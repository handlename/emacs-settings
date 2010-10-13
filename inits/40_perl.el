;; http://d.hatena.ne.jp/hakutoitoi/20090208/1234069614
;; モジュールソースバッファの場合はその場で、
;; その他のバッファの場合は別ウィンドウに開く。

(put 'perl-module-thing 'end-op
     (lambda ()
       (re-search-forward "\\=[a-zA-Z][a-zA-Z0-9_:]*" nil t)))
(put 'perl-module-thing 'beginning-op
     (lambda ()
       (if (re-search-backward "[^a-zA-Z0-9_:]" nil t)
           (forward-char)
         (goto-char (point-min)))))

(defun perldoc-m ()
  (interactive)
  (let ((module (thing-at-point 'perl-module-thing))
        (pop-up-windows t)
        (cperl-mode-hook nil))
    (when (string= module "")
      (setq module (read-string "Module Name: ")))
    (let ((result (substring (shell-command-to-string (concat "perldoc -m " module)) 0 -1))
          (buffer (get-buffer-create (concat "*Perl " module "*")))
          (pop-or-set-flag (string-match "*Perl " (buffer-name))))
      (if (string-match "No module found for" result)
          (message "%s" result)
        (progn
          (with-current-buffer buffer
            (toggle-read-only -1)
            (erase-buffer)
            (insert result)
            (goto-char (point-min))
            (cperl-mode)
            (toggle-read-only 1)
            )
          (if pop-or-set-flag
              (switch-to-buffer buffer)
            (display-buffer buffer)))))))

;; hook
(add-hook 'cperl-mode-hook
          '(lambda ()
             (progn
               (local-set-key (kbd "C-x m") 'perldoc-m)
               )))


;; pod-mode
;; INSTALL
;; (install-elisp "http://github.com/renormalist/emacs-pod-mode/raw/master/pod-mode.el")
(require 'pod-mode)
(add-to-list 'auto-mode-alist
             '("\\.pod$" . pod-mode))
(add-hook 'pod-mode-hook
          '(lambda ()
             (progn
               (font-lock-mode)
               (auto-fill-mode 1)
               (flyspell-mode 1)
               (local-set-key (kbd "C-x m") 'perldoc-m)
               )))
