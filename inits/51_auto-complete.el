;; INSTALL
;; (auto-complete-batch "auto-complete development version")
;; (auto-install-from-emacswiki "auto-complete-etags.el")
;; (auto-install-from-emacswiki "etags-table.el")

(require 'auto-complete-config)
(require 'auto-complete-etags)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-set-key (kbd "M-_") 'auto-complete)
(setq ac-auto-start 1)       ; 補完を開始する文字数
(setq ac-auto-show-menu 0.2) ; 補完リストが表示されるまでの時間
(setq ac-sources '(ac-source-yasnippet
                   ac-source-dictionary
                   ac-source-gtags
                   ac-source-words-in-buffer))

(add-to-list 'ac-modes 'javascript-mode)
(add-to-list 'ac-modes 'html-helper-mode)
(add-to-list 'ac-modes 'emacs-lisp-mode)
(add-to-list 'ac-modes 'yaml-mode)

;; ; for cperl-mode
;; (add-hook 'cperl-mode-hook
;;           '(lambda ()
;;              (progn
;;                (require 'perl-completion)
;;                (add-to-list 'ac-sources 'ac-source-perl-completion)
;;                (perl-completion-mode t)
;;               )))

; for emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (add-to-list 'ac-sources 'ac-source-functions)
             (add-to-list 'ac-sources 'ac-source-symbols)))

; for yaml-mode
(add-hook 'yaml-mode-hook
          '(lambda ()
             (setq ac-sources '(ac-source-words-in-buffer))))

; for objc-mode
(require 'etags-table)
(add-to-list  'etags-table-alist
              '("\\.[mh]$" "~/.emacs.d/tags/objc.TAGS"))
(defvar ac-source-etags-table
  '((candidates . (lambda ()
         (all-completions ac-target (tags-completion-table))))
    (candidate-face . ac-candidate-face)
    (selection-face . ac-selection-face)
    (requires . 1))
  "etags をソースにする")
(add-hook 'objc-mode-hook
          (lambda ()
            (push 'ac-source-etags-table ac-sources)))
