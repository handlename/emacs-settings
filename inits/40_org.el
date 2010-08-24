; 最新版を使うと org-get-x-clipboard が定義されていない旨のエラーが出るのであえて使わない
; (add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp")

(require 'org-install)

(setq org-log-done t)
(setq org-tags-column 60)                 ; タグを表示する位置
(setq org-hide-leading-stars t)           ; 見出しの余計なアスタリスクを表示しない
(set-face-foreground 'org-hide "#282828") ; 表示しないアスタリスクの色
(setq org-startup-truncated nil)          ; 開始時にツリーを閉じない
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; org-remember
(org-remember-insinuate)
(setq org-directory "~/Dropbox/memo/")
(setq org-default-notes-file (concat org-directory "note.org"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
        ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
        ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")
        ("Memo" ?m "** %?\n   %i\n   %a\n   %t" nil "Memo")
        ))

;; org-agenda
(setq org-agenda-files (list org-default-notes-file))
(add-hook 'org-mode-hook (lambda ()
                           (local-set-key (kbd "C-c a") 'org-agenda)))