; 最新版を使うと org-get-x-clipboard が定義されていない旨のエラーが出るのであえて使わない
;(add-to-list 'load-path "~/.emacs.d/site-lisp/org-mode/lisp")

(require 'org-install)

(setq org-log-done t)
(setq org-tags-column 72)                 ; タグを表示する位置
(setq org-hide-leading-stars t)           ; 見出しの余計なアスタリスクを表示しない
;(set-face-foreground 'org-hide "#282828") ; 表示しないアスタリスクの色
(setq org-startup-truncated nil)          ; 開始時にツリーを閉じない
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; org-remember
(org-remember-insinuate)
(setq org-directory "~/Dropbox/memo/")
(setq org-default-notes-file (concat org-directory "note.org"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %t\n   %i" nil "Inbox")
        ("Bug" ?b "** TODO %?\n   %t   :bug:\n   %i" nil "Inbox")
        ("Idea" ?i "** %?\n   %t\n   %i" nil "New Ideas")
        ("Memo" ?m "** %?\n   %t\n   %i" nil "Memo")
        ))
;; (setq org-capture-templates
;;       '(("t" "Todo" entry
;;          (file+headline nil "Inbox")
;;          "** TODO %?\n   %t\n   %i")
;;         ("b" "Bug" entry
;;          (file+headline nil "Inbox")
;;          "** TODO %?\n   %t   :bug:\n   %i")
;;         ("i" "Idea" entry
;;          (file+headline nil "New Ideas")
;;          "** %?\n   %t\n   %i")))

;; todo
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(x)" "CANCEL(c)")))

;; org-agenda
(defun my-org-get-recuresive-path-list (file-list)
  "Get file path list recuresively."
  (let ((path-list nil))
    (unless (listp file-list)
      (setq file-list (list file-list)))
    (loop for x
          in file-list
          do (if (file-directory-p x)
                 (setq path-list
                       (append
                        (my-org-get-recuresive-path-list
                         (remove-if
                          (lambda(y) (string-match "\\.$\\|\\.svn\\|~$\\|\\.git" y))
                          (directory-files x t)))
                        path-list))
               (setq path-list (push x path-list))))
    path-list))
(setq org-agenda-files (my-org-get-recuresive-path-list (list org-directory)))

(global-set-key (kbd "C-c A") 'org-agenda)
(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-m") 'org-return-indent)))


(setq org-mobile-inbox-for-pull "~/Dropbox/memo/flagged.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")