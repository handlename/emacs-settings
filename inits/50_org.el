(require 'org-install)

(setq org-log-done t)
(setq org-tags-column 72)                 ; タグを表示する位置
(setq org-hide-leading-stars t)           ; 見出しの余計なアスタリスクを表示しない
;(set-face-foreground 'org-hide "#282828") ; 表示しないアスタリスクの色
(setq org-startup-truncated nil)          ; 開始時にツリーを閉じない
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

; 検索対象にアーカイブを含める
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; key bindings
(global-set-key (kbd "C-c l") 'org-store-link)

;; file
(setq org-directory "~/note/")
(setq org-my-private-file (concat org-directory "private.org"))
(setq org-my-kayac-file (concat org-directory "kayac.org"))

(setq org-default-notes-file org-my-private-file)
(setq org-agenda-files
      (list org-my-private-file
            org-my-kayac-file
            ))

;; org-capture
(setq org-capture-templates
      '(("T" "Todo private" entry (file+olp org-my-private-file "Todo" "Future") "** TODO %?\n   %U\n   %i\n" :unnarrowed t)
        ("I" "Idea private" entry (file+headline org-my-private-file "Idea") "** %?\n   %T\n   %i\n" :unnarrowed t)
        ("M" "Memo private" entry (file+headline org-my-private-file "Memo") "** %?\n   %T\n   %i\n" :unnarrowed t)
        ("t" "Todo kayac" entry (file+olp org-my-kayac-file "Todo" "Future") "** TODO %?\n   %U\n   %i\n" :unnarrowed t)
        ("i" "Idea kayac" entry (file+headline org-my-kayac-file "Idea") "** %?\n   %U\n   %i\n" :unnarrowed t)
        ("m" "Memo kayac" entry (file+headline org-my-kayac-file "Memo") "** %?\n   %U\n   %i\n" :unnarrowed t)
        ))

;; todo
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(x)" "CANCEL(c)")))

;; keybindings
(global-set-key (kbd "C-c A") 'org-agenda)
(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-m") 'org-return-indent)))
