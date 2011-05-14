(require 'evernote-mode)

(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)

; macportsで入れたrubyを使う
(setq shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/zsh")