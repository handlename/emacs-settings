;; PATH
(setq exec-path (cons "/opt/local/bin" (cons "/usr/local/bin" exec-path)))

;; mail address
(setq user-mail-address "main@handlename.net")

;; meta key
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; バックアップファイルを残さない
(setq make-backup-files nil)

;; ベルを鳴らさない
(setq ring-bell-function 'ignore)

;; 1行ずつスクロール
(setq scroll-step 1)

;; ステータスに行番号＆列番号表示
(column-number-mode t)
(setq default-fill-column 72)

;; 行番号表示
(global-linum-mode)
(setq linum-format "%4d")

;; 折り返さない
(setq truncate-lines t)
(setq truncate-partial-width-windows t)

;; インデント設定
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(c-set-offset 'case-label '+)

;; narrowingを使う
(put 'narrow-to-region 'disabled nil)

;; スタートページ非表示
(setq inhibit-startup-message t)
