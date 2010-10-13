;; INSTALL
;; (auto-install-batch "anithing")
;; (install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el")
;; (install-elisp-from-emacswiki "anything-etags.el")
;; (install-elisp-from-emacswiki "anything-gtags.el")

(require 'anything-config)

(define-key anything-map (kbd "C-M-n") 'anything-next-source)
(define-key anything-map (kbd "C-M-p") 'anything-previous-source)
(set-face-foreground 'anything-file-name "#CCCCCC")

;; 履歴を保存するとエラーになるので
(remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)
(ad-disable-advice 'anything-exit-minibuffer 'before 'anything-c-adaptive-exit-minibuffer)
(ad-disable-advice 'anything-select-action 'before 'anything-c-adaptive-select-action)
(setq anything-c-adaptive-history-length 0)

;; anything-migemo
(require 'anything-migemo)
;(define-key global-map [(control ?:)] 'anything-migemo)


;; バッファ内検索
;; http://d.hatena.ne.jp/IMAKADO/20080724/1216882563
;; INSTALL
;; (install-elisp-from-emacs-wiki "color-moccur.el")
;; (install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el")
(require 'color-moccur)
(require 'anything-c-moccur)
(setq moccur-split-word t)
(when (require 'migemo nil t) (setq moccur-use-migemo t))
(setq anything-c-moccur-anything-idle-delay 0.2)    ; `anything-idle-delay'
(setq anything-c-moccur-enable-initial-pattern nil) ; はじめからキャレット位置のパターンが入力されるのを抑止
(setq anything-c-moccur-higligt-info-line-flag t)   ; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
(setq anything-c-moccur-enable-auto-look-flag t)    ; 現在選択中の候補の位置を他の window に表示する
(setq anything-c-moccur-enable-initial-pattern t)   ; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする
(define-key global-map (kbd "C-s") 'anything-c-moccur-occur-by-moccur)
(define-key global-map (kbd "C-S-s") 'anything-c-moccur-dmoccur)

;; anything-for-files
(define-key global-map (kbd "C-x b") 'anything-for-files)

;; anything-find-files
;(define-key global-map (kbd "C-x C-f") 'anything-find-files)

;; calc
(define-key global-map (kbd "C-c C-a c")
  (lambda ()
    "Calculate in anything"
    (interactive)
    (anything '(anything-c-source-calculation-result))))

;; killring history
(define-key global-map (kbd "C-M-y") 'anything-show-kill-ring)

;; emacs commands
(define-key global-map (kbd "M-x")
  (lambda ()
    "Execute emacs commands in anything"
    (interactive)
    (anything '(anything-c-source-emacs-commands))))

;; tag jump
(require 'anything-etags)
(require 'anything-gtags)
(define-key global-map (kbd "C-x t")
  (lambda ()
    "Tag jump using etags, gtags and `anything'."
    (interactive)
    (let* ((initial-pattern (regexp-quote (or (thing-at-point 'symbol) ""))))
      (anything (list anything-c-source-gtags-select
                      anything-c-source-etags-select))
      "Find Tag: " nil)))

;; anything-hatena-bookmark
(require 'anything-hatena-bookmark)
(define-key global-map (kbd "C-c C-a b") 'anything-hatena-bookmark)


;; 候補ウィンドウを常に下部に表示
;; INSTALL
;; (install-elisp "http://nschum.de/src/emacs/split-root/split-root.el")
(require 'split-root)
(defun anything-display-function--split-root (buf)
  (let ((parcent 40.0))
    (set-window-buffer (split-root-window (truncate (* (frame-height) (/ parcent 100.0)))) buf)))
(setq anything-display-function 'anything-display-function--split-root)
