;;; GCを減らす
(setq gc-cons-threshold (* 50 gc-cons-threshold))

;;; ログの記録量を増やす
(setq message-log-max 10000)

;;; 履歴存数を増やす
(setq history-length 1000)


;;; ダイアログボックスを使わない
(setq use-dialog-box nil)
(defalias 'message-box 'message)

;;; yesで答る部分もyで答えられるように
(defalias 'yes-or-no-p 'y-or-n-p)
