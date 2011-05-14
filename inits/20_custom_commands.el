;; 現在の日付を挿入
(defun current-date-string () (interactive) "現在の日付を挿入する" (insert (format-time-string "%Y-%m-%d")))

;; すべてのバッファをkill
(defun kill-all-buffers ()
  (interactive)
  (loop for buffer being the buffers
        do (kill-buffer buffer)))

;; ケータイコーディング用
(defun ktai-hankaku-katakana-region (start end)
  (interactive "r")
  (while (string-match
          "[０-９Ａ-Ｚａ-ｚァ-ンー]+"
          (buffer-substring start end))
    (save-excursion
      (japanese-hankaku-region
       (+ start (match-beginning 0))
       (+ start (match-end 0))
       ))))

(defun ktai-hankaku-katakana-buffer ()
  (interactive)
  (ktai-hankaku-katakana-region (point-min) (point-max)))

;; リージョン内の文字幅をしらべる
(defun string-width-in-region (start end)
  (interactive "r")
  (princ (string-width (buffer-substring start end))))
