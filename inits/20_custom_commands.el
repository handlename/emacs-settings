;;
;;
;; 関数定義
;;
;;


;; 現在の日付を挿入
(defun current-date-string () (interactive) "現在の日付を挿入する" (insert (format-time-string "%Y-%m-%d")))

;; すべてのバッファをkill
(defun kill-all-buffers ()
  (interactive)
  (loop for buffer being the buffers
        do (kill-buffer buffer)))
