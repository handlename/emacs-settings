;; 保存前に余計な空白文字を削除
(add-hook 'before-save-hook
          '(lambda ()
             (whitespace-cleanup)))
