;; read-onlyならsudoで開く
(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))


;; sudoで固まるのを解消
;; (eval-after-load "tramp"
;;   '(progn
;;      (add-to-list 'tramp-methods
;;                   (cons "sudoHs" (copy-tree (cdr (assoc "sudo" tramp-methods)))))
;;      (let ((x (cdr (cadr (assoc 'tramp-login-args (cdr (assoc "sudo" tramp-methods)))))))
;;        (setcar x (cons "-i" (delete "-H" (delete "-s" (car x))))))
;;      (let ((x (cdr (cadr (assoc 'tramp-login-args (cdr (assoc "sudoHs" tramp-methods)))))))
;;        (setcar x (cons "-H" (delete "-H" (car x)))))))