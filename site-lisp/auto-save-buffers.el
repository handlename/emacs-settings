;;
;; auto-save-buffers.el
;;
;; ���Υ����ɤϻ��������᤬�񤤤Ƥ������ä� (ELF:01128)
;;
;; �Ȥ���:
;;
;;   (require 'auto-save-buffers)
;;   (run-with-idle-timer 0.5 t 'auto-save-buffers) ; �����ɥ�0.5�ä���¸
;;
;; auto-save-buffers �� on/off ���ڤ��ؤ��뤿��Υ������ (C-x a s)
;;
;;   (define-key ctl-x-map "as" 'auto-save-buffers-toggle)
;;

;; auto-save-buffers ���оݤȤ���ե�����̾������ɽ��
(defvar auto-save-buffers-regexp ""
  "*Regexp that matches `buffer-file-name' to be auto-saved.")

;; auto-save-buffers �ǽ�������ե�����̾������ɽ��
(defvar auto-save-buffers-exclude-regexp "^$"
  "*Regexp that matches `buffer-file-name' not to be auto-saved.")

;;
;; ���뤤�� auto-save-buffers �ΰ���������ɽ������ꤹ�뤳�Ȥ�Ǥ���
;;
;; (require 'auto-save-buffers)
;; (run-with-idle-timer 0.5 t 'auto-save-buffers "\\.c$" "^$") ; .c �����о�
;; (run-with-idle-timer 0.5 t 'auto-save-buffers ""   "\\.h$") ; .h ��������
;;

;; nil �ʤ饻���֤��ʤ� (�����ޡ��ϲ�ä��ޤ�)
(defvar auto-save-buffers-active-p t
  "If non-nil, `auto-save-buffers' saves buffers.")

;; ��ά��ǽ�ΰ����ǡ�include/exclude �Ѥ�����ɽ�������Ǥ���
(defun auto-save-buffers (&rest regexps)
  "Save buffers if `buffer-file-name' matches `auto-save-buffers-regexp'."
  (let ((include-regexp (or (car  regexps) auto-save-buffers-regexp))
        (exclude-regexp (or (cadr regexps) auto-save-buffers-exclude-regexp))
        (buffers (buffer-list)))
    (save-excursion
      (while buffers
	(set-buffer (car buffers))
	(if (and buffer-file-name
                 auto-save-buffers-active-p 
		 (buffer-modified-p)
		 (not buffer-read-only)
		 (string-match include-regexp buffer-file-name)
                 (not (string-match exclude-regexp buffer-file-name))
		 (file-writable-p buffer-file-name))
	    (save-buffer))
	(setq buffers (cdr buffers))))))

;; auto-save-buffers �� on/off ��ȥ�����ڤ��ؤ���
;; Based on the code by Yoshihiro (��������� 2004-03-23)
(defun auto-save-buffers-toggle ()
  "Toggle `auto-save-buffers'"
  (interactive)
  (if auto-save-buffers-active-p
      (setq auto-save-buffers-active-p nil)
    (setq auto-save-buffers-active-p t))
  (if auto-save-buffers-active-p
      (message "auto-save-buffers on")
    (message "auto-save-buffers off")))

;;
;; Emacs 21 �ʹߤ� Makefile ���Խ����� "Suspicious line XXX. Save anyway"
;; �Ȥ����ץ��ץȤ�Ф��ʤ��褦�ˤ��뤿��Τ��ޤ��ʤ�
;;
(add-hook 'makefile-mode-hook
          (function (lambda ()
                      (fset 'makefile-warn-suspicious-lines 'ignore))))

(provide 'auto-save-buffers)

