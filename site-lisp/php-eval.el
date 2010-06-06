;;; php-eval.el --- eval PHP script and display the result.

;; Copyright (C) 2008 Kazushi NODA

;; Author: Kazushi NODA (http://www.ne.jp/asahi/alpha/kazu/)
;; Version: $Id: php-eval.el,v 1.17 2008/03/10 17:31:53 kazu Exp $
;; Keywords: 

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; (require 'php-mode)
;; (require 'php-eval)
;; (eval-after-load 'php-eval
;;   '(progn
;;     (define-key php-mode-map "\C-x\C-e" 'php-eval-delimiter)
;;     (define-key php-mode-map "\C-c\C-i" 'php-eval-insert-delimiter)
;;     (define-key php-mode-map "\C-c\C-x" 'php-eval-tag)
;;     (define-key php-mode-map "\C-c\C-e" 'php-eval-last-tag)
;;     (define-key php-mode-map "\C-c\C-r" 'php-eval-region)
;;     (define-key php-mode-map "\C-c\C-v" 'php-eval-display-buffer)
;;     ))

;;; Code:

(defvar php-eval-output-buffer-height 10
  "*Window height of `php-eval-output-buffer'.")

(defvar php-eval-browse-url-of-buffer-key "\C-c\C-b"
  "*Key binding for `browse-url-of-buffer' in `php-eval-output-buffer'.")

(defvar php-eval-tag-template "<?php . ?>"
  "*PHP tag template for `php-eval-insert-delimiter'.")

(defvar php-eval-delimiter-string (format-time-string "// %Y-%m-%d %T\n")
  "*Delimiter string.")

(defvar php-eval-delimiter-re "^// [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
  "*Delimiter regexp.")

(defvar php-eval-progname "php" "*PHP command name.")

(defvar php-eval-output-buffer "*PHP-OUTPUT*"
  "*OUTPUT-BUFFER name of `shell-command-on-region'.")

(defvar php-eval-start-tag "<\\?\\(?:php\\)?" "*Start PHP tag.")

(defvar php-eval-end-tag "\\?>" "*End PHP tag.")

(defvar php-eval-highlight-mode t
  "*Highlight the region between the boundaries.
If nil, the region is not highlighted.")

(defvar php-eval-boundaries-face 'highlight
  "*php-eval face use to highlight boundaries.")

(defun php-eval-insert-delimiter ()
  (interactive)
  (insert php-eval-delimiter-string)
  (unless current-prefix-arg
    (insert php-eval-tag-template)
    (and (search-backward "." (length php-eval-tag-template) t)
	 (delete-char 1))))

(defun php-eval-region (start end)
  (interactive "r")
  (php-eval-do-eval start end))

(defun php-eval-tag (start end)
  (interactive (php-eval-interactive 'in-tag))
  (php-eval-do-eval start end))

(defun php-eval-last-tag (start end)
  (interactive (php-eval-interactive 'last-tag))
  (php-eval-do-eval start end))

(defun php-eval-delimiter (start end)
  (interactive (php-eval-interactive 'delimiter))
  (php-eval-do-eval start end))

(defun php-eval-do-eval (start end)
  (let ((buffer php-eval-output-buffer)
	(height php-eval-output-buffer-height))
    (setq current-prefix-arg nil)
    (php-eval-display-buffer 'erase)
    (when php-eval-highlight-mode
      (php-eval-highlight-boundaries start end))
    (shell-command-on-region start end php-eval-progname buffer)))

(defun php-eval-display-buffer (&optional erase)
  (interactive)
  (let ((height php-eval-output-buffer-height)
	(buffer php-eval-output-buffer)
	(key php-eval-browse-url-of-buffer-key))
    (save-selected-window
      (pop-to-buffer buffer)
      (when erase (erase-buffer))
      (unless (key-binding key)
	(local-set-key key 'browse-url-of-buffer))
      (shrink-window (- (window-height) height 1)))))

(defun php-eval-highlight-boundaries (start end)
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face php-eval-boundaries-face)
    (sit-for 0.1)
    (delete-overlay ov)))

(defun php-eval-interactive (boundary)
  (cond ((eq boundary 'region)
	 (list (region-beginning) (region-end)))
	((memq boundary '(in-tag last-tag))
	 (php-eval-boundaries
	  php-eval-start-tag php-eval-end-tag boundary))
	((eq boundary 'delimiter)
	 (php-eval-boundaries
	  php-eval-delimiter-re php-eval-delimiter-re boundary))
	(t (error "Not supported."))))

(defun php-eval-boundaries (start-tag end-tag boundary)
  (save-excursion
    (let ((p (point))
	  (start (or (and (looking-at start-tag) (point))
		     (progn
		       (goto-char (line-end-position))
		       (re-search-backward start-tag nil t))))
	  (end (progn
		 (forward-char)
		 (if (re-search-forward end-tag nil t)
		     (cond ((eq boundary 'delimiter)
			    (forward-line 0) (point))
			   (t (point)))
		   (and (eq boundary 'delimiter) (point-max))))))
      (unless (and start end)
	(error "There is no boundary."))
      (cond ((and (eq boundary 'last-tag)
		  (<= end p))
	     (list start end))
	    ((and (eq boundary 'in-tag)
		  (> end p))
	     (list start end))
	    ((and (eq boundary 'delimiter)
		  (>= end p))
	     (list start end))
	    (t (error "There are neither a start tag nor an end tag."))))))

(provide 'php-eval)

;;; php-eval.el ends here
