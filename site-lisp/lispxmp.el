;;; lispxmp.el --- Automagic emacs lisp code annotation
;; $Id: lispxmp.el,v 1.9 2009/03/17 09:23:41 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: lisp, convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/lispxmp.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Automagical annotation of lisp values like Ruby's xmpfilter. For
;; example, executing M-x lispxmp on the following buffer:
;;
;; ====
;; 1 ; =>
;; (+ 3 4) ; =>
;; (dotimes (i 3)
;;   (* i 4)  ; =>
;; )
;; ====
;; 
;; produces
;; 
;; ====
;; 1 ; => 1
;; (+ 3 4) ; => 7
;; (dotimes (i 3)
;;   (* i 4) ; => 0, 4, 8
;; )
;; ====


;;; Commands:
;;
;; Below are complete command list:
;;
;;  `lispxmp'
;;    Annotate value of lines containing `; =>' .
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;;
;; Put lispxmp.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lispxmp)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lispxmp RET
;;


;;; History:

;; $Log: lispxmp.el,v $
;; Revision 1.9  2009/03/17 09:23:41  rubikitch
;; Enter debugger if debug-on-error is non-nil.
;;
;; Revision 1.8  2009/03/17 08:42:38  rubikitch
;; *** empty log message ***
;;
;; Revision 1.7  2009/03/16 22:02:52  rubikitch
;; Bug fix
;;
;; Revision 1.6  2009/03/16 21:50:42  rubikitch
;; Error handling
;;
;; Revision 1.5  2009/03/16 12:15:00  rubikitch
;; refactoring
;;
;; Revision 1.4  2009/03/16 11:59:37  rubikitch
;; Bug fix. (New algorithm)
;;
;; Revision 1.3  2009/03/16 11:23:05  rubikitch
;; refactoring
;;
;; Revision 1.2  2009/03/06 20:19:06  rubikitch
;; Add missing requires
;;
;; Revision 1.1  2009/03/06 19:02:14  rubikitch
;; Initial revision
;;

;;; Code:

(defvar lispxmp-version "$Id: lispxmp.el,v 1.9 2009/03/17 09:23:41 rubikitch Exp $")
(require 'cl)
(require 'newcomment)
(defgroup lispxmp nil
  "lispxmp"
  :group 'emacs)

(defvar lispxmp-temp-buffer " *lispxmp tmp*")
(defvar lispxmp-results nil)
(defun lispxmp ()
  "Annotate value of lines containing `; =>' ."
  (interactive)
  (let ((pt (point)) (wstart (window-start (selected-window))))
    (lispxmp-create-code (current-buffer))
    (erase-buffer)
    (insert-buffer-substring lispxmp-temp-buffer)
    (unwind-protect
        (if debug-on-error
            (eval-buffer)
          (condition-case err
              (eval-buffer)
            (error
             (ding)
             ;; next action when error
             (run-with-timer 0 nil 'message "Error in eval: %S" err))))
      (lispxmp-create-annotations (current-buffer) lispxmp-results)
      (goto-char pt)
      (set-window-start (selected-window) wstart))))

(defun lispxmp-create-code (buf)
  (setq lispxmp-results nil)
  (with-current-buffer (get-buffer-create lispxmp-temp-buffer)
    (buffer-disable-undo)
    (erase-buffer) 
    (let (emacs-lisp-mode-hook) (emacs-lisp-mode))
    (insert-buffer-substring buf)
    (goto-char 1)
    (loop while (re-search-forward "\\(;+\\) =>" nil t)
          for semicolons = (match-string 1)
          for i from 0
          when (lispxmp-annotation-p) do
          (delete-region (match-beginning 0) (point-at-eol))
          (lispxmp-out-make-sexp i)
          (insert (format "%s <<%%lispxmp-out-marker %d>>" semicolons i)))))
;; (progn (lispxmp-create-code (current-buffer))(display-buffer lispxmp-temp-buffer))

(defun lispxmp-annotation-p ()
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (ignore-errors (comment-search-forward (point-at-eol) t))
      (looking-at "=>"))))

(defun lispxmp-out-make-sexp (i)
  (end-of-line)
  (let ((e (make-marker)))
    (set-marker e (point))
    (forward-sexp -1)
    (insert (format "(%%lispxmp-out %s " i))
    (goto-char e)
    (insert ")")))

(defun lispxmp-out-remove ()
  (goto-char (point-min))
  (while (re-search-forward "(%lispxmp-out [0-9]+ " nil t)
    (replace-match "")
    (save-excursion
      (forward-sexp)
      (and (search-forward ")" nil t) (replace-match "")))))

(defvar lispxmp-results nil)
(defun %lispxmp-out (index result)
  (push (cons index result) lispxmp-results)
  result)

(defun lispxmp-create-annotations (buf results)
  (set-buffer buf)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(;+\\) <<%lispxmp-out-marker \\([0-9]+\\)>> *$" nil t)
      (let ((index (string-to-number (match-string 2))))
        (replace-match
         (concat (match-string 1)       ; semicolons
                 " => "
                 ;; pair := (INDEX . VALUE)
                 (mapconcat (lambda (pair) (prin1-to-string (cdr pair)))
                            (remove-if-not (lambda (pair) (= index (car pair)))
                                           (reverse results))
                            ", "))))))
  (lispxmp-out-remove))
;; (with-new-window (find-epp lispxmp-results))

(defadvice comment-dwim (around lispxmp-hack activate)
  "If comment-dwim is successively called, add => mark."
  (if (and (eq major-mode 'emacs-lisp-mode)
           (eq last-command 'comment-dwim)
           ;; TODO =>check
           )
      (insert " =>")
    ad-do-it))

(provide 'lispxmp)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "lispxmp.el")
;;; lispxmp.el ends here
