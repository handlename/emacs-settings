;; -*- Emacs-Lisp -*-
;; -*- coding: euc-jp-unix -*-
;;; migemo.el - Japanese incremental search trough dynamic pattern expansion

;; $Id: migemo.el.in,v 1.27 2003/05/29 08:09:00 satoru Exp $
;; Copyright (C) Satoru Takabayashi

;; Author: Satoru Takabayashi <satoru-t@is.aist-nara.ac.jp>
;; Keywords: 

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(defvar migemo-command "ruby"
  "*Name or full path of the executable for running migemo.")

;; -t emacs for specifying the type of regular expression.
;;  "-i" "\a" for searching a word over multi-lines.
(defvar migemo-options '("-S" "migemo" "-t" "emacs"  "-i" "\a")
  "*Options for migemo command.")

(defvar migemo-white-space-regexp "[ 　\t\r\n]*"
  "*Regexp representing white spaces.")

;; for C/Migemo
;; (setq migemo-command "cmigemo")
;; (setq migemo-options '("-q" "--emacs" "-i" "\g"))
;; (setq migemo-dictionary "somewhere/migemo/euc-jp/migemo-dict")
;; (setq migemo-user-dictionary nil)
;; (setq migemo-regex-dictionary nil))

(defvar migemo-directory "/usr/local/share/migemo"
  "*Directory where migemo files are placed")

(defvar migemo-isearch-enable-p t
  "*Enable the migemo feature on isearch or not.")

(defvar migemo-dictionary (expand-file-name "migemo-dict" migemo-directory)
  "*Migemo dictionary file.")

(defvar migemo-user-dictionary (expand-file-name "user-dict" migemo-directory)
  "*Migemo user dictionary file.")

(defvar migemo-regex-dictionary (expand-file-name "regex-dict" migemo-directory)
  "*Migemo regex dictionary file.")

(defvar migemo-coding-system
  (if (>= emacs-major-version 20)
      (if (featurep 'mule)
	  (if (string-match "XEmacs" emacs-version)
	      (cond
	       ((memq 'euc-japan-unix (coding-system-list)) 'euc-japan-unix)
	       ((memq 'euc-jp-unix (coding-system-list)) 'euc-jp-unix))
	    'euc-japan-unix))
    (and (boundp 'MULE) *euc-japan*unix))
  "*Default coding system for migemo.el")

(defvar migemo-use-pattern-alist nil
  "*Use pattern cache.")

(defvar migemo-use-frequent-pattern-alist nil
  "*Use frequent patttern cache.")

(defvar migemo-pattern-alist-length 512
  "*Maximal length of migemo-pattern-alist.")

(defvar migemo-pattern-alist-file "~/.migemo-pattern"
  "*Path of migemo alist file. If nil, don't save and restore the file.")

(defvar migemo-frequent-pattern-alist-file "~/.migemo-frequent"
  "*Path of migemo frequent alist file. If nil, don't save and restore the file.")

(defconst migemo-mw32-input-method (and (featurep 'meadow) "MW32-IME")
  "Support \"MW32-IME\" for Meadow.")

;; internal variables
(defvar migemo-process nil)
(defvar migemo-buffer nil)
(defvar migemo-current-input-method nil)
(defvar migemo-search-pattern nil)
(defvar migemo-pattern-alist nil)
(defvar migemo-frequent-pattern-alist nil)
(defconst migemo-emacs21p (> emacs-major-version 20) (not (featurep 'xemacs)))
(defvar migemo-search-pattern-alist nil)
(defvar migemo-do-isearch nil)

(defsubst migemo-search-pattern-get (string)
  (let ((pattern (cdr (assoc string migemo-search-pattern-alist))))
    (unless pattern
      (setq pattern (migemo-get-pattern string))
      (setq migemo-search-pattern-alist
	    (cons (cons string pattern)
		  migemo-search-pattern-alist)))
    pattern))

(defun migemo-toggle-isearch-enable ()
  (interactive)
  (setq migemo-isearch-enable-p (not migemo-isearch-enable-p))
  (message (if migemo-isearch-enable-p
	       "t"
	     "nil")))

(defun migemo-start-process (name buffer program args) 
  (let ((proc (apply 'start-process name buffer program args)))
    (if (fboundp 'set-process-coding-system)
	(set-process-coding-system proc 
				   migemo-coding-system 
				   migemo-coding-system)
      (set-process-input-coding-system  proc migemo-coding-system)
      (set-process-output-coding-system proc migemo-coding-system))
    proc))

(defun migemo-init ()
  (when (and migemo-use-frequent-pattern-alist
	     migemo-frequent-pattern-alist-file
	     (null migemo-frequent-pattern-alist))
    (setq migemo-frequent-pattern-alist
	  (migemo-pattern-alist-load migemo-frequent-pattern-alist-file)))
  (when (and migemo-use-pattern-alist
	     migemo-pattern-alist-file
	     (null migemo-pattern-alist))
    (setq migemo-pattern-alist
	  (migemo-pattern-alist-load migemo-pattern-alist-file)))
  (or (and migemo-process
	   (eq (process-status migemo-process) 'run))
      (let ((options
	     (delq nil
		   (append migemo-options
			   (when (and migemo-user-dictionary
				      (file-exists-p migemo-user-dictionary))
			     (list "-u" migemo-user-dictionary))
			   (when (and migemo-regex-dictionary
				      (file-exists-p migemo-regex-dictionary))
			     (list "-r" migemo-regex-dictionary))
			   (list "-d" migemo-dictionary)))))
	(setq migemo-buffer (get-buffer-create " *migemo*"))
	(setq migemo-process (migemo-start-process 
			      "migemo" migemo-buffer migemo-command options))
	(process-kill-without-query migemo-process)
	t)))

;;
;; Imported from subr.el in GNU Emacs 21.2.1
;;
(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp rep string &optional
					  fixedcase literal subexp start)
    "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"
  
    ;; To avoid excessive consing from multiple matches in long strings,
    ;; don't just call `replace-match' continually.  Walk down the
    ;; string looking for matches of REGEXP and building up a (reversed)
    ;; list MATCHES.  This comprises segments of STRING which weren't
    ;; matched interspersed with replacements for segments that were.
    ;; [For a `large' number of replacments it's more efficient to
    ;; operate in a temporary buffer; we can't tell from the function's
    ;; args whether to choose the buffer-based implementation, though it
    ;; might be reasonable to do so for long enough STRING.]
    (let ((l (length string))
	  (start (or start 0))
	  matches str mb me)
      (save-match-data
	(while (and (< start l) (string-match regexp string start))
	  (setq mb (match-beginning 0)
		me (match-end 0))
	  ;; If we matched the empty string, make sure we advance by one char
	  (when (= me mb) (setq me (min l (1+ mb))))
	  ;; Generate a replacement for the matched substring.
	  ;; Operate only on the substring to minimize string consing.
	  ;; Set up match data for the substring for replacement;
	  ;; presumably this is likely to be faster than munging the
	  ;; match data directly in Lisp.
	  (string-match regexp (setq str (substring string mb me)))
	  (setq matches
		(cons (replace-match (if (stringp rep)
					 rep
				       (funcall rep (match-string 0 str)))
				     fixedcase literal str subexp)
		      (cons (substring string start mb) ; unmatched prefix
			    matches)))
	  (setq start me))
	;; Reconstruct a string from the pieces.
	(setq matches (cons (substring string start l) matches)) ; leftover
	(apply #'concat (nreverse matches))))))

(defun migemo-get-pattern (word)
  (if (equal word "")
      ""
    (set-text-properties 0 (length word) nil word)
    (migemo-init)
    (let ((orig-buffer (current-buffer))
	  pattern freq alst)
      (cond
       ((setq freq (and migemo-use-frequent-pattern-alist
			(assoc word migemo-frequent-pattern-alist)))
	(cdr freq))
       ((setq alst (and migemo-use-pattern-alist
			(assoc word migemo-pattern-alist)))
	(setq migemo-pattern-alist (cons alst (delq alst migemo-pattern-alist)))
	(cdr alst))
       (t
	(save-excursion
	  (set-buffer (process-buffer migemo-process))
	  (delete-region (point-min) (point-max))
	  (process-send-string migemo-process (concat word "\n"))
	  (while (not (and (> (point-max) 1)
			   (eq (char-after (1- (point-max))) ?\n)))
	    (accept-process-output migemo-process 0 5))
	  (setq pattern (buffer-substring (point-min) (1- (point-max))))
	  (when (and (memq system-type '(windows-nt OS/2 emx))
		     (> (length pattern) 1)
		     (eq ?\r (aref pattern (1- (length pattern)))))
	    (setq pattern (substring pattern 0 -1)))
	  (prog1
 	      (setq pattern (replace-regexp-in-string 
                             "\a" 
                             migemo-white-space-regexp
                             pattern nil t))
	    (when migemo-use-pattern-alist
	      (setq migemo-pattern-alist
		    (cons (cons word pattern) migemo-pattern-alist))
	      (when (and migemo-pattern-alist-length
			 (> (length migemo-pattern-alist)
			    (* migemo-pattern-alist-length 2)))
		(setcdr (nthcdr (1- (* migemo-pattern-alist-length 2))
				migemo-pattern-alist) nil))))))))))

(defun migemo-pattern-alist-load (file)
  "Load migemo alist file."
  (let ((coding-system-for-read migemo-coding-system)
	(file-coding-system-for-read migemo-coding-system))
    (setq file (expand-file-name file))
    (when (file-readable-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(condition-case err
	    (read (current-buffer))
	  (error
	   (message "Error while reading %s; %s"
		    (file-name-nondirectory file)
		    (error-message-string err))
	   nil))))))

(defun migemo-pattern-alist-save (&optional clear)
  "Save migemo alist file."
  (interactive)
  (when (and migemo-use-pattern-alist
	     migemo-pattern-alist-file
	     (or migemo-pattern-alist clear))
    (let ((file (expand-file-name migemo-pattern-alist-file))
	  (coding-system-for-write migemo-coding-system)
	  (file-coding-system  migemo-coding-system))
      (when (file-writable-p file)
	(when clear
	  (setq migemo-pattern-alist nil))
	(when (and migemo-pattern-alist-length
		   (> (length migemo-pattern-alist) migemo-pattern-alist-length))
	  (setcdr (nthcdr (1- migemo-pattern-alist-length)
			  migemo-pattern-alist) nil))
	(with-temp-buffer
	  (if (fboundp 'pp)
	      (pp migemo-pattern-alist (current-buffer))
	    (prin1 migemo-pattern-alist (current-buffer)))
	  (write-region (point-min) (point-max) file nil 'nomsg))
	(setq migemo-pattern-alist nil)))))

(defun migemo-kill ()
  "Kill migemo process"
  (interactive)
  (when (and migemo-process (eq (process-status migemo-process) 'run))
    (kill-process migemo-process)
    (setq migemo-process nil)
    (when (get-buffer migemo-buffer)
      (kill-buffer migemo-buffer))))

(defun migemo-pattern-alist-clear ()
  "Clear migemo alist data & file."
  (interactive)
  (migemo-kill)
  (migemo-pattern-alist-save 'clear)
  (migemo-init))

(defun migemo-frequent-pattern-make (fcfile)
  "Create frequent pattern from `frequent-chars'."
  (interactive "ffrequent-chars: ")
  (migemo-pattern-alist-save 'clear)
  (when migemo-frequent-pattern-alist-file
    (migemo-kill)
    (migemo-init)
    (let ((file (expand-file-name migemo-frequent-pattern-alist-file))
	  (coding-system-for-write migemo-coding-system)
	  (file-coding-system  migemo-coding-system)
	  (migemo-use-pattern-alist nil)
	  (migemo-use-frequent-pattern-alist nil)
	  word regex)
      (setq migemo-frequent-pattern-alist nil)
      (with-temp-buffer
	(insert-file-contents fcfile)
	(goto-char (point-min))
	(message "Make frequently pattern...")
	(while (not (eobp))
	  (when (looking-at "^[a-z]+$")
	    (setq word (match-string 0))
	    (message "Make frequently pattern...%s" word)
	    (setq migemo-frequent-pattern-alist
		  (cons (cons word (migemo-get-pattern word))
			migemo-frequent-pattern-alist)))
	  (forward-line 1))
	(when (file-writable-p file)
	  (setq migemo-frequent-pattern-alist
		(nreverse migemo-frequent-pattern-alist))
	  (erase-buffer)
	  (if (fboundp 'pp)
	      (pp migemo-frequent-pattern-alist (current-buffer))
	    (prin1 migemo-frequent-pattern-alist (current-buffer)))
	  (write-region (point-min) (point-max) file nil 'nomsg)))
      (migemo-kill)
      (migemo-init)
      (message "Make frequently pattern...done"))))

(defun migemo-expand-pattern () "\
Expand the Romaji sequences on the left side of the cursor
into the migemo's regexp pattern."
  (interactive)
  (let ((pos (point)))
    (goto-char (- pos 1))
    (if (re-search-backward "[^-a-zA-Z]" (line-beginning-position) t)
	(forward-char 1)
      (beginning-of-line))
    (let* ((str (buffer-substring-no-properties (point) pos))
	   (jrpat (migemo-get-pattern str)))
      (delete-region (point) pos)
      (insert jrpat))))

(defun migemo-forward (word &optional bound noerror count)
  (interactive "sSearch: \nP\nP")
  (if (delq 'ascii (find-charset-string word))
      (setq migemo-search-pattern word)
    (setq migemo-search-pattern (migemo-search-pattern-get word)))
  (search-forward-regexp migemo-search-pattern bound noerror count))

(defun migemo-backward (word &optional bound noerror count)
  (interactive "sSearch backward: \nP\nP")
  (if (delq 'ascii (find-charset-string word))
      (setq migemo-search-pattern word)
    (setq migemo-search-pattern (migemo-search-pattern-get word)))
  (if (null migemo-do-isearch)
      (search-backward-regexp migemo-search-pattern bound noerror count)
    (or (and (not (eq this-command 'isearch-repeat-backward))
	     (not (get-char-property (point) 'invisible (current-buffer)))
	     (looking-at migemo-search-pattern))
	(search-backward-regexp migemo-search-pattern bound noerror count))))

;; experimental
;; (define-key global-map "\M-;" 'migemo-dabbrev-expand)
(defvar migemo-dabbrev-display-message nil
  "*Display dabbrev message to minibuffer.")
  
(defvar migemo-dabbrev-pattern nil)
(defvar migemo-dabbrev-start-point nil)
(defvar migemo-dabbrev-search-point nil)
(defvar migemo-dabbrev-pre-patterns nil)
(defvar migemo-dabbrev-ol nil)
(defvar migemo-dabbrev-ol-face 'highlight)

(defun migemo-dabbrev-expand-done ()
  (remove-hook 'pre-command-hook 'migemo-dabbrev-expand-done)
  (unless (eq last-command this-command)
      (setq migemo-search-pattern-alist nil)
      (setq migemo-dabbrev-pre-patterns nil))
  (when migemo-dabbrev-ol
    (delete-overlay migemo-dabbrev-ol)))

(defun migemo-dabbrev-expand ()
  (interactive)
  (let ((end-pos (point))
	matched-start matched-string)
    (if (eq last-command this-command)
	(goto-char migemo-dabbrev-search-point)
      (goto-char (- end-pos 1))
      (if (re-search-backward "[^a-z-]" (line-beginning-position) t)
	  (forward-char 1)
	(beginning-of-line))
      (setq migemo-search-pattern-alist nil)
      (setq migemo-dabbrev-start-point (point))
      (setq migemo-dabbrev-search-point (point))
      (setq migemo-dabbrev-pattern
	    (buffer-substring-no-properties (point) end-pos))
      (setq migemo-dabbrev-pre-patterns nil))
    (if (catch 'found
	  (while (if (> migemo-dabbrev-search-point migemo-dabbrev-start-point)
		     (and (migemo-forward migemo-dabbrev-pattern (point-max) t)
			  (setq migemo-dabbrev-search-point (match-end 0)))
		   (if (migemo-backward migemo-dabbrev-pattern (point-min) t)
		       (setq migemo-dabbrev-search-point (match-beginning 0))
		     (goto-char migemo-dabbrev-start-point)
		     (forward-word 1)
		     (message (format "Trun back for `%s'" migemo-dabbrev-pattern))
		     (and (migemo-forward migemo-dabbrev-pattern (point-max) t)
			  (setq migemo-dabbrev-search-point (match-end 0)))))
	    (setq matched-start (match-beginning 0))
	    (unless (re-search-forward ".\\>" (line-end-position) t)
	      (end-of-line))
	    (setq matched-string (buffer-substring-no-properties matched-start (point)))
	    (unless (member matched-string migemo-dabbrev-pre-patterns)
	      (let ((matched-end (point))
		    (str (copy-sequence matched-string))
		    lstart lend)
		(if (and (pos-visible-in-window-p matched-start)
			 (pos-visible-in-window-p matched-end))
		    (progn
		      (if migemo-dabbrev-ol
			  (move-overlay migemo-dabbrev-ol matched-start (point))
			(setq migemo-dabbrev-ol (make-overlay matched-start (point))))
		      (overlay-put migemo-dabbrev-ol 'evaporate t)
		      (overlay-put migemo-dabbrev-ol 'face migemo-dabbrev-ol-face))
		  (when migemo-dabbrev-ol
		    (delete-overlay migemo-dabbrev-ol))
		  (when migemo-dabbrev-display-message
		    (save-excursion
		      (save-restriction
			(goto-char matched-start)
			(setq lstart (progn (beginning-of-line) (point)))
			(setq lend (progn (end-of-line) (point)))
			(if migemo-emacs21p
			    (put-text-property 0 (length str)
					       'face migemo-dabbrev-ol-face str)
			  (setq str (concat "【" str "】")))
			(message "(%d): %s%s%s"
				 (count-lines (point-min) matched-start)
				 (buffer-substring-no-properties lstart matched-start)
				 str
				 (buffer-substring-no-properties matched-end lend)))))))
	      (throw 'found t))
	    (goto-char migemo-dabbrev-search-point)))
	(progn
	  (setq migemo-dabbrev-pre-patterns
		(cons matched-string migemo-dabbrev-pre-patterns))
	  (delete-region migemo-dabbrev-start-point end-pos)
	  (forward-char 1)
	  (goto-char migemo-dabbrev-start-point)
	  (insert matched-string))
      (goto-char end-pos)
      (message (format "No dynamic expansion for `%s' found"
		       migemo-dabbrev-pattern)))
    (add-hook 'pre-command-hook 'migemo-dabbrev-expand-done)))

;; Use migemo-{forward,backward} instead of search-{forward,backward}.
(defadvice isearch-search (around migemo-search-ad activate)
  "adviced by migemo."
  (when migemo-isearch-enable-p
    (setq migemo-do-isearch t))
  (unwind-protect
      ad-do-it
    (setq migemo-do-isearch nil)))

(defadvice isearch-search-and-update (around migemo-search-ad activate)
  "adviced by migemo."
  (let ((isearch-adjusted isearch-adjusted))
    (when (and migemo-isearch-enable-p
	       (not isearch-forward) (not isearch-regexp) (not isearch-word))
      ;; don't use 'looking-at'
      (setq isearch-adjusted t))
    ad-do-it))

(defadvice search-forward (around migemo-search-ad activate)
  "adviced by migemo."
  (if migemo-do-isearch
      (setq ad-return-value
	    (migemo-forward (ad-get-arg 0) (ad-get-arg 1) (ad-get-arg 2) (ad-get-arg 3)))
    ad-do-it))

(defadvice search-backward (around migemo-search-ad activate)
  "adviced by migemo."
  (if migemo-do-isearch
      (setq ad-return-value
	    (migemo-backward (ad-get-arg 0) (ad-get-arg 1) (ad-get-arg 2) (ad-get-arg 3)))
    ad-do-it))

;; Turn off input-method automatically when C-s or C-r are typed.
(defadvice isearch-mode (before migemo-search-ad activate)
  "adviced by migemo."
  (setq migemo-search-pattern nil)
  (setq migemo-search-pattern-alist nil)
  (when (and migemo-isearch-enable-p
	     (boundp 'current-input-method))
    (setq migemo-current-input-method current-input-method)
    (when (and migemo-mw32-input-method
	       (stringp migemo-current-input-method)
	       (string= migemo-current-input-method migemo-mw32-input-method))
      (set-input-method nil))
    (setq current-input-method nil)))

(defadvice isearch-done (after migemo-search-ad activate)
  "adviced by migemo."
  (setq migemo-search-pattern nil)
  (setq migemo-search-pattern-alist nil)
  (when (and migemo-isearch-enable-p
	     (boundp 'current-input-method))
    (when (and migemo-mw32-input-method
	       (stringp migemo-current-input-method)
	       (string= migemo-current-input-method migemo-mw32-input-method))
      (set-input-method migemo-current-input-method))
    (setq current-input-method migemo-current-input-method)))

(defvar migemo-message-prefix-face 'highlight
  "*Face of minibuffer prefix")

(defadvice isearch-message-prefix (after migemo-status activate)
  "adviced by migemo."
  (let ((ret ad-return-value)
	(str "[MIGEMO]"))
    (when migemo-emacs21p
      (put-text-property 0 (length str) 'face migemo-message-prefix-face str))
    (when (and migemo-isearch-enable-p
	       (not (or isearch-regexp isearch-word)))
      (setq ad-return-value (concat str " " ret)))))

;;;; for isearch-lazy-highlight (Emacs 21)
;; Avoid byte compile warningsfor other emacsen
(defvar isearch-lazy-highlight-wrapped)
(defvar isearch-lazy-highlight-start)
(defvar isearch-lazy-highlight-end)

(defun migemo-isearch-lazy-highlight-search ()
  "Search ahead for the next or previous match, for lazy highlighting.
Attempt to do the search exactly the way the pending isearch would.
This function used with Megemo feature."
  (let ((case-fold-search isearch-case-fold-search)
        (choices (cond (isearch-word
                        '(word-search-forward word-search-backward))
                       (isearch-regexp
                        '(re-search-forward re-search-backward))
                       (migemo-isearch-enable-p
                        '(re-search-forward re-search-backward t))
                       (t
                        '(search-forward search-backward))))
	(pattern isearch-string))
    (when (nth 2 choices)
     (setq pattern (migemo-search-pattern-get isearch-string)))
    (funcall (if isearch-forward
		 (nth 0 choices) (nth 1 choices))
	     pattern
             (if isearch-forward
                 (if isearch-lazy-highlight-wrapped
                     isearch-lazy-highlight-start
                   (window-end))
               (if isearch-lazy-highlight-wrapped
                   isearch-lazy-highlight-end
                 (window-start)))
             t)))

(when (fboundp 'isearch-lazy-highlight-search)
  (defalias 'isearch-lazy-highlight-search 'migemo-isearch-lazy-highlight-search))

;;;; for isearch-highlightify-region (XEmacs 21)
;; Avoid byte compile warningsfor other emacsen
(defvar isearch-highlightify-region)

(when (fboundp 'isearch-highlightify-region)
  (defadvice isearch-highlightify-region (around migemo-highlightify-region
						 activate)
    "adviced by migemo."
    (let ((isearch-string (migemo-search-pattern-get isearch-string))
	  (isearch-regexp t))
      ad-do-it)))

;; supports C-w C-d for GNU emacs only [migemo:00171]
(when (and (not (featurep 'xemacs))
	   (fboundp 'isearch-yank-line))
  (add-hook 'isearch-mode-hook
	    (lambda ()
	      (define-key isearch-mode-map "\C-d" 'migemo-isearch-yank-char)
	      (define-key isearch-mode-map "\C-w" 'migemo-isearch-yank-word)
	      (define-key isearch-mode-map "\C-y" 'migemo-isearch-yank-line)
	      (define-key isearch-mode-map "\C-e" 'migemo-isearch-toggle-migemo)))
  
  (defun migemo-isearch-toggle-migemo ()
    "Toggle migemo mode in isearch."
    (interactive)
    (unless (or isearch-regexp isearch-word)
      (discard-input)
      (setq migemo-isearch-enable-p (not migemo-isearch-enable-p)))
    (isearch-message))

  (defun migemo-isearch-yank-char ()
    "Pull next character from buffer into search string with migemo."
    (interactive)
    (when (and migemo-isearch-enable-p
               (not isearch-regexp) isearch-other-end)
      (setq isearch-string (buffer-substring-no-properties
                            isearch-other-end (point)))
      (setq isearch-message isearch-string))
    (let ((search-upper-case (unless migemo-isearch-enable-p
			       search-upper-case)))
      (isearch-yank-string
       (save-excursion
	 (and (not isearch-forward) isearch-other-end
	      (goto-char isearch-other-end))
	 (buffer-substring-no-properties (point) 
					 (progn (forward-char 1) (point)))))))

  (defun migemo-isearch-yank-word ()
    "Pull next character from buffer into search string with migemo."
    (interactive)
    (when (and migemo-isearch-enable-p
               (not isearch-regexp) isearch-other-end)
      (setq isearch-string (buffer-substring-no-properties
                            isearch-other-end (point)))
      (setq isearch-message isearch-string))
    (let ((search-upper-case (unless migemo-isearch-enable-p
			       search-upper-case)))
      (isearch-yank-string
       (save-excursion
	 (and (not isearch-forward) isearch-other-end
	      (goto-char isearch-other-end))
	 (buffer-substring-no-properties (point) 
					 (progn (forward-word 1) (point)))))))

  (defun migemo-isearch-yank-line ()
    "Pull next character from buffer into search string with migemo."
    (interactive)
    (when (and migemo-isearch-enable-p
               (not isearch-regexp) isearch-other-end)
      (setq isearch-string (buffer-substring-no-properties
                            isearch-other-end (point)))
      (setq isearch-message isearch-string))
    (let ((search-upper-case (unless migemo-isearch-enable-p
			       search-upper-case)))
      (isearch-yank-string
       (save-excursion
	 (and (not isearch-forward) isearch-other-end
	      (goto-char isearch-other-end))
	 (buffer-substring-no-properties (point)
					 (line-end-position))))))
)

(add-hook 'kill-emacs-hook 'migemo-pattern-alist-save)

(provide 'migemo)

;; sample
;; 0123 abcd ABCD ひらがな カタカナ 漢字 !"[#\$]%^&_':`(;)<*=+>,?-@./{|}~

