;;; clmemo.el --- Change Log MEMO -*-emacs-lisp-*-

;; Copyright (c) 2002, 2003, 2004  Masayuki Ataka <ataka@milk.freemail.ne.jp>
;; $Id: clmemo.el,v 2.47.1.4 2004/09/05 14:45:31 Mark Stab $

;; Author: Masayuki Ataka <ataka@milk.freemail.ne.jp>
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; clmemo provides some commands and minor modes for ChangeLog MEMO.

;; `ChangeLog MEMO' is a kind of concept that writing memo into _ONE_
;; file in ChangeLog format.  You can take a memo about address book,
;; bookmark, diary, idea, memo, news, schedule, time table, todo list,
;; citation from book, one-liner that you wrote but will forget, etc....
;;
;; (1) Why one file?
;;
;; * Easy for you to copy and move file, to edit memo, and to find
;;   something important.
;;
;; * Obvious that text you want is in this file.  In other words, if not
;;   in this file, you didn't take a memo about it.  You will be free
;;   from searching all of the files for what you have taken or might
;;   have not.
;;
;; (2) Why ChangeLog format?
;;
;; * One of well known format.
;;
;; * A plain text file.  Binary is usually difficult to edit and search.
;;   File size gets bigger.  And most of binary file needs special soft.
;;   You should check that your soft is distributed permanently.
;;
;; * Easier to read than HTML and TeX.
;;
;; * Entries are automatically sorted by chronological order.  The
;;   record when you wrote memo is stored.
;;

;; Ref.
;;
;; * ChangeLog
;;
;;  - Change Logs in `GNU Emacs Reference Manual'
;;
;; * ChangeLog MEMO
;;
;;  - http://namazu.org/~satoru/unimag/1/ (Japanese)
;;

;; [Acknowledgement]
;;
;; Special thanks to rubikitch for clmemo-yank, clmemo-indent-region,
;; and bug fix of quitting title.  Great thanks goes to Tetsuya Irie,
;; Souhei Kawazu, Shun-ichi Goto, Hideaki Shirai, Keiichi Suzuki, Yuuji
;; Hirose, Katsuwo Mogi, and ELF ML members for all their help.
;;

;; [How to install]
;;
;; The latest clmemo.el is available at:
;;
;;   http://isweb22.infoseek.co.jp/computer/pop-club/emacs/changelog.html
;;

;; Put this in your .emacs file:
;;
;;   (autoload 'clmemo "clmemo" "ChangeLog memo mode." t)

;; And bind it to your favourite key and set titles of MEMO.
;;
;; Example:
;;
;;   (global-set-key "\C-xM" 'clmemo)
;;   (setq clmemo-title-list
;;        '("Emacs" "Music" ("lotr" . "The Load of the Rings") etc...))
;;

;; Finally, put this at the bottom of your ChangeLog MEMO file.
;;
;;   ^L
;;   Local Variables:
;;   mode: change-log
;;   clmemo-mode: t
;;   End:
;;
;; This code tells Emacs to set major mode change-log and toggle minor
;; mode clmemo-mode ON in your ChangeLog MEMO.  For more information,
;; see section "File Variables" in `GNU Emacs Reference Manual'.
;;
;; `^L' is a page delimiter.  You can insert it by `C-q C-l'.
;;
;; If you are Japanese, it is good idea to specify file coding system
;; like this;
;;
;;   ^L
;;   Local Variables:
;;   mode: change-log
;;   clmemo-mode: t
;;   coding: iso-2022-jp
;;   End:
;;

;; [Usage]
;;
;; `M-x clmemo' directly open ChangeLog MEMO file in ChangeLog MEMO
;; mode.  Select your favourite title with completion.  User option
;; `clmemo-title-list' is used for complition.
;;

;; [Related Softwares]
;;
;; * clgrep -- ChangeLog GREP
;;   A grep command specialized for ChangeLog Memo.
;;
;;   - clgrep (Ruby)
;;       http://namazu.org/~satoru/unimag/1/
;;   - blgrep (EmacsLisp)
;;       http://isweb22.infoseek.co.jp/computer/pop-club/emacs/blgrep/
;;
;; * chalow -- CHAnge Log On the Web
;;   A ChangeLog Memo to HTML converter.
;;
;;   - chalow (Perl)
;;       http://nais.to/~yto/tools/chalow/


;;; Code:

(provide 'clmemo)
(require 'add-log)
(eval-when-compile (require 'time-date))


;;
;; User Options
;;

(defvar clmemo-file-name "~/clmemo.txt"
  "*ChangeLog MEMO file name.")

(defvar clmemo-time-string-with-weekday nil
  "*If non-nil, append the day of week after date.")

(defvar clmemo-grep-function 'clgrep
  "*Your favourite ChangeLog grep function.")

(defvar clmemo-paragraph-start "[-*] "
  "*Regexp of `paragraph-start' for ChangeLog Memo.")
(defvar clmemo-paragraph-separate "\\(>>\\|<<\\)"
  "*Regexp of `paragraph-paragraph' for ChangeLog Memo.")



;
; title
;
(defvar clmemo-title-list '("idea" "computer")
  "*List of titles.
Set your favourite title of ChangeLog MEMO.
You can set the alias of the title: (alias . title)")

(defvar clmemo-subtitle-char "("
  "*If this char is in the end of title, ask subtitle.")

(defvar clmemo-subtitle-punctuation-char '(" (" . ")")
  "*Car is left string of subtitle punctuation char; Cdr is right.")

(defvar clmemo-title-format-function nil
  "*Function for formating the title.
The function should take one arg and return the formated string.")

(defvar clmemo-buffer-function-list nil
  "*Function list called after clmemo-new-title.
Function must have one argument BUF.")

(defvar clmemo-new-title-hook nil
  "*Hook run when new title is added.")

;
; tag
;
(defvar clmemo-tag-list '(("url" browse-url-at-point)
			  ("file" find-file-at-point clmemo-read-file-name))
  "*List of TAG in ChangeLog MEMO.
You can set functions when insert or jump: (TAG JUMP-FUNCTION INSERT-FUNCTION).")

(defvar clmemo-tag-url "url" "*Tag name for url.")

;
; Quote
;
(defvar clmemo-quote-prefix ">"
  "*Prefix char for quote")

;
; misc
;
(defvar clmemo-schedule-string "[s]"
  "*Header string for schedule.")


;;
;; System Variables and Functions
;;

(defvar clmemo-date-regexp "^\\<.")
(defvar clmemo-heading-regexp "^\t\\* ")
(defvar clmemo-inline-date "[12][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]")
(defvar clmemo-inline-date-and-num "[12][0-9][0-9][0-9]-[01][0-9]-[0-3][0-9]-\\([0-9]+\\)")
(defvar clmemo-inline-date-format "[%s]")
(defvar clmemo-tag-format '("(%s: " . ")"))
(defvar clmemo-winconf nil)

;
; misc functions
;

;; function mapc is a new function from Emacs 21.1.
(defsubst clmemo-mapc (function sequence)
  (if (fboundp 'mapc)
      (mapc function sequence)
    (mapcar function sequence)))


;;
;; font-lock
;;
(defface clmemo-inline-date-face
  '((((class color) (background light))
     (:foreground "slateblue"))
    (((class color) (background dark))
     (:foreground "yellow"))
    (t
     (:bold t)))
  "Face for highlighting date.")

(defvar clmemo-inline-date-face 'clmemo-inline-date-face)

(defvar clmemo-font-lock-keywords
  '(;;
    ;; Date lines, with weekday
    ("^\\sw.........[0-9:+ ]*\\((...)\\)?"
     (0 'change-log-date-face)
     ("\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.-]+@[A-Za-z0-9_.-]+\\)[>)]" nil nil
      (1 'change-log-name-face)
      (2 'change-log-email-face)))
    ;;
    ;; Date
    ("\\[[0-9-]+\\]" (0 'clmemo-inline-date-face)))
  "Additional expressions to highlight in ChangeLog Memo mode.")

(setq change-log-font-lock-keywords
      (append clmemo-font-lock-keywords change-log-font-lock-keywords))


;;
;; clmemo
;;

;;;###autoload
(defun clmemo (arg)
  "Open ChangeLog memo file `clmemo-file-name' and ask title.

With prefix argument ARG, just open ChangeLog memo file.
 If already visited the ChangeLog memo file,
 ask title and insert it in the date at point.
With prefix argument more than once, call `clmemo-grep-function'.

See also `add-change-log-entry' and `clmemo-get-title'."
  (interactive "P")
  (cond
   ((equal arg '(64)) (clmemo-grep t))	       ;C-u C-u C-u
   ((equal arg '(16)) (clmemo-grep nil))       ;C-u C-u
   ((equal arg '(4))  (clmemo-one-prefix-arg)) ;C-u
   (t (clmemo-new-title-today))))

(defun clmemo-one-prefix-arg ()
  "Function callend from C-u `clmemo'."
  (let ((file (expand-file-name clmemo-file-name)))
    (if (equal (buffer-file-name) file)
	(clmemo-new-title t)
      (setq clmemo-winconf (current-window-configuration))
      (switch-to-buffer (or (get-file-buffer file)
			    (find-file-noselect file)))
      (clmemo-mode))))

(defun clmemo-new-title-today ()
  "Ask title and insert it.
Function called from `clmemo'."
  (setq clmemo-winconf (current-window-configuration))
  (clmemo-new-title)
  (clmemo-mode))

(defun clmemo-new-title (&optional not-today)
  "Ask title and insert it.
If optional argument NOT-TODAY is non-nil, insert title the date at point."
  (let ((title (clmemo-get-title))
	(buf (current-buffer))
	(add-log-always-start-new-record nil)
	(add-log-time-format (if clmemo-time-string-with-weekday
				 'add-log-iso8601-time-string-with-weekday
			       'add-log-iso8601-time-string)))
    (if not-today
	(progn
	  (forward-line 1)
	  (clmemo-backward-entry)
	  (end-of-line)
	  (insert "\n\n\t* "))
      (add-change-log-entry nil clmemo-file-name t)
      (beginning-of-line)
      (when (looking-at "^\t\\* .+: ")
	(replace-match "\t* "))
      (end-of-line))
    ;; Insert item-heading separater after title.
    (unless (string= "" title)
      (insert title ": "))
    (clmemo-mapc (lambda (func) (funcall func buf)) clmemo-buffer-function-list)
    (run-hooks 'clmemo-new-title-hook)))

(defun clmemo-get-title ()
  "Ask title of ChangeLog MEMO and return it.
Ask the subtitle if `clmemo-subtitle-char' is at the end of title."
  (let ((title (clmemo-completing-read "clmemo title: ")))
    (when (clmemo-subtitle-p title)
      (setq title (clmemo-split-title title))
      (let* ((sub   (clmemo-completing-read (format "subtitle for `%s': " title)))
	     (left  (car clmemo-subtitle-punctuation-char))
	     (right (cdr clmemo-subtitle-punctuation-char)))
	;; Recursive subtitle
	(while (clmemo-subtitle-p sub)
	  (setq title (concat title left (clmemo-split-title sub) right)
		sub (clmemo-completing-read (format "subtitle for `%s': " title))))
	(unless (equal sub "")
	  (setq title (concat title left sub right)))))
    title))

(defun clmemo-completing-read (prompt)
  "Read a string in the minibuffer, with completion using `clmemo-title-list'.
PROMPT is a string to prompt with; normally it ends in a colon and space."
  (let* ((completion-ignore-case t)
	 (alist (mapcar (lambda (x) (if (consp x) x (cons x x)))
			clmemo-title-list))
	 (title (completing-read prompt alist))
	 (subp (clmemo-subtitle-p title)))
    ;; Get title
    (when subp
      (setq title (clmemo-split-title title)))
    (setq title (or (cdr (assoc title alist)) title))
    ;; Format title.
    (when clmemo-title-format-function
      (setq title (funcall clmemo-title-format-function title)))
    ;; Add subtitle suffix if needed.
    (if subp
	(concat title clmemo-subtitle-char)
      title)))

(defun clmemo-subtitle-p (title)
  "Return t if argument TITLE has subtitle suffix.
Subtitle suffix is defined in variable `clmemo-subtitle-char'."
  (and clmemo-subtitle-char
       (not (string= title ""))
       (string= clmemo-subtitle-char (clmemo-split-title title t))))

(defun clmemo-split-title (title &optional tail)
  "Return the substring of TITLE.

A substring is the title which the the length of
`clmemo-split-title' is deleted from tail of the title.
If optional argument TAIL is non-nil, return the deleted one."
  (if tail
      (substring title (- (length clmemo-subtitle-char)))
    (substring title 0 (- (length clmemo-subtitle-char)))))

;
; Buffer Function
;

(defun clmemo-insert-region (buf)
  "Insert the text between region to clmemo if the region is available."
  (when (and (bufferp buf)
	     (save-excursion (set-buffer buf) (clmemo-region-exists-p)))
    (let ((text (save-excursion (set-buffer buf)
				(buffer-substring-no-properties
				 (region-beginning) (region-end))))
	  beg end)
      (save-excursion
	(insert "\n")
	(setq beg (point)
	      end (progn (insert text) (point)))
	(clmemo-indent-region beg end)))))

(defun clmemo-tag-insert-url-from-w3m (buf)
  "Insert w3m's title and url to clmemo if the buffer BUF is under emacs-w3m."
  (when (and (bufferp buf) (featurep 'w3m)
	     (eq 'w3m-mode (save-excursion
				 (set-buffer buf)
				 (symbol-value 'major-mode))))
    (let (url title exist-p tag (c ?i))
      (save-excursion
	(set-buffer buf)
	(setq url (if (boundp 'w3m-current-url) w3m-current-url)
	      title (if (boundp 'w3m-current-title) w3m-current-title)
	      tag (concat (format (car clmemo-tag-format) "url") url ")")))
      (save-excursion
	(goto-char (point-min))
	(setq exist-p
	      (when (search-forward tag nil t)
		(clmemo-get-date))))
      (when exist-p
	(setq c (read-char
		 (format "This url is already exists on %s-%s-%s: (G)o  (I)nsert (Q)uit"
			 (nth 0 exist-p) (nth 1 exist-p) (nth 2 exist-p)))))
      (cond
       ((equal c ?i)			;ignore
	;; Insert title
	(when (and title (y-or-n-p (format "Insert `%s' as title? " title)))
	  (insert title))
	;; Insert url
	   (save-excursion
	     (when (and url (y-or-n-p "Insert URL? "))
	       (insert "\n\t")
	       (clmemo-tag-insert-url url))))
       ((equal c ?g)			;go to the item which contains url-tag
	(search-forward tag nil t)
	(clmemo-previous-item))
       (t ;quit
	   )
       ))))



;;
;; ChangeLog MEMO Mode
;;
(defvar clmemo-mode nil
  "Toggle clmemo-mode.")
(make-variable-buffer-local 'clmemo-mode)

;; Variable minor-mode-list comes after Emacs-21.4.
(when (boundp 'minor-mode-list)
  (unless (memq 'clmemo-mode minor-mode-list)
    (setq minor-mode-list (cons 'clmemo-mode minor-mode-list)))
)

(unless (assq 'clmemo-mode minor-mode-alist)
  (setq minor-mode-alist
	(cons '(clmemo-mode " MEMO") minor-mode-alist)))

(defvar clmemo-mode-hook nil
  "*Hook run at the end of function `clmemo-mode'.")

(defun clmemo-mode (&optional arg)
  "Minor mode for editing ChangeLog MEMO.
For detail, See function `clmemo'.

\\{clmemo-mode-map}
"
  (interactive "P")
  (if (< (prefix-numeric-value arg) 0)
      (setq clmemo-mode nil)
    (setq clmemo-mode t)
    (unless (local-variable-p 'paragraph-start)
      (make-local-variable 'paragraph-start))
    (setq paragraph-start (concat clmemo-paragraph-start "\\|" paragraph-start))
    (unless (local-variable-p 'paragraph-separate)
      (make-local-variable 'paragraph-separate))
    (setq paragraph-separate (concat clmemo-paragraph-separate "\\|" paragraph-separate))
    (run-hooks 'clmemo-mode-hook)))

(defvar clmemo-mode-map nil)
(if clmemo-mode-map
    nil
  (let ((map (make-keymap)))
    ;; Movement & mark
    (define-key map "\C-c\C-n" 'clmemo-next-item)
    (define-key map "\C-c\C-p" 'clmemo-previous-item)
    (define-key map "\C-c\C-f" 'clmemo-forward-entry)
    (define-key map "\C-c\C-b" 'clmemo-backward-entry)
    (define-key map "\C-c}" 'clmemo-previous-month)
    (define-key map "\C-c{" 'clmemo-next-month)
    (define-key map "\C-c\C-h" 'clmemo-mark-month)
    (substitute-key-definition 'forward-page 'clmemo-previous-year map global-map)
    (substitute-key-definition 'backward-page 'clmemo-next-year map global-map)
    (substitute-key-definition 'mark-page 'clmemo-mark-year map global-map)
    ;; yank & indent
    (define-key map "\C-c\C-w" 'clmemo-kill-ring-save)
    (define-key map "\C-c\C-y" 'clmemo-yank)
    (define-key map "\C-c\C-i" 'clmemo-indent-region)
    (define-key map "\C-c>" 'clmemo-quote-region)
    ;; Date
    (define-key map "\C-c\C-d" 'clmemo-inline-date-insert)
    (define-key map "\C-i" 'clmemo-next-inline-date)
    (define-key map [(shift tab)] 'clmemo-previous-inline-date)
    (define-key map [backtab] 'clmemo-previous-inline-date)
    ;; Tag
    (define-key map "\C-c(" 'clmemo-tag-insert-quick)
    (define-key map "\C-c\C-t" 'clmemo-forward-tag)
    (define-key map "\C-c;" 'clmemo-forward-tag)
    (define-key map "\C-c:" 'clmemo-backward-tag)
    ;; Schedule
    (define-key map "\C-c\C-c" 'clmemo-schedule)
    ;; Exit
    (define-key map "\C-c\C-q" 'clmemo-exit)
    (define-key map "\C-c\C-m" 'clmemo-jump)
    (setq clmemo-mode-map map)))

(unless (assq 'clmemo-mode minor-mode-map-alist)
  (setq minor-mode-map-alist
	(cons (cons 'clmemo-mode clmemo-mode-map) minor-mode-map-alist)))

(defun clmemo-next-item (&optional arg)
  "Move to the next item.
With argument, repeats or can move backward if negative."
  (interactive "p")
  (re-search-forward clmemo-heading-regexp nil t arg)
  (beginning-of-line)
  (skip-chars-forward "\t"))

(defun clmemo-previous-item (&optional arg)
  "Move to the previous item.
With argument, repeats or can move forward if negative."
  (interactive "p")
  (re-search-backward clmemo-heading-regexp nil t arg)
  (skip-chars-forward "\t"))

(defun clmemo-forward-entry (&optional arg)
  "Move forward to the ARG'th entry."
  (interactive "p")
  (if (and arg (< arg 0))
      (clmemo-backward-entry (- arg))
    (beginning-of-line)
    (forward-char 1)
    (re-search-forward clmemo-date-regexp nil t arg)
    (beginning-of-line)))

(defun clmemo-backward-entry (&optional arg)
  "Move backward to the ARG'th entry."
  (interactive "p")
  (if (and arg (< arg 1))
      (clmemo-forward-entry (- arg))
    (beginning-of-line)
    (backward-char 1)
    (re-search-backward clmemo-date-regexp nil t arg)
    (beginning-of-line)))

;
; kill-ring Save & Yank
;
(defun clmemo-kill-ring-save (beg end)
  "Same as `kill-ring-save' but remove TAB at the beginning of line."
  (interactive "r")
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf beg end)
      (goto-char (point-min))
      (while (re-search-forward "^\t" nil t)
        (replace-match ""))
      (kill-ring-save (point-min) (point-max)))))

(defun clmemo-yank (&optional arg)
  "Yank and indent with one TAB.

Not support `yank-pop'.
Use function `clmemo-indent-region' after `yank' and `yank-pop'."
  (interactive "P*")
  (if (fboundp 'looking-back) ; looking-back is defined on 2003-05-31.
      (and (looking-back "^\t[ \t]*") (replace-match ""))
    (save-excursion
      (skip-chars-backward " \t")
      (when (and (bolp) (looking-at "\t[ \t]*"))
	(replace-match ""))))
  (let ((beg (point))
        (end (progn (yank) (point))))
    (clmemo-indent-region beg end)))

(defun clmemo-indent-region (beg end)
  "Indent region by one TAB."
  (interactive "r*")
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (while (>= (point) beg)
      (insert "\t")
      (forward-line -1))))

;
; Quote
;
(defun clmemo-quote-region (beg end &optional qstr)
  "Add quote string before every line in the region.
You can customize the quote string by the variable `clmemo-quote-prefix'.

If called with prefix arg, ask quote string."
  (interactive "r*\nP")
  (setq qstr (if qstr
		 (read-string (format "Quote string (%s): " clmemo-quote-prefix)
			      nil nil clmemo-quote-prefix)
	       clmemo-quote-prefix))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "^\t" nil t)
	(replace-match (concat "\t" qstr " "))))))

(defun clmemo-region-exists-p ()
  "Return t if mark is active."
  (cond
   ((boundp 'mark-active) mark-active)		  ;For Emacs
   ((fboundp 'region-exists-p) (region-exists-p)) ;For XEmacs
   (t (error "No funcntion for checking region"))))

;
; Schedule
;

(defun clmemo-schedule ()
  "Insert schedule flags and puts date string.
See variable `clmemo-schedule-string' for header flag string."
  (interactive)
  (end-of-line)
  (clmemo-previous-item)
  (forward-char 2)
  (insert clmemo-schedule-string)
  (search-forward ": ")
  (insert "[] ")
  (backward-char 2))

;
; Exit
;

(defun clmemo-exit ()
  "Turn back where enter the ChangeLog memo."
  (interactive)
  (basic-save-buffer)
  (set-window-configuration clmemo-winconf))

(defun clmemo-jump ()
  "Jump command for clmemo.
Change behaviour depending on the text at point."
  (interactive)
  (cond ((clmemo-inline-date-p) (clmemo-goto-date))
	((clmemo-tag-p) (clmemo-tag-func))
	((thing-at-point 'url) (browse-url-at-point))
	(t nil)))

(defun clmemo-tag-func ()
  "Function called at tag.
Change behaviour depending on the tag at point."
  (skip-chars-backward "^(")
  (when (looking-at "\\(.+\\): ")
    (let* ((tag (match-string 1))
	   (cc (assoc tag clmemo-tag-list)))
      (search-forward ": " nil t)
      (when cc
	(funcall (nth 1 cc))))))


;;
;; Date
;;
(defun clmemo-get-date ()
  "Return the list of (YEAR MONTH DAY WEEK) of the entry at point."
  (save-excursion
    (forward-line 1)
    (clmemo-backward-entry)
    (let ((end (save-excursion (end-of-line) (point)))
	  year month day week)
      (and (re-search-forward "^[0-9]+" end t)
	   (setq year (match-string 0)))
      (skip-chars-forward "-" end)
      (and (re-search-forward "[0-9]+" end t)
	   (setq month (match-string 0)))
      (skip-chars-forward "-" end)
      (and (re-search-forward "[0-9]+" end t)
	   (setq day (match-string 0)))
      (skip-chars-forward " " end)
      (and (looking-at "(\\(...\\))")
	   (setq week (match-string 1)))
      (list year month day week))))

(defun clmemo-forward-year (&optional arg)
  "Move forward year.
With argument, repeats or can move backward if negative."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (clmemo-backward-year (- arg))
    (when (interactive-p) (push-mark))
    (let ((year (string-to-number (nth 0 (clmemo-get-date))))
	  (pos (point)))
      (setq year (+ year arg))
      (goto-char (point-min))
      (if (re-search-forward (format "^%d-" year) nil t)
	  (beginning-of-line)
	(goto-char pos)
	(when (interactive-p) (pop-mark))))))

(defun clmemo-backward-year (&optional arg)
  "Move backward year.
With argument, repeats or can move forward if negative."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 1)
      (clmemo-forward-year (- arg))
    (when (interactive-p) (push-mark))
    (let ((year (string-to-number (nth 0 (clmemo-get-date)))))
      (setq year (- year arg))
      (if (re-search-forward (format "^%d-" year) nil t)
	  (beginning-of-line)
	(when (interactive-p) (pop-mark))))))

(defun clmemo-next-year (&optional arg)
  "Move to the date after ARG's year.
With argument, repeats or can move backward if negative."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (clmemo-backward-year (- arg))
    (when (interactive-p) (push-mark))
    (let ((date (clmemo-get-date))
	  (pos (point))
	  year month day regexp)
      (setq year (+ arg (string-to-number (nth 0 date)))
	    month (nth 1 date)
	    day (nth 2 date))
      (if (= arg 0)
	  (setq regexp (format "^%d-" year))
	(setq regexp (format "^%d-%s-%s" year month day)))
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
	  (beginning-of-line)
	(goto-char pos)
	(when (interactive-p) (pop-mark))))))

(defun clmemo-previous-year (&optional arg)
  "Move to the date before ARG's year.
With argument, repeats or can move forward if negative."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 1)
      (clmemo-forward-year (- arg))
    (when (interactive-p) (push-mark))
    (let ((date (clmemo-get-date))
	  year month day)
      (setq year (- (string-to-number (nth 0 date)) arg)
	    month (nth 1 date)
	    day (nth 2 date))
      (if (re-search-forward (format "^%d-%s-%s" year month day) nil t)
	  (beginning-of-line)
	(when (interactive-p) (pop-mark))))))

(defun clmemo-forward-month (&optional arg)
  "Move forward month.
With argument, repeats or can move backward if negative."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (clmemo-backward-month (- arg))
    (when (interactive-p) (push-mark))
    (let ((date (clmemo-get-date))
	  (pos (point))
	  year month)
      (setq year (string-to-number (nth 0 date))
	    month (string-to-number (nth 1 date)))
      (setq month (+ month arg))
      (when (> month 12)
	(setq year (+ year (/ month 12))
	      month (% month 12)))
      (goto-char (point-min))
      (if (re-search-forward (format "^%d-%02d-" year month) nil t)
	  (beginning-of-line)
	(goto-char pos)
	(when (interactive-p) (pop-mark))))))

(defun clmemo-backward-month (&optional arg)
  "Move backward month.
With argument, repeats or can move forward if negative."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 1)
      (clmemo-forward-month (- arg))
    (when (interactive-p) (push-mark))
    (let ((date (clmemo-get-date))
	  year month)
      (setq year (string-to-number (nth 0 date))
	    month (string-to-number (nth 1 date)))
      (if (> month arg)
	  (setq month (- month arg))
	(setq arg (- arg month))
	(setq year (- year (1+ (/ arg 12)))
	      month (- 12 (% arg 12))))
      (if (re-search-forward (format "^%d-%02d-" year month) nil t)
	  (beginning-of-line)
	(when (interactive-p) (pop-mark))))))

(defun clmemo-next-month (&optional arg)
  "Move to the date after ARG's month.
With argument, repeats or can move backward if negative."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (clmemo-backward-month (- arg))
    (when (interactive-p) (push-mark))
    (let ((date (clmemo-get-date))
	  (pos (point))
	  year month day regexp)
      (setq year (string-to-number (nth 0 date))
	    month (string-to-number (nth 1 date))
	    day (nth 2 date))
      (setq month (+ month arg))
      (when (> month 12)
	(setq year (+ year (/ month 12))
	      month (% month 12)))
      (if (= arg 0)
	  (setq regexp (format "^%d-%02d-" year month))
	(setq regexp (format "^%d-%02d-%s" year month day)))
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
	  (beginning-of-line)
	(goto-char pos)
	(when (interactive-p) (pop-mark))))))

(defun clmemo-previous-month (&optional arg)
  "Move to the date before ARG's month.
With argument, repeats or can move forward if negative."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 1)
      (clmemo-forward-month (- arg))
    (when (interactive-p) (push-mark))
    (let ((date (clmemo-get-date))
	  year month day)
      (setq year (string-to-number (nth 0 date))
	    month (string-to-number (nth 1 date))
	    day (nth 2 date))
      (if (> month arg)
	  (setq month (- month arg))
	(setq arg (- arg month))
	(setq year (- year (1+ (/ arg 12)))
	      month (- 12 (% arg 12))))
      (if (re-search-forward (format "^%d-%02d-%s" year month day) nil t)
	  (beginning-of-line)
	(when (interactive-p) (pop-mark))))))

(defun clmemo-mark-year (&optional arg)
  "Put point at end of this year, mark at beginning.
With argument ARG, puts mark at end of a following year, so that
the number of years marked equals ARG."
  (interactive "p")
  (or arg (setq arg 1))
  (clmemo-forward-year 0)
  (push-mark)
  (clmemo-backward-year arg)
  (exchange-point-and-mark))

(defun clmemo-mark-month (&optional arg)
  "Put point at end of this month, mark at beginning.
With argument ARG, puts mark at end of a following month, so that
the number of months marked equals ARG."
  (interactive "p")
  (or arg (setq arg 1))
  (clmemo-forward-month 0)
  (push-mark)
  (clmemo-backward-month arg)
  (exchange-point-and-mark))


;;
;; Inline Date
;;
;; Function define-minor-mode comes after Emacs 21.
;; Reported by TSURUDA Naoki [2002-12-21].
(defvar clmemo-inline-date-mode nil
  "Non-nil if Clmemo-Inline-Date mode is enabled.
Use the command `clmemo-inline-date-mode' to change this variable.")
(make-variable-buffer-local 'clmemo-inline-date-mode)

;; Variable minor-mode-list comes after from Emacs-21.4.
(when (boundp 'minor-mode-list)
  (unless (memq 'clmemo-inline-date-mode minor-mode-list)
    (setq minor-mode-list (cons 'clmemo-inline-date-mode minor-mode-list)))
)

(unless (assq 'clmemo-inline-date-mode minor-mode-alist)
  (setq minor-mode-alist (cons '(clmemo-inline-date-mode " Date") minor-mode-alist)))


(defvar clmemo-inline-date-mode-hook nil
  "*Hook run at the end of function `clmemo-inline-date-mode'.")

(defvar clmemo-inline-date-pos nil)


(defalias 'clmemo-inline-date-insert 'clmemo-inline-date-mode)
(defun clmemo-inline-date-mode (&optional arg)
  "Minor mode for looking for the date to insert.

\\{clmemo-inline-date-mode-map}"
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq clmemo-inline-date-mode
	(cond ((eq arg (quote toggle)) (not clmemo-inline-date-mode))
	      (arg (> (prefix-numeric-value arg) 0))
	      (t (if (null clmemo-inline-date-mode)
		     t
		   (message "Toggling %s off; better pass an explicit argument."
			    (quote clmemo-inline-date-mode)) nil))))
  (when (and clmemo-inline-date-mode buffer-read-only)
    (setq clmemo-inline-date-mode nil)
    (error "Buffer is read-only: %S" (current-buffer)))
  (if clmemo-inline-date-mode
      (setq buffer-read-only t
	    clmemo-inline-date-pos (cons (point) (current-window-configuration)))
    (setq buffer-read-only nil
	  clmemo-inline-date-pos nil))
  (run-hooks 'clmemo-inline-date-mode-hook)
  (force-mode-line-update)
  clmemo-inline-date-mode)

;
; keymap
;
(defvar clmemo-inline-date-mode-map nil)
(if clmemo-inline-date-mode-map
    nil
  (let ((map (make-keymap)))
    (suppress-keymap map)
    ;; Date
    (define-key map "q"  'clmemo-inline-date-quit)
    (define-key map "\C-m" 'clmemo-inline-date-insert-today)
    ;; Move
    (define-key map "n"  'clmemo-next-item)
    (define-key map "p"  'clmemo-previous-item)
    (define-key map "f"  'clmemo-forward-entry)
    (define-key map "b"  'clmemo-backward-entry)
    (define-key map "}"  'clmemo-backward-month)
    (define-key map "{"  'clmemo-forward-month)
    (define-key map "]"  'clmemo-backward-year)
    (define-key map "["  'clmemo-forward-year)
    (define-key map "s"  'isearch-forward)
    (define-key map "r"  'isearch-backward)
    (define-key map " "  'scroll-up)
    (define-key map [backspace] 'scroll-down)
    (setq clmemo-inline-date-mode-map map)))

(unless (assq 'clmemo-inline-date-mode minor-mode-map-alist)
  (setq minor-mode-map-alist
	(cons (cons 'clmemo-inline-date-mode clmemo-inline-date-mode-map)
	      minor-mode-map-alist)))


(defun clmemo-inline-date-insert-today (&optional arg)
  "Insert the date where point is."
  (interactive "P")
  ;; Get error when called from not clmemo-inline-date-mode.
  (unless clmemo-inline-date-mode
    (error "Call this function from function clmemo-insert-date"))
  (end-of-line)
  (unless (eobp) (forward-line 1))
  (let ((num 0)
	(pos (point)))
    ;; Get item number if ARG is t.
    (when arg
      (save-excursion
	(clmemo-forward-entry)
	(while (< pos (point))
	  (setq num (1+ num))
	  (clmemo-previous-item))))
    ;; Get date and insert it.
    (clmemo-backward-entry)
    (if (looking-at clmemo-inline-date)
	(progn (clmemo-inline-date-quit)
	       (setq buffer-undo-list (cons (point) buffer-undo-list))
	       (insert (format clmemo-inline-date-format 
			       (if arg
				   (concat (match-string 0) "-" (number-to-string num))
				 (match-string 0)))))
      (clmemo-inline-date-quit)
      (error "Can't search ChangeLog title"))))

(defun clmemo-inline-date-quit ()
  "Quit clmemo-inline-date-mode."
  (interactive)
  (let ((pos clmemo-inline-date-pos))
    (when (eq major-mode 'clgrep-mode)
      (clmemo-inline-date-mode -1)
      (set-window-configuration (cdr pos)))
    (goto-char (car pos))
    (clmemo-inline-date-mode -1)))

(defun clmemo-inline-date-p ()
  "Return t if point is in the inline date."
  (save-excursion
    (skip-chars-backward "-0-9")
    (skip-chars-forward "[")
    (and (not (looking-at clmemo-date-regexp))
	 (equal (char-before) ?\[)
	 (looking-at clmemo-inline-date))))

(defun clmemo-goto-date (&optional date num)
  "Move point where the date at point."
  (interactive nil)
  (let ((pos (point)))
    ;; Get inline date
    (unless (or date (clmemo-inline-date-p)) (error "No date here"))
    (skip-chars-backward "-0-9")
    (unless date
      (setq date (progn (looking-at clmemo-inline-date)
			(match-string 0))))
    (unless num
      (setq num  (progn (looking-at clmemo-inline-date-and-num)
		      (match-string 1))))
    ;; Goto the date
    (push-mark)
    (goto-char (point-min))
    (if (re-search-forward (concat "^" date) nil t)
	(beginning-of-line)
      (goto-char pos)
      (error "No date: %s" date)))
  (when num
    (setq num (string-to-number num))
    (clmemo-forward-entry)
    (while (> num 0)
      (clmemo-previous-item)
      (unless (looking-at "\\* p:")	;skip private item.
	(setq num (1- num)))))
  (recenter 0))

(defun clmemo-next-inline-date (&optional arg)
  "Search forward ARG'th date from point."
  (interactive "p")
  (let* ((fmt (regexp-quote clmemo-inline-date-format))
	 (regexp (format (concat fmt "\\|" fmt)
			 clmemo-inline-date
			 clmemo-inline-date-and-num)))
    (when (re-search-forward regexp nil t arg)
      (if (< arg 0)
	  (skip-chars-forward "^0-9")
	(skip-chars-backward "^0-9")
	(skip-chars-backward "-0-9")))))

(defun clmemo-previous-inline-date (&optional arg)
  "Search backward ARG'th date from point."
  (interactive "p")
  (clmemo-next-inline-date (- arg)))


;;
;; Tag
;;
(defun clmemo-tag-p ()
  "Return t if point is in the tag."
  (let ((lim (save-excursion (beginning-of-line) (point))))
    (unless (eobp)
      (save-excursion
	(forward-char 1)
	(skip-chars-backward "^(" lim)
	(and (equal (char-before) ?\()
	     (looking-at ".+: .+)"))))))

(defun clmemo-tag-insert-quick (tag)
  "Insert tag quickly.
See also function `clmemo-tag-completing-read'."
  (interactive (list (clmemo-tag-completing-read t)))
  (clmemo-tag-insert tag))

(defun clmemo-tag-insert (tag)
  "Insert tag.
See also function `clmemo-tag-completing-read'."
  (interactive (list (clmemo-tag-completing-read)))
  (let ((cc (assoc tag clmemo-tag-list))
	(fmt-left (car clmemo-tag-format))
	(fmt-right (cdr clmemo-tag-format)))
    (if (> (length cc) 2)
	(setq cc (cons (car cc) (nth 2 cc)))
      (setq cc nil))
    (insert (format fmt-left tag))
    (save-excursion
      (insert fmt-right))
    (if cc
	(insert (funcall (cdr cc))))))

(defun clmemo-tag-completing-read (&optional quick)
  "Return tag name.
The variable `clmemo-tag-list' is used for completion of tag name.

If optional argument QUICK is non-nil, clmemo-tag-completing-read
choose the tag name by the initial letter of tag name.  When
initial letters are overlapped, the first tag name in the
`clmemo-tag-list' will be chosen."
  (let (tag)
    (save-window-excursion
      (while (not tag)
	(if quick
	    (let ((char (char-to-string (read-char "tag: "))))
	      (cond
	       ((equal char " ") (setq quick nil))
	       ((equal char "\t") (clmemo-tag-show))
	       (t (setq tag (all-completions char clmemo-tag-list)))))
	  (setq tag (completing-read "tag: " clmemo-tag-list)))))
    (if (listp tag)
	(car tag)
      tag)))

(defun clmemo-tag-show ()
  "Show the tag name for completion."
  (let ((buf (get-buffer-create " *clmemo-tag*")))
    (switch-to-buffer-other-window buf t)
    (erase-buffer)
    (goto-char (point-min))
    (insert "==* clkwd tag *==\n\n")
    (let (init-letter l)
      (insert (mapconcat (lambda (tag)
			   (unless (stringp tag)
			     (setq tag (car tag)))
			   (setq l (substring tag 0 1))
			   (unless (member l init-letter)
			     (setq init-letter (cons l init-letter))
			     (format "  %s ... %s\n" l tag)))
			 clmemo-tag-list "")))
    (insert "\nC-g for quit.\n")))


(defun clmemo-next-tag (tag &optional arg)
  "Move next tag TAG.
With argument, repeats or can move backward if negative."
  (interactive (list (clmemo-tag-completing-read)
		     (prefix-numeric-value current-prefix-arg)))
  (if (and arg (< arg 0))
      (clmemo-previous-tag (- arg))
    (and (re-search-forward (format "(%s: " tag)  nil t arg)
	 (clmemo-beginning-of-tag-string))))

(defun clmemo-previous-tag (tag &optional arg)
  "Move previous tag TAG.
With argument, repeats or can move forward if negative."
  (interactive (list (clmemo-tag-completing-read)
		     (prefix-numeric-value current-prefix-arg)))
  (if (and arg (< arg 1))
      (clmemo-next-tag (- arg))
    (beginning-of-line)
    (and (re-search-backward (format "(%s: " tag) nil t arg)
	 (clmemo-beginning-of-tag-string))))

(defun clmemo-forward-tag (&optional arg)
  "Move forward to the ARG'th tag.
With argument, repeats or can move backward if negative."
  (interactive "p")
  (if (and arg (< arg 0))
      (clmemo-backward-tag (- arg))
    (and (re-search-forward "^\t(.+: " nil t arg)
	 (clmemo-beginning-of-tag-string))))

(defun clmemo-backward-tag (&optional arg)
  "Move backward to the ARG'th tag.
With argument, repeats or can move forward if negative."
  (interactive "p")
  (if (and arg (< arg 0))
      (clmemo-forward-tag (- arg))
    (beginning-of-line)
    (and (re-search-backward "^\t(.+: " nil t arg)
	 (clmemo-beginning-of-tag-string))))

(defun clmemo-beginning-of-tag-string ()
  "Move point to the beginning of tag, skipping inline date."
  (beginning-of-line)
  (when (re-search-forward "^\t([^:]+:" nil t)
    (skip-chars-forward " ")
    (when (clmemo-inline-date-p)
      (skip-chars-forward "^\]")
      (forward-char 1)
      (skip-chars-forward " "))))
;
; url
;
(defun clmemo-tag-insert-url (url)
  "Insert url URL as tag."
  (clmemo-tag-insert "url")
  (insert url)
  (end-of-line))

(defun clmemo-next-tag-url (&optional arg)
  (interactive "p")
  (clmemo-next-tag clmemo-tag-url arg))

(defun clmemo-previous-tag-url (&optional arg)
  (interactive "p")
  (clmemo-previous-tag clmemo-tag-url arg))

;
; File
;
(defun clmemo-read-file-name ()
  "Read file name."
  (read-file-name "File: "))


;;
;; Header with/out weekday
;;
(defun add-log-iso8601-time-string-with-weekday ()
  ;; Code contributed from Satoru Takabayashi <satoru@namazu.org>
  (let ((system-time-locale "C"))
    (concat (add-log-iso8601-time-string)
            " (" (format-time-string "%a") ")")))

(defun clmemo-format-header-with-weekday (beg end)
  "Format ChangeLog header with weekday
FROM: 2001-01-01  ataka
TO:   2001-01-01 (Mon)  ataka

See also function `clmemo-format-header-without-weekday'."
  (interactive "*r")
  (let* ((system-time-locale "C")
	 date weekday)
    (save-excursion
      (goto-char end)
      (while (re-search-backward "^\\([-0-9]+\\)" beg t)
	(save-match-data
	  (setq date    (match-string 0)
		weekday (format-time-string "%a" (date-to-time (concat date " 0:0:0")))))
	(replace-match (concat date " (" weekday ")"))
	(beginning-of-line)))))

(defun clmemo-format-header-without-weekday (beg end)
  "Format ChangeLog header without weekday
FROM:   2001-01-01 (Mon)  ataka
TO:     2001-01-01  ataka

See also function `clmemo-format-header-with-weekday'."
  (interactive "*r")
  (save-excursion
    (goto-char end)
    (while (re-search-backward "^\\([-0-9]+\\) (.+)" beg t)
      (replace-match "\\1")
      (beginning-of-line))))


;;
;; clgrep mode
;;
(defun clmemo-grep (arg)
  "Switch to ChangeLog Memo and grep it immediately."
  ;; Ask query before switching buffer and set QUERY non-nil if the
  ;; value of variable clmemo-grep-function is 'clgrep.
  (let ((query (and (fboundp clmemo-grep-function)
		    (eq clmemo-grep-function 'clgrep)
		    (read-string "Query: "))))
    (switch-to-buffer (find-file-noselect clmemo-file-name))
    (clmemo-mode)
    (cond
     ;; clmemo-grep-function is clgrep
     (query (funcall clmemo-grep-function query arg))
     ;; clmemo-grep-function is available
     ((fboundp clmemo-grep-function)
      (let ((current-prefix-arg arg))
	(call-interactively clmemo-grep-function)))
     ;; Otherwise
     (t
      (let ((current-prefix-arg nil))
	(call-interactively 'occur))))))

;;; clmemo.el ends here

;; Local Variables:
;; fill-column: 72
;; End:
