;;; hatenahelper-mode.el --- �ϤƤʥإ�ѡ��⡼��

;; Copyright (C) 2006  ITSUMI ken-ichi

;; Author: ITSUMI ken-ichi <itsumi@gmail.com>
;; Keywords: blog �֥� �ϤƤʥ������꡼���� �ϤƤʵ�ˡ �ϤƤ�

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA

;;; Commentary:

;;; emacs ��ǤϤƤʵ�ˡ�����Ϥ�ٱ礹��ޥ��ʡ��⡼�ɤǤ���
;;; 
;;; ��Ȥ�Ȥ� hatena-mode(http://d.hatena.ne.jp/hikigaeru/20040617)��
;;; �ȤäƤϤƤʥ������꡼������񤯻��ˡ��ϤƤʵ�ˡ�����Ϥλٱ�ΰ٤� 
;;; .emacs ����ɲä��Ƥ��ä��������ޥ����������ν��ѤʤΤǤ�������
;;; �θ�ϤƤʥ��롼�������䡢�������ʤɡ��ϤƤʥ������꡼�����ʳ��Ǥ�
;;; �ϤƤʵ�ˡ��Ȥ����Ȥ�¿���ʤäƤ����Τǡ��ޥ��ʡ��⡼�ɤȤ�����Ω
;;; �����Ƥߤ���ΤǤ���
;;; 
;;; ���Τ褦�ʾ����ǻȤ��뤳�Ȥ����ꤷ�Ƥ��ޤ���
;;; 1) hatena-mode.el �ǡ��ϤƤʥ������꡼�������Խ������
;;; 2) w3m.el �� html���ϥۡ����ǡ��ϤƤʥ�������¾���Ф��ƤϤƤʵ�
;;;    ˡ��Ԥʤ���
;;; 3) mozex ��Ȥäơ�firefox �� html���ϥե�����ؤ����Ϥ� emacs ��
;;;    �Ԥʤ����ϤƤʵ�ˡ��Ȥ���
;;; 4) hatena-mode �ʳ��Υ᥸�㡼�⡼�ɤǡ��ϤƤʵ�ˡ���Խ������
;;;    ��������˥ץ�����񤭹���褦�ʻ�����������
;;; 

;; �����󥹥ȡ�����ˡ
;; 1) Ŭ���ʥǥ��쥯�ȥ�ˤ��Υե�����򤪤�.
;;    (�ʲ���~/elisp/hatenahelper-mode/ �ˤ������Ȳ��ꤷ���ä�ʤ��). 
;;
;; 2) .emacs �˰ʲ��ιԤ��ɲä���
;;
;; (add-to-list 'load-path "~/elisp/hatenahelper-mode/")
;; (require 'hatenahelper-mode)
;; (global-set-key "\C-xH" 'hatenahelper-mode)
;; ;(add-hook 'hatena-mode-hook 'hatenahelper-mode)  ; �����Ϥ���
;; (add-hook 'hatena-mode-hook
;;	  '(lambda ()
;;           ; other hooks must be wrote here!
;;	     (hatenahelper-mode 1)))


;;
;; ��hatenahelper-mode ��ͭ���ˤ���
;; 
;; 1)hatena-mode �ξ��
;;   hatena-mode �ˤʤ�ȼ�ưŪ�� hatenahelper-mode ��ͭ���ˤʤ�ޤ�
;;
;; 2)����¾�ξ��
;;   C-x H �ޤ��� M-x hatenahelper-mode �����Ϥ��ޤ���
;;
;; ���Ȥ���
;; hatenahelper-mode �ˤʤ�ȡ���˥塼�С��α�ü������ Hatena �Ȥ���
;; ����ȥ꤬�и����ޤ��Τǡ���������򤷤Ƥ��С����Ȥ�����ʬ��Ȥ�
;; �⤤�ޤ���
;; ����Ū���ΰ��Ϥ൭ˡ�ˤĤ��Ƥϡ������ȥ꡼������Ϥ�褦����
;; �����������ȥ꡼����󤬶��ξ��䡢��ս����������褦�ʤ�Τ�
;; ����������֤���������褦��ư��ޤ���
;;
;; �ϤƤʵ�ˡ�Υ����������������
;; C-c q 	���ѵ�ˡ(>>��<<)
;; C-c C-q	��Ťΰ��ѵ�ˡ(>>>>��<<<<)
;; C-c C-t	pre��ˡ(>|��|<)
;; C-c C-s	�����ѡ�pre��ˡ(>||��||<)
;; C-c C-s	���ѵ�ˡ����˥����ѡ�pre��ˡ��ͥ���(>> >||��||< <<)
;; C-c d	���񤭵�ǽ(><!--��--><)
;; C-c k	�������([[��]])
;; 
;; �ϤƤʵ�ˡ��ʻ�Ѥ��뤳�Ȥ�¿�� html�����������������
;; C-c r	��������(<BR>)
;; C-c ^	���󥿥��(<CENTER>��</CENTER>
;; C-c +	�ե���ȳ���(<BIG>��</BIG>)
;; C-c +	�ե���ȳ���(<BIG>��</BIG>)
;; C-c -	�ե���Ƚ̾�(<SMALL>��</SMALL>)
;; C-c =	�����ä�(<S>��</S>)
;; C-c =	����(<U>��</U>)

;; ��hook
;;    easy-mmode ���������� hatenahelper-mode-hook �եå��ѿ��˴ؿ�̾
;;    �� add-hook ���Ƥ��С������� hatenahelper-mode �¹ԤκǸ��
;;    �¹Ԥ���ޤ�

;; ��ToDO
;; hatena-mode �Ȥ�ɤ� ����� debian �ѥå���������
;; w3m.el ����Ȥ��Ȥ��������ˤʤ�褦��ĥ����


;;; Code:
(defconst hatenahelper-mode-version 
  "0.1" "Version number of hatenahelper-mode.el")
;;;
;;; easy-mmode ��Ȥä��ޥ��ʡ��⡼�����
;;; 
(easy-mmode-define-minor-mode 
 hatenahelper-mode
 "Toggle HatenaHelper mode.
     With no argument, this command toggles the mode. 
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode."
 ;; �����
 nil
 ;; �⡼�ɹԤؤ�ɽ��
 ""
 ;; �ޥ��ʥ⡼�ɤΥХ���ǥ���
 ;; keymap �ϡ�hatenahelper-mode-map �Ȥ����ѿ��˳�Ǽ�����
 '(
   ("\C-cq" . hatenahelper-insert-blockquote)
   ("\C-c\C-q" . hatenahelper-insert-nested-blockquote)
   ("\C-c\C-t" . hatenahelper-insert-seikeizumi-textblock)
   ("\C-cs" . hatenahelper-insert-sonomama-textblock)
   ("\C-c\C-s" . hatenahelper-insert-nested-sonomama-textblock)

   ("\C-cp" . hatenahelper-insert-p-stop-textblock) ;; added
   
   ("\C-cd" . hatenahelper-insert-draft-tag)
   ("\C-ck" . hatenahelper-insert-kwd-tag)
   ("\C-c\C-k" . hatenahelper-insert-kwd-tag)
   
   ("\C-cl" . hatenahelper-insert-link-inhibit-tag)
   ("\C-cr" . hatenahelper-insert-br-tag)
   ("\C-c^" . hatenahelper-insert-center-tag)
   ("\C-c+" . hatenahelper-insert-font-enlarge-tag)
   ("\C-c-" . hatenahelper-insert-font-shrinkage-tag)
   ("\C-c=" . hatenahelper-insert-strike-tag)
   ("\C-c_" . hatenahelper-insert-underline-tag)
 
    ("\C-c\C-y" . clipboard-yank)	; ����åץܡ������Ƥ�����
   ("\C-c\y" . clipboard-yank)		;firefox �� copyURL �����ۤ�ĥ��Τ˻Ȥ�
   ))

;;;
;;; easy-menu ��Ȥä���˥塼���
;;;

(defvar hatenahelper-menu-map nil
  "Menu for hatena mode.")
(defvar hatenahelper-html-submenu-map nil
  "�ϤƤʵ�ˡ�ȸ򤨤Ƥ褯�Ȥ� html���������Ϥ��륵�֥�˥塼")
(defvar hatenahelper-special-tag-submenu-map nil
  "�ϤƤʵ�ˡ�Υ��������Ϥ��륵�֥�˥塼")

(defvar hatenahelper-menu-map-spec
  '("Hatena"
    ["Submit" hatena-submit (equal major-mode 'hatena-mode)]
    ["Previous Diary" hatena-find-previous (equal major-mode 'hatena-mode)]
    ["Next Diary" hatena-find-following (equal major-mode 'hatena-mode)]
;    ["Chottosita Henko" hatena-change-trivial [:style toggle] 
;     (equal major-mode 'hatena-mode)]
    ["Chottosita Henko" hatena-change-trivial (equal major-mode 'hatena-mode)]
    ["Delete Diary" hatena-delete-diary (equal major-mode 'hatena-mode)]
    "----"
    "Insert Tag"
    ("Insert Hatena Tag"
     "[With Newline]"
     ["Blockquote(>>)" hatenahelper-insert-blockquote t]
     ["Nested Blockquote(>> >>)" hatenahelper-insert-nested-blockquote t]
     ["Seikeizumi(>|)" hatenahelper-insert-seikeizumi-textblock t]
     ["Sonomama(>||)" hatenahelper-insert-sonomama-textblock t]
     ["Draft-tag(><--!)" hatenahelper-insert-draft-tag t]
     ["Nested Sonomama(>> >||)" hatenahelper-insert-nested-sonomama-textblock t]
     ["Keyword-tag([[)" hatenahelper-insert-kwd-tag t]
	 ["p-stop-tag(><p>)" hatenahelper-insert-p-stop-textblock t] ;; added
     ["Inhibit PTAG Auto Insertion(><blockquote> <p>)" 
      hatenahelper-insert-inhibit-p-tag-auto-insertion-block t]
     "----"
     "[Inline]"
     ["Unlink block([])" hatenahelper-insert-link-inhibit-tag t])

    ("Insert HTML Tag"
     ["Kyousei Kaigyou" hatenahelper-insert-br-tag t]
     ["Centerling" hatenahelper-insert-center-tag t]
     ["Left Alignment" hatenahelper-insert-left-tag t]
     ["Right Alignment" hatenahelper-insert-right-tag t]
     ["Font Color (red)" hatenahelper-insert-font-color-tag t]
     ["Big" hatenahelper-insert-font-enlarge-tag t]
     ["Small" hatenahelper-insert-font-shrinkage-tag t]
     ["Strike" hatenahelper-insert-strike-tag t]
     ["Underline" hatenahelper-insert-underline-tag t]
     ["Bold" hatenahelper-insert-bold-tag t]
     ["Italic" hatenahelper-insert-italic-tag t] 
     ["Strong" hatenahelper-insert-strong-tag t]
     ["Blink" hatenahelper-insert-blink-tag t])
    "----"
    ("[N/W Operation]"
     ["Get WebDiary" hatena-get-webdiary  (equal major-mode 'hatena-mode)]
     ["Login" hatena-login (equal major-mode 'hatena-mode)]
     ["Logout" hatena-logout (equal major-mode 'hatena-mode)])
    "----"
    ["Exit" hatena-exit (equal major-mode 'hatena-mode)]
    ["HatenaHelperExit" hatenahelper-exit (not(equal major-mode 'hatena-mode))]
    ))

(require 'easymenu)
(easy-menu-define
  hatenahelper-menu-map
  hatenahelper-mode-map
  "menu map for hatenahelper-mode"
  hatenahelper-menu-map-spec)

;;;
;;; ��������
;;; 
(defvar hatenahelper-blockquote-prefix ">>"
  "Hatena blockquote prefix")
(defvar hatenahelper-blockquote-suffix "<<"
  "Hatena blockquote suffix")
(defvar hatenahelper-nested-blockquote-prefix ">>\n>>"
  "Hatena blockquote prefix")
(defvar hatenahelper-nested-blockquote-suffix "<<\n<<"
  "Hatena blockquote suffix")
(defvar hatenahelper-nested-sonomama-prefix ">>\n>||"
  "Hatena blockquote prefix")
(defvar hatenahelper-nested-sonomama-suffix "||<\n<<"
  "Hatena blockquote suffix")
(defvar hatenahelper-seikeizumi-textblock-prefix ">|"
  "Hatena blockquote prefix")
(defvar hatenahelper-seikeizumi-textblock-suffix "|<"
  "Hatena blockquote suffix")
(defvar hatenahelper-sonomama-textblock-prefix ">||"
  "Hatena blockquote prefix")
(defvar hatenahelper-sonomama-textblock-suffix "||<"
  "Hatena blockquote suffix")
(defvar hatenahelper-draft-tag-prefix "><!--"
  "Hatena Draft prefix")
(defvar hatenahelper-draft-tag-suffix "--><"
  "Hatena Draft suffix")
(defvar hatenahelper-kwd-tag-prefix "[["
  "Hatena Draft prefix")
(defvar hatenahelper-kwd-tag-suffix "]]"
  "Hatena Draft suffix")
(defvar hatenahelper-raw-blockquote-prefix "><blockquote>"
  "Hatena raw blockquote prefix")
(defvar hatenahelper-raw-blockquote-suffix "</blockquote><"
  "Hatena raw blockquote suffix")
(defvar hatenahelper-p-textblock-prefix "<p>"
  "Hatena Draft Tag prefix")
(defvar hatenahelper-p-textblock-suffix "</p>"
  "Hatena Draft Tag suffix")
(defvar hatenahelper-inhibit-p-insertion-prefix "><blockquote>\n<p>"
  "Hatena inhibit auto ptag insertion prefix")
(defvar hatenahelper-inhibit-p-insertion-suffix "</p>\n</blockquote><"
  "Hatena inhibit auto ptag insertion suffix")

;; added
(defvar hatenahelper-p-stop-textblock-prefix "><p>"
  "Hatena p Stop Tag Prefix")
(defvar hatenahelper-p-stop-textblock-suffix "</p><"
  "Hatena p Stop Tag Suffix")

(defvar hatenahelper-default-font-color-4-font-color-tag "red"
  "Default font color for inserted font color tag.")
(defvar hatenahelper-font-color-prefix "<FONT color=\"\">"
  "HTML Font Color Tag Prefix")
(defvar hatenahelper-font-color-suffix "</FONT>"
  "HTML Font Color Tag Suffix")
(defvar hatenahelper-font-enlarge-prefix "<BIG>"
  "HTML Font Enlarge Tag Prefix")
(defvar hatenahelper-font-enlarge-suffix "</BIG>"
  "HTML Font Enlarge Tag Suffix")
(defvar hatenahelper-font-shrinkage-prefix "<SMALL>"
  "HTML Font Shrinkage Tag Prefix")
(defvar hatenahelper-font-shrinkage-suffix "</SMALL>"
  "HTML Font Shrinkage Tag Suffix")
(defvar hatenahelper-strike-prefix "<S>"
  "HTML strike prefix")
(defvar hatenahelper-strike-suffix "</S>"
  "HTML strike suffix")
(defvar hatenahelper-underline-prefix "<U>"
  "HTML underline prefix")
(defvar hatenahelper-underline-suffix "</U>"
  "HTML underline suffix")
(defvar hatenahelper-bold-prefix "<B>"
  "HTML bold prefix")
(defvar hatenahelper-bold-suffix "</B>"
  "HTML bold suffix")
(defvar hatenahelper-italic-prefix "<I>"
  "HTML italic prefix")
(defvar hatenahelper-italic-suffix "</I>"
  "HTML italic suffix")
(defvar hatenahelper-strong-prefix "<STRONG>"
  "HTML strong prefix")
(defvar hatenahelper-strong-suffix "</STRONG>"
  "HTML strong suffix")
(defvar hatenahelper-blink-prefix "<BLINK>"
  "HTML blink prefix")
(defvar hatenahelper-blink-suffix "</BLINK>"
  "HTML blink suffix")

(defvar hatenahelper-link-inhibit-prefix "[]"
  "Hatena link inhibit prefix")
(defvar hatenahelper-link-inhibit-suffix "[]"
  "Hatena link inhibit suffix")

(defvar hatenahelper-center-prefix "<CENTER>"
  "Hatena html centre prefix")
(defvar hatenahelper-center-suffix "</CENTER>"
  "Hatena html centre suffix")
(defvar hatenahelper-left-prefix "<P align=\"left\">"
  "Hatena html left alignment tag prefix")
(defvar hatenahelper-left-suffix "</P>"
  "Hatena html left alignment tag suffix")
(defvar hatenahelper-right-prefix "<P align=\"right\">"
  "Hatena html right alignment tag prefix")
(defvar hatenahelper-right-suffix "</P>"
  "Hatena html right alignment tag suffix")

;;;
;;; �����������ޥ��
;;; 
(defun hatenahelper-insert-blockquote ()
  "Insert Hatena blockquote '>> <<' on current cursor position."
  (interactive)
  (hatenahelper-insert-block-aux hatenahelper-blockquote-prefix 
				 hatenahelper-blockquote-suffix))
(defun hatenahelper-insert-nested-blockquote ()
  "Insert Hatena blockquote '>> >> << <<' on current cursor position."
  (interactive)
  (hatenahelper-insert-block-aux hatenahelper-nested-blockquote-prefix 
				 hatenahelper-nested-blockquote-suffix))
(defun hatenahelper-insert-seikeizumi-textblock ()
  "Insert Hatena blockquote '>| |<' on current cursor position."
  (interactive)
  (hatenahelper-insert-block-aux hatenahelper-seikeizumi-textblock-prefix
				 hatenahelper-seikeizumi-textblock-suffix))
(defun hatenahelper-insert-sonomama-textblock ()
  "Insert Hatena blockquote '>>| |<<' on current cursor position."
  (interactive)
  (hatenahelper-insert-block-aux hatenahelper-sonomama-textblock-prefix
				 hatenahelper-sonomama-textblock-suffix))
(defun hatenahelper-insert-draft-tag ()
  "Insert Hatena Draft Tag '><--! --><' on current cursor position."
  (interactive)
  (hatenahelper-insert-block-aux hatenahelper-draft-tag-prefix
				 hatenahelper-draft-tag-suffix))
(defun hatenahelper-insert-nested-sonomama-textblock ()
  "Insert Hatena nested sonomma textblock'>> >|| ||< <<' on current cursor position."
  (interactive)
  (hatenahelper-insert-block-aux hatenahelper-nested-sonomama-prefix
				 hatenahelper-nested-sonomama-suffix))
(defun hatenahelper-insert-inhibit-p-tag-auto-insertion-block ()
  "Insert Hatena inhibit p-tag auto insertion block around current region."
  (interactive)
  (hatenahelper-insert-block-aux hatenahelper-inhibit-p-insertion-prefix
				 hatenahelper-inhibit-p-insertion-suffix))
;; added
(defun hatenahelper-insert-p-stop-textblock ()
  "Insert Hatena stop p block around current region."
  (interactive)
  (hatenahelper-insert-block-aux hatenahelper-p-stop-textblock-prefix
				 hatenahelper-p-stop-textblock-suffix))

(defun hatenahelper-insert-block-aux (prefix suffix)
  "�����ȥݥ������ޤ��ϥ꡼����������˲��Ԥ��ƥ֥�å�������"
  (let* ((insert-area (hatenahelper-tag-insert-area))
	 (beg (car insert-area))
	 (end (cdr insert-area))
	 (empty-p (= beg end))
	 (delta 0))
    
    (goto-char beg)
	 
    (unless (bolp)			;��Ƭ�Ǥʤ���в���
      (progn (insert "\n")
	     (setq delta (1+ delta))))
    (insert prefix)
    (insert "\n")
    (setq delta (+ delta (length prefix) 1))

    (goto-char end)
    (forward-char delta)
    (unless (bolp) (insert "\n"))	;��Ƭ�Ǥʤ���в���
    (if empty-p (insert "\n")) ;��Ƭ�Ǥ��äƤ�꡼�������Ǥ���в���
    (insert suffix)
    (insert "\n")

    (goto-char (+ beg delta))
    ))

(defun hatenahelper-insert-font-color-tag ()
  "�ե���ȥ��顼�����ǡ��꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
  (interactive)
  (let* ((insert-area (hatenahelper-tag-insert-area))
	 (beg (car insert-area))
	 (end (cdr insert-area))
	 (pref-length (length hatenahelper-font-color-prefix))
	 (total-pref-length (+ pref-length 
			       (length 
				hatenahelper-default-font-color-4-font-color-tag))))
    (goto-char beg)
    (insert hatenahelper-font-color-prefix)
    (backward-char 2)			;���θ��ˤ���'">'��ʬ���
    (insert hatenahelper-default-font-color-4-font-color-tag)
    (forward-char 2)			;��Ҥ��ᤷ��ʬ�ʤ��
    (goto-char end)
    (forward-char total-pref-length)
    (insert hatenahelper-font-color-suffix)
    (backward-char (length hatenahelper-font-color-suffix))
    (goto-char beg)
    (forward-char pref-length)
    (backward-char 2)			;���θ��ˤ���'">'��ʬ���
    )
  )

(defun hatenahelper-insert-link-inhibit-tag ()
  (interactive)
  "��󥯤��������� '[]' �ǡ��꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
					;  (interactive "r") ���ȡ�markerer inactive �ǥ��顼�ˤʤ�
					; �����ǡ��ޡ�����ͭ��̵���˴ؤ�餺������Ԥʤ����᢭�Υ�åѤ�Ȥ�
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-link-inhibit-prefix 
					   hatenahelper-link-inhibit-suffix)))
(defun hatenahelper-tag-insert-area ()
  "�ޡ����������ƥ��֤Ǥ��뤫�ɤ�����ۼ������å�"
  (let* ((p (point))
	 (m (if mark-active    ;marker inactive �ʤ饫���ȥݥ������
		(mark)
	      p))
	 (beg (min p m))
	 (end (max p m)))
    (cons beg end)))

(defun hatenahelper-insert-font-enlarge-tag ()
  "<BIG>�����ǡ��꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-font-enlarge-prefix 
					   hatenahelper-font-enlarge-suffix)))
(defun hatenahelper-insert-font-shrinkage-tag ()
  "<SMALL>�����ǡ��꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-font-shrinkage-prefix 
					   hatenahelper-font-shrinkage-suffix)))
(defun hatenahelper-insert-strike-tag ()
  "�����ä��ˤ���<S>�����ǡ��꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-strike-prefix 
					   hatenahelper-strike-suffix)))
(defun hatenahelper-insert-underline-tag ()
  "��������<U>�ǡ��꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-underline-prefix 
					   hatenahelper-underline-suffix)))
(defun hatenahelper-insert-bold-tag ()
  "�ܡ���ɥ���<B>�ǡ��꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-bold-prefix
					   hatenahelper-bold-suffix)))
(defun hatenahelper-insert-italic-tag ()
  "������å�����<I>�ǡ��꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-italic-prefix 
					   hatenahelper-italic-suffix)))
(defun hatenahelper-insert-strong-tag ()
  "��Ĵ����<STRONG>�ǡ��꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-strong-prefix 
					   hatenahelper-strong-suffix)))
(defun hatenahelper-insert-blink-tag ()
  "�֥�󥯥���<BLINK>�ǡ��꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-blink-prefix
					   hatenahelper-blink-suffix)))
(defun hatenahelper-insert-center-tag ()
  "���󥿥�󥰥���<CENTER>�ǡ��꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-center-prefix 
					   hatenahelper-center-suffix)))
(defun hatenahelper-insert-left-tag ()
  "���󤻤Υ������꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-left-prefix 
					   hatenahelper-left-suffix)))
(defun hatenahelper-insert-right-tag ()
  "���󤻤Υ������꡼�����ޤ��ϥ����ȥݥ�������Ϥ�"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-right-prefix 
					   hatenahelper-right-suffix)))

(defun hatenahelper-insert-kwd-tag ()
  "Insert Hatena Keyword Tag '[[ ]]' on current cursor position."
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-kwd-tag-prefix   
					   hatenahelper-kwd-tag-suffix)))
;; (defun hatenahelper-insert-tag (prefix suffix)
;;   "Insert simple tag(w/o newline) at current cursor position."
;;   (insert prefix)
;;   (insert suffix)
;;   (backward-char (length suffix)))

(defun hatenahelper-insert-tag-around-region (beg end prefix suffix)
  "Insert simple tag around current region."
  (let* ((pref-length (length prefix))
	 (beg-of-region (+ beg pref-length)))
    (goto-char beg)
    (insert prefix)
    (goto-char end)
    (forward-char pref-length)
    (insert suffix)
    (goto-char beg-of-region)))


(defun hatenahelper-insert-br-tag ()
  "�����ȥܥ������˶������ԥ��� <BR>������"
  (interactive)
  (insert "<br>")
  )

(defun hatenahelper-exit()
  "hatenahelper-mode ��λ"
  (interactive)
  (hatenahelper 0)
)

(provide 'hatenahelper-mode)
;;; hatenahelper-mode.el ends here