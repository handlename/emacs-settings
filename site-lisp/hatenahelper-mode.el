;;; hatenahelper-mode.el --- はてなヘルパーモード

;; Copyright (C) 2006  ITSUMI ken-ichi

;; Author: ITSUMI ken-ichi <itsumi@gmail.com>
;; Keywords: blog ブログ はてなダイアリー日記 はてな記法 はてな

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

;;; emacs 上ではてな記法の入力を支援するマイナーモードです。
;;; 
;;; もともとは hatena-mode(http://d.hatena.ne.jp/hikigaeru/20040617)を
;;; 使ってはてなダイアリー日記を書く時に、はてな記法等入力の支援の為に 
;;; .emacs 上に追加していったカスタマイゼーションの集積なのですが、そ
;;; の後はてなグループ日記や、あしかなど、はてなダイアリー日記以外でも
;;; はてな記法を使うことが多くなってきたので、マイナーモードとして独立
;;; させてみたものです。
;;; 
;;; 次のような状況で使われることを想定しています。
;;; 1) hatena-mode.el で、はてなダイアリー日記を編集する時
;;; 2) w3m.el の html入力ホーム上で、はてなダイアリ他に対してはてな記
;;;    法を行なう時
;;; 3) mozex を使って、firefox の html入力フォームへの入力を emacs で
;;;    行ない、はてな記法を使う時
;;; 4) hatena-mode 以外のメジャーモードで、はてな記法を編集する時
;;;    日記の中にプログラムを書き込むような時は便利かも
;;; 

;; ■インストール方法
;; 1) 適当なディレクトリにこのファイルをおく.
;;    (以下、~/elisp/hatenahelper-mode/ においたと仮定して話を進める). 
;;
;; 2) .emacs に以下の行を追加する
;;
;; (add-to-list 'load-path "~/elisp/hatenahelper-mode/")
;; (require 'hatenahelper-mode)
;; (global-set-key "\C-xH" 'hatenahelper-mode)
;; ;(add-hook 'hatena-mode-hook 'hatenahelper-mode)  ; 本当はこう
;; (add-hook 'hatena-mode-hook
;;	  '(lambda ()
;;           ; other hooks must be wrote here!
;;	     (hatenahelper-mode 1)))


;;
;; ■hatenahelper-mode を有効にする
;; 
;; 1)hatena-mode の場合
;;   hatena-mode になると自動的に hatenahelper-mode が有効になります
;;
;; 2)その他の場合
;;   C-x H または M-x hatenahelper-mode と入力します。
;;
;; ■使い方
;; hatenahelper-mode になると、メニューバーの右端の方に Hatena という
;; エントリが出現しますので、それを選択してやれば、あとは大体分るとお
;; もいます。
;; 基本的に領域を囲む記法については、カレントリージョンを囲むように挿
;; 入し、カレントリージョンが空の場合や、一箇所に挿入するようなものは
;; カーソル位置に挿入するように動作します。
;;
;; はてな記法のタグを挿入する操作
;; C-c q 	引用記法(>>〜<<)
;; C-c C-q	二重の引用記法(>>>>〜<<<<)
;; C-c C-t	pre記法(>|〜|<)
;; C-c C-s	スーパーpre記法(>||〜||<)
;; C-c C-s	引用記法の中にスーパーpre記法をネスト(>> >||〜||< <<)
;; C-c d	下書き機能(><!--〜--><)
;; C-c k	キーワード([[〜]])
;; 
;; はてな記法と併用することの多い htmlタグを挿入する操作
;; C-c r	強制改行(<BR>)
;; C-c ^	センタリング(<CENTER>〜</CENTER>
;; C-c +	フォント拡大(<BIG>〜</BIG>)
;; C-c +	フォント拡大(<BIG>〜</BIG>)
;; C-c -	フォント縮小(<SMALL>〜</SMALL>)
;; C-c =	見え消し(<S>〜</S>)
;; C-c =	下線(<U>〜</U>)

;; ■hook
;;    easy-mmode が生成する hatenahelper-mode-hook フック変数に関数名
;;    を add-hook してやれば、それらは hatenahelper-mode 実行の最後に
;;    実行されます

;; ■ToDO
;; hatena-mode ともども 非公式 debian パッケージを作る
;; w3m.el から使うときに便利になるよう拡張する


;;; Code:
(defconst hatenahelper-mode-version 
  "0.1" "Version number of hatenahelper-mode.el")
;;;
;;; easy-mmode を使ったマイナーモード定義
;;; 
(easy-mmode-define-minor-mode 
 hatenahelper-mode
 "Toggle HatenaHelper mode.
     With no argument, this command toggles the mode. 
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode."
 ;; 初期値
 nil
 ;; モード行への表示
 ""
 ;; マイナモードのバインディング
 ;; keymap は、hatenahelper-mode-map という変数に格納される
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
 
    ("\C-c\C-y" . clipboard-yank)	; クリップボード内容を挿入
   ("\C-c\y" . clipboard-yank)		;firefox で copyURL した奴を張るのに使う
   ))

;;;
;;; easy-menu を使ったメニュー定義
;;;

(defvar hatenahelper-menu-map nil
  "Menu for hatena mode.")
(defvar hatenahelper-html-submenu-map nil
  "はてな記法と交えてよく使う htmlタグを入力するサブメニュー")
(defvar hatenahelper-special-tag-submenu-map nil
  "はてな記法のタグを入力するサブメニュー")

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
;;; 挿入タグ
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
;;; タグ挿入コマンド
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
  "カレントポジションまたはリージョンの前後に改行してブロックを挿入"
  (let* ((insert-area (hatenahelper-tag-insert-area))
	 (beg (car insert-area))
	 (end (cdr insert-area))
	 (empty-p (= beg end))
	 (delta 0))
    
    (goto-char beg)
	 
    (unless (bolp)			;行頭でなければ改行
      (progn (insert "\n")
	     (setq delta (1+ delta))))
    (insert prefix)
    (insert "\n")
    (setq delta (+ delta (length prefix) 1))

    (goto-char end)
    (forward-char delta)
    (unless (bolp) (insert "\n"))	;行頭でなければ改行
    (if empty-p (insert "\n")) ;行頭であってもリージョン空であれば改行
    (insert suffix)
    (insert "\n")

    (goto-char (+ beg delta))
    ))

(defun hatenahelper-insert-font-color-tag ()
  "フォントカラータグで、リージョンまたはカレントポジションを囲む"
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
    (backward-char 2)			;色の後ろにくる'">'の分戻る
    (insert hatenahelper-default-font-color-4-font-color-tag)
    (forward-char 2)			;上述の戻した分進める
    (goto-char end)
    (forward-char total-pref-length)
    (insert hatenahelper-font-color-suffix)
    (backward-char (length hatenahelper-font-color-suffix))
    (goto-char beg)
    (forward-char pref-length)
    (backward-char 2)			;色の後ろにくる'">'の分戻る
    )
  )

(defun hatenahelper-insert-link-inhibit-tag ()
  (interactive)
  "リンクを抑制する '[]' で、リージョンまたはカレントポジションを囲む"
					;  (interactive "r") だと、markerer inactive でエラーになる
					; そこで、マーカが有効無効に関わらず処理を行なうため↓のラッパを使う
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-link-inhibit-prefix 
					   hatenahelper-link-inhibit-suffix)))
(defun hatenahelper-tag-insert-area ()
  "マーカがアクティブであるかどうかを吸収するラッパ"
  (let* ((p (point))
	 (m (if mark-active    ;marker inactive ならカレントポジション
		(mark)
	      p))
	 (beg (min p m))
	 (end (max p m)))
    (cons beg end)))

(defun hatenahelper-insert-font-enlarge-tag ()
  "<BIG>タグで、リージョンまたはカレントポジションを囲む"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-font-enlarge-prefix 
					   hatenahelper-font-enlarge-suffix)))
(defun hatenahelper-insert-font-shrinkage-tag ()
  "<SMALL>タグで、リージョンまたはカレントポジションを囲む"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-font-shrinkage-prefix 
					   hatenahelper-font-shrinkage-suffix)))
(defun hatenahelper-insert-strike-tag ()
  "見え消しにする<S>タグで、リージョンまたはカレントポジションを囲む"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-strike-prefix 
					   hatenahelper-strike-suffix)))
(defun hatenahelper-insert-underline-tag ()
  "下線タグ<U>で、リージョンまたはカレントポジションを囲む"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-underline-prefix 
					   hatenahelper-underline-suffix)))
(defun hatenahelper-insert-bold-tag ()
  "ボールドタグ<B>で、リージョンまたはカレントポジションを囲む"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-bold-prefix
					   hatenahelper-bold-suffix)))
(defun hatenahelper-insert-italic-tag ()
  "イタリックタグ<I>で、リージョンまたはカレントポジションを囲む"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-italic-prefix 
					   hatenahelper-italic-suffix)))
(defun hatenahelper-insert-strong-tag ()
  "強調タグ<STRONG>で、リージョンまたはカレントポジションを囲む"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-strong-prefix 
					   hatenahelper-strong-suffix)))
(defun hatenahelper-insert-blink-tag ()
  "ブリンクタグ<BLINK>で、リージョンまたはカレントポジションを囲む"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-blink-prefix
					   hatenahelper-blink-suffix)))
(defun hatenahelper-insert-center-tag ()
  "センタリングタグ<CENTER>で、リージョンまたはカレントポジションを囲む"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-center-prefix 
					   hatenahelper-center-suffix)))
(defun hatenahelper-insert-left-tag ()
  "左寄せのタグ、リージョンまたはカレントポジションを囲む"
  (interactive)
  (let ((insert-area (hatenahelper-tag-insert-area)))
    (hatenahelper-insert-tag-around-region (car insert-area)
					   (cdr insert-area)
					   hatenahelper-left-prefix 
					   hatenahelper-left-suffix)))
(defun hatenahelper-insert-right-tag ()
  "右寄せのタグ、リージョンまたはカレントポジションを囲む"
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
  "カレントボジションに強制改行タグ <BR>を挿入"
  (interactive)
  (insert "<br>")
  )

(defun hatenahelper-exit()
  "hatenahelper-mode を終了"
  (interactive)
  (hatenahelper 0)
)

(provide 'hatenahelper-mode)
;;; hatenahelper-mode.el ends here