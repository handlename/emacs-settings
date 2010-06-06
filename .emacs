;; .emacs written by NAGATA Hiroaki (handlename.net)

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

(require 'cl)

(setq byte-compile-warnings '(free-vars unresolved callargs redefine obsolete noruntime cl-functions interactive-only make-local))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; PATH
;;______________________________________________________________________

(setq exec-path (cons "/usr/local/bin" exec-path))

;;
;; mail address
;;______________________________________________________________________

(setq user-mail-address "main@handlename.net")


;;
;; meta key
;;______________________________________________________________________

(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))


;;
;; Window settings
;;______________________________________________________________________

(if window-system
    (progn
      (set-frame-parameter nil 'alpha 100) ; 透明度
      (tool-bar-mode nil)                  ; ツールバー非表示
      (set-scroll-bar-mode nil)            ; スクロールバー非表示
      (setq line-spacing 0.2)              ; 行間
      (when (>= emacs-major-version 23)
        (tool-bar-mode nil)
        (set-frame-font "Menlo-12")
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0208
                          (font-spec :family "M+2VM+IPAG circle" :size 14)))
      (setq ns-pop-up-frames nil)))


;;
;; Color
;;______________________________________________________________________

(set-foreground-color                                  "#CCCCCC") ; 文字色
(set-background-color                                  "#333333") ; 背景色
(set-cursor-color                                      "#FF0000") ; カーソル色
(set-face-background 'region                           "#222244") ; リージョン
(set-face-foreground 'modeline                         "#CCCCCC") ; モードライン文字
(set-face-background 'modeline                         "#333333") ; モードライン背景
(set-face-foreground 'mode-line-inactive               "#333333") ; モードライン文字(非アクティブ)
(set-face-background 'mode-line-inactive               "#CCCCCC") ; モードライン背景(非アクティブ)
(set-face-foreground 'font-lock-comment-delimiter-face "#888888") ; コメントデリミタ
(set-face-foreground 'font-lock-comment-face           "#888888") ; コメント
(set-face-foreground 'font-lock-string-face            "#7FFF7F") ; 文字列
(set-face-foreground 'font-lock-function-name-face     "#BF7FFF") ; 関数名
(set-face-foreground 'font-lock-keyword-face           "#FF7F7F") ; キーワード
(set-face-foreground 'font-lock-constant-face          "#FFBF7F") ; 定数(this, selfなども)
(set-face-foreground 'font-lock-variable-name-face     "#7F7FFF") ; 変数
(set-face-foreground 'font-lock-type-face              "#FFFF7F") ; クラス
(set-face-foreground 'fringe                           "#666666") ; fringe(折り返し記号なでが出る部分)
(set-face-background 'fringe                           "#282828") ; fringe

(add-hook 'org-mode-hook
          '(lambda ()
             (set-face-foreground 'org-hide "#282828")))

(add-hook 'mmm-mode-hook
          '(lambda ()
             (set-face-background 'mmm-default-submode-face "#404040")))

(add-hook 'linum-mode-hook
          '(lambda ()
             (set-face-foreground 'linum "#666666")
             (set-face-background 'linum "#000000")))



;;
;; encoding
;;______________________________________________________________________

(set-language-environment       "Japanese")
(prefer-coding-system           'utf-8-unix)
(setq                           default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system  'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-clipboard-coding-system    'utf-8)


;;
;; general key bind
;;______________________________________________________________________

(global-set-key (kbd "C-c a")   'align)
(global-set-key (kbd "C-c M-a") 'align-regexp)
(global-set-key (kbd "C-h")     'backward-delete-char)
(global-set-key (kbd "C-c d")   'delete-indentation)
(global-set-key (kbd "M-g")     'goto-line)
(global-set-key (kbd "C-S-i")   'indent-region)
(global-set-key (kbd "C-m")     'newline-and-indent)
(global-set-key (kbd "C-t")     'next-multiframe-window)
(global-set-key (kbd "M-<RET>") 'ns-toggle-fullscreen)
(global-set-key (kbd "C-S-t")   'previous-multiframe-window)
(global-set-key (kbd "C-M-r")   'replace-regexp)
(global-set-key (kbd "C-r")     'replace-string)
(global-set-key (kbd "C-/")     'undo)


;;
;; backup file
;;______________________________________________________________________

;; 残さない
(setq make-backup-files nil)


;;
;; miscellaneous
;;______________________________________________________________________

;; Bell
(setq ring-bell-function 'ignore)

;; Scroll
(setq scroll-step 1)

;; Column number
(column-number-mode t)

;; Fill column
(setq default-fill-column 72)

;; Truncate
(setq truncate-lines t)
(setq truncate-partial-width-windows t)

;; Indent
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(c-set-offset 'case-label '+)

;; Narrowing
(put 'narrow-to-region 'disabled nil)

;; 行番号表示
(global-linum-mode)
(setq linum-format "%4d")

;; スタートページ非表示
(setq inhibit-startup-message t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; kill-all-buffers
;;______________________________________________________________________

(defun kill-all-buffers ()
  (interactive)
  (loop for buffer being the buffers
        do (kill-buffer buffer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; current-date-string
;;______________________________________________________________________

(defun current-date-string () (interactive) "現在の日付を挿入する" (insert (format-time-string "%Y-%m-%d")))


;;
;; align
;;______________________________________________________________________

(require 'align)

;; Align for php-mode
;; http://d.hatena.ne.jp/Tetsujin/20070614/1181757931
(add-to-list 'align-rules-list
             '(php-assignment
               (regexp   . "[^-=!^&*+<>/.| \t\n]\\(\\s-*[.-=!^&*+<>/|]*\\)=>?\\(\\s-*\\)\\([^= \t\n]\\|$\\)")
               (justify  . t)
               (tab-stop . nil)
               (modes    . '(php-mode))))
(add-to-list 'align-dq-string-modes 'php-mode)
(add-to-list 'align-sq-string-modes 'php-mode)
(add-to-list 'align-open-comment-modes 'php-mode)
(setq align-region-separate (concat "\\(^\\s-*$\\)\\|"
                                    "\\([({}\\(/\*\\)]$\\)\\|"
                                    "\\(^\\s-*[)}\\(\*/\\)][,;]?$\\)\\|"
                                    "\\(^\\s-*\\(}\\|for\\|while\\|if\\|else\\|"
                                    "switch\\|case\\|break\\|continue\\|do\\)[ ;]\\)"
                                    ))


;; for ruby-mode
;; http://d.hatena.ne.jp/rubikitch/20080227/1204051280
(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
               (regexp . ",\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-hash-literal
               (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-assignment-literal
               (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list          ;TODO add to rcodetools.el
             '(ruby-xmpfilter-mark
               (regexp . "\\(\\s-*\\)# => [^#\t\n]")
               (repeat . nil)
               (modes  . '(ruby-mode))))


;;
;; auto-save-buffers
;;______________________________________________________________________

(require 'auto-save-buffers)
(run-with-idle-timer 0.5 t 'auto-save-buffers)


;;
;; migemo
;;______________________________________________________________________

(setq migemo-command "migemo")
(setq migemo-options '("-t" "emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setenv "RUBYLIB" "/Library/Ruby/Site/")
(require 'migemo)
(migemo-init)


;;
;; anything
;;______________________________________________________________________

(require 'anything-config)

(define-key anything-map (kbd "C-M-n") 'anything-next-source)
(define-key anything-map (kbd "C-M-p") 'anything-previous-source)

;; 履歴を保存するとエラーになるので
(remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)
(ad-disable-advice 'anything-exit-minibuffer 'before 'anything-c-adaptive-exit-minibuffer)
(ad-disable-advice 'anything-select-action 'before 'anything-c-adaptive-select-action)
(setq anything-c-adaptive-history-length 0)

;; anything-migemo
(require 'anything-migemo)
;(define-key global-map [(control ?:)] 'anything-migemo)

;; バッファ内検索
(define-key global-map (kbd "C-s")
  '(lambda ()
     "Search current buffer in anything"
     (interactive)
     (anything '(anything-c-source-occur))))

;; buffer list + buffer history + files in current directory
(define-key global-map (kbd "C-x b")
  (lambda ()
    "Open buffer list in anything"
    (interactive)
    (anything '(anything-c-source-buffers
                anything-c-source-file-name-history
                anything-c-source-files-in-current-dir))))

;; calc
(define-key global-map (kbd "C-c C-a c")
  (lambda ()
    "Calculate in anything"
    (interactive)
    (anything '(anything-c-source-calculation-result))))

;; killring history
(define-key global-map (kbd "C-M-y") 'anything-show-kill-ring)

;; emacs commands
(define-key global-map (kbd "M-x")
  (lambda ()
    "Execute emacs commands in anything"
    (interactive)
    (anything '(anything-c-source-emacs-commands))))

;; tag jump
(require 'anything-etags)
(require 'anything-gtags)
(define-key global-map (kbd "C-x t")
  (lambda ()
    "Tag jump using etags, gtags and `anything'."
    (interactive)
    (let* ((initial-pattern (regexp-quote (or (thing-at-point 'symbol) ""))))
      (anything (list anything-c-source-gtags-select
                      anything-c-source-etags-select))
      "Find Tag: " nil)))

;; anything-hatena-bookmark
(require 'anything-hatena-bookmark)
(define-key global-map (kbd "C-c C-a b") 'anything-hatena-bookmark)


;;
;; anything-grep
;;______________________________________________________________________

(require 'anything-grep)

;; プロジェクトのルートを自動的に判別してanything-grepをする。
;; http://d.hatena.ne.jp/IMAKADO/20090225/1235526604
(defun anything-define-kyr-grep (command)
  (lexical-let ((command command))
    (lambda ()
      (interactive)
      (let ((query (read-string "Grep query: " (or (thing-at-point 'symbol) "")))
            (root-dir (agrep-find-project-root-dir
                       (or (and buffer-file-name (file-name-directory (buffer-file-name)))
                           (expand-file-name default-directory)
                           (expand-file-name "~")))))
        (when (and query root-dir)
          (anything-grep-base
           (list
            (agrep-source (format (agrep-preprocess-command command)
                                  (shell-quote-argument query)) root-dir))))))))

(defun agrep-find-project-root-dir (dir)
  "Determine whether the given directory is project root dir.
This function checks parent directories recursively. If this
function found the user's home directory or the system root directory,
returns nil."
  (let* ((expanded-dir (expand-file-name dir))
        (file (some 'agrep-resource--project-filep (directory-files expanded-dir))))
    (if file expanded-dir
      (if (or
           (string= expanded-dir "/")
           (string= expanded-dir (expand-file-name "~/"))
           ) nil
        (agrep-find-project-root-dir
         (concat (file-name-as-directory dir) ".."))))))

(defvar agrep-project-root-files
  '("build.xml" "prj.el" ".project" "pom.xml"
    "Makefile" "configure" "Rakefile"
    "NAnt.build" "Makefile.PL"))

(defun agrep-resource--project-filep (file)
  "Determine whether the given file is a project build file.
The current implementation checks
`agrep-project-root-files'."
  (find file
        agrep-project-root-files
        :test 'string=))

(global-set-key (kbd "M-8")
  (anything-define-kyr-grep "ack -afG '(tt2|tt|yaml|yml|p[lm]|php)$' | xargs egrep -Hin %s"))


;;
;; color-moccur
;; http://d.hatena.ne.jp/IMAKADO/20080724/1216882563
;;______________________________________________________________________

;;; color-moccur.elの設定
(require 'color-moccur)
;; 複数の検索語や、特定のフェイスのみマッチ等の機能を有効にする
;; 詳細は http://www.bookshelf.jp/soft/meadow_50.html#SEC751
(setq moccur-split-word t)
;; migemoがrequireできる環境ならmigemoを使う
(when (require 'migemo nil t) ;第三引数がnon-nilだとloadできなかった場合にエラーではなくnilを返す
  (setq moccur-use-migemo t))
;;; anything-c-moccurの設定
(require 'anything-c-moccur)
;; カスタマイズ可能変数の設定(M-x customize-group anything-c-moccur でも設定可能)
(setq anything-c-moccur-anything-idle-delay 0.2    ; `anything-idle-delay'
      anything-c-moccur-enable-initial-pattern nil ; はじめからカーソル位置のパターンが入力されるのを抑止
      anything-c-moccur-higligt-info-line-flag t   ; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
      anything-c-moccur-enable-auto-look-flag t    ; 現在選択中の候補の位置を他のwindowに表示する
      anything-c-moccur-enable-initial-pattern t)  ; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする

;;; キーバインドの割当(好みに合わせて設定してください)
(global-set-key (kbd "C-S-s") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
(global-set-key (kbd "C-c C-a S") 'anything-c-moccur-dmoccur) ;ディレクトリ
(add-hook 'dired-mode-hook ;dired
          '(lambda ()
             (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))


;;
;; elscreen
;;______________________________________________________________________

(load "elscreen" "ElScreen" t)
(setq elscreen-display-tab nil) ; タブを非表示
(define-key global-map (kbd "M-t") 'elscreen-next)
(define-key global-map (kbd "M-C-t") 'elscreen-create)
(define-key global-map (kbd "M-C-w") 'elscreen-kill)

;;
;; highlight
;;______________________________________________________________________

;; highlight current line
(require 'highlight-current-line)
(highlight-current-line-on t)
(set-face-background 'highlight-current-line-face "#000000")

;; hilight paren
(show-paren-mode 1)

;; highlight reagion
(setq transient-mark-mode t)

;; highlight edit characters
(require 'jaspace)
(setq jaspace-highlight-tabs t)
(add-hook 'mmm-mode-hook 'jaspace-mmm-mode-hook)

;; highlight current buffer
;; http://ksugita.blog62.fc2.com/blog-entry-8.html
;; (load-file "~/.emacs.d/site-lisp/hiwin.el")
;; (setq hiwin-color "#496B22")
;; (hiwin-mode)


;;
;; Install elisp
;;______________________________________________________________________

(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/")


;;
;; open file as root
;;______________________________________________________________________

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


;;
;; sense-region
;; http://taiyaki.org/elisp/sense-region/
;;______________________________________________________________________

(autoload 'sense-region-on
  "sense-region"
  "System to toggle region and rectangle." t nil)
(sense-region-on)


;;
;; revive
;;______________________________________________________________________

(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe emacs" t)
(define-key global-map (kbd "C-x S") 'save-current-configuration)
(define-key global-map (kbd "C-x F") 'resume)
(add-hook 'kill-emacs-hook 'save-current-configuration)


;;
;; uniq
;;______________________________________________________________________

(load "uniq")


;;
;; yasnippet
;;______________________________________________________________________

(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)

(set-face-background 'yas/field-highlight-face "#404040")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; major mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; c++-mode
;;______________________________________________________________________

(setenv "CPLUS_INCLUDE_PATH" "/opt/local/include")
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c c") 'compile)
             (c-set-style "cc-mode")))


;;
;; change-log-mode
;;______________________________________________________________________

(add-hook 'change-log-mode-hook
          '(lambda ()
             (setq tab-width 4)
             (setq left-margin 4)))


;;
;; html-helper-mode
;;______________________________________________________________________

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq html-helper-basic-offset 0)
(setq html-helper-item-continue-indent 0)
(defvar html-helper-new-buffer-template
  '("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
    "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"ja\" lang=\"ja\">\n"
    "\n"
    "<head>\n"
    "<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\">\n"
    "<title></title>\n"
    "</head>\n"
    "\n"
    "<body>\n"
    "\n"
    "\n"
    "\n"
    "</body>\n"
    "</html>\n")
  "*Template for new buffers, inserted by html-helper-insert-new-buffer-strings if html-helper-build-new-buffer is set to t")
(require 'sgml-mode)
(add-hook 'html-helper-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-e") 'sgml-close-tag)))



;;
;; emacs-lisp-mode
;;______________________________________________________________________

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-e") 'eval-last-sexp)
             (local-set-key (kbd "C-c e")   'eval-region)
             (setq indent-tabs-mode nil)))


;;
;; js2-mode
;;______________________________________________________________________

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; fixing indentation
; refer to http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(autoload 'espresso-mode "espresso")

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4
        js2-mirror-mode nil)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map (kbd "C-m") 'newline-and-indent)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)


;;
;; lisp-mode
;;______________________________________________________________________

(add-hook 'lisp-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-e") 'eval-last-sexp)
             (local-set-key (kbd "C-c e")   'eval-region)
             (setq indent-tabs-mode nil)))


;;
;; markdown-mode
;;______________________________________________________________________

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)

;;
;; objc-mode
;;______________________________________________________________________

(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

;; Xcode上でコンパイル＆実行
(defun xcode-buildandrun ()
 (interactive)
 (do-applescript
  (format
   (concat
    "tell application \"Xcode\" to activate \r"
    "tell application \"System Events\" \r"
    "     tell process \"Xcode\" \r"
    "          key code 36 using {command down} \r"
    "    end tell \r"
    "end tell \r"
    ))))

;; Xcodeにブレークポイント追加
;; http://d.hatena.ne.jp/kaniza/20090915/p1
(defun xcode-add-breakpoint-at-line ()
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (file-path buffer-file-name))
    (do-applescript (concat
     "tell application \"Xcode\"
        activate
        tell front project
          repeat with r in file references
            set p to full path of r
            if \"" file-path "\" = p then
              set bp to make new file breakpoint with properties {line number:" line "}
              set file reference of bp to r
              set enabled of bp to true
              exit repeat
            end if
         end repeat
       end tell
     end tell"))))

(add-hook 'objc-mode-hook
          (lambda ()
            (define-key objc-mode-map (kbd "C-c C-r") 'xcode-buildandrun)
            (define-key objc-mode-map (kbd "C-c C-b") 'xcode-add-breakpoint-at-line)
            ))


;;
;; org-mode
;;______________________________________________________________________

(require 'org-install)
(add-hook 'org-mode-hook
          '(lambda ()
             (setq org-log-done t)
             (setq org-tags-column 60)
             (setq org-hide-leading-stars t)))


;;
;; php-mode
;;______________________________________________________________________

(require 'php-mode)
(autoload 'php-mode "php-mode")
;; (setq auto-mode-alist
;;       (cons '("\\.php\\'" . php-mode) auto-mode-alist))
(setq php-mode-force-pear)
(defun pear/php-mode-init()
  "Set some buffer-local variables."
  (setq case-fold-search t)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0))
(add-hook 'php-mode-hook 'pear/php-mode-init)
(add-hook 'php-mode-hook
          '(lambda ()
             (c-set-style "stroustrup")
             (setq php-manual-path "/usr/share/doc/php/html")
             (setq php-search-url  "http://www.phppro.jp/")
             (setq php-manual-url  "http://www.phppro.jp/phpmanual")
             (setq tab-width 4)
            ))

;; php-eval.el
;; http://www.ne.jp/asahi/alpha/kazu/pub/emacs/php-eval.el
(require 'php-eval)

; リージョン内のコード(<?php ?>がなくてもOK)を評価
; http://d.hatena.ne.jp/kitokitoki/20100605/p1
(defun my-php-eval-region ()
  (interactive)
  (when (region-active-p)
    (let ((region-str (buffer-substring-no-properties (region-beginning) (region-end)))
          (result-buf "*php*")
          (temp-file "/tmp/hoge.php"))
      (with-temp-file temp-file
        (insert "<?php \n" region-str))
      (shell-command (concat "php " temp-file) result-buf)
      (view-buffer-other-window result-buf t
                                (lambda (buf)
                                  (kill-buffer-and-window)
                                  (delete-file temp-file))))))

(eval-after-load 'php-eval
  '(progn
    (define-key php-mode-map (kbd "C-c C-r C-r") 'my-php-eval-region)
    (define-key php-mode-map (kbd "C-c C-r C-v") 'php-eval-display-buffer)
    ))


;;
;; ruby-mode
;;______________________________________________________________________

(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq tab-width 4)
             (setq ruby-indent-level tab-width)))


;;
;; text-mode
;;______________________________________________________________________

(add-hook 'text-mode-hook
          '(lambda ()
             (setq tab-width 4)))


;;
;; wdired
;;______________________________________________________________________

(autoload 'wdired-change-to-wdired-mode "wdired")
(add-hook 'dired-load-hook
          '(lambda ()
             (define-key dired-mode-map "r" 'wdired-change-to-wdired-
               (define-key dired-mode-map
                 [menu-bar immediate wdired-change-to-wdired-mode]
                 '("Edit File Names" . wdired-change-to-wdired-mode)))))


;;
;; yatex-mode
;;______________________________________________________________________

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;
;; yatex-mode
;;______________________________________________________________________

(add-to-list 'load-path "~/.emacs.d/site-lisp/yatex/")
(setq auto-mode-alist (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq tex-command "/opt/local/bin/platex")
(setq dvi2-command "/opt/local/bin/xdvi")
(setq YaTeX-kanji-code 4) ;; utf-8
(add-hook 'yatex-mode-hook'(lambda ()(setq auto-fill-function nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minor mode settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; clmemo
;;______________________________________________________________________

(autoload 'clmemo "clmemo" "ChangeLog memo mode." t)
(setq clmemo-file-name "~/Dropbox/chalow/changelog")
(global-set-key (kbd "C-x m") 'clmemo)


;;
;; flymake-mode
;;______________________________________________________________________

(require 'flymake)

;; GUIの警告は表示しない
(setq flymake-gui-warnings-enabled nil)

;; 全てのファイルで flymakeを有効化
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; エラーメッセージをポップアップ表示
(defun flymake-popup-err-message ()
  "Display a menu with errors/warnings for current line if it has errors and/or warnings."
  (interactive)
  (let* ((line-no            (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (menu-data          (flymake-make-err-menu-data line-no line-err-info-list)))
    (if menu-data
      (popup-tip (mapconcat '(lambda (e) (nth 0 e))
                            (nth 1 menu-data)
                            "\n")))
    ))
;; ;; カーソルをエラー行に載せるとエラーメッセージをポップアップ表示
;; ;; anythingと干渉するようなのでコメントアウト 
;; (defadvice flymake-mode (before post-command-stuff activate compile)
;;   "エラー行にカーソルが当ったら自動的にエラーが minibuffer に表示されるように
;; post command hook に機能追加"
;;   (set (make-local-variable 'post-command-hook)
;;        (add-hook 'post-command-hook 'flymake-popup-err-message)))

;; キーバインド
(global-set-key "\M-p" 'flymake-goto-prev-error)
(global-set-key "\M-n" 'flymake-goto-next-error)
(global-set-key "\C-cd" 'flymake-popup-err-message)

;; Objective-C 用設定
(defvar xcode:gccver "4.2.1")
(defvar xcode:sdkver "3.1.2")
(defvar xcode:sdkpath "/Developer/Platforms/iPhoneSimulator.platform/Developer")
(defvar xcode:sdk (concat xcode:sdkpath "/SDKs/iPhoneSimulator" xcode:sdkver ".sdk"))
(defvar flymake-objc-compiler (concat xcode:sdkpath "/usr/bin/gcc-" xcode:gccver))
(defvar flymake-objc-compile-default-options (list "-Wall" "-Wextra" "-fsyntax-only" "-ObjC" "-std=c99" "-isysroot" xcode:sdk))
(defvar flymake-last-position nil)
(defvar flymake-objc-compile-options '("-I."))
(defun flymake-objc-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                    'flymake-create-temp-inplace))
         (local-file (file-relative-name
                     temp-file
                     (file-name-directory buffer-file-name))))
     (list flymake-objc-compiler (append flymake-objc-compile-default-options flymake-objc-compile-options (list local-file)))))

(add-hook 'objc-mode-hook
         (lambda ()
           ;; 拡張子 m と h に対して flymake を有効にする設定 flymake-mode t の前に書く必要があります
           (push '("\\.m$" flymake-objc-init) flymake-allowed-file-name-masks)
           (push '("\\.h$" flymake-objc-init) flymake-allowed-file-name-masks)
           ;; 存在するファイルかつ書き込み可能ファイル時のみ flymake-mode を有効にします
           (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
               (flymake-mode t))))

;;
;; gtags
;;______________________________________________________________________

(autoload 'gtags-mode "gtags" "" t)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (gtags-mode 1)
             (gtags-make-complete-list)
             ))
(add-hook 'php-mode-common-hook
          '(lambda ()
             (gtags-mode 1)
             (gtags-make-complete-list)
             ))

;;
;; mmm-mode
;;______________________________________________________________________

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

;; Setting from http://www.bookshelf.jp/soft/meadow_13.html#SEC101

;; html + css
(mmm-add-classes
 '((mmm-css
    :submode css-mode
    :front "<style[^>]*>"
    :back "</style>")))
(mmm-add-mode-ext-class nil "\\.html?\\'" 'mmm-css)

;; html + js
(mmm-add-classes
 '((mmm-js
    :submode js-mode
    :front "<script[^>]*>[^<]"
    :front-offset -1
    :back "\n?[ \t]*</script>")))
(mmm-add-mode-ext-class nil "\\.html?\\'" 'mmm-js)

;; php + html + js + css
;;(add-to-list 'auto-mode-alist '("\\.php?\\'" . html-helper-mode))
(mmm-add-classes
 '((mmm-php
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")))
;; (mmm-add-mode-ext-class nil "\\.php\\'" 'mmm-php-in-html)
;; (mmm-add-mode-ext-class nil "\\.php\\'" 'mmm-css-in-html)
;; (mmm-add-mode-ext-class nil "\\.php\\'" 'mmm-js-in-html)
(mmm-add-mode-ext-class nil "\\.yml\\'" 'mmm-php)
;; インデントが効かなくなるのを解消
(defun save-mmm-c-locals ()
  (with-temp-buffer
    (php-mode)
    (dolist (v (buffer-local-variables))
      (when (string-match "\\`c-" (symbol-name (car v)))
        (add-to-list 'mmm-save-local-variables `(,(car v) nil
                                                 ,mmm-c-derived-modes))))))
(save-mmm-c-locals)

;; yaml + php
(mmm-add-classes
 '((mmm-php-in-yaml
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")))
(mmm-add-mode-ext-class nil "\\.yml?\\'" 'mmm-php-in-yaml)


;;
;; hs-minor-mode
;;______________________________________________________________________

(load-library "hideshow")
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'php-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

(define-key hs-minor-mode-map (kbd "C-c C-h t") 'hs-toggle-hiding)

;;
;; outputz
;; http://taiyaki.org/elisp/sense-region/
;;______________________________________________________________________

(require 'outputz)
(load-file "~/.emacs.d/outputz-pass.el")
(setq outputz-key outputz-pass)      ;; 復活の呪文
(setq outputz-uri "http://emacs.curly.local/%s") ;; 適当なURL。%sにmajor-modeの名前が入るので、major-modeごとのURLで投稿できます。
(global-outputz-mode t)


;;
;; symfony-minor-mode
;;______________________________________________________________________

(require 'symfony)


;;
;; zencoding-mode
;;______________________________________________________________________

(require 'zencoding-mode)
(add-hook 'html-helper-mode-hook 'zencoding-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(safe-local-variable-values (quote ((clmemo-mode . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;
;; auto-complete
;;______________________________________________________________________

; 下の方に置かないとうまくいかない

(require 'auto-complete-config)
(require 'auto-complete-etags)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-set-key (kbd "M-_") 'auto-complete)
(setq ac-auto-start 1)       ; 補完を開始する文字数
(setq ac-auto-show-menu 0.2) ; 補完リストが表示されるまでの時間
(setq ac-sources '(ac-source-yasnippet
                   ac-source-dictionary
                   ac-source-gtags
                   ac-source-words-in-buffer))

(add-to-list 'ac-modes 'javascript-mode)
(add-to-list 'ac-modes 'html-helper-mode)
(add-to-list 'ac-modes 'emacs-lisp-mode)
(add-to-list 'ac-modes 'yaml-mode)

; for emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (add-to-list 'ac-sources 'ac-source-functions)
             (add-to-list 'ac-sources 'ac-source-symbols)))

; for yaml-mode
(add-hook 'yaml-mode-hook
          '(lambda ()
             (setq ac-sources '(ac-source-words-in-buffer))))

; for objc-mode
(require 'etags-table)
(add-to-list  'etags-table-alist
              '("\\.[mh]$" "~/.emacs.d/tags/objc.TAGS"))
(defvar ac-source-etags-table
  '((candidates . (lambda ()
         (all-completions ac-target (tags-completion-table))))
    (candidate-face . ac-candidate-face)
    (selection-face . ac-selection-face)
    (requires . 1))
  "etags をソースにする")
(add-hook 'objc-mode-hook
          (lambda ()
            (push 'ac-source-etags-table ac-sources)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial process ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(resume)
