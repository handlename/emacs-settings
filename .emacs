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
        (set-face-attribute 'default nil
                            :family "M+2VM+IPAG circle"
                            :height 140)
        (set-fontset-font
         (frame-parameter nil 'font)
         'japanese-jisx0208
         '("M+2VM+IPAG circle" . "iso10646-1"))
        (set-fontset-font
         (frame-parameter nil 'font)
         'japanese-jisx0212
         '("M+2VM+IPAG circle" . "iso10646-1"))
        (set-fontset-font
         (frame-parameter nil 'font)
         'mule-unicode-0100-24ff
         '("M+2VM+IPAG circle" . "iso10646-1"))
        (setq face-font-rescale-alist
              '(("^-apple-hiragino.*"                . 1.3)
                (".*osaka-bold.*"                    . 1.2)
                (".*osaka-medium.*"                  . 1.2)
                (".*courier-bold-.*-mac-roman"       . 1.0)
                (".*monaco cy-bold-.*-mac-cyrillic"  . 0.9)
                (".*monaco-bold-.*-mac-roman"        . 0.9)
                ("-cdac$"                            . 1.3))))
      ;(ns-toggle-fullscreen)
      (setq ns-pop-up-frames nil)
      ))


;;
;; Color
;;______________________________________________________________________

(set-foreground-color                                  "#333333") ; 文字色
(set-background-color                                  "#EEEEEE") ; 背景色
(set-cursor-color                                      "#FF69B4") ; カーソル色
(set-face-background 'region                           "#B0C4DE") ; リージョン
(set-face-foreground 'modeline                         "#EEEEEE") ; モードライン文字
(set-face-background 'modeline                         "#333333") ; モードライン背景
(set-face-foreground 'mode-line-inactive               "#EEEEEE") ; モードライン文字(非アクティブ)
(set-face-background 'mode-line-inactive               "#888888") ; モードライン背景(非アクティブ)
(set-face-foreground 'font-lock-comment-delimiter-face "#888888") ; コメントデリミタ
(set-face-foreground 'font-lock-comment-face           "#888888") ; コメント
(set-face-foreground 'font-lock-string-face            "#808000") ; 文字列
(set-face-foreground 'font-lock-function-name-face     "#006400") ; 関数名
(set-face-foreground 'font-lock-keyword-face           "#FF4C00") ; キーワード
(set-face-foreground 'font-lock-constant-face          "#800000") ; 定数(this, selfなども)
(set-face-foreground 'font-lock-variable-name-face     "#4169E1") ; 変数
(set-face-foreground 'font-lock-type-face              "#0000CD") ; クラス
(set-face-foreground 'fringe                           "#C0C0C0") ; fringe(折り返し記号なでが出る部分)
(set-face-background 'fringe                           "#EEEEEE") ; fringe

(add-hook 'org-mode-hook
          '(lambda ()
             (set-face-foreground 'org-level-3 "#000080")
             (set-face-foreground 'org-level-4 "#227722")
             (set-face-foreground 'org-hide "#EEEEEE")
             (set-face-foreground 'org-done "#EEEEEE")
             (set-face-background 'org-done "#328832")
             (set-face-foreground 'org-todo "#EEEEEE")
             (set-face-background 'org-todo "#FF0000")))

(add-hook 'mmm-mode-hook
          '(lambda ()
             (set-face-background 'mmm-default-submode-face "#E5E5E5")))


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

(defun make-backup-file-name (filename)
  (expand-file-name
   (concat "~/.backup/" (file-name-nondirectory filename) "~")
   (file-name-directory filename)))


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

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 行番号表示
(global-linum-mode)
(setq linum-format "%5d")

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
;; anything
;;______________________________________________________________________

(require 'anything-config)

(define-key anything-map (kbd "C-M-n") 'anything-next-source)
(define-key anything-map (kbd "C-M-p") 'anything-previous-source)


;; Don't record histories. If not, anything get error
(remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)
(ad-disable-advice 'anything-exit-minibuffer 'before 'anything-c-adaptive-exit-minibuffer)
(ad-disable-advice 'anything-select-action 'before 'anything-c-adaptive-select-action)
(setq anything-c-adaptive-history-length 0)

;; Serach for current buffer
(defun anything-for-occur ()
  "Search current buffer in anything"
  (interactive)
  (anything '(anything-c-source-occur)))
(define-key global-map (kbd "C-s") 'anything-for-occur)

;; buffer list + buffer history + files in current directory
(defun anything-for-buffers ()
  "Open buffer list in anything"
  (interactive)
  (anything '(anything-c-source-buffers
              anything-c-source-file-name-history
              anything-c-source-files-in-current-dir)))
(define-key global-map (kbd "C-x b") 'anything-for-buffers)

;; calc
(defun anything-for-calc ()
  "Calculate in anything"
  (interactive)
  (anything '(anything-c-source-calculation-result)))
(define-key global-map (kbd "C-M-c") 'anything-for-calc)

;; killring history
(define-key global-map (kbd "C-M-y") 'anything-show-kill-ring)

;; emacs commands
(defun anything-for-emacs-commands ()
  "Execute emacs commands in anything"
  (interactive)
  (anything '(anything-c-source-emacs-commands)))
(define-key global-map (kbd "M-x") 'anything-for-emacs-commands)

;; tag jump
(require 'anything-etags)
(require 'anything-gtags)
(defun anything-etags-and-gtags-select ()
  "Tag jump using etags, gtags and `anything'."
  (interactive)
  (let* ((initial-pattern (regexp-quote (or (thing-at-point 'symbol) ""))))
    (anything (list anything-c-source-gtags-select
                    anything-c-source-etags-select))
    "Find Tag: " nil))
(define-key global-map (kbd "C-x t") 'anything-etags-and-gtags-select)


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
;(set-face-background 'highlight-current-line-face "#597B32")
(set-face-background 'highlight-current-line-face "#FFFFFF")

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
             (setq php-manual-path "/usr/share/doc/php/html")
             (setq php-search-url  "http://www.phppro.jp/")
             (setq php-manual-url  "http://www.phppro.jp/phpmanual")
             (setq tab-width 4)
             (setq c-basic-offset 4)))


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
 '((mmm-css-in-html
    :submode css-mode
    :front "<style[^>]*>"
    :back "</style>")))
(mmm-add-mode-ext-class nil "\\.html?\\'" 'mmm-css-in-html)

;; html + js
(mmm-add-classes
 '((mmm-js-in-html
    :submode js-mode
    :front "<script[^>]*>[^<]"
    :front-offset -1
    :back "\n?[ \t]*</script>")))
(mmm-add-mode-ext-class nil "\\.html?\\'" 'mmm-js-in-html)

;; php + html + js + css
;;(add-to-list 'auto-mode-alist '("\\.php?\\'" . html-helper-mode))
(mmm-add-classes
 '((mmm-php-in-html
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")))
(mmm-add-mode-ext-class nil "\\.php?\\'" 'mmm-php-in-html)
(mmm-add-mode-ext-class nil "\\.php?\\'" 'mmm-css-in-html)
(mmm-add-mode-ext-class nil "\\.php?\\'" 'mmm-js-in-html)
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
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-set-key (kbd "M-_") 'auto-complete)
(setq ac-auto-start 1)       ; 補完を開始する文字数
(setq ac-auto-show-menu 0.2) ; 補完リストが表示されるまでの時間
;(set-face-background 'ac-candidate-face "lightyellow")
(setq ac-sources '(ac-source-yasnippet
                   ac-source-dictionary
                   ac-source-filename
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial process ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(resume)
