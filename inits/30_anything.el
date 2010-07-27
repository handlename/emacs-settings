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
