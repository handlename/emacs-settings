;; objc-mode

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
