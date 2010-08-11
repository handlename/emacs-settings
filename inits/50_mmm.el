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

;; ;; php + html + js + css
;; ;;(add-to-list 'auto-mode-alist '("\\.php?\\'" . html-helper-mode))
;; (mmm-add-classes
;;  '((mmm-php
;;     :submode php-mode
;;     :front "<\\?\\(php\\)?"
;;     :back "\\?>")))
;; ;; (mmm-add-mode-ext-class nil "\\.php\\'" 'mmm-php-in-html)
;; (mmm-add-mode-ext-class nil "\\.php\\'" 'mmm-css)
;; ;; (mmm-add-mode-ext-class nil "\\.php\\'" 'mmm-js-in-html)
;; (mmm-add-mode-ext-class nil "\\.yml\\'" 'mmm-php)
;; ;; インデントが効かなくなるのを解消
;; (defun save-mmm-c-locals ()
;;   (with-temp-buffer
;;     (php-mode)
;;     (dolist (v (buffer-local-variables))
;;       (when (string-match "\\`c-" (symbol-name (car v)))
;;         (add-to-list 'mmm-save-local-variables `(,(car v) nil
;;                                                  ,mmm-c-derived-modes))))))
;; (save-mmm-c-locals)

;; yaml + php
(mmm-add-classes
 '((mmm-php-in-yaml
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")))
(mmm-add-mode-ext-class nil "\\.yml?\\'" 'mmm-php-in-yaml)

;; color
(add-hook 'mmm-mode-hook
          '(lambda ()
             (set-face-background 'mmm-default-submode-face "#404040")))
