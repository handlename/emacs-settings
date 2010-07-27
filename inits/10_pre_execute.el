;; 起動前に実行する処理

(require 'cl)
(setq byte-compile-warnings '(free-vars unresolved callargs redefine obsolete noruntime cl-functions interactive-only make-local))
