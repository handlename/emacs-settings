;; undo-tree
;; INSTALL
;; (install-elisp "http://www.dr-qubit.org/undo-tree/undo-tree.el")
(require 'undo-tree)
(global-undo-tree-mode)


;; undohist
;; INSTALL
;;(install-elisp "http://cx4a.org/pub/undohist.el")

;; (require 'undohist)
;; (undohist-initialize)


;; point-undo
;; INSTALL
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/point-undo.el")
(require 'point-undo)
(define-key global-map (kbd "C-=") 'point-undo)
(define-key global-map (kbd "C-+") 'point-redo)
