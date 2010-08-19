;; INSTALL
;; (install-elisp-from-emacswiki "igrep.el")
;; (install-elisp-from-emacswiki "grep-edit.el")

(require 'igrep)
(require 'grep-edit)
(igrep-define lgrep (igrep-use-zgrep nil) (igrep-regex-option "-n -0u8"))
(igrep-find-define lgrep (igrep-use-zgrep nil) (igrep-read-options "-n -0u8"))
