;; M-x install-elisp http://www.emacswiki.org/cgi-bin/wiki/download/key-chord.el
;; ref http://d.hatena.ne.jp/rubikitch/20081104/1225745862

(require 'key-chord)
(setq key-chord-two-keys-delay 0.07)
(key-chord-mode 1)

(key-chord-define-global "gk" 'describe-bindings)
(key-chord-define-global "ht" 'view-mode)

(key-chord-define-global "ue" 'magit-status)
