;; INSTALL
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/key-chord.el")
;; ref http://d.hatena.ne.jp/rubikitch/20081104/1225745862

(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)

(key-chord-define-global "_=" 'open-junk-file)
(key-chord-define-global "s-" 'org-capture)
(key-chord-define-global "ao" 'whitespace-cleanup)
(key-chord-define-global ",'" 'align)
