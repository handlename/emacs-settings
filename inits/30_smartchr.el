;; INSTALL
;; (install-elisp "https://github.com/imakado/emacs-smartchr/raw/master/smartchr.el")

(global-set-key (kbd "=") (smartchr '(" = " " == " "=")))
(global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
(global-set-key (kbd "'") (smartchr '("'`!!''" "'")))
(global-set-key (kbd ">") (smartchr '("->" ">")))
(global-set-key (kbd "(") (smartchr '("(`!!')" "(")))
(global-set-key (kbd "{") (smartchr '("{ `!!' }" "{")))
(global-set-key (kbd "[") (smartchr '("[`!!']" "[")))