(load "elscreen" "ElScreen" t)
(setq elscreen-display-tab nil) ; タブを非表示
(define-key global-map (kbd "M-t") 'elscreen-next)
(define-key global-map (kbd "M-C-t") 'elscreen-create)
(define-key global-map (kbd "M-C-w") 'elscreen-kill)
