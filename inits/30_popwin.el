;; INSTALL
;; (install-elisp "https://github.com/m2ym/popwin-el/raw/v0.2/popwin.el")

(if (require 'popwin nil t)
    (progn
      (setq display-buffer-function 'popwin:display-buffer)
      (setq popwin:popup-window-height 0.4)
      (setq anything-samewindow nil)
      (push '("\\*anything-?" :regexp t) popwin:special-display-config)
      (push '(dired-mode :position top) popwin:special-display-config)
      (push '("\\*[Vv][Cc]" :regexp t) popwin:special-display-config)
      (push '("\\*git-" :regexp t) popwin:special-display-config)
      (push '("*image-dired*") popwin:special-display-config)
      ))
