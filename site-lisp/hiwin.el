(defvar hiwin-color       "#101015")  ;; 非アクティブwindowの背景色
(defvar hiwin-overlay-num  8)        ;; 非アクティブwindowのoverlay数
(defvar hiwin-face         nil)      ;; 非アクティブwindow用face
(defvar hiwin-overlay      nil)      ;; 非アクティブwindow用overlay
(defvar hiwin-window       nil)      ;; アクティブwindowのwindow
(defvar hiwin-buffer       nil)      ;; アクティブwindowのbuffer
(defvar hiwin-ignore-buffer '("+draft/1" "+draft/2" "+draft/3"))

(defun hiwin-init ()
  ;; 非アクティブwindow用faceを作成
  (make-face 'hiwin-face)
  ;; 非アクティブwindow用faceの背景色を設定
  (set-face-background 'hiwin-face hiwin-color)
  (let ((num 0)      ;; カウンタ
        (buf nil))   ;; 作業用バッファ
    ;; 作業用バッファを作成
    (setq buf (get-buffer-create "*hiwin-temp*"))
    ;; 作成するoverlay分を処理（ループ開始）
    (while (< num hiwin-overlay-num)
      ;; 非アクティブwindow用overlayを作成
      (setq hiwin-overlay (cons (make-overlay 1 1 buf nil t) hiwin-overlay))
      ;; 非アクティブwindow用overlayの本文のfaceを設定
      (overlay-put (nth 0 hiwin-overlay) 'face 'hiwin-face)
      ;; 非アクティブwindow用overlayのEOFのfaceを設定
      (overlay-put (nth 0 hiwin-overlay) 'after-string
                   (propertize (make-string 100 ?\n) 'face 'hiwin-face))
      ;; カウンタアップ
      (setq num (1+ num))
      ) ;; （ループ終了）
    ;; 作業用バッファを削除
    (kill-buffer buf)
    )
  )

(defun hiwin-load ()
  ;; アクティブwindowのwindowを取得
  (setq hiwin-window (selected-window))
  ;; アクティブwindowのbufferを取得
  (setq hiwin-buffer (current-buffer))
  (let ((num 0)                         ;; カウンタ
        (target-window nil)             ;; 処理対象window
        (target-list (window-list)))    ;; 表示windowのリスト
    ;; 表示winndowのすべてを処理（ループ開始）
    (while target-list
      ;; 処理対象windowを取得
      (setq target-window (car target-list))
      ;; 処理対象windowをリストから削除
      (setq target-list (cdr target-list))
      ;; 処理対象windowとアクティブwindowが一致する場合
      (if (eq target-window hiwin-window)
          ;; EOB一つ前の場合，一文字進む
          (progn (if (eq (point) (1- (point-max))) (forward-char 1) ) )
        ;; 処理対象windowとアクティブwindowが一致しない場合
        (progn
          (let ((buf (window-buffer target-window)))
            (if (member buf hiwin-ignore-buffer)
                ()
              ;; 処理対象windowをアクティブ化
              (select-window target-window)
              ;; EOBの場合，一文字戻る
              (if (eq (point) (point-max)) (backward-char 1) )
              ;; 処理対象windowにoverlayを設定
              (move-overlay (nth num hiwin-overlay)
                            (point-min) (point-max) (current-buffer))
              (overlay-put (nth num hiwin-overlay) 'window target-window)
              ;; カウンタアップ
              (setq num (1+ num))
              )
            )
          )
        )
      ) ;; （ループ終了）
    ;; アクティブwindowをアクティブ化
    (select-window hiwin-window)
    )
  )

(defun hiwin-unload ()
  (let ((num 0))   ;; カウンタ
    ;; 作成されたoverlay分を処理（ループ開始）
    (while (< num hiwin-overlay-num)
      ;; 非アクティブwindow用overlayを削除
      (delete-overlay (nth num hiwin-overlay))
      ;; カウンタアップ
      (setq num (1+ num))
      )) ;; （ループ終了）
  ;; 非アクティブwindow用overlayの変数を初期化
  (setq hiwin-overlay nil)
  )

(defun hiwin-highlight-window ()
  ;; アクティブwindowがミニバッファのウィンドウの場合，
  ;; あるいは別のウィンドウの場合に処理
  (unless (or (eq (selected-window) (minibuffer-window))
              (and (eq hiwin-window (selected-window))
                   (eq hiwin-buffer (current-buffer))))
    ;; 非アクティブwindow用overlayの変数がnullの場合，初期化
    (if (null hiwin-overlay) (hiwin-init))
    ;; 非アクティブwindowのoverlayを設定
    (hiwin-load)
    )
  )

(defun hiwin-mode ()
  (interactive)
  (if (not (null hiwin-overlay))
      (progn (remove-hook 'post-command-hook 'hiwin-highlight-window)
             (hiwin-unload))
    (add-hook 'post-command-hook 'hiwin-highlight-window)
    (hiwin-highlight-window)))

(defadvice split-window-vertically (around split-window-vertically-around)
  (interactive) ad-do-it (hiwin-load) )
(ad-activate 'split-window-vertically)

(defadvice split-window-horizontally (around split-window-horizontally)
  (interactive) ad-do-it (hiwin-load) )
(ad-activate 'split-window-horizontally)

(defadvice delete-window (around delete-window)
  (interactive) ad-do-it (hiwin-mode) (hiwin-mode))
(ad-activate 'delete-window)

(defadvice recenter (around recenter-around)
  (interactive) (progn ad-do-it (hiwin-load)) )
(ad-activate 'recenter)

;(hiwin-mode)
