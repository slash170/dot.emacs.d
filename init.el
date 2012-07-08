;; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; init.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  基本設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; elispとconfディレクトリをサブディレクトリごとload-pathに追加
(add-to-load-path "elisp" "conf")
;;; conf ディレクトリ内の設定ファイルの読み込み方法の例
;; ~/.emacs.d/conf/init-anything.el というファイルを読み込む場合
;; (load "init-anything")
;; で読み込み可能です。

;; Emacsからの質問を y/n で回答する
(fset 'yes-or-no-p 'y-or-n-p)

;;; 文字コードと言語を設定する
;; 言語設定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
;; MacOSX
(require 'ucs-normalize)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;;; cua-mode: 矩形編集を強力にする
;; リージョン選択中に C-RET で矩形編集に入る、C-g で終了
(require 'cua-mode nil t)
(cua-mode t)
(setq cua-enable-cua-keys nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  画面設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 画面サイズ
(if window-system
    (progn (setq initial-frame-alist
;;		 '((width . 130) (height . 25)))))
		 '((width . 130) ))))
		 
;; color-theme
(when window-system
  (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-arjen))
;; 透明化
(set-frame-parameter nil 'alpha 90 )
;; 行番号の表示
(global-linum-mode)
;; メニューバーにファイルパスを表示する
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))
;; paren: 対応する括弧を光らせる
(setq show-paren-delay 0)
(show-paren-mode t)
;(setq show-paren-style 'expression)                    ; カッコ内の色も変更
;(set-face-background 'show-paren-match-face nil)       ; カッコ内のフェイス
;(set-face-underline-p 'show-paren-match-face "yellow") ; カッコ内のフェイス

;; タブ、全角スペースの可視化
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  キーバインド
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac の command + → でウィンドウを左右に分割
(define-key global-map (kbd "s-<right>") 'split-window-horizontally)
;; Mac の Command + ↓ でウィンドウを上下に分割
(define-key global-map (kbd "s-<down>") 'split-window-vertically)
;; Mac の Command + w で現在のウィンドを削除
(define-key global-map (kbd "s-w") 'delete-window)
;; "C-m" に newline-and-indent を割り当てる。初期値は newline
(define-key global-map (kbd "C-m") 'newline-and-indent)
;; "M-k" でカレントバッファを閉じる。初期値は kill-sentence
(define-key global-map (kbd "M-k") 'kill-this-buffer)
;; "C-t" でウィンドウを切り替える。初期値は transpose-chars
(define-key global-map (kbd "C-t") 'other-window)
;; ¥の代わりにバックスラッシュを入力する
(define-key global-map [?¥] [?\\])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  拡張機能
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; info 設定
;; (require 'info)
;; (setq Info-directory-list
;;       ;; (cons (expand-file-name "~/.emacs.d/info")
;;       (cons (expand-file-name "~/info")
;; 	    Info-directory-list))

;;; auto-install.el
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; redo+: Emacsにredoコマンドを与える
;; (install-elisp "http://www.emacswiki.org/emacs/download/redo+.el")
(when (require 'redo+ nil t)
  (define-key global-map (kbd "C-'") 'redo)) ; C-' に redo を割り当てる

;;; color-moccur: 検索結果をリストアップ
;; (install-elisp "http://www.emacswiki.org/emacs/download/color-moccur.el")
;; (install-elisp "http://www.emacswiki.org/emacs/download/moccur-edit.el")
(when (require 'color-moccur nil t)
  ;; グローバルマップにoccur-by-moccurを割り当て
  (define-key global-map (kbd "M-o") 'occur-by-moccur)
  ;; スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索のとき除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  (require 'moccur-edit nil t)
  ;; migemo 利用できる環境であれば migemo を使う
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (setq moccur-use-migemo t)))

;; grep-edit: grep から直接置換
;; (install-elisp "http://www.emacswiki.org/emacs/download/grep-edit.el")
(require 'grep-edit)

;;; undohist: 閉じたバッファも Undo できる
;; (install-elisp "http://cx4a.org/pub/undohist.el")
(when (require 'undohist nil t)
  (undohist-initialize))

;;; undo-tree: Undo の分岐を視覚化する
;; (install-elisp "http://www.dr-qubit.org/undo-tree/undo-tree.el")
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;;; point-undo: カーソル位置を Undo
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/point-undo.el")
(when (require 'point-undo nil t)
  (define-key global-map (kbd "M-[") 'point-undo)
  (define-key global-map (kbd "M-]") 'point-redo))

;;; wdiree: dired で直接ファイルをリネーム
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;; auto-complete-mode: 高機能補完+ポップアップメニュー
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;;; smartchr: サイクルスニペット
;; (install-elisp "http://github.com/imakado/emacs-smartchr/raw/master/smartchr.el")
(when (require 'smartchr nil t)
  (define-key global-map (kbd "=") (smartchr '("=" " = " " == " " === ")))
  (defun css-mode-hooks ()
    (define-key cssm-mode-map (kbd ":") (smartchr '(": " ":"))))
  
  (add-hook 'css-mode-hook 'css-mode-hooks))

;; ;;; Egg: Git フロントエンド
;; ;; (install-elisp "http://github.com/byplayer/egg/raw/master/egg.el")
;; (when (executable-find "git")
;;   (require 'egg nil t))

;; ;;; Emacsから本格的にシェルを使う
;; ;; (install-elisp "http://www.emacswiki.org/emacs/download/multi-term.el")
;; (when (require 'multi-term nil t)
;;   (setq multi-term-program "/bin/bash"))

;; ;;; Windowサイズのリサイズ
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((= c ?l)
               (enlarge-window-horizontally dx))
              ((= c ?h)
               (shrink-window-horizontally dx))
              ((= c ?j)
               (enlarge-window dy))
              ((= c ?k)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))
;; Windowサイズのリサイズ
(global-set-key "\C-c\C-r" 'window-resizer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Anything 設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間。デフォルトは0.5
   anything-idle-delay 0.3
   ;; タイプして再描写するまでの時間。デフォルトは0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示数。デフォルトは50
   anything-candidate-number-limit 100
   ;; 候補が多いときに体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    ;; root権限でアクションを実行するときのコマンド
    ;; デフォルトは"su"
    (setq anything-su-or-sudo "sudo"))

  (require 'anything-match-plugin nil t)
  (and (equal current-language-environment "Japanese")
       (executable-find "cmigemo")
       (require 'anything-migemo nil t))
  (when (require 'anything-complete nil t)
    ;; M-xによる補完をAnythingで行なう
    ;; (anything-read-string-mode 1)
    ;; lispシンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ;; describe-bindingsをAnythingに置き換える
    (descbinds-anything-install))

  (require 'anything-grep nil t)

  ;; Command+f で anything
  (define-key global-map (kbd "s-f") 'anything)
  ;; Command+y で anything-show-kill-ring
  (define-key global-map (kbd "s-y") 'anything-show-kill-ring)

  ;; ;;; anything-project: プロジェクトからファイルを絞り込み
  ;; ;; (install-elisp "http://github.com/imakado/anything-project/raw/master/anything-project.el")
  ;; (when (require 'anything-project nil t)
  ;;   (global-set-key (kbd "C-c C-f") 'anything-project)
  ;;   (setq ap:project-files-filters
  ;;         (list
  ;;          (lambda (files)
  ;;            (remove-if 'file-directory-p files)
  ;;            (remove-if '(lambda (file) (string-match-p "~$" file)) files)))))
  )

;; anything-etags+.el
(require 'anything-etags+)
(global-set-key "\M-." 'anything-etags+-select-one-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun arrow-right-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\".	c %s\",
\" 	c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"  color1 color2))

(defun arrow-left-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\".	c %s\",
\" 	c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"  color2 color1))


;; (defconst color1 "#999")
;; (defconst color2 "#555")
(defconst color1 "#922")
(defconst color2 "#955")

(defvar arrow-right-1 (create-image (arrow-right-xpm color1 color2) 'xpm t :ascent 'center))
(defvar arrow-right-2 (create-image (arrow-right-xpm color2 "None") 'xpm t :ascent 'center))
(defvar arrow-left-1  (create-image (arrow-left-xpm color2 color1) 'xpm t :ascent 'center))
(defvar arrow-left-2  (create-image (arrow-left-xpm "None" color2) 'xpm t :ascent 'center))

(setq-default mode-line-format
 (list  '(:eval (concat (propertize " %b " 'face 'mode-line-color-1)
                        (propertize " " 'display arrow-right-1)))
        '(:eval (concat (propertize " %m " 'face 'mode-line-color-2)
                        (propertize " " 'display arrow-right-2)))

        ;; Justify right by filling with spaces to right fringe - 16
        ;; (16 should be computed rahter than hardcoded)
        '(:eval (propertize " " 'display '((space :align-to (- right-fringe 17)))))

        '(:eval (concat (propertize " " 'display arrow-left-2)
                        (propertize " %p " 'face 'mode-line-color-2)))
        '(:eval (concat (propertize " " 'display arrow-left-1)
                        (propertize "%4l:%2c  " 'face 'mode-line-color-1)))
)) 

(make-face 'mode-line-color-1)
(set-face-attribute 'mode-line-color-1 nil
                    :foreground "#fff"
                    :background color1)

(make-face 'mode-line-color-2)
(set-face-attribute 'mode-line-color-2 nil
                    :foreground "#fff"
                    :background color2)

(set-face-attribute 'mode-line nil
                    :foreground "#fff"
                    :background "#000"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#fff"
                    :background "#000")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ruby-mode
(add-to-list 'load-path "~/.emacs.d/elisp/ruby-mode")
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda () (inf-ruby-keys)))

;; ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

;; rubydb
(autoload 'rubydb "rubydb3x"
  "run rubydb on program file in buffer *gud-file*.
the directory containing file becomes the initial working directory
and source-file directory for your debugger." t)

;; flymake ruby
(require 'flymake)

;; I don't like the default colors :)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()
	     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))))

(add-hook 'ruby-mode-hook
 '(lambda ()
    (define-key ruby-mode-map "\C-cd" 'flymake-display-err-menu-for-current-line)))

;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)

;; ghc-mod
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))
;;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; haskell-mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

