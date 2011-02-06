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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  拡張機能
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

