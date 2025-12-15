;;  1. Package System
;;  2. Variable Definition
;;  3. Appearance (Global)
;;  4. Core Settings (Global)
;;  5. History
;;  6. Workspace
;;  7. Directory & Bookmark
;;  8. Mini Buffer
;;  9. Editing & Completion
;; 10. Functions & Keybindings (Global)
;; 11. Version
;; 12. Terminal
;; 13. Web Develop
;; 14. Org Mode
;; 15. Markdown Mode
;; 16. Emacs Lisp Mode
;; 17. AI Assistant
;; 18. Auto-generated Code
;; ---------------------------------------
;;; 1. Package System

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package
(require 'use-package)
(setq use-package-always-ensure t) ; 全てのパッケージで自動インストールを有効化(事前にpackage-refresh-contents)

;; ---------------------------------------
;;; 2. Variable Definition

;; File Variables
(defvar sheep/home-org-file (expand-file-name "~/daily/HOME.org")
  "HOME.orgのパスの定義")

;; Font Variables
(defvar sheep/fonts
  `((default  . "IBM Plex Mono")
    (japanese . "Noto Sans Mono CJK JP")
    (write-h  . "Noto Sans CJK JP")
    (table    . "PlemolJP"))
  "Font Familyの定義")

(defun sheep/font (name)
  (alist-get name sheep/fonts))

;; Color Variables
(defvar solarized-colors
  '((base03  . "#002b36")
    (base02  . "#073642")
    (base01  . "#586e75")
    (base00  . "#657b83")
    (base0   . "#839496")
    (base1   . "#93a1a1")
    (base2   . "#eee8d5")
    (base3   . "#fdf6e3")
    (yellow  . "#b58900")
    (orange  . "#cb4b16")
    (red     . "#dc322f")
    (magenta . "#d33682")
    (violet  . "#6c71c4")
    (blue    . "#268bd2")
    (cyan    . "#2aa198")
    (green   . "#859900"))
  "Solarizedカラーパレットの定義")

(defun solarized (name)
  (alist-get name solarized-colors))

(defvar sheep/colors
  `((fg         . ,(solarized 'base00))
    (bg         . ,(solarized 'base3))
    (bg-alt     . ,(solarized 'base2))
    (link       . ,(solarized 'blue))
    (h1         . ,(solarized 'orange))
    (h2         . ,(solarized 'cyan))
    (h3         . ,(solarized 'yellow))
    (h4         . ,(solarized 'violet))
    (h5         . ,(solarized 'green))
    (strong     . ,(solarized 'base01))
    (emphasis   . ,(solarized 'orange))
    (annotation . ,(solarized 'violet))
    (caption    . ,(solarized 'green))
    (cursor-jp  . ,(solarized 'orange)))
  "UI要素ごとのカラーの定義")

(defun sheep/color (name)
  (alist-get name sheep/colors))

;; ---------------------------------------
;;; 3. Appearance (Global)

(use-package solarized-theme
  :config
  (load-theme 'solarized-light t)) ; テーマの読み込み(tで読み込み時に確認メッセージなし)

(use-package emacs
  :ensure nil
  :config
  ;; Fonts & Faces
  (set-face-attribute 'default nil
                      :family (sheep/font 'default)
                      :height 109
                      :weight 'normal
                      :foreground (sheep/color 'fg)
                      :background (sheep/color 'bg))

  (set-fontset-font t 'japanese-jisx0208
                    (font-spec :family (sheep/font 'japanese))) ; 日本語(全角)のフォントの指定

  (set-face-attribute 'variable-pitch nil
                      :family (sheep/font 'write-h)
                      :height 1.0
                      :weight 'normal) ; プロポーショナルフォントの指定
  (set-face-attribute 'bold nil
                      :foreground (sheep/color 'strong)
                      :weight 'bold)
  (set-face-attribute 'italic nil
                      :foreground (sheep/color 'caption)
                      :height 0.9
                      :underline nil) ;ItalicをCaption用に流用
  (set-face-attribute 'underline nil
                      :foreground (sheep/color 'emphasis)
                      :underline nil) ;Underlineを強調用に流用

  (set-face-background 'fringe (sheep/color 'bg)) ; フリンジ背景色
  (set-face-attribute 'mode-line nil :underline nil) ; モードライン下線なし
  (set-face-attribute 'mode-line-inactive nil :underline nil) ; モードライン下線なし(非アクティブ時)
  
  ;; Frame & Window
  (add-to-list 'default-frame-alist '(fullscreen . maximized)) ; 最大化で起動
  (add-to-list 'default-frame-alist '(undecorated . t)) ; タイトルバーなし
  (add-to-list 'default-frame-alist '(internal-border-width . 22)) ; ウィンドウ内余白
  (menu-bar-mode -1) ; メニューバー非表示
  (tool-bar-mode -1) ; ツールバー非表示
  (global-display-line-numbers-mode 1) ; 行番号表示
  (scroll-bar-mode -1) ; スクロールバー非表示
  (fringe-mode 11)) ; フリンジ幅設定

;; ---------------------------------------
;;; 4. Core Settings (Global)

(use-package emacs
  :ensure nil
  :custom
  ;; Save & Revert
  (kill-ring-max 20) ; クリップボード履歴の最大数
  (backup-directory-alist '((".*" . "~/.emacs.d/backups/"))) ; バックアップ保存先
  (auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))) ; 自動保存先
  ;; Window Behavior
  (split-width-threshold 0)   ; 画面分割は常に左右分割を優先
  (split-height-threshold nil) ; 上下分割を抑制
  ;; Scrolling
  (scroll-conservatively 101) ; スクロール時の画面飛びを防止
  (scroll-margin 0) ; スクロールマージンなし
  :init
  ;; Smooth Scroll
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))) ; 精密スクロール有効化

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode) ; 起動後に自動リロード有効化
  :custom
  (global-auto-revert-non-file-buffers t) ; Dired等も自動更新
  (auto-revert-verbose nil) ; メッセージを抑制
  (auto-revert-check-vc-info t)) ; Git等のステータス変更も検知

(use-package desktop
  :ensure nil
  :config
  (add-to-list 'desktop-locals-to-save 'display-line-numbers) ; 行番号の表示設定(display-line-numbers)を保存対象リストに追加
  (desktop-save-mode 1) ; 終了時のバッファ･ウィンドウ構成を復元
  :custom
  (desktop-save t) ; 終了時に確認せずに保存
  (desktop-load-locked-desktop t) ; ロックファイルがあっても警告せずに読み込む
  (desktop-restore-eager 10) ; 起動時に即座に復元するバッファ数(残りは遅延読み込み)
  (desktop-globals-to-save nil)) ; WindowやBufferの情報のみ.emacs.desktopに保存する(savehist-modeとの競合防止)
  
;; ---------------------------------------
;;; 5. History

(use-package undo-fu
  :bind
  (([remap undo] . undo-fu-only-undo) ; undoコマンドの置き換え
   ([remap undo-only] . undo-fu-only-undo) ; [remap undo]と合わせ,Emacs標準のUndo/Redoの独特な挙動を置き換え
   ("C-z" . undo-fu-only-undo)
   ("M-z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")) ; Gitコミットメッセージ用のBufferは永続化から除外
  (setq undo-fu-session-file-limit 100) ; 保存する履歴ファイルの最大数
  (global-undo-fu-session-mode)) ; undo履歴の永続化(~/.emacs.d/undo-session/)

(use-package vundo
  :bind ("C-x u" . vundo) ; Undo Treeの可視化
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)) ; Undo Tree表示にUnicodeを使用

;; ---------------------------------------
;;; 6. Workspace

;; ---------------------------------------
;;; 7. Directory & Bookmark

(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode) ; ファイル詳細情報を非表示
  :config
  (setq dired-listing-switches "-lA")) ; lsコマンドのオプション(隠しファイル表示)

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1) ; ブックマークを~/.emacs.d/bookmarksに直ちに保存
  )

;; ---------------------------------------
;;; 8. Minibuffer

(savehist-mode) ; コマンド履歴等を永続化

(use-package vertico
  :init (vertico-mode) ; 補完UIを有効化
  :custom (vertico-count 20)) ; 候補表示数

(use-package orderless
  :custom
  (completion-styles '(orderless basic)) ; あいまい検索を有効化
  (completion-category-defaults nil) ; すべての補完にグローバル設定(上の行のorderless basic)を適用
  (completion-category-overrides '((file (styles partial-completion))))) ; ファイルパスは部分一致

(use-package consult
  :config
  (add-to-list 'consult-buffer-filter "^magit") ; バッファリストからmagit関連を除外
  :bind
  (("C-s"     . consult-line)
   ("C-c o"   . consult-outline)
   ("C-x b"   . consult-buffer)
   ("C-c 3 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("M-y"     . consult-yank-from-kill-ring)
   ("C-c m"   . consult-mark)
   ("C-x r b" . consult-bookmark)))

(use-package embark
  :bind (("C-@" . embark-act))) ; アクションメニュー呼び出し

(use-package embark-consult
  :after (embark consult)) ; Consultと連携

(use-package marginalia
  :init (marginalia-mode) ; 補完候補に詳細情報を付与
  :config
  (add-to-list 'marginalia-annotators '(file builtin))) ; ファイル候補の詳細情報は標準のものを使用

;; ---------------------------------------
;;; 9. Editing & Completion

(use-package electric
  :ensure nil
  :hook (after-init . electric-pair-mode)) ; カッコの入力補助

(use-package sis
  :config
  (sis-ism-lazyman-config "1" "2" 'fcitx5) ; Fcitx5を使用
  (sis-global-cursor-color-mode t) ; IME状態に応じたカーソル色変更
  (setq sis-other-cursor-color (sheep/color 'cursor-jp)) ; IME ON時の色
  (setq sis-prefix-override-buffer-disable-predicates (list 'minibufferp)) ; Mini Buffer以外で有効化

  ;; describe-key実行時のIME干渉を防止
  (defun sheep/sis-fix-interactive-read (orig-fun command &rest args)
    (if (memq command '(describe-key describe-key-briefly))
        (let ((sis-active sis-global-respect-mode))
          (when sis-active (sis-global-respect-mode -1))
          (unwind-protect
              (apply orig-fun command args)
            (when sis-active (sis-global-respect-mode 1))))
      (apply orig-fun command args)))
  (advice-add 'call-interactively :around #'sheep/sis-fix-interactive-read)
  
  (sis-global-respect-mode t) ; モード切替時にIME自動OFF
  (sis-global-context-mode t)) ; BufferごとのIME状態を記憶

(use-package crux
  :bind
  (("C-k"   . crux-kill-whole-line)
   ("C-a"   . crux-move-beginning-of-line)
   ("M-o"   . crux-smart-open-line)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c r" . crux-rename-file-and-buffer)
   ("C-c t" . crux-transpose-windows)))

(use-package corfu
  :init (global-corfu-mode) ; 全バッファで自動補完を有効化
  :custom
  (corfu-auto t)             ; 自動ポップアップ
  (corfu-auto-delay 0.2)     ; 表示遅延
  (corfu-auto-prefix 2)      ; 発動文字数
  (corfu-cycle t)            ; 候補リストを循環
  (corfu-preselect 'prompt)) ; 誤確定防止のためプロンプトを選択状態に

;; ---------------------------------------
;;; 10. Functions & Keybindings(Global)

(use-package emacs
  :ensure nil
  :config
  ;; Functions
  (defun sheep/copy-line ()
    "現在行をコピーする"
    (interactive)
    (beginning-of-line)
    (set-mark-command nil)
    (forward-line 1)
    (kill-ring-save (point) (mark))
    (forward-line -1)
    (deactivate-mark))
    
  (defun sheep/copy-to-end-of-line ()
    "カーソル位置から行末までをコピーする"
    (interactive)
    (kill-ring-save (point) (line-end-position)))
      
  (defun sheep/goto-home-org ()
    "HOME.orgを開く"
    (interactive)
    (find-file sheep/home-org-file))
    
  (defun sheep/open-init-file ()
    "init.elを開く"
    (interactive)
    (find-file user-init-file))
    
  (defun sheep/split-window-and-focus ()
    "画面を右に分割してフォーカスを移動する"
    (interactive)
    (let ((new-window (split-window-right)))
      (select-window new-window)))
  :bind
  (("M-k"     . kill-region)
   ("C-c k k" . kill-line)
   ("C-w"     . sheep/copy-line)
   ("C-c w w" . sheep/copy-to-end-of-line)
   ("C-c g h" . sheep/goto-home-org)
   ("C-c g s" . scratch-buffer)
   ("C-c g i" . sheep/open-init-file)
   ("C-c g p" . previous-buffer)
   ("C-c g n" . next-buffer)
   ("C-x f"   . find-file)
   ("C-x 3"   . sheep/split-window-and-focus)
   ("C-c 3 f" . find-file-other-window)
   ("C-t"     . other-window)
   ("C-x o"   . other-frame)
   ("C-c b"   . ibuffer)
   ("C-x k"   . kill-current-buffer)
   ("C-c k w" . kill-buffer-and-window)
   ("M-["     . beginning-of-buffer)
   ("M-]"     . end-of-buffer)
   ("M-l"     . visual-line-mode)
   ("M-r"     . read-only-mode)
   ("C-h"     . mark-sexp)
   ("M-n"     . forward-sexp)
   ("M-m"     . kill-sexp)
   ("M-p"     . backward-sexp)
   ("M-@"     . backward-kill-sexp)
   ("M-h"     . mark-word)
   ("M-j"     . backward-kill-word))
  :init
  (define-key key-translation-map (kbd "C-j") (kbd "<DEL>"))) ; C-jをBackSpaceに割り当て

;; ---------------------------------------
;;; 11. Version

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1) ; 現在のウィンドウで開く
  (magit-bury-buffer-function 'magit-mode-quit-window) ; 終了時にバッファを削除
  (magit-log-section-commit-count 50) ; Recent commitsの表示数
  :hook
  (magit-status-mode . sis-set-english) ; Magit起動時にIMEをOFFにする
  :config
  (advice-add 'magit-commit-diff :override #'ignore) ; コミット時にDiffを表示しない
  :bind ("C-x g" . magit-status))

;; ---------------------------------------
;;; 12. Terminal

(use-package vterm
  :commands vterm
  :hook
  (vterm-mode . (lambda ()
                  (display-line-numbers-mode -1) ; 行番号を非表示
		  (sis-set-english) ; 起動時にIMEをOFF
		  ))
  :config
  (setq vterm-max-scrollback 10000) ; スクロールバックの行数
  :bind
  (("C-x v"   . vterm)
   ("C-c 3 v" . vterm-other-window)
   :map vterm-mode-map
   ("C-a"   . vterm-beginning-of-line)
   ("C-e"   . vterm-end-of-line)
   ("C-M-p" . vterm-previous-prompt)
   ("C-M-n" . vterm-next-prompt)
   ("C-y"   . vterm-yank)
   ("M-y"   . vterm-yank-pop)
   ("C-t"   . other-window)
   ("C-z"   . vterm-undo)
   ("C-w"   . vterm-copy-mode)))

;; ---------------------------------------
;;; 13. Web Develop
;; (将来的なWeb開発設定用に予約)

;; ---------------------------------------
;;; 14. Org Mode

(use-package org
  :ensure nil
  :hook
  (org-mode . (lambda ()
                (display-line-numbers-mode -1))) ; 行番号を非表示
  :custom
  (org-hide-emphasis-markers t) ; マークアップ記号を隠す
  (org-use-speed-commands t)    ; スピードコマンドの有効化
  (org-startup-indented t)      ; コンテンツを自動インデント
  (org-indent-indentation-per-level 4) ; インデント幅
  (org-startup-with-inline-images t); 画像をインライン表示
  (org-image-actual-width nil)      ; 画像サイズ自動調整を無効化
  (org-image-max-width 600)         ; 画像の最大幅
  (org-src-preserve-indentation t)  ; ソースブロックのインデントを保持
  (org-fontify-todo-headline t)     ; TODO行全体を色付け
  
  :config
  (require 'org-tempo) ; <s TAB 等でのテンプレート挿入を有効化

  ;; Faces
  (let ((colors (list (sheep/color 'h1)
                      (sheep/color 'h2)
                      (sheep/color 'h3)
                      (sheep/color 'h4)
                      (sheep/color 'h5)
                      (sheep/color 'fg)
                      (sheep/color 'fg)
                      (sheep/color 'fg))))
    (dotimes (i 8)
      (set-face-attribute
       (intern (format "org-level-%d" (1+ i))) nil
       :foreground (nth i colors)
       :height 1.0
       :weight 'normal))) ; 色の定義リストを作成し,org-level(1-8)に割り当て
       
  (set-face-attribute 'org-link nil
                      :foreground (sheep/color 'link)
                      :weight 'normal
                      :underline t)
  (set-face-attribute 'org-verbatim nil
                      :foreground (sheep/color 'annotation))
  (set-face-attribute 'org-table nil
                      :family (sheep/font 'table)
                      :height 0.98)

  ;; Code Block Faces
  (set-face-attribute 'org-block nil
                      :background (sheep/color 'bg-alt)
                      :height 0.97)
  (set-face-attribute 'org-block-begin-line nil
                      :background (sheep/color 'bg)
                      :height 0.8
                      :underline nil)
  (set-face-attribute 'org-block-end-line nil
                      :background (sheep/color 'bg)
                      :height 0.8
                      :overline nil)
  (set-face-attribute 'org-code nil
                      :foreground (sheep/color 'fg)
                      :background (sheep/color 'bg-alt))

  ;; Tasklist Faces
  (setq org-todo-keyword-faces
        `(("TODO" . (:foreground ,(sheep/color 'emphasis) :weight normal))
          ("DONE" . (:foreground ,(sheep/color 'link) :weight normal))))
  (set-face-attribute 'org-headline-todo nil
                      :foreground (sheep/color 'annotation))
  (set-face-attribute 'org-headline-done nil
                      :foreground (sheep/color 'fg))

  ;; Functions
  (defun sheep/org-insert-heading-eol ()
    "行末で見出しを追加"
    (interactive)
    (end-of-line)
    (org-insert-heading))
    
  (defun sheep/org-insert-item-eol ()
    "行末でリストアイテムを追加"
    (interactive)
    (end-of-line)
    (org-meta-return))
    
  (defun sheep/org-kill-element ()
    "要素全体を削除"
    (interactive)
    (let ((beg (point)))
      (org-mark-element)
      (kill-region (region-beginning) (region-end))))
      
  (defun sheep/org-backward-kill-element ()
    "前の要素全体を削除"
    (interactive)
    (let ((end (point)))
      (org-backward-element)
      (kill-region (point) end)))
      
  :bind
  (:map org-mode-map
        ("C-S-<return>" . sheep/org-insert-heading-eol)
        ("M-<return>"   . sheep/org-insert-item-eol)
        ("M-S-<return>" . org-meta-return)
        ("C-M-y"        . org-metaup)
        ("C-M-u"        . org-metadown)
        ("C-M-h"        . org-metaleft)
        ("C-M-j"        . org-metaright)
        ("C-M-n"        . org-shiftleft)
        ("C-M-m"        . org-shiftright)
        ("M-Y"          . org-shiftmetaup)
        ("M-U"          . org-shiftmetadown)
        ("M-H"          . org-shiftmetaleft)
        ("M-J"          . org-shiftmetaright)
        ;; Overwrite
        ("C-h"     . org-mark-element)
        ("M-n"     . org-forward-element)
        ("M-m"     . sheep/org-kill-element)
        ("M-p"     . org-backward-element)
        ("M-@"     . sheep/org-backward-kill-element)
        ("M-h"     . mark-word)))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode) ; カーソル位置のマークアップを表示
  :config
  (setq org-appear-autoentities t) ; 特殊文字の自動表示を有効化
  (setq org-appear-autolinks t)) ; リンクの自動表示を有効化

;; ---------------------------------------
;;; 15. Markdown Mode

(use-package markdown-mode
  :mode
  ("\\.md\\'" . markdown-mode) ; .mdファイルに関連付け
  :defer t)

;; ---------------------------------------
;;; 16. Emacs Lisp Mode

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (outline-minor-mode 1) ; アウトラインモード有効化
            (setq-local outline-regexp "^;;;"))) ; ;;;を見出しとして扱う

;; ---------------------------------------
;;; 17. AI Assistant 

(use-package gptel
  :config
  (setq gptel-model 'gemini-2.5-flash) ; デフォルトモデル
  (setq gptel-system-message "あなたは,私(質問者)のことを｢羊さん｣と呼んで下さい.
    公式のドキュメントや信頼性の高いソースから最新の情報を収集して下さい
    私(質問者)がUbuntu 24.04 LTS(GNOME,Wayland)ユーザーであることを考慮して下さい")
  (setq gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          (emacs . "あなたはGNU Emacsをメインエディタとして使用するシニアエンジニアであり,Web開発のエキスパートです.
            SpacemacsやDoom Emacsは使用せず,プレーンなEmacsに厳選したパッケージを組み合わせることを好みます.
            最新のEmacs 30.2環境を生かして,Apheleia,gptel,Vertico,Consult,Corfuなど,モダンなパッケージを積極的に導入しています.ただしレガシーであっても,MagitやDiredなど,今なお強力なパッケージは使い続けています.
            あなたは,新人エンジニアの羊さんを教育します.最新技術に加え,シェルスクリプト･正規表現･Makefileといった基本技術も適宜教えます")
          (linux . "あなたはLinuxとUbuntuのエキスパートであり,CLIにもGUI(GNOME)にも精通したシニアエンジニアであり,新人Webエンジニアである羊さんのメンターです")))
  (setq gptel-track-media t) ; メディア(画像など)をプロンプトに含める
  (setq gptel-include-reasoning nil) ; 推論･過程をBufferに表示させない
  
  ;; Backends
  (gptel-make-gemini "Gemini-Pro"
    :key (lambda () (getenv "GEMINI_API_KEY_PRO"))
    :stream t
    :models '(gemini-2.5-flash
              gemini-2.5-pro
              gemini-3-pro-preview))
              
  (setq gptel-backend
        (gptel-make-gemini "Gemini-Free"
          :key (lambda () (getenv "GEMINI_API_KEY_FREE"))
          :stream t
          :models '(gemini-2.5-flash
                    gemini-2.5-pro)))
                    
  ;; Custom Functions
  (defun sheep/create-gptel-buffer ()
    "Gptel用の使い捨てバッファを作成"
    (interactive)
    (let* ((base-name "*Gptel*")
           (buffer-name (generate-new-buffer-name base-name))
           (buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
        (erase-buffer)
        (markdown-mode)
        (gptel-mode)
        (set-buffer-modified-p nil)
        (insert "### "))
      (switch-to-buffer buf)
      (message "Temporary buffer '%s' created." buffer-name)))
      
  (defun sheep/gptel-send-with-separator ()
    "区切り線を入れて送信"
    (interactive)
    (insert "\n\n---")
    (call-interactively 'gptel-send))
    
  (defun sheep/erase-buffer-insert-heading ()
    "バッファをクリアして見出しを挿入"
    (interactive)
    (erase-buffer)
    (insert "### "))
    
  :bind
  ("C-c a a" . sheep/create-gptel-buffer)
  ("C-c 3 a" . gptel)
  ("C-c a d" . gptel-add)
  ("C-c a f" . gptel-add-file)
  ("C-c a p" . gptel-system-prompt)
  ("C-c a n" . gptel-end-of-response)
  ("C-c a k" . gptel-context-remove-all)
  ("C-c a e" . sheep/erase-buffer-insert-heading)
  ("C-c a m" . gptel-menu)
  ("C-c a g" . gptel-mode)
  ("C-c <return>"   . sheep/gptel-send-with-separator)
  ("C-c a <return>" . gptel-send))

;; ---------------------------------------
;;; 18.Auto-generated Code

(setq custom-file (locate-user-emacs-file "auto-generated.el")) ; Emacsが自動記述する内容をinit.elから分離
(when (file-exists-p custom-file)
  (load custom-file)) ; 分離したファイルの読込
