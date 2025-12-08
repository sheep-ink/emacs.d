;; 1. Package System
;; 2. Variable Definition
;; 3. Appearance (Global > Per-package)
;; 4. Functionality (Global)
;; 5. Functionality (Per-package)
;; 6. AI Assistant
;; 7. Auto-generated Code
;; ---------------------------------------
;;; 1. Package System

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package
(require 'use-package)
(setq use-package-always-ensure t) ;全ての`use-package`に`:ensure t`(Package Auto Install)適用.事前にpackages Listのrefreshが必要

;; ---------------------------------------
;;; 2-1. File Variables

(defvar sheep/home-org-file (expand-file-name "~/configs/HOME.org"))

;; ---------------------------------------
;;; 2-2. Font Variables

(defvar sheep/fonts
  `((default  . "IBM Plex Mono")
    (japanese . "Noto Sans Mono CJK JP")
    (write-h  . "Noto Sans CJK JP")
    (table    . "PlemolJP")))

(defun sheep/font (name)
    (alist-get name sheep/fonts))

;; ---------------------------------------
;;; 2-3. Color Variables

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
    (green   . "#859900")))

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
    (cursor-jp  . ,(solarized 'orange))))

(defun sheep/color (name)
  (alist-get name sheep/colors))

;; ---------------------------------------
;;; 3-1. Theme

(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))

;; ---------------------------------------
;;; 3-2. Faces(Global)

(set-face-attribute 'default nil
                    :family (sheep/font 'default)
                    :height 109
		    :weight 'normal
                    :foreground (sheep/color 'fg)
                    :background (sheep/color 'bg))

(set-fontset-font t 'japanese-jisx0208
		  (font-spec :family (sheep/font 'japanese)))

(set-face-attribute 'variable-pitch nil
                    :family (sheep/font 'write-h)
                    :height 1.0
		    :weight 'normal)
(set-face-attribute 'bold nil
                    :foreground (sheep/color 'strong)
                    :weight 'bold)
(set-face-attribute 'italic nil
		    :foreground (sheep/color 'caption)
                    :height 0.9
                    :underline nil)
(set-face-attribute 'underline nil
                    :foreground (sheep/color 'emphasis)
                    :underline nil)

;; Interface Elements
(set-face-background 'fringe (sheep/color 'bg))
(set-face-attribute 'mode-line nil :underline nil)
(set-face-attribute 'mode-line-inactive nil :underline nil)

;; ---------------------------------------
;;; 3-3. Faces(Org Mode)

(with-eval-after-load 'org
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
       :weight 'normal)))
  (set-face-attribute 'org-link nil
		      :foreground (sheep/color 'link)
                      :weight 'normal
                      :underline t)
  (set-face-attribute 'org-verbatim nil
		      :foreground (sheep/color 'annotation))
  (set-face-attribute 'org-table nil
		      :family (sheep/font 'table)
		      :height 0.98)

  ;; Code
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
		      :background (sheep/color 'bg-alt))

  ;; Tasklist
  (setq org-todo-keyword-faces
	`(("TODO" . (:foreground ,(sheep/color 'emphasis) :weight normal))
	  ("DONE" . (:foreground ,(sheep/color 'link) :weight normal))))
  (set-face-attribute 'org-headline-todo nil
		      :foreground (sheep/color 'annotation))
  (set-face-attribute 'org-headline-done nil
		      :foreground (sheep/color 'fg)))

;; ---------------------------------------
;;; 4-1. Save & Backup

(setq kill-ring-max 20)
(global-auto-revert-mode t) ;別ブランチでチェックアウト時など,ファイルの変更を自動更新
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))

;; ---------------------------------------
;;; 4-2. History & Version

(use-package undo-fu
  :bind
  (([remap undo] . undo-fu-only-undo)
  ([remap undo-only] . undo-fu-only-undo)
  ("C-z" . undo-fu-only-undo)
  ("M-z" . undo-fu-only-redo)))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (global-undo-fu-session-mode))

(use-package vundo
  :bind
  ("C-x u" . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1) ;現在のウィンドウに表示
  :bind
  ("C-x g" . magit-status))

;; ---------------------------------------
;;; 4-3. Frame & Window

(setq initial-buffer-choice sheep/home-org-file)
(setq split-width-threshold 0) ;自動で開くBufferは常に水平分割で表示
(setq split-height-threshold nil) ;自動で開くBufferの垂直分割での表示を禁止
(push '(fullscreen . maximized) default-frame-alist)
(add-to-list 'default-frame-alist '(undecorated . t)) ;タイトルバーなし
(add-to-list 'default-frame-alist '(internal-border-width . 22))
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode 1)
(scroll-bar-mode -1)
(fringe-mode 11)

;; Smooth Scroll 
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(setq scroll-conservatively 101)
(setq scroll-margin 0)

;; ---------------------------------------
;;; 4-4. Workspace & Directory

(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode) ;詳細情報は非表示
  :config
  (setq dired-listing-switches "-lA")) ;隠しファイルを表示

;; ---------------------------------------
;;; 4-5. Minibuffer

(savehist-mode)

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-count 20))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind
  (("C-s"     . consult-line)
   ("C-c o"   . consult-outline)
   ("C-x b"   . consult-buffer)
   ("C-c 3 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("M-y"     . consult-yank-from-kill-ring)
   ("C-c m"   . consult-mark)))

(use-package embark
  :bind
  (("C-@" . embark-act)))

(use-package embark-consult
  :after (embark consult))

(use-package marginalia
  :init
  (marginalia-mode)
  :config
  (add-to-list 'marginalia-annotators '(file builtin))) ;ファイルの候補リストでは詳細情報を非表示

;; ---------------------------------------
;;; 4-6. Editing & Completion

(use-package sis
  :config
  (sis-ism-lazyman-config "1" "2" 'fcitx5)
  (sis-global-cursor-color-mode t)
  (setq sis-other-cursor-color (sheep/color 'cursor-jp)) ;IME ON時の色
  (setq sis-prefix-override-buffer-disable-predicates (list 'minibufferp)) ;*で始まるbufferでもsisを有効化

  ;; describe-key の引数読み取り(interactive)が始まる前に sis を止める
  (defun sheep/sis-fix-interactive-read (orig-fun command &rest args)
    (if (memq command '(describe-key describe-key-briefly))
        (let ((sis-active sis-global-respect-mode))
          (when sis-active (sis-global-respect-mode -1))
          (unwind-protect
              (apply orig-fun command args)
            (when sis-active (sis-global-respect-mode 1))))
      (apply orig-fun command args)))
  (advice-add 'call-interactively :around #'sheep/sis-fix-interactive-read)
  
  (sis-global-respect-mode t) ;IMEの自動オフ
  (sis-global-context-mode t)) ;バッファ切替え時にIMEの状態を記憶

(use-package crux
  :bind
  (("C-k"   . crux-kill-whole-line)
   ("C-a"   . crux-move-beginning-of-line)
   ("M-o"   . crux-smart-open-line)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c r" . crux-rename-file-and-buffer)
   ("C-c t" . crux-transpose-windows)))

(use-package corfu
  :init
  (global-corfu-mode) ; 全バッファでCorfuを有効化
  :custom
  (corfu-auto t)          ; 自動的にポップアップ表示
  (corfu-auto-delay 0.2)  ; 0.2秒後に表示
  (corfu-auto-prefix 2)   ; 2文字入力で発動
  (corfu-cycle t)         ; 候補リストの端でループ
  (corfu-preselect 'prompt)) ; 候補の一番上ではなく、入力行を選択状態にする(誤確定防止)

;; ---------------------------------------
;;; 4-7. Functions & Keybindings (Global)

(use-package emacs
  :ensure nil
  :config
  ;; Functions
  (defun sheep/copy-line ()
    (interactive)
    (beginning-of-line)
    (set-mark-command nil)
    (forward-line 1)
    (kill-ring-save (point) (mark))
    (forward-line -1)
    (deactivate-mark))
  (defun sheep/copy-to-end-of-line ()
    (interactive)
    (if (eolp)
	(message "End of line")
      (kill-ring-save (point) (line-end-position))))
  (defun sheep/goto-home-org ()
    (interactive)
    (find-file sheep/home-org-file))
  (defun sheep/open-init-file ()
    (interactive)
    (find-file user-init-file))
  (defun sheep/split-window-and-focus ()
    (interactive)
    (let ((new-window (split-window-right)))
      (select-window new-window)))
  :bind
  (("M-k"     . kill-region)
   ("C-c C-k" . kill-line)
   ("C-w"     . sheep/copy-line)
   ("C-c C-w" . sheep/copy-to-end-of-line)
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
   ("C-x s"   . save-buffer)
   ("C-c s a" . save-some-buffers)
   ("C-c s o" . write-file)
   ("M-["     . beginning-of-buffer)
   ("M-]"     . end-of-buffer)
   ("C-h"     . mark-sexp)
   ("M-n"     . kill-sexp)
   ("M-p"     . backward-kill-sexp)
   ("M-h"     . mark-word)
   ("M-j"     . backward-kill-word))
  :init
  (define-key key-translation-map (kbd "C-j") (kbd "<DEL>")))

;; ---------------------------------------
;;; 5-1. Web Develop

;; ---------------------------------------
;;; 5-2. Org Mode

(use-package org
  :ensure nil
  :hook
  (org-mode . (lambda ()
                (display-line-numbers-mode -1)))
  :config
  (require 'org-tempo)
  (setq org-use-speed-commands t)
  (setq org-startup-indented t)
  (setq org-indent-indentation-per-level 4)
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil)
  (setq org-image-max-width 600)
  (setq org-src-preserve-indentation t)
  (setq org-fontify-todo-headline t)
  ;; Functions
  (defun sheep/org-insert-heading-eol ()
    (interactive)
    (end-of-line)
    (org-insert-heading))
  (defun sheep/org-insert-item-eol ()
    (interactive)
    (end-of-line)
    (org-meta-return))
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
	("M-J"          . org-shiftmetaright)))

;; ---------------------------------------
;;; 5-3. Markdown Mode

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :defer t)

;; ---------------------------------------
;;; 5-4. Emacs Lisp Mode

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (outline-minor-mode 1)
            (setq-local outline-regexp "^;;;")))

;; ---------------------------------------
;;; 6. AI Assistant 

(use-package gptel
  :config
  (setq gptel-model 'gemini-2.5-flash) ;デフォルトモデル
  (setq gptel-system-message "あなたは,私(質問者)のことを｢羊さん｣と呼んで下さい.
    公式のドキュメントや信頼性の高いソースから最新の情報を収集して下さい
    私(質問者)がUbuntu 24.04 LTS(GNOME,Wayland)ユーザーであることを考慮して下さい")
  (setq gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.") ;defaultの内容は変更不可
	  (emacs . "あなたはGNU Emacsをメインエディタとして使用するシニアエンジニアであり,Web開発のエキスパートです.
            SpacemacsやDoom Emacsは使用せず,プレーンなEmacsに厳選したパッケージを組み合わせることを好みます.
            最新のEmacs 30.2環境を生かして,Apheleia,gptel,Vertico,Consult,Corfuなど,モダンなパッケージを積極的に導入しています.ただしレガシーであっても,MagitやDiredなど,今なお強力なパッケージは使い続けています.
            あなたは,新人エンジニアの羊さんを教育します.最新技術に加え,シェルスクリプト･正規表現･Makefileといった基本技術も適宜教えます")
	  (linux . "あなたはLinuxとUbuntuのエキスパートであり,CLIにもGUI(GNOME)にも精通したシニアエンジニアであり,新人Webエンジニアである羊さんのメンターです")))
  (setq gptel-track-media t)
  (setq gptel-include-reasoning nil) ;回答から推論を削除
  (setq gptel-response-prefix-alist
	'((t . "## Answer\n\n")))
  ;; ;; Buckends
  ;バックエンドの追加
  (gptel-make-gemini "Gemini-Pro"
    :key (lambda () (getenv "GEMINI_API_KEY_PRO"))
    :stream t
    :models '(gemini-2.5-flash
              gemini-2.5-pro
              gemini-3-pro-preview))
  ;デフォルトのバックエンド
  (setq gptel-backend
        (gptel-make-gemini "Gemini-Free"
          :key (lambda () (getenv "GEMINI_API_KEY_FREE"))
          :stream t
          :models '(gemini-2.5-flash
                    gemini-2.5-pro)))
  ;デフォルトのモデル
  (setq gptel-model 'gemini-2.5-flash)
  ;; Functions
  (defun sheep/create-gptel-buffer ()
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
    (interactive)
    (insert "\n\n---")
    (call-interactively 'gptel-send))
  (defun sheep/erase-buffer-insert-heading ()
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
;;; 7.Auto-generated Code

(setq custom-file (locate-user-emacs-file "auto-generated.el")) ;別ファイルに保存
(when (file-exists-p custom-file)
  (load custom-file)) ;別ファイルから読込
