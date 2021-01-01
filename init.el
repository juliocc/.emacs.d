(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (dolist (handler file-name-handler-alist)
              (add-to-list 'file-name-handler-alist-old handler))
            (setq file-name-handler-alist file-name-handler-alist-old) t))

(setq load-prefer-newer noninteractive
      inhibit-compacting-font-caches t)

;; Put this file in register e for easy access
(set-register ?e `(file . ,user-init-file))

;;==================================================
;; Setup basic stuff
;;==================================================
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-windowed-mac* (and *is-a-mac* window-system))
(defvar jc-interactive-mode (not noninteractive))
(defvar jc-debug init-file-debug)

(if jc-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          garbage-collection-messages t
          message-log-max 16384
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(unless *is-a-windowed-mac*
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Less noise at startup. The dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(eval-and-compile
  (defun emacs-path (path)
    (expand-file-name path user-emacs-directory)))

;;==================================================
;; Setup package management tools
;;==================================================

(unless (fboundp 'url-insert-buffer-contents)
  (require 'url-handlers))

(require 'package)
(setq package-enable-at-startup nil
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      gnutls-verify-error t
      tls-checktrust t
      gnutls-min-prime-bits 3072
      network-security-level 'high
      ;; gnutls-log-level 2
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not (version< emacs-version "26.3"))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2")))

(package-initialize)

;; use-package setup

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-enable-imenu-support t)
(require 'use-package)
(setq use-package-always-ensure t)

;; (use-package benchmark-init
;;   :when jc-debug
;;   :demand t
;;   :hook (window-setup . benchmark-init/deactivate))

(use-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))

  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        custom-file (no-littering-expand-etc-file-name "custom.el"))

  (when (file-exists-p custom-file)
    (load-file custom-file))

  ;; no-littering changes default snippets directory, so i changed it back.
  ;; (add-to-list 'yas-snippet-dirs
  ;;              (expand-file-name "snippets" user-emacs-directory))
  )

(use-package jc-doom
  :ensure nil
  :load-path "site-lisp")

(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :config
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold (* 32 1024 1024)
        gcmh-low-cons-threshold (* 16 1024 1024)
        gcmh-verbose jc-debug))

;;==================================================
;; Appearance settings
;;==================================================

(use-package hide-mode-line
  :commands hide-mode-line-mode)

(use-package all-the-icons
  :if (display-graphic-p)
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil
        doom-one-brighter-modeline nil
        doom-themes-treemacs-theme "doom-colors")
  ;; (load-theme 'doom-opera t)
  ;; (load-theme 'doom-spacegrey t)
  ;; (load-theme 'doom-tomorrow-night t)
  ;; (load-theme 'doom-nord t)
  ;; (load-theme 'doom-molokai t)
  (load-theme 'doom-one t)
  (doom-themes-treemacs-config)
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  (setq doom-modeline-bar-width 2
        doom-modeline-enable-word-count t)
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))
  :config
  (add-hook 'magit-mode-hook
            (lambda ()
              "Show minimal modeline in magit-status buffer, no modeline elsewhere."
              (if (eq major-mode 'magit-status-mode)
                  (doom-modeline-set-vcs-modeline)
                (hide-mode-line-mode))))
  (doom-modeline-mode))

(use-package winum
  :defer 3
  :config
  (winum-mode +1))

;; (use-package solaire-mode
;;   :hook (change-major-mode . turn-on-solaire-mode)
;;   :hook (after-revert . turn-on-solaire-mode)
;;   :hook (ediff-prepare-buffer . solaire-mode)
;;   :hook (minibuffer-setup . solaire-mode-in-minibuffer)
;;   :config
;;   (setq solaire-mode-auto-swap-bg t)
;;   (solaire-global-mode +1))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)
(setq default-frame-alist '((cursor-type . (bar . 2))))
(setq-default frame-background-mode 'dark)

;; (setq redisplay-dont-pause t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))
        icon-title-format frame-title-format)
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Set default font
;(if (find-font (font-spec :name "Fira Code"))
;;    (set-default-font "Fira Code 13")
; (message "Can't find font"))

;; Other good fonts. Test text: ()[]l1t  O0o Ilegal1 = O0
;; (set-face-font 'default "Envy Code R 13")
;; (set-face-font 'default "ProggyCleanTT Nerd Font Complete 12")
;; (set-face-font 'default "gohufont")
;; (set-face-font 'default "Consolas 14")
;; (set-face-font 'default "Source Code Pro 14")
;; (set-face-font 'default "Droid Sans Mono Dotted 10")
;; (set-face-font 'default "Anonymous Pro 14")
;; (set-face-font 'default "Liberation Mono 10")
;; (set-face-font 'default "Ubuntu Mono 11")
;; (set-face-font 'default "MonteCarlo")
;; (set-face-font 'default "Inconsolata 16")

;; Put fringe on the side
(if (fboundp 'fringe-mode) (fringe-mode))

(global-font-lock-mode +1)               ; just in case

;; Explicitly define a width to reduce computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions makes it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen nil)
;; (global-linum-mode 1)
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'display-line-numbers-mode)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; (setq jit-lock-defer-time 0    ; only defer while processing input
;;       jit-lock-stealth-time 2) ; fontify the rest of the buffer after a delay

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;; (setq scroll-conservatively 100000)
;; (setq scroll-preserve-screen-position 'always)
;; (setq scroll-step 0); what?

;; (use-package display-fill-column-indicator
;;   :ensure nil
;;   :hook ((prog-mode conf-mode) . display-fill-column-indicator-mode))

(setq hscroll-margin 5
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 2
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

(when *is-a-mac*
  ;; sane trackpad/mouse scroll settings
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil))

;;==================================================
;; General settings
;;=================================================

(setq mouse-yank-at-point t                 ; mouse pastes at point
      select-enable-clipboard t             ; Allow pasting selection outside of Emacs
      global-auto-revert-non-file-buffers t ; auto refresh dired
      auto-revert-verbose nil               ; and be quiet about it
      eval-expression-print-level nil
      echo-keystrokes 0.02                  ; Show keystrokes in progress
      confirm-kill-emacs 'yes-or-no-p       ; ask me before closing
      history-length 2000                   ; looong history
      use-dialog-box nil                    ; never show a dialog box
      use-file-dialog nil
      mark-even-if-inactive t
      enable-recursive-minibuffers t        ; yes, please
      highlight-nonselected-windows t       ; show region even on inactive windows
      require-final-newline t               ; end files with a newline
      fill-column 80
      compilation-scroll-output t
      grep-highlight-matches t
      set-mark-command-repeat-p t
      isearch-allow-scroll t
      blink-matching-paren-distance 51200
      confirm-nonexistent-file-or-buffer nil
      indicate-buffer-boundaries nil
      indicate-empty-lines t
      x-underline-at-descent-line t
      idle-update-delay 2.0
      window-combination-resize t
      next-line-add-newlines nil)           ; don't add new lines when scrolling down

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (or that the user needs to stop relying on case
;; insensitivity).
(setq auto-mode-case-fold nil)

;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setq ad-redefinition-action 'accept)

;; Make apropos omnipotent. It's more useful this way.
(setq apropos-do-all t)


(setq-hook! '(prog-mode-hook text-mode-hook conf-mode-hook) show-trailing-whitespace t)
(setq-default highlight-tabs t)
(setq-default indicate-empty-lines t)
(setq-default word-wrap t)
(setq-default truncate-lines t)         ; don't word-wrap
(setq truncate-partial-width-windows nil)
(setq-default save-interprogram-paste-before-kill t)
(setq-default set-mark-command-repeat-pop t)
(setq shift-select-mode nil)            ; this is not windows
(setq delete-by-moving-to-trash t)
(setq sentence-end-double-space nil)

;; Disable bidirectional text rendering for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; confirm with y/n only
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package hippie-exp
  :ensure nil
  :bind (("M-/"   . hippie-expand)
         ("C-M-/" . dabbrev-completion))
  :init
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                           try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           ;;try-expand-line
                                           try-expand-dabbrev-from-kill
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))


(use-package hydra)

(use-package helpful
  :commands (helpful--read-symbol
             helpful-command
             helpful-variable
             helpful-function
             helpful-callable)
  :bind (([remap describe-command]  . #'helpful-command)
         ([remap describe-key]      . #'helpful-key)
         ([remap describe-symbol]   . #'helpful-symbol)
         ("C-h ." . helpful-at-point))
  :init
  (after! apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol)))))))

(use-package whitespace-cleanup-mode
  :hook (after-init . global-whitespace-cleanup-mode))

;; But don't show trailing whitespace in these modes
(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(add-hook! '(special-mode-hook
             Info-mode-hook
             term-mode-hook
             ido-minibuffer-setup-hook
             comint-mode-hook
             compilation-mode-hook
             isearch-mode-hook
             minibuffer-setup-hook)
           #'sanityinc/no-trailing-whitespace)

;; don't confirm killing buffers with attached processes
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; Backup settings
(use-package files
  :ensure nil
  :init
  (setq backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        create-lockfiles nil))

  ;; UTF-8 everything please
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))
  (setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

;; Don't cripple my emacs
(setq disabled-command-function nil)

;; Useful modes
(use-package image-file
  :defer 5
  :config
  (auto-image-file-mode 1))

;(size-indication-mode +1)                ; display file size
(delete-selection-mode +1)               ; delete selected text on input
;(global-subword-mode 1)
(global-auto-revert-mode +1)             ; auto reload files if changed outside emacs
(auto-compression-mode +1)               ; open compressed files a la dired
(transient-mark-mode +1)
(minibuffer-depth-indicate-mode +1)
(electric-indent-mode -1)             ; make return key not auto indent
;; (desktop-save-mode 1)
;; (fancy-narrow-mode)

(use-package winner
  :hook (after-init . winner-mode))

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode t))

(use-package volatile-highlights
  :defer 5
  :config
  (volatile-highlights-mode +1))

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :config
  ;; TODO:
  ;; (defadvice undo-tree-make-history-save-file-name
  ;;   (after undo-tree activate)
  ;;   (setq ad-return-value (concat ad-return-value ".gz")))
  ;; (advice-add #'undo-tree-save-history :around #'doom-shut-up-a)
  (setq undo-tree-auto-save-history t
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000))

;; Save a list of recent files visited.
(use-package recentf
  :hook (after-init . recentf-mode)
  ;; :bind ("C-x f" . recentf-ido-find-file)
  ;; :commands recentf-open-files
  :config
  (setq recentf-max-saved-items 500
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-exclude '("/tmp/" "/ssh:"))
  (add-hook! 'dired-mode-hook
    (defun doom--recentf-add-dired-directory-h ()
      "Add dired directory to recentf file list."
      (recentf-add-file default-directory))))

(use-package elec-pair
  :hook (after-init . electric-pair-mode)
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-save-minibuffer-history t
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (add-hook! 'kill-emacs-hook
    (defun doom-unpropertize-kill-ring-h ()
      "Remove text properties from `kill-ring' for a smaller savehist file."
      (setq kill-ring (cl-loop for item in kill-ring
                               if (stringp item)
                               collect (substring-no-properties item)
                               else if item collect it)))))

;; Never ever use tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)            ;; but maintain correct apeparance

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; uniquify:  provide meaningful names for buffers with the same name
(use-package uniquify
  :ensure nil
  :init
  ;(setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)     ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

;; use shift + arrow keys to switch between visible buffers
(use-package windmove
  :hook (after-init . windmove-default-keybindings))

;; show-paren-mode: subtle highlighting of matching parens (global-mode)
(use-package paren
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-style 'parenthesis
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; Save point position between sessions
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; guide-key setup
;; (use-package guide-key
;;   :config
;;   (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8"
;;                                        "C-x C-k" "<f8>" "C-c !" "M-s"
;;                                        "C-x n" "C-c p"))
;;   (add-hook 'dired-mode-hook
;;             (lambda () (guide-key/add-local-guide-key-sequence "%")))
;;   (guide-key-mode 1)
;;   (setq guide-key/recursive-key-sequence-flag t)
;;   (setq guide-key/popup-window-position 'bottom))

(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-max-description-length 45))

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(bind-key "C-S-P" #'pop-to-mark-command)

;;==================================================
;; Mac-specific settings
;;==================================================
(when *is-a-mac*
  (setq delete-by-moving-to-trash t)
  ;; left and right commands are meta
  (setq ns-command-modifier 'meta)
  (setq ns-right-command-modifier 'left)

  ;; left opt key is super
  (setq ns-alternate-modifier 'super)
  ;; right opt is ignored by emacs (useful for mac-style accent input)
  (setq ns-right-alternate-modifier 'none)

  ;; left and right controls are control
  (setq ns-control-modifier 'control)
  (setq ns-right-control-modifier 'left)

  ;; function key is hyper
  (setq ns-function-modifier 'hyper)

  (setq default-input-method "MacOSX")
  (setq insert-directory-program "gls")  ; dired works better with gls
  (setq default-directory (getenv "HOME"))
  (set-face-font 'default "Iosevka SS09 18"))

(when *is-a-windowed-mac*
  (setq visible-bell nil) ;; The default
  (setq ns-use-native-fullscreen nil)
  (setq ns-use-fullscreen-animation nil)
  (setq ns-pop-up-frames nil)

  ;; set my path manually on mac
  (setenv "LANG" "en_US.UTF-8")
  (let* ((mypaths '("~/bin" "~/homebrew/bin" "~/google-cloud-sdk/bin/"))
         (expanded (mapcar 'expand-file-name mypaths)))
    (setenv "PATH" (concat (string-join expanded ":") ":" (getenv "PATH")))
    (setq exec-path (append expanded exec-path))))

;; breaks doom theme
;; (setq ring-bell-function 'ignore)

(use-package ns-auto-titlebar
  :defer 1
  :if *is-a-windowed-mac*
  :config
  (ns-auto-titlebar-mode +1))

(use-package reveal-in-osx-finder
  :if *is-a-mac*
  :commands reveal-in-osx-finder)

; Stop C-z from minimizing windows under OS X
(when *is-a-windowed-mac*
  (unbind-key "C-z"))

(bind-key "RET" #'newline-and-indent)

;;==================================================
;; which-func-mode settings
;;==================================================
(use-package which-func
  :defer 5
  :config
  (which-function-mode +1))

;;==================================================
;; git and magit settings
;;==================================================

(use-package gitconfig-mode
  :mode (("\\.gitconfig\\'" . gitconfig-mode)
         ("\\.git/config\\'" . gitconfig-mode)
         ("\\.gitmodules\\'" . gitconfig-mode)))

(use-package gitignore-mode
  :mode "\\.gitignore\\'")

;; (use-package gl-conf-mode
;;   :load-path "site-lisp/gl-conf-mode"
;;   :mode "gitolite\\.conf\\'")

(use-package magit
  :bind (("C-x C-z" . magit-status)
         :map magit-status-mode-map
         ("q" . magit-quit-session))
  :config

  (use-package git-commit
    :hook
    (git-commit-mode . turn-on-flyspell)

    :config
    (global-git-commit-mode +1)

    ;; Enforce git commit conventions.
    ;; See https://chris.beams.io/posts/git-commit/
    (setq git-commit-summary-max-length 50
          git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
    (setq-hook! 'git-commit-mode-hook fill-column 72))

  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)

  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-no-confirm '(stage-all-changes unstage-all-changes)
        magit-delete-by-moving-to-trash t
        magit-diff-refine-hunk 'all
        magit-git-executable (executable-find magit-git-executable)
        magit-revision-insert-related-refs nil
        magit-save-repository-buffers nil)

  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package git-gutter
  :hook
  ((prog-mode text-mode conf-mode) . git-gutter-mode)
  :init
  (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                        :hint nil)
    "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
    ("j" git-gutter:next-hunk)
    ("k" git-gutter:previous-hunk)
    ("h" (progn (goto-char (point-min))
                (git-gutter:next-hunk 1)))
    ("l" (progn (goto-char (point-min))
                (git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("p" git-gutter:popup-hunk)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (progn (git-gutter-mode -1)
                ;; git-gutter-fringe doesn't seem to
                ;; clear the markup right away
                (sit-for 0.1)
                (git-gutter:clear))
     :color blue)))

;; (use-package xterm-color
;;   :defer t)

;; (use-package magit-delta
;;   :after (magit xterm-color)
;;   :config
;;   (magit-delta-mode))

;;==================================================
;; isearch settings
;;==================================================

;; use regexp isearch by default
(bind-key [remap isearch-forward] #'isearch-forward-regexp)
(bind-key [remap isearch-backward] #'isearch-backward-regexp)

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(use-package jc-search
  :ensure nil
  :commands (zap-to-isearch isearch-exit-other-end isearch-yank-symbol)
  :bind (:map isearch-mode-map
              ("M-z" . zap-to-isearch)
              ("C-w" . isearch-yank-symbol)
              ("C-RET" . isearch-exit-other-end)))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)
(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(use-package anzu
  :bind (([remap query-replace-regexp] . anzu-query-replace)
         ([remap query-replace] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)
         ([remap isearch-query-replace] . anzu-isearch-query-replace-regexp))
  :config
  ;; show number of matches while searching
  (global-anzu-mode +1))

;;==================================================
;; elisp
;;==================================================
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;;==================================================
;; ediff
;;==================================================

(after! ediff
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))

;;==================================================
;; scss-mode settings
;;==================================================
;; (use-package scss-mode
;;   :config
;;   (setq scss-compile-at-save nil))

;;==================================================
;; Dired settings
;;==================================================

(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump))
  :init
  (setq dired-listing-switches "--time-style long-iso -alhF --group-directories-first"
        dired-auto-revert-buffer t
        dired-hide-details-hide-symlink-targets nil
        dired-recursive-copies 'always
        )
  (setq-default diredp-hide-details-initially-flag nil
                dired-dwim-target t) ; Move files between split pane
  )


(use-package dired-x
  :ensure nil
  :after dired
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ("h" . dired-omit-mode))
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store\\'"
                "\\|^.project\\(?:ile\\)?\\'"
                "\\|^.\\(svn\\|git\\)\\'"
                "\\|^.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'")))

;; (use-package dired+
;;   :after dired
;;   :config
;;   (global-dired-hide-details-mode -1))

(use-package peep-dired
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package dired-hide-dotfiles
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))


(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'make-parent-directory)

(use-package markdown-mode
  :hook (markdown-mode . turn-on-flyspell)
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-fontify-code-blocks-natively t
        markdown-asymmetric-header t
        markdown-italic-underscore t
        markdown-content-type "application/xhtml+xml"
        markdown-css-paths
        '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
          "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-xhtml-header-content
        (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
                "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
                "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>")
        markdown-open-command (cond (*is-a-mac* "open")
                                    (t "xdg-open"))))

(use-package grip-mode
  :commands grip-mode)


(use-package multiple-cursors
  :bind (("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c c r" . set-rectangular-region-anchor)
         ("C-c c t" . mc/mark-sgml-tag-pair)
         ("C-c c c" . mc/edit-lines)
         ("C-c c e" . mc/edit-ends-of-lines)
         ("C-c c a" . mc/edit-beginnings-of-lines)))

(use-package change-inner
  :bind (("C-c i" . change-inner)
         ("C-c o" . change-outer)))

;; (use-package misc
;;   :ensure nil
;;   :bind (("M-z" . zap-up-to-char)
;;          ("M-Z" . zap-to-char)))

(use-package avy-zap
  :bind (("M-Z" . avy-zap-up-to-char-dwim)))

(use-package misc
  :ensure nil
  :bind ("M-z" . zap-up-to-char))

(use-package jc-misc
  :ensure nil
  :defer 1
  :commands (chmod+x-this jc/doctor)
  :bind ("M-p" . goto-match-paren)
  :config (jc/doctor))


(use-package crux
  :commands crux-find-shell-init-file
  :bind (("C-a" . crux-move-beginning-of-line)
         ("S-<return>" . crux-smart-open-line)
         ("C-S-k" . crux-kill-whole-line)
         ("C-S-<return>" . crux-smart-open-line-above))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-reopen-as-root-mode +1))

(use-package jc-windows
  :ensure nil
  :bind (("C-x |" . split-window-horizontally-instead)
         ("C-x _" . split-window-vertically-instead)
         ("C-2"   . split-window-vertically-with-other-buffer)
         ("C-3"   . split-window-horizontally-with-other-buffer)
         ("S-C-j" . quick-switch-buffer)))

(use-package window
  :ensure nil
  :bind (("C-1"         . delete-other-windows)
         ("C-0"         . delete-window)
         ("S-C-<left>"  . shrink-window-horizontally)
         ("S-C-<right>" . enlarge-window-horizontally)
         ("S-C-<down>"  . shrink-window)
         ("S-C-<up>"    . enlarge-window)))

; ibuffer
(use-package ibuffer
  :hook (ibuffer . hydra:ibuffer-main/body)
  :bind ("C-x C-b" . ibuffer)
  :bind (:map ibuffer-mode-map
              ("." . hydra:ibuffer-main/body))
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face '(:inherit (success bold)))
  (define-ibuffer-column size
    (:name "Size"
           :inline t
           :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size)))
  :init
  (defhydra hydra:ibuffer-main (:color pink :hint nil)
    "
  ^Navigation^  |  ^Mark^        |  ^Actions^        |  ^View^
--^----------^--+--^----^--------+--^-------^--------+--^----^----------
   _k_:    ʌ    |  _m_: mark     |  _D_: delete      |  _g_: refresh
  _RET_: visit  |  _u_: unmark   |  _S_: save        |  _s_: sort...
   _j_:    v    |  _*_: mark...  |  _a_: actions...  |  _/_: filter...
--^----------^--+--^----^--------+--^-------^--------+--^----^----------
"
    ("j" ibuffer-forward-line)
    ("RET" ibuffer-visit-buffer :color blue)
    ("k" ibuffer-backward-line)

    ("m" ibuffer-mark-forward)
    ("u" ibuffer-unmark-forward)
    ("*" hydra:ibuffer-mark/body :color blue)

    ("D" ibuffer-do-delete)
    ("S" ibuffer-do-save)
    ("a" hydra:ibuffer-action/body :color blue)

    ("g" ibuffer-update)
    ("s" hydra:ibuffer-sort/body :color blue)
    ("/" hydra:ibuffer-filter/body :color blue)

    ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
    ("q" nil "toggle hydra" :color blue)
    ("." nil "toggle hydra" :color blue))


  (defhydra hydra:ibuffer-mark (:color teal :columns 5
                                       :after-exit (hydra:ibuffer-main/body))
    "Mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("f" ibuffer-mark-by-file-name-regexp "file name regexp")

    ("b" hydra:ibuffer-main/body "back" :color blue)
    ("q" hydra:ibuffer-main/body "back" :color blue))

  (defhydra hydra:ibuffer-action (:color teal :columns 4
                                         :after-exit
                                         (if (eq major-mode 'ibuffer-mode)
                                             (hydra:ibuffer-main/body)))
    "Action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")

    ("b" nil "back")
    ("q" nil "back"))

  (defhydra hydra:ibuffer-sort (:color amaranth :columns 3)
    "Sort"
    ("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")

    ("b" hydra:ibuffer-main/body "back" :color blue)
    ("q" hydra:ibuffer-main/body "back" :color blue))

  (defhydra hydra:ibuffer-filter (:color amaranth :columns 4)
    "Filter"
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")

    ("b" hydra:ibuffer-main/body "back" :color blue)
    ("q" hydra:ibuffer-main/body "back" :color blue)))

(use-package ibuffer-projectile
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :config
  (when (display-graphic-p)
    (setq ibuffer-projectile-prefix
          (concat (all-the-icons-octicon
                   "file-directory"
                   :face ibuffer-filter-group-name-face
                   :v-adjust -0.05)
                  " "))))

(use-package shrink-whitespace
  :bind ("M-SPC" . shrink-whitespace))

(use-package beacon
  :defer 2
  :config
  (setq beacon-color "#6F6F6F"
        beacon-blink-when-focused  t)
  (defun not-display-graphic-p ()
    (not (display-graphic-p)))
  (add-hook 'beacon-dont-blink-predicates #'not-display-graphic-p)
  (beacon-mode))

(use-package pulse
  :ensure nil
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region))))
  :hook (((dumb-jump-after-jump
           imenu-after-jump) . my/recenter-and-pulse)
         ((bookmark-after-jump
           magit-diff-visit-file
           next-error) . my/recenter-and-pulse-line))
  :init
  (with-no-warnings
    (defun my/pulse-momentary-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

    (defun my/pulse-momentary (&rest _)
      "Pulse the region or the current line."
      (if (fboundp 'xref-pulse-momentarily)
          (xref-pulse-momentarily)
        (my/pulse-momentary-line)))

    (defun my/recenter-and-pulse(&rest _)
      "Recenter and pulse the region or the current line."
      (recenter)
      (my/pulse-momentary))

    (defun my/recenter-and-pulse-line (&rest _)
      "Recenter and pulse the current line."
      (recenter)
      (my/pulse-momentary-line))

    (dolist (cmd '(recenter-top-bottom
                   other-window windmove-do-window-select
                   ace-window aw--select-window
                   pager-page-down pager-page-up
                   winum-select-window-by-number
                   ;; treemacs-select-window
                   symbol-overlay-basic-jump))
      (advice-add cmd :after #'my/pulse-momentary-line))

    (dolist (cmd '(pop-to-mark-command
                   pop-global-mark
                   goto-last-change))
      (advice-add cmd :after #'my/recenter-and-pulse))))

(use-package drag-stuff
  :hook
  ((prog-mode text-mode conf-mode) . turn-on-drag-stuff-mode)
  :config
  (setq drag-stuff-modifier '(meta super))
  (drag-stuff-define-keys))

;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :hook
  ((prog-mode text-mode conf-mode) . whole-line-or-region-local-mode)
  :config
  (whole-line-or-region-global-mode +1))

;;==================================================
;; ispell
;;==================================================
(use-package ispell
  :ensure nil
  :init
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"
                            "--run-together")))

(use-package flyspell
  :ensure nil
  :hook (text-mode . turn-on-flyspell)
  :hook (prog-mode . flyspell-prog-mode)
  :commands (turn-on-flyspell flyspell-buffer))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; (use-package flyspell-correct-ivy
;;   :after flyspell-correct)

;; (use-package flyspell-correct-avy-menu
;;   :after flyspell-correct)


;; (bind-keys :prefix-map jc-spelling-map
;;            :prefix "<f8>"
;;            ("<f8>" . ispell-word)
;;            ("m" . flyspell-mode)
;;            ("b" . flyspell-buffer)
;;            ("p" . flyspell-check-previous-highlighted-word)
;;            ("n" . flyspell-check-next-highlighted-word)
;;            ("c" . cycle-ispell-languages))

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-mode-line
        '(:eval
          (format " Prj:%s"
                  (projectile-project-name))))

  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-sort-order 'recently-active)
  projectile-globally-ignored-files '(".DS_Store" "TAGS")
  (when (executable-find "fd")
    (let ((fd-command "fd . --type f --print0"))
      (setq projectile-hg-command fd-command)
      (setq projectile-git-command fd-command)
      (setq projectile-fossil-command fd-command)
      (setq projectile-bzr-command fd-command)
      (setq projectile-darcs-command fd-command)
      (setq projectile-svn-command fd-command)
      (setq projectile-generic-command fd-command)))
  (projectile-mode +1)
  (setq projectile-require-project-file nil))

;; (setq projectile-project-root-files-bottom-up
;;       (append '(".projectile"  ; projectile's root marker
;;                 ".project"     ; doom project marker
;;                 ".git")        ; Git VCS root dir
;;               (when (executable-find "hg")
;;                 '(".hg"))      ; Mercurial VCS root dir
;;               (when (executable-find "bzr")
;;                 '(".bzr")))    ; Bazaar VCS root dir
;;       ;; This will be filled by other modules. We build this list manually so
;;       ;; projectile doesn't perform so many file checks every time it resolves
;;       ;; a project's root -- particularly when a file has no project.
;;       projectile-project-root-files '()
;;       projectile-project-root-files-top-down-recurring '("Makefile"))



(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'rg))

(use-package company
  :defer 2
  :commands (company-mode company-indent-or-complete-common)
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (local-set-key (kbd "<tab>")
                             #'company-indent-or-complete-common)))
  :config
  (setq company-idle-delay 0.6
        company-show-numbers t
        company-tooltip-align-annotations t
        company-minimum-prefix-length 1
        company-dabbrev-downcase nil)
  (global-company-mode))

(use-package company-terraform
  :after (company terraform-mode)
  :disabled t
  :config
  (add-to-list 'company-backends 'company-terraform))

;; (use-package company-quickhelp
;;  :hook
;;  (global-company-mode . company-quickhelp))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  ;; https://github.com/sebastiencs/company-box/issues/44
  (defun jc/fix-company-scrollbar (orig-fn &rest args)
    "disable company-box scrollbar"
    (cl-letf (((symbol-function #'display-buffer-in-side-window)
               (symbol-function #'ignore)))
      (apply orig-fn args)))

  (advice-add #'company-box--update-scrollbar :around #'jc/fix-company-scrollbar))

(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :commands yas-hippie-try-expand
  :init
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

;; (use-package yasnippet-snippets         ; Collection of snippets
;;   :after yasnippet)

;; (use-package flycheck
;;   :after-call after-find-file
;;   :config
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (setq flycheck-display-errors-delay 0.25)
;;   (global-flycheck-mode))

;; (use-package flymake-shellcheck
;;   :after flycheck
;;   :hook (sh-mode . flymake-shellcheck-load))

;; ;; (use-package flycheck-popup-tip
;; ;;   :after flycheck
;; ;;   :config
;; ;;   (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))


;; (use-package flycheck-posframe
;;   :after flycheck
;;   :hook
;;   (flycheck-mode . flycheck-posframe-mode)
;;   :config
;;   (setq flycheck-posframe-border-width 2
;;         flycheck-posframe-warning-prefix "⚠ "
;;         flycheck-posframe-info-prefix "··· "
;;         flycheck-posframe-error-prefix "✕ ")
;;   (after! company
;;     ;; Don't display popups if company is open
;;     (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p)))


;; (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
;; (flycheck-posframe-configure-pretty-defaults)
;; (setq flycheck-posframe-border-width 2)
;; (set-face-foreground 'flycheck-posframe-border-face "red"))


;;==================================================
;; wgrep
;;==================================================

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode)

(use-package ag
  :after wgrep
  :if (executable-find "ag")
  :config
  (use-package wgrep-ag)
  (setq ag-highlight-search t))

(use-package dockerfile-mode
  :mode "Dockerfile[a-zA-Z.-]*\\'")

(use-package terraform-mode
  :hook
  (terraform-mode . terraform-format-on-save-mode))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(setq js-indent-level 2)

(use-package yaml-mode
  :mode "\\.yaml?\\'")

(use-package json-mode
  :mode "\\.json?\\'")

(use-package scss-mode
  :mode "\\.scss?\\'"
  :config
  (setq scss-compile-at-save nil))

(use-package js2-mode
  :mode "\\.js\\'")

;;==================================================
;; jump-char
;;==================================================

;; NOTE: keep this after loading ido, otherwise M-m won't work inside
;; ido
;; (use-package jump-char
;;   :config
;;   :bind (("M-m" . jump-char-forward)
;;          ("M-M" . jump-char-backward)))

(use-package pip-requirements
  :mode "requirements\\.txt\\'")

(use-package paradox
  :commands paradox-list-packages)

(use-package highlight-symbol
  :commands (highlight-symbol
             highlight-symbol-query-replace
             highlight-symbol-occur))

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'"   . ssh-config-mode)
         ("/known_hosts\\'"     . ssh-known-hosts-mode)
         ("/authorized_keys\\'" . ssh-authorized-keys-mode)))

(use-package avy
  :bind (("M-g g" . avy-goto-line)
         ("C-c C-j" . avy-resume)
         ("C-c C-n" . avy-next)
         ("C-c C-p" . avy-prev)
         ("C-'" . avy-goto-char)
         ("C-\"" . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 0.6)
  (avy-setup-default))

(use-package ace-window
  :after avy
  :bind ([remap other-window] . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ? ?j ?k ?l))
  (setq aw-dispatch-always nil))

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-c I" . er/mark-inside-pairs)
         ("C-c O" . er/mark-outside-pairs)
         ("C-c '" . er/mark-inside-quotes)))

(use-package rainbow-mode
  :hook (css-mode html-mode))

(defadvice comment-dwim (around comment-line-maybe activate)
  "If invoked from the beginning of a line or the beginning of
text on a line, comment the current line instead of appending a
comment to the line."
  (if (and (not (use-region-p))
           (not (eq (line-end-position)
                    (save-excursion (back-to-indentation) (point))))
           (or (eq (point) (line-beginning-position))
               (eq (point) (save-excursion (back-to-indentation) (point)))))
      (comment-or-uncomment-region (line-beginning-position)
                                   (line-end-position))
    ad-do-it
    (setq deactivate-mark nil)))


;;==================================================
;; require additional local settings (if they exist)
;;==================================================

(use-package jc-local
  :ensure nil
  :load-path "site-lisp"
  :if (file-exists-p (emacs-path "site-lisp/jc-local.el")))

(use-package server
  :if window-system
  :hook (after-init . server-start))


;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defun doom/escape ()
  "Run `doom-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(bind-key [remap keyboard-quit] #'doom/escape)

;; Don't resize windows & frames in steps; it's prohibitive to prevent the user
;; from resizing it to exact dimensions, and looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)
                                        ;(window-divider-mode t)

 ;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160
      split-height-threshold nil)

(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Enable mouse in terminal Emacs
;; (add-hook 'tty-setup-hook #'xterm-mouse-mode)

(use-package hl-line
  ;; Highlights the current line
  :if (display-graphic-p)
  :hook ((prog-mode text-mode conf-mode special-mode) . hl-line-mode)
  :config
  ;; Not having to render the hl-line overlay in multiple buffers offers a tiny
  ;; performance boost. I also don't need to see it in other buffers.
  (setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  (defvar doom--hl-line-mode nil)

  (add-hook 'activate-mark-hook
            (defun doom-disable-hl-line-h ()
              (when hl-line-mode
                (setq-local doom--hl-line-mode t)
                (hl-line-mode -1))))

  (add-hook 'deactivate-mark-hook
            (defun doom-enable-hl-line-maybe-h ()
              (when doom--hl-line-mode
                (hl-line-mode +1)))))


(setq image-animate-loop t)

;; Resolve symlinks when opening files, so that any operations are conducted
;; from the file's true directory (like `find-file').
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
(setq find-file-suppress-same-file-warnings t)

;; Allow UTF or composed text from the clipboard, even in the terminal or on
;; non-X systems (like Windows or macOS), where only `STRING' is used.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


(use-package restart-emacs
  :commands restart-emacs)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package highlight-indent-guides
  ;;:hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :commands highlight-indent-guides-mode
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top-edge))

(after! compile
  (setq compilation-always-kill t       ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  ;; Handle ansi codes in compilation buffer
  (add-hook 'compilation-filter-hook #'doom-apply-ansi-color-to-compilation-buffer-h))


(add-hook! '(completion-list-mode-hook Man-mode-hook)
           #'hide-mode-line-mode)

;; (use-package persistent-scratch
;;   :unless (or (null window-system)
;;               noninteractive)
;;   :defer 5
;;   :config
;;   (persistent-scratch-autosave-mode)
;;   (with-demoted-errors "Error: %S"
;;     (persistent-scratch-setup-default))
;;   :commands persistent-scratch-setup-default)

(use-package vterm
  :disabled t
  :hook (vterm-mode . hide-mode-line-mode)
  :commands vterm)

(use-package imenu-list
  :commands imenu-list-minor-mode)

(use-package copy-as-format
  :commands copy-as-format)

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package ialign
  :commands ialign)

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  :commands treemacs)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; (use-package flx
;;   :defer t                              ; loaded by ivy
;;   :init
;;   (setq ivy-flx-limit 15000))

(use-package ivy
  :hook (after-init . ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume))
  :bind (:map ivy-switch-buffer-map
              ("C-k" . ivy-switch-buffer-kill))
  :bind (:map ivy-minibuffer-map
              ("C-j" . ivy-immediate-done)
              ("RET" . ivy-alt-done))
  :config

  ;; TODO
  ;; (use-package orderless
  ;;   :custom (completion-styles '(orderless)))

  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-magic-tilde nil
        ivy-wrap t
        ivy-on-del-error-function #'ignore
        ivy-dynamic-exhibit-delay-ms 200
        ivy-use-selectable-prompt t
        ;; ivy-re-builders-alist
        ;; '((counsel-rg       . ivy--regex-plus)
        ;;   (swiper           . ivy--regex-plus)
        ;;   (swiper-isearch   . ivy--regex-plus)
        ;;   (t                . ivy--regex-fuzzy))
        ivy-more-chars-alist
        '((counsel-rg . 1)
          (counsel-search . 2)
          (t . 3))
        ivy-initial-inputs-alist nil
        ivy-height 20)

  ;; Highlight each ivy candidate including the following newline, so that it
  ;; extends to the right edge of the window
  (setf (alist-get 't ivy-format-functions-alist)
        #'ivy-format-function-line)

  (with-eval-after-load 'counsel
    (setq ivy-initial-inputs-alist nil)))

(use-package counsel
  :after ivy
  :bind (;;("C-x b"    . counsel-switch-buffer)
         ;;("C-x B"    . counsel-switch-buffer-other-window)
         ("C-x C-f"  . counsel-find-file)
         ("C-x r b"  . counsel-bookmark)
         ("M-x"      . counsel-M-x)
         ;; ("M-y"      . counsel-yank-pop)
         ("M-i"      . counsel-imenu)
         ("M-s f"    . counsel-file-jump)
         ("M-s g"    . counsel-rg)
         ("M-s j"    . counsel-dired-jump)
         ("C-x f"    . counsel-recentf)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-variable] . counsel-describe-variable))
  :bind (:map ivy-minibuffer-map
              ("M-y" . ivy-next-line))
  :config

  (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
    (if (equal major-mode 'vterm-mode)
        (let ((inhibit-read-only t)
              (yank-undo-function (lambda (_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
                     (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
      (apply orig-fun args)))
  (advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)

  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))
;; ("C-c e l" . counsel-find-library)
;; ("C-c e q" . counsel-set-variable)
;; ("C-h e l" . counsel-find-library)
;; ("C-h e u" . counsel-unicode-char)
;; ("C-h f"   . counsel-describe-function)

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode +1))

(use-package ivy-rich
  :after ivy
  :config

  ;; -- fix https://github.com/Yevgnen/ivy-rich/issues/87
  (defvar ek/ivy-rich-cache
    (make-hash-table :test 'equal))

  (defun ek/ivy-rich-cache-lookup (delegate candidate)
    (let ((result (gethash candidate ek/ivy-rich-cache)))
      (unless result
        (setq result (funcall delegate candidate))
        (puthash candidate result ek/ivy-rich-cache))
      result))

  (defun ek/ivy-rich-cache-reset ()
    (clrhash ek/ivy-rich-cache))

  (defun ek/ivy-rich-cache-rebuild ()
    (mapc (lambda (buffer)
            (ivy-rich--ivy-switch-buffer-transformer (buffer-name buffer)))
          (buffer-list)))

  (defun ek/ivy-rich-cache-rebuild-trigger ()
    (ek/ivy-rich-cache-reset)
    (run-with-idle-timer 1 nil 'ek/ivy-rich-cache-rebuild))

  (advice-add 'ivy-rich--ivy-switch-buffer-transformer :around 'ek/ivy-rich-cache-lookup)
  (advice-add 'ivy-switch-buffer :after 'ek/ivy-rich-cache-rebuild-trigger)
  ;; --

  (ivy-rich-mode 1)
  (setq ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-path-style 'abbrev))

(use-package all-the-icons-ivy-rich
  :if (display-graphic-p)
  :after ivy-rich
  :config
  (setq all-the-icons-ivy-rich-icon-size 0.75)
  (nsubst-if '(ivy-rich-candidate (:width 60))
             #'(lambda (item)
                 (and
                  (listp item)
                  (eq (length item) 2)
                  (eq (car item) 'ivy-rich-candidate)))
             all-the-icons-ivy-rich-display-transformers-list)
  (all-the-icons-ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-M-s" . swiper-isearch)
         ("C-M-r" . swiper-isearch-backward)
         :map swiper-map
         ("C-y" . yank)
         ("M-%" . swiper-query-replace)
         ("M-o" . swiper-isearch-toggle)
         :map isearch-mode-map
         ("M-o" . swiper-isearch-toggle)))

(use-package imenu-anywhere
  :bind ("M-i" . ivy-imenu-anywhere)
  :init
  (setq imenu-auto-rescan t))

(use-package ivy-prescient
  :after (counsel swiper)
  :config
  (setq ivy-prescient-enable-filtering t
        prescient-filter-method '(literal initialism prefix regexp)
        prescient-sort-length-enable nil
        ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode +1)
  (prescient-persist-mode +1)
  (defun jc-prescient-fuzzy-re-buider (query)
    (let ((prescient-filter-method '(literal initialism regexp fuzzy)))
      (ivy-prescient-re-builder query)))
  (setq ivy-re-builders-alist
        '((counsel-rg       . ivy--regex-plus)
          (swiper           . ivy--regex-plus)
          (swiper-isearch   . ivy--regex-plus)
          (counsel-M-x      . jc-prescient-fuzzy-re-buider)
          (t                . ivy-prescient-re-builder))))


(use-package restart-emacs
  :commands restart-emacs)

;; (use-package smartscan
;;   :bind (:map smartscan-map
;;               ("M-[" . smartscan-symbol-go-backward)
;;               ("M-]" . smartscan-symbol-go-forward))
;;   :hook (after-init . global-smartscan-mode)
;;   :config
;;   (unbind-key "M-p" smartscan-map)
;;   (unbind-key "M-n" smartscan-map))


(use-package browse-kill-ring
  :hook (after-init . browse-kill-ring-default-keybindings))

(use-package clipetty
  :commands (global-clipetty-mode clipetty-kill-ring-save))

(use-package guru-mode
  :hook (after-init . guru-global-mode)
  :config
  (setq guru-warn-only t))

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :hook ((terraform-mode python-mode) . lsp)
;;   :hook (lsp-mode . lsp-enable-which-key-integration)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l"))

;; (use-package lsp-ui
;;   :commands lsp-ui)

;; (use-package company-lsp
;;   :after (lsp-mode company)
;;   :config
;;   (push 'company-lsp company-backends))


(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))

(use-package try
  :commands try)

(use-package smerge-mode
  :after hydra
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (hydra:smerge/body))))
  :config
  (defhydra hydra:smerge
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue)))

(use-package kmacro
  :ensure nil
  :bind ("C-x r h" . hydra:rectangle/body)
  :config
  (defhydra hydra:rectangle (:body-pre (rectangle-mark-mode 1)
                                       :color pink
                                       :hint nil
                                       :post (deactivate-mark))
    "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines
_h_   _l_     _y_ank        _t_ype       _e_xchange-point
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark
^^^^          _u_ndo        _q_ quit     ^ ^
"
    ("k" rectangle-previous-line)
    ("j" rectangle-next-line)
    ("h" rectangle-backward-char)
    ("l" rectangle-forward-char)
    ("d" kill-rectangle)                    ;; C-x r k
    ("y" yank-rectangle)                    ;; C-x r y
    ("w" copy-rectangle-as-kill)            ;; C-x r M-w
    ("o" open-rectangle)                    ;; C-x r o
    ("t" string-rectangle)                  ;; C-x r t
    ("c" clear-rectangle)                   ;; C-x r c
    ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
    ("N" rectangle-number-lines)            ;; C-x r N
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)))
    ("u" undo nil)
    ("q" nil)))

(use-package tramp
  :ensure nil
  :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; TRAMP gcloud ssh
  (add-to-list 'tramp-methods
               '("iap"
                 (tramp-login-program        "gcloud compute ssh --tunnel-through-iap")
                 (tramp-login-args           (("%h")))
                 (tramp-async-args           (("-q")))
                 (tramp-remote-shell         "/bin/bash")
                 (tramp-remote-shell-args    ("-c"))
                 (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                              ("-o" "UserKnownHostsFile=/dev/null")
                                              ("-o" "StrictHostKeyChecking=no")))
                 (tramp-default-port         22))))

(use-package color-identifiers-mode
  :commands color-identifiers-mode)

(use-package py-isort
  :commands py-isort-buffer)

(use-package blacken
  :hook (python-mode . blacken-mode)
  :commands blacken-mode blacken-buffer)

(use-package keypression
  :commands keypression-mode)

(use-package command-log-mode
  :commands command-log-mode)

(use-package focus
  :commands focus-mode
  :config
  (add-to-list 'focus-mode-to-thing '(python-mode . paragraph)))

(use-package dimmer
  :commands dimmer-mode
  :config
  (setq dimmer-fraction 0.1)
  (dimmer-configure-which-key)
  (dimmer-configure-magit))

;; (add-to-list 'tramp-methods
;;              '("gcssh"
;;                (tramp-login-program        "gcloud compute ssh")
;;                (tramp-login-args           (("%h")))
;;                (tramp-async-args           (("-q")))
;;                (tramp-remote-shell         "/bin/bash")
;;                (tramp-remote-shell-args    ("-c"))
;;                (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
;;                                             ("-o" "UserKnownHostsFile=/dev/null")
;;                                             ("-o" "StrictHostKeyChecking=no")))
;;                (tramp-default-port         22)))



;; MISC STUFF: snoopy-mode editorconfig
;;
;; TODO doom:  dtrt-indent smartparens so-long
;;  pcre2el  ace-mc miniedit iedit hideshow
;; company  workspaces lsp visual-line-mode
;; ivy-occur cousel-mark-ring
;;  spell-fu-mode
;;

;; hydras: expand, multiple-cursors, avy
;; emacs-format-all-the-code

(when init-file-debug
  (use-package-report))
