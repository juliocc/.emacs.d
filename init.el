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
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-windowed-mac* (and *is-a-mac* (display-graphic-p)))
(defvar jccb/interactive-mode (not noninteractive))
(defvar jccb/debug init-file-debug)

(if jccb/debug
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
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode 10))

;; Less noise at startup. The dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; *scratch* stats using fundamental-mode, but change it to
;; lisp-interaction-mode once we're all set up
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq initial-major-mode 'lisp-interaction-mode)
            (with-current-buffer (get-buffer "*scratch*")
              (lisp-interaction-mode))))

(eval-and-compile
  (defun emacs-path (path)
    (expand-file-name path user-emacs-directory)))

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;;==================================================
;; Setup package management tools
;;==================================================

(unless (fboundp 'url-insert-buffer-contents)
  (require 'url-handlers))

(setq straight-use-package-by-default t
      straight-profiles `((nil . ,(emacs-path "straight.lockfile.el")))
      package-enable-at-startup nil
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


(setq use-package-enable-imenu-support t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load-file custom-file)))

(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 64 1024 1024)
        gcmh-low-cons-threshold (* 32 1024 1024)
        gcmh-verbose jccb/debug))

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

;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic nil
;;         doom-one-brighter-modeline nil
;;         doom-themes-treemacs-theme "doom-colors")
;;   (load-theme 'doom-one t)
;;   ;;(doom-themes-treemacs-config)
;;   (doom-themes-visual-bell-config)
;;   ;;(doom-themes-org-config)
;;   )

(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-subtle-line-numbers nil
        modus-themes-intense-markup t
        modus-themes-tabs-accented t
        modus-themes-fringes 'subtle
        modus-themes-syntax nil
        modus-themes-hl-line '(accented)
        modus-themes-paren-match '(bold intense)
        modus-themes-links '(neutral-underline background)
        modus-themes-prompts '(intense)
        modus-themes-completions 'subtle
        modus-themes-region '(bg-only no-extend)
        modus-themes-mode-line '(1 accented borderless)
        modus-themes-diffs nil)
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))


(use-package doom-modeline
  :hook ((after-init . doom-modeline-mode)
         (doom-modeline-mode . size-indication-mode) ; filesize in modeline
         (doom-modeline-mode . column-number-mode))   ; cursor column in modeline
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

;; switch windows with C-x w <number>
(use-package winum
  :hook (after-init . winum-mode))

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
(setq-default cursor-type '(bar . 2))
(setq-default frame-background-mode 'dark)

(when (display-graphic-p)
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))
        icon-title-format frame-title-format)
  (blink-cursor-mode -1))


(defvar jccb/fixed-font "Iosevka SS09")
(when (find-font (font-spec :name jccb/fixed-font))
  (set-face-attribute 'default nil
                      :font jccb/fixed-font
                      :height 200)
  (set-face-attribute 'fixed-pitch nil
                      :font jccb/fixed-font
                      :height 200))
(global-font-lock-mode +1)

(use-package display-line-numbers
  :straight nil
  :ensure nil
  :hook ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :config
  ;; Explicitly define a width to reduce computation
  (setq-default display-line-numbers-width 3)
  ;; Show absolute line numbers for narrowed regions makes it easier to tell the
  ;; buffer is narrowed, and where you are, exactly.
  (setq-default display-line-numbers-widen nil))

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

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
(dolist (hook '(eshell-mode-hook term-mode-hook))
  (add-hook hook (lambda () (setq hscroll-margin 0
                                  hscroll-step 0
                                  scroll-margin 0))))
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
      x-underline-at-descent-line t
      idle-update-delay 2.0
      window-combination-resize t
      next-line-add-newlines nil            ; don't add new lines when scrolling down
      kill-read-only-ok t
      kill-do-not-save-duplicates t)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (or that the user needs to stop relying on case
;; insensitivity).
(setq auto-mode-case-fold nil)

;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setq ad-redefinition-action 'accept)

;; Make apropos omnipotent. It's more useful this way.
(setq apropos-do-all t)

(setq-default highlight-tabs t)
(setq-default indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
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

;; (use-package hippie-exp
;;   :ensure nil
;;   :straight nil
;;   :bind (("M-/"   . hippie-expand)
;;          ("C-M-/" . dabbrev-completion))
;;   :init
;;   (setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible
;;                                            try-expand-dabbrev
;;                                            try-expand-dabbrev-all-buffers
;;                                            try-complete-file-name-partially
;;                                            try-complete-file-name
;;                                            try-expand-all-abbrevs
;;                                            try-expand-list
;;                                            ;;try-expand-line
;;                                            try-expand-dabbrev-from-kill
;;                                            try-complete-lisp-symbol-partially
;;                                            try-complete-lisp-symbol)))

(use-package helpful
  :commands (helpful--read-symbol
             helpful-callable)
  :bind (([remap describe-command]  . helpful-command)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-function] . helpful-function)
         ([remap describe-variable] . helpful-variable)
         ("C-h ." . helpful-at-point))
  :init
  (with-eval-after-load 'apropos
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

(setq nobreak-char-display 0)

(use-package whitespace-cleanup-mode
  :hook (after-init . global-whitespace-cleanup-mode))

(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                isearch-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'sanityinc/no-trailing-whitespace))

;; don't confirm killing buffers with attached processes
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Backup settings
(use-package files
  :straight nil
  :ensure nil
  :init
  (setq backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        create-lockfiles nil))

;; UTF-8 everything please
(set-language-environment 'utf-8)
;; apparently that is enough

;; (when (fboundp 'set-charset-priority)
;;   (set-charset-priority 'unicode))
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)

;; Don't cripple my emacs
(setq disabled-command-function nil)

;; Useful modes
(use-package image-file
  :defer 5
  :config
  (auto-image-file-mode 1))

;; (size-indication-mode +1)                ; display file size
(delete-selection-mode +1)               ; delete selected text on input
;; (global-subword-mode 1)
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
      '(read-only t cursor-intangible t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :hook (after-init . global-highlight-parentheses-mode))

;; (use-package volatile-highlights
;;   :defer 5
;;   :config
;;   (volatile-highlights-mode +1))

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
        undo-outer-limit 120000000
        undo-tree-enable-undo-in-region t))

;; Save a list of recent files visited.
(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 500
        recentf-auto-cleanup 1200
        recentf-max-menu-items 0)

  (dolist (path `("/tmp/" "/private/var" "/ssh:" "/iap:"
                  ,package-user-dir
                  ,no-littering-var-directory
                  ,no-littering-etc-directory))
    (add-to-list 'recentf-exclude path))

  (add-hook 'dired-mode-hook
            (defun doom--recentf-add-dired-directory-h ()
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
  (add-hook 'kill-emacs-hook
            (defun doom-unpropertize-kill-ring-h ()
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
  :straight nil
  :ensure nil
  :init
  ;; (setq uniquify-buffer-name-style 'forward)
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
  :straight nil
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package which-key
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-idle-delay 0.75
        which-key-idle-secondary-delay 0.05
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
  (setq mac-command-modifier 'meta)
  (setq mac-right-command-modifier 'left)
  ;; left opt key is super
  (setq mac-option-modifier 'super)
  ;; right opt is ignored by emacs (useful for mac-style accent input)
  (setq mac-right-option-modifier 'none)
  ;; left and right controls are control
  (setq mac-control-modifier 'control)
  (setq mac-right-control-modifier 'left)
  ;; function key is hyper
  (setq mac-function-modifier 'hyper)
  (setq default-input-method "MacOSX")
  (setq insert-directory-program "gls")  ; dired works better with gls
  (setq default-directory (getenv "HOME")))

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

;; Stop C-z from minimizing windows under OS X
(when *is-a-windowed-mac*
  (unbind-key "C-z")
  (unbind-key "C-x C-z"))

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

;; (use-package gl-conf-mode
;;   :load-path "site-lisp/gl-conf-mode"
;;   :mode "gitolite\\.conf\\'")

(use-package git-modes)

(with-eval-after-load 'ediff
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))

;; (use-package magit-delta
;;   :after magit
;;   ;;:hook (magit-mode . magit-delta-mode)
;;   :config
;;   (setq magit-delta-delta-args (append magit-delta-delta-args '("--features" "magit-delta"))))

(use-package magit
  ;;:after selectrum
  :bind (("C-c C-g" . magit-status)
         ;;("C-x C-z" . magit-status-quick)
         ("C-c g" . magit-file-dispatch)
         ("C-c M-g" . magit-dispatch))
  :config
  (global-git-commit-mode +1)
  (setq git-commit-summary-max-length 70)
  (defun jccb/git-commit-mode-hook ()
    (turn-on-flyspell)
    (setq fill-column 70))
  (add-hook 'git-commit-mode-hook #'jccb/git-commit-mode-hook)

  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)
  (setq ;magit-completing-read-function #'selectrum-completing-read
   magit-bury-buffer-function #'magit-restore-window-configuration
   magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
   magit-no-confirm '(stage-all-changes unstage-all-changes discard resurrect)
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-topleft-v1
   magit-diff-refine-hunk 'all
   ;; magit-branch-adjust-remote-upstream-alist '(("origin/master" "master"))
   magit-branch-prefer-remote-upstream '("master" "main")
   magit-delete-by-moving-to-trash t
   magit-git-executable (executable-find magit-git-executable)
   magit-revision-insert-related-refs nil
   magit-save-repository-buffers nil))

(use-package forge
  :config
  ;; add to keychain:
  ;; security add-internet-password -a '{user}^forge' -r 'htps' -s "api.github.com"
  ;; https://github.com/magit/ghub/issues/101
  (add-to-list 'auth-sources 'macos-keychain-internet)
  (setq  forge-topic-list-limit '(100 . -10))
  :after magit)

(use-package magit-todos
  :after magit)

(use-package git-gutter
  :commands git-gutter-mode
  :init
  (defun jccb/git-gutter-mode-if-local ()
    (unless (file-remote-p default-directory)
      (git-gutter-mode)))
  (add-hook 'prog-mode-hook #'jccb/git-gutter-mode-if-local)
  (add-hook 'text-mode-hook #'jccb/git-gutter-mode-if-local)
  (add-hook 'config-mode-hook #'jccb/git-gutter-mode-if-local))

(use-package git-timemachine
  :commands git-timemachine)

;; use regexp isearch by default
(bind-key [remap isearch-forward] #'isearch-forward-regexp)
(bind-key [remap isearch-backward] #'isearch-backward-regexp)

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(use-package jccb-search
  :straight nil
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
  :hook (after-init . global-anzu-mode)
  :bind (([remap query-replace-regexp] . anzu-query-replace)
         ([remap query-replace] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)
         ([remap isearch-query-replace] . anzu-isearch-query-replace-regexp)))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;; (use-package scss-mode
;;   :config
;;   (setq scss-compile-at-save nil))

(use-package dired
  :straight nil
  :ensure nil
  :bind (("C-x C-j" . dired-jump))
  :hook (dired-mode . dired-collapse-mode)
  :init
  (setq dired-listing-switches "--time-style long-iso -alhF --group-directories-first"
        dired-auto-revert-buffer t
        dired-kill-when-opening-new-dired-buffer t
        dired-hide-details-hide-symlink-targets nil
        dired-recursive-copies 'always)
  (setq-default diredp-hide-details-initially-flag nil
                dired-dwim-target t))

(use-package dired-x
  :straight nil
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

(use-package dired+
  :after dired
  :config)

(use-package dired-imenu
  :after dired)

(use-package dired-collapse
  :commands dired-collapse-mode)

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



(use-package visual-fill-column
  :commands visual-fill-column-mode
  :init
  (setq  visual-fill-column-width 80
         visual-fill-column-center-text t))

(use-package markdown-mode
  :hook ((markdown-mode . turn-on-flyspell)
         (markdown-mode . visual-line-mode)
         (markdown-mode . visual-fill-column-mode-map)
         (markdown-mode . jccb/markdown-setup))
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (defun jccb/markdown-setup ()
    (message "jccb-mdown mode")
    (turn-on-flyspell)
    (visual-line-mode +1)
    (visual-fill-column-mode +1)
    (dolist (face '((markdown-header-face-1 . 1.3)
                    (markdown-header-face-2 . 1.2)
                    (markdown-header-face-3 . 1.1)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))
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
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-c c r"       . set-rectangular-region-anchor)
         ("C-c c t"       . mc/mark-sgml-tag-pair)
         ("C-c c c"       . mc/edit-lines)
         ("C-c c e"       . mc/edit-ends-of-lines)
         ("C-c c a"       . mc/edit-beginnings-of-lines)))

(use-package change-inner
  :bind (("C-c i" . change-inner)
         ("C-c o" . change-outer)))

(use-package avy-zap
  :bind (("M-Z" . avy-zap-up-to-char-dwim)))

(use-package misc
  :straight nil
  :ensure nil
  :bind ("M-z" . zap-up-to-char))

(use-package jccb-misc
  :straight nil
  :ensure nil
  :defer 1
  :commands (chmod+x-this jccb/doctor)
  :bind (("M-p" . goto-match-paren)
         ("C-o" . jccb/open-next-line)
         ("C-M-o" . jccb/open-previous-line))
  :config (jccb/doctor))

(use-package crux
  :commands crux-find-shell-init-file
  :bind (("C-a"          . crux-move-beginning-of-line)
         ("S-<return>"   . crux-smart-open-line)
         ("C-S-k"        . crux-kill-whole-line)
         ("C-S-<return>" . crux-smart-open-line-above))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-reopen-as-root-mode +1))

(use-package jccb-windows
  :straight nil
  :ensure nil
  :bind (("C-x |" . split-window-horizontally-instead)
         ("C-x _" . split-window-vertically-instead)
         ;; ("C-2"   . split-window-vertically-with-other-buffer)
         ;; ("C-3"   . split-window-horizontally-with-other-buffer)
         ("M-o" . quick-switch-buffer)))

(use-package window
  :straight nil
  :ensure nil
  :bind (("C-0"            . delete-window)
         ("C-1"            . delete-other-windows)
         ("C-2"            . split-window-below)
         ("C-3"            . split-window-right)
         ("C-;"            . other-window)
         ("S-C-<left>"     . shrink-window-horizontally)
         ("S-C-<right>"    . enlarge-window-horizontally)
         ("S-C-<down>"     . shrink-window)
         ("S-C-<up>"       . enlarge-win1dow)
         ("C-x <C-return>" . window-swap-states))
  :init
  (unbind-key "C-x o"))

; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face '(:inherit (success bold)))
  (define-ibuffer-column size
    (:name "Size"
           :inline t
           :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

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
  :bind ("M-\\" . shrink-whitespace))

;; (use-package beacon
;;   :defer 2
;;   :config
;;   (setq beacon-color "#6F6F6F"
;;         beacon-blink-when-focused  t)
;;   (defun not-display-graphic-p ()
;;     (not (display-graphic-p)))
;;   (add-hook 'beacon-dont-blink-predicates #'not-display-graphic-p)
;;   (beacon-mode))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package drag-stuff
  :hook ((prog-mode text-mode conf-mode) . turn-on-drag-stuff-mode)
  :config
  (setq drag-stuff-modifier '(meta super))
  (drag-stuff-define-keys))

;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :hook ((prog-mode text-mode conf-mode) . whole-line-or-region-local-mode)
  :config
  (whole-line-or-region-global-mode +1))

(use-package ispell
  :straight nil
  :ensure nil
  ;; :bind ("C-." . jccb/cycle-ispell-languages)
  :config
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"
                            "--run-together"))

  (defvar jccb/ispell-langs (make-ring 2))
  (ring-insert jccb/ispell-langs "american")
  (ring-insert jccb/ispell-langs "castellano8")

  (defun jccb/cycle-ispell-languages ()
    (interactive)
    (let ((lang (ring-ref jccb/ispell-langs -1)))
      (ring-insert jccb/ispell-langs lang)
      (ispell-change-dictionary lang)
      (flyspell-buffer)
      (message "Spell language changed to %s" lang))))

(use-package flyspell
  :straight nil
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         ((prog-mode conf-mode yaml-mode) . flyspell-prog-mode))
  :config
  (unbind-key "C-;" flyspell-mode-map)
  (unbind-key "C-." flyspell-mode-map)
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-:" . flyspell-correct-wrapper)))

(use-package ripgrep
  :commands ripgrep-regexp)

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-mode-line
        '(:eval
          (format " Prj:%s"
                  (projectile-project-name))))

  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'auto)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  (when (executable-find "fd")
    (let ((fd-command "fd . --type f --print0 --color=never"))
      (setq projectile-hg-command fd-command)
      (setq projectile-git-command fd-command)
      (setq projectile-fossil-command fd-command)
      (setq projectile-bzr-command fd-command)
      (setq projectile-darcs-command fd-command)
      (setq projectile-svn-command fd-command)
      (setq projectile-generic-command fd-command)))
  (projectile-mode +1)
  (setq projectile-require-project-file nil))

(use-package dumb-jump
  :config
  (add-to-list 'xref-backend-functions 'dumb-jump-xref-activate t)
  (setq dumb-jump-selector 'completing-read)
  (setq dumb-jump-prefer-searcher 'rg))

;; (use-package company
;;   :defer 2
;;   :commands (company-mode company-indent-or-complete-common)
;;   :init
;;   (add-hook 'prog-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "<tab>")
;;                              #'company-indent-or-complete-common)))
;;   :config
;;   (setq company-idle-delay 0.6
;;         company-show-numbers t
;;         company-tooltip-limit 20
;;         company-tooltip-align-annotations t
;;         company-minimum-prefix-length 1
;;         company-dabbrev-downcase nil)
;;   (global-company-mode)
;;   ;; use numbers 0-9 to select company completion candidates
;;   (let ((map company-active-map))
;;     (mapc (lambda (x) (define-key map (format "%d" x)
;;                         `(lambda () (interactive) (company-complete-number ,x))))
;;           (number-sequence 0 9))))

;; (use-package company-terraform
;;   :after (company terraform-mode)
;;   :disabled t
;;   :config
;;   (add-to-list 'company-backends 'company-terraform))

;; ;; (use-package company-quickhelp
;; ;;  :hook
;; ;;  (global-company-mode . company-quickhelp))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   ;; https://github.com/sebastiencs/company-box/issues/44
;;   (defun jccb/fix-company-scrollbar (orig-fn &rest args)
;;     "disable company-box scrollbar"
;;     (cl-letf (((symbol-function #'display-buffer-in-side-window)
;;                (symbol-function #'ignore)))
;;       (apply orig-fn args)))

;;   (advice-add #'company-box--update-scrollbar :around #'jccb/fix-company-scrollbar))

(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :commands yas-hippie-try-expand
  :init
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  :config
  (yas-reload-all))

(use-package consult-yasnippet
  :after yasnippet)

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
  :hook (terraform-mode . terraform-format-on-save-mode))

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

;; (use-package jump-char
;;   :config
;;   :bind (("M-m" . jump-char-forward)
;;          ("M-M" . jump-char-backward)))

(use-package pip-requirements
  :mode "requirements\\.txt\\'")

;; (use-package paradox
;;   :commands paradox-list-packages
;;   :config
;;   (setq paradox-column-width-package 40))

(use-package highlight-symbol
  :commands (highlight-symbol
             highlight-symbol-query-replace
             highlight-symbol-occur))

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'"   . ssh-config-mode)
         ("/known_hosts\\'"     . ssh-known-hosts-mode)
         ("/authorized_keys\\'" . ssh-authorized-keys-mode)))

(use-package avy
  :bind (("M-g g"   . avy-goto-line)
         ("C-c C-j" . avy-resume)
         ("C-c C-n" . avy-next)
         ("C-c C-p" . avy-prev)
         ("C-'"     . avy-goto-char)
         ("C-\""    . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 0.6)
  (avy-setup-default))

;; (use-package ace-window
;;   :bind ("C-x o" . ace-window)
;;   :config
;;   (setq aw-keys '(?a ?s ?d ?f ? ?j ?k ?l))
;;   (setq aw-dispatch-always nil))

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


;; load additional local settings (if they exist)
(use-package jccb-local
  :straight nil
  :ensure nil
  :load-path "site-lisp"
  :if (file-exists-p (emacs-path "site-lisp/jccb-local.el")))

(use-package server
  :if (display-graphic-p)
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

;; (defun doom/escape ()
;;   "Run `doom-escape-hook'."
;;   (interactive)
;;   (cond ((minibuffer-window-active-p (minibuffer-window))
;;          ;; quit the minibuffer if open.
;;          (abort-recursive-edit))
;;         ;; don't abort macros
;;         ((or defining-kbd-macro executing-kbd-macro) nil)
;;         ;; Back to the default
;;         ((keyboard-quit))))

;; (bind-key [remap keyboard-quit] #'doom/escape)

;; Don't resize windows & frames in steps; it's prohibitive to prevent the user
;; from resizing it to exact dimensions, and looks weird.
(setq window-resize-pixelwise nil ; jccb: t breaks org-fast-tag-insert with doom-modeline
      frame-resize-pixelwise t)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

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
  :commands restart-emacs
  :config
  (defun jccb/disable-confirm-kill-emacs (&rest _)
    (setq confirm-kill-emacs nil))
  (advice-add 'restart-emacs :before #'jccb/disable-confirm-kill-emacs))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO" warning bold)
          ("FIXME" error bold)
          ("HACK" font-lock-constant-face bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("NOTE" success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package highlight-indent-guides
  :disabled
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :commands highlight-indent-guides-mode
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top-edge))

;; (add-hook 'completion-list-mode-hook #'hide-mode-line-mode)
(add-hook 'Man-mode-hook #'hide-mode-line-mode)

;; (use-package vterm
;;   :disabled t
;;   :hook (vterm-mode . hide-mode-line-mode)
;;   :commands vterm)

(use-package imenu-list
  :commands imenu-list-minor-mode)

(use-package copy-as-format
  :commands copy-as-format)

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package ialign
  :commands ialign)

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

;; (use-package clipetty
;;   :commands (global-clipetty-mode clipetty-kill-ring-save))

;; (use-package guru-mode
;;   :hook (after-init . guru-global-mode)
;;   :config
;;   (setq guru-warn-only nil))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode))
  :mode (("\\.tsx\\'" . typescript-mode)))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package lsp-mode
  :after (orderless)
  :commands (lsp lsp-deferred)
  :custom (lsp-completion-provider :none)
  :hook (((typescript-mode js2-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . jccb/corfu-lsp-setup))
  :bind (:map corfu-map
              ("M-m" . corfu-move-to-minibuffer))
  :init
  (setq read-process-output-max (* 1024 1024)
        lsp-keymap-prefix "C-c l")

  (defun jccb/corfu-lsp-setup ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

  :config
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  (setq lsp-headerline-breadcrumb-enable nil
        lsp-signature-render-documentation nil
        ;; lsp-eldoc-enable-hover nil
        ;; lsp-eldoc-render-all nil
        lsp-lens-enable nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 1
        lsp-ui-doc-delay 2
        ;; lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicates t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-alignment 'frame
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-childframe t))


(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

;; (use-package lsp-jedi)

;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     (add-to-list 'lsp-enabled-clients 'jedi)))

;; (use-package goto-line-preview
;;   :bind ([remap goto-line] . goto-line-preview))

(use-package try
  :commands try)

(use-package color-identifiers-mode
  :commands color-identifiers-mode)

(use-package py-isort
  :commands py-isort-buffer)

;; (use-package blacken
;;   :hook (python-mode . blacken-mode)
;;   :commands blacken-mode blacken-buffer)

(use-package format-all
  ;; :hook (python-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("Python" yapf))))

(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode +1))

(use-package keypression
  :commands keypression-mode)

(use-package command-log-mode
  :commands command-log-mode)

(use-package focus
  :commands focus-mode
  :config
  (add-to-list 'focus-mode-to-thing '(python-mode . paragraph)))

(use-package dimmer
  :hook (after-init . dimmer-mode)
  :config
  (setq dimmer-fraction 0.25)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  ;;(dimmer-configure-org)
  )

;; (use-package selectrum-prescient
;;   :commands selectrum-prescient-mode
;;   :config
;;   (setq prescient-filter-method '(literal regexp fuzzy))
;;   (setq prescient-history-length 1000))

(use-package marginalia
  :commands marginalia-mode
  :bind (:map minibuffer-local-map ("C-M-a" . marginalia-cycle))
  :config
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit))))
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package vertico
  :init
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))
  (vertico-mode +1)
  (marginalia-mode +1)
  (setq vertico-count 15)
  (setq vertico-cycle nil))

(use-package vertico-extensions
  :straight (vertico-extensions
             :host github
             :repo "minad/vertico"
             :local-repo "vertico-extensions"
             :files ("extensions/vertico-directory.el"
                     ;; "extensions/vertico-flat.el"
                     ;; "extensions/vertico-grid.el"
                     ;; "extensions/vertico-reverse.el"
                     ;; "extensions/vertico-multiform.el"
                     ;; "extensions/vertico-unobtrusive.el"
                     "extensions/vertico-quick.el"
                     "extensions/vertico-repeat.el"))
  :after vertico
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy)
         (minibuffer-setup . vertico-repeat-save))
  :bind ("C-c C-r" . vertico-repeat)
  :bind (:map vertico-map
              ;; ("M-V" . vertico-multiform-vertical)
              ;; ("M-G" . vertico-grid-mode)
              ;; ("M-F" . vertico-flat-mode)
              ;; ("M-R" . vertico-reverse-mode)
              ;; ("M-U" . vertico-unobtrusive-mode)
              ("M-q" . vertico-quick-insert)
              ("C-q" . vertico-quick-exit)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.75)
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;; (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  :bind (:map corfu-map
              ("C-a" . corfu-beginning-of-prompt)
              ("C-e" . corfu-end-of-prompt))
  :init
  (defun corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))

  (defun corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))

  (corfu-global-mode))

(use-package dabbrev
  :bind (;;("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :init
  (setq dabbrev-check-all-buffers t
        dabbrev-check-other-buffers t))

(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c / p" . completion-at-point) ;; capf
         ;; ("C-c p t" . complete-tag)        ;; etags
         ;; ("C-c / d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c / f" . cape-file)
         ("C-c / k" . cape-keyword)
         ("C-c / s" . cape-symbol)
         ;; ("C-c p a" . cape-abbrev)
         ("C-c / i" . cape-ispell)
         ("M-/"     . cape-dabbrev)
         ;; ("C-c p l" . cape-line)
         ;; ("C-c p w" . cape-dict)
         ;; ("C-c p \\" . cape-tex)
         ;; ("C-c p _" . cape-tex)
         ;; ("C-c p ^" . cape-tex)
         ;; ("C-c p &" . cape-sgml)
         ;; ("C-c p r" . cape-rfc1345)
         )
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(setq completion-cycle-threshold 3)
(setq read-extended-command-predicate
      #'command-completion-default-include-p)
(setq tab-always-indent 'complete)

(use-package orderless
  :config
  ;; orderless-flex is probably more "flex" but this makes
  ;; highlighting easier to understand (at least for me)
  (defun jccb/orderless-flex-non-greedy (component)
    (orderless--separated-by
     '(minimal-match (zero-or-more nonl))
     (cl-loop for char across component collect char)))

  (defun jccb/orderless-dispatcher (pattern _index _total)
    (cond ((string-suffix-p "!" pattern)
           `(orderless-without-literal . ,(substring pattern 0 -1)))
          ((string-suffix-p "~" pattern)
           `(orderless-regexp . ,(substring pattern 0 -1)))
          ((string-suffix-p "\\" pattern)
           `(orderless-initialism . ,(substring pattern 0 -1)))
          ((string-suffix-p "/" pattern)
           `(orderless-prefixes . ,(substring pattern 0 -1)))
          ((string-suffix-p "$" pattern)
           `(orderless-regexp . ,pattern))
          ((string-prefix-p "^" pattern)
           `(orderless-regexp . ,pattern))
          ((string-suffix-p "=" pattern)
           `(orderless-literal . ,(substring pattern 0 -1)))))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  (setq orderless-matching-styles '(jccb/orderless-flex-non-greedy)
        orderless-style-dispatchers '(jccb/orderless-dispatcher)))



;; (use-package selectrum
;;   :hook (after-init . jccb/selectrum-setup)
;;   :bind ("C-c C-r" . selectrum-repeat)
;;   :commands selectrum-mode
;;   :config
;;   (defun jccb/selectrum-setup ()
;;     (setq selectrum-prescient-enable-filtering nil)
;;     (setq selectrum-refine-candidates-function #'orderless-filter)
;;     (setq orderless-skip-highlighting (lambda () selectrum-is-active))
;;     (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
;;     (setq completion-styles '(orderless))
;;     (selectrum-mode +1)
;;     (selectrum-prescient-mode +1)
;;     (prescient-persist-mode +1)
;;     (marginalia-mode +1))
;;   (setq selectrum-fix-vertical-window-height 15
;;         ;; selectrum-num-candidates-displayed 15
;;         selectrum-extend-current-candidate-highlight t
;;         selectrum-show-indices nil))

(use-package consult-flycheck
  :commands consult-flycheck)

;; (use-package consult-lsp
;;   :commands (consult-lsp-symbols consult-lsp-diagnostics consult-lsp-file-symbols))

(use-package consult
  :bind (("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ;; ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r x"  . consult-register)
         ("C-x r b"  . consult-bookmark)

         ("M-g f"    . consult-flycheck)
         ("M-g g"    . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g"  . consult-goto-line)           ;; orig. goto-line
         ("M-g o"    . consult-outline)
         ("M-g m"    . consult-mark)
         ("M-g k"    . consult-global-mark)
         ("M-g e"    . consult-compile-error)
         ("M-i"      . consult-imenu)
         ;; M-s bindings (search-map)
         ("M-s r" . consult-ripgrep)
         ;; ("M-s f" . consult-fd)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; ("M-s e" . consult-isearch)
         ("<help> a" . consult-apropos))
  :commands consult-ref
  :init
  (fset 'multi-occur #'consult-multi-occur)
  (fset 'projectile-ripgrep #'consult-ripgrep)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  (setq consult-narrow-key "`"
        register-preview-delay 0
        consult-preview-key (kbd "<C-return>")
        register-preview-function #'consult-register-format)

  (consult-customize
   consult-goto-line :preview-key 'any)

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))
  ;; (defun reduce-which-key-delay (fun &rest args)
  ;;   (let ((timer (and consult-narrow-key
  ;;                     (memq :narrow args)
  ;;                     (run-at-time 0.01 0.01
  ;;                                  (lambda ()
  ;;                                    (when (eq last-input-event (elt consult-narrow-key 0))
  ;;                                      (which-key--start-timer 0.001)))))))
  ;;     (unwind-protect
  ;;         (apply fun args)
  ;;       (when timer
  ;;         (cancel-timer timer)
  ;;         (which-key--start-timer)))))
  ;; (advice-add #'consult--read :around #'reduce-which-key-delay)

  (advice-add #'register-preview :override #'consult-register-window)
  (set-face-attribute 'consult-file nil :inherit 'doom-modeline-buffer-file)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-f" . consult-dir-jump-file)
         ("C-x C-d" . consult-dir)))

(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package org-plus-contrib
;;   :pin org)

;; (use-package org
;;   :ensure org-plus-contrib
;;   :pin org
;;   ;; :hook (org-mode . org-indent-mode)
;;   :commands org-agenda org-capture org-buffer-list
;;   :bind (("<f12> a"     . org-agenda)
;;          ("<f12> c"     . org-capture)
;;          ("<f12> b"     . org-switchb)
;;          ("<f12> <f11>" . jccb/org-agenda-switch-to-buffer)
;;          ("<f12> <f12>" . jccb/capture-task)
;;          ("<f12> w"     . jccb/org-agenda-work)
;;          ("<f12> p"     . jccb/org-agenda-personal)
;;          ("<f12> l"     . org-store-link)
;;          :map org-agenda-mode-map
;;          ("y" . org-agenda-todo-yesterday))
;;   :init
;;   (setq org-modules '(org-habit))
;;   :config
;;   (defun jccb/capture-task () (interactive) (org-capture nil "t"))
;;   (defun jccb/org-agenda-work () (interactive) (org-agenda nil "w"))
;;   (defun jccb/org-agenda-personal () (interactive) (org-agenda nil "p"))
;;   (defun jccb/org-agenda-switch-to-buffer () (interactive) (switch-to-buffer "*Org Agenda*"))

;;   (org-load-modules-maybe t)
;;   (advice-add 'org-refile :after 'org-save-all-org-buffers)
;;   ;; (add-hook 'org-agenda-finalize-hook 'hide-mode-line-mode)
;;   (setq org-agenda-span 7
;;         org-agenda-start-with-log-mode t
;;         org-special-ctrl-a/e t
;;         org-special-ctrl-k t
;;         org-use-fast-todo-selection t
;;         org-log-done 'time
;;         org-log-into-drawer t
;;         org-catch-invisible-edits 'show-and-error
;;         org-habit-graph-column 50
;;         org-habit-show-habits-only-for-today t
;;         org-habit-show-all-today nil
;;         org-habit-following-days 3
;;         org-habit-show-done-always-green t
;;         org-directory "~/org/"
;;         org-default-notes-file "~/org/notes.org"
;;         org-agenda-files (list org-directory)
;;         org-agenda-start-on-weekday nil
;;         org-ellipsis "…"
;;         org-agenda-use-time-grid nil
;;         org-refile-targets '((nil :maxlevel . 2)
;;                              (org-agenda-files :maxlevel . 2))
;;         org-outline-path-complete-in-steps nil
;;         org-refile-use-outline-path 'file)

;;   (setq org-todo-keywords
;;         '((sequence "TODO(t)" "NEXT(n!)" "IN-PROGRESS(i!)" "|" "DONE(d!)")
;;           (sequence "SOMETIME(s)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)")))

;;   (setq org-todo-keyword-faces
;;         `(("TODO"        . ,(doom-color 'yellow))
;;           ("IN-PROGRESS" . ,(doom-color 'orange))
;;           ("NEXT"        . ,(doom-color 'red))
;;           ("SOMETIME"    . ,(doom-color 'base7))
;;           ("WAITING"     . ,(doom-color 'base6))
;;           ("HOLD"        . ,(doom-color 'base5))
;;           ("DONE"        . ,(doom-color 'green))
;;           ("CANCELED"    . ,(doom-color 'green)))
;;         org-priority-faces
;;         `((?A . ,(doom-color 'base8))
;;           (?B . ,(doom-color 'base7))
;;           (?C . ,(doom-color 'base6))))

;;   (setq org-tag-alist '(("work"     . ?w)
;;                         ("personal" . ?p)
;;                         ("kb"       . ?k)
;;                         ("idea"     . ?i)
;;                         ("learn"    . ?l)))

;;   (setq jccb/org-todo-sort-order '("NEXT"
;;                                    "IN-PROGRESS"
;;                                    "TODO"
;;                                    "WAITING"
;;                                    "HOLD"
;;                                    "SOMETIME"
;;                                    "DONE"
;;                                    "CANCELED"))
;;   (defun jccb/org-sort (a b)
;;     (when-let ((state-a (get-text-property 14 'todo-state a))
;;                (state-b (get-text-property 14 'todo-state b))
;;                (cmp (--map (cl-position-if (lambda (x)
;;                                              (equal x it))
;;                                            jccb/org-todo-sort-order)
;;                            (list state-a state-b))))
;;       (cond ((apply '> cmp) 1)
;;             ((apply '< cmp) -1)
;;             (t nil))))
;;   (setq org-agenda-cmp-user-defined #'jccb/org-sort
;;         org-agenda-sorting-strategy '(user-defined-up priority-down timestamp-up))

;;   (setq org-agenda-custom-commands
;;         '(("w" "Work Agenda"
;;            ((agenda "" nil)
;;             (tags-todo "-habit"
;;                        ((org-agenda-overriding-header "Tasks"))))
;;            ((org-agenda-tag-filter-preset '("+work" "-xhome"))))
;;           ("p" "Personal Agenda"
;;            ((agenda "" nil)
;;             (tags-todo "-habit"
;;                        ((org-agenda-overriding-header "Tasks"))))
;;            ((org-agenda-tag-filter-preset '("-work" "-xhome"))
;;             (org-habit-show-habits-only-for-today t)
;;             (org-habit-show-all-today t)
;;             (org-agenda-span 1)))))

;;   ;; make org work with shackle
;;   (defun org-switch-to-buffer-other-window (&rest args)
;;     (apply 'switch-to-buffer-other-window args)))

(use-package so-long
  :hook (after-init . global-so-long-mode))

(use-package dtrt-indent
  :hook (after-init . dtrt-indent-global-mode))

;; (use-package go-mode)

(use-package super-save
  :hook (after-init . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t))

(use-package darkroom
  :commands (darkroom-mode darkroom-tentative-mode)
  :config
  (setq darkroom-text-scale-increase 1.1))

(use-package puni
  :hook ((after-init . puni-global-mode)
         (term-mode . puni-disable-puni-mode))
  :bind (:map puni-mode-map
              (;; puni-raise
               ;; puni-split
               ;; puni-splice
               ;; puni-squeeze
               ;; puni-transpose
               ;; puni-convolute
               ("C-{"   . puni-slurp-backward)
               ("C-}"   . puni-barf-backward)
               ("M-C-{" . puni-barf-forward)
               ("M-C-}" . puni-slurp-forward))))

;; (use-package shackle
;;   :defer 1
;;   :config
;;   (setq
;;    shackle-inhibit-window-quit-on-same-windows t
;;    shackle-rules
;;    '((helpful-mode   :select t :same t)
;;      ("*Org Agenda*" :select t :same t)))
;;   (shackle-mode t))

;; (use-package org-superstar
;;   :after org
;;   :hook (org-mode . org-superstart-mode))

;; ;; Cycle completion on smaller number of candidates
;; (setq completion-cycle-threshold 5)

;; ;;; Don't show help for completions
;; (setq completion-show-help nil)


;; (setq tramp-ssh-controlmaster-options  "-o ControlPath=~/.ssh/tmp/master-%%C -o ControlMaster=auto -o ControlPersist=yes")

;; use-package seq: init -> config
(when init-file-debug
  (use-package-report))
