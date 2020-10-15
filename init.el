(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      inhibit-compacting-font-caches t
      message-log-max 16384
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'emacs-startup-hook
          `(lambda ()
             (dolist (handler file-name-handler-alist)
               (add-to-list 'file-name-handler-alist-old handler))
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold (* 256 1024 1024)
                   gc-cons-percentage 0.1)) t)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Put this file in register e for easy access
(set-register ?e `(file . ,user-init-file))

;;==================================================
;; Setup basic stuff
;;==================================================

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-windowed-mac* (and *is-a-mac* window-system))
(defvar jc-interactive-mode (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")

;; Turn off mouse interface early in startup to avoid momentary display
(unless *is-a-windowed-mac* ; hide menu if not in Mac
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Less noise at startup. The dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Keep emacs custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;; (use-package benchmark-init
;;   :when init-file-debug
;;   :demand t
;;   :hook (window-setup . benchmark-init/deactivate))

(use-package dash  :defer t)
(use-package f     :defer t)
(use-package s     :defer t)

(use-package jc-doom
  :ensure nil
  :load-path "site-lisp")

(use-package gcmh
  :if jc-interactive-mode
  :hook (emacs-startup . gcmh-mode)
  :config
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold (* 256 1024 1024)
        gcmh-low-cons-threshold (* 64 1024 1024)
        gcmh-verbose nil))

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


;; (use-package centaur-tabs
;;   :init
;;   (setq centaur-tabs-style "bar"
;;         centaur-tabs-height 32
;;         centaur-tabs-gray-out-icons 'buffer
;;         centaur-tabs-set-icons t
;;         centaur-tabs-set-bar 'under
;;         centaur-tabs-set-close-button t
;;         centaur-tabs-icon-scale-factor 0.7
;;         centaur-tabs-close-button "✕"
;;         centaur-tabs-show-navigation-buttons nil
;;         centaur-tabs-set-modified-marker nil
;;         centaur-tabs-modified-marker "⬤")
;;   :config
;;   (centaur-tabs-headline-match)
;;   (centaur-tabs-mode +1)
;;   (after! undo-tree (add-hook 'undo-tree-visualizer-mode-hook #'centaur-tabs-local-mode)))

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
  (tooltip-mode nil)
  (blink-cursor-mode nil))

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
      select-enable-clipboard t          ; Allow pasting selection outside of Emacs
      global-auto-revert-non-file-buffers t ; auto refresh dired
      auto-revert-verbose nil               ; and be quiet about it
      eval-expression-print-level nil
      echo-keystrokes 0.02                  ; Show keystrokes in progress
      confirm-kill-emacs 'yes-or-no-p       ; ask me before closing
      history-length t                      ; looong history
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
;(setq-default visible-bell t)
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
                                         try-complete-lisp-symbol))
(bind-key "M-/" #'hippie-expand)

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
  (setq backup-by-copying t            ; don't clobber symlinks
        delete-old-versions t          ; delete old backups
        kept-new-versions 6
        kept-old-versions 2
        version-control t         ; use versioned backups
        create-lockfiles nil
        ;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
        auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "backups/") t))
        backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))))


;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

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

;; (use-package highlight-parentheses
;;   :config
;;   (global-highlight-parentheses-mode t))

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
        undo-outer-limit 120000000
        undo-tree-history-directory-alist
        `((".*" . ,(concat user-emacs-directory "undo-list")))))

;; Save a list of recent files visited.
(use-package recentf
  :hook (after-init . recentf-mode)
  ;; :bind ("C-x f" . recentf-ido-find-file)
  ;; :commands recentf-open-files
  :config
  (setq recentf-max-saved-items 500
        recentf-save-file (expand-file-name ".recentf" user-emacs-directory)
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-exclude '("/tmp/" "/ssh:"))
  (add-hook! 'dired-mode-hook
    (defun doom--recentf-add-dired-directory-h ()
      "Add dired directory to recentf file list."
      (recentf-add-file default-directory))))

(use-package elec-pair
  :hook (after-init . electric-pair-mode))

(setq history-length 1000)

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (expand-file-name ".savehist" user-emacs-directory)
        savehist-save-minibuffer-history t
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
  :hook (after-init . save-place-mode)
  :config
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

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
  (set-face-font 'default "Inconsolata 15"))

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
  :mode ("\\.gitignore\\'" . gitignore-mode))

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
  ((prog-mode text-mode conf-mode) . git-gutter-mode))

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
                                    (t "xdg-open")))
  )

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
  :bind ("M-SPC" . shrink-whitespace))

;; (use-package beacon
;;   :defer 2
;;   :config
;;   (setq beacon-color "#6F6F6F"
;;         beacon-blink-when-focused  t)
;;   (defun not-display-graphic-p ()
;;     (not (display-graphic-p)))
;;   (add-hook 'beacon-dont-blink-predicates #'not-display-graphic-p)
;;   (beacon-mode))

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

;;==================================================
;; mark customizations
;;==================================================
(use-package jc-marks
  :ensure nil
  :commands exchange-point-and-mark-no-activate
  :bind (("C-`" . push-mark-no-activate)
         ("M-`" . jump-to-mark)))


;;==================================================
;; smex settings
;;==================================================

;; (use-package amx
;;   :after ivy
;;   :config
;;   (amx-mode +1))

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
            #'(lambda ()
                (local-set-key (kbd "<tab>")
                               #'company-indent-or-complete-common)))
  :config
  (setq company-idle-delay 0.2
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

;; (use-package yasnippet
;;   :commands yas-hippie-try-expand
;;   :init
;;   (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
;;   :config
;;   (yas-global-mode +1))

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
         ;; ("C-'" . avy-goto-char)
         ("C-c C-j" . avy-resume)
         ("C-c C-n" . avy-next)
         ("C-c C-p" . avy-prev)
         ("C-'" . avy-goto-char-timer))
  :config
  (setq avy-timeout-seconds 0.25)
  (avy-setup-default))

(use-package ace-window
  :bind ([remap other-window] . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always nil))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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

(setq jc-local-settings
      (expand-file-name "jc-local.el" user-emacs-directory))

(if (file-exists-p jc-local-settings)
    (load-file jc-local-settings))

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

(use-package hydra
  :defer t)

(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))

(use-package try
  :commands try)

;; (use-package tramp
;;   :ensure nil
;;   :defer t
;;   :config
;;   (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;;   ;; TRAMP gcloud ssh
;;   (add-to-list 'tramp-methods
;;                '("gcssh"
;;                  (tramp-login-program        "gcloud compute ssh")
;;                  (tramp-login-args           (("%h")))
;;                  (tramp-async-args           (("-q")))
;;                  (tramp-remote-shell         "/bin/bash")
;;                  (tramp-remote-shell-args    ("-c"))
;;                  (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
;;                                               ("-o" "UserKnownHostsFile=/dev/null")
;;                                               ("-o" "StrictHostKeyChecking=no")))
;;                  (tramp-default-port         22)))

;;   (add-to-list 'tramp-methods
;;                '("gcsshiap"
;;                  (tramp-login-program        "gcloud compute ssh --tunnel-through-iap")
;;                  (tramp-login-args           (("%h")))
;;                  (tramp-async-args           (("-q")))
;;                  (tramp-remote-shell         "/bin/bash")
;;                  (tramp-remote-shell-args    ("-c"))
;;                  (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
;;                                               ("-o" "UserKnownHostsFile=/dev/null")
;;                                               ("-o" "StrictHostKeyChecking=no")))
;;                  (tramp-default-port         22))))

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
