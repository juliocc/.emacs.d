(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (dolist (handler file-name-handler-alist)
              (add-to-list 'file-name-handler-alist-old handler))
            (setq file-name-handler-alist file-name-handler-alist-old) t))

(setq load-prefer-newer t
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
(if (fboundp 'fringe-mode) (fringe-mode 4))

;; (setq tooltip-use-echo-area t)

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
(setq native-comp-async-report-warnings-errors 'silent)
;; try to remove old compilation caches after 10 seconds idle
(run-with-idle-timer 10 nil #'native-compile-prune-cache)

;;==================================================
;; Setup package management tools
;;==================================================

(unless (fboundp 'url-insert-buffer-contents)
  (require 'url-handlers))

(setq package-enable-at-startup nil
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

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(setq use-package-enable-imenu-support t)
(setq use-package-always-ensure t)

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(elpaca-wait)

(use-package no-littering
  :ensure (:wait t)
  :config
  (no-littering-theme-backups)
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (when (file-exists-p custom-file)
    (load-file custom-file)))

(use-package gcmh
  :hook (elpaca-after-init . gcmh-mode)
  :config
  (setq gcmh-idle-delay 'auto
        gc-cons-percentage 0.2
        ;; gcmh-auto-idle-delay-factor 10
        ;; gcmh-high-cons-threshold (* 32 1024 1024)
        gcmh-verbose jccb/debug))

;;==================================================
;; Appearance settings
;;==================================================

;; (use-package hide-mode-line
;;   :hook (Man-mode . hide-mode-line-mode))

(use-package nerd-icons)

(use-package ef-themes
  :config
  (setq modus-themes-prompts '(extrabold)
        ef-themes-mixed-fonts t
        ef-themes-to-toggle '(ef-maris-dark ef-maris-light)
        ef-themes-headings '((0 variable-pitch light 1.9)
                             (1 variable-pitch light 1.8)
                             (2 variable-pitch regular 1.7)
                             (3 variable-pitch regular 1.6)
                             (4 variable-pitch regular 1.5)
                             (5 variable-pitch 1.4) ; absence of weight means `bold'
                             (6 variable-pitch 1.3)
                             (7 variable-pitch 1.2)
                             (t variable-pitch 1.1))
        ef-themes-common-palette-overrides
        '((comment fg-dim)
          ;; (line-number-current-line red)
          ;; (bg-line-number-active unspecified)
          ))
  (load-theme 'ef-maris-dark)
  (set-face-attribute 'italic nil :slant 'normal))

(use-package doom-modeline
  :init
  (size-indication-mode +1)
  (column-number-mode +1)
  (setq doom-modeline-bar-width 0
        doom-modeline-buffer-file-name-style 'truncate-upto-project ;; 'buffer-name
        doom-modeline-indent-info t
        doom-modeline-total-line-number t
        doom-modeline-enable-word-count t)
  (setq doom-modeline-continuous-word-count-modes '(org-mode))
  ;; (unless after-init-time
  ;;   (setq-default mode-line-format nil))
  (doom-modeline-mode 1))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)
(setq-default cursor-type '(bar . 2))
;; (setq-default frame-background-mode 'dark)

(when (display-graphic-p)
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))
        icon-title-format frame-title-format)
  (blink-cursor-mode -1))

;; ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; abcdefghijklmnopqrstuvwxyz
;; 0123456789   !@#$¢%^&*~|
;; `'\"‘’“”.,;:  ()[]{}—-_+=<>
;; ()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
;; !ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
;; x×X .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO"

(let ((jccb/font-name "Iosevka SS04")
      (jccb/vp-font-name "Roboto")
      (jccb/font-size (if *is-a-windowed-mac* 170 150)))
  (if (member jccb/font-name (font-family-list))
      (progn (set-face-attribute 'default nil
                                 :family jccb/font-name
                                 :height jccb/font-size)
             (set-face-attribute 'fixed-pitch nil
                                 :family jccb/font-name)
             (set-face-attribute 'variable-pitch nil
                                 :family jccb/vp-font-name))
    (message "Can't find font %s" jccb/font-name)))

(global-font-lock-mode +1)

(use-package display-line-numbers
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
  :config
  (setq highlight-numbers-generic-regexp
        (rx symbol-start
            (optional (any "+-"))
            (or (seq (one-or-more digit)
                     (optional (seq "." (zero-or-more digit))))
                (seq "." (one-or-more digit)))
            (optional (or "e" "E")
                      (optional (or "+" "-"))
                      (one-or-more digit))
            symbol-end)))

;; (use-package highlight-escape-sequences
;;   :hook (prog-mode . hes-mode))

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
      scroll-margin 3
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

(pixel-scroll-precision-mode 1)

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(dolist (hook '(eshell-mode-hook term-mode-hook))
  (add-hook hook (lambda () (setq hscroll-margin 0
                                  hscroll-step 0
                                  scroll-margin 0))))
(when *is-a-mac*
  ;; sane trackpad/mouse scroll settings
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil))

(defvar jccb/fd-command (if *is-a-linux* "fdfind" "fd"))

;;==================================================
;; General settings
;;=================================================

;; UTF-8 everything please
(set-language-environment 'utf-8)

(setq mouse-yank-at-point t                 ; mouse pastes at point
      select-enable-clipboard t             ; Allow pasting selection outside of Emacs
      global-auto-revert-non-file-buffers t ; auto refresh dired
      auto-revert-verbose nil               ; and be quiet about it
      eval-expression-print-level nil
      echo-keystrokes 0.02                  ; Show keystrokes in progress
      history-length 2500                   ; looong history
      kill-ring-max 2500
      use-dialog-box nil                    ; never show a dialog box
      use-file-dialog nil
      mark-even-if-inactive t
      enable-recursive-minibuffers t        ; yes, please
      highlight-nonselected-windows t       ; show region even on inactive windows
      require-final-newline t               ; end files with a newline
      fill-column 80
      compilation-scroll-output t
      grep-highlight-matches t
      grep-use-headings t
      set-mark-command-repeat-pop t
      isearch-allow-scroll t
      blink-matching-paren-distance 51200
      confirm-nonexistent-file-or-buffer nil
      indicate-buffer-boundaries nil
      x-underline-at-descent-line t
      ;; idle-update-delay 2.0
      window-combination-resize t
      next-line-add-newlines nil            ; don't add new lines when scrolling down
      kill-read-only-ok t
      confirm-kill-processes nil
      kill-do-not-save-duplicates t
      disabled-command-function nil
      nobreak-char-display 0
      large-file-warning-threshold 100000000
      image-animate-loop t
      find-file-visit-truename t
      vc-follow-symlinks t
      ind-file-suppress-same-file-warnings t)

(setq confirm-kill-emacs (lambda (prompt)
                           (y-or-n-p-with-timeout prompt 2 nil)))


(setq auto-mode-case-fold nil)

;; ;; Disable warnings from legacy advice system. They aren't useful, and what can
;; ;; we do about them, besides changing packages upstream?
;; (setq ad-redefinition-action 'accept)

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

;; Never ever use tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)            ;; but maintain correct appearance

;; Disable bidirectional text rendering for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; confirm with y/n only
(if (version< emacs-version "28.1")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t))

;; don't confirm killing buffers with attached processes
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(use-package project
  :ensure nil)


;;==================================================
;; Completion
;;=================================================

(use-package savehist
  :ensure nil
  :config
  (dolist (var '(kill-ring
                 search-ring
                 regexp-search-ring))
    (add-to-list 'savehist-additional-variables var))
  (setq savehist-save-minibuffer-history t
        history-delete-duplicates t)
  (add-hook 'kill-emacs-hook
            (defun doom-unpropertize-kill-ring-h ()
              (setq kill-ring (cl-loop for item in kill-ring
                                       if (stringp item)
                                       collect (substring-no-properties item)
                                       else if item collect it))))
  (savehist-mode t))

;; (use-package typo
;;   :ensure (:host sourcehut :repo "pkal/typo"))

(use-package orderless
  :config
  (setq orderless-affix-dispatch-alist
        `(;;(?! . ,#'orderless-without-literal)
          (?! . ,#'orderless-not)
          (?/ . ,#'orderless-regexp)
          (?, . ,#'orderless-initialism)
          (?& . ,#'orderless-annotation)
          (?% . ,#'char-fold-to-regexp)
          (?= . ,#'orderless-literal)
          (?^ . ,#'orderless-prefixes)
          (?@ . ,#'orderless-literal-prefix)
          (?? . ,#'orderless-flex)))
  (defun basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))
  (add-to-list 'orderless-style-dispatchers #'orderless-kwd-dispatch)
  (setq completion-styles '(orderless basic)
        orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)
        orderless-component-separator 'orderless-escapable-split-on-space
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic-remote partial-completion)))))


(use-package vertico
  :after (savehist)
  ;;:commands vertico-mode
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :hook (minibuffer-setup . vertico-repeat-save)

  :bind (:map vertico-map
         ("M-q"   . vertico-quick-insert)
         ("C-q"   . vertico-quick-exit)
         ("S-SPC" . jccb/vertico-restrict-to-matches)
         ("RET"   . vertico-directory-enter)
         ("DEL"   . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ("C-M-n" . vertico-next-group)
         ("C-M-p" . vertico-previous-group))
  :bind (("C-c C-r" . vertico-repeat)
         ("M-S"     . vertico-suspend))
  :custom
  (vertico-count 25)
  (vertico-cycle t)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  :init
  (vertico-mode)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)

  (defun jccb/vertico-restrict-to-matches ()
    (interactive)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert " ")
      (add-text-properties (minibuffer-prompt-end) (point-max)
                           '(invisible t read-only t cursor-intangible t rear-nonsticky t))))

  (vertico-multiform-mode 1)
  (vertico-indexed-mode 1)
  :config
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-package corfu
  :commands global-corfu-mode
  :hook (minibuffer-setup . corfu-enable-always-in-minibuffer)
  :bind ("M-/" . completion-at-point)
  :bind (:map corfu-map
         ("SPC" . corfu-insert-separator)
         ;; ("C-l" . corfu-show-location)
         ;; ("C-a" . corfu-beginning-of-prompt)
         ;; ("C-e" . corfu-end-of-prompt)
         ("M-m" . corfu-move-to-minibuffer)
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous))

  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-min-width 50)
  (corfu-max-width 100)
  (corfu-count 20)
  (corfu-preview-current nil)
  :init
  (global-corfu-mode)

  (setq completion-cycle-threshold 3)
  ;;(setq completion-cycle-threshold nil)
  (setq tab-always-indent 'complete)
  ;; (setq tab-first-completion 'word-or-paren-or-punct)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

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

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)       ; Ensure auto completion is disabled
      (corfu-mode 1)))

  (set-face-attribute 'corfu-current nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'vertico-current)
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package cape
  ;; Bind dedicated completion commands
  :bind (("<f8> s" . cape-elisp-symbol)
         ("<f8> <f8>" . cape-dabbrev)
         ("<f8> k" . cape-keyword)
         ("<f8> f" . cape-file)
         ("<f8> t" . complete-tag)
         ("<f8> i" . cape-ispell)
         ("<f8> a" . cape-abbrev)
         ("<f8> l" . cape-line)
         ("<f8> w" . cape-dict))
  :commands cape-capf-buster
  :init
  (dolist (cape '(cape-file cape-keyword cape-dabbrev cape-elisp-symbol))
    (add-hook 'completion-at-point-functions cape)))

(use-package dabbrev
  :ensure nil
  ;;:bind ("C-M-/" . dabbrev-completion)
  :init
  (setq dabbrev-check-all-buffers t
        dabbrev-check-other-buffers t))

(use-package marginalia
  :after vertico
  :bind (:map vertico-map
         ("M-]" . marginalia-cycle))
  :config
  (setq marginalia_ellipsis "…"
        marginalia-align 'left
        marginalia-field-width 80
        marginalia-align-offset 0)
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; (global-display-fill-column-indicator-mode 1)
;; (set-face-attribute 'fill-column-indicator nil
;;                     :foreground "#717C7C")
;;                     ;;:background "transparent")

(use-package hippie-exp
  :ensure nil
  :bind ("C-M-/"   . hippie-expand)
  :init
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev-visible
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

;;==================================================
;; Editor
;;==================================================

(setq default-directory "~/")
(delete-selection-mode +1)
(global-auto-revert-mode +1)
(auto-compression-mode +1)
(transient-mark-mode +1)
;; (minibuffer-depth-indicate-mode +1) ;; replaced with recursion-indicator-mode
(electric-indent-mode -1)
;; (global-subword-mode 1)
(if (fboundp 'kill-ring-deindent-mode) (kill-ring-deindent-mode +1))

;; (bind-key "RET" #'newline-and-indent)

;; Stop C-z from minimizing windows under OS X
(when *is-a-windowed-mac*
  (unbind-key "C-z")
  (unbind-key "C-x C-z"))

(use-package undo-fu
  :bind (("C-/" . undo-fu-only-undo)
         ("C-S-/".  undo-fu-only-redo))
  :init
  (setq undo-limit 6710886400) ;; 64mb.
  (setq undo-strong-limit 100663296) ;; 96mb.
  (setq undo-outer-limit 1e006632960) ;; 960mb.
  (setq undo-fu-allow-undo-in-region t))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (when (executable-find "zstd")
    (setq undo-fu-session-compression 'zst))
  (undo-fu-session-global-mode))

(use-package vundo
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (setq vundo-compact-display t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :init (global-highlight-parentheses-mode t))

(use-package iedit
  :commands iedit-mode
  :custom
  (iedit-toggle-key-default (kbd "C-c ;")))

(use-package hl-line
  :ensure nil
  ;; Highlights the current line
  ;; :if (display-graphic-p)
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

(use-package lin
  :custom
  (lin-mode-hooks '(bongo-mode-hook
                    dired-mode-hook
                    elfeed-search-mode-hook
                    git-rebase-mode-hook
                    grep-mode-hook
                    ibuffer-mode-hook
                    ilist-mode-hook
                    ledger-report-mode-hook
                    log-view-mode-hook
                    magit-log-mode-hook
                    mu4e-headers-mode-hook
                    notmuch-search-mode-hook
                    notmuch-tree-mode-hook
                    occur-mode-hook
                    org-agenda-mode-hook
                    pdf-outline-buffer-mode-hook
                    proced-mode-hook
                    tabulated-list-mode-hook))
  :config
  (lin-global-mode 1))


;;==================================================
;; File management
;;==================================================

;; Backup settings
(use-package files
  :ensure nil
  :init
  (setq insert-directory-program (if *is-a-mac* "gls" "ls")
        backup-by-copying t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        create-lockfiles nil))

;; Useful modes
(use-package image-file
  :ensure nil
  ;;:defer 5
  :config
  (auto-image-file-mode 1))

;; Save a list of recent files visited.
(use-package recentf
  :ensure nil
  :init
  (recentf-mode t)
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

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p nil)
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

(use-package saveplace
  :ensure nil
  :init (save-place-mode t))

;; (use-package super-save
;;   :init (super-save-mode t)
;;   :config
;;   (setq super-save-triggers '(windmove-up windmove-down windmove-left windmove-right next-buffer previous-buffer))
;;   (setq super-save-auto-save-when-idle nil)
;;   (setq super-save-remote-files nil))

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'make-parent-directory)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;==================================================
;; Window management
;;==================================================

(use-package winner
  :ensure nil
  :init (winner-mode t))

(use-package transpose-frame)

(use-package windmove
  :ensure nil
  :init
  (windmove-default-keybindings) ;; S-<arrow>
  (windmove-delete-default-keybindings) ;; C-x S-<arrow>
  (windmove-display-default-keybindings) ;; ;; S-M-<arrow> CMD
  (windmove-swap-states-default-keybindings)) ;; S-s-<arrow>

(use-package window
  :ensure nil
  :bind (:repeat-map jccb/windows
         ("o" . other-window)
         ;;("a" . ace-window)
         ("n" . next-buffer)
         ("p" . previous-buffer)
         ("0" . delete-window)
         ("1" . delete-other-windows)
         ("2" . split-window-below)
         ("3" . split-window-right))
  :bind (("C-0"            . delete-window)
         ("C-1"            . delete-other-windows)
         ("C-2"            . split-window-below)
         ("C-3"            . split-window-right)
         ;;("C-;"            . other-window)
         ("S-C-<left>"     . shrink-window-horizontally)
         ("S-C-<right>"    . enlarge-window-horizontally)
         ("S-C-<down>"     . shrink-window)
         ("S-C-<up>"       . enlarge-window)
         ("C-x <C-return>" . window-swap-states))
  )

(use-package winum
  :bind*
  ("M-0" . winum-select-window-0-or-10)
  ("M-1" . winum-select-window-1)
  ("M-2" . winum-select-window-2)
  ("M-3" . winum-select-window-3)
  ("M-4" . winum-select-window-4)
  ("M-5" . winum-select-window-5)
  ("M-6" . winum-select-window-6)
  ("M-7" . winum-select-window-7)
  ("M-8" . winum-select-window-8)
  ("M-9" . winum-select-window-9)
  :init
  (winum-mode +1))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;;(setq aw-keys '(?a ?s ?d ?f       ?j ?k ?l))
  (setq aw-dispatch-always nil)
  ;; (ace-window-display-mode +1)
  )
(bind-key "<f2>" #'previous-buffer)
(bind-key "S-<f2>" #'next-buffer)
(bind-key "C-M-o" #'mode-line-other-buffer)
(bind-key "C-x k" #'kill-current-buffer)

(use-package jccb-windows
  :ensure nil
  :bind (("C-x |" . split-window-horizontally-instead)
         ("C-x _" . split-window-vertically-instead)
         ;; ("C-2"   . split-window-vertically-with-other-buffer)
         ;; ("C-3"   . split-window-horizontally-with-other-buffer)
         ("M-o" . other-window)))


(setq window-resize-pixelwise nil ; jccb: t breaks org-fast-tag-insert with doom-modeline
      frame-resize-pixelwise t)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160
      split-height-threshold nil)

(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.4)

;;==================================================
;; Buffer management
;;==================================================

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filter-group-name-face '(:inherit (success bold)))
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-default-sorting-mode 'filename/process)
  (add-to-list 'ibuffer-help-buffer-modes 'helpful-mode)
  (add-to-list 'ibuffer-help-buffer-modes 'Man-mode)
  ;; (setq ibuffer-display-summary nil)
  (setq ibuffer-use-other-window t)
  ;; (setq ibuffer-movement-cycle nil)
  ;; (setq ibuffer-default-shrink-to-minimum-size t)
  ;; (setq ibuffer-saved-filter-groups nil)

  ;; (setq ibuffer-display-summary nil)
  (define-ibuffer-column size
    (:name "Size"
     :inline t
     :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;==================================================
;; Mac-specific settings
;;==================================================
(when *is-a-mac*
  (setq delete-by-moving-to-trash t)
  ;; left and right commands are meta
  (setq mac-command-modifier 'meta)
  (setq mac-right-command-modifier 'super)
  (setq mac-right-option-modifier 'hyper)
  ;; left opt key is super
  (setq mac-option-modifier 'super)
  ;; right opt is ignored by emacs (useful for mac-style accent input)
  ;; left and right controls are control
  (setq mac-control-modifier 'control)

  (setq mac-right-control-modifier 'left)
  ;; function key is hyper
  (setq mac-function-modifier 'hyper)
  (setq default-input-method "MacOSX"))

(when *is-a-windowed-mac*
  ;; breaks doom theme ??
  ;; (setq visible-bell nil) ;; The default
  (setq visible-bell nil
        ring-bell-function 'flash-mode-line)
  (defun flash-mode-line ()
    (invert-face 'mode-line)
    (run-with-timer 0.05 nil #'invert-face 'mode-line))
  (setq ns-use-native-fullscreen nil)
  (setq ns-use-fullscreen-animation nil)
  (setq ns-pop-up-frames nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; set my path manually on mac
  ;; (Deprecated in favor of fix-mac-path.sh)
  ;; (setenv "LANG" "en_US.UTF-8")
  ;; (let* ((mypaths '("~/bin" "~/homebrew/bin" "~/google-cloud-sdk/bin/"))
  ;;        (expanded (mapcar 'expand-file-name mypaths)))
  ;;   (setenv "PATH" (concat (string-join expanded ":") ":" (getenv "PATH")))
  ;;   (setq exec-path (append expanded exec-path)))
  )


(use-package reveal-in-osx-finder
  :if *is-a-mac*
  :commands reveal-in-osx-finder)

;;==================================================
;; git and magit settings
;;==================================================

;; (use-package gl-conf-mode
;;   :load-path "site-lisp/gl-conf-mode"
;;   :mode "gitolite\\.conf\\'")

(use-package git-modes)
(use-package ediff
  :ensure nil
  :functions ediff-setup-windows-plain
  :init
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-keep-variants nil ; Kill variants upon quitting an Ediff session
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))

;; (use-package magit-delta
;;   :after magit
;;   ;;:hook (magit-mode . magit-delta-mode)
;;   :config
;;   (setq magit-delta-delta-args (append magit-delta-delta-args '("--features" "magit-delta"))))

(use-package transient)

(use-package magit
  ;;:defer 1
  :bind (("C-c C-g" . magit-status)
         ;;("C-x C-z" . magit-status-quick)
         ("C-c g"   . magit-file-dispatch)
         ("C-c M-g" . magit-dispatch))
  :config
  (global-git-commit-mode +1)
  (setq
   magit-format-file-function #'magit-format-file-nerd-icons
   magit-bury-buffer-function #'magit-restore-window-configuration
   ;; magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
   magit-no-confirm '(stage-all-changes unstage-all-changes discard resurrect)
   magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
   magit-diff-refine-hunk 'all
   ;; magit-branch-adjust-remote-upstream-alist '(("origin/master" "master"))
   magit-branch-prefer-remote-upstream '("master" "main")
   magit-delete-by-moving-to-trash t
   ;;magit-git-executable (executable-find magit-git-executable)
   magit-revision-insert-related-refs 'all
   magit-save-repository-buffers 'dontask
   git-commit-summary-max-length 70))

(use-package ztree)

(use-package llm
  :custom
  (llm-warn-on-nonfree nil)
  :init
  (require 'llm-gemini))

(use-package magit-gptcommit
  :after (magit jccb-local llm)
  :demand t
  :bind (:map git-commit-mode-map
         ("C-c C-g" . magit-gptcommit-commit-accept))
  :init
  (setq magit-gptcommit-llm-provider (make-llm-gemini :key (jccb/get-gemini-key)
                                                      :chat-model "gemini-2.5-pro-exp-03-25")
        magit-gptcommit-prompt "You are an expert programmer writing a commit message.
You went over every file diff that was changed in it.
Summarize the commit into a single specific and cohesive theme.
Remember to write in only one line, no more than 50 characters.
Write your response using the imperative tense following the kernel git commit style guide.
Write a high level title. Do not include a label in front of the commit title.

THE FILE DIFFS:
```
%s
```
Now write the commit message:
")

  :config
  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  ;; (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup))

;; (use-package magit-todos
;;   :after magit)

;; (use-package git-gutter
;;   :commands git-gutter-mode
;;   :hook ((prog-mode text-mode config-mode) . jccb/git-gutter-mode-if-local)
;;   :config
;;   (setq git-gutter:update-interval 0.05)
;;   (defun jccb/git-gutter-mode-if-local ()
;;     (unless (file-remote-p default-directory)
;;       (git-gutter-mode))))

;; (use-package git-gutter
;;   :hook (prog-mode . git-gutter-mode)
;;   :config
;;   (setq git-gutter:update-interval 2))

;; (use-package git-gutter-fringe
;;   :config
;;   (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package diff-hl
  :after magit
  :hook (dired-mode . diff-hl-dired-mode)
  :init
  (setq diff-hl-draw-borders t)
  (setq-default diff-hl-inline-popup--height 4)
  (global-diff-hl-mode t)
  :config
  (diff-hl-flydiff-mode 1)

  (advice-add 'diff-hl-next-hunk :after
              (defun my/diff-hl-recenter
                  (&optional _) (recenter)))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package git-link)
(use-package git-timemachine)
;; (setq dired-vc-rename-file t)

;;==================================================
;; Search settings
;;=================================================

(use-package ripgrep
  :commands ripgrep-regexp)

(use-package deadgrep
  :bind ("M-s R" . deadgrep))

;; use regexp isearch by default
(bind-key [remap isearch-forward] #'isearch-forward-regexp)
(bind-key [remap isearch-backward] #'isearch-backward-regexp)

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(use-package isearch
  :ensure nil
  :config
  (setq search-ring-max 100)
  (setq regexp-search-ring-max 100)
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited))

(use-package jccb-search
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


(use-package occur
  :ensure nil
  :commands occur
  ;; :init
  ;; (bind-key "<f2>" 'my-occur-dwim)
  :bind (:map occur-mode-map
         ("n" . occur-next)
         ("p" . occur-prev)
         ("C-x C-q" . occur-edit-mode)
         ("o" . occur-mode-display-occurrence))
  :config
  (advice-add 'isearch-occur :after
              #'(lambda (origin &rest args)
                  (isearch-exit)
                  (select-window (get-buffer-window "*Occur*"))
                  (goto-char (point-min)))))

;; alternative: (setq isearch-lazy-count t)
(use-package anzu
  :init (global-anzu-mode t)
  :bind (([remap query-replace-regexp] . anzu-query-replace)
         ([remap query-replace] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)
         ([remap isearch-query-replace] . anzu-isearch-query-replace-regexp)))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :bind (:map grep-mode-map
         ("e" . wgrep-change-to-wgrep-mode)
         ("C-x C-q" . wgrep-change-to-wgrep-mode)
         ("C-c C-c" . wgrep-finish-edit)))

;; (use-package ag
;;   :after wgrep
;;   :if (executable-find "ag")
;;   :config
;;   (use-package wgrep-ag)
;;   (setq ag-highlight-search t))

;;==================================================
;; Dired
;;=================================================

(use-package dired
  :ensure nil
  :bind (("C-x C-j" . dired-jump))
  ;; :hook (dired-mode . dired-collapse-mode)
  :init
  (setq dired-listing-switches "--time-style long-iso -alhFgG --group-directories-first"
        dired-auto-revert-buffer t
        dired-kill-when-opening-new-dired-buffer t
        dired-clean-confirm-killing-deleted-buffers nil
        dired-confirm-shell-command nil
        dired-hide-details-hide-symlink-targets nil
        dired-recursive-deletes 'always
        delete-by-moving-to-trash t
        dired-recursive-copies 'always
        dired-dwim-target t))

;; (use-package dired-git-info
;;   :bind (:map dired-mode-map
;;               (")" . dired-git-info-mode)))

;; (use-package diredfl
;;   :hook (after-init . diredfl-global-mode))

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

(use-package dired-hist
  :after dired
  :ensure (:host github :repo "karthink/dired-hist")
  :bind (:map  dired-mode-map
         ("l" . dired-hist-go-back)
         ("r" . dired-hist-go-forward))
  :config
  (dired-hist-mode 1))

;; (use-package dired+
;;   :after dired
;;   :config)

(use-package dired-imenu
  :after dired)

(use-package dired-collapse
  :after dired
  :commands dired-collapse-mode)

(use-package peep-dired
  :after dired
  :bind (:map dired-mode-map
         ("P" . peep-dired)))

(use-package dired-hide-dotfiles
  :after dired
  :bind (:map dired-mode-map
         ("." . dired-hide-dotfiles-mode)))

(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
  (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; (use-package casual-dired
;;   :ensure t
;;   :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))


;;==================================================
;; Writing
;;==================================================

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :init
  (setq visual-fill-column-width 80
        visual-fill-column-center-text t))

(setq long-line-threshold 5000)
(setq large-hscroll-threshold 5000)

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode)
  :hook (markdown-mode . visual-wrap-prefix-mode)
  :hook (markdown-mode . variable-pitch-mode)
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode))
  :bind (:map markdown-mode-map
         ("<f5>" . jccb/tf-format-current-block)
         ("S-<f5>" . jccb/fabric-create-open-example-inventory))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\.erb\\'" . markdown-mode)
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

;;==================================================
;; Edit utilities
;;==================================================

(use-package elec-pair
  :ensure nil
  :config

  ;; https://old.reddit.com/r/emacs/comments/1hwf46n/weekly_tips_tricks_c_thread_20250108_week_01/m63mddk/
  (defun my/electric-pair-conservative-inhibit (char)
    (or
     ;; I find it more often preferable not to pair when the
     ;; same char is next.
     (eq char (char-after))
     ;; Don't pair up when we insert the second of "" or of ((.
     (and (eq char (char-before))
          (eq char (char-before (1- (point)))))
     ;; I also find it often preferable not to pair next to a word.
     (eq (char-syntax (following-char)) ?w)
     ;; Don't pair at the end of a word, unless parens.
     (and
      (eq (char-syntax (char-before (1- (point)))) ?w)
      (eq (preceding-char) char)
      (not (eq (char-syntax (preceding-char)) 40) ;; 40 is open paren
           ))))

  (setq electric-pair-inhibit-predicate #'my/electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance nil)
  (setq electric-pair-delete-adjacent-pairs t)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-open-newline-between-pairs t)
  (electric-pair-mode +1))

(use-package paren
  :ensure nil
  :init (show-paren-mode t)
  :config
  (setq show-paren-delay 0.1
        show-paren-style 'parenthesis
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package phi-search)
(use-package multiple-cursors
  :commands ar/mc-mark-all-symbol-overlays
  :bind (("C-c c r" . mc/mark-pop))
  :config
  (setq mc/always-run-for-all t)
  (defun ar/mc-mark-all-symbol-overlays ()
    "Mark all symbol overlays using multiple cursors."
    (interactive)
    (mc/remove-fake-cursors)
    (when-let* ((overlays (symbol-overlay-get-list 0))
                (point (point))
                (point-overlay (seq-find
                                (lambda (overlay)
                                  (and (<= (overlay-start overlay) point)
                                       (<= point (overlay-end overlay))))
                                overlays))
                (offset (- point (overlay-start point-overlay))))
      (setq deactivate-mark t)
      (mapc (lambda (overlay)
              (unless (eq overlay point-overlay)
                (mc/save-excursion
                 (goto-char (+ (overlay-start overlay) offset))
                 (mc/create-fake-cursor-at-point))))
            overlays)
      (mc/maybe-multiple-cursors-mode))))

(use-package avy-zap
  :bind (("M-Z" . avy-zap-up-to-char-dwim)))

(use-package misc
  :ensure nil
  :bind ("M-z" . zap-up-to-char))

(use-package so-long
  :init (global-so-long-mode t))

(use-package dtrt-indent
  :init (dtrt-indent-global-mode t))

(use-package ialign
  :commands ialign)

(use-package xref
  :ensure nil)

(use-package dumb-jump
  :commands dumb-jump-xref-activate
  :init
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t)
  (setq dumb-jump-prefer-searcher 'rg))

;; (use-package xref
;;   :hook (xref-after-jump . xref-pulse-momentarily)
;;   :hook (xref-after-return . xref-pulse-momentarily))


(use-package simple
  :ensure nil
  ;; :bind (:repeat-map jccb/mark-repeat-map
  ;;        ("p" . pop-to-mark-command)
  ;;        ("g" . pop-global-mark))
  :bind (("C-S-P" . pop-to-mark-command)
         ("C-S-O" . pop-global-mark))
  :init
  (setq mark-ring-max 64
        global-mark-ring-max 64)
  ;; When popping the mark, continue popping until the cursor actually moves
  ;; Also, if the last command was a copy - skip past all the expand-region cruft.
  (defun jccb/pop-to-mark-command-advice (orig-fun &rest args)
    (let ((p (point)))
      (when (eq last-command 'save-region-or-current-line)
        (apply orig-fun args)
        (apply orig-fun args)
        (apply orig-fun args))
      (dotimes (i 10)
        (when (= p (point)) (apply orig-fun args)))))

  (advice-add 'pop-to-mark-command :around #'jccb/pop-to-mark-command-advice))

(use-package jccb-misc
  :ensure nil
  :commands (chmod+x-this jccb/doctor)
  :bind (("M-p" . goto-match-paren)
         ([remap save-some-buffers] . jccb/project-save-some-buffers))
  :config (jccb/doctor))

(use-package shrink-whitespace
  :unless (fboundp 'cycle-spacing)
  :bind ("M-\\" . shrink-whitespace))

(use-package emacs
  :ensure nil
  :if (fboundp 'cycle-spacing)
  :bind ("M-\\" . cycle-spacing))

(use-package drag-stuff
  :hook ((text-mode prog-mode conf-mode) . turn-on-drag-stuff-mode)
  :config
  (setq drag-stuff-modifier '(meta super))
  (drag-stuff-define-keys))

;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :hook ((text-mode prog-mode conf-mode) . whole-line-or-region-local-mode)
  ;;:bind ("C-w" . backward-kill-word)
  :config
  (whole-line-or-region-global-mode +1))

(use-package puni
  ;; :hook ((after-init . puni-global-mode)
  ;;        (term-mode . puni-disable-puni-mode))
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  :bind (;; puni-raise
         ;; puni-split
         ;; puni-transpose
         ;; puni-convolute
         ("C-<f9>" . puni-splice)
         ("<f9>"   . puni-squeeze)
         ("C-{"    . puni-slurp-backward)
         ("C-}"    . puni-barf-backward)
         ("M-C-{"  . puni-barf-forward)
         ("M-C-}"  . puni-slurp-forward)
         ;; ("C-="    . puni-expand-region)
         :map puni-mode-map
         ("C-M-<right>"  . puni-forward-sexp)
         ("C-M-<left>"   . puni-backward-sexp))
  :config
  (setq puni-confirm-when-delete-unbalanced-active-region nil))

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  (setq expand-region-fast-keys-enabled t)
  (setq expand-region-subword-enabled t)
  (setq expand-region-preferred-python-mode 'python-mode)
  (setq expand-region-smart-cursor t))

(use-package selected
  :init (selected-global-mode t)
  :bind (:map selected-keymap
         ("q" . selected-off)

         ("M-l" . mc/edit-lines)
         ("M-." . mc/mark-next-like-this)
         ("M-," . mc/mark-previous-like-this)
         ;; ("M->" . mc/skip-to-next-like-this)
         ;; ("M-<" . mc/unmark-next-like-this)
         ("M-@" . mc/mark-all-like-this)
         ("M-!" . mc/mark-all-in-region)
         ("M-a" . mc/edit-beginnings-of-lines)
         ("M-e" . mc/edit-ends-of-lines)
         ;;("M-s" . ar/mc-mark-all-symbol-overlays)

         ("M-u" . upcase-region)
         ("M-d" . downcase-region)
         ("M-c" . count-words-region)
         ("M-f" . flush-lines)
         ("M-k" . keep-lines)
         ("M-S" . sort-lines)
         ("M-m" . apply-macro-to-region-lines)))

;; (use-package change-inner
;;   :bind (("C-c i" . change-inner)
;;          ("C-c o" . change-outer)))

(use-package rainbow-mode
  :hook (css-mode html-mode))

(use-package symbol-overlay)

(use-package avy
  :bind (:repeat-map jccb/avy-repeat-map
         ("r" . avy-resume)
         ("n" . avy-next)
         ("p" . avy-prev))
  :bind (("M-g g"   . avy-goto-line)
         ("M-g C-j" . avy-resume)
         ("M-g C-n" . avy-next)
         ("M-g C-p" . avy-prev)
         ("C-'"     . avy-goto-char-timer)
         ("C-\""    . avy-goto-word-0)
         :map isearch-mode-map
         (""     . avy-isearch))
  :config
  (setq avy-timeout-seconds 0.6)
  (avy-setup-default))


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
;; Spell
;;==================================================

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :custom
  (jinx-languages "en es")
  :bind (("C-;" . jinx-correct)))

(setopt dictionary-server "dict.org")

;;==================================================
;; coding modes
;;==================================================

(use-package dockerfile-mode
  :mode "Dockerfile[a-zA-Z.-]*\\'")

(use-package compile
  :ensure nil
  :bind ("<f12>" . compile)
  :config

  (defun compile-yamllint--find-filename ()
    "Find the filename for current error."
    (save-match-data
      (save-excursion
        (when (re-search-backward (rx bol (group (or "/" ".") (+ any)) eol))
          (list (match-string 1))))))


  (let ((form `(yamllint
                ,(rx-to-string
                  '(and (group (group (+ digit)) ":" (group (+ digit)))
                        (+ " ") (or "error" "warning")))
                compile-yamllint--find-filename
                2 3 2 1)))
    (if (assq 'yamllint compilation-error-regexp-alist-alist)
        (setf (cdr (assq 'yamllint compilation-error-regexp-alist-alist)) (cdr form))
      (push form compilation-error-regexp-alist-alist)))

  (push 'yamllint compilation-error-regexp-alist))

;; (use-package ansi-color
;;   :hook (compilation-filter . ansi-color-compilation-filter))

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode)
  :mode ("\\.terraformrc\\'" . hcl-mode)
  ;; :hook (terraform-mode . jccb/terraform-mode-hook)
  :bind (:map terraform-mode-map
         ("C-c C-f" . jccb/tf-fabric-find-module-file))
  :init
  (with-eval-after-load 'compile
    (add-to-list 'compilation-error-regexp-alist '("on \\([a-z0-9A-Z/._-]+\\) line \\([0-9]+\\)" 1 2)))
  :config
  (defun jccb/tf-capf-setup ()
    (setq-local completion-at-point-functions '(cape-dabbrev cape-keyword))))


(use-package jccb-fabric
  :ensure nil
  :commands (jccb/fabric-create-open-example-inventory
             jccb/tf-format-region
             jccb/tf-format-current-block
             jccb/tf-fabric-find-module-file)
  :load-path "site-lisp")

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

(use-package jq-mode)

(use-package json-mode
  :mode "\\.json?\\'")

;; (use-package scss-mode
;;   :mode "\\.scss?\\'"
;;   :config
;;   (setq scss-compile-at-save nil))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package pip-requirements
  :mode "requirements\\.txt\\'")

(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'"   . ssh-config-mode)
         ("/known_hosts\\'"     . ssh-known-hosts-mode)
         ("/authorized_keys\\'" . ssh-authorized-keys-mode)))

;; (use-package csv-mode
;;   :mode "\\.csv\\'")

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode))
  :mode (("\\.tsx\\'" . typescript-mode)))


(setq-default python-indent-offset 2)



;; (use-package scss-mode
;;   :config
;;   (setq scss-compile-at-save nil))

;;==================================================
;; programming stuff
;;==================================================

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package macrostep)

;; (use-package highlight-defined
;;   :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package ipretty
  :init (ipretty-mode t))

(use-package highlight-sexp
  :ensure nil
  :commands highlight-sexp-mode)

(use-package hs-minor-mode
  :ensure nil
  :hook (emacs-lisp-mode . hs-minor-mode))

(use-package eros
  :commands (eros-mode eros-eval-last-sexp eros-eval-region eros-eval-defun)
  :bind (:map emacs-lisp-mode-map
         ("C-M-x" . im-eval-dwim))
  :init
  :config
  (defun im-eval-dwim ()
    (interactive)
    (cond
     ((use-region-p)
      (call-interactively 'eros-eval-region))
     ((or (-contains? '(?\) ?\") (char-before))
          (-contains? '(?\ ?\)) (char-after)))
      (call-interactively 'eros-eval-last-sexp))
     (t
      (call-interactively 'eros-eval-defun)))))

;;https://github.com/Fuco1/.emacs.d/blob/master/site-lisp/my-redef.el#LL18C1-L100C62
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (goto-char calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same
         ;; line as calculate-lisp-indent-last-sexp.  Note that first
         ;; thing on that line has to be complete sexp since we are
         ;; inside the innermost containing sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (goto-char indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (goto-char orig-point)
                (looking-at ":")))
         (save-excursion
           (goto-char (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

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
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top-edge))

(use-package color-identifiers-mode
  :commands color-identifiers-mode)

(use-package py-isort
  :commands py-isort-buffer)

;; (use-package blacken
;;   :hook (python-mode . blacken-mode)
;;   :commands blacken-mode blacken-buffer)

(use-package format-all
  :hook (python-mode . format-all-mode)
  :config
  (setq format-all-show-errors 'never)
  (setq-default format-all-formatters
                '(("Python" yapf))))

;; (use-package pyvenv
;;   :hook (after-init . pyvenv-mode)
;;   :config
;;   (setq pyvenv-mode-line-indicator
;;         '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] "))))

;; (use-package pyenv-mode)

(use-package imenu
  :ensure nil
  :config
  (setq imenu-use-markers t
        imenu-auto-rescan t
        imenu-max-item-length 100
        imenu-use-popup-menu nil
        imenu-eager-completion-buffer t
        imenu-space-replacement "SPACE"
        imenu-level-separator "LEVEL SEP"))

(use-package imenu-list
  :commands imenu-list-minor-mode)

;;==================================================
;; Embark + Consult + Tempel
;;==================================================

;; (use-package consult-flycheck
;;   :commands consult-flycheck)

(use-package eglot
  :ensure nil
  ;; :bind (:map eglot-mode-map
  ;;             ("C-c C-d" . eldoc)
  ;;             ("C-c C-e" . eglot-rename)
  ;;             ("C-c C-o" . python-sort-imports)
  ;;             ("C-c C-f" . eglot-format-buffer))
  :hook ((bash-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (markdown-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (json-mode . eglot-ensure)
         (go-mode . eglot-ensure))
  ;; :custom
  ;; (eglot-report-progress nil)
  ;; :config
  ;; (fset #'jsonrpc--log-event #'ignore)
  ;; (setq jsonrpc-event-hook nil)
  )

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-man) ;;TODO: not working
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b"  . consult-bookmark)           ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

         ;; ("C-x r j" . consult-register-load)
         ("C-x r s" . consult-register-store)
         ("C-x r x" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ;; ("M-g f"    . consult-flycheck)
         ;; ("M-g g"    . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g"  . consult-goto-line)           ;; orig. goto-line
         ("M-g o"    . consult-outline)
         ("M-g m"    . consult-mark)
         ("M-g k"    . consult-global-mark)
         ("M-g e"    . consult-compile-error)
         ("M-i"      . consult-imenu)
         ("M-g I"    . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-fd)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)
         )
  :commands consult-ref
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (setq consult-narrow-key "`"
        register-preview-delay 0
        consult-preview-key "C-S-<return>"
        register-preview-function #'consult-register-format)

  ;; show filtered buffers with SPC
  ;; (add-to-list 'consult-buffer-filter "^\\*")

  (consult-customize
   consult-global-mark consult-mark
   :preview-key 'any
   consult-theme consult-imenu consult-goto-line
   :preview-key '(:debounce 0.2 any)

   consult-buffer
   :preview-key '("C-S-<return>")

   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   consult-line
   :preview-key '("C-S-<return>"
                  :debounce 0.1 "<up>" "<down>"))

  ;; narrow to files by default
  (dolist (src consult-buffer-sources)
    (unless (eq src 'consult--source-buffer)
      (set src (plist-put (symbol-value src) :hidden t))))

  ;;(define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  (advice-add #'register-preview :override #'consult-register-window)
  ;; (set-face-attribute 'consult-file nil :inherit 'doom-modeline-buffer-file)

  ;; using full file names in buffer
  ;; https://amitp.blogspot.com/2024/05/emacs-consult-buffer-filenames.html
  (defun my/consult--source-buffer ()
    "Make consult-buffer match the entire filename of a buffer"
    ;; items is a list of (name . buffer)
    (let ((items (consult--buffer-query :sort 'visibility
                                        :as #'consult--buffer-pair)))
      ;; TODO: sort these so the current project is first?
      (--map
       (let* ((label (car it))
              (buffer (cdr it))
              (filename (buffer-file-name buffer)))
         (if filename
             (progn
               (setq filename (abbreviate-file-name filename))
               (setq label
                     (concat (file-name-directory filename)
                             (propertize (file-name-nondirectory filename) 'face 'consult-file))))
           (setq label (propertize label 'face 'consult-buffer)))
         (cons label buffer))
       items)))
  (defvar my/consult--source-buffer
    `(:name     "Open file"
      :narrow   ?o
      :category buffer
      :history  buffer-name-history
      :state    ,#'consult--buffer-state
      :default  t
      :items    ,#'my/consult--source-buffer)
    "Buffer with filename matching for `consult-buffer'.")
  (cl-nsubst 'my/consult--source-buffer 'consult--source-buffer consult-buffer-sources))

(use-package consult-eglot)

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-f" . consult-dir-jump-file)
         ("C-x C-d" . consult-dir))
  :config
  (setq consult-dir-shadow-filenames nil))

(use-package embark
  :after vertico
  :commands (embark-prefix-help-command embark-act-noquit)
  :bind (("C-."   . embark-act)
         ("M-."   . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)
         :map vertico-map
         ;;("C->" . embark-become)
         ("C-*"   . embark-act-all)
         ("C-o"   . embark-export))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config

  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act)))

  (setq embark-prompter 'embark-completing-read-prompter)
  (setq embark-keymap-prompter-key "`")
  (setq embark-indicators
        '(embark-minimal-indicator  ; default is embark-mixed-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package sudo-edit
  :after embark
  :bind
  (:map embark-file-map
   ("s" . sudo-edit-find-file))
  (:map embark-become-file+buffer-map
   ("s" . sudo-edit-find-file)))

;;==================================================
;; Misc
;;==================================================

;; Use ssh connection sharing, but let me manage it from my ~/.ssh/config
(customize-set-variable 'tramp-use-connection-share t)
;; (customize-set-variable 'tramp-ssh-controlmaster-options nil)
(customize-set-variable 'tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath=/Users/jccb/.ssh/tmp/master-%%C -o ControlPersist=8h")

(use-package kmacro-x
  :init (kmacro-x-atomic-undo-mode t))

(use-package crux
  :commands crux-find-shell-init-file
  :bind (("C-a"          . crux-move-beginning-of-line)
         ("S-<return>"   . crux-smart-open-line)
         ("C-S-k"        . crux-kill-whole-line)
         ("C-^"          . crux-top-join-line)
         ("C-S-<return>" . crux-smart-open-line-above))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-reopen-as-root-mode +1))

;; (use-package elisp-demos
;;   :commands elisp-demos-advice-helpful-update)

(use-package helpful
  :commands helpful--read-symbol
  :bind (([remap describe-command]  . helpful-command)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ("C-h F"                   . helpful-function)
         ("C-h ."                   . helpful-at-point)))

(use-package help
  :ensure nil
  :init
  (setq help-window-keep-selected t))

(use-package whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode t))

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
                minibuffer-setup-hook
                vterm-mode-hook
                eat-mode-hook))
  (add-hook hook #'sanityinc/no-trailing-whitespace))

(use-package which-key
  :custom
  ;; (which-key-sort-order #'which-key-key-order)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 5)
  (which-key-idle-delay 1)
  (which-key-idle-secondary-delay 0.05)
  (which-key-use-C-h-commands nil)
  (which-key-side-window-max-height 0.3)
  (which-key-side-window-slot -10)
  (which-key-show-prefix 'top)
  (which-key-show-remaining-keys t)
  (which-key-max-description-length 45)
  (which-key-dont-use-unicode nil)
  ;;(which-key-compute-remaps t)
  (which-key-allow-multiple-replacements t)
  ;; (which-key-special-keys nil)
  ;; (which-key-popup-type 'minibuffer)
  (which-key-preserve-window-configuration t)
  ;; (which-key-show-transient-maps t)
  :init
  (which-key-mode t)
  :config
  (which-key-setup-side-window-bottom))

(use-package copy-as-format
  :commands copy-as-format)

;; Allow UTF or composed text from the clipboard, even in the terminal or on
;; non-X systems (like Windows or macOS), where only `STRING' is used.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))


(use-package files
  :ensure nil
  :init
  (defun jccb/disable-confirm-kill-emacs (&rest _)
    (message "disable confirm kill emacs")
    (setq confirm-kill-emacs nil))
  (advice-add 'restart-emacs :before #'jccb/disable-confirm-kill-emacs))

(defun jccb/set-black-bg nil
  (face-remap-add-relative 'default '(:background "black")))

(use-package eat
  :config
  (setq process-adaptive-read-buffering nil)
  (setq read-process-output-max (* 4 1024 1024)))

(use-package keypression
  :commands keypression-mode)

(use-package command-log-mode
  :commands command-log-mode)

(use-package server
  :ensure nil
  :if (display-graphic-p)
  :hook (elpaca-after-init . server-start))

(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind ("C-<tab>" . toggle-fold)
  :init
  (defun toggle-fold ()
    (interactive)
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding))))


(use-package string-inflection
  :bind (:repeat-map jccb/string-inflection-repeat-map
         ("s" . jccb/string-inflection-cycle)
         ("u" . string-inflection-underscore)
         ("U" . string-inflection-upcase)
         ("k" . string-inflection-kebab-case)
         ("c" . string-inflection-camelcase))
  :bind ("C-c s" . jccb/string-inflection-cycle)
  :config
  (defun jccb/string-inflection-cycle ()
    "switching by major-mode"
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
     (t
      (string-inflection-all-cycle)))))

(use-package goto-chg
  :commands goto-last-change)

(use-package piper
  :encsure (:host gitlab :repo "howardabrams/emacs-piper")
  :disabled)
;; pretty print lisp stuff
(use-package pp  :disabled)

(use-package repeat
  :ensure nil
  :init
  (setq repeat-exit-key (kbd "<TAB>"))
  (setq repeat-exit-timeout 1.0)
  (repeat-mode t))

(defun jccb/replace-copyright nil
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (and (re-search-forward "Copyright [0-9]+" nil t) (nth 4 (syntax-ppss)))
      (replace-match (concat "Copyright " (format-time-string "%Y"))))))
(add-hook 'before-save-hook #'jccb/replace-copyright)


(use-package recursion-indicator
  :init (recursion-indicator-mode t))

(use-package beginend
  :init (beginend-global-mode t))

(use-package free-keys
  :commands free-keys)

(use-package fancy-compilation
  :init (fancy-compilation-mode t))

(use-package eshell
  :ensure nil
  :hook (eshell-mode . jccb/set-black-bg))

(use-package shell
  :ensure nil
  :hook (shell-mode . jccb/set-black-bg))

(use-package term
  :ensure nil
  :hook (term-mode . jccb/set-black-bg))

(use-package comint
  :ensure nil
  :hook (comint-mode . jccb/set-black-bg))

(use-package go-mode
  :mode (("\\.go\\'" . go-mode)
         ("\\.go\\.erb\\'" . go-mode)))

(use-package mmm-mode
  :config
  (require 'mmm-auto)
  (require 'mmm-erb)
  ;; (define-derived-mode go-erb-mode go-mode "ERB-GO"
  ;;   (set (make-local-variable 'mmm-indent-line-function) #'mmm-erb-indent-line)
  ;;   (add-hook 'mmm-after-syntax-propertize-functions
  ;;             #'html-erb-after-syntax-propertize nil t))

  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'go-mode nil 'erb)
  (mmm-add-mode-ext-class 'markdown-mode "\\.markdown\\.erb" 'erb)
  ;;(mmm-add-mode-ext-class 'go-erb-mode nil 'erb)
  )

;; load additional local settings (if they exist)
(use-package jccb-local
  :load-path "site-lisp"
  :if (file-exists-p (emacs-path "site-lisp/jccb-local.el")))

;; (use-package flycheck-yamllint
;;   :after (flycheck compile)
;;   :hook (flycheck-mode . flycheck-yamllint-setup))

(use-package query-replace-many
  :ensure (:host github :repo "slotThe/query-replace-many")
  :commands query-replace-many)

(use-package graphviz-dot-mode)

;; (use-package sh-script
;;   :hook (sh-mode . flymake-mode))

(setq recenter-positions '(5 bottom))

(use-package surround
  :bind-keymap ("M-'" . surround-keymap))

(use-package change-inner
  :after surround
  :bind (:map surround-keymap
         ("I" . change-inner)
         ("O" . change-outer)))

(use-package gptel
  :if (fboundp 'jccb/get-gemini-key)
  :bind ("C-S-<f5>" . gptel-menu)
  :bind ("<f5> <f5>" . gptel-send)
  :bind ("<f5> r" . gptel-rewrite)
  :bind ("<f5> g" . gptel)
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (setq gptel-model 'gemini-2.5-pro-exp-03-25
        gptel-backend (gptel-make-gemini "Gemini jccb"
                        :key #'jccb/get-gemini-key
                        :stream t))
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(zephyr:latest
              llama3.2:latest
              qwen2.5:14b)))

(use-package gptel-fn-complete
  :bind ("<f5> c" . gptel-fn-complete))

(use-package beancount
  :mode ("\\.beancount\\'" . beancount-mode))


(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(bind-key "C-g" #'prot/keyboard-quit-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; closing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package man
;;   :ensure nil
;;   :config
;;   ;; use gman on macos. Download man-db and run mandb regularly
;;   (setq manual-program (if *is-a-mac* "gman" "man")
;;         Man-notifiy-method 'aggressive))

;; use-package seq: init -> config
;; Local Variables:
;; jinx-local-words: "flyspell"
;; End:
