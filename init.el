;; Report load time after initializing
(add-hook 'after-init-hook 'emacs-init-time)

;; initiate GC every 250 MB allocated (default is 0.8MB)
(setq gc-cons-threshold 250000000)
(setq load-prefer-newer noninteractive)

(setq use-package-verbose t)

;; Put this file in register e for easy access
(set-register ?e `(file . ,user-init-file))

;;==================================================
;; Harden emacs TLS config based on
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html but
;; using a local cacert.pem instead of relying on certify.
;; https://raw.githubusercontent.com/certifi/python-certifi/master/certifi/cacert.pem
;; ==================================================

;;(unless (executable-find "gnutls-cli")
;;  (error "make sure gnutls-cli is in your $PATH"))

(let ((trustfile (expand-file-name "cacert.pem" user-emacs-directory)))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile))
        tls-checktrust t
        gnutls-verify-error t
        gnutls-trustfiles (list trustfile)))

;;==================================================
;; Setup basic stuff
;;==================================================

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-windowed-mac* (and *is-a-mac* window-system))

;; Turn off mouse interface early in startup to avoid momentary display
(unless *is-a-windowed-mac* ; hide menu if not in Mac
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
;; (setq inhibit-startup-message t)

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
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(require 'use-package)

(use-package jc-lib
  :ensure nil
  :load-path "site-lisp")

;;==================================================
;; Appearance settings
;;==================================================
;; (use-package zenburn-theme
;;   :config
;;   (load-theme 'zenburn t))
;; (use-package sunburn-theme
;;   :config
;;   (load-theme 'sunburn t))

;; (use-package spacemacs-theme
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

(use-package winum
  :config
  (winum-mode t))

;; (use-package spaceline
;;   :ensure t
;;   :init
;;   (setq powerline-default-separator 'arrow-fade)
;;   (setq ns-use-srgb-colorspace nil)
;;   (setq anzu-cons-mode-line-p nil)
;;   :config
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)
(setq default-frame-alist '((cursor-type . (bar . 2))))
(setq-default frame-background-mode 'dark)

; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

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

(global-font-lock-mode t)               ; just in case
(line-number-mode 1)
(column-number-mode 1)

;; Explicitly define a width to reduce computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions makes it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen nil)
                                        ;(global-linum-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)


;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;(setq scroll-conservatively 100000)
;(setq scroll-preserve-screen-position 'always)
;(setq scroll-step 0); what?

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; TODO: Remove hscroll-margin in shells, otherwise it causes jumpiness
;; (setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

(when *is-a-mac*
  ;; sane trackpad/mouse scroll settings
  (setq mac-redisplay-dont-reset-vscroll t
        mac-mouse-wheel-smooth-scroll nil))

;;==================================================
;; General settings
;;=================================================

(setq mouse-yank-at-point t                  ; mouse pastes at point
      x-select-enable-clipboard t            ; Allow pasting selection outside of Emacs
      global-auto-revert-non-file-buffers t  ; auto refresh dired
      auto-revert-verbose nil                ; and be quiet about it
      eval-expression-print-level nil
      echo-keystrokes 0.02                    ; Show keystrokes in progress
      confirm-kill-emacs 'yes-or-no-p        ; ask me before closing
      history-length 1000                    ; looong history
      use-dialog-box nil                     ; never show a dialog box
      use-file-dialog nil
      mark-even-if-inactive t                ;
      enable-recursive-minibuffers t         ; yes, please
      highlight-nonselected-windows t        ; show region even on inactive windows
      require-final-newline t                ; end files with a newline
      fill-column 80
      compilation-scroll-output t
      grep-highlight-matches t
      set-mark-command-repeat-p t
      isearch-allow-scroll t
      blink-matching-paren-distance 51200
      confirm-nonexistent-file-or-buffer nil
      indicate-buffer-boundaries nil
      indicate-empty-lines t
      next-line-add-newlines nil) ; don't add new lines when scrolling down

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (or that the user needs to stop relying on case
;; insensitivity).
(setq auto-mode-case-fold nil)

;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setq ad-redefinition-action 'accept)

;; Make apropos omnipotent. It's more useful this way.
(setq apropos-do-all t)


(setq-default visible-bell t)
;(setq-default show-trailing-whitespace t)
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

;; confirm with y/n only
(defalias 'yes-or-no-p 'y-or-n-p)

(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
(bind-key "M-s-/" 'hippie-expand)

(use-package diminish)
(use-package helpful
  :commands helpful--read-symbol
  :init
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol)  
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


;; Delete whitespace at the end of lines when saving
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package whitespace-cleanup-mode
  :diminish (whitespace-cleanup-mode . "_")
  :commands whitespace-cleanup-mode
  :init
  (add-hook 'text-mode-hook #'whitespace-cleanup-mode)
  (add-hook 'prog-mode-hook #'whitespace-cleanup-mode))

;; But don't show trailing whitespace in these modes
(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                term-mode-hook
                ido-minibuffer-setup-hook
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
  :ensure nil
  :init
  (setq backup-by-copying t            ; don't clobber symlinks
        delete-old-versions t          ; delete old backups
        kept-new-versions 6
        kept-old-versions 2
        version-control t         ; use versioned backups
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
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Useful modes
(auto-image-file-mode 1)                ; display images
(size-indication-mode 1)                ; display file size
(delete-selection-mode 1)               ; delete selected text on input
;(global-subword-mode 1)
(global-auto-revert-mode 1)             ; auto reload files if changed outside emacs
(auto-compression-mode t)               ; open compressed files a la dired
(transient-mark-mode 1)                 ; show me the region, please
(winner-mode 1)                         ; stack window settings
(minibuffer-depth-indicate-mode 1)
(electric-indent-mode 0)             ; make return key not auto indent
;(desktop-save-mode 1)

;(fancy-narrow-mode)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; (use-package highlight-parentheses
;;   :diminish highlight-parentheses-mode
;;   :config
;;   (global-highlight-parentheses-mode t))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        `((".*" . ,(concat user-emacs-directory "undo-list"))))
  (global-undo-tree-mode))

;; Save a list of recent files visited.
(setq recentf-max-saved-items 1000)
(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(setq recentf-exclude '("/tmp/" "/ssh:"))
(recentf-mode 1)

(setq history-length 1000)
(setq savehist-file (expand-file-name ".savehist" user-emacs-directory))
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(savehist-mode t)

(add-hook 'kill-emacs-hook
          (defun doom-unpropertize-kill-ring-h ()
            "Remove text properties from `kill-ring' for a smaller savehist file."
            (setq kill-ring (cl-loop for item in kill-ring
                                     if (stringp item)
                                     collect (substring-no-properties item)
                                     else if item collect it))))

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
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)     ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; show-paren-mode: subtle highlighting of matching parens (global-mode)
(use-package paren
  :config
  (setq show-paren-delay 0.1
        show-paren-style 'parenthesis
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode +1))

;; Save point position between sessions
(require 'saveplace)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
(if (fboundp #'save-place-mode)
    (save-place-mode 1)
  (setq-default save-place t))


;; guide-key setup
;; (use-package guide-key
;;   :diminish guide-key-mode
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
  :diminish which-key-mode
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)  
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-max-description-length 45)
  (which-key-mode t))

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


;;==================================================
;; Mac-specific settings
;;==================================================
(when *is-a-mac*
  (setq delete-by-moving-to-trash t)

  ; left and right commands are meta
  (setq ns-command-modifier 'meta)
  (setq ns-right-command-modifier 'left)

  ; left opt key is super
  (setq ns-alternate-modifier 'super) 
  ; right opt is ignored by emacs (useful for mac-style accent input)
  (setq ns-right-alternate-modifier 'none) 

  ; left and right controls are control
  (setq ns-control-modifier 'control)
  (setq ns-right-control-modifier 'left)

  ; function key is hyper
  (setq ns-function-modifier 'hyper)
  
  (setq default-input-method "MacOSX")
  (setq insert-directory-program "gls")  ; dired works better with gls
  (setq default-directory (getenv "HOME"))
  (set-face-font 'default "Source Code Pro 14")
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (kbd (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key [kp-delete] 'delete-char);; sets fn-delete to be right-delete
  (defun jc/toggle-mac-option-modifier ()
    "Toggle between passing option modifier either to Emacs or OS X."
    (interactive)
    (let ((old-opt mac-option-modifier))
      (setq mac-option-modifier
            (if (eq mac-option-modifier 'super)
                'none
              'super))
      (message "Toggled `mac-option-modifier' from %s to %s."
               old-opt
               mac-option-modifier))))

;; disable visible bell in windowed OSX (doesn't work in El Capitan)
(when *is-a-windowed-mac*
  (setq visible-bell nil) ;; The default
  (setq ns-use-native-fullscreen nil)
  (setq ns-pop-up-frames nil)
  (setq ring-bell-function 'ignore))

(use-package ns-auto-titlebar
  :if *is-a-windowed-mac*
  :config
  (ns-auto-titlebar-mode +1))


(use-package exec-path-from-shell
  :if *is-a-mac*
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package reveal-in-osx-finder
  :if *is-a-mac*)


; Stop C-z from minimizing windows under OS X
(defun sanityinc/maybe-suspend-frame ()
  (interactive)
  (unless *is-a-windowed-mac*
    (suspend-frame)))

(bind-key "C-z" 'sanityinc/maybe-suspend-frame)

;; enable electric pairs and indent
;; (when (fboundp 'electric-pair-mode)
;;   (electric-pair-mode))

(when (eval-when-compile (version< "24.4" emacs-version))
  (electric-indent-mode 1))

(global-set-key (kbd "RET") 'newline-and-indent)

;;==================================================
;; ido settings
;;==================================================
(use-package ido)
(use-package flx)
(use-package ido-grid-mode)
(use-package ido-completing-read+)
(use-package flx-ido
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
                                        ;ido-max-prospects 10
        ido-max-window-height 10
        ido-save-directory-list-file (expand-file-name ".ido.last" user-emacs-directory)
        ido-auto-merge-work-directories-length -1
        ido-default-file-method 'selected-window
        ido-ignore-extensions t
        ido-file-extensions-order '(".py" ".html" ".css" ".scss" "js"
                                    ".rb" ".org" ".txt"
                                    ".c" ".cpp" ".cxx" ".h" ".hpp")
        ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  ;; (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (setq ido-use-faces nil)
  (add-hook 'ido-setup-hook
            (lambda ()
              ;; Go straight home
              (define-key ido-file-completion-map
                (kbd "~")
                (lambda ()
                  (interactive)
                  (cond
                   ((looking-back "~/") (insert "code/"))
                   ((looking-back "/") (insert "~/"))
                   (:else (call-interactively 'self-insert-command)))))

              ;; Use C-w to go back up a dir to better match normal usage of C-w
              ;; - insert current file name with C-x C-w instead.
              (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
              (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)))
  :config
  (ido-mode t)
  (ido-everywhere t)
  (flx-ido-mode 1)
  (ido-grid-mode)
  (ido-ubiquitous-mode 1))

;; TODO: move to autoloaded file
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(bind-key "C-x f" 'recentf-ido-find-file)

;;==================================================
;; which-func-mode settings
;;==================================================
(which-function-mode 1)                 ; show me where I'm standing

; puts which-function-mode in the header
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))
;; (setq mode-line-misc-info
;;             ;; We remove Which Function Mode from the mode line, because it's mostly
;;             ;; invisible here anyway.
;;             (assq-delete-all 'which-func-mode mode-line-misc-info))

;;==================================================
;; git and magit settings
;;==================================================

;; TODO: load as needed
(use-package gitconfig-mode
  :mode (("\\.gitconfig\\'" . gitconfig-mode)
	 ("\\.git/config\\'" . gitconfig-mode)
	 ("\\.gitmodules\\'" . gitconfig-mode)))
(use-package gitignore-mode
  :mode ("\\.gitignore\\'" . gitignore-mode))
;; (use-package git-timemachine)
(use-package gl-conf-mode
  :load-path "site-lisp/gl-conf-mode"
  :mode "gitolite\\.conf\\'")

(use-package magit
  :bind ("C-x C-z" . magit-status)
  :init
  (setq magit-repository-directories '("~/code/"))
  (setq-default
   magit-stage-all-confirm nil
   magit-unstage-all-confirm nil
   magit-save-some-buffers nil
   magit-process-popup-time 5
   magit-diff-refine-hunk t
   magit-completing-read-function 'magit-ido-completing-read)
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

  (defun magit-toggle-whitespace ()
    (interactive)
    (if (member "-w" magit-diff-options)
        (magit-dont-ignore-whitespace)
      (magit-ignore-whitespace)))

  (defun magit-ignore-whitespace ()
    (interactive)
    (add-to-list 'magit-diff-options "-w")
    (magit-refresh))

  (defun magit-dont-ignore-whitespace ()
    (interactive)
    (setq magit-diff-options (remove "-w" magit-diff-options))
    (magit-refresh))

  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message))

;;==================================================
;; isearch settings
;;==================================================

;; use regexp isearch by default
(global-set-key [remap isearch-forward] 'isearch-forward-regexp)
(global-set-key [remap isearch-backward] 'isearch-backward-regexp)

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(use-package jc-search
  :ensure nil
  :commands (zap-to-isearch isearch-exit-other-end isearch-yank-symbol)
  :init (bind-keys :map isearch-mode-map
                   ("M-z" . zap-to-isearch)
                   ("C-w" . isearch-yank-symbol)
                   ("C-RET" . isearch-exit-other-end)))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)
(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(use-package anzu
  :diminish anzu-mode
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :config
  ;; show number of matches while searching
  (global-anzu-mode 1)
  ;; use anzu for query for query-replace
  ;;(global-set-key [remap query-replace] 'anzu-query-replace-regexp)
  )

;;==================================================
;; elisp
;;==================================================
(use-package highlight-quoted
  :commands highlight-quoted-mode
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (highlight-quoted-mode 1))))

;(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;;==================================================
;; smart-mode-line
;;==================================================
;; (use-package smart-mode-line
;;   :config
;;   (sml/setup))

;;==================================================
;; smartparens
;;==================================================
;;; req-todo
;; (use-package smartparens-config
;;   :ensure smartparens
;;   :diminish (smartparens-mode . "SP")
;;   :bind (:map smartparens-mode-map
;;               ("C-M-a" . sp-beginning-of-sexp)
;;               ("C-M-e" . sp-end-of-sexp)

;;               ("C-<down>" . sp-down-sexp)
;;               ("C-<up>"   . sp-up-sexp)
;;               ("M-<down>" . sp-backward-down-sexp)
;;               ("M-<up>"   . sp-backward-up-sexp)

;;               ("C-M-f" . sp-forward-sexp)
;;               ("C-M-b" . sp-backward-sexp)

;;               ("C-M-n" . sp-next-sexp)
;;               ("C-M-p" . sp-previous-sexp)

;;               ("C-S-f" . sp-forward-symbol)
;;               ("C-S-b" . sp-backward-symbol)

;;               ("C-<right>" . sp-forward-slurp-sexp)
;;               ("M-<right>" . sp-backward-barf-sexp)
;;               ("C-<left>"  . sp-forward-barf-sexp)
;;               ("M-<left>"  . sp-backward-slurp-sexp)

;;               ("C-M-t" . sp-transpose-sexp)
;;               ("C-M-k" . sp-kill-sexpa)
;;               ("C-k"   . sp-kill-hybrid-sexp)
;;               ("M-k"   . sp-backward-kill-sexp)
;;               ("C-M-w" . sp-copy-sexp)

;;               ("C-M-d" . delete-sexp)

;;               ("M-<backspace>" . backward-kill-word)
;;               ("C-<backspace>" . sp-backward-kill-word)
;;               ([remap sp-backward-kill-word] . backward-kill-word)

;;               ;; breaks bracketed paste mode
;;               ;("M-[" . sp-backward-unwrap-sexp)
;;               ;("M-]" . sp-unwrap-sexp)

;;               ("C-x C-t" . sp-transpose-hybrid-sexp))
;;   :init
;;   (show-smartparens-global-mode)
;;   (smartparens-global-mode t))

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

(require 'dired)

; dired
;(setq dired-listing-switches "--time-style long-iso --group-directories-first -alhF")
(setq dired-listing-switches "--time-style long-iso -alhF")
(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t) ; Move files between split pane

;; TODO: Move to autoload
;; M-up is nicer in dired if it moves to the fourth line - the first file
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
(define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)

(bind-key "C-x C-j" 'dired-jump)
(bind-key "C-x M-j" '(lambda () (interactive) (dired-jump 1)))

;; req-todo
;; (use-package dired+
;;   :config
;;   (global-dired-hide-details-mode -1))

(use-package peep-dired
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'make-parent-directory)

;;==================================================
;; browse-kill-ring settings
;;==================================================
;; req-todo
(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-highlight-inserted-item t)
  (setq browse-kill-ring-quit-action 'save-and-restore))

;;==================================================
;; deft
;;==================================================
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; (use-package deft
;;   :init
;;   (setq deft-extensions '("md" "txt"))
;;   :config
;;   (setq deft-auto-save-interval 15.0)
;;   (setq deft-text-mode 'markdown-mode))

;;==================================================
;; fasd settings
;;==================================================

;; (use-package fasd
;;   :bind ("C-h C-/" . fasd-find-file)
;;   :config
;;   (global-fasd-mode 1))

;;==================================================
;; multiple cursors
;;==================================================

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

;;==================================================
;; paredit
;;==================================================
;; (use-package paredit
;;   :diminish (paredit-mode . "Par")
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

;;==================================================
;; change-inner/outer
;;==================================================

(use-package change-inner
  :bind (("M-I" . change-inner)
         ("M-O" . change-outer)))

;;==================================================
;; misc
;;==================================================
(use-package misc
  :ensure nil
  :bind ("M-z" . zap-up-to-char)
  :init (bind-key "\M-Z" 'zap-to-char))

(use-package jc-misc
  :ensure nil
  :commands (chmod+x-this)
  :bind ("M-p" . goto-match-paren))

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("S-<return>" . crux-smart-open-line)
         ("C-S-k" . crux-kill-whole-line)
         ("C-S-<return>" . crux-smart-open-line-above))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-reopen-as-root-mode t))

;; (defadvice ido-find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(use-package jc-windows
  :ensure nil
  :load-path "site-lisp"
  :bind (("C-x |" . split-window-horizontally-instead)
         ("C-x _" . split-window-vertically-instead)
         ("C-2" . split-window-vertically-with-other-buffer)
         ("C-3" . split-window-horizontally-with-other-buffer)
         ("C-s-j" . quick-switch-buffer)))

(bind-key "C-1" 'delete-other-windows)
(bind-key "C-0" 'delete-window)

(use-package bm
  :ensure nil
  :bind (("<C-f2>" . bm-toggle)
         ("<f2>" . bm-next)
         ("<S-f2>" . bm-previous)))

;; resize windows
(bind-key "S-C-<left>" 'shrink-window-horizontally)
(bind-key "S-C-<right>" 'enlarge-window-horizontally)
(bind-key "S-C-<down>" 'shrink-window)
(bind-key "S-C-<up>" 'enlarge-window)

; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

; shrink-whitespace
(use-package shrink-whitespace
  :commands shrink-whitespace
  :bind ("M-SPC" . shrink-whitespace))

; beacon
(use-package beacon
  :diminish beacon-mode
  :config
  (setq beacon-color "#6F6F6F"
        beacon-blink-when-focused  t)
  (beacon-mode))

;; move-text
(use-package drag-stuff
  :diminish drag-stuff-mode
  :config
  (setq drag-stuff-modifier '(meta super))
  (drag-stuff-define-keys)
  (drag-stuff-global-mode t))

;; (defun sanityinc/newline-at-end-of-line ()
;;   "Move to end of line, enter a newline, and reindent."
;;   (interactive)
;;   (move-end-of-line 1)
;;   (newline-and-indent))

;; (bind-key "S-<return>" 'sanityinc/newline-at-end-of-line)

;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :diminish whole-line-or-region-global-mode
  :config
  (whole-line-or-region-global-mode t))

;;==================================================
;; ispell
;;==================================================
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(let ((langs '("american" "castellano8")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

;; Todo: Move to autoload
(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)
    (flyspell-buffer)
    (message "Spell language changed to %s" lang)))

;; TODO: Move to autoload
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(bind-keys :prefix-map jc-spelling-map
           :prefix "<f8>"
           ("<f8>" . ispell-word)
           ("m" . flyspell-mode)
           ("b" . flyspell-buffer)
           ("p" . flyspell-check-previous-highlighted-word)
           ("n" . flyspell-check-next-highlighted-word)
           ("c" . cycle-ispell-languages))

;;==================================================
;; mark customizations
;;==================================================
(use-package jc-marks
  :ensure nil
  :commands exchange-point-and-mark-no-activate
  :bind (("C-`" . push-mark-no-activate)
         ("M-`" . jump-to-mark)))

;;==================================================
;; swier settings
;;==================================================

;; (use-package ivy
;;   :require counsel
;;   :init
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (global-set-key "\C-s" 'swiper)
;;   (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;   (global-set-key (kbd "<f6>") 'ivy-resume)
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;   (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;   (global-set-key (kbd "<f1> l") 'counsel-load-library)
;;   (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;   (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;   (global-set-key (kbd "C-c g") 'counsel-git)
;;   (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;   (global-set-key (kbd "C-c k") 'counsel-ag)
;;   (global-set-key (kbd "C-x l") 'counsel-locate)
;;   (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;   (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

;;==================================================
;; smex settings
;;==================================================

;; (use-package smex
;;   :require ido
;;   :bind (("M-x" . smex)
;;          ("M-X" . smex-major-mode-commands)
;;          ("C-c C-c M-x" . execute-extended-command))
;;   :config
;;   (setq smex-save-file (expand-file-name "smex.items" user-emacs-directory))
;;   (smex-initialize))

(use-package amx
  :config
  (amx-mode t))

;;==================================================
;; Projectile settings
;;==================================================

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-mode-line
        '(:eval
          (format " Prj:%s"
                  (projectile-project-name))))

  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-sort-order 'recently-active)
  (when (executable-find "fd")
    (let ((fd-command "fd . --type f --print0"))
      (setq projectile-hg-command fd-command)
      (setq projectile-git-command fd-command)
      (setq projectile-fossil-command fd-command)
      (setq projectile-bzr-command fd-command)
      (setq projectile-darcs-command fd-command)
      (setq projectile-svn-command fd-command)
      (setq projectile-generic-command fd-command)))
  (projectile-global-mode)
  (setq projectile-require-project-file nil))

(use-package dumb-jump
  :config
  (dumb-jump-mode t))

;;==================================================
;; popup-imenu settings
;;==================================================

(use-package popup-imenu
  :bind ("M-i" . popup-imenu))

;;==================================================
;; company-mode settings
;;==================================================
(use-package company
  :diminish company-mode
  :defer 5
  :init
  (setq
   company-idle-delay 0.5
   company-show-numbers t
   company-dabbrev-downcase nil)
  :config
  (global-company-mode))

(use-package company-terraform
  :after company
  :config
  (add-to-list 'company-backends 'company-terraform))

;; (use-package company-quickhelp          ; Documentation popups for Company
;;   :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :commands yas-hippie-try-expand
  :init (progn
          (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
          (yas-global-mode 1)))

(use-package yasnippet-snippets         ; Collection of snippets
  :after yasnippet)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flymake-shellcheck
  :after flycheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package flycheck-popup-tip
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))


;; (use-package flycheck-posframe
;;   :ensure t
;;   :after flycheck
;;   :config
;;   (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
;;   (flycheck-posframe-configure-pretty-defaults)
;;   (setq flycheck-posframe-border-width 2)
;;   (set-face-foreground 'flycheck-posframe-border-face "red"))

;;==================================================
;; auto-complete settings
;;==================================================
;; (use-package fuzzy)
;; (use-package auto-complete
;;   :bind ("M-/" . auto-complete)
;;   :config
;;   (ac-config-default)
;;   (setq ac-auto-show-menu nil)
;;   (setq ac-auto-start nil)
;;   (setq ac-menu-height 15)
;;   (setq ac-fuzzy-enable t)
;;   (setq ac-dwim-enable t)
;;   (setq ac-use-menu-map t)
;;   (setq-default ac-sources '(ac-source-abbrev
;;                              ac-source-words-in-same-mode-buffers
;;                              ac-source-files-in-current-dir
;;                              ac-source-filename
;;                              ac-source-dictionary)))

;;==================================================
;; wgrep
;;==================================================

(use-package wgrep)
(use-package ag)
(use-package wgrep-ag
  :if (executable-find "ag")
  :config
  (setq-default ag-highlight-search t))

;;==================================================
;; dockerfile settings
;;==================================================

(use-package dockerfile-mode)

;;==================================================
;; terraform settings
;;==================================================

(use-package terraform-mode
  ;:requires auto-complete
  :mode ("\\.tf\\'" . terraform-mode)
  :config
  ;(add-to-list 'ac-modes 'terraform-mode)
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;;==================================================
;; web
;;==================================================
(use-package rust-mode
  :mode "\\.rs\\'")

;;==================================================
;; web
;;==================================================
(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

;; (setq web-mode-engines-alist
;;       '(("php" . "\\.phtml\\'")
;;         ("django" . "\\.html\\'")))


(setq js-indent-level 2)

(use-package yaml-mode
  :mode "\\.yaml?\\'")

(use-package json-mode
  :mode "\\.json?\\'")

(use-package scss-mode
  :mode "\\.scss?\\'"
  :init
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

;;==================================================
;; python
;;==================================================

(use-package pip-requirements
  :mode "requirements\\.txt\\'")

;;==================================================
;; Misc packages and utilities
;;==================================================
;; (use-package paradox)
;; (use-package cypher-mode)
;; (use-package jade-mode)
(use-package highlight-symbol)
;; (use-package ssh-config-mode)

(use-package avy
  :bind (("M-g g" . avy-goto-line)
         ;("C-'" . avy-goto-char)
         ("C-\"" . avy-goto-char-timer))
  :config
  (setq avy-keys
        '(?c ?a ?s ?d ?e ?f ?h ?w ?y ?j ?k ?l ?n ?m ?v ?r ?u ?p))
  (avy-setup-default))

(use-package ace-window
  ;; :bind
  ;; ("M-o" . ace-window)
  :init
  (global-set-key [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always nil))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package rainbow-mode
  :commands rainbow-mode
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))

;; ;; keep scratch around
;; (save-excursion
;;   (set-buffer (get-buffer-create "*scratch*"))
;;   (lisp-interaction-mode)
;;   (make-local-variable 'kill-buffer-query-functions)
;;   (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

;; (defun kill-scratch-buffer ()
;;   ;; The next line is just in case someone calls this manually
;;   (set-buffer (get-buffer-create "*scratch*"))
;;   ;; Kill the current (*scratch*) buffer
;;   (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
;;   (kill-buffer (current-buffer))
;;   ;; Make a brand new *scratch* buffer
;;   (set-buffer (get-buffer-create "*scratch*"))
;;   (lisp-interaction-mode)
;;   (make-local-variable 'kill-buffer-query-functions)
;;   (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
;;   (insert initial-scratch-message)
;;   ;; Since we killed it, don't let caller do that.
;;   nil)


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

;;==================================================
;; experiments
;;==================================================
;; (use-package golden-ratio
;;   :diminish golden-ratio-mode
;;   :init (golden-ratio-mode 1))

;; (use-package guru-mode
;;   :config (guru-global-mode 1)
;;   :diminish guru-mode)


;(use-package indent-guide)

;;==================================================
;; server
;;==================================================

(use-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))


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

(defvar doom-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).
More specifically, when `doom/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun doom/escape ()
  "Run `doom-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'doom-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'doom/escape)

;; Don't resize windows & frames in steps; it's prohibitive to prevent the user
;; from resizing it to exact dimensions, and looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)


;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode t)

 ;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160
      split-height-threshold nil)

(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Enable mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

(use-package hl-line
  ;; Highlights the current line
  :hook ((prog-mode text-mode conf-mode) . hl-line-mode)
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


(use-package restart-emacs)

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

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

;; doom: recentf better-jumber dtrt-indent smartparens so-long
;; ws-butler pcre2el highlight-indent-guides doom/escape auto-revert
;; company ivy nav-flash workspaces
;;
;; use-package defer
