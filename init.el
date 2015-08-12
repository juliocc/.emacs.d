;; Report load time after initializing
(add-hook 'after-init-hook 'emacs-init-time)
;(setq use-package-verbose t)

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-windowed-mac* (and *is-a-mac* window-system))

;; Turn off mouse interface early in startup to avoid momentary display
(unless *is-a-windowed-mac* ; hide menu if not in Mac
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1)))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Put this file in register e for easy access
(set-register ?e `(file . ,user-init-file))

;; No splash screen
(setq inhibit-startup-message t)

;; Keep emacs custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;==================================================
;; Setup package management tools
;;==================================================

(require 'package)

;; Add melpa to package repos
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'req-package)
  (package-refresh-contents)
  (package-install 'req-package))

(require 'req-package)

;; (req-package-force el-get
;;   :init (progn (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
;;                (el-get 'sync)))


;;==================================================
;; Appearance settings
;;==================================================
(req-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(setq default-frame-alist '((cursor-type . (bar . 2))))
(setq-default frame-background-mode 'dark)

; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode nil)
  (blink-cursor-mode nil))

;; Set default font
(if (find-font (font-spec :name "Inconsolata"))
    (set-face-font 'default "Inconsolata 11"))

;; Other good fonts. Test text: ()[]l1t  O0o Ilegal1 = O0
;; (set-face-font 'default "Envy Code R 10")
;; (set-face-font 'default "ProggyCleanTT 12")
;; (set-face-font 'default "gohufont")
;; (set-face-font 'default "Consolas 11")
;; (set-face-font 'default "Droid Sans Mono Dotted 10")
;; (set-face-font 'default "Anonymous Pro 10")
;; (set-face-font 'default "Liberation Mono 10")
;; (set-face-font 'default "Ubuntu Mono 11")
;; (set-face-font 'default "Source Code Pro 10")
;; (set-face-font 'default "MonteCarlo")

;; Put fringe on the side
(if (fboundp 'fringe-mode) (fringe-mode 5))

(global-font-lock-mode t)               ; just in case
(line-number-mode 1)
(column-number-mode 1)

(req-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(req-package smooth-scrolling)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 'always)
;(setq scroll-step 0); what?

;;==================================================
;; General settings
;;=================================================

(setq mouse-wheel-scroll-amount '(0.01))

(setq
 mouse-yank-at-point t                  ; mouse pastes at point
 x-select-enable-clipboard t            ; Allow pasting selection outside of Emacs
 global-auto-revert-non-file-buffers t  ; auto refresh dired
 auto-revert-verbose nil                ; and be quiet about it
 eval-expression-print-level nil
 echo-keystrokes 0.1                    ; Show keystrokes in progress
 confirm-kill-emacs 'yes-or-no-p        ; ask me before closing
 history-length 1000                    ; looong history
 use-dialog-box nil                     ; never show a dialog box
 use-file-dialog nil

 ; browse-url-browser-function 'browse-url-generic
 ; browse-url-generic-program "firefox-trunk"
 ; browse-url-new-window-flag  t
 ; browse-url-firefox-new-window-is-tab
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
 next-line-add-newlines nil
 ) ; don't add new lines when scrolling down

(setq-default visible-bell t)
(setq-default show-trailing-whitespace t)
(setq-default highlight-tabs t)
(setq-default indicate-empty-lines t)
(setq-default truncate-lines t)         ; don't word-wrap
(setq-default save-interprogram-paste-before-kill t)
(setq-default set-mark-command-repeat-pop t)
(setq shift-select-mode nil)            ; this is not windows
(setq delete-by-moving-to-trash t)

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
(bind-key "M-/" 'hippie-expand)

;; Delete whitespace at the end of lines when saving
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

(req-package whitespace-cleanup-mode
  :config
  ;(add-to-list 'whitespace-cleanup-mode-ignore-modes 'deft-mode)
  (global-whitespace-cleanup-mode))

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
(req-package files
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

(req-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :config
  (global-highlight-parentheses-mode t))

(req-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(req-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist
        `((".*" . ,(concat user-emacs-directory "undo-list")))))

;; Save a list of recent files visited.
(setq recentf-max-saved-items 1000)
(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(setq recentf-exclude '("/tmp/" "/ssh:"))
(recentf-mode 1)

;; Save minibuffer history
(setq history-length 1000)
(setq savehist-file (expand-file-name ".savehist" user-emacs-directory))
;(setq savehist-additional-variables '(search ring regexp-search-ring)
(savehist-mode t)

;; Never ever use tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)            ;; but maintain correct apeparance

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; uniquify:  provide meaningful names for buffers with the same name
(req-package uniquify
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
;; (show-paren-mode +1)
;; (setq show-paren-style 'parenthesis)

;; Save point position between sessions
(require 'saveplace)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
(save-place-mode t)

;; initiate GC every 20 MB allocated (default is 0.8MB)
(setq gc-cons-threshold 20000000)

;; guide-key setup
(req-package guide-key
  :diminish guide-key-mode
  :config
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8"
                                       "C-x C-k" "<f8>" "C-c !" "M-s"
                                       "C-x n" "C-c p"))
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%")))
  (guide-key-mode 1)
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom))

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
  (setq mac-option-modifier 'none)
  (setq mac-control-modifier 'control)
  (setq ns-function-modifier 'hyper)
  (setq mac-command-modifier 'meta)
  (setq default-input-method "MacOSX")
  (setq insert-directory-program "gls")  ; dired works better with gls
  (set-face-font 'default "Consolas 14")
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (kbd (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key [kp-delete] 'delete-char)) ;; sets fn-delete to be right-delete

(req-package exec-path-from-shell
  :if *is-a-mac*
  :config
  (exec-path-from-shell-initialize))

(req-package reveal-in-finder
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
(req-package flx-ido
  :require (ido flx ido-vertical-mode ido-ubiquitous)
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
                                    ".c" ".cpp" ".cxx" ".h" ".hpp"))
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (setq ido-use-faces nil)
  (setq confirm-nonexistent-file-or-buffer nil)
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
  (flx-ido-mode 1)
  (ido-vertical-mode)
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
(req-package git-blame)
(req-package git-commit-mode)
(req-package git-rebase-mode)
(req-package gitconfig-mode)
(req-package gitignore-mode)
(req-package git-timemachine)
(req-package gl-conf-mode
  :load-path "site-lisp/gl-conf-mode"
  :mode "gitolite\\.conf\\'")

(req-package magit
  :diminish magit-auto-revert-mode
  :bind ("C-x C-z" . magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-repo-dirs '("~/code/"))
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

(req-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message))

;;==================================================
;; isearch settings
;;==================================================

;; use regexp isearch by default
(global-set-key [remap isearch-forward] 'isearch-forward-regexp)
(global-set-key [remap isearch-backward] 'isearch-backward-regexp)

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(req-package jc-search
  :commands (zap-to-isearch isearch-exit-other-end isearch-yank-symbol)
  :init (bind-keys :map isearch-mode-map
                   ("M-z" . zap-to-isearch)
                   ("C-w" . isearch-yank-symbol)
                   ("C-RET" . isearch-exit-other-end)))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)
(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(req-package anzu
  :diminish anzu-mode
  :config
  ;; show number of matches while searching
  (global-anzu-mode 1)
  ;; use anzu for query for query-replace
  (global-set-key [remap query-replace] 'anzu-query-replace-regexp))

;;==================================================
;; elisp
;;==================================================
(req-package highlight-quoted
  :commands highlight-quoted-mode
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (highlight-quoted-mode 1))))

;(add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;;==================================================
;; smart-mode-line
;;==================================================
(req-package smart-mode-line
  :config
  (sml/setup))

;;==================================================
;; smartparens
;;==================================================
(req-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (sp-use-smartparens-bindings)
  (show-smartparens-global-mode))

;;==================================================
;; ediff
;;==================================================

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;==================================================
;; scss-mode settings
;;==================================================

; TODO: add mode
(req-package scss-mode
  :config
  (setq scss-compile-at-save nil))

;;==================================================
;; Dired settings
;;==================================================

(require 'dired)

; dired
(setq dired-listing-switches "-alhF")
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

(req-package dired+
  :config
  (global-dired-hide-details-mode -1))

;;==================================================
;; browse-kill-ring settings
;;==================================================
(req-package browse-kill-ring+
  :require (browse-kill-ring)
  :config
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-highlight-inserted-item t)
  (setq browse-kill-ring-quit-action 'save-and-restore))

;;==================================================
;; deft
;;==================================================

(req-package deft
  :require markdown-mode
  :config
  (setq deft-extension "md")
  (setq deft-auto-save-interval 15.0)
  (setq deft-text-mode 'markdown-mode))

;;==================================================
;; fasd settings
;;==================================================

(req-package fasd
  :bind ("C-h C-/" . fasd-find-file)
  :config
  (global-fasd-mode 1))

;;==================================================
;; multiple cursors
;;==================================================

(req-package multiple-cursors
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
;; change-inner/outer
;;==================================================
;; (req-package paredit
;;   :diminish (paredit-mode . "Par")
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

;;==================================================
;; change-inner/outer
;;==================================================

(req-package change-inner
  :bind (("M-I" . change-inner)
         ("M-O" . change-outer)))

;;==================================================
;; misc
;;==================================================
(req-package misc
  :bind ("M-z" . zap-up-to-char)
  :init (bind-key "\M-Z" 'zap-to-char))

(req-package jc-misc
  :commands (chmod+x-this find-shell-init-file)
  :bind (("M-p" . goto-match-paren)
         ("C-a" . beginning-of-line-or-indentation)))

;; (defadvice ido-find-file (after find-file-sudo activate)
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(req-package jc-windows
  :load-path "site-lisp"
  :bind (("C-x |" . split-window-horizontally-instead)
         ("C-x _" . split-window-vertically-instead)
         ("C-x 2" . split-window-vertically-with-other-buffer)
         ("C-x 3" . split-window-horizontally-with-other-buffer)))

;; resize windows
(bind-key "S-C-<left>" 'shrink-window-horizontally)
(bind-key "S-C-<right>" 'enlarge-window-horizontally)
(bind-key "S-C-<down>" 'shrink-window)
(bind-key "S-C-<up>" 'enlarge-window)

; ibuffer
(req-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; move-text
(req-package move-text
  :bind (("<M-up>" . move-text-up)
         ("<M-down>" . move-text-down)))

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(bind-key "S-<return>" 'sanityinc/newline-at-end-of-line)

;; Cut/copy the current line if no region is active
(req-package whole-line-or-region
  :diminish whole-line-or-region-mode
  :config
  (whole-line-or-region-mode t))

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
(req-package jc-marks
  :commands exchange-point-and-mark-no-activate
  :bind (("C-`" . push-mark-no-activate)
         ("M-`" . jump-to-mark)))

;;==================================================
;; smex settings
;;==================================================

(req-package smex
  :require ido
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :config
  (setq smex-save-file (expand-file-name "smex.items" user-emacs-directory))
  (smex-initialize))

;;==================================================
;; Projectile settings
;;==================================================

(req-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-require-project-file nil))

;;==================================================
;; wgrep
;;==================================================

(req-package wgrep)

(req-package wgrep-ag
  :require (ag wgrep)
  :if (executable-find "ag")
  :config
  (setq-default ag-highlight-search t))


;;==================================================
;; web
;;==================================================
(req-package web-mode
  :mode "\\.html?\\'"
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

;; (setq web-mode-engines-alist
;;       '(("php" . "\\.phtml\\'")
;;         ("django" . "\\.html\\'")))


(setq js-indent-level 2)

(req-package yaml-mode
  :mode "\\.yaml?\\'")

(req-package json-mode
  :mode "\\.json?\\'")

(req-package scss-mode
  :mode "\\.scss?\\'"
  :init
  (setq scss-compile-at-save nil))

;;==================================================
;; jump-char
;;==================================================

;; NOTE: keep this after loading ido, otherwise M-m won't work inside
;; ido
(req-package jump-char
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward)))

;;==================================================
;; python
;;==================================================

(req-package pip-requirements)

;;==================================================
;; yasnippet
;;==================================================

;; (req-package yasnippet
;;   :diminish yas-global-mode
;;   :config (progn
;;           (setq yas/prompt-functions '(yas/dropdown-prompt
;;                                        yas/ido-prompt
;;                                        yas/completing-prompt))

;;           (yas-global-mode 1)))

;;==================================================
;; Misc packages and utilities
;;==================================================
(req-package "paradox")
(req-package "cypher-mode")
(req-package "jade-mode")
(req-package "highlight-symbol")
(req-package "markdown-mode")

(req-package avy-mode
  :bind (("M-g M-g" . avy-goto-line)
         ("C-'" . avy-goto-char)
         ("C-\"" . avy-goto-char))
  :config
  (avy-setup-default))

(req-package ace-window
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(req-package expand-region
  :bind ("C-=" . er/expand-region))

(req-package fic-mode
  :diminish fic-mode
  :commands turn-on-fic-mode
  :init
  (add-hook 'prog-mode-hook 'turn-on-fic-mode)
  :config
  (set-face-attribute 'font-lock-fic-face nil
                      :inherit font-lock-warning-face
                      :foreground nil
                      :background nil
                      :underline nil))

(req-package rainbow-mode
  :commands rainbow-mode
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'html-mode-hook 'rainbow-mode))

;; keep scratch around
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (insert initial-scratch-message)
  ;; Since we killed it, don't let caller do that.
  nil)


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


(fset 'quick-switch-buffer [?\C-x ?b return])
(bind-key "C-S-j" 'quick-switch-buffer)


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
;; (req-package golden-ratio
;;   :diminish golden-ratio-mode
;;   :init (golden-ratio-mode 1))

;; (req-package guru-mode
;;   :config (guru-global-mode 1)
;;   :diminish guru-mode)


;(req-package indent-guide)

;;==================================================
;; server
;;==================================================

(req-package server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t))

;; TODO:
;; * ibuffer setup
;; * Check tagedit: https://github.com/magnars/tagedit

;;==================================================
;; Now finally load everything
;;==================================================
(req-package-finish)
