;; Report load time after initializing
(add-hook 'after-init-hook 'emacs-init-time)

;; Put this file in register e for easy access
(set-register ?e '(file . "~/.emacs.d/init.el"))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-message t)

;;==================================================
;; Load path
;;==================================================
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Keep emacs custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'jc-utils)
;;==================================================
;; Packages
;;==================================================

(require 'package)
(require 'dash)

;; Add melpa to package repos
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Add marmalade to repos
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)


(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun packages-install (packages)
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))


(defun init--install-packages ()
  (packages-install
   '(s
     flx
     flx-ido
     yasnippet
     smartparens
     ido-vertical-mode
     ido-ubiquitous
     ;simple-httpd
     ;restclient
     ;whitespace-cleanup-mode
     magit
     git-blame
     git-commit-mode
     git-rebase-mode
     gitconfig-mode
     gitignore-mode
     git-messenger
     ;prodigy
     scss-mode
     yaml-mode
     haml-mode
     apache-mode
     nginx-mode
     browse-kill-ring
     idle-highlight-mode
     rainbow-delimiters
     ace-jump-mode
     ace-jump-buffer
     expand-region
     change-inner
     wgrep
     multiple-cursors
     haskell-mode
     move-text
     projectile
     volatile-highlights
     diminish
     web-mode
     css-mode
     php-mode
     guide-key
     smex
                                        ;tangotango-theme
     zenburn-theme
     smooth-scrolling
     window-numbering
     rainbow-mode
     jump-char
     virtualenvwrapper
     jedi
     keyfreq
     annoying-arrows-mode
     fic-mode
     anzu
     js2-mode
     fasd
     org-plus-contrib
     flycheck
     json-mode)))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;;==================================================
;; Appearance settings
;;==================================================
(setq default-frame-alist '((cursor-type . (bar . 2))))
(setq-default frame-background-mode 'dark)
(load-theme 'zenburn)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Put fringe on the side
(if (fboundp 'fringe-mode) (fringe-mode 5))

;; Set default font
(set-face-font 'default "Inconsolata 11")
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


;; show line and column number in the modeline
(line-number-mode 1)
(column-number-mode 1)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; diminish settings
;; (diminish 'subword-mode)
(after-load 'projectile (diminish 'projectile-mode))
(after-load 'yasnippet (diminish 'yas-minor-mode))
(after-load 'volatile-highlights (diminish 'volatile-highlights-mode))
(after-load 'guide-key (diminish 'guide-key-mode))
(after-load 'auto-complete (diminish 'auto-complete-mode))
(after-load 'flymake (diminish 'flymake-mode))
(after-load 'smartparens (diminish 'smartparens-mode))
(after-load 'fic-mode (diminish 'fic-mode))
(after-load 'anzu (diminish 'anzu-mode))


;; just in case
(global-font-lock-mode t)


;;==================================================
;; General settings
;;=================================================

(setq mouse-wheel-scroll-amount
      '(1 ((shift) . 1)))               ; ??
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
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "firefox-trunk"
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


;(set-default 'imenu-auto-rescan t)
(setq-default visible-bell t)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default truncate-lines t)         ; don't word-wrap
(setq-default save-interprogram-paste-before-kill t)
(setq-default set-mark-command-repeat-pop t)

;; Delete whitespace at the end of lines when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; But don't show trailing whitespace in these modes
(dolist (hook '(term-mode-hook
                comint-mode-hook
                compilation-mode-hook))
  (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))


;; don't confirm killing buffers with attached processes
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; Backup settings
(setq
 backup-by-copying t                          ; don't clobber symlinks
 delete-old-versions t                        ; delete old backups
 kept-new-versions 6
 kept-old-versions 2
 version-control t)                     ; use versioned backups

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; confirm with y/n only
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 everything please
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Useful modes
(auto-image-file-mode 1)                ; display images
(size-indication-mode 1)                ; display file size
(delete-selection-mode 1)               ; delete selected text on input
;(global-subword-mode 1)
(global-auto-revert-mode 1)             ; auto reload files if changed outside emacs
(auto-compression-mode t)               ; open compressed files a la dired
(transient-mark-mode 1)                 ; show me the region, please
(winner-mode 1)                         ; stack window settings

(global-rainbow-delimiters-mode 1)
(window-numbering-mode 1)
(minibuffer-depth-indicate-mode 1)
;(global-annoying-arrows-mode 1)
(electric-indent-mode 0)             ; make return key not auto indent
(desktop-save-mode 1)

(setq shift-select-mode nil)            ; this is not windows
(setq delete-by-moving-to-trash t)

(require 'volatile-highlights)
(volatile-highlights-mode 1)

;; Save a list of recent files visited.
(setq recentf-max-saved-items 1000)
(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(recentf-mode 1)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-x f") 'recentf-ido-find-file)


;; Save minibuffer history
(setq history-length 1000)
(setq savehist-file (expand-file-name ".savehist" user-emacs-directory))
;(setq savehist-additional-variables '(search ring regexp-search-ring)
(savehist-mode t)

;; Never ever use tabs
(set-default 'indent-tabs-mode nil)
(setq-default tab-width 8)            ;; but maintain correct apeparance

;; uniquify
;; provide meaningful names for buffers with the same name
(require 'uniquify)
;(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; smartparens
;; (require 'smartparens)
;; (require 'smartparens-config)
;; (smartparens-global-mode)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; show-paren-mode: subtle highlighting of matching parens (global-mode)
(show-paren-mode +1)
(setq show-paren-style 'parenthesis)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; initiate GC every 20 MB allocated (default is 0.8MB)
(setq gc-cons-threshold 20000000)

;; guide-key setup
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8"
                                     "C-x C-k" "<f8>" "C-c !" "M-s"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; Font lock dash.el
(after-load 'dash (dash-enable-font-lock))

;; Don't cripple my emacs
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

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
;; ispell settings
;;==================================================
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(let ((langs '("american" "castellano8")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)
    (flyspell-buffer)
    (message "Spell language changed to %s" lang)))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

;;==================================================
;; flycheck settings
;;==================================================
(dolist (hook '(css-mode-hook
                js2-mode-hook
                ;; python ; use flymake for now
                ;; sass-mode-hook ; figure out how to use rvm's version
                ;; scss-mode-hook
                sh-mode-hook-hook
                haskell-mode-hook
                json-mode
                ;; web-mode-hook
                ))
  (add-hook hook 'flycheck-mode))


;;==================================================
;; ido settings
;;==================================================
(ido-mode t)
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
(setq confirm-nonexistent-file-or-buffer nil)

(flx-ido-mode 1)
(setq ido-use-faces nil)
(ido-vertical-mode)
(ido-ubiquitous-mode 1)

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


;;==================================================
;; isearch settings
;;==================================================

(global-anzu-mode 1)                    ; show number of matches while searching

;; use regexp isearch by default
(global-set-key [remap isearch-forward] 'isearch-forward-regexp)
(global-set-key [remap isearch-backward] 'isearch-backward-regexp)

;; use anzu for query for query-replace
(global-set-key [remap query-replace] 'anzu-query-replace-regexp)

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; http://www.emacswiki.org/emacs/ZapToISearch
(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
the isearch match string. The behaviour is meant to be analogous
to zap-to-char; let's call it zap-to-isearch. The deleted region
does not include the isearch word. This is meant to be bound only
in isearch mode. The point of this function is that oftentimes
you want to delete some portion of text, one end of which happens
to be an active isearch word. The observation to make is that if
you use isearch a lot to move the cursor around (as you should,
it is much more efficient than using the arrows), it happens a
lot that you could just delete the active region between the mark
and the point, not include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)
    ))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)

;; http://www.emacswiki.org/emacs/ZapToISearch
(defun isearch-exit-other-end (rbeg rend)
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;;==================================================
;; git and magit settings
;;==================================================
(require 'magit)
(global-set-key (kbd "C-x C-z") 'magit-status)
(setq magit-repo-dirs '("~/code/"))

(setq-default
 magit-stage-all-confirm nil
 magit-unstage-all-confirm nil
 magit-save-some-buffers nil
 magit-process-popup-time 5
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

;; ;; C-c C-a to amend without any prompt
;; (defun magit-just-amend ()
;;   (interactive)
;;   (save-window-excursion
;;     (magit-with-refresh
;;       (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

;; (eval-after-load "magit"
;;   '(define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend))

;; full screen magit-status

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

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

;;==================================================
;; org
;;==================================================

(setq org-agenda-include-diary t
      org-log-done 'note
      org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "MAYBE(m@)"
		  "|" "DONE(d!)" "CANCELED(c@)" "POSTPONED(p@)"))
      org-lowest-priority ?C
      org-default-priority ?B
      org-completion-use-ido t
      org-agenda-ndays 14
      org-agenda-start-on-weekday nil
      org-agenda-show-all-dates t
      org-agenda-skip-scheduled-if-done nil
      org-return-follows-link t
      org-agenda-tags-column -100
      org-agenda-todo-ignore-scheduled nil
      org-columns-default-format
        ;"%30ITEM(Task) %1PRIORITY(P) %7Effort(Effort){:} %CLOCKSUM %20SCHEDULED %DEADLINE %TODO(T)"
        "%30ITEM %1PRIORITY(P) %SCHEDULED %DEADLINE %TODO %ALLTAGS "
      org-deadline-warning-days 7
      ;; org-global-properties
      ;; '(("Effort_ALL" .
      ;;    "0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00 3:00 4:00 5:00 6:00 7:00 8:00"))
      org-hide-leading-stars t
      org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-timestamp-if-done t
      org-agenda-use-time-grid t
      org-blank-before-new-entry '((heading . t) (plain-list-item . t))
      org-clock-out-remove-zero-time-clocks t
      org-clock-persist t
      org-use-fast-todo-selection t
      org-use-fast-tag-selection nil
      org-default-notes-file "~/org/notes.org"
      org-refile-targets '((org-agenda-files :maxlevel . 2)
                           ("~/org/research.org" :maxlevel . 3))
      org-agenda-restore-windows-after-quit t
      org-odd-levels-only t
      )

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-co" 'org-switchb)


;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;;==================================================
;; ediff
;;==================================================

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;==================================================
;; ibuffer
;;==================================================

(after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  )

(after-load 'ibuffer
            (require 'ibuffer-git)
            (require 'ibuffer-vc))

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)
        (mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              ""
              (git-status 8 8 :left)
              " "
              filename-and-process)))

;;==================================================
;; Python (jedi + flymake)
;;==================================================
(require 'virtualenvwrapper)

(defun project-directory (buffer-name)
  "Returns the root directory of the project that contains the
given buffer. Any directory with a .git or .jedi file/directory
is considered to be a project root."
  (interactive)
  (let ((root-dir (file-name-directory buffer-name)))
    (while (and root-dir
                (not (file-exists-p (concat root-dir ".git")))
                (not (file-exists-p (concat root-dir ".jedi"))))
      (setq root-dir
            (if (equal root-dir "/")
                nil
              (file-name-directory (directory-file-name root-dir)))))
    root-dir))

(defun project-name (buffer-name)
  "Returns the name of the project that contains the given buffer."
  (let ((root-dir (project-directory buffer-name)))
    (if root-dir
        (file-name-nondirectory
         (directory-file-name root-dir))
      nil)))

(defun jedi-setup-venv ()
  "Activates the virtualenv of the current buffer."
  (let ((project-name (project-name buffer-file-name)))
    (when project-name
        (message "Using %s for jedi virtualenv" project-name)
        project-name (venv-workon project-name))))

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi-setup-venv)
(add-hook 'python-mode-hook 'jedi:setup)

(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "/home/julio/.virtualenvs/clitools/bin/flake8")

;;==================================================
;; browse-kill-ring settings
;;==================================================
(require 'browse-kill-ring)
(require 'browse-kill-ring+)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-highlight-current-entry t)
(setq browse-kill-ring-highlight-inserted-item t)
(setq browse-kill-ring-quit-action 'save-and-restore)

;;==================================================
;; scss-mode settings
;;==================================================

(after-load 'scss-mode (setq scss-compile-at-save nil))

;;==================================================
;; javascript settings
;;==================================================

(add-to-list 'auto-mode-alist '("\\.js\\'"   . js2-mode))
(after-load 'js2-mode
  (add-hook 'js2-mode-hook '(lambda () (setq mode-name "JS2"))))

(setq js2-use-font-lock-faces t
      js2-mode-must-byte-compile nil
      js2-basic-offset 2
      js2-indent-on-enter-key t
      js2-auto-indent-p t
      js2-bounce-indent-p nil)

;;==================================================
;; fasd settings
;;==================================================

(global-set-key (kbd "C-h C-/") 'fasd-find-file)
(global-fasd-mode 1)

;;==================================================
;; auto-complete settings
;;==================================================
(require 'auto-complete-config)
(ac-config-default)

;;==================================================
;; smex settings
;;==================================================

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;==================================================
;; Projectile settings
;;==================================================

(projectile-global-mode)
(setq projectile-require-project-file nil)

;;==================================================
;; web-mode settings
;;==================================================

(require 'web-mode)
(setq web-mode-engines-alist
      '(("php" . "\\.phtml\\'")
        ("django" . "\\.html\\'")))

;;==================================================
;; Dired settings
;;==================================================

(require 'dired)

; dired
(setq dired-listing-switches "-alh")
(setq dired-dwim-target t) ; Move files between split pane

;; M-up is nicer in dired if it moves to the fourth line - the first file
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
(define-key dired-mode-map (vector 'remap 'smart-up) 'dired-back-to-top)

;;==================================================
;; Misc packages and utilities
;;==================================================

(require 'smooth-scrolling)
(require 'wgrep)
(require 'gl-conf-mode)
(require 'fic-mode)

(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'turn-on-fic-mode)

(add-hook 'package-menu-mode-hook 'hl-line-mode)

(keyfreq-mode 1)

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


(defun chmod+x-this ()
  "Add executable permissions to the current file."
  (interactive)
  (if buffer-file-name
      (let ((new-mode (logior #o111 (file-modes buffer-file-name))))
        (set-file-modes buffer-file-name new-mode))
    (message "No such file to make executable.")))

(defun find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (split-string (getenv "SHELL") "/"))))
         (shell-init-file (cond
                           ((string-equal "zsh" shell) ".zshrc")
                           ((string-equal "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))

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

(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)
(global-set-key (kbd "<f6>")
                (lambda ()
                  (interactive)
                  (switch-to-buffer nil)))

; When splitting window, show (other-buffer) in the new window
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))

;;==================================================
;; Other keybindings
;;==================================================

(defun make-repeatable-command (cmd)
  "Returns a new command that is a repeatable version of CMD.
The new command is named CMD-repeat.  CMD should be a quoted
command.

This allows you to bind the command to a compound keystroke andб
repeat it with just the final key.  For example:

  (global-set-key (kbd \"C-c a\") (make-repeatable-command 'foo))

will create a new command called foo-repeat.  Typing C-c a will
just invoke foo.  Typing C-c a a a will invoke foo three times,
and so on."
  (fset (intern (concat (symbol-name cmd) "-repeat"))
        `(lambda ,(help-function-arglist cmd) ;; arg list
           ,(format "A repeatable version of `%s'."
                    (symbol-name cmd)) ;; doc string
           ,(interactive-form cmd) ;; interactive form
           ;; see also repeat-message-function
           (setq last-repeatable-command ',cmd)
           (repeat nil)))
  (intern (concat (symbol-name cmd) "-repeat")))


(global-set-key (kbd "C-'") 'ace-jump-mode)

(require 'expand-region)
(global-set-key (kbd "C-c w") (make-repeatable-command 'er/expand-region))


(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key (kbd "<f9>") 'switch-to-previous-buffer)
(global-set-key (kbd "C-<f9>") 'ace-jump-buffer)

(require 'jump-char)
(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(shift meta m)] 'jump-char-backward)

(require 'yasnippet)
(yas/global-mode 1)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))


(require 'change-inner)
(global-set-key (kbd "M-I") 'change-inner)
(global-set-key (kbd "M-O") 'change-outer)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
;(global-set-key (kbd "C-s-SPC") 'set-rectangular-region-anchor)

;; move-text
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

;; resize windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(autoload 'zap-up-to-char "misc")
(global-set-key "\M-z"      'zap-up-to-char)
(global-set-key "\M-X"      'zap-to-char)
(global-set-key (kbd "C-x g") 'webjump)

(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x M-j") '(lambda () (interactive) (dired-jump 1)))

(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "C-S-j") 'quick-switch-buffer)

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))
(global-set-key (kbd "C-x F") 'djcb-find-file-as-root)

(define-prefix-command 'jc-spelling-map)
(global-set-key (kbd "<f8>") 'jc-spelling-map)
(define-key jc-spelling-map (kbd "<f8>") 'ispell-word)
(define-key jc-spelling-map (kbd "m") 'flyspell-mode)
(define-key jc-spelling-map (kbd "b") 'flyspell-buffer)
(define-key jc-spelling-map (kbd "p") 'flyspell-check-previous-highlighted-word)
(define-key jc-spelling-map (kbd "n") 'flyspell-check-next-highlighted-word)
(define-key jc-spelling-map (kbd "c") 'cycle-ispell-languages)


(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on paranthesis. Else go to the
   opening paranthesis one level up."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))
                  ))))))

(global-set-key (kbd "M-p") 'goto-match-paren)

(global-set-key (kbd "M-/") 'hippie-expand)

;;==================================================
;; faces
;;==================================================
(set-face-attribute 'font-lock-fic-face nil
  :inherit font-lock-warning-face
  :foreground nil
  :background nil
  :underline nil)

(require 'paren) ; show-paren-mismatch is defined in paren.el
(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'show-paren-mismatch)

(defface my-outermost-paren-face
  '((t (:weight bold)))
  "Face used for outermost parens.")

(setq rainbow-delimiters-outermost-only-face-count 1)
(set-face-attribute 'rainbow-delimiters-depth-1-face nil
                    :foreground 'unspecified
                    :inherit 'my-outermost-paren-face)

;;==================================================
;; Mode mappings
;;==================================================
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("gitolite\\.conf\\'" . gl-conf-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'"   . yaml-mode))


;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
